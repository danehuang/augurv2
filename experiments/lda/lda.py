import os.path as op
import numpy as np
import scipy as sp

from util.infersys import *
from util.modelenv import *

#-------------------------------------------------------------
# Models

augur_lda = '''(K : Int, D : Int, N : Vec Int, alpha : Vec Real, beta : Vec Real) => {
  param theta[d] ~ Dirichlet(alpha)
      for k <- 0 until D ;
  param phi[k] ~ Dirichlet(beta)
      for k <- 0 until K ;
  param z[d, n] ~ Categorical(theta[d])
      for d <- 0 until D, n <- 0 until N[d] ;
  data w[d, n] ~ Categorical(phi[z[d, n]])
      for d <- 0 until D, n <- 0 until N[d] ;
}
'''

jags_lda = '''
model {
  for (d in 1:D) {
    theta[d,1:K] ~ ddirch(alpha)
  }
  for (k in 1:K) {
    phi[k,1:V] ~ ddirch(beta)
  }
  for (d in 1:D) {
    for (j in 1:N[d]){
      z[d,j] ~ dcat(theta[d,1:K])
      w[d,j] ~ dcat(phi[z[d,j],1:V])
    }
  }
}
'''

stan_lda = '''
data {
  int<lower=2> K;               // num topics
  int<lower=2> V;               // num words
  int<lower=1> D;               // num docs
  int<lower=1> N;               // total word instances
  int<lower=1,upper=V> w[N];    // word n
  int<lower=1,upper=D> doc[N];  // doc ID for word n
  vector<lower=0>[K] alpha;     // topic prior
  vector<lower=0>[V] beta;      // word prior
}
parameters {
  simplex[K] theta[D];          // topic dist for doc d
  simplex[V] phi[K];            // word dist for topic k
}
model {
  for (d in 1:D)
    theta[d] ~ dirichlet(alpha); 
  for (k in 1:K)
    phi[k] ~ dirichlet(beta); 
  for (n in 1:N) {
    real gamma[K];
    for (k in 1:K) {
      gamma[k] = log(theta[doc[n], k]) + log(phi[k, w[n]]);
    }
    target += log_sum_exp(gamma); 
  }
}
'''

model_phi_fixed = '''(K : Int, D : Int, N : Vec Int, alpha : Vec Real, phi : Vec (Vec Real)) => {
  param theta[d] ~ Dirichlet(alpha) 
      for d <- 0 until D ;
  param z[d, n] ~ Categorical(theta[d])
      for d <- 0 until D, n <- 0 until N[d] ;
  data w[d, n] ~ Categorical(phi[z[d, n]])
      for d <- 0 until D, n <- 0 until N[d] ;
}
'''

models = { 'augur': augur_lda, 'jags': jags_lda, 'stan': stan_lda }

KEY_NUMTOPICS = 'numtopics'
KEY_ALPHA = 'alpha'
KEY_BETA = 'beta'
MODEL_SETTINGS = { KEY_NUMTOPICS: 50, KEY_ALPHA: 0.1, KEY_BETA: 0.1 }


#-------------------------------------------------------------
# LDA Environment

class LdaEnv(ModelEnv):
	def __init__(self, models, path_to_data, base_path_to_sbox, dataset, trial):
		super(LdaEnv, self).__init__(models, path_to_data, base_path_to_sbox, dataset, trial)

	def load_dataset(self):
		# type: tuple[?]
		"""
		"""
		base_path = op.join(self.path_to_data, self.dataset)

		# path_to_data/dataset/train1/dataset.npy
		with open(op.join(base_path, 'train1', self.dataset + '.npy'), 'r') as f:
			train_w = np.load(f)
		# path_to_data/dataset/test1/dataset-obs.npy
		with open(op.join(base_path, 'test1', self.dataset + '-obs.npy'), 'r') as f:
			test_obs_w = np.load(f)
		# path_to_data/dataset/test1/dataset-ho.npy
		with open(op.join(base_path, 'test1', self.dataset + '-ho.npy'), 'r') as f:
			test_ho_w = np.load(f)

		return train_w, test_obs_w, test_ho_w

	def load_vocab(self):
		# type: int
		# path_to_data/dataset/clean/dataset.vocab
		with open(op.join(self.path_to_data, self.dataset, 'clean', self.dataset + '.vocab'), 'r') as f:
			vocab = {}
			for idx, line in enumerate(f):
				vocab[idx] = line.strip()
			V = idx + 1

		assert len(vocab.keys()) == V

		return vocab, V

	def init_model_hypers(self, model_settings, data):
		# type: Dict[str, object] -> List[(str, object)] -> List[(str, object)]
		w = data[0][1]
		D = len(w)
		N = np.zeros(D, dtype=np.int32)
		for i in range(0, D):
			N[i] = len(w[i])

		K = model_settings[KEY_NUMTOPICS]
		alpha = np.full(K, model_settings[KEY_ALPHA])		
		_, V = self.load_vocab()
		beta = np.full(V, model_settings[KEY_BETA])
		
		return [ ('K', K), ('D', D), ('N', N), ('alpha', alpha), ('beta', beta) ]

	def mk_init_mcmc_pt(self, hypers):
		# type: List[(str, object)] -> Dict[str, object]
		raise NotImplementedError


#-------------------------------------------------------------
# Experiment code

def predictive_ll(hypers, phi, thetas, w_test_ho):
	hypers_p = dict(hypers)
	K = hypers_p['K']
	alpha = hypers_p['alpha']
	beta = hypers_p['beta']
	V = beta.shape[0]

	S = len(thetas)
	D = len(w_test_ho)
	prev_ll = 0.0
	pred_ll = np.zeros(S)
	tmp = np.zeros(K)
	for s in range(S - 10, S):
		acc = 0.0
		theta = thetas[s]
		for d in range(0, D):
			for j in range(0, len(w_test_ho[d])):
				for k in range(0, K):
					tmp[k] = np.log(theta[d][k]) + np.log(phi[k][w_test_ho[d][j]])
				acc += sp.misc.logsumexp(tmp)
		prev_ll += acc
		pred_ll[s] = prev_ll / (s - (S - 10) + 1.0)
		print('pred_ll[%d]: %f, acc: %f, prev_ll: %f' % (s, pred_ll[s], acc, prev_ll))

	return pred_ll[S-1]

def infer_vocab_assign(lda_env, infer_sys, config, hypers, data, infer_settings, params):
	# type: LdaEnv -> InferSys -> Dict[(str, object)] -> List[(str, object)] -> List[(str, object)] -> InferSettings -> List[str] -> (double, double, Dict[(str, object)])
	"""
	"""
	# Create model
	model = lda_env.select_model(infer_sys)
	
	# Run model
	compile_time = infer_sys.compile(model, hypers, data, config)
	sample_time, samples = infer_sys.run(infer_settings, params)

	# Save phi samples for infer_topic_assign
	save_np_arr(lda_env.mk_tmp_dir(infer_sys), 'phi', samples['phi'])

	return compile_time, sample_time, samples

def infer_topic_assign(lda_env, infer_sys, hypers_phi_fixed, data, infer_settings, params, hypers, test_ho_w):
	# Load vocab assignments
	phi = load_np_arr(lda_env.mk_tmp_dir(infer_sys), 'phi')

	# Compute log-predictive likelihood
	config = { 'target':'cpu', 'sched':'ConjGibbs [theta] (*) DiscGibbs [z]' }
	pred_ll = np.zeros(phi.shape[0])
	for i in range(0, phi.shape[0]):
		infer_sys = AugurSys()
		hypers_phi_fixed[4] = ('phi', phi[i])
		infer_sys.compile(model_phi_fixed, hypers_phi_fixed, data, config)

		_, samples = infer_sys.run(infer_settings, params)

		pred_ll[i] = predictive_ll(hypers, phi[i], samples['theta'], test_ho_w)

	return pred_ll

def invoke_experiment(infer_sys, config, path_to_data, dataset, model_settings, infer_settings_train, infer_settings_pred, base_path_to_sbox, trial):
	# Create environment
	lda_env = LdaEnv(models, path_to_data, base_path_to_sbox, dataset, trial)
	train_w, test_obs_w, test_ho_w = lda_env.load_dataset()

	# Train LDA
	data = [ ('w', train_w) ]
	hypers = lda_env.init_model_hypers(model_settings, data)
	params = [ 'theta', 'phi', 'z' ]
	compile_time, sample_time, samples = infer_vocab_assign(lda_env, infer_sys, config, hypers, data, infer_settings_train, params)

	# Test LDA
	data = [ ('w', test_obs_w) ]
	hypers_phi_fixed = lda_env.init_model_hypers(model_settings, data)
	params = [ 'theta', 'z' ]
	pred_ll = infer_topic_assign(lda_env, infer_sys, hypers_phi_fixed, data, infer_settings_pred, params, hypers, test_ho_w)
	
	# Save experiment
	lda_env.dump_pred_ll_stats(infer_sys, compile_time, sample_time, pred_ll)
