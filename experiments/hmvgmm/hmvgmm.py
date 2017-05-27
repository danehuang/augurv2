import os.path as op
import numpy as np
import scipy as sp
import scipy.stats

from util.infersys import *
from util.modelenv import *

#-------------------------------------------------------------
# Models

augur_hmvgmm = '''
(K : Int, N : Int, D : Int, alpha : Vec Real, mu0 : Vec Real, covs0 : Mat Real, df : Int, scale0 : Mat Real) => {
  param pis ~ Dirichlet(alpha) ;
  param mu[k] ~ MvNormal(mu0, covs0)
      for k <- 0 until K ;
  param cov[k] ~ InvWishart(df, scale0)
      for k <- 0 until K ;
  param z[n] ~ Categorical(pis)
      for n <- 0 until N ;
  data y[n] ~ MvNormal(mu[z[n]], cov[z[n]])
      for n <- 0 until N ;
}
'''

jags_hmvgmm = '''
model {
  pis ~ ddirch(alpha)
  for (k in 1:K) {
	mu[k,1:D] ~ dmnorm(mu0, covs0)
	cov[k,1:D,1:D] ~ dwish(scale0, df)
  }
  for (n in 1:N) {
	z[n] ~ dcat(pis)
	y[n,1:D] ~ dmnorm(mu[z[n],1:D], cov[z[n],1:D,1:D])
  }
}
'''

stan_hmvgmm = '''
data {
  int K;
  int N;
  int D;
  vector[D] mu0;
  cov_matrix[D] covs0;
  real df;
  cov_matrix[D] scale0;
  simplex[K] alpha;
  vector[D] y[N];
}
parameters {
  simplex[K] pis;
  vector[D] mu[K];
  cov_matrix[D] cov[K];
}
model {
  pis ~ dirichlet(alpha);
  for (k in 1:K) {
    mu[k] ~ multi_normal(mu0, covs0);
    cov[k] ~ inv_wishart(df, scale0);
  }
  for (n in 1:N) {
    real marg[K];
    for (k in 1:K) {
      marg[k] = multi_normal_lpdf(y[n] | mu[k], cov[k]) + log(pis[k]);
    }
    target += log_sum_exp(marg);
  }
}
'''

hmvgmm_models = { 'augur': augur_hmvgmm, 'jags': jags_hmvgmm, 'stan': stan_hmvgmm }

MODEL_SETTINGS = { 'K': 3, 'D': 2, 'N': 1000 }

#-------------------------------------------------------------
# HMVGMM Environment

def model_settings_to_dataset(model_settings):
	K = model_settings['K']
	D = model_settings['D']
	N = model_settings['N']

	return 'k-' + str(K) + '-n-' + str(N) + '-d-' + str(D)

class HmvgmmEnv(ModelEnv):
	def __init__(self, models, path_to_data, base_path_to_sbox, dataset, trial):
		super(HmvgmmEnv, self).__init__(models, path_to_data, base_path_to_sbox, dataset, trial)

	def load_dataset(self):
		# type: tuple[?]
		with open(op.join(self.path_to_data, self.dataset, 'train1', 'y.npy'), 'r') as f:
			train_y = np.load(f)
		with open(op.join(self.path_to_data, self.dataset, 'test1', 'y.npy'), 'r') as f:
			test_y = np.load(f)
		
		return train_y, test_y

	def init_model_hypers(self, model_settings, data):
		# type: Dict[str, object] -> List[(str, object)] -> List[(str, object)]
		"""
		optional: model_settings['K'] = ?
		optional: model_settings['D'] = ?
		optional: model_settings['N'] = ?
		"""
		K = int(model_settings['K'])
		D = int(model_settings['D'])
		N = int(model_settings['N'])

		mu0 = np.zeros(D)
		cov0 = np.eye(D)
		df = D + 3
		scale0 = np.eye(D)
		alpha = np.full(K, 1.0 / K)

		return [ ('K', K), ('N', N), ('D', D), ('alpha', alpha), ('mu0', mu0), ('covs0', cov0), ('df', df), ('scale0', scale0) ]

	def mk_init_mcmc_pt(self, hypers):
		# Unpack hypers
		K, N, D, alpha, mu0, covs0, df, scale0 = tuple(map(lambda x: x[1], hypers))
		
		# Make initial point
		init = {}
		init['pis'] = sp.stats.dirichlet.rvs(alpha)[0]
		init['mu'] = sp.stats.multivariate_normal.rvs(mu0, covs0, size=K)
		init['cov'] = sp.stats.invwishart.rvs(df, scale0, size=K)
		init['z'] = np.zeros(N)
		pii = np.full(len(alpha), 1.0 / len(alpha))
		for n in range(0, N):
			cutoffs = np.cumsum(pii)
			init['z'][n] = cutoffs.searchsorted(np.random.uniform(0, cutoffs[-1]))

		# Save
		self.save_init_mcmc_pt(init)
			
		return init


#-------------------------------------------------------------
# Experiment code

def predictive_ll(hypers, pis, mus, covs, test_y, init_pt, S):
	# type: List[(str, object)] -> np.array -> np.array -> np.array -> np.array -> McmcSt -> int -> np.array
	"""
	1/S sum_{s=1}^S (sum_{k=1}^K p(test_y | mus[s][k], sigma2s[s][k]))
	"""
	# Unpack hypers
	K, _, _, alpha, mu0, covs0, df, scale0 = tuple(map(lambda x: x[1], hypers))
	N = len(test_y)

	# sum_{k=1}^K p(test_y | mus[s][k], sigma2s[s][k])
	def inner_ll(z_marg_pdf, tmp, pis, mu, cov):
		acc = 0.0
		for k in range(0, K):
			z_marg_pdf[k] = np.log(pis[k])
		for n in range(0, N):
			for k in range(0, K):
				tmp[k] = sp.stats.multivariate_normal.logpdf(test_y[n], mu[k], cov[k]) + z_marg_pdf[k]
			acc += sp.misc.logsumexp(tmp)
		return acc

	tmp = np.zeros(K)
	z_marg_pdf = np.zeros(K)

	# Compute initial point
	init_ll = inner_ll(z_marg_pdf, tmp, init_pt['pis'], init_pt['mu'], init_pt['cov'])
	print('pred_ll[init]: %f' % (init_ll))

	# Do the rest
	prev_ll = 0.0
	pred_ll = np.zeros(S)
	for s in range(0, S):
		prev_ll += inner_ll(z_marg_pdf, tmp, pis[s], mus[s], covs[s])
		pred_ll[s] = prev_ll / (s + 1.0)
		print('pred_ll[%d]: %f, prev_ll: %f' % (s, pred_ll[s], prev_ll))

	return np.append([init_ll], pred_ll)

def flatten_jags_pii(piis):
	K, N, _ = piis.shape
	pi2 = np.zeros((N, K))
	for k in range(0, K):
		for n in range(0, N):
			pi2[n][k] = piis[k][n]
	return pi2

def flatten_jags_mu(mus):
	K, D, N, _ = mus.shape
	mu2 = np.zeros((N, K, D))
	for k in range(0, K):
		for d in range(0, D):
			for n in range(0, N):
				mu2[n][k][d] = mus[k][d][n]
	return mu2

def flatten_jags_sigma(invsigmas):
	K, D, _, N, _ = invsigmas.shape
	sigma2 = np.zeros((N, K, D, D))
	for k in range(0, K):
		for d1 in range(0, D):
			for d2 in range(0, D):
				for n in range(0, N):
					sigma2[n][k][d1][d2] = invsigmas[k][d1][d2][n]
	for n in range(0, N):
		for k in range(0, K):
			sigma2[n][k] = np.linalg.inv(sigma2[n][k])
	return sigma2

def train(hmvgmm_env, infer_sys, config, hypers, data, infer_settings, params):
	# Create model
	model = hmvgmm_env.select_model(infer_sys)
	
	# Run model
	compile_time = infer_sys.compile(model, hypers, data, config)
	sample_time, samples = infer_sys.run(infer_settings, params)

	if isinstance(infer_sys, JagsSys):
		samples2 = {}
		samples2['pis'] = flatten_jags_pii(samples['pis'])
		samples2['mu'] = flatten_jags_mu(samples['mu'])
		samples2['cov'] = flatten_jags_sigma(samples['cov'])
		return compile_time, sample_time, samples2

	return compile_time, sample_time, samples


def invoke_experiment(infer_sys, config, path_to_data, model_settings, infer_settings_train, num_pred_samples, base_path_to_sbox, trial):
	# Create environment
	dataset = model_settings_to_dataset(model_settings)
	hmvgmm_env = HmvgmmEnv(hmvgmm_models, path_to_data, base_path_to_sbox, dataset, trial)
	train_y, test_y = hmvgmm_env.load_dataset()
	print model_settings['N'], type( model_settings['N'])
	print len(train_y), type (len(train_y))
	print model_settings['N'] is len(train_y)
	assert int(model_settings['N']) == len(train_y)

	# Train Hmvgmm
	data = [ ('y', train_y) ]
	hypers = hmvgmm_env.init_model_hypers(model_settings, train_y)
	params = [ 'pis', 'mu', 'cov', 'z' ]
	if infer_settings_train.load_init:
		init_pt = hmvgmm_env.try_load_init_mcmc_pt(hypers, params)
	else:
		init_pt = hmvgmm_env.mk_init_mcmc_pt(hypers)
	compile_time, sample_time, samples = train(hmvgmm_env, infer_sys, config, hypers, data, infer_settings_train, params)

	# Test HmvGmm
	pred_ll = predictive_ll(hypers, samples['pis'], samples['mu'], samples['cov'], test_y, init_pt, num_pred_samples)
	
	# Save experiment
	hmvgmm_env.dump_pred_ll_stats(infer_sys, compile_time, sample_time, pred_ll)

