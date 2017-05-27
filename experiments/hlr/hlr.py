import os.path as op
import numpy as np
import scipy as sp
import scipy.stats

from util.infersys import *
from util.modelenv import *

#-------------------------------------------------------------
# Models

augur_hlr = '''
(K : Int, N : Int, lam : Real, x : Vec (Vec Real)) => {
  param v ~ Exponential(lam) ;
  param b ~ Normal(0.0, v) ;
  param theta[k] ~ Normal(0.0, v)
      for k <- 0 until K ;
  data y[n] ~ Bernoulli( sigmoid(dotprod(theta, x[n]) + b) )
      for n <- 0 until N ;
}
'''

jags_hlr = '''
model {
  v ~ dexp(lam)
  b ~ dnorm(0, 1 / v)
  for (k in 1:K) {
    theta[k] ~ dnorm(0, 1 / v)
  }
  for (n in 1:N) {
    y[n] ~ dbern(ilogit(inprod(x[n,], theta) + b))
  }
}
'''

stan_hlr = '''
data {
  int        K;
  int        N;
  real       lam;
  vector[K]  x[N];
  int        y[N];
}
parameters {
  real       b;
  real       v;
  vector[K]  theta;
}
model {
  v ~ exponential(lam);
  b ~ normal(0, sqrt(v));
  for (k in 1:K)
	theta[k] ~ normal(0, sqrt(v));
  for (n in 1:N)
	y[n] ~ bernoulli(inv_logit(dot_product(x[n], theta) + b));
}
'''

hlr_models = { 'augur': augur_hlr, 'jags': jags_hlr, 'stan': stan_hlr }

MODEL_SETTINGS = { 'lam': 1.0 }

#-------------------------------------------------------------
# hlr Environment

class HlrEnv(ModelEnv):
	def __init__(self, models, path_to_data, base_path_to_sbox, dataset, trial):
		super(HlrEnv, self).__init__(models, path_to_data, base_path_to_sbox, dataset, trial)

	def load_dataset(self):
		# type: tuple[?]
		with open(op.join(self.path_to_data, self.dataset, 'clean', self.dataset + '.attrib.norm.npy'), 'r') as f:
			train_x = np.load(f)
		with open(op.join(self.path_to_data, self.dataset, 'clean', self.dataset + '.class.npy'), 'r') as f:
			train_y = np.load(f)

		return train_x, train_y

	def init_model_hypers(self, model_settings, data):
		# type: Dict[str, object] -> List[(str, object)] -> List[(str, object)]
		data_dict = dict(data)
		train_x = data_dict['x']
		train_y = data_dict['y']
		K = train_x.shape[1]
		N = len(train_y)
		
		lam = model_settings['lam']

		return [ ('K', K), ('N', N), ('lam', lam), ('x', train_x) ]

	def mk_init_mcmc_pt(self, hypers):
		# Unpack hypers
		K, N, lam = tuple(map(lambda x: x[1], hypers))
		
		# Make initial point
		init = {}
		v = sp.stats.expon.rvs(lam)
		init['v'] = v
		init['b'] = sp.stats.norm.rvs(0.0, np.sqrt(v))
		init['theta'] = sp.stats.norm.rvs(0.0, np.sqrt(v), size=K)
		
		# Save
		self.save_init_mcmc_pt(init)
			
		return init

#-------------------------------------------------------------
# Experiment code

def flatten_jags(samples):
	if samples.shape[0] == 1:
		tmp = np.zeros(samples.shape[1])
		for i in range(0, samples.shape[1]):
			tmp[i] = samples[0][i][0]
		return tmp
	else:
		tmp = np.zeros((samples.shape[0], samples.shape[1]))
		for i in range(0, samples.shape[0]):			
			for j in range(0, samples.shape[1]):
				tmp[i][j] = samples[i][j][0]
		return tmp

def train(hlr_env, infer_sys, config, hypers, data, infer_settings, params):
	# Create model
	model = hlr_env.select_model(infer_sys)
	
	# Run model
	compile_time = infer_sys.compile(model, hypers, data, config)
	sample_time, samples = infer_sys.run(infer_settings, params)

	if isinstance(infer_sys, JagsSys):
		samples2 = {}
		samples2['v'] = samples['v']
		samples2['b'] = samples['b']
		samples2['theta'] = np.transpose(np.array(flatten_jags(samples['theta'])))
		return compile_time, sample_time, samples2

	return compile_time, sample_time, samples

def invoke_experiment(infer_sys, config, path_to_data, dataset, model_settings, infer_settings_train, base_path_to_sbox, trial):
	# Create environment
	hlr_env = HlrEnv(hlr_models, path_to_data, base_path_to_sbox, dataset, trial)
	train_x, train_y = hlr_env.load_dataset()

	# Train hlr (data/hyperparameter is a little bit weird because we don't have distribution on x)
	hypers = hlr_env.init_model_hypers(model_settings, [ ('x', train_x), ('y', train_y) ])
	params = [ 'v', 'b', 'theta' ]
	if infer_settings_train.load_init:
		init_pt = hlr_env.try_load_init_mcmc_pt(hypers, params)
	else:
		init_pt = hlr_env.mk_init_mcmc_pt(hypers)
	data = [ ('y', train_y) ]
	compile_time, sample_time, samples = train(hlr_env, infer_sys, config, hypers, data, infer_settings_train, params)

	print sample_time


"""
class ModelXface(object):
	def __init__(self):
		pass

	def mk_parser(self, subparsers):
		raise NotImplementedError

	def invoke(self, args):
		raise NotImplementedError

class HlrXface(ModelXface):
	def __init__(self):
		super(HlrXface, self).__init__()

	def mk_parser(self, subparsers):
		parser = subparsers.add_parser('hlr')
		parser.set_defaults(func=invoke_hlr)
		append_parse_infer_settings(parser)
		append_parse_system_settings(parser)

		# Hlr data
		parser.add_argument('--datapath', default='/home/dehuang/Documents/research/datasets/classification', type=str)
		parser.add_argument('--dataset', default='german', type=str)

		# Hlr model settings
		parser.add_argument('--lam', default=1.0, type=float)
		
		# Hlr inference settings
		parser.add_argument('--sched', default='HMC [v] (*) HMC [b] (*) HMC [theta]', type=str)

	def invoke(self, args):
		# Setup system and inference settings
		infer_sys, config = parse_system_settings(args)
		infer_settings_train = parse_infer_settings(args)

		# Setup data and experiment sandbox
		path_to_data = args.datapath
		dataset = args.dataset
		base_path_to_sbox = './hlr'
		trial = args.trial
		model_settings = { 'lam': args.lam }
		
		# Run experiment
		invoke_experiment(infer_sys, config, path_to_data, dataset, model_settings, infer_settings_train, base_path_to_sbox, trial)
"""