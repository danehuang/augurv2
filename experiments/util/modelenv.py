import os
import os.path as op

from util.infersys import *
from util.logger import *

class ModelEnv(object):
	def __init__(self, models, path_to_data, base_path_to_sbox, dataset, trial):
		self.models = models
		self.path_to_data = path_to_data
		self.dataset = dataset

		# base_path_to_sbox/dataset/trial
		self.path_to_sbox = op.join(base_path_to_sbox, dataset, 'trial'+str(trial))

		# base_path_to_sbox/dataset/trial/init
		self.path_to_init = op.join(self.path_to_sbox, 'init')
		if not os.path.exists(self.path_to_init):
			os.makedirs(self.path_to_init)

	def load_dataset(self):
		# type: tuple[?]
		raise NotImplementedError

	def init_model_hypers(self, model_settings, data):
		# type: Dict[str, object] -> Dict[str, object] -> List[(str, object)], 
		raise NotImplementedError

	def mk_init_mcmc_pt(self, hypers):
		# type: List[(str, object)] -> Dict[str, object]
		raise NotImplementedError

	def select_model(self, infer_sys):
		# type: Dict[str, str] -> InferSys -> str
		if isinstance(infer_sys, AugurSys):
			return self.models['augur']
		elif isinstance(infer_sys, JagsSys):
			return self.models['jags']
		elif isinstance(infer_sys, StanSys):
			return self.models['stan']
		else:
			raise False

	def try_load_init_mcmc_pt(self, hypers, param_keys):
		# type: List[(str, object)] -> List[str] -> Dict[str, object]
		try:
			init_pt = self.load_init_mcmc_pt(param_keys)
			return init_pt
		except:
			init_pt = self.mk_init_mcmc_pt(hypers)
			self.save_init_mcmc_pt(init_pt)
			return init_pt

	def load_init_mcmc_pt(self, param_keys):
		# type: List[str] -> Dict[str, object]
		init_pt = {}
		for param_key in param_keys:
			init_pt[param_key] = load_np_arr(self.path_to_init, param_key)
		return init_pt

	def save_init_mcmc_pt(self, init_pt):
		# type: McmcSt -> ()
		for param_key in init_pt:
			save_np_arr(self.path_to_init, param_key, init_pt[param_key])

	def mk_tmp_dir(self, infer_sys):
		# base_path_to_sbox/dataset/trial/init
		path_to_tmp = op.join(self.path_to_sbox, infer_sys.emit(), 'tmp')
		if not os.path.exists(path_to_tmp):
			os.makedirs(path_to_tmp)
		return path_to_tmp

	def dump_pred_ll_stats(self, infer_sys, compile_time, sample_time, pred_ll):
		# type: InferSys -> double -> double -> List[Double] -> ()
		"""
		Dump log-predictive probability statistics to <path_to_stat> specified in model_env.
		"""
		# base_path_to_sbox/dataset/trial/init
		path_to_stat = op.join(self.path_to_sbox, infer_sys.emit(), 'stat')
		if not os.path.exists(path_to_stat):
			os.makedirs(path_to_stat)

		with open(op.join(path_to_stat, 'stats.yml'), 'w') as f:
			f.write('compile: ' + str(compile_time) + '\n')
			f.write('sample: ' + str(sample_time) + '\n')
			f.write('predll: ' + '[' + ', '.join(map(str, pred_ll)) + ']' + '\n')


class ModelSettings(object):
	def __init__(self, model_settings):
		self.model_settings = model_settings

	def append_model_settings_to_parser(self, parser):
		for key, val in self.model_settings:
			parser.add_argument('--' + key, default=val)