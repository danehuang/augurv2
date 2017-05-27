import os
import os.path as op

from util.timer import *

from pyaugur.augurlib import AugurOpt, AugurInfer
import pyjags
import pystan


class InferSys(object):
	def __init__(self):
		self._config = None
		self._infer_obj = None

		# Changes with compile
		self.model = None
		self.hypers = None
		self.data = None

	def compile(self, model, hypers, data, config=None):
		# type: List[(str, object)] -> List[(str, object)] -> Dict[(str, object)] -> unit
		raise NotImplementedError

	def run(self, infer_settings, params):
		# type: InferSettings -> List[str] -> (Time, [samples])
		raise NotImplementedError

	def emit(self):
		# type: str
		raise NotImplementedError

	def _update_state(self, infer_obj, config, model, hypers, data):
		# Update system state
		self._config = config
		self._infer_obj = infer_obj

		# Store latest value of model and hyper-parameters/data
		self.model = model
		self.hypers = hypers
		self.data = data


class AugurSys(InferSys):
	def __init__(self):
		super(AugurSys, self).__init__()

	def compile(self, model, hypers, data, config=None):
		with AugurInfer('config.yml', model) as infer_obj:
			# Augur only needs values
			hypers_p = map(lambda kv: kv[1], hypers)
			data_p = map(lambda kv: kv[1], data)

			# Compile
			augur_opt = AugurOpt(cached=False, target=config['target'], paramScale=None)
			infer_obj.set_compile_opt(augur_opt)
			infer_obj.set_user_sched(config['sched'])

			#infer_obj.compile(*hypers_p)(*data_p)
			def time_augur():
				infer_obj.compile(*hypers_p)(*data_p)
			stats_file = op.join('./', 'augur-compile.stats')
			cProfile.runctx('time_augur()', globals(), locals(), stats_file)
			compile_time = get_time_sys(stats_file, 'time_augur')

		self._update_state(infer_obj, config, model, hypers, data)
				
		return compile_time

	def run(self, infer_settings, params):
		assert self._infer_obj is not None
		assert isinstance(infer_settings, InferSettings)

		burnin = infer_settings.burnin
		num_samples = infer_settings.num_samples
		thin = infer_settings.thin
		with Timer() as timer:
			samples = self._infer_obj.samplen(burnIn=burnin, numSamples=num_samples, thin=thin)
		print('Drawing %d samples with %d burnin took %.03f sec' % (num_samples, burnin, timer.interval))

		return timer.interval, samples

	def emit(self):
		#return os.path.join('augur', self.config.emit())
		return 'augur'


class JagsSys(InferSys):
	def __init__(self):
		super(JagsSys, self).__init__()

	def compile(self, model, hypers, data, config=None):
		model_data = dict(hypers + data)
		#infer_obj = pyjags.Model(model, data=model_data, chains=1, adapt=0)

		def time_jags():
			return pyjags.Model(model, data=model_data, chains=1, adapt=0)
		stats_file = op.join('./', 'jags-compile.stats')
		cProfile.runctx('result = time_jags()', globals(), locals(), stats_file)
		infer_obj = locals()['result']
		compile_time = get_time_sys(stats_file, 'time_jags')

		self._update_state(infer_obj, config, model, hypers, data)

		return compile_time

	def run(self, infer_settings, params):
		assert self._infer_obj is not None
		assert isinstance(infer_settings, InferSettings)

		burnin = infer_settings.burnin
		num_samples = infer_settings.num_samples
		thin = infer_settings.thin
		with Timer() as timer:
			samples = self._infer_obj.sample(burnin + num_samples, vars=params)
		print('Drawing %d samples with %d burnin took %.03f sec' % (num_samples, burnin, timer.interval))

		return timer.interval, samples

	def emit(self):
		return 'jags'


class StanSys(InferSys):
	def __init__(self):
		super(StanSys, self).__init__()

	def compile(self, model, hypers, data, config=None):
		#infer_obj = pystan.StanModel(model_code=model, verbose=True)

		def time_stan():
			return pystan.StanModel(model_code=model, verbose=True)
		stats_file = op.join('./', 'stan-compile.stats')
		cProfile.runctx('result = time_stan()', globals(), locals(), stats_file)
		infer_obj = locals()['result']
		compile_time = get_time_sys(stats_file, 'time_stan')

		self._update_state(infer_obj, config, model, hypers, data)

		return compile_time

	def run(self, infer_settings, params):
		assert self._infer_obj is not None
		assert isinstance(infer_settings, InferSettings)
		
		burnin = infer_settings.burnin
		num_samples = infer_settings.num_samples
		thin = infer_settings.thin
		model_data = dict(self.hypers + self.data)
		with Timer() as timer:
			samples = self._infer_obj.sampling(data=model_data, warmup=burnin, iter=burnin + num_samples, chains=1)
		print('Drawing %d samples with %d burnin took %.03f sec' % (num_samples, burnin, timer.interval))

		return timer.interval, samples

	def emit(self):
		return 'stan'


class InferSettings(object):
	def __init__(self, load_init, num_samples, burnin=0, thin=1):
		assert type(load_init) is bool
		assert num_samples >= 1
		assert burnin >= 0
		assert thin >= 1

		self.load_init = load_init
		self.num_samples = num_samples
		self.burnin = burnin
		self.thin = thin

