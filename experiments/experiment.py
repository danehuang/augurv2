import argparse

from util.infersys import *
import hlr.hlr as hlr
import hmvgmm.hmvgmm as hmvgmm
import lda.lda as lda


#-------------------------------------------------------------
# Parsing helper

def append_parse_infer_settings(parser):
	parser.add_argument('--burnin', default=50, type=int)
	parser.add_argument('--numsamples', default=100, type=int)
	parser.add_argument('--numpredsamples', default=20, type=int)
	parser.add_argument('--thin', default=1, type=int)
	parser.add_argument('--loadinit', default=True, type=bool)

def parse_infer_settings(args):
	num_samples = args.numsamples
	burnin = args.burnin
	thin = args.thin
	load_init = args.loadinit
	infer_settings = InferSettings(load_init, num_samples, burnin, thin)

	return infer_settings


def append_parse_system_settings(parser):
	parser.add_argument('--system', default='augur', choices=['augur', 'jags', 'stan', 'all'])
	parser.add_argument('--trial', default=1, type=int)
	parser.add_argument('--target', default='cpu', type=str)

def parse_system_settings(args):
	# Setup system
	if args.system == 'augur':
		infer_sys = AugurSys()
		config = { 'target': args.target, 'sched': args.sched }
	elif args.system == 'jags':
		infer_sys = JagsSys()
		config = None
	elif args.system == 'stan':
		infer_sys = StanSys()
		config = None

	return infer_sys, config


def append_model_settings(model_settings, parser):
	for key, val in model_settings.iteritems():
		parser.add_argument('--' + key, default=val)

def parse_model_settings(model_settings, args):
	model_settings_p = {}
	for key, val in model_settings.iteritems():
		model_settings_p[key] = getattr(args, key)

	return model_settings_p


#-------------------------------------------------------------
# Parse Hlr

def invoke_hlr(args):
	# Setup system and inference settings
	infer_sys, config = parse_system_settings(args)
	infer_settings_train = parse_infer_settings(args)

	# Setup data and experiment sandbox
	path_to_data = args.datapath
	dataset = args.dataset
	base_path_to_sbox = './hlr'
	trial = args.trial
	model_settings = parse_model_settings(hlr.MODEL_SETTINGS, args)
	
	# Run experiment
	hlr.invoke_experiment(infer_sys, config, path_to_data, dataset, model_settings, infer_settings_train, base_path_to_sbox, trial)

def mk_parse_hlr(subparsers):
	parser = subparsers.add_parser('hlr')
	parser.set_defaults(func=invoke_hlr)
	append_parse_infer_settings(parser)
	append_parse_system_settings(parser)

	# Hlr data
	parser.add_argument('--datapath', type=str)
	parser.add_argument('--dataset', default='german', type=str)

	# Hlr model settings
	append_model_settings(hlr.MODEL_SETTINGS, parser)
	
	# Hlr inference settings
	parser.add_argument('--sched', default='HMC [v] (*) HMC [b] (*) HMC [theta]', type=str)


#-------------------------------------------------------------
# Parse Hmvgmm

def invoke_hmvgmm(args):
	# Setup system and inference settings
	infer_sys, config = parse_system_settings(args)
	infer_settings_train = parse_infer_settings(args)
	num_pred_samples = args.numpredsamples

	# Setup data and experiment sandbox
	path_to_data = args.datapath
	base_path_to_sbox = './hmvgmm'
	trial = args.trial
	model_settings = parse_model_settings(hmvgmm.MODEL_SETTINGS, args)
	
	# Run experiment
	hmvgmm.invoke_experiment(infer_sys, config, path_to_data, model_settings, infer_settings_train, num_pred_samples, base_path_to_sbox, trial)

def mk_parse_hmvgmm(subparsers):
	parser = subparsers.add_parser('hmvgmm')
	parser.set_defaults(func=invoke_hmvgmm)
	append_parse_infer_settings(parser)
	append_parse_system_settings(parser)

	# Hmvgmm data
	parser.add_argument('--datapath', type=str)

	# Hmvmm model settings
	append_model_settings(hmvgmm.MODEL_SETTINGS, parser)

	# Hmvgmm inference settings
	parser.add_argument('--sched', default='ConjGibbs [pis] (*) ConjGibbs [mu] (*) ConjGibbs [cov] (*) DiscGibbs [z]', type=str)


#-------------------------------------------------------------
# Parse Lda

def invoke_lda(args):
	# Setup system and inference settings
	infer_sys, config = parse_system_settings(args)
	infer_settings_train = parse_infer_settings(args)
	infer_settings_pred = InferSettings(infer_settings_train.load_init, args.numpredsamples, infer_settings_train.burnin, infer_settings_train.thin)

	# Setup data and experiment sandbox
	path_to_data = args.datapath
	dataset = args.dataset
	base_path_to_sbox = './lda'
	trial = args.trial
	model_settings = parse_model_settings(lda.MODEL_SETTINGS, args)
	
	# Run experiment
	lda.invoke_experiment(infer_sys, config, path_to_data, dataset, model_settings, infer_settings_train, infer_settings_pred, base_path_to_sbox, trial)

def mk_parse_lda(subparsers):
	parser = subparsers.add_parser('lda')
	parser.set_defaults(func=invoke_lda)
	append_parse_infer_settings(parser)
	append_parse_system_settings(parser)

	# LDA data
	parser.add_argument('--datapath', type=str)
	parser.add_argument('--dataset', default='kos', type=str)

	# LDA model settings
	append_model_settings(lda.MODEL_SETTINGS, parser)

	# LDA inference settings
	parser.add_argument('--sched', default='ConjGibbs [theta] (*) ConjGibbs [phi] (*) DiscGibbs [z]', type=str)


#-------------------------------------------------------------
# Parse everything

def parse_args():
	parser = argparse.ArgumentParser(description='Experiments comparing AugurV2, Jags, and Stan.')
	subparsers = parser.add_subparsers()

	mk_parse_hlr(subparsers)
	mk_parse_hmvgmm(subparsers)
	mk_parse_lda(subparsers)

	return parser.parse_args()


#-------------------------------------------------------------
# Main

if __name__ == "__main__":
	args = parse_args()
	args.func(args)
