import argparse
from ctypes import *
import os.path as op
import subprocess
import tempfile
import shutil
import yaml
import sys


augurIfaceHbegin = """#ifndef AUGUR_IFACE_H
#define AUGUR_IFACE_H
#include <gsl/gsl_rng.h>
#include <augur_vecop.h>
#include <augur_dist.h>


#define THREADS        256
#define BLOCKS(x)      ((int) ceil(((double) x) / THRDS))


"""

augurIfaceHend = """
#endif
"""

augurIfaceHdr = """#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "augur_iface.h"
#include <augur_vecop.h>
#include <augur_matop.h>
#include <augur_dist.h>
#include <augur_rtmem.h>
#include <augur_mcmclib.h>
"""

def logmsg(str):
	print str

class TestAugurCompiler():
	def __init__(self, path_config):
		with open(path_config, 'r') as file_yml:
			config = yaml.load(file_yml)
			
			self.os = config['os']
			assert config['os'] == 'osx' or config['os'] == 'linux'
			self.workdir = config['workingdir']
			self.incdir = config['incdir']
			self.libdir = config['libdir']
			self.augurdir = config['augurdir']
			self.libaugurdir = config['libaugurdir']

			self.failed = []
			self.compiler = self.__load_compiler()

	def __load_compiler(self):
		#path = op.join(self.libaugurdir, 'libHSaugur-0.1.0.0-ghc7.8.4.dylib')
		if self.os == 'osx':
			path = op.join(self.libaugurdir, 'libHSaugur-0.1.0.0-BRXl6V4Cbnj3aW4oo2NE22-ghc8.0.2.dylib')
		elif self.os == 'linux':
			path = op.join(self.libaugurdir, 'libHSaugur-0.1.0.0-BRXl6V4Cbnj3aW4oo2NE22-ghc8.0.2.so')
		lib_hs_augur_compiler = cdll.LoadLibrary(path)

		compiler = getattr(lib_hs_augur_compiler, 'hs_compile')
		compiler.argtypes = [ c_char_p, c_int, c_char_p, c_int, c_char_p ]
		compiler.restype = c_char_p

		return compiler

	def get_failed(self):
		return self.failed

	def load_and_compile(self, test_case_name, f_model, f_inference, target='cpu', user_mode='user'):
		assert target == 'cpu' or target == 'gpu'
		assert user_mode == 'auto' or user_mode == 'user'

		with open(f_model) as f:
			model = f.read()
		logmsg('Model:\n' + model)

		with open(f_inference) as f:
			inference = f.read()
		logmsg('Inference:\n' + inference)

		try:
			self._compile(model, inference, target, user_mode)

			return True
		except:
			print("Unexpected error:", sys.exc_info()[0])
			self.failed += [ test_case_name ]
			return False

	def _compile(self, model, inference, target, user_mode):
		workdir = tempfile.mkdtemp(prefix='augur', dir=self.workdir)
		LIBAUGUR_IFACE = op.join(workdir, 'libaugur_iface.dylib')

		# Invoke Augur Compiler
		if target == 'cpu':
			target_ = 0       # CPU
		elif target == 'gpu':
			target_ = 1       # GPU
		if user_mode == 'auto':
			user_mode_ = 0    # auto set
		elif user_mode == 'user':
			user_mode_ = 1    # have user schedule
		rtsizes = ''     # no runtime sizes 
		result = self.compiler(model, target_, inference, user_mode_, rtsizes)

		# Parse results
		arr = result.split('$-SEP-$')
		res = arr[0]
		if res == "ERROR":
			raise NameError('Error: ' + str(arr[1]))
		mk_modparams = arr[1]
		paramtys = arr[2]
		c_inferhdr = arr[3]
		c_infercode = arr[4]

		# Emit inference header
		f_hdr = op.join(self.workdir, 'augur_iface.h')
		with open(f_hdr, 'w') as f:
			f.write(augurIfaceHbegin)
			f.write(c_inferhdr)
			f.write(augurIfaceHend)

		# Emit inference code
		if target == 'cpu':
			AUGUR_IFACE_C = 'augur_iface.c'
		elif target == 'gpu':
			AUGUR_IFACE_C = 'augur_iface.cu'
		f_infer = op.join(self.workdir, AUGUR_IFACE_C) 
		with open(f_infer, 'w') as f:
			f.write(augurIfaceHdr)
			f.write(c_infercode)

		# Compile code to shared library
		logmsg('Compiling...')
		inc_path = [ '-I' + self.augurdir, '-I' + self.workdir ] + map(lambda x: '-I' + x, self.incdir)
		lib_path = [ '-L' + self.augurdir ] + map(lambda x: '-L' + x, self.libdir)
		path_flags =  inc_path + lib_path

		if target == 'cpu':
			comp_flags = [ '-O3', '-shared', '-fPIC', '-DAUGURCPU', '-o', LIBAUGUR_IFACE, f_infer ]
			extra_flags = [ '-lgsl', '-Wl,-rpath,' + self.augurdir, '-laugur_util_cpu' ]
			cmd = [ 'clang' ] + path_flags + comp_flags + extra_flags
			logmsg(' '.join(cmd))
			exit_code = subprocess.check_call(cmd)
			logmsg('Done with exit code: ' + repr(exit_code))
		elif target == 'gpu':
			lib_path = [ '-L' + op.join(self.augurdir, 'gpu') ] + map(lambda x: '-L' + x, self.libdir)
			path_flags = inc_path + lib_path

			AUGUR_IFACE_OBJ = op.join(self.workdir, 'augur_iface.o')
			LIBAUGUR_IFACE_OBJ = op.join(self.workdir, 'libaugur_iface.o')
			augur_lib = [ 'augur_blkop.o', 'augur_dist.o', 'augur_math.o', 'augur_matop.o', 'augur_rtmem.o', 'augur_util.o', 'augur_rtval.o', 'augur_vecop.o' ]
			augur_lib_p = map(lambda obj_file: op.join(self.augurdir, 'gpu', obj_file), augur_lib)
			logmsg('augur_gpu: ' + str(augur_lib_p))

			# Compile inference code to object code
			comp_flags = [ '-arch=sm_35', '-O3', '-Xcompiler', '-fPIC', '-dc', '-o', AUGUR_IFACE_OBJ, f_infer ]
			cmd = [ 'nvcc' ] + path_flags + comp_flags
			logmsg(' '.join(cmd))
			exit_code = subprocess.check_call(cmd)
			logmsg('Done with exit code: ' + repr(exit_code))

			# Link augur lirbary object code
			comp_flags = [ '-arch=sm_35', '-Xcompiler', '-fPIC', '-dlink', '-o', LIBAUGUR_IFACE_OBJ, AUGUR_IFACE_OBJ ] + augur_lib_p
			cmd = [ 'nvcc' ] + path_flags + comp_flags
			logmsg(' '.join(cmd))
			exit_code = subprocess.check_call(cmd)
			logmsg('Done with exit code: ' + repr(exit_code))

			# Link and create shared library
			comp_flags = [ '-arch=sm_35', '-Xcompiler', '-fPIC', '--shared', '-o', LIBAUGUR_IFACE, LIBAUGUR_IFACE_OBJ, AUGUR_IFACE_OBJ ] + augur_lib_p
			cmd = [ 'nvcc' ] + path_flags + comp_flags + [ '-lgsl', '-lgslcblas', '-lm' ]
			logmsg(' '.join(cmd))
			exit_code = subprocess.check_call(cmd)
			logmsg('Done with exit code: ' + repr(exit_code))

		if workdir is not None:
			shutil.rmtree(workdir)


if __name__ == "__main__":
	parser = argparse.ArgumentParser()

	parser.add_argument('--model', default='all')
	parser.add_argument('--target', default='cpu')

	tester = TestAugurCompiler('config.yml')

	parser.add_argument('--dims', default=2)
	args = parser.parse_args()
	target = args.target

	if args.model == 'hlr' or args.model == 'all':
		tester.load_and_compile('hlr-auto', 'test/hlr.rv', 'test/hlr1.infer', target=target, user_mode='auto')
		tester.load_and_compile('hlr1', 'test/hlr.rv', 'test/hlr1.infer', target=target)
		tester.load_and_compile('hlr2', 'test/hlr.rv', 'test/hlr2.infer', target=target)
	
	if args.model == 'dethlr' or args.model == 'all':
		tester.load_and_compile('dethlr-auto', 'test/dethlr.rv', 'test/hlr1.infer', target=target, user_mode='auto')
		tester.load_and_compile('dethlr1', 'test/dethlr.rv', 'test/hlr1.infer', target=target)
		tester.load_and_compile('dethlr2', 'test/dethlr.rv', 'test/hlr2.infer', target=target)

	if args.model == 'mvgmm' or args.model == 'all':
		tester.load_and_compile('mvgmm-auto', 'test/mvgmm.rv', 'test/mvgmm1.infer', target=target, user_mode='auto')
		tester.load_and_compile('mvgmm1', 'test/mvgmm.rv', 'test/mvgmm1.infer', target=target)
		tester.load_and_compile('mvgmm2', 'test/mvgmm.rv', 'test/mvgmm2.infer', target=target)
		tester.load_and_compile('mvgmm3', 'test/mvgmm.rv', 'test/mvgmm3.infer', target=target)
		tester.load_and_compile('mvgmm3', 'test/mvgmm.rv', 'test/mvgmm4.infer', target=target)

	if args.model == 'hmvgmm' or args.model == 'all':
		tester.load_and_compile('hmvgmm-auto', 'test/hmvgmm.rv', 'test/hmvgmm1.infer', target=target, user_mode='auto')
		tester.load_and_compile('hmvgmm1', 'test/hmvgmm.rv', 'test/hmvgmm1.infer', target=target)
		tester.load_and_compile('hmvgmm2', 'test/hmvgmm.rv', 'test/hmvgmm2.infer', target=target)
		tester.load_and_compile('hmvgmm3', 'test/hmvgmm.rv', 'test/hmvgmm3.infer', target=target)

	if args.model == 'lda' or args.model == 'all':
		tester.load_and_compile('lda-auto', 'test/lda.rv', 'test/lda1.infer', target=target, user_mode='auto')
		tester.load_and_compile('lda', 'test/lda.rv', 'test/lda1.infer', target=target)


	print 'Failed test cases: ' + str(tester.get_failed())

