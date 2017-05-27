"""
Copyright 2017 Daniel Eachern Huang

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""

from ctypes import *
import numpy as np
import subprocess as subp
import yaml
import os.path as op
import tempfile
import shutil
from copy import deepcopy
import time

import ciface as ci
from augurdistlib import *


#----------------------------------------------------------
# Augur options

class AugurOpt:
    # cached: for debugging purposes
    # target: cpu or gpu
    # initStrat: initialization strategy
    # paramScale: dictionary of variances
    def __init__(self, cached=False, target='cpu', initStrat=AugurRandom(), paramScale=None):
        self.cached = cached
        
        assert (target == 'cpu' or target == 'gpu')
        self.target = target
        
        assert (isinstance(initStrat, AugurInitStrat))
        self.initStrat = initStrat

        self.paramScale = paramScale


def logmsg(str):
    print str

def modParam2Dict(modParam):
    tmp = {}
    for p in modParam:
        if ci.isList(type(p)):
            for p2 in p:
                tmp[p2[0]] = p2[1]
        else:
            tmp[p[0]] = p[1]
    return tmp

def dict2ModParam(modParam, dic):
    res = []
    for p in modParam:
        if ci.isList(type(p)):
            tmp = []
            for p2 in p:
                tmp.append((p2[0], dic[p2[0]]))
            res.append(tmp)
        else:
            res.append((p[0], dic[p[0]]))
    return res


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


#----------------------------------------------------------
# Augur infererence

"""
For now, every param can only be mentioned once
(name_1, param_1), ..., (name_n, param_n)       (Blocks are implicit)
"""

class AugurInfer:
    def __init__(self, path_config, model):
        self.model = model

        with open(path_config, 'r') as file_yml:
            config = yaml.load(file_yml)
            
            assert (config['os'] == 'osx' or config['os'] == 'linux')
            self.os = config['os']

            self.workdir = config['workingdir']
            self.incdir = config['incdir']
            self.libdir = config['libdir']
            self.augurdir = config['augurdir']
            self.libaugurdir = config['libaugurdir']

        self.tmpdir = None
        self.aopt = None
        self.inferLib = None
        self.userSched = None

        self.mapSamp = None
        self.mapLL = float("-inf")

    def __enter__(self):
        return self
    
    def __exit__(self, exc_type, exc_value, traceback):
        if self.tmpdir is not None:
            shutil.rmtree(self.tmpdir)


    #------------------------------
    # Public interface

    def set_compile_opt(self, aopt):
        self.aopt = aopt
        return self

    def set_user_sched(self, userSched):
        self.userSched = userSched
        return self

    def set_pt(self, pt):
        for param in self.param_names:
            assert param in pt
        self._call_augur_set_pt(pt)

    def compile(self, *hypers):
        def _compile(*data):
            if self.aopt is None:
                raise NameError('Set compiler options first...')

            # == Invoke model compiler
            (mk_modparams, paramtys, c_inferhdr, c_infercode) = self._hs_augur_compile(*hypers)

            # == Invoke native compiler
            self._native_augur_compile(c_inferhdr, c_infercode)
            
            # == Invoke native compiler
            self._setup_py_xface(paramtys, mk_modparams)

            # == Transfer data
            self._call_augur_init(data, *hypers)

        return _compile

    def curr_ll(self):
        assert self.inferLib is not None

        return self._call_augur_curr_ll()

    def sample(self):
        assert self.inferLib is not None
        
        # Step and copy
        self._call_augur_step()
        ll, modParams = self._call_augur_cpy()
        logmsg('LL: ' + str(ll))
        return modParam2Dict(modParams)

    def samplen(self, burnIn=0, numSamples=1, thin=1, log_like=False):
        assert self.inferLib is not None

        # Burn in
        for i in range(0, burnIn):
            self._call_augur_step()

        # Initialize sample map
        samples = {}
        for name in self.param_names:
            samples[name] = []

        for i in range(0, numSamples * thin):
            self._call_augur_step()
            
            # Copy sample back
            if i % thin == 0:
                if i % 100 == 0:
                    logmsg('Doing iter: ' + str(i) + ' / ' + str(numSamples))
                ll, modParams = self._call_augur_cpy()
                if log_like:
                    print('log-like[%d]: %f' % (i / thin, self.curr_ll()))

                modParamsDict = modParam2Dict(modParams)
                for name in self.param_names:
                    samples[name].append(modParamsDict[name])

        return samples

    #------------------------------
    # Python / CTypes helper 

    # Convert base values to pointer values
    def _wrap_modparams(self, initStrat, *args):
        modparams = self.mk_modparams(initStrat, *args)
        return modparams

    # Convert pointer to base values back to base values
    def _unwrap_modparams(self, c_args, modparams):
        for i, ((name, param), pyTy) in enumerate(zip(modparams, self.paramTys)):
            if isinstance(pyTy, ci.PyAugurIntTy):
                modparams[i] = (name, c_args[i].value)
            elif isinstance(pyTy, ci.PyAugurRealTy):
                modparams[i] = (name, c_args[i].value)

        return modparams

    #------------------------------
    # Compile helper

    def _parse_modhypers(self, *hypers):
        # Store hyper-parameters for future use
        self.hypers = hypers    # used to be args

        # Get hyper-parameter sizes 
        sizes = []
        for hyper in hypers:
            if ci.isInt(type(hyper)):
                sizes.append(hyper)
            elif ci.isFlt(type(hyper)):
                sizes.append(-1)
            elif type(hyper) == np.ndarray:
                sizes.append(hyper.shape[0])
            else:
                sizes.append(-1)

        return ','.join(map(str, sizes))

    def _hs_augur_compile(self, *hypers):
        # Load augur compiler as dynamic library
        logmsg('Loading Augur compiler...')
        if self.os == 'osx':
            #lib_hs_augur_compiler = cdll.LoadLibrary(op.join(self.libaugurdir, 'libHSaugur-0.1.0.0-ghc7.8.4.dylib'))
            lib_hs_augur_compiler = cdll.LoadLibrary(op.join(self.libaugurdir, 'libHSaugur-0.1.0.0-BRXl6V4Cbnj3aW4oo2NE22-ghc8.0.2.dylib'))
            logmsg('Successfully loaded ' + repr(lib_hs_augur_compiler) + ' as osx dynamic library')
        elif self.os == 'linux':
            lib_hs_augur_compiler = cdll.LoadLibrary(op.join(self.libaugurdir, 'libHSaugur-0.1.0.0-BRXl6V4Cbnj3aW4oo2NE22-ghc8.0.2.so'))
            logmsg('Successfully loaded ' + repr(lib_hs_augur_compiler) + ' as linux dynamic library')
        
        # [ model, target, infer, mode, sizes ] -> String
        compiler = getattr(lib_hs_augur_compiler, 'hs_compile')
        compiler.argtypes = [ c_char_p, c_int, c_char_p, c_int, c_char_p ]
        compiler.restype = c_char_p            
        
        # Prepare compiler arguments
        if self.aopt.target == 'cpu':
            target = 0
        else:
            target = 1
        if self.userSched is None:
            userMode = 0
            userSched = ''
        else:
            userMode = 1
            userSched = self.userSched
        rtsizes = self._parse_modhypers(*hypers)

        # Invoke the compiler
        logmsg('Compiling model...')
        logmsg('RtSizes... ' + str(rtsizes))
        result = compiler(self.model, target, userSched, userMode, rtsizes)

        # Parse the results
        arr = result.split('$-SEP-$')
        res = arr[0]
        if res == "ERROR":
            raise NameError('Error: ' + str(arr[1]))
        mk_modparams = arr[1]
        paramtys = arr[2]
        #print paramtys
        c_inferhdr = arr[3]
        c_infercode = arr[4]

        return (mk_modparams, paramtys, c_inferhdr, c_infercode)

    def _native_augur_compile(self, c_inferhdr, c_infercode):
        # Set file name for inference code
        if self.aopt.target == 'cpu':
            f_infer = op.join(self.workdir, 'augur_iface.c') 
        elif self.aopt.target == 'gpu':
            f_infer = op.join(self.workdir, 'augur_iface.cu')

        # Note that we are writing the generated file to the workdir, not the tmpdir
        if not(self.aopt.cached):
            # Write c header to file
            f_hdr = op.join(self.workdir, 'augur_iface.h')
            logmsg('Overwriting... ' + f_hdr)
            with open(f_hdr, 'w') as f:
                f.write(augurIfaceHbegin)
                f.write(c_inferhdr)
                f.write(augurIfaceHend)

            # Write c inference code to file
            logmsg('Overwriting... ' + f_infer)
            with open(f_infer, 'w') as f:
                f.write(augurIfaceHdr)
                f.write(c_infercode)

        # Create temporary working directory for this particular model
        self.tmpdir = tempfile.mkdtemp(prefix='augur', dir=self.workdir)
        if self.os == 'osx':
            self.LIBAUGUR_IFACE = op.join(self.tmpdir, 'libaugur_iface.dylib')
        elif self.os == 'linux':
            self.LIBAUGUR_IFACE = op.join(self.tmpdir, 'libaugur_iface.so')
        logmsg('Generating inference library as: ' + self.LIBAUGUR_IFACE)


        # Compile code to shared library
        logmsg('Compiling...')
        inc_path = [ '-I' + self.augurdir, '-I' + self.workdir ] + map(lambda x: '-I' + x, self.incdir)
        lib_path = [ '-L' + self.augurdir ] + map(lambda x: '-L' + x, self.libdir)
        path_flags =  inc_path + lib_path

        if self.aopt.target == 'cpu':
            # Compile and link inference code
            comp_flags = [ '-O3', '-shared', '-fPIC', '-DAUGURCPU', '-o', self.LIBAUGUR_IFACE, f_infer ]
            extra_flags = [ '-lgsl', '-Wl,-rpath,' + self.augurdir, '-laugur_util_cpu' ]
            cmd = [ 'clang' ] + path_flags + comp_flags + extra_flags
            logmsg(' '.join(cmd))
            exit_code = subp.check_call(cmd)
            logmsg('Done with exit code: ' + repr(exit_code))

            # osx specific dynamic library path update
            if self.os == 'osx':
                cmd = [ 'install_name_tool', '-change', 'libaugur_util_cpu.dylib', '@rpath/libaugur_util_cpu.dylib', self.LIBAUGUR_IFACE ]
                exit_code = subp.check_call(cmd)
                logmsg('Done with library shenanigans: ' + repr(exit_code))

        elif self.aopt.target == 'gpu':
            lib_path = [ '-L' + op.join(self.augurdir, 'gpu') ] + map(lambda x: '-L' + x, self.libdir)
            path_flags = inc_path + lib_path

            AUGUR_IFACE_OBJ = op.join(self.workdir, 'augur_iface.o')
            LIBAUGUR_IFACE_OBJ = op.join(self.workdir, 'libaugur_iface.o')
            augur_lib = [ 'augur_blkop.o', 'augur_dist.o', 'augur_math.o', 'augur_matop.o', 'augur_rtmem.o', 'augur_util.o', 'augur_rtval.o', 'augur_vecop.o', 'augur_blkstk.o' ]
            augur_lib_p = map(lambda obj_file: op.join(self.augurdir, 'gpu', obj_file), augur_lib)
            logmsg('augur_gpu: ' + str(augur_lib_p))

            # Compile inference code to object code
            comp_flags = [ '-arch=sm_35', '-O3', '-Xcompiler', '-fPIC', '-dc', '-o', AUGUR_IFACE_OBJ, f_infer ]
            cmd = [ 'nvcc' ] + path_flags + comp_flags
            logmsg(' '.join(cmd))
            exit_code = subp.check_call(cmd)
            logmsg('Done with exit code: ' + repr(exit_code))

            # Link augur lirbary object code
            comp_flags = [ '-arch=sm_35', '-Xcompiler', '-fPIC', '-dlink', '-o', LIBAUGUR_IFACE_OBJ, AUGUR_IFACE_OBJ ] + augur_lib_p
            cmd = [ 'nvcc' ] + path_flags + comp_flags
            logmsg(' '.join(cmd))
            exit_code = subp.check_call(cmd)
            logmsg('Done with exit code: ' + repr(exit_code))

            # Link and create shared library
            comp_flags = [ '-arch=sm_35', '-Xcompiler', '-fPIC', '--shared', '-o', self.LIBAUGUR_IFACE, LIBAUGUR_IFACE_OBJ, AUGUR_IFACE_OBJ ] + augur_lib_p
            cmd = [ 'nvcc' ] + path_flags + comp_flags + [ '-lgsl', '-lgslcblas', '-lm' ]
            logmsg(' '.join(cmd))
            exit_code = subp.check_call(cmd)
            logmsg('Done with exit code: ' + repr(exit_code))

            """
            AUGUR_IFACE_OBJ = op.join(self.tmpdir, 'augur_iface.o')
            LIBAUGUR_IFACE_OBJ = op.join(self.tmpdir, 'libaugur_iface.o')

            # Compile inference code to object code
            comp_flags = [ '-O3 -arch=sm_35', '-Xcompiler', '-fPIC', '-dc', '-o', AUGUR_IFACE_OBJ, f_infer ]
            cmd = [ 'nvcc' ] + path_flags + comp_flags
            logmsg(' '.join(cmd))
            exit_code = subp.check_call(cmd)
            logmsg('Done with exit code: ' + repr(exit_code))

            # Link augur lirbary object code
            comp_flags = [ '-arch=sm_35', '-Xcompiler', '-fPIC', '-dlink', '-o', LIBAUGUR_IFACE_OBJ, AUGUR_IFACE_OBJ, op.join(self.augurdir, 'augur_veclib_gpu.o'), op.join(self.augurdir, 'augur_distlib_gpu.o') ]
            cmd = [ 'nvcc' ] + path_flags + comp_flags
            logmsg(' '.join(cmd))
            exit_code = subp.check_call(cmd)
            logmsg('Done with exit code: ' + repr(exit_code))

            # Link and create shared library
            comp_flags = [ '-arch=sm_35', '-Xcompiler', '-fPIC', '--shared', '-o', self.LIBAUGUR_IFACE, LIBAUGUR_IFACE_OBJ, AUGUR_IFACE_OBJ, op.join(self.augurdir, 'augur_veclib_gpu.o'), op.join(self.augurdir, 'augur_distlib_gpu.o') ]
            cmd = [ 'nvcc' ] + path_flags + comp_flags + [ '-lgsl', '-lgslcblas', '-lm' ]
            logmsg(' '.join(cmd))
            exit_code = subp.check_call(cmd)
            logmsg('Done with exit code: ' + repr(exit_code))
            """

        # Load inference code as shared library
        logmsg('Transferring data to native inference library...')
        self.inferLib = cdll.LoadLibrary(self.LIBAUGUR_IFACE)
        logmsg('Successfully loaded augur interface... ' + repr(self.inferLib))

    def _setup_py_xface(self, paramtys, mk_modparams):
        # Load python types
        hdr ="from pyaugur.ciface import*\n_paramAndDataTy = "
        exec(hdr + paramtys) in locals()
        
        self.hyperTys = map(lambda x: x[1], _paramAndDataTy[0])
        self.paramTys = map(lambda x: x[1], _paramAndDataTy[1])
        self.paramTyCtx = dict(_paramAndDataTy[1])
        self.dataTys = map(lambda x: x[1], _paramAndDataTy[2])

        # Load python inferace (called genMod)
        hdr = "from pyaugur.augurdistlib import*\nimport numpy as np\n"
        #print hdr + mk_modparams
        exec(hdr + mk_modparams) in locals()
        self.mk_modparams = genMod
    

    #------------------------------
    # Augur interface calls

    def _call_augur_init(self, data, *hypers):
        modParams = self._wrap_modparams(self.aopt.initStrat, *hypers)
        #print modParams

        self.param_names = []
        for name, _ in modParams:
            self.param_names.append(name)

        f = getattr(self.inferLib, 'augur_iface_init')
        f.restype = None
        f.argtypes = []
        
        c_args = []
        self.sampleArgs = []
        for (hyper, pyTy) in zip(hypers, self.hyperTys):
            f.argtypes.append(ci.pyTyToCtype(pyTy))
            c_args.append(ci.pyValToCVal(pyTy, hyper)) 
        for ((name, param), pyTy) in zip(modParams, self.paramTys):
            f.argtypes.append(ci.pyTyToCtype(pyTy, promoteBaseTy=True))
            #c_args.append(ci.pyValToCVal(pyTy, param))
            c_args.append(ci.chkByRefCVal(pyTy, ci.pyValToCVal(pyTy, param)))
            self.sampleArgs.append(ci.pyTyToCtype(pyTy, promoteBaseTy=True))
        for (data_, pyTy) in zip(data, self.dataTys):
            f.argtypes.append(ci.pyTyToCtype(pyTy))
            c_args.append(ci.pyValToCVal(pyTy, data_))
        logmsg('augur_iface_init types: ' + str(f.argtypes))
        
        # Initialize inference with c_args (includes hyperparams, params, and data)
        logmsg('Initializing and transferring data...')
        f(*c_args)
        logmsg('Done...')

        """
        # If default init
        if self.aopt.initpt != None:
            self._call_augur_set(modParams)
        """

    def _call_augur_cpy(self):
        modparams = self._wrap_modparams(AugurDefault(), *self.hypers)

        g = getattr(self.inferLib, 'augur_cpy')
        g.restype = c_double
        g.argtypes = self.sampleArgs
        c_args = []
        for ((name, param), pyTy) in zip(modparams, self.paramTys):
            obj = ci.pyValToCVal(pyTy, param)
            c_args.append(obj)
        ll = g(*c_args)

        foobar = self._unwrap_modparams(c_args, modparams)
        return (ll, foobar)

    def _call_augur_set_pt(self, pt):
        g = getattr(self.inferLib, 'augur_set_pt')
        g.restype = None
        g.argtypes = self.sampleArgs
        c_args = []
        for name in self.param_names:
            obj = ci.pyValToCVal(self.paramTyCtx[name], pt[name])
            c_args.append(obj)
        g(*c_args)

    def _call_augur_step(self):
        f = getattr(self.inferLib, 'augur_step')
        f.restype = c_double
        f.argtypes = []
        return f()


    def _call_augur_curr_ll(self):
        f = getattr(self.inferLib, 'augur_get_curr_ll')
        f.restype = c_double
        f.argtypes = []
        return f()
