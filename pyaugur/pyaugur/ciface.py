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

"""
[Note] Contains interface between Python objects and Augur's
native representation of objects. 

"""


#-------------------------------------------------------------
# Ctypes

class AugurTyp:
    AUGUR_INT = 0
    AUGUR_DBL = 1
    AUGUR_VEC = 2
    AUGUR_MAT = 3

class AugurVec(Structure):
    _fields_ = [ ("ty", c_int)
               , ("stride", c_uint)
               , ("elems", c_uint)
               , ("data", c_void_p) ]

class AugurMat(Structure):
    _fields_ = [ ("ty", c_int)
               , ("row", c_uint)
               , ("col", c_uint)
               , ("data", c_void_p) ]

class AugurBlk(Structure):
    _fields_ = [ ("num_blks", c_uint)
               , ("typs", POINTER(c_int))
               , ("blks", POINTER(c_void_p))
               , ("base_elems", c_uint)
               , ("base_data", c_double) ]


#-------------------------------------------------------------
# Types and operations

class PyAugurTy():
    def __init__(self):
        pass
    
class PyAugurIntTy(PyAugurTy):
    def __init__(self):
        pass
    
    def __str__(self):
        return 'Int'

class PyAugurRealTy(PyAugurTy):
    def __init__(self):
        pass
    
    def __str__(self):
        return 'Real'
    
class PyAugurVecTy(PyAugurTy):
    def __init__(self, ty):
        self.ty = ty
        
    def __str__(self):
        return 'Vec(' + str(self.ty) + ')'

class PyAugurMatTy(PyAugurTy):
    def __init__(self, dim, ty):
        self.dim = dim
        self.ty = ty

    def __str__(self):
        return 'Mat(' + str(self.dim) + ', ' + str(self.ty) + ')'

class PyAugurBlkTy(PyAugurTy):
    def __init__(self, tys):
        self.tys = tys

    def __str__(self):
        return 'Blk(' + str(self.tys) + ')'



#-------------------------------------------------------------
# Types and operations

def isInt(ty):
    return ty == np.int32 or ty == int

def isFlt(ty):
    return ty == np.float64 or ty == np.float32 or ty == float

def isArr(ty):
    return ty == np.ndarray

def isMat(ty):
    return ty == np.matrix

def isList(ty):
    return ty == list

def isTuple(ty):
    return ty == tuple 


#
# Converts a PyAugurTy into the corresponding ctype 
# pyTy : PyAugurTy 
# promoteBaseTy : Bool
#
def pyTyToCtype(pyTy, promoteBaseTy=False):
    if isinstance(pyTy, PyAugurIntTy):
        if promoteBaseTy:
            return POINTER(c_int)
        else:
            return c_int
    elif isinstance(pyTy, PyAugurRealTy):
        if promoteBaseTy:
            return POINTER(c_double)
        else:
            return c_double
    elif isinstance(pyTy, PyAugurVecTy):
        return AugurVec
    elif isinstance(pyTy, PyAugurMatTy):
        return AugurMat
    elif isinstance(pyTy, PyAugurBlkTy):
        return AugurBlk
    else:
        raise NameError('@pyTyToCType | Type ' + str(pyTy) + ' not supported')


#
# Converts a PyAugurTy into the corresponding native C enum 
# pyTy : PyAugurTy 
#
def pyTyToAugurTyp(pyTy):
    if isinstance(pyTy, PyAugurIntTy):
        return AugurTyp.AUGUR_INT
    elif isinstance(pyTy, PyAugurRealTy):
        return AugurTyp.AUGUR_DBL
    elif isinstance(pyTy, PyAugurVecTy):
        return AugurTyp.AUGUR_VEC
    elif isinstance(pyTy, PyAugurMatTy):
        return AugurTyp.AUGUR_MAT
    else:
        raise NameError('@pyTyToAugurTyp | Type ' + str(ty) + ' not supported')


def pyMatToCMat(pyTy, pyMat):
    assert isinstance(pyTy, PyAugurMatTy)
    assert len(pyMat.shape) == 2

    (row, col) = pyMat.shape

    mat = AugurMat()
    mat.ty = pyTyToAugurTyp(pyTy.ty)
    mat.row = row
    mat.col = col
    mat.data = cast(pyMat.ctypes.data, c_void_p)

    return mat


def chkByRefCVal(pyTy, cVal):
    if isinstance(pyTy, PyAugurIntTy):
        return pointer(cVal)
    elif isinstance(pyTy, PyAugurRealTy):
        return pointer(cVal)
    else:
        return cVal

def pyValToCVal(pyTy, pyVal):
    if isinstance(pyTy, PyAugurIntTy):
        assert isInt(type(pyVal))
        return c_int(pyVal)

    elif isinstance(pyTy, PyAugurRealTy):
        assert isFlt(type(pyVal))
        return c_double(pyVal)

    elif isinstance(pyTy, PyAugurVecTy):
        assert isArr(type(pyVal))

        avec = AugurVec()
        avec.ty = pyTyToAugurTyp(pyTy.ty)
        avec.stride = 0
        elems = pyVal.shape[0]
        avec.elems = elems

        if isinstance(pyTy.ty, PyAugurIntTy) or isinstance(pyTy.ty, PyAugurRealTy):
            avec.data = cast(pyVal.ctypes.data, c_void_p)
        elif isinstance(pyTy.ty, PyAugurVecTy):
            avec_arr = (AugurVec * elems)()
            avec.data = cast(avec_arr, c_void_p)
            for i in range(0, elems):
                avec_arr[i] = pyValToCVal(pyTy.ty, pyVal[i])
        elif isinstance(pyTy.ty, PyAugurMatTy):
            avec_mat = (AugurMat * elems)()
            avec.data = cast(avec_mat, c_void_p)
            for i in range(0, elems):
                avec_mat[i] = pyMatToCMat(pyTy.ty, pyVal[i])
        else:
            raise NameError('@pyValToCVal | Type: ' + str(pyTy) + ' not supported for object ' + str(pyVal))
        return avec

    elif isinstance(pyTy, PyAugurMatTy):
        return pyMatToCMat(pyTy, pyVal)

    elif isinstance(pyTy, PyAugurBlkTy):
        raise NameError('@pyValToCVal | TODO')

    else:
        raise NameError('@pyValToCVal | Type ' + str(ty) + ' not supported')





