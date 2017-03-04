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

import numpy as np
import scipy as sp
import scipy.stats as sps
import scipy.special


class AugurInitStrat(object):
    pass

class AugurRandom(AugurInitStrat):
    def __init__(self):
        pass

class AugurDefault(AugurInitStrat):
    def __init__(self):
        pass

class AugurUser(AugurInitStrat):
    def __init__(self, initPt):
        self.initPt = initPt



def initArr(size, ty):
	if ty == 0:
		return np.ndarray(size, dtype=np.int32)
	elif ty == 1:
		return np.ndarray(size, dtype=np.float64)
	elif ty == 2:
		return np.ndarray(size, dtype=np.ndarray)
	elif ty == 3:
		return np.ndarray(size, dtype=np.matrix)
	else:
		raise NameError('Type ' + str(ty) + ' not supported')

def sizeBVec(vec):
	return vec.shape[0]

def logit(p):
	return np.log(p) - np.log(1.0 - p)

def initDirac(init_strat, e):
	return e


def initBernoulli(init_strat, bias):
	if isinstance(init_strat, AugurDefault):
		return 0
	elif isinstance(init_strat, AugurRandom):
		return sps.bernoulli(bias).rvs()
	else:
		raise False

def initCategorical(init_strat, pmf):
	if isinstance(init_strat, AugurDefault):
		return 0
	elif isinstance(init_strat, AugurRandom):
		cutoffs = np.cumsum(pmf)
		return cutoffs.searchsorted(np.random.uniform(0, cutoffs[-1]))
	else:
		raise False

def initGeometric(init_strat, bias):
	if isinstance(init_strat, AugurDefault):
		return 1
	elif isinstance(init_strat, AugurRandom):
		return sps.geom(bias).rvs()
	else:
		raise False

def initPoisson(init_strat, rate):
	if isinstance(init_strat, AugurDefault):
		return 1
	elif isinstance(init_strat, AugurRandom):
		return sps.poisson(rate).rvs()
	else:
		raise False

def initBeta(init_strat, a, b):
	if isinstance(init_strat, AugurDefault):
		return 1.0
	elif isinstance(init_strat, AugurRandom):
		return sps.beta(a, b).rvs()
	else:
		raise False

def initExponential(init_strat, rate):
	if isinstance(init_strat, AugurDefault):
		return 1.0
	elif isinstance(init_strat, AugurRandom):
		return sps.expon(rate).rvs()
	else:
		raise False

def initGamma(init_strat, a, b):
	if isinstance(init_strat, AugurDefault):
		return 0.5
	elif isinstance(init_strat, AugurRandom):
		return sps.gamma(a, b).rvs()
	else:
		raise False

def initInverseGamma(init_strat, shape, scale):
	if isinstance(init_strat, AugurDefault):
		return 0.5
	elif isinstance(init_strat, AugurRandom):
		return 1.0 / np.random.gamma(shape, 1.0 / scale)
	else:
		raise False

def initNormal(init_strat, mean, variance):
	if isinstance(init_strat, AugurDefault):
		return mean
	elif isinstance(init_strat, AugurRandom):
		return np.random.normal(mean, np.sqrt(variance))
	else:
		raise False

def initUniform(init_strat, lower, upper):
	if isinstance(init_strat, AugurDefault):
		return (lower + upper) / 2.0
	elif isinstance(init_strat, AugurRandom):
		return np.random.uniform(lower, upper)
	else:
		raise False

def initDirichlet(init_strat, alpha):
	if isinstance(init_strat, AugurDefault):
		size = len(alpha)
		arr = np.zeros(size)
		for i in range(0, size):
			arr[i] = 1.0 / size
		return arr
	elif isinstance(init_strat, AugurRandom):
		return np.random.dirichlet(alpha)
	else:
		raise False

def initMvNormal(init_strat, mu, cov):
	if isinstance(init_strat, AugurDefault):
		return np.zeros(len(mu), dtype=np.float64)
	elif isinstance(init_strat, AugurRandom):
		return sps.multivariate_normal.rvs(mean=mu, cov=cov)
	else:
		raise False

def initInvWishart(init_strat, df, scale):
	if isinstance(init_strat, AugurDefault):
		return np.eye(scale.shape[0], dtype=np.float64)
	elif isinstance(init_strat, AugurRandom):
		return sps.invwishart.rvs(df=df, scale=scale)
	else:
		raise False


def DotArg1_Bernoulli(pt, bias):
	return ((2.0 * pt) - 1.0) / (pt * bias + (1.0 - pt) * (1.0 - bias))

def DotPt_Exponential(pt, rate):
	return -rate

def DotArg1_Exponential(pt, rate):
	return 1.0 / rate - pt;

def DotPt_Normal(pt, mean, sigma2):
	return -(pt - mean) / sigma2

def DotArg1_Normal(pt, mean, sigma2):
	return (pt - mean) / sigma2

def DotArg2_Normal(pt, mean, sigma2):
	return (-1.0 / (2.0 * sigma2)) + ((pt - mean) * (pt - mean)) / (2.0 * sigma2 * sigma2)

def DotPt_sigmoid(x):
	return sp.special.expit(x) * (1.0 - sp.special.expit(x))    

def LL_Bernoulli(pt, bias):
	return sp.stats.bernoulli.logpmf(pt, bias)

def LL_Exponential(pt, rate):
	return np.log(rate) - rate * pt

def LL_Normal(pt, mean, sigma2):
	return sp.stats.norm.logpdf(pt, mean, np.sqrt(sigma2))


def augur_exp(x):
	return np.exp(x)

def augur_log(x):
	return np.log(x)

def augur_expit(x):
	return sp.special.expit(x)

def augur_logit(x):
	return sp.special.logit(x)

def augur_dotprod(x, y):
	return np.dot(x, y)

