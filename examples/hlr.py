
# coding: utf-8

# In[1]:

from pyaugur.augurlib import AugurOpt, AugurInfer
import numpy as np
import scipy as sp
import scipy.stats as sps
import os.path

augur_hlr = '''(K : Int, N : Int, lam : Real, x : Vec (Vec Real)) => {
  param v ~ Exponential(lam) ;
  param b ~ Normal(0.0, v) ;
  param theta[k] ~ Normal(0.0, v)
      for k <- 0 until K ;
  data y[n] ~ Bernoulli( sigmoid(dotprod(theta, x[n]) + b) )
      for n <- 0 until N ;
}
'''

def run_hlr(K, N, lam, train_x, train_y, sched, burnin=0, num_samples=100):
    with AugurInfer('config.yml', augur_hlr) as infer_obj:
        # Compile
        augur_opt = AugurOpt(cached=False, target='cpu', paramScale=None)
        infer_obj.set_compile_opt(augur_opt)
        infer_obj.set_user_sched(sched)
        infer_obj.compile(K, N, lam, train_x)(train_y)
    
        # Run
        samples = infer_obj.samplen(burnIn=burnin, numSamples=num_samples)
        
        # Print last sample
        print samples['v'][num_samples-1]
        print samples['b'][num_samples-1]
        print samples['theta'][num_samples-1]
        
        return samples

def load_dataset(datapath, dataset):
    with open(os.path.join(datapath, dataset, 'clean', dataset + '.attrib.norm.npy'), 'r') as f:
        train_x = np.load(f)
    with open(os.path.join(datapath, dataset, 'clean', dataset + '.class.npy'), 'r') as f:
        train_y = np.load(f)
    print train_x.shape
    K = train_x.shape[1]
    N = len(train_y)
    lam = 1.0

    return K, N, lam, train_x, train_y

datapath = '/path/to/data'
dataset = 'german'
K, N, lam, train_x, train_y = load_dataset(datapath, dataset)    

sched1 = 'HMC [v] [0.5, 0.05] (*) HMC [b] [0.5, 0.05] (*) HMC [theta] [0.5, 0.05]'
sched2 = 'MWG [v] ~ Normal(v, 1.0) (*) MWG [b] ~ Normal(b, 1.0) (*) HMC [theta] [0.5, 0.05]'

num_samples = 100
samples = run_hlr(K, N, lam, train_x, train_y, sched1, num_samples=num_samples)

