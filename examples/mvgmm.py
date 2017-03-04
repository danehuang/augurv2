
# coding: utf-8

# In[4]:

get_ipython().magic(u'matplotlib inline')
from pyaugur.augurlib import AugurOpt, AugurInfer
import numpy as np
import scipy as sp
import scipy.stats as sps
import matplotlib.pyplot as plt


augur_mvgmm = '''
(K : Int, N : Int, mu0 : Vec Real, covs0 : Mat Real, pis : Vec Real, covs : Mat Real) => {
  param mu[k] ~ MvNormal(mu0, covs0)
      for k <- 0 until K ;
  param z[n] ~ Categorical(pis)
      for n <- 0 until N ;
  data y[n] ~ MvNormal(mu[z[n]], covs)
      for n <- 0 until N ;
}
'''

def run_mvgmm(K, D, N, mu0, covs0, pis, covs, train_y, sched, burnin=0, num_samples=100):
    with AugurInfer('config.yml', augur_mvgmm) as infer_obj:
        # Compile
        augur_opt = AugurOpt(cached=False, target='cpu', paramScale=None)
        infer_obj.set_compile_opt(augur_opt)
        infer_obj.set_user_sched(sched)
        infer_obj.compile(K, N, mu0, covs0, pis, covs)(train_y)
    
        # Run
        samples = infer_obj.samplen(burnIn=burnin, numSamples=num_samples)
        
        return samples
    
    
def mk_synthetic_dataset_k3_d2(N):
    # Hyper-parameters
    K = 3; D = 2
    mu0 = np.zeros(D); covs0 = np.eye(D); pis = np.full(K, 1.0 / K); 
    covs = np.matrix([[0.1, 0.0], [0.0, 0.1]])

    # Cluster centers
    mu = [np.array([0.0, 2.0]), np.array([2.0, -2.0]), np.array([-2.0, -2.0])]

    # Data
    y = []
    for n in range(0, N):
        if n % K == 0:
            #y.append(mu[0])
            y.append(sps.multivariate_normal.rvs(mean=mu[0], cov=covs))
        elif n % K == 1:
            #y.append(mu[1])
            y.append(sps.multivariate_normal.rvs(mean=mu[1], cov=covs))
        else:
            #y.append(mu[2])
            y.append(sps.multivariate_normal.rvs(mean=mu[2], cov=covs))
    train_y = np.array(y)
    
    return K, D, N, mu0, covs0, pis, covs, train_y

K, D, N, mu0, covs0, pis, covs, train_y = mk_synthetic_dataset_k3_d2(20)


sched1 = 'ConjGibbs [mu] (*) DiscGibbs [z]'
sched2 = 'ESlice [mu] (*) DiscGibbs [z]'
sched3 = 'HMC [mu] [0.2, 0.01] (*) DiscGibbs [z]'
sched4 = 'MWG [mu] ~ MvNormal(mu[k], covs0) for k <- 0 until K (*) DiscGibbs [z]'

num_samples = 20
samples = run_mvgmm(K, D, N, mu0, covs0, pis, covs, train_y, sched1, num_samples=num_samples)

plt.scatter(train_y[:,0], train_y[:,1])
mus = samples['mu'][num_samples-1]
plt.plot(mus[0][0], mus[0][1], "o")
plt.plot(mus[1][0], mus[1][1], "o")
plt.plot(mus[2][0], mus[2][1], "o")


# In[ ]:



