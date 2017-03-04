
# coding: utf-8

# In[1]:

from pyaugur.augurlib import AugurOpt, AugurInfer
import numpy as np
import scipy as sp
import scipy.stats as sps

augur_lda = '''(K : Int, D : Int, N : Vec Int, alpha : Vec Real, beta : Vec Real) => {
  param theta[d] ~ Dirichlet(alpha)
      for d <- 0 until D ;
  param phi[k] ~ Dirichlet(beta)
      for k <- 0 until K ;
  param z[d, n] ~ Categorical(theta[d])
      for d <- 0 until D, n <- 0 until N[d] ;
  data w[d, n] ~ Categorical(phi[z[d, n]])
      for d <- 0 until D, n <- 0 until N[d] ;
}
'''

def run_lda(K, D, N, alpha, beta, train_w, sched, burnin=0, num_samples=100):
    with AugurInfer('config.yml', augur_lda) as infer_obj:
        # Compile
        augur_opt = AugurOpt(cached=False, target='cpu', paramScale=None)
        infer_obj.set_compile_opt(augur_opt)
        infer_obj.set_user_sched(sched)
        infer_obj.compile(K, D, N, alpha, beta)(train_w)
    
        # Run
        samples = infer_obj.samplen(burnIn=burnin, numSamples=num_samples)
        
        # Print last sample
        print samples['theta'][num_samples-1]
        print samples['phi'][num_samples-1]
        print samples['z'][num_samples-1]
        
        return samples
    
def mk_synthetic_dataset_d4_k4_v8():
    D = 4; K = 4; V = 8
    N = np.array([10, 10, 10, 10], dtype=np.int32)
    
    alpha = np.full(K, 0.1)
    beta = np.full(V, 0.01)
    print 'alpha', alpha
    print 'beta', beta

    # Intended phi
    # K = 0   [0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    # K = 1   [0.0, 0.0, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0]
    # K = 2   [0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0.0]
    # K = 3   [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5]

    # z1 = [0, 1, 0, 1, 0, 1, 0, 1]
    w1 = np.array([0, 2, 1, 3, 0, 2, 1, 3, 0, 2], dtype=np.int32)

    # z2 = [2, 3, 2, 3, 2, 3, 2, 3]
    w2 = np.array([4, 6, 5, 7, 4, 6, 5, 7, 5, 7], dtype=np.int32)

    # z3 = [1, 2, 1, 2, 1, 2, 1, 2]
    w3 = np.array([2, 4, 3, 5, 2, 4, 3, 5, 2, 4], dtype=np.int32)

    # z4 = [0, 3, 0, 3, 0, 3, 0, 3]
    w4 = np.array([0, 6, 1, 7, 0, 6, 1, 7, 1, 7], dtype=np.int32)

    w = np.array([w1, w2, w3, w4])

    return K, V, D, N, alpha, beta, w
    
K, V, D, N, alpha, beta, w = mk_synthetic_dataset_d4_k4_v8() 
sched1 = 'ConjGibbs [theta] (*) ConjGibbs [phi] (*) DiscGibbs [z]'

num_samples=5
samples = run_lda(K, D, N, alpha, beta, w, sched1, num_samples=num_samples)


# In[ ]:



