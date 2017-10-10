# Example

AugurV2 provides a simple modeling language for expressing statistical
models. All you can do in AugurV2 is describe a sequence of random
variables and their distributions. The hope is that you can translate
a model found in a statistics textbook into AugurV2 without too much
effort. Below, we give an example of a Gaussian Mixture Model (GMM)
encoded in AugurV2.
```
(K : Int, N : Int, mu0 : Vec Real, covs0 : Mat Real, pis : Vec Real, covs : Mat Real) => {
  param mu[k] ~ MvNormal(mu0, covs0)
      for k <- 0 until K ;
  param z[n] ~ Categorical(pis)
      for n <- 0 until N ;
  data y[n] ~ MvNormal(mu[z[n]], covs)
      for n <- 0 until N ;
}
```

We should not think of a the modeling language as a traditional
programming language, but as a syntax for writing down standard
statistical models. Hence, AugurV2 is a pure language. Indeed, there
are several differences from a traditional programming language:
1. The for syntax indicates a parallel comprehension so this is not a traditional for loop, which expresses sequential dependencies.
2. The comprehension bounds must be constant.

The semantics of an AugurV2 model is a collection of random
variables. The random variables marked `param` are inferred and the
random variables marked `data` are conditioned on. You can use the
Python interface to obtain samples from the posterior distribution
induced by the collection of random variables.
```
from pyaugur.augurlib import AugurOpt, AugurInfer
import numpy as np
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
```

Now, we can invoke the GMM and see how the points cluster on a synthetic dataset:
```
import numpy as np
import matplotlib.pyplot as plt

# See end of example for mk_synthetic_dataset_k3_d2
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
```

There are multiple schedules we can use. In the example above, we used
`sched1`. Try running the above with different schedules to try out
different kernels.


See `examples/mvgmm.py` for the full example.


Appendix:
```
def mk_synthetic_dataset_k3_d2(N):
	import scipy as sp
	import scipy.stats as sps
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
            y.append(sps.multivariate_normal.rvs(mean=mu[0], cov=covs))
        elif n % K == 1:
            y.append(sps.multivariate_normal.rvs(mean=mu[1], cov=covs))
        else:
            y.append(sps.multivariate_normal.rvs(mean=mu[2], cov=covs))
    train_y = np.array(y)
    
    return K, D, N, mu0, covs0, pis, covs, train_y
```

# Language

## Modeling Language

Syntax:
```
prog ::= "(" hparams ")" "=>" "{" decls "}"
hparams ::= hparam | hparam "," hparams
hparam ::= id ":" typ

decls ::= decl | decl decls
decl ::= kind id "[" ids "]" "~" expr comps ";"
	   | kind id "~" expr ";"
	   | "param" id "[" ids "]" ":" typ "=" expr comps ";"
	   | "param" id ":" typ "=" expr ";"

kind ::= "param" | "data"

comps ::= comp | comp "," comps
comp ::= "for" id "<-" expr "until" expr

typ ::= "Int" | "Real" | "Vec" typ | "Mat" typ

exprs ::= expr | expr "," exprs
expr ::= id
       | i
       | r
	   | dist "(" exprs ")"
	   | uop expr
	   | expr bop expr
	   | expr "[" exprs "]"
	   
ids ::= id | id "," ids
id ::= (identifier)
i ::= (integer literal)
r ::= (real literal)
dist ::= (see distribution)
uop ::= (see primitive operations)
bop ::= (see primitive operations)
```

Distributions currently supported are:
- `Bernoulli(bias)`
  * `bias: Real`
- `Categorical(pmf)`
  * `pmf: Vec Real`
- `Poisson(rate)`
  * `rate: Real`
- `Beta(alpha, beta)`
  * `alpha: Real`
  * `beta: Real`
- `Exponential(rate)`
  * `rate: Real`
- `Gamma(shape, rate)`
  * `shape: Real`
  * `rate: Real`
- `InvGamma(shape, scale)`
  * `shape: Real`
  * `scale: Real`
- `Normal(mean, variance)`
  * `mean: Real`
  * `variance: Real`
- `Uniform(lower, upper)`
  * `lower: Real`
  * `upper: Real`
- `Dirichlet(alpha)`
  * `alpha: Vec Real`
- `MvNormal(mean, covariance)`
  * `mean: Vec Real`
  * `covariance: Mat Real`
- `InvWishart(degrees_freedom, scale)`
  * `degrees_freedom: Int`
  * `scale: Mat Real`

Primitive operations currently support are:
- `+`
- `*`
- `-`
- `/`
- `expon`
- `log`
- `logit`


## Kernel Language

Syntax:
```
kern ::= base | kern (*) kern

base ::= HMC [xs] [r, r]
       | RSlice [xs] [r, r]
       | NUTS [xs] [r]
       | MWG [x] ~ expr
       | ConjGibbs [x]
       | DiscGibbs [x]
       | ESlice [x]
```

HMC:
```
HMC [xs] [r1, r2]
```
1. `r1` is the simulation length
2. `r2` is the step size
WARNING: these parameters need to be set appropriately to ensure that the simulation is stable.


Reflective slice sampling:
```
RSlice [xs] [r1, r2]
```
1. `r1` is the simulation length
2. `r2` is the step size
WARNING: these parameters need to be set appropriately to ensure that the simulation is stable.


No U-Turn sampling:
```
NUTS [xs] [r]
```
1. `r` is the step size
WARNING: `r` needs to be set appropriately to ensure that the simulation is stable.


Gibbs sampling with conjugacy relation:
```
ConjGibbs [x]
```
AugurV2 will throw an error if it cannot detect a conjugacy relation involving `x`.


Gibbs sampling with discrete marginalization:
```
DiscGibbs [x]
```
AugurV2 will throw an error if `x` is not discrete.


Elliptical slice sampling:
```
ESlice [x]
```
AugurV2 will throw an error if `x` is not Gaussian.


## Usage

### Schedule / Model mismatch

When specifying your own kernel, you must make sure to sample all
parameters specified in the model. I'm planning to add static-checking
for this.

### Sampling deterministic variables

If your model contains determinsitic transformations of random
variables, you should not sample them. I'm planning to add
static-checking for this. For example, if your model looks like
```
...
theta[n] : Real = alpha[n] * beta[n]
   for n <- 0 until N ;
...
```
then your kernel schedule **should not** sample `theta`. More concretely,
```
... (*) NUTS [theta] [0.05] (*) ...
```
is **wrong**. AugurV2 will automatically update these parameters for you.


## Python Interoperability

Currently, AugurV2 works with Python 2 and interoperates with numpy.

| AugurV2 type       | numpy type                          |
|--------------------+-------------------------------------|
| `Int`              | `np.int32`                          |
| `Real`             | `np.double`                         |
| `Vec t`            | `np.array(dtype=t')`, t' = trans(t) |
| `Mat t`, t base    | `np.mat(dtype=t')`, t' = trans(t)   |


WARNING: 
If your model uses discrete distributions, please make sure that the
`dtype` field of a numpy array is set to `np.int32`. Otherwise, you
will get weird results. I'm hoping to fix this by improving the static
type-checking. For example, if your model contains
```
...
data y[n] ~ Categorical(pi)
    for n <- 0 until N ;
...
```
Then, the Python representation should look something like
```
...
y = np.array([1, 2, 3, 4], dtype=np.int32)
...
```
You only need to worry about this for model data or hyper-parameters.


# Known Issues

- If your model uses discrete distributions, please make sure that the `dtype` field of a numpy array is set to `np.int32`. Otherwise, you will get weird results. I'm hoping to fix this by improving the static type-checking.
- Deterministic parameters should not be sampled by kernels. I'm hoping to fix this by improving the static type-checking.


# Roadmap

1. Improve static checking.
2. Make it possible to add user-defined primitives and distributions.
3. Add sequential comprehensions.
4. Add support for controlled C/C++ interoperability.
5. Add tensorflow support?
