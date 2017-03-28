# AugurV2

AugurV2 generates compositional MCMC inference algorithms for a simple probabilistic modeling language. 

### Dependencies

AugurV2 additional depends on:
* [GSL](https://www.gnu.org/software/gsl/) - GNU scientific library (tested with version 2.3)
* [clang](https://clang.llvm.org/) - C compiler
* [Cuda](https://developer.nvidia.com/cuda-75-downloads-archive) - Cuda Toolkit (tested with version 7.5)


### Installation


#### Compiler
Option 1: Build the AugurV2 compiler from source (using cabal).
```
$cd compiler/augur
$cabal sandbox init
$cabal install --dependencies-only
$cabal build
```

Option 2: Build the AugurV2 compiler from source (using [stack](https://docs.haskellstack.org/en/stable/README/)).
```sh
$cd compiler/augur
$stack init --solver
$stack build
```


#### Runtime
Build the AugurV2 runtime.
```sh
$cd cbits
$make libcpu
```
If you have Cuda support with dynamic parallelism (architecture >= sm_35), you can also build the GPU library.
```
$make libgpu
```


#### Interface
Build the AugurV2 Python interface.
```sh
$cd pyaugur
$python setup.py install
```

### Todos
 - release optimizations

License
----

Apache-2.0
