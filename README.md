# AugurV2

AugurV2 generates compositional MCMC inference algorithms for a simple probabilistic modeling language. 

### Dependencies

AugurV2 additional depends on:
* [GSL](https://www.gnu.org/software/gsl/) - GNU scientific library
* [clang](https://clang.llvm.org/) - C compiler


### Installation

Build the AugurV2 compiler from source (using [stack](https://docs.haskellstack.org/en/stable/README/)).
```sh
$cd compiler/augur
$stack init --solver
$stack build
```
Build the AugurV2 runtime.
```sh
$cd cbits
$make
```

Build the AugurV2 Python interface.
```sh
$cd pyaugur
$python setup.py install
```

### Todos
 - release GPU backend

License
----

Apache-2.0