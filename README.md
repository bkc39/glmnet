# glmnet

Lasso, ridge and elastic-net regularized models for Racket, in R
[glmnet](https://glmnet.stanford.edu/)'s six families: Gaussian, binomial,
multinomial, Poisson, Cox and multi-response Gaussian. The solver is R glmnet's
own Fortran.

## Install

```sh
raco pkg install glmnet
```

Prebuilt native libraries exist for Linux x86-64 and macOS arm64 only: the
package's own `libglmnetcompat`, and the library that its dependency
[rkt-polars](https://github.com/bkc39/rkt-polars) ships. On Intel macOS the
install stages the arm64 libraries, so it completes but they fail to load; on
any other platform the install fails. CI tests Racket 9.3 under Nix, and the
current stable release by installing from the sources, as the package catalog
does, on Ubuntu 22.04, the latest Ubuntu and macOS arm64.

## Example

Fuel economy from weight and horsepower, in R's `mtcars`: fit, read the
coefficients and predict a new car; then a lasso fit, and the same lasso from a
formula.

```racket
(require glmnet glmnet/datasets)
(define fit (ols mtcars "mpg" #:predictors '("wt" "hp")))
(coef fit)
(predict fit '((wt 3.0) (hp 150)))
(define lasso-fit (lasso mtcars "mpg" #:predictors '("wt" "hp") #:lambda 4.0))
(define named-fit (formula-fit (~ mpg (+ wt hp)) mtcars #:lambda 4.0))
(equal? (predict named-fit mtcars) (predict lasso-fit mtcars))
```

## Documentation

The manual is at <https://docs.racket-lang.org/glmnet/>, and `raco docs glmnet`
opens the installed copy:

- [Getting started](https://docs.racket-lang.org/glmnet/getting-started.html):
  a first regression on mtcars, cross-validation and prediction.
- [Concepts](https://docs.racket-lang.org/glmnet/concepts.html): data, the
  penalty, the families, paths, prediction and cross-validation.
- [Data](https://docs.racket-lang.org/glmnet/data.html): example datasets,
  tables, CSV files, `math/matrix` and Polars.
- [Formulas](https://docs.racket-lang.org/glmnet/formulas.html) and
  [Plots](https://docs.racket-lang.org/glmnet/plots.html).
- [Examples](https://docs.racket-lang.org/glmnet/examples.html): one worked
  example per family.
- [Reference](https://docs.racket-lang.org/glmnet/reference.html).

## Status

All six of R glmnet 4.1's families fit at a single λ, along a regularization
path and with cross-validation, from matrices, lists, tables and Polars
dataframes or through R-style formulas, and `glmnet/plot` draws R's path and
cross-validation plots. Parity tests check the numbers against R. Not bound
yet: observation weights, penalty factors, coefficient limits, offsets and
`exclude` ([#12](https://github.com/bkc39/glmnet/issues/12)), and sparse
predictor matrices ([#11](https://github.com/bkc39/glmnet/issues/11)).

## Development

`AGENTS.md` describes the architecture and the workflow. With Nix:

```sh
nix develop                     # builds the native library, link-installs the package
raco test ./glmnet/             # unit tests and the examples' harnesses
bash scripts/run-examples.sh    # runs every example in glmnet/examples/
nix flake check                 # builds everything, runs the tests, renders the manual
```

With a local toolchain (gfortran, CMake and Racket):

```sh
cmake -S fortran -B fortran/build -DBUILD_TESTING=ON
cmake --build fortran/build
ctest --test-dir fortran/build --output-on-failure
cp fortran/build/libglmnetcompat.* glmnet/native-libs/
raco pkg install --batch --auto --link --name glmnet ./glmnet
raco test ./glmnet/
```

The package shares its architecture with the Racket bindings
[bkc39/rkt-polars](https://github.com/bkc39/rkt-polars),
[bkc39/scs](https://github.com/bkc39/scs) and
[bkc39/xgboost](https://github.com/bkc39/xgboost).

## License, acknowledgements and AI disclosure

**License.** This package is distributed under **GPL-2.0-or-later**.

**Acknowledgements.** The solver and its algorithms are the work of Jerome
Friedman, Trevor Hastie, Rob Tibshirani and the other authors of the
[R glmnet package](https://glmnet.stanford.edu/), whose vignettes shaped this
manual.

**AI Disclosure.** This package and its documentation were created with the
use of AI tools.
