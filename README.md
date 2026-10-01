# glmnet

Racket bindings for [glmnet](https://glmnet.stanford.edu/) — lasso and
elastic-net regularized models — via the original Friedman/Hastie/Tibshirani
coordinate-descent **Fortran**, wrapped behind a clean C ABI.

This is the Fortran member of a family of Racket FFI bindings (`scs`,
`xgboost-rkt`, `rkt-polars`). The numerics come from the self-contained glmnet
Fortran (vendored, no BLAS/LAPACK dependency); a small `iso_c_binding` shim
exports a C ABI that the Racket FFI binds to.

## Status

All six of R glmnet's families are bound: Gaussian (OLS, ridge, lasso and
elastic net), binomial, multinomial, Cox, Poisson and multi-response Gaussian.
Each fits at a single λ, along a regularization path and with cross-validation
(R's `cv.glmnet`), and every result works with the generic `predict`, `coef`
and `deviance-ratio`. A formula front end fits any family from named columns,
and `glmnet/plot` draws R's path and cross-validation plots. Parity tests check
the numbers against R glmnet 4.1.10. Not bound yet: observation weights,
penalty factors, coefficient limits and offsets (#12), and sparse predictor
matrices (#11). See `AGENTS.md` for the development workflow.

The Scribble manual (`glmnet/scribblings/`) has a user guide (getting started,
concepts, data, formulas and named data, plots, R glmnet's Quick Start on R's
own data, one worked example per model family and three on formulas:
interactions, transforms and factors) and an API reference.
With the package installed, build it with
`raco scribble --htmls glmnet/scribblings/glmnet.scrbl`.

## Install

```racket
(require glmnet)
```

A prebuilt native library is staged at install time; no Fortran toolchain is
needed to use the package.

## Formulas and named data

Any family can be fitted from a table of named columns (an association list,
a hash, or a design matrix with column names) with an R-style formula, which
has R's algebra of terms: interactions, crossing, powers and the intercept,
prefix or infix; transforms such as `(log x1)` and `(I (expt x1 2))`; and
factors, columns of strings, symbols or booleans and `(factor x)`, coded by
R's treatment contrasts. The model keys its coefficients by name, and
`predict` builds its predictors from a new table by name, with the factors'
levels of the fit:

```racket
(define data
  (list (cons "y"  '(1.0 4.0 3.0 6.0 5.0 8.0))
        (cons "x1" '(1.0 2.0 3.0 4.0 5.0 6.0))
        (cons "x2" '(2.0 1.0 4.0 3.0 6.0 5.0))))
(define m (formula-fit (~ y all) data #:lambda 0.05))   ; also formula-path, formula-cv
(coef m)                                  ; => '(("(Intercept)" . ...) ("x1" . ...) ("x2" . ...))
(predict m (list (cons "x2" '(6.0)) (cons "x1" '(7.0))))
(formula-path (y . ~ . x1 * x2) data)     ; R's y ~ x1 * x2: x1, x2 and x1:x2
(formula-path (y . ~ . x1 + (I (expt x1 2))) data)   ; R's y ~ x1 + I(x1^2)
(formula-path (mpg . ~ . wt * (factor cyl)) mtcars)  ; R's mpg ~ wt * factor(cyl)
(formula-fit (~ Species all) iris #:family 'multinomial #:lambda 0.05)  ; string classes
(formula-cv (~ (surv time status) (- all id)) patients #:family 'cox)
```

## Example data and CSV files

R glmnet's example datasets, the data of its vignettes, and R's `mtcars` and
`iris` ship with the package in `glmnet/datasets`, exported from R with every
double exact. Each loader returns its family fitter's arguments, as R's
`data(QuickStartExample)` gives `x` and `y`. `glmnet/data/csv` reads and
writes tables as CSV files, with no dependency:

```racket
(require glmnet glmnet/datasets glmnet/data/csv)
(define-values (x y) (quick-start-example))   ; R's QuickStartExample, 100 x 20
(elnet-cv x y #:fold-ids (for/list ([i 100]) (modulo i 10)))
(call-with-values cox-example cox-path)        ; x, times and statuses
(define t (csv-file->table "patients.csv"))    ; numbers as flonums, strings as factors
(table->csv-file t "copy.csv")
```

## Plots

The coefficient-path and cross-validation plots of R's `plot.glmnet` and
`plot.cv.glmnet` are in `glmnet/plot`, a module of this package.
`(require glmnet)` does not load it, so a program that only fits models does
not load the plot library:

```racket
(require glmnet glmnet/plot)
(plot-cv (elnet-cv X y))               ; => a pict
(plot-coefficient-path (elnet-path X y) #:label #t #:out-file "path.png")
```

The manual's *Plots* chapter draws every plot it describes.

## Polars dataframes

`glmnet/data/polars` converts the dataframes of
[rkt-polars](https://github.com/bkc39/rkt-polars) to design matrices,
responses and tables, and back. Reading a file with Polars and converting it
is the fast way to fit real data. `(require glmnet)` does not load Polars:

```racket
(require glmnet glmnet/data/polars (only-in polars read-csv))
(define df (read-csv "iris.csv"))
(lasso (polars->design-matrix df '("Sepal.Width" "Petal.Width"))
       (polars->response df "Sepal.Length")
       #:lambda 0.01)
(formula-fit (Sepal.Length . ~ . Petal.Width + Species) (polars->table df) #:lambda 0.01)
```

## Quick check

```racket
(require glmnet)
(ols '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0)) '(1.0 4.0 3.0 6.0))  ; => an elnet-result
(glmnet-default-real-bytes)  ; => 8   (the double-precision contract)
```

## Build and run the examples (via Nix)

```bash
nix develop                     # builds the native lib + link-installs the package
bash scripts/run-examples.sh    # runs all fourteen examples (glmnet/examples/); prints each fit
raco test ./glmnet/             # full suite: unit tests + example harnesses
```

Or verify everything in one shot:

```bash
nix build .#native              # build libglmnetcompat + run the Fortran ctest suite
nix flake check                 # build everything, run raco test, render the docs
```

A local toolchain (gfortran + cmake + Racket) works too; see `AGENTS.md`.

## License

**GPL-2.0-or-later.** This package vendors and links the GPL-2.0 glmnet Fortran
(R glmnet's own, under `fortran/vendor/`); see `LICENSE` and
`fortran/vendor/NOTICE.md`. The plot library that `glmnet/plot` draws with,
plot-lib, and rkt-polars, which `glmnet/data/polars` adapts, are Apache-2.0 or
MIT.
