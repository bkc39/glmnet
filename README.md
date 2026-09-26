# glmnet

Racket bindings for [glmnet](https://glmnet.stanford.edu/) — lasso and
elastic-net regularized models — via the original Friedman/Hastie/Tibshirani
coordinate-descent **Fortran**, wrapped behind a clean C ABI.

This is the Fortran member of a family of Racket FFI bindings (`scs`,
`xgboost-rkt`, `rkt-polars`). The numerics come from the self-contained glmnet
Fortran (vendored, no BLAS/LAPACK dependency); a small `iso_c_binding` shim
exports a C ABI that the Racket FFI binds to.

## Status

Example-driven, in progress. Phase 0 (toolchain + FFI spine) is complete; the
four core models — OLS, ridge, lasso, elastic net — land one at a time, each as
a runnable literate example with a matching section in the user guide. See
`AGENTS.md` for the development workflow and `plans/` notes.

The Scribble manual (`glmnet/scribblings/`) has a user guide (getting started,
concepts, one worked example per model family) and an API reference. With the
package installed, build it with
`raco scribble --htmls glmnet/scribblings/glmnet.scrbl`.

## Install

```racket
(require glmnet)
```

A prebuilt native library is staged at install time; no Fortran toolchain is
needed to use the package.

## Formulas and named data

Any family can be fitted from a table of named columns (an association list,
a hash, or a design matrix with column names) with an R-style formula. The
model keys its coefficients by name, and `predict` reads a new table by name:

```racket
(define data
  (list (cons "y"  '(1.0 4.0 3.0 6.0 5.0 8.0))
        (cons "x1" '(1.0 2.0 3.0 4.0 5.0 6.0))
        (cons "x2" '(2.0 1.0 4.0 3.0 6.0 5.0))))
(define m (formula-fit (~ y all) data #:lambda 0.05))   ; also formula-path, formula-cv
(coef m)                                  ; => '(("(Intercept)" . ...) ("x1" . ...) ("x2" . ...))
(predict m (list (cons "x2" '(6.0)) (cons "x1" '(7.0))))
(formula-cv (~ (surv time status) (- all id)) patients #:family 'cox)
```

## Plots (`glmnet-plot`)

The coefficient-path and cross-validation plots of R's `plot.glmnet` and
`plot.cv.glmnet` are in a second package in this repository, `glmnet-plot`
(directory `glmnet-plot/`). It adds `glmnet/plot` to the `glmnet` collection
and is separate so that `glmnet` does not depend on the plot library:

```bash
raco pkg install glmnet-plot
```

```racket
(require glmnet glmnet/plot)
(plot-cv (elnet-cv X y))               ; => a pict
(plot-coefficient-path (elnet-path X y) #:label #t #:out-file "path.png")
```

Its manual, `glmnet-plot/scribblings/glmnet-plot.scrbl`, draws every plot it
describes.

## Quick check

```racket
(require glmnet)
(ols '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0)) '(1.0 4.0 3.0 6.0))  ; => an elnet-result
(glmnet-default-real-bytes)  ; => 8   (the double-precision contract)
```

## Build and run the examples (via Nix)

```bash
nix develop                     # builds the native lib + link-installs the package
bash scripts/run-examples.sh    # runs OLS, ridge, lasso, elastic net; prints each fit
raco test ./glmnet/             # full suite: unit tests + example harnesses
```

Or verify everything in one shot:

```bash
nix build .#native              # build libglmnetcompat + run the Fortran ctest suite
nix build .#plot                # install glmnet + glmnet-plot, test glmnet-plot, render its manual
nix flake check                 # build everything, run raco test, render the docs
```

A local toolchain (gfortran + cmake + Racket) works too; see `AGENTS.md`.

## License

**GPL-2.0-or-later.** This package vendors and links the GPL-2.0 glmnet Fortran
(R glmnet's own, under `fortran/vendor/`); see `LICENSE` and
`fortran/vendor/NOTICE.md`. `glmnet-plot` links `glmnet` and has the same
licence; its plot library, plot-lib, is Apache-2.0 or MIT.
