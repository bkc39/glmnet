# Vendored third-party source

## `glmnet5dpclean.f`

This is R glmnet's own Fortran: the classic Friedman/Hastie/Tibshirani coordinate-descent solvers for every family we bind (`elnet`, `lognet`, `coxnet`, `fishnet`, `multelnet`, plus the sparse variants). It is self-contained, with no BLAS/LAPACK dependency, and it is **byte-for-byte upstream, with no local modifications**.

| | |
| --- | --- |
| Upstream | `src/glmnet5dpclean.f` from R glmnet **4.1**, via the CRAN GitHub mirror <https://github.com/cran/glmnet> |
| Pinned commit | `537e1a9ea45f0c98ff64e65d818cf250b4b2851b` (tag `4.1`) |
| SHA-256 | `1954875ba66f81ffe1fb24997718f65cf1ab1510546121f65a52b088ef99d177` |
| Retrieved | 2026-09-25 |
| License | GPL-2 (only), per R glmnet's `DESCRIPTION`, copied here as `R-glmnet-DESCRIPTION`. The GPL-2 text is the project root `LICENSE`. |

To re-fetch and verify:

```bash
curl -fsSL https://raw.githubusercontent.com/cran/glmnet/537e1a9ea45f0c98ff64e65d818cf250b4b2851b/src/glmnet5dpclean.f \
  -o fortran/vendor/glmnet5dpclean.f
echo "1954875ba66f81ffe1fb24997718f65cf1ab1510546121f65a52b088ef99d177  fortran/vendor/glmnet5dpclean.f" | sha256sum -c
```

### Why R glmnet 4.1

4.1 is the **last** R glmnet release that contains this file. From 4.1-1 on, R runs the Gaussian, binomial, Poisson and multi-response solvers in C++ (`glmnetpp`) and keeps only its Cox solver in Fortran (`src/coxnet5dpclean.f`). That later Cox Fortran differs from this file's `coxnet` only by a `maxit` guard in `coxnet1`. The guard matters only when a fit fails to converge, and it is not needed for parity (see below).

The Gaussian no-intercept fix (#33) is part of upstream since R glmnet 3.0-3.

## Behaviour R adds around the Fortran

R's R-level wrappers do some work before calling the Fortran. What our shim (`../glmnet_capi.f90`) and the Racket layer reproduce:

- **Cox ties.** R's `coxnet` wrapper (`R/coxnet.R`) nudges censored times up by `100 * .Machine$double.eps`, so that a subject censored at an event time stays in that event's risk set. `glmnet_coxnet_solo` does the same (#21). Without it, tied data gives fits that differ from R's.
- **The first λ of an automatic path (`fix.lam`).** When glmnet chooses the λ sequence, the Fortran reports the first λ as its `big` sentinel (9.9e35). R's `glmnet()` replaces it with `exp(2·log λ₂ − log λ₃)`, the log-linear extrapolation from the next two, and leaves it alone when the path has only two λ. `finish-lambdas` in `glmnet/core/path.rkt` does the same (#10).
- **Centred multinomial intercepts.** The softmax is unchanged when the same constant is added to every class's intercept, and the Fortran leaves that constant free. R's `getcoef.multinomial` subtracts the mean over the classes from the intercepts at every λ (`scale(a0, TRUE, FALSE)`), and so do `multinomial-fit` and `multinomial-path` (#25). The multi-response Gaussian intercepts are not centred, as in R (`center.intercept = FALSE`).
- **Coefficients between fitted λ values.** R's `predict` and `coef` with the default `exact = FALSE` take a λ `s` that is not on the path by linear interpolation of the coefficients (intercepts included) between the two fitted λ on either side, after rescaling the path to [0, 1] (`lambda.interp`). An `s` above the largest fitted λ or below the smallest is clamped to that end, and a fit with one λ returns that fit for every `s`. `glmnet/core/model.rkt` reproduces the rule, including the tie handling of R's `approx` for a λ that appears twice in a user sequence (#25). R's `exact = TRUE`, which refits at `s`, is not reproduced.
- **Cross-validation.** R's `cv.glmnet` is R code around the Fortran, and `glmnet/core/cv.rkt` reproduces it (#27):
  - With the default `alignment = "lambda"`, each fold's path is fitted with R's `lambda` argument. Without a user sequence, each fold therefore chooses its own automatic λ values, and its predictions at the full-data λ values come from `lambda.interp` (above), clamped at the ends of the fold's path.
  - The losses are those of `cv.elnet`, `cv.lognet`, `cv.multnet`, `cv.fishnet`, `cv.coxnet` and `cv.mrelnet`, including the probability clamp to [1e-5, 1 − 1e-5] in the binomial and multinomial deviances. `cvm` and `cvsd` follow `cvcompute` and `cvstats`: with `grouped = TRUE`, per-fold means (an infinite loss counts as missing), weighted by fold size, and `cvsd` = sqrt(weighted mean of squared deviations / (number of folds − 1)). λ values whose `cvsd` is undefined are dropped. Sums use a compensated sum, which approximates R's extended-precision `sum`, so that values equal in R, such as tied misclassification rates, stay equal.
  - `lambda.min` and `lambda.1se` follow `getOptcv.glmnet`, including its ties: the largest λ at the minimum, `cvm` negated for AUC and the C-index, the bound `cvm + cvsd` taken at `lambda.min` with `<=`, and each index taken as the first position of that λ (`match`).
  - R's adjustments for small folds, each with a logged warning: fewer than 3 observations per fold turns grouping off; fewer than 10 per fold turns `type.measure = "auc"` into `"deviance"`, and turns grouping back on for the Cox deviance.
  - The Cox deviance is `coxnet.deviance`: twice the saturated minus the Breslow partial log-likelihood, which the Fortran's `loglike` computes from the linear predictor centred at its mean, with censored times nudged by `100 * .Machine$double.eps`. It is computed in Racket rather than through a new shim entry point. With `grouped = TRUE`, each fold contributes the deviance of all the data less that of its training data, as `buildPredmat.coxnetlist` computes it.
  - AUC and the C-index are `survival::concordance`, which R's `auc` and `Cindex` call: ties in the prediction count a half, and an event comes before a censored time equal to it. R's `timefix` step (`aeqSurv`), which merges times that differ by less than about 1.5e-8 relative, is not reproduced.
  - `nzero` counts as `predict(type = "nonzero")` does: for the multinomial, the median over the classes rounded up; for the multi-response Gaussian, the nonzero entries of the first response's coefficients, including its intercept, so it is one more than the path's `df` whenever that intercept is nonzero.
  - Deliberate differences: fold ids count from 0, not 1; a `type.measure` the family does not have is a contract error, where R warns and uses the default; and when a fold's training data lacks a class (or, for Cox, an event), the error names the fold and comes before any fitting. R stops too, from inside the fold's fit, and it also stops when a class has only one training observation, which is fitted here.
- **Coefficient names.** R's `coef` labels its rows `(Intercept)` (absent for Cox) and then the column names of `x`, and names the elements of a multinomial or multi-response result by class level or by the column names of `y` (`y1`, `y2`, ... when `y` has none). The formula front end (`glmnet/core/formula.rkt`, #26) fits from named columns, and `coef` of its models is keyed the same way (`glmnet/core/model.rkt`), with class labels as integers. R's `glmnet` has no formula interface (the `glmnetUtils` package adds one), so a formula fit is the matrix fit of the columns the formula selects, and has no numbers of its own.
- **Plots.** The separate `glmnet-plot` package (#28) draws R's `plot.glmnet` and `plot.cv.glmnet` as 4.1.10's `plotCoef`, `plot.multnet`, `plot.mrelnet` and `plot.cv.glmnet` draw them:
  - The default x axis is −log λ (`xvar = "lambda"`, `sign.lambda = -1`), 4.1-10's default; before 4.1-9 it was the L1 norm.
  - The counts along the top of a path plot are `approx(index, df, method = "constant", rule = 2, f)` at the ticks, with `f = 0` for log λ and `f = 1` otherwise.
  - The multinomial and multi-response panels count each class's or response's nonzero coefficients (`dfmat`). The `type.coef = "2norm"` panel shows `round(colMeans(dfmat), 1)` for the multinomial and the first response's count for the multi-response family.
  - Where the plots differ from R's (tick positions, the labels along the top of the CV plot, labels that R clips, one stacked picture instead of one plot per class) is listed in its manual.
- **Missing and non-finite values.** R's `glmnet()` stops with "x has missing values" when `any(is.na(x))`, which includes `NaN`, and a non-finite Gaussian `y` makes it stop with "missing value where TRUE/FALSE needed". The Racket design-matrix layer (`glmnet/data.rkt`, #35) rejects a `NaN` or an infinity in `x` or in any response before the shim is called, and names its position. It is stricter than R in one case: R passes an infinite `x` to the Fortran, and `glmnet(x, y, lambda = 0)` with one `Inf` in `x` returns coefficients without an error.

## Build notes

- **Form.** The file is **fixed-form** Fortran. `../CMakeLists.txt` sets `Fortran_FORMAT FIXED`, whose default 72-column width ignores the sequence numbers in columns 73–80.
- **Precision.** The file is explicitly `double precision` (`implicit double precision(a-h,o-z)`). The build keeps `-fdefault-real-8`, which the `glmnet_default_real_bytes` probe checks, and adds `-fdefault-double-8`. Without that second flag, `-fdefault-real-8` would widen `double precision` to 16 bytes. `tests/test_precision.f90` asserts both kinds are 8 bytes.
- **`setpb`.** R's Fortran reports progress through `setpb`, which R defines in C. It is only called when `itrace` is nonzero (the default is 0), so `../r_stubs.f90` supplies a no-op.
- **Diffs.** `.gitattributes` marks the file `linguist-generated`, so GitHub collapses it in PR diffs.

## Parity reference (R `glmnet`)

The parity goldens are R glmnet's reference outputs. `glmnet/tests/parity-test.rkt` uses them to check that these bindings reproduce R's numbers on real datasets. `scripts/r-parity/gen-reference.R` regenerates them through the Nix-pinned R environment (`nix run .#gen-goldens`), so `flake.lock` fixes the versions:

- **R version:** 4.5.3
- **R `glmnet` package:** 4.1.10

Update these when the goldens are regenerated against a newer pinned glmnet.
