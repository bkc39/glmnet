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

R's R-level wrappers do some work before and after calling the Fortran. What our shim (`../glmnet_capi.f90`) and the Racket side reproduce:

- **Cox ties.** R's `coxnet` wrapper (`R/coxnet.R`) nudges censored times up by `100 * .Machine$double.eps`, so that a subject censored at an event time stays in that event's risk set. `glmnet_coxnet_path`, which `glmnet_coxnet_solo` calls, does the same (#21). Without it, tied data gives fits that differ from R's.
- **`lmu = 0`.** R passes `lmu = integer(1)`, that is 0, into the Fortran, which returns without setting it when it fails at the first lambda. Every `glmnet_<family>_path` sets `lmu = 0` (and `nlp = 0`) before the call.
- **The lambda sequence (paths, #10).** A user `lambda` is fitted largest first, as R's `rev(sort(lambda))`. Without one, `lambda.min.ratio` defaults to 0.01 when there are fewer observations than predictors and to 1e-4 otherwise, as in R's `glmnet()`; the Fortran raises a ratio below 1e-6 (R's `glmnet.control(eps)`) to 1e-6, so 0 is accepted, as in R. In automatic mode the Fortran reports the first λ as its `big` sentinel (9.9e35). R's `glmnet()` (`fix.lam`, `R/fix.lam.R`) replaces it with `exp(2·log λ₂ − log λ₃)`, the log-linear extrapolation from the next two, when at least three λ were fitted, and leaves it alone when the path has only two λ. `finish-lambdas` in `glmnet/core/path.rkt` does the same.
- **Multinomial intercepts.** The K intercepts are identified only up to a common shift: the softmax, and so every probability, is unchanged when the same constant is added to every class's intercept, and the Fortran leaves that constant free. R's `getcoef.multinomial` subtracts the mean over the classes from the intercepts at every λ (`scale(a0, TRUE, FALSE)`, `center.intercept = TRUE`), and so do `multinomial-fit` and `multinomial-path` (#10). The multi-response Gaussian intercepts are not centred, as in R (`center.intercept = FALSE`).
- **Constant Gaussian response.** R's `elnet` wrapper stops with "y is constant; gaussian glmnet fails at standardization step" when the null deviance is zero; `elnet-fit` (and so `ols`, `ridge`, `lasso`, `elastic-net`) and `elnet-path` raise the same message.
- **Coefficients between fitted λ values.** R's `predict` and `coef` with the default `exact = FALSE` take a λ `s` that is not on the path by linear interpolation of the coefficients (intercepts included) between the two fitted λ on either side, after rescaling the path to [0, 1] (`lambda.interp`). An `s` above the largest fitted λ or below the smallest is clamped to that end, and a fit with one λ returns that fit for every `s`. `glmnet/core/model.rkt` reproduces the rule, including the tie handling of R's `approx` for a λ that appears twice in a user sequence (#25). R's `exact = TRUE`, which refits at `s`, is not reproduced. One case differs: when every fitted λ is the same, as for a user sequence `(0.1 0.1)`, the rescaling divides by zero and R's `coef` and `predict` stop with "need at least two non-NA values to interpolate" for any `s`. Ours return the first fitted point.
- **The printed path.** R's `print.glmnet` prints a data frame of `Df`, `round(dev.ratio * 100, 2)` and `signif(lambda, 4)` with row numbers, through `print.anova`, which rounds each of the last two columns with `zapsmall(x, 5)` and formats every column with `format(x, digits = 5)` (`printCoefmat`). `write-path` in `glmnet/core/path.rkt` reproduces that table line for line, following the pinned R: since R 4.4, `zapsmall` rounds to `5 - log10(max |x|)` places rounded to the nearest integer, where it used the ceiling of `log10` before. The `Call:` line R prints above the table is left out (#25).
- **Cross-validation.** R's `cv.glmnet` is R code around the Fortran, and `glmnet/core/cv.rkt` reproduces it (#27):
  - With the default `alignment = "lambda"`, each fold's path is fitted with R's `lambda` argument. Without a user sequence, each fold therefore chooses its own automatic λ values, and its predictions at the full-data λ values come from `lambda.interp` (above), clamped at the ends of the fold's path.
  - The losses are those of `cv.elnet`, `cv.lognet`, `cv.multnet`, `cv.fishnet`, `cv.coxnet` and `cv.mrelnet`, including the probability clamp to [1e-5, 1 − 1e-5] in the binomial and multinomial deviances. `cvm` and `cvsd` follow `cvcompute` and `cvstats`: with `grouped = TRUE`, per-fold means (an infinite loss counts as missing), weighted by fold size, and `cvsd` = sqrt(weighted mean of squared deviations / (number of folds − 1)). λ values whose `cvsd` is undefined are dropped. Sums use a compensated sum, which approximates R's extended-precision `sum`, so that values equal in R, such as tied misclassification rates, stay equal.
  - `lambda.min` and `lambda.1se` follow `getOptcv.glmnet`, including its ties: the largest λ at the minimum, `cvm` negated for AUC and the C-index, the bound `cvm + cvsd` taken at `lambda.min` with `<=`, and each index taken as the first position of that λ (`match`).
  - R's adjustments for small folds, each with a logged warning. They look at the average fold size, the number of observations divided by the number of folds, as R does: below 3, grouping is turned off; below 10, `type.measure = "auc"` becomes `"deviance"`, and grouping is turned back on for the Cox deviance.
  - The Cox deviance is `coxnet.deviance`: twice the saturated minus the Breslow partial log-likelihood, which the Fortran's `loglike` computes from the linear predictor centred at its mean, with censored times nudged by `100 * .Machine$double.eps`. It is computed in Racket rather than through a new shim entry point. With `grouped = TRUE`, each fold contributes the deviance of all the data less that of its training data, as `buildPredmat.coxnetlist` computes it. With `grouped = FALSE`, it is the deviance of the held-out fold alone, and R stops (jerr 30000 from the Fortran's `groups`) when that fold has no event or its first event, in time order, is among its last two observations. `cox-cv` raises an error naming `cox-cv` and the fold in that case, before any fold is fitted. It counts an event as coming before a censored time equal to it, which the nudge makes true for times below 256. From 256 on the nudge is lost to rounding, R's sort puts such a tie in either order, and the two can disagree.
  - AUC and the C-index are `survival::concordance`, which R's `auc` and `Cindex` call: ties in the prediction count a half, and an event comes before a censored time equal to it. R's `timefix` step (`aeqSurv`), which merges times that differ by less than about 1.5e-8 relative, is not reproduced.
  - `nzero` counts as `predict(type = "nonzero")` does: for the multinomial, the median over the classes rounded up; for the multi-response Gaussian, the nonzero entries of the first response's coefficients, including its intercept, so it is one more than the path's `df` whenever that intercept is nonzero.
  - Deliberate differences:
    - Fold ids count from 0, not 1.
    - A `type.measure` the family does not have is a contract error, where R warns and uses the default.
    - An `#:nfolds` larger than the number of observations, including the default of 10, is an error. R runs: `sample(rep(seq(nfolds), length = N))` leaves some folds empty, and the "training data" of an empty fold is all the data.
    - Fold ids that skip a number (0, 1, 3) are a contract error that names the missing fold, checked without allocating anything the size of the largest id. R warns ("number of rows of result is not a multiple of vector length") and then stops with "'x' and 'w' must have the same length".
    - The response is checked before anything is fitted, and each fold's training data before any fold is fitted: when the training data lack a class (or, for Cox, an event), the error names the fold. R stops too, from inside the fit, and it also stops when a class has only one training observation, which is fitted here. When a fit fails all the same, the error names the `*-cv` procedure and the held-out fold whose training data failed; R's names only the error code.
- **Missing and non-finite values.** R's `glmnet()` stops with "x has missing values" when `any(is.na(x))`, which includes `NaN`, and a non-finite Gaussian `y` or a `NaN` Poisson `y` makes it stop with "missing value where TRUE/FALSE needed". The Racket design-matrix layer (`glmnet/data.rkt`, #35) rejects a `NaN` or an infinity in `x`, in any response and in the new data of every prediction helper before the shim is called, and names its position. That is stricter than R 4.1.10 in three cases:
  - **An infinite `x`** in the Gaussian, binomial, multinomial, Poisson and multi-response Gaussian families. R passes it to its solver without an error and returns a fit in which that column's coefficient is 0: `glmnet(x, y, lambda = 0)` with one `Inf` in `x` returns coefficients. For Cox there is no difference, because R stops with "NA/NaN/Inf in foreign function call".
  - **An infinite Poisson count.** R warns that convergence was not reached at the first λ (error code −1) and returns an empty model, every coefficient 0. Without a `lambda`, it then stops with an internal error ("number of columns of matrices must match").
  - **Non-finite new data in a prediction.** R's `predict()` does not check `newx`. A row with a `NaN`, `NA` or infinite value predicts `NaN`, `NA` or ±`Inf`, unless every such value falls in a column whose coefficient is 0: the sparse product skips that column, and the prediction is finite. The prediction helpers raise an error instead.

  Where R also stops, only the message differs: an infinite Cox time, and a non-finite multi-response `Y`, for which R warns about convergence and then fails with "length of 'dimnames' [2] not equal to array extent".

## Where we differ from R

- **No lambda fitted.** When the Fortran fits no lambda at all (`lmu = 0`, for example convergence not reached at the first lambda within `maxit`), R warns and returns an empty model. Every single fit and every path fitter here raises `exn:fail` instead, naming the procedure called and glmnet's reason (R's `jerr` message).
- **A partial path.** When a path fails at a later lambda, R warns and returns the lambdas before it; so do we, but the warning is a Racket log message at level `warning` with the topic `glmnet`, which the default error display does not show.
- **Constant test.** R compares its computed null deviance with zero, which rounding can miss; we test the values themselves (all equal, or all zero without an intercept).
- **Constant multi-response `Y`.** R's `mgaussian` wrapper has no constant check: a `Y` with one constant column fits normally (so here too), but when every column is constant the Fortran does not converge and R fails with an internal `dimnames` error. `mgaussian-fit` and `mgaussian-path` raise the Gaussian "y is constant" error instead.
- **All-zero Poisson response.** R's `fishnet` warns that convergence was not reached at the first lambda and returns an empty model. `poisson-fit` and `poisson-path` raise before fitting ("the response has no positive count").
- **Prediction types.** R's `predict` accepts `type = "class"` for a Gaussian fit and returns the linear predictor, because `predict.elnet` hands the type on to `predict.glmnet`. Our `predict` raises for `'class` on the Gaussian family, as R's methods for the Poisson, Cox and multi-response families do for theirs (#25).

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
