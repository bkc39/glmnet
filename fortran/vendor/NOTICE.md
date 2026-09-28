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
- **The lambda sequence (paths, #10).** A user `lambda` is fitted largest first, as R's `rev(sort(lambda))`. Without one, `lambda.min.ratio` defaults to 0.01 when there are fewer observations than predictors and to 1e-4 otherwise, as in R's `glmnet()`; the Fortran raises a ratio below 1e-6 (R's `glmnet.control(eps)`) to 1e-6, so 0 is accepted, as in R. In automatic mode the Fortran reports the first lambda as a sentinel, and `fix.lam` (`R/fix.lam.R`) replaces it with `exp(2 log lambda[2] - log lambda[3])` when at least three lambdas were fitted; `core/path.rkt` does the same.
- **Multinomial intercepts.** The K intercepts are identified only up to a common shift. R's `coef()` reports them centred to sum to zero at each lambda (`getcoef.multinomial`, `center.intercept = TRUE`); `multinomial-fit` and `multinomial-path` centre them too. Probabilities are unchanged.
- **Constant Gaussian response.** R's `elnet` wrapper stops with "y is constant; gaussian glmnet fails at standardization step" when the null deviance is zero; `elnet-fit` (and so `ols`, `ridge`, `lasso`, `elastic-net`) and `elnet-path` raise the same message.

## Where we differ from R

- **No lambda fitted.** When the Fortran fits no lambda at all (`lmu = 0`, for example convergence not reached at the first lambda within `maxit`), R warns and returns an empty model. Every single fit and every path fitter here raises `exn:fail` instead, naming the procedure called and glmnet's reason (R's `jerr` message).
- **A partial path.** When a path fails at a later lambda, R warns and returns the lambdas before it; so do we, but the warning is a Racket log message at level `warning` with the topic `glmnet`, which the default error display does not show.
- **Constant test.** R compares its computed null deviance with zero, which rounding can miss; we test the values themselves (all equal, or all zero without an intercept).
- **Constant multi-response `Y`.** R's `mgaussian` wrapper has no constant check: a `Y` with one constant column fits normally (so here too), but when every column is constant the Fortran does not converge and R fails with an internal `dimnames` error. `mgaussian-fit` and `mgaussian-path` raise the Gaussian "y is constant" error instead.
- **All-zero Poisson response.** R's `fishnet` warns that convergence was not reached at the first lambda and returns an empty model. `poisson-fit` and `poisson-path` raise before fitting ("the response has no positive count").

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
