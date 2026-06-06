#!/usr/bin/env Rscript
## Generate R glmnet reference outputs ("goldens") for the Racket parity test.
##
## Run from the repo root (e.g. `nix run .#gen-goldens`, or inside
## `nix develop .#r-parity`). Reads the SAME committed CSVs that the Racket
## loaders in glmnet/private/demo-utils.rkt read, fits R glmnet per fixture, and
## writes one golden JSON per fixture to scripts/r-parity/goldens/.
##
## Conventions mirrored from fortran/glmnet_capi.f90 (so R == our bindings):
##  - single supplied lambda (glmnet uses flmin=1 when `lambda` is scalar),
##  - standardize=TRUE, intercept=TRUE, thresh=1e-7,
##  - binomial: factor(levels=c(0,1)) so the 2nd level "1" is P(y==1),
##  - type.logistic="Newton" (== our kopt=0).

suppressWarnings(suppressMessages({
  library(glmnet)
  library(jsonlite)
}))

## Goldens are written to GLMNET_GOLDENS_OUT (the CI check points this at a temp
## dir; local `nix run .#gen-goldens` defaults to scripts/r-parity/goldens, which
## is gitignored). They are never committed -- the parity check regenerates them.
data_dir    <- "glmnet/private/data"
goldens_dir <- Sys.getenv("GLMNET_GOLDENS_OUT", "scripts/r-parity/goldens")
dir.create(goldens_dir, showWarnings = FALSE, recursive = TRUE)

tol <- list(coef = 1e-4, intercept = 1e-4, dev_ratio = 1e-4, pred = 1e-4)
## No timestamp here: goldens must be byte-deterministic so checks.parity can
## diff freshly-regenerated goldens against the committed ones. Provenance (R +
## glmnet versions) is recorded below and in fortran/vendor/NOTICE.md.
meta <- list(
  r_version      = R.version.string,
  glmnet_version = as.character(packageVersion("glmnet")),
  tolerances     = tol
)

## --- dataset loaders (must match glmnet/private/demo-utils.rkt column choices) ---
load_longley <- function() {
  d <- read.csv(file.path(data_dir, "longley.csv"))
  list(X = as.matrix(d[, 1:6]), y = d[, 7])          # y = Employed (last col)
}
load_wdbc <- function() {
  d <- read.csv(file.path(data_dir, "wdbc.csv"))
  list(X = as.matrix(d[, -1]), y = d[, 1])           # y = diagnosis 0/1 (col 1)
}
datasets <- list(longley = load_longley(), wdbc = load_wdbc())

fit_gaussian <- function(X, y, alpha, lambda, thresh = 1e-7) {
  fit <- suppressWarnings(glmnet(X, y, family = "gaussian", alpha = alpha,
                                 lambda = lambda, standardize = TRUE,
                                 intercept = TRUE, thresh = thresh))
  b <- as.numeric(coef(fit, s = lambda, exact = TRUE, x = X, y = y))
  list(intercept    = b[1],
       coefficients = b[-1],
       dev_ratio    = fit$dev.ratio[1],
       lambda_used  = fit$lambda[1],
       predictions  = as.numeric(predict(fit, newx = X, s = lambda,
                                          exact = TRUE, x = X, y = y)))
}

fit_binomial <- function(X, y, alpha, lambda, thresh = 1e-7) {
  yf  <- factor(y, levels = c(0, 1))                 # 2nd level "1" == P(y==1)
  fit <- suppressWarnings(glmnet(X, yf, family = "binomial", alpha = alpha,
                                 lambda = lambda, standardize = TRUE,
                                 intercept = TRUE, thresh = thresh,
                                 type.logistic = "Newton"))
  b <- as.numeric(coef(fit, s = lambda, exact = TRUE, x = X, y = yf))
  list(intercept    = b[1],
       coefficients = b[-1],
       dev_ratio    = fit$dev.ratio[1],
       lambda_used  = fit$lambda[1],
       predictions  = as.numeric(predict(fit, newx = X, s = lambda,
                                          type = "response", exact = TRUE,
                                          x = X, y = yf)))   # P(y==1)
}

fixtures <- list(
  list(id = "gaussian-longley-lasso-0.5", dataset = "longley", family = "gaussian", alpha = 1.0, lambda = 0.5),
  list(id = "gaussian-longley-ridge-1.0", dataset = "longley", family = "gaussian", alpha = 0.0, lambda = 1.0),
  list(id = "gaussian-longley-enet-0.5",  dataset = "longley", family = "gaussian", alpha = 0.5, lambda = 0.5),
  list(id = "binomial-wdbc-lasso-0.02",   dataset = "wdbc",    family = "binomial", alpha = 1.0, lambda = 0.02),
  list(id = "binomial-wdbc-lasso-0.05",   dataset = "wdbc",    family = "binomial", alpha = 1.0, lambda = 0.05),
  list(id = "binomial-wdbc-ridge-0.05",   dataset = "wdbc",    family = "binomial", alpha = 0.0, lambda = 0.05)
)

for (f in fixtures) {
  d   <- datasets[[f$dataset]]
  res <- if (f$family == "gaussian") fit_gaussian(d$X, d$y, f$alpha, f$lambda)
         else                        fit_binomial(d$X, d$y, f$alpha, f$lambda)
  golden <- c(list(id = f$id, dataset = f$dataset, family = f$family,
                   alpha = f$alpha, lambda = f$lambda, thresh = 1e-7),
              res, list(meta = meta))
  path <- file.path(goldens_dir, paste0(f$id, ".json"))
  writeLines(toJSON(golden, digits = NA, auto_unbox = TRUE, pretty = TRUE), path)
  cat("wrote", path, "  (dev.ratio", round(res$dev_ratio, 4), ")\n")
}
