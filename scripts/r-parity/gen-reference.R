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
##  - standardize=TRUE, thresh=1e-7, intercept=TRUE unless a fixture sets
##    `intercept = FALSE` (recorded in the golden as `fit_intercept`),
##  - binomial: factor(levels=c(0,1)) so the 2nd level "1" is P(y==1),
##  - type.logistic="Newton" (== our kopt=0).

suppressWarnings(suppressMessages({
  library(glmnet)
  library(jsonlite)
  library(survival)   # Surv() for the Cox family
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
load_iris <- function() {
  d <- read.csv(file.path(data_dir, "iris.csv"))
  list(X = as.matrix(d[, 1:4]), y = d[, 5])          # y in {0,1,2}
}
load_veteran <- function() {
  d <- read.csv(file.path(data_dir, "veteran.csv"))
  list(X = as.matrix(d[, 1:5]), time = d$time, status = d$status)
}
load_warpbreaks <- function() {
  d <- read.csv(file.path(data_dir, "warpbreaks.csv"))
  list(X = as.matrix(d[, 1:3]), y = d$breaks)        # counts
}
load_linnerud <- function() {
  d <- read.csv(file.path(data_dir, "linnerud.csv"))
  list(X = as.matrix(d[, 1:3]), Y = as.matrix(d[, 4:6]))
}
datasets <- list(longley = load_longley(), wdbc = load_wdbc(),
                 iris = load_iris(), veteran = load_veteran(),
                 warpbreaks = load_warpbreaks(), linnerud = load_linnerud())

fit_gaussian <- function(X, y, alpha, lambda, intercept = TRUE, thresh = 1e-7) {
  fit <- suppressWarnings(glmnet(X, y, family = "gaussian", alpha = alpha,
                                 lambda = lambda, standardize = TRUE,
                                 intercept = intercept, thresh = thresh))
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

fit_multinomial <- function(X, y, alpha, lambda, thresh = 1e-7) {
  yf  <- factor(y)                                   # sorted levels -> classes 0..K-1
  fit <- suppressWarnings(glmnet(X, yf, family = "multinomial", alpha = alpha,
                                 lambda = lambda, standardize = TRUE, intercept = TRUE,
                                 thresh = thresh, type.multinomial = "ungrouped"))
  co  <- coef(fit, s = lambda, exact = TRUE, x = X, y = yf)   # list of K (intercept+betas)
  probs <- predict(fit, newx = X, s = lambda, type = "response", exact = TRUE, x = X, y = yf)
  list(intercepts    = unname(sapply(co, function(m) as.numeric(m)[1])),
       coefficients  = unname(lapply(co, function(m) as.numeric(m)[-1])),   # K vectors
       dev_ratio     = fit$dev.ratio[1],
       lambda_used   = fit$lambda[1],
       probabilities = probs[, , 1])                 # n x K
}

fit_cox <- function(X, time, status, alpha, lambda, thresh = 1e-7) {
  sy  <- Surv(time, status)
  fit <- suppressWarnings(glmnet(X, sy, family = "cox", alpha = alpha, lambda = lambda,
                                 standardize = TRUE, thresh = thresh))
  list(coefficients     = as.numeric(coef(fit, s = lambda, exact = TRUE, x = X, y = sy)),  # no intercept
       dev_ratio        = fit$dev.ratio[1],
       lambda_used      = fit$lambda[1],
       linear_predictor = as.numeric(predict(fit, newx = X, s = lambda, type = "link",
                                             exact = TRUE, x = X, y = sy)))
}

fit_poisson <- function(X, y, alpha, lambda, thresh = 1e-7) {
  fit <- suppressWarnings(glmnet(X, y, family = "poisson", alpha = alpha, lambda = lambda,
                                 standardize = TRUE, intercept = TRUE, thresh = thresh))
  b <- as.numeric(coef(fit, s = lambda, exact = TRUE, x = X, y = y))
  list(intercept    = b[1], coefficients = b[-1],
       dev_ratio    = fit$dev.ratio[1], lambda_used = fit$lambda[1],
       predictions  = as.numeric(predict(fit, newx = X, s = lambda, type = "response",
                                          exact = TRUE, x = X, y = y)))   # fitted means
}

fit_mgaussian <- function(X, Y, alpha, lambda, thresh = 1e-7) {
  fit <- suppressWarnings(glmnet(X, Y, family = "mgaussian", alpha = alpha, lambda = lambda,
                                 standardize = TRUE, standardize.response = FALSE,
                                 intercept = TRUE, thresh = thresh))
  co  <- coef(fit, s = lambda, exact = TRUE, x = X, y = Y)    # list of nr (intercept+betas)
  preds <- predict(fit, newx = X, s = lambda, exact = TRUE, x = X, y = Y)
  list(intercepts   = unname(sapply(co, function(m) as.numeric(m)[1])),
       coefficients = unname(lapply(co, function(m) as.numeric(m)[-1])),  # nr vectors
       r_squared    = fit$dev.ratio[1],
       lambda_used  = fit$lambda[1],
       predictions  = preds[, , 1])                 # n x nr
}

fixtures <- list(
  list(id = "gaussian-longley-lasso-0.5", dataset = "longley", family = "gaussian", alpha = 1.0, lambda = 0.5),
  list(id = "gaussian-longley-ridge-1.0", dataset = "longley", family = "gaussian", alpha = 0.0, lambda = 1.0),
  list(id = "gaussian-longley-enet-0.5",  dataset = "longley", family = "gaussian", alpha = 0.5, lambda = 0.5),
  list(id = "gaussian-longley-lasso-0.5-nointercept", dataset = "longley", family = "gaussian", alpha = 1.0, lambda = 0.5, intercept = FALSE),
  list(id = "gaussian-longley-ridge-1.0-nointercept", dataset = "longley", family = "gaussian", alpha = 0.0, lambda = 1.0, intercept = FALSE),
  list(id = "binomial-wdbc-lasso-0.02",   dataset = "wdbc",    family = "binomial", alpha = 1.0, lambda = 0.02),
  list(id = "binomial-wdbc-lasso-0.05",   dataset = "wdbc",    family = "binomial", alpha = 1.0, lambda = 0.05),
  list(id = "binomial-wdbc-ridge-0.05",   dataset = "wdbc",    family = "binomial", alpha = 0.0, lambda = 0.05),
  list(id = "multinomial-iris-lasso-0.02",   dataset = "iris",       family = "multinomial", alpha = 1.0, lambda = 0.02),
  list(id = "multinomial-iris-ridge-0.05",   dataset = "iris",       family = "multinomial", alpha = 0.0, lambda = 0.05),
  list(id = "cox-veteran-lasso-0.05",        dataset = "veteran",    family = "cox",         alpha = 1.0, lambda = 0.05),
  list(id = "cox-veteran-ridge-0.1",         dataset = "veteran",    family = "cox",         alpha = 0.0, lambda = 0.1),
  list(id = "poisson-warpbreaks-lasso-0.05", dataset = "warpbreaks", family = "poisson",     alpha = 1.0, lambda = 0.05),
  list(id = "poisson-warpbreaks-ridge-0.1",  dataset = "warpbreaks", family = "poisson",     alpha = 0.0, lambda = 0.1),
  list(id = "mgaussian-linnerud-lasso-1.0",  dataset = "linnerud",   family = "mgaussian",   alpha = 1.0, lambda = 1.0),
  list(id = "mgaussian-linnerud-ridge-2.0",  dataset = "linnerud",   family = "mgaussian",   alpha = 0.0, lambda = 2.0)
)

for (f in fixtures) {
  d   <- datasets[[f$dataset]]
  fit_intercept <- if (is.null(f$intercept)) TRUE else f$intercept
  res <- switch(f$family,
    gaussian    = fit_gaussian(d$X, d$y, f$alpha, f$lambda, fit_intercept),
    binomial    = fit_binomial(d$X, d$y, f$alpha, f$lambda),
    multinomial = fit_multinomial(d$X, d$y, f$alpha, f$lambda),
    poisson     = fit_poisson(d$X, d$y, f$alpha, f$lambda),
    cox         = fit_cox(d$X, d$time, d$status, f$alpha, f$lambda),
    mgaussian   = fit_mgaussian(d$X, d$Y, f$alpha, f$lambda),
    stop("unknown family ", f$family))
  golden <- c(list(id = f$id, dataset = f$dataset, family = f$family,
                   alpha = f$alpha, lambda = f$lambda, thresh = 1e-7,
                   fit_intercept = fit_intercept),
              res, list(meta = meta))
  path <- file.path(goldens_dir, paste0(f$id, ".json"))
  writeLines(toJSON(golden, digits = NA, auto_unbox = TRUE, pretty = TRUE), path)
  dr <- if (!is.null(res$dev_ratio)) res$dev_ratio else res$r_squared
  cat("wrote", path, "  (fit", round(dr, 4), ")\n")
}

## --- regularization paths (#10) ---------------------------------------------
## R's automatic path (nlambda = 100; lambda.min.ratio 0.01 when n < p, else
## 1e-4), or a user sequence when a fixture sets `lambda`. The golden records
## every fitted lambda (after R's fix.lam) and, per lambda, the intercepts,
## coefficients, deviance ratio and df.

fit_path <- function(family, d, alpha, lambda = NULL, thresh = 1e-7) {
  y <- switch(family,
    binomial    = factor(d$y, levels = c(0, 1)),
    multinomial = factor(d$y),
    cox         = Surv(d$time, d$status),
    mgaussian   = d$Y,
    d$y)
  args <- list(d$X, y, family = family, alpha = alpha, standardize = TRUE,
               thresh = thresh)
  if (!is.null(lambda)) args$lambda <- lambda
  if (family == "binomial") args$type.logistic <- "Newton"
  if (family == "multinomial") args$type.multinomial <- "ungrouped"
  if (family == "mgaussian") args$standardize.response <- FALSE
  fit <- suppressWarnings(do.call(glmnet, args))
  L  <- length(fit$lambda)
  co <- coef(fit)
  if (is.list(co)) {                 # multinomial, mgaussian: one matrix per class/response
    coefficients <- lapply(seq_len(L), function(m)
      unname(lapply(co, function(B) unname(as.numeric(B[-1, m])))))
    intercepts <- lapply(seq_len(L), function(m) unname(sapply(co, function(B) B[1, m])))
  } else if (family == "cox") {      # no intercept row
    coefficients <- lapply(seq_len(L), function(m) unname(as.numeric(co[, m])))
    intercepts <- NULL
  } else {
    coefficients <- lapply(seq_len(L), function(m) unname(as.numeric(co[-1, m])))
    intercepts <- unname(as.numeric(co[1, ]))
  }
  res <- list(lambda_path = fit$lambda, dev_ratio_path = fit$dev.ratio,
              df_path = as.integer(fit$df), coefficients_path = coefficients)
  if (!is.null(intercepts)) res$intercepts_path <- intercepts
  res
}

path_fixtures <- list(
  list(id = "path-gaussian-longley-lasso",      dataset = "longley",    family = "gaussian",    alpha = 1.0),
  list(id = "path-gaussian-longley-enet-user",  dataset = "longley",    family = "gaussian",    alpha = 0.5,
       lambda = c(0.1, 1, 0.05, 0.5)),
  list(id = "path-binomial-wdbc-lasso",         dataset = "wdbc",       family = "binomial",    alpha = 1.0),
  list(id = "path-multinomial-iris-lasso",      dataset = "iris",       family = "multinomial", alpha = 1.0),
  list(id = "path-cox-veteran-lasso",           dataset = "veteran",    family = "cox",         alpha = 1.0),
  list(id = "path-poisson-warpbreaks-lasso",    dataset = "warpbreaks", family = "poisson",     alpha = 1.0),
  list(id = "path-mgaussian-linnerud-lasso",    dataset = "linnerud",   family = "mgaussian",   alpha = 1.0)
)

for (f in path_fixtures) {
  res <- fit_path(f$family, datasets[[f$dataset]], f$alpha, f$lambda)
  golden <- list(id = f$id, dataset = f$dataset, family = f$family, kind = "path",
                 alpha = f$alpha, thresh = 1e-7)
  if (!is.null(f$lambda)) golden$lambda_user <- f$lambda
  golden <- c(golden, res, list(meta = meta))
  path <- file.path(goldens_dir, paste0(f$id, ".json"))
  writeLines(toJSON(golden, digits = NA, auto_unbox = TRUE, pretty = TRUE), path)
  cat("wrote", path, "  (", length(res$lambda_path), "lambdas )\n")
}
