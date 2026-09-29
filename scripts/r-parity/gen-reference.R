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

## --- the generic interface (#25) ---------------------------------------------
## R's coef(fit, s) and predict(fit, newx, s, type) with the default
## exact = FALSE, for every type the family has, as one entry per s. newx is
## the training X. Class labels become integers (the factor levels are 0..K-1).
## coef_names records how R names coef's result (#26): the row names, which
## are "(Intercept)" and colnames(X), and, for a list, the names of its
## elements (the class levels, or colnames(Y) for the multi-response family).

coef_names <- function(co)
  if (is.list(co)) list(groups = names(co), rows = rownames(co[[1]])) else
    list(rows = rownames(co))

predict_types <- function(family)
  switch(family, binomial = , multinomial = c("link", "response", "class"),
         c("link", "response"))

generic_outputs <- function(fit, family, X, s) {
  per_s <- function(P)               # n x |s|, or n x K x |s| (multinomial, mgaussian)
    lapply(seq_along(s), function(i)
      if (length(dim(P)) == 3) unname(P[, , i]) else unname(P[, i]))
  preds <- list()
  for (type in predict_types(family)) {
    P <- predict(fit, newx = X, s = s, type = type)
    if (type == "class") P <- array(as.integer(P), dim(P))
    preds[[type]] <- per_s(P)
  }
  co <- coef(fit, s = s)
  coefs <- if (is.list(co))          # one matrix per class or response
    lapply(seq_along(s), function(i) unname(lapply(co, function(B) unname(as.numeric(B[, i])))))
  else
    lapply(seq_along(s), function(i) unname(as.numeric(co[, i])))
  list(s = s, coef_s = coefs, coef_names = coef_names(co), predict_s = preds)
}

## A single-lambda fit is a one-lambda path: lambda.interp returns it for any s.
single_s <- function(lambda) c(lambda, 3 * lambda, lambda / 3)

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
                                          exact = TRUE, x = X, y = y)),
       generic      = generic_outputs(fit, "gaussian", X, single_s(lambda)))
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
                                          x = X, y = yf)),   # P(y==1)
       generic      = generic_outputs(fit, "binomial", X, single_s(lambda)))
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
       probabilities = probs[, , 1],                 # n x K
       generic       = generic_outputs(fit, "multinomial", X, single_s(lambda)))
}

fit_cox <- function(X, time, status, alpha, lambda, thresh = 1e-7) {
  sy  <- Surv(time, status)
  fit <- suppressWarnings(glmnet(X, sy, family = "cox", alpha = alpha, lambda = lambda,
                                 standardize = TRUE, thresh = thresh))
  list(coefficients     = as.numeric(coef(fit, s = lambda, exact = TRUE, x = X, y = sy)),  # no intercept
       dev_ratio        = fit$dev.ratio[1],
       lambda_used      = fit$lambda[1],
       linear_predictor = as.numeric(predict(fit, newx = X, s = lambda, type = "link",
                                             exact = TRUE, x = X, y = sy)),
       generic          = generic_outputs(fit, "cox", X, single_s(lambda)))
}

fit_poisson <- function(X, y, alpha, lambda, thresh = 1e-7) {
  fit <- suppressWarnings(glmnet(X, y, family = "poisson", alpha = alpha, lambda = lambda,
                                 standardize = TRUE, intercept = TRUE, thresh = thresh))
  b <- as.numeric(coef(fit, s = lambda, exact = TRUE, x = X, y = y))
  list(intercept    = b[1], coefficients = b[-1],
       dev_ratio    = fit$dev.ratio[1], lambda_used = fit$lambda[1],
       predictions  = as.numeric(predict(fit, newx = X, s = lambda, type = "response",
                                          exact = TRUE, x = X, y = y)),   # fitted means
       generic      = generic_outputs(fit, "poisson", X, single_s(lambda)))
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
       predictions  = preds[, , 1],                 # n x nr
       generic      = generic_outputs(fit, "mgaussian", X, single_s(lambda)))
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
## 1e-4), or a user sequence when a fixture sets `lambda`. A fixture may also set
## `nlambda` and `lambda_min_ratio`, and `nobs` to use only the first nobs
## observations. The golden records every fitted lambda (after R's fix.lam) and,
## per lambda, the intercepts, coefficients, deviance ratio and df.

first_rows <- function(d, n) {
  lapply(d, function(v) if (is.matrix(v)) v[seq_len(n), , drop = FALSE] else v[seq_len(n)])
}

path_fit <- function(family, d, alpha, lambda = NULL, nlambda = NULL,
                     lambda_min_ratio = NULL, thresh = 1e-7) {
  y <- switch(family,
    binomial    = factor(d$y, levels = c(0, 1)),
    multinomial = factor(d$y),
    cox         = Surv(d$time, d$status),
    mgaussian   = d$Y,
    d$y)
  args <- list(d$X, y, family = family, alpha = alpha, standardize = TRUE,
               thresh = thresh)
  if (!is.null(lambda)) args$lambda <- lambda
  if (!is.null(nlambda)) args$nlambda <- nlambda
  if (!is.null(lambda_min_ratio)) args$lambda.min.ratio <- lambda_min_ratio
  if (family == "binomial") args$type.logistic <- "Newton"
  if (family == "multinomial") args$type.multinomial <- "ungrouped"
  if (family == "mgaussian") args$standardize.response <- FALSE
  suppressWarnings(do.call(glmnet, args))
}

fit_path <- function(family, d, alpha, lambda = NULL, nlambda = NULL,
                     lambda_min_ratio = NULL, thresh = 1e-7) {
  fit <- path_fit(family, d, alpha, lambda, nlambda, lambda_min_ratio, thresh)
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
  list(id = "path-gaussian-longley-lasso-ratio", dataset = "longley",   family = "gaussian",    alpha = 1.0,
       nlambda = 20, lambda_min_ratio = 0.05),
  list(id = "path-gaussian-longley5-lasso",     dataset = "longley",    family = "gaussian",    alpha = 1.0,
       nobs = 5),                                                     # n < p: ratio 0.01
  list(id = "path-binomial-wdbc-lasso",         dataset = "wdbc",       family = "binomial",    alpha = 1.0),
  list(id = "path-multinomial-iris-lasso",      dataset = "iris",       family = "multinomial", alpha = 1.0),
  list(id = "path-cox-veteran-lasso",           dataset = "veteran",    family = "cox",         alpha = 1.0),
  list(id = "path-poisson-warpbreaks-lasso",    dataset = "warpbreaks", family = "poisson",     alpha = 1.0),
  list(id = "path-mgaussian-linnerud-lasso",    dataset = "linnerud",   family = "mgaussian",   alpha = 1.0)
)

for (f in path_fixtures) {
  d <- datasets[[f$dataset]]
  if (!is.null(f[["nobs"]])) d <- first_rows(d, f[["nobs"]])
  res <- fit_path(f$family, d, f$alpha, f[["lambda"]], f[["nlambda"]], f[["lambda_min_ratio"]])
  golden <- list(id = f$id, dataset = f$dataset, family = f$family, kind = "path",
                 alpha = f$alpha, thresh = 1e-7)
  if (!is.null(f[["lambda"]])) golden$lambda_user <- f[["lambda"]]
  if (!is.null(f[["nlambda"]])) golden$nlambda <- f[["nlambda"]]
  if (!is.null(f[["lambda_min_ratio"]])) golden$lambda_min_ratio <- f[["lambda_min_ratio"]]
  if (!is.null(f[["nobs"]])) golden$nobs <- f[["nobs"]]
  golden <- c(golden, res, list(meta = meta))
  path <- file.path(goldens_dir, paste0(f$id, ".json"))
  writeLines(toJSON(golden, digits = NA, auto_unbox = TRUE, pretty = TRUE), path)
  cat("wrote", path, "  (", length(res$lambda_path), "lambdas )\n")
}

## --- predict and coef along a path (#25) --------------------------------------
## The generic interface on each path fixture above, at four s given in this
## (unsorted) order: a fitted lambda, a point 30% of the way from one fitted
## lambda to the next, and points above the largest and below the smallest
## fitted lambda, which lambda.interp clamps to the ends of the path. `print`
## holds the lines of R's print.glmnet table, from its header on.

print_table <- function(fit) {
  out <- capture.output(print(fit))
  out[grep("^ +Df +%Dev +Lambda$", out)[1]:length(out)]
}

for (f in path_fixtures) {
  d   <- datasets[[f$dataset]]
  if (!is.null(f[["nobs"]])) d <- first_rows(d, f[["nobs"]])
  fit <- path_fit(f$family, d, f$alpha, f[["lambda"]], f[["nlambda"]], f[["lambda_min_ratio"]])
  lam <- fit$lambda
  L   <- length(lam)
  i   <- ceiling(L / 2)
  s   <- c(lam[i], 0.7 * lam[i] + 0.3 * lam[i + 1], 2 * lam[1], lam[L] / 2)
  golden <- list(id = sub("^path-", "predict-", f$id), dataset = f$dataset,
                 family = f$family, kind = "predict", alpha = f$alpha, thresh = 1e-7)
  if (!is.null(f[["lambda"]])) golden$lambda_user <- f[["lambda"]]
  if (!is.null(f[["nlambda"]])) golden$nlambda <- f[["nlambda"]]
  if (!is.null(f[["lambda_min_ratio"]])) golden$lambda_min_ratio <- f[["lambda_min_ratio"]]
  if (!is.null(f[["nobs"]])) golden$nobs <- f[["nobs"]]
  golden <- c(golden, generic_outputs(fit, f$family, d$X, s),
              list(print = print_table(fit), meta = meta))
  path <- file.path(goldens_dir, paste0(golden$id, ".json"))
  writeLines(toJSON(golden, digits = NA, auto_unbox = TRUE, pretty = TRUE), path)
  cat("wrote", path, "  ( s =", signif(s, 4), ")\n")
}

## --- cross-validation (#27) ---------------------------------------------------
## R's cv.glmnet with a fixed foldid, for every family and every type.measure
## the bindings ship. The fold ids are drawn here from a seed and recorded in
## the golden (1-based, as R numbers folds), so both sides use the same folds.
## For the multinomial the seed is advanced until no training fold has two
## largest classes of equal size: at the largest lambdas the folds' fits are
## intercept-only, their class probabilities would then tie up to rounding
## noise, and neither side's predicted class would mean anything.

cv_fold_ids <- function(n, nfolds, seed, y = NULL) {
  tied <- function(foldid)
    any(sapply(seq_len(nfolds), function(k) {
      counts <- table(factor(y[foldid != k], levels = sort(unique(y))))
      sum(counts == max(counts)) > 1
    }))
  repeat {
    set.seed(seed)
    foldid <- sample(rep(seq_len(nfolds), length = n))
    if (is.null(y) || !tied(foldid)) return(foldid)
    seed <- seed + 1
  }
}

## coef(cv, s) at one named s: a vector, or one vector per class or response.
cv_coef <- function(cv, s) {
  co <- coef(cv, s = s)
  if (is.list(co)) unname(lapply(co, function(B) unname(as.numeric(B[, 1]))))
  else unname(as.numeric(co[, 1]))
}

cv_fixtures <- list(
  list(id = "cv-gaussian-longley-mse",           dataset = "longley",    family = "gaussian",    nfolds = 4,  seed = 1, type_measure = "mse"),
  list(id = "cv-gaussian-longley-deviance",      dataset = "longley",    family = "gaussian",    nfolds = 4,  seed = 1, type_measure = "deviance"),
  list(id = "cv-gaussian-longley-mae",           dataset = "longley",    family = "gaussian",    nfolds = 4,  seed = 1, type_measure = "mae"),
  list(id = "cv-gaussian-longley-mse-ungrouped", dataset = "longley",    family = "gaussian",    nfolds = 4,  seed = 1, type_measure = "mse", grouped = FALSE),
  list(id = "cv-gaussian-longley-mse-8folds",    dataset = "longley",    family = "gaussian",    nfolds = 8,  seed = 1, type_measure = "mse"),
  list(id = "cv-gaussian-longley-enet-user",     dataset = "longley",    family = "gaussian",    nfolds = 4,  seed = 1, type_measure = "mse",
       alpha = 0.5, lambda = c(0.05, 1, 0.5, 0.2, 0.1, 0.02, 0.01)),
  list(id = "cv-binomial-wdbc-deviance",         dataset = "wdbc",       family = "binomial",    nfolds = 10, seed = 3, type_measure = "deviance"),
  list(id = "cv-binomial-wdbc-class",            dataset = "wdbc",       family = "binomial",    nfolds = 10, seed = 3, type_measure = "class"),
  list(id = "cv-binomial-wdbc-auc",              dataset = "wdbc",       family = "binomial",    nfolds = 10, seed = 3, type_measure = "auc"),
  list(id = "cv-binomial-wdbc-mse",              dataset = "wdbc",       family = "binomial",    nfolds = 10, seed = 3, type_measure = "mse"),
  list(id = "cv-binomial-wdbc-mae",              dataset = "wdbc",       family = "binomial",    nfolds = 10, seed = 3, type_measure = "mae"),
  list(id = "cv-multinomial-iris-deviance",      dataset = "iris",       family = "multinomial", nfolds = 10, seed = 2, type_measure = "deviance"),
  list(id = "cv-multinomial-iris-class",         dataset = "iris",       family = "multinomial", nfolds = 10, seed = 2, type_measure = "class"),
  list(id = "cv-multinomial-iris-mse",           dataset = "iris",       family = "multinomial", nfolds = 10, seed = 2, type_measure = "mse"),
  list(id = "cv-multinomial-iris-mae",           dataset = "iris",       family = "multinomial", nfolds = 10, seed = 2, type_measure = "mae"),
  list(id = "cv-cox-veteran-deviance",           dataset = "veteran",    family = "cox",         nfolds = 5,  seed = 4, type_measure = "deviance"),
  list(id = "cv-cox-veteran-deviance-ungrouped", dataset = "veteran",    family = "cox",         nfolds = 5,  seed = 4, type_measure = "deviance", grouped = FALSE),
  list(id = "cv-cox-veteran-C",                  dataset = "veteran",    family = "cox",         nfolds = 5,  seed = 4, type_measure = "C"),
  list(id = "cv-poisson-warpbreaks-deviance",    dataset = "warpbreaks", family = "poisson",     nfolds = 5,  seed = 5, type_measure = "deviance"),
  list(id = "cv-poisson-warpbreaks-mse",         dataset = "warpbreaks", family = "poisson",     nfolds = 5,  seed = 5, type_measure = "mse"),
  list(id = "cv-poisson-warpbreaks-mae",         dataset = "warpbreaks", family = "poisson",     nfolds = 5,  seed = 5, type_measure = "mae"),
  list(id = "cv-mgaussian-linnerud-mse",         dataset = "linnerud",   family = "mgaussian",   nfolds = 5,  seed = 6, type_measure = "mse"),
  list(id = "cv-mgaussian-linnerud-deviance",    dataset = "linnerud",   family = "mgaussian",   nfolds = 5,  seed = 6, type_measure = "deviance"),
  list(id = "cv-mgaussian-linnerud-mae",         dataset = "linnerud",   family = "mgaussian",   nfolds = 5,  seed = 6, type_measure = "mae")
)

for (f in cv_fixtures) {
  d       <- datasets[[f$dataset]]
  alpha   <- if (is.null(f$alpha)) 1.0 else f$alpha
  grouped <- if (is.null(f$grouped)) TRUE else f$grouped
  foldid  <- cv_fold_ids(nrow(d$X), f$nfolds, f$seed,
                         if (f$family == "multinomial") d$y else NULL)
  y <- switch(f$family,
    binomial    = factor(d$y, levels = c(0, 1)),
    multinomial = factor(d$y),
    cox         = Surv(d$time, d$status),
    mgaussian   = d$Y,
    d$y)
  args <- list(d$X, y, family = f$family, alpha = alpha, standardize = TRUE, thresh = 1e-7,
               foldid = foldid, type.measure = f$type_measure, grouped = grouped)
  if (!is.null(f$lambda)) args$lambda <- f$lambda
  if (f$family == "binomial") args$type.logistic <- "Newton"
  if (f$family == "multinomial") args$type.multinomial <- "ungrouped"
  if (f$family == "mgaussian") args$standardize.response <- FALSE
  cv <- suppressWarnings(do.call(cv.glmnet, args))
  golden <- list(id = f$id, dataset = f$dataset, family = f$family, kind = "cv",
                 alpha = alpha, thresh = 1e-7, type_measure = f$type_measure,
                 grouped = grouped, foldid = foldid)
  if (!is.null(f$lambda)) golden$lambda_user <- f$lambda
  golden <- c(golden,
              list(measure = names(cv$name), name = unname(cv$name),
                   lambda = cv$lambda, cvm = unname(cv$cvm), cvsd = unname(cv$cvsd),
                   cvup = unname(cv$cvup), cvlo = unname(cv$cvlo),
                   nzero = as.integer(cv$nzero),
                   lambda_min = cv$lambda.min, lambda_1se = cv$lambda.1se,
                   index_min = cv$index[1, 1], index_1se = cv$index[2, 1],
                   coef_min = cv_coef(cv, "lambda.min"), coef_1se = cv_coef(cv, "lambda.1se"),
                   coef_names = coef_names(coef(cv, s = "lambda.min")),
                   meta = meta))
  path <- file.path(goldens_dir, paste0(f$id, ".json"))
  writeLines(toJSON(golden, digits = NA, auto_unbox = TRUE, pretty = TRUE), path)
  cat("wrote", path, "  ( lambda.min", signif(cv$lambda.min, 4),
      "lambda.1se", signif(cv$lambda.1se, 4), ")\n")
}

## --- formulas (#53) -----------------------------------------------------------
## R's formula algebra on mtcars (R's own copy; the Racket copy is
## glmnet/examples/data/mtcars.rkt) and longley (the committed CSV). For each
## formula: terms()'s term labels, intercept and factors attribute (one entry
## per term, from each of its variables to 1 or 2); model.matrix() without its
## intercept column, as column names and columns, and the warnings it gave; and
## glmnet on that matrix with intercept = attr(terms, "intercept"), at a user
## lambda sequence, through the generic outputs at `s`. `rkt` holds the Racket
## spellings of the formula, which must all give R's matrix. `names` maps R's
## name of each transform to the Racket one, its source: log(hp) is
## "(log hp)", and an interaction's name maps each of its variables.

formula_data <- list(mtcars = datasets::mtcars,
                     longley = read.csv(file.path(data_dir, "longley.csv")))

formula_fixtures <- list(
  list(id = "formula-mtcars-cross", dataset = "mtcars", r = "mpg ~ wt * hp",
       rkt = c("(~ mpg (* wt hp))", "(mpg . ~ . wt * hp)", "(~ mpg wt hp (: wt hp))")),
  list(id = "formula-mtcars-interact", dataset = "mtcars", r = "mpg ~ hp:wt + qsec",
       rkt = c("(~ mpg (: hp wt) qsec)", "(mpg . ~ . hp : wt + qsec)")),
  list(id = "formula-mtcars-power-sum", dataset = "mtcars", r = "mpg ~ (wt + hp + qsec)^2",
       rkt = c("(~ mpg (^ (+ wt hp qsec) 2))", "(mpg . ~ . (wt + hp + qsec) ^ 2)")),
  list(id = "formula-mtcars-power-column", dataset = "mtcars", r = "mpg ~ wt^2 + hp",
       rkt = c("(~ mpg (^ wt 2) hp)", "(mpg . ~ . wt ^ 2 + hp)", "(~ mpg wt hp)")),
  list(id = "formula-mtcars-cross-sums", dataset = "mtcars", r = "mpg ~ (wt + hp) * (qsec + drat)",
       rkt = c("(~ mpg (* (+ wt hp) (+ qsec drat)))", "(mpg . ~ . (wt + hp) * (qsec + drat))")),
  list(id = "formula-mtcars-cross-empty-left", dataset = "mtcars", r = "mpg ~ 1*wt + hp + qsec",
       rkt = c("(mpg . ~ . 1 * wt + hp + qsec)", "(~ mpg (* 1 wt) hp qsec)")),
  list(id = "formula-mtcars-remove", dataset = "mtcars", r = "mpg ~ (wt + hp + qsec)^2 - wt:hp",
       rkt = c("(~ mpg (- (^ (+ wt hp qsec) 2) (: wt hp)))", "(mpg . ~ . (wt + hp + qsec) ^ 2 - wt : hp)")),
  list(id = "formula-mtcars-remove-absent", dataset = "mtcars", r = "mpg ~ wt * hp - qsec",
       rkt = c("(~ mpg (- (* wt hp) qsec))", "(mpg . ~ . wt * hp - qsec)")),
  list(id = "formula-mtcars-zero", dataset = "mtcars", r = "mpg ~ 0 + wt + hp",
       rkt = c("(~ mpg 0 wt hp)", "(mpg . ~ . 0 + wt + hp)")),
  list(id = "formula-mtcars-minus-one", dataset = "mtcars", r = "mpg ~ wt * hp - 1",
       rkt = c("(~ mpg (- (* wt hp) 1))", "(mpg . ~ . wt * hp - 1)", "(mpg . ~ . - 1 + wt * hp)")),
  list(id = "formula-mtcars-one", dataset = "mtcars", r = "mpg ~ 1 + wt + qsec",
       rkt = c("(~ mpg 1 wt qsec)", "(mpg . ~ . 1 + wt + qsec)")),
  list(id = "formula-mtcars-all", dataset = "mtcars", r = "mpg ~ .",
       rkt = c("(~ mpg all)")),
  list(id = "formula-mtcars-all-minus", dataset = "mtcars", r = "mpg ~ . - cyl - disp",
       rkt = c("(~ mpg (- all cyl disp))", "(mpg . ~ . all - cyl - disp)")),
  list(id = "formula-mtcars-all-interact", dataset = "mtcars", r = "mpg ~ .:wt - wt",
       rkt = c("(~ mpg (- (: all wt) wt))", "(mpg . ~ . all : wt - wt)")),
  list(id = "formula-mtcars-response-rhs", dataset = "mtcars", r = "mpg ~ 1 + wt + mpg + wt*mpg",
       rkt = c("(mpg . ~ . 1 + wt + mpg + (* wt mpg))", "(~ mpg 1 wt mpg (* wt mpg))")),
  list(id = "formula-mtcars-response-main", dataset = "mtcars", r = "mpg ~ wt + hp + mpg",
       rkt = c("(~ mpg wt hp mpg)", "(mpg . ~ . wt + hp + mpg)")),
  list(id = "formula-longley-power-all", dataset = "longley", r = "Employed ~ .^2",
       rkt = c("(~ Employed (^ all 2))", "(Employed . ~ . all ^ 2)"), lambda = c(1, 0.5, 0.2)),
  list(id = "formula-longley-no-intercept", dataset = "longley", r = "Employed ~ GNP * Population - 1",
       rkt = c("(~ Employed (- (* GNP Population) 1))", "(Employed . ~ . - 1 + GNP * Population)")),
  list(id = "formula-longley-mixed", dataset = "longley", r = "Employed ~ Year + GNP:Unemployed:Armed.Forces + Unemployed",
       rkt = c("(Employed . ~ . Year + (: GNP Unemployed Armed.Forces) + Unemployed)")),
  ## Transforms (#53, leg 2).
  list(id = "formula-mtcars-log", dataset = "mtcars", r = "mpg ~ log(hp) + wt",
       rkt = c("(mpg . ~ . (log hp) + wt)", "(~ mpg (log hp) wt)"),
       names = list(`log(hp)` = "(log hp)")),
  list(id = "formula-mtcars-sqrt", dataset = "mtcars", r = "mpg ~ sqrt(disp) + wt",
       rkt = c("(mpg . ~ . (sqrt disp) + wt)", "(~ mpg (sqrt disp) wt)"),
       names = list(`sqrt(disp)` = "(sqrt disp)")),
  list(id = "formula-mtcars-exp", dataset = "mtcars", r = "mpg ~ exp(wt) + hp",
       rkt = c("(mpg . ~ . (exp wt) + hp)"),
       names = list(`exp(wt)` = "(exp wt)")),
  list(id = "formula-mtcars-square-sqr", dataset = "mtcars", r = "mpg ~ hp + I(hp^2)",
       rkt = c("(mpg . ~ . hp + (sqr hp))", "(~ mpg hp (sqr hp))"),
       names = list(`I(hp^2)` = "(sqr hp)")),
  list(id = "formula-mtcars-square-expt", dataset = "mtcars", r = "mpg ~ hp + I(hp^2)",
       rkt = c("(mpg . ~ . hp + (I (expt hp 2)))"),
       names = list(`I(hp^2)` = "(I (expt hp 2))")),
  list(id = "formula-mtcars-product", dataset = "mtcars", r = "mpg ~ wt + I(wt * hp)",
       rkt = c("(mpg . ~ . wt + (I (* wt hp)))"),
       names = list(`I(wt * hp)` = "(I (* wt hp))")),
  list(id = "formula-mtcars-ratio", dataset = "mtcars", r = "mpg ~ I(hp/wt) + qsec",
       rkt = c("(mpg . ~ . (I (/ hp wt)) + qsec)", "(~ mpg (I (/ hp wt)) qsec)"),
       names = list(`I(hp/wt)` = "(I (/ hp wt))")),
  list(id = "formula-mtcars-log-cross", dataset = "mtcars", r = "mpg ~ log(hp) * wt",
       rkt = c("(mpg . ~ . (log hp) * wt)", "(~ mpg (* (log hp) wt))"),
       names = list(`log(hp)` = "(log hp)")),
  list(id = "formula-mtcars-log-interact", dataset = "mtcars", r = "mpg ~ log(hp):wt + qsec",
       rkt = c("(mpg . ~ . (log hp) : wt + qsec)"),
       names = list(`log(hp)` = "(log hp)")),
  list(id = "formula-mtcars-power-trap", dataset = "mtcars", r = "mpg ~ hp + hp^2 + wt",
       rkt = c("(mpg . ~ . hp + hp ^ 2 + wt)", "(~ mpg hp (^ hp 2) wt)", "(~ mpg hp wt)")),
  ## pi is not a column, so it is R's pi and racket/math's.
  list(id = "formula-mtcars-environment", dataset = "mtcars", r = "mpg ~ wt + I(disp/pi)",
       rkt = c("(mpg . ~ . wt + (I (/ disp pi)))"),
       names = list(`I(disp/pi)` = "(I (/ disp pi))")),
  list(id = "formula-mtcars-log-response", dataset = "mtcars", r = "mpg ~ log(mpg) + wt",
       rkt = c("(mpg . ~ . (log mpg) + wt)"),
       names = list(`log(mpg)` = "(log mpg)"))
)

for (fx in formula_fixtures) {
  d  <- formula_data[[fx$dataset]]
  fm <- as.formula(fx$r)
  tt <- terms(fm, data = d)
  warnings <- character(0)
  mm <- withCallingHandlers(model.matrix(fm, d), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  intercept <- attr(tt, "intercept") == 1
  x  <- if (intercept) mm[, -1, drop = FALSE] else mm
  y  <- d[[all.vars(fm)[1]]]
  lambda <- if (is.null(fx$lambda)) c(2, 0.5, 0.1, 0.02) else fx$lambda
  fit <- suppressWarnings(glmnet(x, y, family = "gaussian", alpha = 1, lambda = lambda,
                                 standardize = TRUE, intercept = intercept, thresh = 1e-7))
  fac <- attr(tt, "factors")
  factors <- lapply(seq_len(ncol(fac)), function(j) {
    nz <- fac[, j] != 0
    setNames(as.list(fac[nz, j]), rownames(fac)[nz])
  })
  s <- c(lambda[2], 0.6 * lambda[2] + 0.4 * lambda[3])
  golden <- list(id = fx$id, kind = "formula", dataset = fx$dataset, family = "gaussian",
                 r_formula = fx$r, rkt = I(fx$rkt),
                 names = if (is.null(fx$names)) setNames(list(), character(0)) else fx$names,
                 alpha = 1, thresh = 1e-7,
                 lambda_user = lambda,
                 term_labels = I(attr(tt, "term.labels")), intercept = intercept,
                 factors = factors,
                 column_names = I(colnames(x)),
                 columns = unname(lapply(seq_len(ncol(x)), function(j) unname(x[, j]))),
                 warnings = I(warnings),
                 generic = generic_outputs(fit, "gaussian", x, s),
                 meta = meta)
  path <- file.path(goldens_dir, paste0(fx$id, ".json"))
  writeLines(toJSON(golden, digits = NA, auto_unbox = TRUE, pretty = TRUE), path)
  cat("wrote", path, "  (", ncol(x), "columns )\n")
}
