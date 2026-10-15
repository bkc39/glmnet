#!/usr/bin/env Rscript
## R's print.cv.glmnet output, verbatim, for the committed printing goldens of
## glmnet/tests/cv-test.rkt. Run from the repo root inside `nix develop .#r-parity`:
##   Rscript scripts/r-parity/gen-cv-print.R
## The vignette datasets are cross-validated on the folds rep(1:10, length = n);
## `synthetic` is a cv.glmnet object built by hand, with a lambda column that
## prints in scientific notation and a cvm that zapsmall rounds to 0.

suppressMessages(library(glmnet))

out_dir <- "glmnet/tests/fixtures"

write_print <- function(name, cv)
  writeLines(capture.output(print(cv)), file.path(out_dir, paste0("cv-print-", name, ".txt")))

vignette_cv <- function(dataset, family, type_measure) {
  e <- new.env()
  data(list = dataset, package = "glmnet", envir = e)
  d <- get(dataset, envir = e)
  x <- as.matrix(d$x)
  cv.glmnet(x, d$y, family = family, foldid = rep(seq_len(10), length.out = nrow(x)),
            type.measure = type_measure)
}

write_print("quick-start", vignette_cv("QuickStartExample", "gaussian", "mse"))
write_print("binomial", vignette_cv("BinomialExample", "binomial", "class"))
write_print("synthetic",
            structure(list(call = quote(cv.glmnet()), name = c(mse = "Mean-Squared Error"),
                           lambda = c(2.5e-5, 1.234e-7), cvm = c(12345.678, 0.5),
                           cvsd = c(0, 3.14159e-3), nzero = c(0, 120),
                           lambda.min = 1.234e-7, lambda.1se = 2.5e-5),
                      class = "cv.glmnet"))
