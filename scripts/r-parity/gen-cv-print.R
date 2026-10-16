#!/usr/bin/env Rscript

suppressMessages(library(glmnet))
source("scripts/r-parity/cv-print.R")

out_dir <- "glmnet/tests/fixtures"

write_print <- function(name, cv)
  writeLines(cv_print_lines(cv), file.path(out_dir, paste0("cv-print-", name, ".txt")))

vignette_cv <- function(dataset, family, type_measure) {
  e <- new.env()
  data(list = dataset, package = "glmnet", envir = e)
  d <- get(dataset, envir = e)
  x <- as.matrix(d$x)
  cv.glmnet(x, d$y, family = family, foldid = rep(seq_len(10), length.out = nrow(x)),
            type.measure = type_measure)
}

cv_of_columns <- function(lambda, cvm, cvsd, nzero, lambda.min, lambda.1se)
  structure(list(call = quote(cv.glmnet()), name = c(mse = "Mean-Squared Error"),
                 lambda = lambda, cvm = cvm, cvsd = cvsd, nzero = nzero,
                 lambda.min = lambda.min, lambda.1se = lambda.1se),
            class = "cv.glmnet")

write_print("quick-start", vignette_cv("QuickStartExample", "gaussian", "mse"))
write_print("binomial", vignette_cv("BinomialExample", "binomial", "class"))
write_print("synthetic",
            cv_of_columns(lambda = c(2.5e-5, 1.234e-7), cvm = c(12345.678, 0.5),
                          cvsd = c(0, 3.14159e-3), nzero = c(0, 120),
                          lambda.min = 1.234e-7, lambda.1se = 2.5e-5))
write_print("infinite",
            cv_of_columns(lambda = c(0.5, 0.1), cvm = c(1.5, 2.25),
                          cvsd = c(Inf, 0.3), nzero = c(1, 2),
                          lambda.min = 0.5, lambda.1se = 0.1))
write_print("not-a-number",
            cv_of_columns(lambda = c(0.5, 0.1), cvm = c(NaN, 1.2345e-4),
                          cvsd = c(-Inf, 0.3), nzero = c(1, 2),
                          lambda.min = 0.5, lambda.1se = 0.1))
