#!/usr/bin/env Rscript
## Export R glmnet's example datasets, and R's mtcars and iris, as the CSV
## files that glmnet/datasets loads (glmnet/datasets/*.csv). Run from the repo
## root with the pinned R:
##
##   nix develop .#r-parity -c Rscript scripts/export-datasets.R
##
## Each glmnet dataset is one file: the columns of x, named V1, V2, ... as R's
## coef names them, then y: `y` for a vector or a one-column matrix, R's column
## names (CoxExample's time and status), or y1, y2, ... (MultiGaussianExample).
## SparseExample's x is written dense. A number is written with 17 significant
## digits, which identify a double and which C's printf rounds correctly, so
## that a correctly rounded reader, such as Racket's, reads back R's double.
## Fewer digits are not safe to choose by reading them back in R, whose reader
## is not correctly rounded: it reads -0.827587926753309 as the double
## -0.82758792675330906, where a correctly rounded reader reads
## -0.82758792675330894. The parity goldens (dataset-*) check every number of
## every file against R's own, bit for bit. Lines end with LF, as
## glmnet/data/csv writes them.

suppressMessages(library(glmnet))

out_dir <- "glmnet/datasets"

exact_decimal <- function(v) {
  out <- sprintf("%.17g", v)
  stopifnot(all(as.numeric(out) == v))
  out
}

quote_if_needed <- function(s) {
  needs <- grepl("[\",\r\n]", s) | s == ""
  s[needs] <- paste0("\"", gsub("\"", "\"\"", s[needs]), "\"")
  s
}

format_column <- function(v) {
  stopifnot(!anyNA(v))
  if (is.character(v)) quote_if_needed(v) else exact_decimal(as.double(v))
}

write_dataset <- function(d, name) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  file <- file.path(out_dir, paste0(name, ".csv"))
  lines <- c(paste(quote_if_needed(names(d)), collapse = ","),
             do.call(paste, c(unname(lapply(d, format_column)), sep = ",")))
  con <- file(file, open = "wb")
  writeLines(lines, con, sep = "\n")
  close(con)
  back <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  stopifnot(identical(names(back), names(d)), nrow(back) == nrow(d))
  for (k in names(d)) {
    if (is.character(d[[k]])) stopifnot(identical(back[[k]], d[[k]]))
    else stopifnot(identical(as.double(back[[k]]), as.double(d[[k]])))
  }
  cat(sprintf("wrote %-40s %4d x %2d  %8d bytes\n", file, nrow(d), ncol(d), file.size(file)))
}

glmnet_dataset <- function(name) {
  e <- new.env()
  data(list = name, package = "glmnet", envir = e)
  d <- get(name, envir = e)
  x <- as.matrix(d$x)
  y <- as.matrix(d$y)
  y_names <- if (!is.null(colnames(y))) colnames(y)
             else if (ncol(y) == 1) "y"
             else paste0("y", seq_len(ncol(y)))
  columns <- c(lapply(seq_len(ncol(x)), function(j) x[, j]),
               lapply(seq_len(ncol(y)), function(j) y[, j]))
  names(columns) <- c(paste0("V", seq_len(ncol(x))), y_names)
  as.data.frame(columns, optional = TRUE)
}

glmnet_datasets <- c("QuickStartExample", "BinomialExample", "MultinomialExample",
                     "PoissonExample", "CoxExample", "MultiGaussianExample", "SparseExample")

## Every exported dataset as the data frame its file holds, by file name.
dataset_frames <- function() {
  frames <- lapply(glmnet_datasets, glmnet_dataset)
  names(frames) <- glmnet_datasets
  c(frames, list(mtcars = datasets::mtcars,
                 iris = transform(datasets::iris, Species = as.character(Species))))
}

## scripts/r-parity/gen-reference.R sources this file for dataset_frames().
if (sys.nframe() == 0L) {
  frames <- dataset_frames()
  for (name in names(frames)) write_dataset(frames[[name]], name)
  cat(R.version.string, "with glmnet", as.character(packageVersion("glmnet")), "\n")
}
