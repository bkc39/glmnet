cv_print_lines <- function(cv) {
  out <- sub(" +$", "", capture.output(print(cv)))
  out <- out[nzchar(out)]
  out[grep("^Measure:", out)[1]:length(out)]
}
