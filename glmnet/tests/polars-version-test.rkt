#lang racket/base

;; This file does not require glmnet/data/polars, which does not compile
;; against a polars too old for it, so that it can say why.

(module+ test
  (require rackunit)

  (test-case "the installed polars has the bulk conversions glmnet/data/polars needs"
    (for ([name (in-list '(dataframe->f64vector dataframe->columns))])
      (check-not-false
       (dynamic-require 'polars name (lambda () #f))
       (format (string-append
                "the installed polars predates ~a (rkt-polars 06d9297), which "
                "glmnet/data/polars needs; update it with `raco pkg update polars`. Its version "
                "does not say so yet: see https://github.com/bkc39/rkt-polars/issues/144")
               name)))))
