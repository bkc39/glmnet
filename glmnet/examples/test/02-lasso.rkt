#lang racket/base

;; Runner + tests for the literate example ../02-lasso.rkt.

(require glmnet
         "../02-lasso.rkt")

(module+ main
  (define r (run-example))
  (printf "lasso intercept    = ~a\n" (elnet-result-intercept r))
  (printf "lasso coefficients = ~a\n" (elnet-result-coefficients r))
  (printf "R^2                = ~a\n" (elnet-result-r-squared r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (define b (elnet-result-coefficients r))
  ;; The irrelevant predictor x3 is selected out exactly.
  (check-equal? (vector-ref b 2) 0.0 "lasso zeros the irrelevant coefficient")
  ;; The two real predictors survive with the expected signs.
  (check-true (> (vector-ref b 0) 0.0))
  (check-true (< (vector-ref b 1) 0.0)))
