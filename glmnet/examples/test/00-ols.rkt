#lang racket/base

;; Runner + tests for the literate example ../00-ols.rkt.

(require glmnet
         "../00-ols.rkt")

(module+ main
  (define r (run-example))
  (printf "intercept    = ~a\n" (elnet-result-intercept r))
  (printf "coefficients = ~a\n" (elnet-result-coefficients r))
  (printf "R^2          = ~a\n" (elnet-result-r-squared r))
  (printf "lambda       = ~a\n" (elnet-result-lambda r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (check-= (elnet-result-intercept r) 1.0 1e-4)
  (check-= (vector-ref (elnet-result-coefficients r) 0) 2.0 1e-4)
  (check-= (vector-ref (elnet-result-coefficients r) 1) -1.0 1e-4)
  (check-= (elnet-result-r-squared r) 1.0 1e-6))
