#lang racket/base

;; Runner + tests for the literate example ../01-ridge.rkt.

(require glmnet
         "../01-ridge.rkt")

(module+ main
  (define r (run-example))
  (printf "ridge intercept    = ~a\n" (elnet-result-intercept r))
  (printf "ridge coefficients = ~a\n" (elnet-result-coefficients r))
  (printf "R^2                = ~a\n" (elnet-result-r-squared r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (define b (elnet-result-coefficients r))
  ;; Ridge shrinks but never selects: every coefficient stays nonzero.
  (check-true (for/and ([bi (in-vector b)]) (not (zero? bi)))
              "ridge keeps all coefficients nonzero")
  ;; The leading coefficient is shrunk below its OLS value (~2.0).
  (check-true (< (abs (vector-ref b 0)) 2.0)
              "ridge shrinks the leading coefficient"))
