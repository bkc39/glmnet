#lang racket/base

;; Runner + tests for the literate example ../07-poisson.rkt.

(require glmnet
         "../07-poisson.rkt")

(module+ main
  (define r (run-example))
  (printf "poisson intercept    = ~a\n" (poisson-result-intercept r))
  (printf "poisson coefficients = ~a\n" (poisson-result-coefficients r))
  (printf "dev.ratio            = ~a\n" (poisson-result-dev-ratio r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (define b (poisson-result-coefficients r))
  ;; x1 raises the expected count: positive coefficient.
  (check-true (> (vector-ref b 0) 0.0) "rate rises with x1")
  ;; x2 is noise: the L1 penalty zeros it.
  (check-equal? (vector-ref b 1) 0.0 "lasso zeros the noise coefficient")
  ;; dev.ratio in (0, 1].
  (check-true (< 0.0 (poisson-result-dev-ratio r)))
  (check-true (<= (poisson-result-dev-ratio r) 1.0)))
