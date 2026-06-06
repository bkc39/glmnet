#lang racket/base

;; Runner + tests for the literate example ../06-cox.rkt.

(require glmnet
         "../06-cox.rkt")

(module+ main
  (define r (run-example))
  (printf "cox coefficients = ~a\n" (cox-result-coefficients r))
  (printf "dev.ratio        = ~a\n" (cox-result-dev-ratio r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (define b (cox-result-coefficients r))
  ;; x1 is a risk factor: positive log-hazard coefficient.
  (check-true (> (vector-ref b 0) 0.0) "risk coefficient is positive")
  ;; x2 is noise: the L1 penalty zeros it.
  (check-equal? (vector-ref b 1) 0.0 "lasso zeros the noise coefficient")
  ;; dev.ratio in (0, 1].
  (check-true (< 0.0 (cox-result-dev-ratio r)))
  (check-true (<= (cox-result-dev-ratio r) 1.0)))
