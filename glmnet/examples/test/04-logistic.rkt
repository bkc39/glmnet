#lang racket/base

;; Runner + tests for the literate example ../04-logistic.rkt.

(require glmnet
         "../04-logistic.rkt")

(module+ main
  (define r (run-example))
  (printf "logistic intercept    = ~a\n" (logistic-result-intercept r))
  (printf "logistic coefficients = ~a\n" (logistic-result-coefficients r))
  (printf "dev.ratio             = ~a\n" (logistic-result-dev-ratio r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (define b (logistic-result-coefficients r))
  ;; The noise predictor x3 is selected out exactly by the L1 penalty.
  (check-equal? (vector-ref b 2) 0.0 "lasso logistic zeros the noise coefficient")
  ;; The two informative predictors keep their opposite signs.
  (check-true (> (vector-ref b 0) 0.0) "x1 has positive log-odds weight")
  (check-true (< (vector-ref b 1) 0.0) "x2 has negative log-odds weight")
  ;; dev.ratio is a fraction of null deviance explained, in (0, 1].
  (check-true (< 0.0 (logistic-result-dev-ratio r)))
  (check-true (<= (logistic-result-dev-ratio r) 1.0)))
