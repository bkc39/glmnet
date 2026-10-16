#lang racket/base

;; Runner + tests for the literate example ../07-poisson.rkt.

(require glmnet
         "../07-poisson.rkt")

(module+ main
  (println fit)
  (println (coef fit))
  (println means))

(module+ test
  (require racket/match
           rackunit
           (submod "common.rkt" test))
  (match-define (vector _ β₁ β₂) (coef fit))
  (check-true (positive? β₁) "rate rises with x₁")
  (check-equal? β₂ 0.0 "lasso zeros the noise coefficient")
  (check-deviance-ratio fit)
  (check-true (apply < means) "fitted means increase with x₁")
  (for ([μ (in-list means)] [count (in-list y)])
    (check-= μ count 0.6 "each fitted mean tracks its count")))
