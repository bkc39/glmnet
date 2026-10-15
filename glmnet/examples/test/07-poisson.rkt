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
           rackunit)
  (match-define (vector _ β₁ β₂) (coef fit))
  (check-true (positive? β₁) "rate rises with x₁")
  (check-equal? β₂ 0.0 "lasso zeros the noise coefficient")
  (check-true (< 0.0 (deviance-ratio fit)))
  (check-true (<= (deviance-ratio fit) 1.0))
  (check-true (apply < means) "fitted means increase with x₁")
  (check-= (apply + means) (apply + y) 0.05 "the fitted means track the counts in total"))
