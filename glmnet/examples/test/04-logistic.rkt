#lang racket/base

;; Runner + tests for the literate example ../04-logistic.rkt.

(require glmnet
         "../04-logistic.rkt")

(module+ main
  (println fit)
  (println (coef fit))
  (println probabilities)
  (println labels))

(module+ test
  (require racket/match
           rackunit
           (submod "common.rkt" test))
  (match-define (vector _ β₁ β₂ β₃) (coef fit))
  (check-equal? β₃ 0.0 "lasso logistic zeros the noise coefficient")
  (check-true (positive? β₁) "x₁ has positive log-odds weight")
  (check-true (negative? β₂) "x₂ has negative log-odds weight")
  (check-deviance-ratio fit)
  (for ([p (in-list probabilities)] [class (in-list y)])
    (check-equal? (> p 0.5) (= class 1) "low for class 0, high for class 1"))
  (check-equal? labels y "every training point correct"))
