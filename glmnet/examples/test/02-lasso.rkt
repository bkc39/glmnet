#lang racket/base

;; Runner + tests for the literate example ../02-lasso.rkt.

(require glmnet
         "../02-lasso.rkt")

(module+ main
  (println fit)
  (println (coef fit))
  (println kept))

(module+ test
  (require racket/match
           rackunit)
  (match-define (vector _ b1 b2 b3) (coef fit))
  (check-equal? b3 0.0 "lasso zeros the irrelevant coefficient")
  (check-true (positive? b1))
  (check-true (negative? b2))
  (check-equal? kept '(1 2)))
