#lang racket/base

;; Runner + tests for the literate example ../00-ols.rkt.

(require glmnet
         "../00-ols.rkt")

(module+ main
  (println fit)
  (println (coef fit))
  (println fitted))

(module+ test
  (require racket/match
           rackunit)
  (match-define (vector a0 b1 b2) (coef fit))
  (check-= a0 1.0 1e-4)
  (check-= b1 2.0 1e-4)
  (check-= b2 -1.0 1e-4)
  (check-= (deviance-ratio fit) 1.0 1e-6)
  (for ([fitted-value (in-list fitted)] [observed (in-list y)])
    (check-= fitted-value observed 1e-4)))
