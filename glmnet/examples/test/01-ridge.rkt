#lang racket/base

;; Runner + tests for the literate example ../01-ridge.rkt.

(require glmnet
         "../01-ridge.rkt")

(module+ main
  (println fit)
  (println (coef fit)))

(module+ test
  (require racket/match
           rackunit)
  (match-define (vector _ b1 b2 b3) (coef fit))
  (check-false (ormap zero? (list b1 b2 b3))
               "ridge keeps all coefficients nonzero")
  (check-true (< (abs b1) 2.0)
              "ridge shrinks the leading coefficient below its OLS value"))
