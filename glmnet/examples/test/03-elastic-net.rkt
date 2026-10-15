#lang racket/base

;; Runner + tests for the literate example ../03-elastic-net.rkt.

(require glmnet
         "../03-elastic-net.rkt")

(module+ main
  (println fit)
  (println (coef fit))
  (println zero-counts))

(module+ test
  (require racket/match
           rackunit)
  (match-define (list ridge-zeros enet-zeros lasso-zeros) zero-counts)
  (check-equal? ridge-zeros 0 "ridge zeroes none")
  (check-true (<= 1 enet-zeros lasso-zeros)
              "the elastic net selects at least one out, and no more than the lasso")
  (check-true (< 0.0 (deviance-ratio fit) 1.0) "a penalized fit, so 0 < R² < 1"))
