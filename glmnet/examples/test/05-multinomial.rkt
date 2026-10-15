#lang racket/base

;; Runner + tests for the literate example ../05-multinomial.rkt.

(require glmnet
         "../05-multinomial.rkt")

(module+ main
  (println fit)
  (println (coef fit))
  (println probabilities)
  (println labels))

(module+ test
  (require racket/match
           rackunit)
  (match-define (vector (vector _ low-x₁ _)
                        (vector _ high-x₁ _)
                        (vector _ _ high-x₂))
    (coef fit))
  (check-true (negative? low-x₁) "class 0: low x₁")
  (check-true (positive? high-x₁) "class 1: high x₁")
  (check-true (positive? high-x₂) "class 2: high x₂")
  (check-true (< 0.0 (deviance-ratio fit)))
  (check-true (<= (deviance-ratio fit) 1.0))
  (for ([row (in-list probabilities)])
    (check-equal? (length row) 3)
    (check-= (apply + row) 1.0 1e-12 "each row's probabilities sum to 1"))
  (check-equal? labels y "every training point correct"))
