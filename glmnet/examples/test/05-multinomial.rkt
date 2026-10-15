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
           rackunit
           "common.rkt")
  (match-define (vector (vector _ low-x₁ x₂-in-0)
                        (vector _ high-x₁ x₂-in-1)
                        (vector _ x₁-in-2 high-x₂))
    (coef fit))
  (check-equal? x₂-in-0 0.0 "class 0 drops x₂")
  (check-equal? x₂-in-1 0.0 "class 1 drops x₂")
  (check-equal? x₁-in-2 0.0 "class 2 drops x₁")
  (check-true (negative? low-x₁) "class 0: low x₁")
  (check-true (positive? high-x₁) "class 1: high x₁")
  (check-true (positive? high-x₂) "class 2: high x₂")
  (check-deviance-ratio fit)
  (for ([row (in-list probabilities)])
    (check-equal? (length row) 3)
    (check-= (apply + row) 1.0 1e-12 "each row's probabilities sum to 1"))
  (check-equal? labels y "every training point correct"))
