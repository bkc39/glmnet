#lang racket/base

;; Runner + tests for the literate example ../08-mgaussian.rkt.

(require glmnet
         "../08-mgaussian.rkt")

(module+ main
  (println fit)
  (println (coef fit))
  (println predictions))

(module+ test
  (require racket/match
           rackunit
           "common.rkt")
  (match-define (vector (vector _ up-x₁ x₂-in-y₁)
                        (vector _ down-x₁ x₂-in-y₂))
    (coef fit))
  (check-true (positive? up-x₁) "x₁ raises response 1")
  (check-true (negative? down-x₁) "x₁ lowers response 2")
  (check-equal? x₂-in-y₁ 0.0 "x₂ dropped from response 1")
  (check-equal? x₂-in-y₂ 0.0 "x₂ dropped from response 2")
  (check-deviance-ratio fit)
  (for* ([(predicted observed) (in-parallel predictions Y)]
         [(ŷ y) (in-parallel predicted observed)])
    (check-= ŷ y 0.5 "the predictions track Y")))
