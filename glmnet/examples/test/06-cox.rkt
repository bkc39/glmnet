#lang racket/base

;; Runner + tests for the literate example ../06-cox.rkt.

(require glmnet
         "../06-cox.rkt")

(module+ main
  (println fit)
  (println (coef fit))
  (println log-risk)
  (println risk))

(module+ test
  (require racket/match
           rackunit)
  (match-define (vector β₁ β₂) (coef fit))
  (check-true (positive? β₁) "risk coefficient is positive")
  (check-equal? β₂ 0.0 "lasso zeros the noise coefficient")
  (check-true (< 0.0 (deviance-ratio fit)))
  (check-true (<= (deviance-ratio fit) 1.0))
  (for ([η (in-list log-risk)] [r (in-list risk)])
    (check-= r (exp η) 1e-12 "the relative risk is exp of the linear predictor"))
  (check-true (apply < risk) "risk rises with x₁, the subjects' order"))
