#lang racket/base

;; The manual's examples evaluate in `racket` with glmnet required. That the
;; two export no common name is checked by utils.rkt's for-label require,
;; which a collision would make a compile error.

(module+ test
  (require rackunit
           "../scribblings/utils.rkt")

  (test-case "the examples' evaluator has racket and glmnet"
    (define ev (make-glmnet-eval))
    (check-equal? (ev '(let ()
                         (match-define (vector _ slope) (coef (ols '((1.0) (2.0) (3.0))
                                                                   '(2.0 4.0 6.0))))
                         (~r slope #:precision 3)))
                  "2")))
