#lang racket/base

;; The manual's examples evaluate in `racket` with glmnet required. That the
;; two export no common name is checked by utils.rkt's for-label require,
;; which a collision would make a compile error.

(module+ test
  (require rackunit
           syntax/macro-testing
           "../scribblings/utils.rkt")

  (test-case "the examples' evaluator has racket and glmnet"
    (define ev (make-glmnet-eval))
    (check-equal? (ev '(let ()
                         (match-define (vector _ slope) (coef (ols '((1.0) (2.0) (3.0))
                                                                   '(2.0 4.0 6.0))))
                         (~r slope #:precision 3)))
                  "2"))

  (test-case "noisy-fixture defines the 40 noisy observations in the page's evaluator"
    (define ev (make-glmnet-eval))
    (noisy-fixture ev #:hidden)
    (check-equal? (ev '(map length (list X40 y40))) '(40 40)))

  (test-case "the elastic-net page draws its program's X40, y40 and folds"
    (define ev (make-glmnet-eval))
    (noisy-fixture ev #:hidden)
    (define (program name) (dynamic-require 'glmnet/examples/03-elastic-net name))
    (check-equal? (list (ev 'X40) (ev 'y40) (ev '(random-fold-ids 40)))
                  (map program '(X40 y40 folds))))

  (test-case "noisy-fixture rejects an option examples does not take"
    (check-exn #rx"noisy-fixture: expected one of these literals: #:label or #:hidden"
               (λ () (convert-compile-time-error (noisy-fixture ev #:bogus))))))
