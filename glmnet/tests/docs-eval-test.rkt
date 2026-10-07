#lang racket/base

;; The manual's examples evaluate in `racket` with glmnet required
;; (make-glmnet-eval in scribblings/utils.rkt), which relies on the two
;; exporting no common name.

(module+ test
  (require rackunit
           racket/set
           (only-in racket)
           (only-in glmnet)
           "../scribblings/utils.rkt")

  (define (exported mod)
    (define-values (variables syntaxes) (module->exports mod))
    (for*/set ([exports (in-list (list variables syntaxes))]
               [phase+names (in-list exports)]
               #:when (eqv? (car phase+names) 0)
               [name+origins (in-list (cdr phase+names))])
      (car name+origins)))

  (test-case "racket and glmnet export no common name"
    (check-true (set-member? (exported 'racket) 'match-define))
    (check-true (set-member? (exported 'glmnet) 'coef))
    (check-equal? (sort (set->list (set-intersect (exported 'racket) (exported 'glmnet)))
                        symbol<?)
                  '()))

  (test-case "the examples' evaluator has racket and glmnet"
    (define ev (make-glmnet-eval))
    (check-equal? (ev '(let ()
                         (match-define (vector _ slope) (coef (ols '((1.0) (2.0) (3.0))
                                                                   '(2.0 4.0 6.0))))
                         (~r slope #:precision 3)))
                  "2")))
