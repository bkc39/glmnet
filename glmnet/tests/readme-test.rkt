#lang racket/base

;; The README's examples run (#81): every ```racket block of the repository's
;; README.md is read as data, form by form, and evaluated in a `racket`
;; sandbox, in order, so the README cannot drift from the API; a comparison,
;; such as (equal? a b), must be true. The README is in the repository, beside
;; the glmnet collection, and not in the installed package, so the test says
;; so and checks nothing where it is absent.

(require racket/match
         racket/port
         racket/runtime-path)

(define-runtime-path readme "../../README.md")

;; The text of each ```racket block of `text`, in order.
(define (racket-blocks text)
  (regexp-match* #px"(?m:^```racket[ \t]*\n)(.*?)(?m:^```[ \t]*$)" text
                 #:match-select cadr))

;; The forms of a block's text, read as data.
(define (block-forms block)
  (port->list read (open-input-string block)))

;; Whether a form is a comparison, whose result the README claims is true.
(define (comparison? form)
  (match form
    [(list (or 'equal? 'eqv? 'eq? '= '< '> '<= '>=) _ ...) #t]
    [_ #f]))

(module+ test
  (require racket/file
           racket/sandbox
           rackunit)

  (define (make-readme-evaluator)
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit #f]
                   [sandbox-eval-limits #f]
                   [sandbox-security-guard current-security-guard])
      (make-evaluator 'racket)))

  (test-case "racket-blocks finds each racket block, and only those"
    (check-equal? (racket-blocks "# t\n```racket\n(f 1)\n(g 2)\n```\ntext\n```sh\nls\n```\n```racket\n'x\n```\n")
                  '("(f 1)\n(g 2)\n" "'x\n"))
    (check-equal? (block-forms "(define x 1) ; a comment\n'(a \"b\")\n")
                  '((define x 1) '(a "b"))))

  (test-case "a comparison is a call of equal?, eqv?, eq? or a numeric comparison"
    (check-true (comparison? '(equal? (f 1) 2)))
    (check-true (comparison? '(< x y)))
    (check-false (comparison? '(define x (equal? 1 1))))
    (check-false (comparison? 'equal?)))

  (cond
    [(file-exists? readme)
     (define blocks (racket-blocks (file->string readme)))
     (test-case "the README has a racket example"
       (check-false (null? blocks)))
     (define ev (make-readme-evaluator))
     (for ([block (in-list blocks)]
           [i (in-naturals 1)])
       (for ([form (in-list (block-forms block))])
         (test-case (format "README racket block ~a: ~s" i form)
           (cond
             [(comparison? form) (check-not-false (ev form))]
             [else (check-not-exn (lambda () (ev form)))]))))
     (kill-evaluator ev)]
    [else
     (printf "readme-test: no README.md at ~a; skipping (the README is in the repository, not the package)\n"
             readme)]))
