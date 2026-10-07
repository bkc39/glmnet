#lang racket/base

;; The README's examples run (#81): every ```racket block of the repository's
;; README.md is read as data, form by form, and evaluated in a `racket`
;; sandbox, in order, so the README cannot drift from the API. The README is
;; in the repository, beside the glmnet collection, and not in the installed
;; package, so the test says so and checks nothing where it is absent.

(require racket/port
         racket/runtime-path)

(define-runtime-path readme "../../README.md")

;; The text of each ```racket block of `text`, in order.
(define (racket-blocks text)
  (regexp-match* #px"(?m:^```racket[ \t]*\n)(.*?)(?m:^```[ \t]*$)" text
                 #:match-select cadr))

;; The forms of a block's text, read as data.
(define (block-forms block)
  (port->list read (open-input-string block)))

(module+ test
  (require racket/file
           racket/sandbox
           rackunit)

  (define (make-readme-evaluator)
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit #f]
                   [sandbox-eval-limits #f]
                   [sandbox-security-guard current-security-guard]
                   [sandbox-path-permissions '((exists "/"))])
      (make-evaluator 'racket)))

  (test-case "racket-blocks finds each racket block, and only those"
    (check-equal? (racket-blocks "# t\n```racket\n(f 1)\n(g 2)\n```\ntext\n```sh\nls\n```\n```racket\n'x\n```\n")
                  '("(f 1)\n(g 2)\n" "'x\n"))
    (check-equal? (block-forms "(define x 1) ; a comment\n'(a \"b\")\n")
                  '((define x 1) '(a "b"))))

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
           (check-not-exn (lambda () (ev form))))))
     (kill-evaluator ev)]
    [else
     (printf "readme-test: no README.md at ~a; skipping (the README is in the repository, not the package)\n"
             readme)]))
