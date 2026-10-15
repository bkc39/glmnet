#lang racket/base

;; Reference signatures that must show a binding's whole contract: every
;; cross-validation procedure's #:fold-ids is fold-ids/c, and every plot's
;; #:out-file names the extensions it accepts, with no unexported predicate.
;; The source is read with Scribble's @-reader, not rendered.

(module+ test
  (require rackunit
           racket/list
           racket/match
           racket/runtime-path
           racket/string
           scribble/reader)

  (define-runtime-path reference "../scribblings/reference.scrbl")

  (define forms (call-with-input-file reference read-inside))

  (define (find-forms head tree)
    (match tree
      [(cons (== head) _) (list tree)]
      [(? list?) (append-map (lambda (t) (find-forms head t)) tree)]
      [_ '()]))

  (define defprocs
    (for/hash ([form (in-list (find-forms 'defproc forms))])
      (match-define (list* _ (cons name args) _) form)
      (values name args)))

  (define (keyword-contract name keyword)
    (for/first ([arg (in-list (hash-ref defprocs name))]
                #:when (and (pair? arg) (eq? (car arg) keyword)))
      (match-define (list _ _ contract _) arg)
      contract))

  (define (flatten-text tree)
    (match tree
      [(? string?) tree]
      [(? symbol?) (symbol->string tree)]
      [(list* (? symbol?) (? list? rest)) (string-join (map flatten-text rest) "")]
      [(? list?) (string-join (map flatten-text tree) "")]
      [_ ""]))

  (test-case "every cross-validation procedure's #:fold-ids shows fold-ids/c"
    (define cv-procedures
      '(elnet-cv logistic-cv multinomial-cv poisson-cv cox-cv mgaussian-cv formula-cv))
    (for ([name (in-list cv-procedures)])
      (check-equal? (keyword-contract name '#:fold-ids) 'fold-ids/c (symbol->string name)))
    (check-equal? (sort (for/list ([name (in-hash-keys defprocs)]
                                   #:when (keyword-contract name '#:fold-ids))
                          name)
                        symbol<?)
                  (sort cv-procedures symbol<?))
    (define fold-ids/c-doc
      (for/first ([form (in-list (find-forms 'defthing forms))]
                  #:when (match form [(list* _ 'fold-ids/c _) #t] [_ #f]))
        (flatten-text form)))
    (check-regexp-match #px"use every fold from 0 to the\\s+largest id" fold-ids/c-doc))

  (test-case "every plot's #:out-file shows its extensions in prose"
    (for ([name (in-list '(plot-coefficient-path plot-cv))])
      (define contract (keyword-contract name '#:out-file))
      (check-false (regexp-match? #rx"has-image-extension" (format "~s" contract)))
      (check-match contract
                   (list 'or/c #f (list 'unsyntax (cons 'elem prose)))
                   (regexp-match? #rx"^a path-string\\? ending in \\.png, \\.pdf, \\.svg or \\.eps$"
                                  (string-normalize-spaces (flatten-text prose)))))))
