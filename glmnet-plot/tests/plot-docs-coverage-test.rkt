#lang racket/base

;; Every binding exported by `(require glmnet/plot)` has a reference entry: a
;; defproc, defproc*, defstruct, defstruct*, defthing, defform, defform*,
;; defidform or defparam in glmnet-plot/scribblings/ (deftogether is searched
;; through). A defstruct covers the struct's constructor, predicate, field
;; accessors and struct-type binding. The .scrbl sources are read with
;; Scribble's @-reader, not rendered. The same check for `glmnet` is
;; glmnet/tests/docs-coverage-test.rkt in the glmnet package.

(module+ test
  (require rackunit
           racket/file
           racket/list
           racket/runtime-path
           racket/set
           scribble/reader
           (only-in glmnet/plot))

  (define-runtime-path scribblings-dir "../scribblings")

  (define exported
    (let-values ([(variables syntaxes) (module->exports 'glmnet/plot)])
      (for*/set ([exports (in-list (list variables syntaxes))]
                 [phase+names (in-list exports)]
                 #:when (eqv? (car phase+names) 0)
                 [name+origins (in-list (cdr phase+names))])
        (car name+origins))))

  (define (read-scrbl path)
    (call-with-input-file path read-inside))

  ;; The forms after any leading keyword options, as in (defproc #:kind "..." (f x) ...).
  (define (drop-options forms)
    (if (and (pair? forms) (keyword? (car forms)) (pair? (cdr forms)))
        (drop-options (cddr forms))
        forms))

  (define (struct-bindings name fields constructor?)
    (define field-names
      (for/list ([field (in-list fields)])
        (if (pair? field) (car field) field)))
    (append (list name
                  (string->symbol (format "~a?" name))
                  (string->symbol (format "struct:~a" name)))
            (if constructor? (list (string->symbol (format "make-~a" name))) '())
            (for/list ([field (in-list field-names)])
              (string->symbol (format "~a-~a" name field)))))

  ;; The names one definition form documents.
  (define (form-names head args)
    (define forms (drop-options args))
    (case head
      [(defproc defform)
       (if (and (pair? forms) (pair? (car forms))) (list (caar forms)) '())]
      [(defproc* defform*)
       (for/list ([spec (in-list (car forms))]
                  #:when (and (pair? spec) (pair? (car spec))))
         (caar spec))]
      [(defthing defidform defparam)
       (list (car forms))]
      [(defstruct defstruct*)
       (define id (car forms))
       (struct-bindings (if (pair? id) (car id) id) (cadr forms) (eq? head 'defstruct))]
      [else '()]))

  (define definition-heads
    '(defproc defproc* defform defform* defthing defidform defparam defstruct defstruct*))

  (define (documented-names datum)
    (cond
      [(and (pair? datum) (list? datum))
       (append (if (memq (car datum) definition-heads)
                   (form-names (car datum) (cdr datum))
                   '())
               (append-map documented-names datum))]
      [else '()]))

  (define scrbl-files
    (find-files (lambda (p) (regexp-match? #rx"[.]scrbl$" (path->string p)))
                scribblings-dir))

  (define documented
    (for*/set ([file (in-list scrbl-files)]
               [name (in-list (documented-names (read-scrbl file)))])
      name))

  (test-case "the reference is found and read"
    (check-true (pair? scrbl-files))
    (check-true (set-member? documented 'plot-coefficient-path))
    (check-true (set-member? documented 'plot-cv)))

  (test-case "every export of glmnet/plot has a reference entry"
    (define missing
      (sort (set->list (set-subtract exported documented)) symbol<?))
    (check-equal? missing '()
                  (format "exported by glmnet/plot but not documented in glmnet-plot/scribblings: ~a"
                          missing))))
