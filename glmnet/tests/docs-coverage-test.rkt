#lang racket/base

;; Every binding exported by `(require glmnet)` has a reference entry: a
;; defproc, defproc*, defstruct, defstruct*, defthing, defform, defform*,
;; defidform or defparam in the manual (deftogether is searched through). The
;; manual is glmnet/scribblings/glmnet.scrbl and every file it reaches through
;; include-section; a .scrbl file that nothing includes does not count, and
;; neither does a definition form inside code (racketblock, examples, ...). A
;; defstruct covers the struct's constructor, predicate, field accessors and
;; struct-type binding. The .scrbl sources are read with Scribble's @-reader,
;; not rendered.

(module+ test
  (require rackunit
           racket/file
           racket/list
           racket/match
           racket/path
           racket/runtime-path
           racket/set
           scribble/reader
           (only-in glmnet))

  (define-runtime-path scribblings-dir "../scribblings")

  (define (exported mod)
    (define-values (variables syntaxes) (module->exports mod))
    (for*/set ([exports (in-list (list variables syntaxes))]
               [phase+names (in-list exports)]
               #:when (eqv? (car phase+names) 0)
               [name+origins (in-list (cdr phase+names))])
      (car name+origins)))

  (define (read-scrbl path)
    (call-with-input-file path read-inside))

  ;; Forms whose arguments are code or quoted data rather than documentation.
  (define code-heads
    '(racketblock racketblock0 RACKETBLOCK RACKETBLOCK0 racketmod racketmod0
      racketinput racketinput0 racketresultblock racketresultblock0
      racket racketresult racketid code codeblock codeblock0 typeset-code
      examples interaction interaction0 interaction-eval interaction-eval-show
      defexamples schemeblock schemeblock0 scheme verbatim quote quasiquote))

  ;; Every form in datum outside code, outermost first.
  (define (doc-forms datum)
    (cond
      [(and (pair? datum) (list? datum))
       (if (memq (car datum) code-heads)
           '()
           (cons datum (append-map doc-forms datum)))]
      [else '()]))

  ;; The files that `file` (read as `datum`) includes with include-section.
  (define (included-files file datum)
    (for/list ([form (in-list (doc-forms datum))]
               #:when (eq? (car form) 'include-section))
      (match (cdr form)
        [(list (? string? relative)) (simplify-path (build-path (path-only file) relative))]
        [_ (error 'docs-coverage "~a: unsupported include-section form ~s" file form)])))

  ;; The root and every .scrbl file it reaches through include-section, each
  ;; with its contents.
  (define (manual-files root)
    (let loop ([pending (list (simplify-path root))] [seen (hash)])
      (match pending
        ['() (for/list ([(file datum) (in-hash seen)]) (cons file datum))]
        [(cons file rest)
         (if (hash-ref seen file #f)
             (loop rest seen)
             (let ([datum (read-scrbl file)])
               (loop (append (included-files file datum) rest)
                     (hash-set seen file datum))))])))

  ;; The value of keyword option kw among a form's leading options, or #f.
  (define (option forms kw)
    (match forms
      [(list* (== kw) value _) value]
      [(list* (? keyword?) _ more) (option more kw)]
      [_ #f]))

  ;; The forms after any leading keyword options, as in (defproc #:kind "..." (f x) ...).
  (define (drop-options forms)
    (if (and (pair? forms) (keyword? (car forms)) (pair? (cdr forms)))
        (drop-options (cddr forms))
        forms))

  ;; The identifier at the head of a spec: f in (f x), ((f x) result) and ((f x) y).
  (define (spec-head spec)
    (if (pair? spec) (spec-head (car spec)) spec))

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
    (define id (option args '#:id))
    (define names
      (cond
        [(and id (memq head '(defproc defform defthing defidform)))
         (list (spec-head id))]
        [(null? forms) '()]
        [else
         (case head
           [(defproc defform defthing defidform defparam)
            (list (spec-head (car forms)))]
           [(defproc* defform*)
            (if (list? (car forms)) (map spec-head (car forms)) '())]
           [(defstruct defstruct*)
            (struct-bindings (spec-head (car forms)) (cadr forms) (eq? head 'defstruct))]
           [else '()])]))
    (filter symbol? names))

  (define definition-heads
    '(defproc defproc* defform defform* defthing defidform defparam defstruct defstruct*))

  (define (documented-names datum)
    (for*/list ([form (in-list (doc-forms datum))]
                #:when (memq (car form) definition-heads)
                [name (in-list (form-names (car form) (cdr form)))])
      name))

  (define (read-string-inside s)
    (read-inside (open-input-string s)))

  (test-case "definition forms are recognized, and code is skipped"
    (check-equal? (documented-names
                   (read-string-inside "@defproc[(f [x any/c]) any]{} @defproc[#:kind \"k\" (g) any]{}"))
                  '(f g))
    (check-equal? (documented-names
                   (read-string-inside "@defproc*[([(f [x any/c]) any] [(f) any])]{}"))
                  '(f f))
    (check-equal? (documented-names
                   (read-string-inside "@defform*[((form a) (form a b))]{} @defform[(other a)]{}"))
                  '(form form other))
    (check-equal? (documented-names (read-string-inside "@defform[#:id id (id a)]{}"))
                  '(id))
    (check-equal? (documented-names (read-string-inside "@defthing[t any/c]{} @defparam[p v any/c]{}"))
                  '(t p))
    (check-equal? (list->set
                   (documented-names (read-string-inside "@defstruct*[s ([a any/c])]{}")))
                  (set 's 's? 'struct:s 's-a))
    (check-equal? (documented-names
                   (read-string-inside
                    "@deftogether[(@defproc[(f) any] @defproc[(g) any])]{@examples[(defproc (h) any)]}"))
                  '(f g))
    (check-equal? (documented-names
                   (read-string-inside
                    "@racketblock[(defproc (f x) any)] @racket[(defthing t any/c)] @codeblock{@defproc[(g) any]}"))
                  '()))

  (test-case "only files reachable through include-section are read"
    (define dir (make-temporary-directory))
    (make-directory (build-path dir "sub"))
    (with-output-to-file (build-path dir "root.scrbl")
      (lambda () (display "@include-section[\"sub/a.scrbl\"] @defproc[(r) any]{}")))
    (with-output-to-file (build-path dir "sub" "a.scrbl")
      (lambda () (display "@include-section[\"b.scrbl\"] @defproc[(a) any]{}")))
    (with-output-to-file (build-path dir "sub" "b.scrbl")
      (lambda () (display "@include-section[\"../root.scrbl\"] @defproc[(b) any]{}")))
    (with-output-to-file (build-path dir "orphan.scrbl")
      (lambda () (display "@defproc[(orphan) any]{}")))
    (define files (manual-files (build-path dir "root.scrbl")))
    (delete-directory/files dir)
    (check-equal? (length files) 3)
    (check-equal? (sort (append-map (lambda (file) (documented-names (cdr file))) files)
                        symbol<?)
                  '(a b r)))

  (define files (manual-files (build-path scribblings-dir "glmnet.scrbl")))

  (define documented
    (for*/set ([file (in-list files)]
               [name (in-list (documented-names (cdr file)))])
      name))

  (test-case "the manual and the exports are found and read"
    (check-true (for/or ([file (in-list files)])
                  (equal? (file-name-from-path (car file)) (string->path "reference.scrbl"))))
    (check-true (set-member? documented 'elnet-fit))
    (check-true (set-member? documented 'elnet-result-coefficients))
    (check-false (set-empty? (exported 'glmnet)))
    (check-true (set-member? (exported 'glmnet) 'elnet-fit))
    (check-true (set-member? (exported 'glmnet) 'rows->design-matrix)))

  (for ([mod (in-list '(glmnet))])
    (test-case (format "every export of ~a has a reference entry" mod)
      (define missing
        (sort (set->list (set-subtract (exported mod) documented)) symbol<?))
      (check-equal? missing '()
                    (format "exported by ~a but not documented in the glmnet manual: ~a"
                            mod missing)))))
