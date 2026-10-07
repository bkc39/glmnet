#lang racket/base

;; The one boundary from the user's data, unnamed or named and in any format,
;; to the design matrix and response the solvers read (#74); see AGENTS.md.

(require racket/contract
         syntax/parse/define
         (for-syntax racket/base syntax/parse racket/syntax)
         (only-in "../data.rkt" design-matrix? design-matrix-column-names response/c table?)
         (submod "../data.rkt" support))

(provide
 (contract-out
  [data/c flat-contract?]
  [named-data? (-> any/c boolean?)]
  [response-for/c (-> any/c flat-contract? flat-contract?)]
  [predictors-for/c (-> any/c any/c flat-contract?)]))

;; For the family modules and core/model.rkt only: the contract of a fitter,
;; (fit/c elem (mandatory-kw ctc ...) (optional-kw ctc ...) result); its
;; arguments as a design matrix, a response and the predictors' names (#f for
;; unnamed data), (fit-input who X y predictors [elem]); and new data for a
;; prediction, (new-data->design-matrix who X names).
(module* support #f
  (provide fit/c fit-input new-data->design-matrix))

;; --- libraries glmnet does not load ----------------------------------------------

;; A library whose values glmnet recognises, by its adapter's support
;; submodule, only when a value is no plain form: the module that declares the
;; values, resolved once, and the adapter.
(struct library (module adapter [resolved #:mutable]))

(define here (variable-reference->module-path-index (#%variable-reference)))
(define own-name (variable-reference->resolved-module-path (#%variable-reference)))
(define own-namespace (variable-reference->empty-namespace (#%variable-reference)))

(define (adapter-path relative)
  (module-path-index-join `(submod ,relative support) here))

(define polars (library 'polars (adapter-path "../data/polars.rkt") 'unresolved))
(define math (library 'math/array (adapter-path "../data/math.rkt") 'unresolved))

;; The library's resolved module name, or #f when it is not installed.
(define (library-name lib)
  (define resolved (library-resolved lib))
  (cond
    [(eq? resolved 'unresolved)
     (define name
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (module-path-index-resolve (module-path-index-join (library-module lib) #f))))
     (set-library-resolved! lib name)
     name]
    [else resolved]))

;; An adapter loaded into the namespace ns, its exports fetched by name.
(struct adapter (library namespace exports))

(define (adapter-ref a name)
  (hash-ref! (adapter-exports a) name
             (lambda ()
               (parameterize ([current-namespace (adapter-namespace a)])
                 (dynamic-require (library-adapter (adapter-library a)) name)))))

;; Each module registry's adapters, by library.
(define adapters (make-weak-hasheq))

;; lib's adapter in ns's module registry, loaded there when the program has
;; declared both lib and glmnet in it; otherwise #f.
(define (registry-adapter lib ns)
  (define loaded (hash-ref! adapters (namespace-module-registry ns) make-hasheq))
  (define name (library-name lib))
  (cond
    [(hash-ref loaded lib #f) => values]
    [(and name
          (parameterize ([current-namespace ns])
            (and (module-declared? name #f) (module-declared? own-name #f))))
     (define a (adapter lib ns (make-hasheq)))
     (hash-set! loaded lib a)
     a]
    [else #f]))

;; The adapter of lib whose predicate `kind` accepts v, from the registry of
;; the current namespace or glmnet's own, or #f.
(define (library-value lib kind v)
  (define current (current-namespace))
  (define namespaces
    (if (eq? (namespace-module-registry current) (namespace-module-registry own-namespace))
        (list own-namespace)
        (list current own-namespace)))
  (for/or ([ns (in-list namespaces)])
    (define a (registry-adapter lib ns))
    (and a ((adapter-ref a kind) v) a)))

(define (polars-adapter? v)
  (and (adapter? v) (eq? (adapter-library v) polars)))

;; --- what data is ------------------------------------------------------------------

;; What X is, plain forms first: 'design-matrix, 'rows, 'table, 'table+rows
;; (an association list whose columns are lists, so also rows), the adapter of
;; a dataframe or of a math matrix, or #f.
(define (data-form X)
  (define rows? (nested-matrix? X))
  (define named? (and (not (design-matrix? X)) (table? X)))
  (cond
    [(design-matrix? X) 'design-matrix]
    [(and rows? named?) 'table+rows]
    [rows? 'rows]
    [named? 'table]
    [(library-value polars 'dataframe? X) => values]
    [(library-value math 'math-matrix? X) => values]
    [else #f]))

(define (named-data-form? form)
  (or (memq form '(table table+rows)) (polars-adapter? form)))

(define (unnamed-data-form? form)
  (or (memq form '(design-matrix rows))
      (and (adapter? form) (not (polars-adapter? form)))))

;; Whether data of the form `form` is named data given the response y: a
;; table+rows is when y names columns, and rows otherwise.
(define (named-form? form y)
  (and (named-data-form? form)
       (or (not (eq? form 'table+rows)) (column-names? y))))

(define (column-names? y)
  (or (column-name? y) (and (pair? y) (list? y) (andmap column-name? y))))

(define (named-data? v)
  (and (named-data-form? (data-form v)) #t))

(define data/c
  (flat-contract-with-explanation
   (lambda (v)
     (or (and (data-form v) #t)
         (explain v "~a" "~e"
                  (string-append "unnamed data (a design matrix, a list or vector of rows "
                                 "or a math/matrix matrix) or named data (a table or a "
                                 "Polars dataframe)")
                  v)))
   #:name 'data/c))

;; --- column names ------------------------------------------------------------------

;; The names of named data's columns, as strings, in its order; duplicates are
;; an error of the conversion, in the caller's name.
(define (data-column-names X form)
  (if (polars-adapter? form)
      ((adapter-ref form 'column-names) X)
      (table-name-list X)))

(define (data-kind form) (if (polars-adapter? form) "dataframe" "table"))

;; What is wrong with a column of X as a numeric column, given its name, or #f:
;; any column of a table can be, since its entries are checked as they are read.
(define (dtype-problem X form)
  (if (polars-adapter? form)
      ((adapter-ref form 'numeric-column-problem) X)
      (lambda (name) #f)))

;; A flat contract named `name` on column names of named data X: `problem`
;; returns what is wrong with a value, as column-problem's given: field, or #f.
(define (columns/c X form name expected problem)
  (define present (data-column-names X form))
  (define known (name-set present))
  (define what (data-kind form))
  (define column (dtype-problem X form))
  (flat-contract-with-explanation
   (lambda (v)
     (define given (problem v what known present column))
     (or (not given) (explain v "~a" "~a" (format expected what) given)))
   #:name name))

;; named/c for a column name, or a list of them, and unnamed/c for anything else.
(define (by-name/c named/c unnamed/c)
  (define named-projection (get/build-late-neg-projection named/c))
  (define unnamed-projection (get/build-late-neg-projection unnamed/c))
  (make-flat-contract
   #:name `(or/c ,(contract-name named/c) ,(contract-name unnamed/c))
   #:first-order
   (lambda (v) (contract-first-order-passes? (if (column-names? v) named/c unnamed/c) v))
   #:late-neg-projection
   (lambda (blame)
     (define named (named-projection blame))
     (define unnamed (unnamed-projection blame))
     (lambda (v neg-party)
       (if (column-names? v) (named v neg-party) (unnamed v neg-party))))))

;; --- responses ---------------------------------------------------------------------

;; y given X: for named data, the name of one of its numeric columns; otherwise
;; one entry per observation, as response/c with `elem`, a math array or a
;; Polars series, whose entries are checked as they are read.
(define (response-for/c X elem)
  (define form (data-form X))
  (cond
    [(eq? form 'table+rows) (by-name/c (response-column/c X form) (unnamed-response/c elem))]
    [(named-data-form? form) (response-column/c X form)]
    [else (unnamed-response/c elem)]))

(define (response-column/c X form)
  (columns/c X form 'column-name/c "the name of a numeric column of the ~a"
             (lambda (v what known present column)
               (if (column-name? v)
                   (column-problem (column-name->string v) what known present column)
                   (format "~e" v)))))

;; A math array or a Polars series, as the adapter that recognises it, or #f.
(define (response-value v)
  (and (not (one-dimensional? v))
       (or (library-value polars 'series? v) (library-value math 'response-array? v))))

(define (unnamed-response/c elem)
  (define sequence/c (response/c elem))
  (define sequence-projection (get/build-late-neg-projection sequence/c))
  (make-flat-contract
   #:name `(or/c ,(contract-name sequence/c) series? array?)
   #:first-order
   (lambda (v) (or (contract-first-order-passes? sequence/c v) (and (response-value v) #t)))
   #:late-neg-projection
   (lambda (blame)
     (define check (sequence-projection blame))
     (lambda (v neg-party)
       (if (response-value v) v (check v neg-party))))))

;; --- predictors --------------------------------------------------------------------

;; #:predictors given X and y: for named data, a non-empty list of distinct
;; names of its numeric columns, without the response; for unnamed data, only
;; #f, since its predictors are all of its columns.
(define (predictors-for/c X y)
  (define form (data-form X))
  (cond
    [(named-form? form y) (predictor-columns/c X form y)]
    [else
     (flat-contract-with-explanation
      (lambda (v)
        (or (not v)
            (explain v "~a" "~e"
                     (string-append "#f, since X is unnamed data: rows, a matrix or a design "
                                    "matrix, even one whose columns have names")
                     v)))
      #:name 'not)]))

(define (predictor-columns/c X form y)
  (define response (and (column-name? y) (column-name->string y)))
  (columns/c X form '(and/c (listof column-name/c) pair?)
             "a non-empty list of distinct names of numeric columns of the ~a, without the response"
             (lambda (v what known present column)
               (column-list-problem v what known present
                                    (lambda (s)
                                      (if (equal? s response) "which is the response" (column s)))))))

;; fit/c's precondition: named data needs #:predictors.
(define (predictors-problem X y predictors)
  (define form (data-form X))
  (or (not (named-form? form y))
      (and (not (unsupplied-arg? predictors)) predictors #t)
      (list (format (string-append "#:predictors is required when X is a ~a; it names the "
                                   "predictor columns, as in #:predictors '(\"wt\" \"hp\")")
                    (data-kind form)))))

;; A keyword argument's contract, its ->i name the keyword itself, so that
;; blame names "the #:lambda argument".
(begin-for-syntax
  (define-splicing-syntax-class keyword-contract
    #:attributes (kw id ctc)
    (pattern (~seq kw:keyword ctc:expr)
             #:with id (format-id #'kw "#:~a" (keyword->string (syntax-e #'kw))))))

;; The contract's name is the ->* form of the documented signature, its
;; optional keywords elided, so that an error does not print the whole ->i.
(define-syntax-parse-rule (fit/c elem:expr (mandatory:keyword-contract ...)
                                 (optional:keyword-contract ...)
                                 result:expr)
  #:with predictors (format-id #'elem "#:predictors")
  (rename-contract
   (->i ([X data/c]
         [y (X) (response-for/c X elem)]
         (~@ mandatory.kw [mandatory.id mandatory.ctc]) ...)
        (#:predictors [predictors (X y) (predictors-for/c X y)]
         (~@ optional.kw [optional.id optional.ctc]) ...)
        #:pre/desc (X y predictors) (predictors-problem X y predictors)
        [_ result])
   '(->* (data/c (response-for/c X elem) (~@ mandatory.kw mandatory.ctc) ...)
         (#:predictors (predictors-for/c X y) (... ...))
         result)))

;; --- conversion --------------------------------------------------------------------

(define (unnamed->design-matrix who what X form)
  (cond
    [(eq? form 'design-matrix) X]
    [(memq form '(rows table+rows)) (nested->dm X 'rows #f who what)]
    [else ((adapter-ref form 'matrix->dm) who what X)]))

;; The columns `names` (strings) of named data X; checked? when the caller's
;; contract has checked them.
(define (named->design-matrix who X form names #:checked? [checked? #f])
  (if (polars-adapter? form)
      ((adapter-ref form 'dataframe->design-matrix) who X names #:checked? checked?)
      (select-table-columns X names who)))

(define (fit-input who X y predictors [elem #f])
  (define form (data-form X))
  (cond
    [(named-form? form y)
     (define names (map column-name->string predictors))
     (define x (named->design-matrix who X form names #:checked? #t))
     (values x (named-response who X form (column-name->string y) x elem) names)]
    [else
     (define x (unnamed->design-matrix who "X" X form))
     (values x (unnamed-response who y x elem) #f)]))

;; New data X for a model whose predictors are `names` (#f when it was not
;; fitted from named data): named data and a design matrix with column names
;; by those names, and other unnamed data by position. Without names, a table
;; whose columns are lists is a table, not rows.
(define (new-data->design-matrix who X names)
  (define form (data-form X))
  (cond
    [(and names (eq? form 'design-matrix) (design-matrix-column-names X))
     (select-table-columns X names who)]
    [(and names (named-data-form? form)) (named->design-matrix who X form names)]
    [(unnamed-data-form? form) (unnamed->design-matrix who "X" X form)]
    [else
     (raise-arguments-error
      who (string-append "the model's predictors are not named, so X must be a design matrix, "
                         "rows or a matrix, not a table or a dataframe"))]))

;; The response column `name` of named data X, whose predictors are x.
(define (named-response who X form name x elem)
  (define no (design-matrix-nrows x))
  (define entries
    (if (polars-adapter? form)
        ((adapter-ref form 'dataframe-column->response) who X name #:checked? #t)
        (table-response who X name no)))
  (as-response entries no who (format "the response column ~s" name) elem))

(define (table-response who X name no)
  (define column (car (select-table-values X (list name) who)))
  (cond
    [(= (vector-length column) no) (table-column->flvector column name who)]
    [else
     (raise-arguments-error who "the table's columns have different lengths"
                            "column" name "length" (vector-length column)
                            "rows of the predictors" no)]))

;; The response y of unnamed predictors x. When y has one entry per column of
;; x rather than per row, the error suggests that x may be transposed.
(define (unnamed-response who y x elem)
  (define a (response-value y))
  (define entries
    (cond
      [(not a) y]
      [(polars-adapter? a) ((adapter-ref a 'series->response) who "y" y)]
      [else ((adapter-ref a 'array->reals) who "y" y)]))
  (define n (one-dimensional-length entries))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (cond
    [(= n no) (as-response entries no who "y" elem)]
    [(= n ni)
     (raise-arguments-error who "y does not have one entry per row of X"
                            "length of y" n "rows of X" no "columns of X" ni
                            "hint" (unquoted-printing-string
                                    "rows are observations; is X transposed?"))]
    [else (as-response entries no who "y" elem)]))
