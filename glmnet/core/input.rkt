#lang racket/base

;; The one boundary from the user's data, unnamed or named and in any format,
;; to the design matrix and response the solvers read (#74); see AGENTS.md.
;; The families differ only in their response: one column of numbers or of
;; class labels, a time and a status column (Cox), or several columns
;; (mgaussian).

(require racket/contract
         racket/match
         (only-in racket/flonum for/flvector)
         (only-in racket/vector vector-map)
         ffi/vector
         syntax/parse/define
         (for-syntax racket/base syntax/parse racket/syntax)
         (only-in "../data.rkt" design-matrix? design-matrix-column-names response/c table?)
         (submod "../data.rkt" support)
         (only-in "terms.rkt" value-kind response-classes check-classes))

(provide
 (contract-out
  [data/c flat-contract?]
  [named-data? (-> any/c boolean?)]
  [response-for/c (->* (data/c flat-contract?) (#:classes? boolean?) flat-contract?)]
  [survival-for/c (-> data/c flat-contract?)]
  [statuses-for/c (-> data/c any/c flat-contract?)]
  [responses-for/c (-> data/c flat-contract?)]
  [predictors-for/c (-> data/c any/c flat-contract?)]))

;; For the family modules and core/model.rkt only: the contract of a fitter,
;; (fit/c response-ctc [#:argument id] [#:statuses ctc] (mandatory-kw ctc ...)
;; (optional-kw ctc ...) result); its arguments as a design matrix, the
;; response and the predictors' names (#f for unnamed data), by fit-input,
;; class-fit-input (with the classes), survival-fit-input (the times and the
;; statuses) or responses-fit-input (the responses and their names); new data
;; for a prediction, (new-data->design-matrix who X names); and fold ids as a
;; sequence, (one-dimensional-values v).
(module* support #f
  (provide fit/c fit-input class-fit-input survival-fit-input responses-fit-input
           new-data->design-matrix one-dimensional-values))

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
(define adapters (make-ephemeron-hasheq))

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

;; What is wrong with a column of X, given its name, as a column of `kind`, or
;; #f: 'numeric, 'labels (class labels) or 'status (numbers or booleans). Any
;; column of a table can be any of them, since its entries are checked as they
;; are read.
(define ((dtype-problem X form) name [kind 'numeric])
  (and (polars-adapter? form)
       (((adapter-ref form 'column-dtype-problem) X kind) name)))

;; A flat contract named `name` on column names of named data X: `problem`
;; returns what is wrong with a value, as column-problem's given: field, or #f,
;; given the data's kind, its columns and dtype-problem.
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

;; The contract on a response argument given X: (named/c X form) for named
;; data, unnamed/c for unnamed data, and either, by the value, for a
;; table+rows.
(define (response-by-form/c X named/c unnamed/c)
  (define form (data-form X))
  (cond
    [(eq? form 'table+rows) (by-name/c (named/c X form) unnamed/c)]
    [(named-data-form? form) (named/c X form)]
    [else unnamed/c]))

;; --- responses ---------------------------------------------------------------------

;; y given X: for named data, the name of one of its numeric columns, or with
;; classes? one of class labels; otherwise one entry per observation, as
;; response/c with `elem`, or with classes? strings, symbols or booleans, or a
;; math array or a Polars series, whose entries are checked as they are read.
(define (response-for/c X elem #:classes? [classes? #f])
  (response-by-form/c X
                      (lambda (X form) (response-column/c X form classes?))
                      (labels-response/c elem (if classes? '(text boolean) '()))))

(define (response-column/c X form classes?)
  (define kind (if classes? 'labels 'numeric))
  (columns/c X form 'column-name/c
             (if classes?
                 "the name of a column of class labels of the ~a"
                 "the name of a numeric column of the ~a")
             (lambda (v what known present column)
               (if (column-name? v)
                   (column-problem (column-name->string v) what known present
                                   (lambda (s) (column s kind)))
                   (format "~e" v)))))

;; A math array or a Polars series, as the adapter that recognises it, or #f.
(define (response-value v)
  (and (not (one-dimensional? v))
       (or (library-value polars 'series? v) (library-value math 'response-array? v))))

;; The entries of v, a list, vector, flvector, f64vector, Polars series or math
;; array, as one of the first four, or #f for any other value: fold ids.
(define (one-dimensional-values v)
  (define a (and v (not (one-dimensional? v)) (response-value v)))
  (cond
    [(not a) (and v (one-dimensional? v) v)]
    [(polars-adapter? a) ((adapter-ref a 'series->list) v)]
    [else ((adapter-ref a 'array->vector) v)]))

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

;; The kind of a label, a value that names a class or a status, as
;; core/terms.rkt's value-kind gives it: 'text (a string or a symbol) or
;; 'boolean; #f for a number or another value.
(define (label-kind x)
  (case (value-kind x)
    [(text) 'text]
    [(boolean) 'boolean]
    [else #f]))

;; The entries of a non-empty list or vector v, each read once, as a list;
;; otherwise #f. A vector that another thread writes to, or an impersonator,
;; can change between reads, and the contract and the conversion each read an
;; entry once.
(define (entries v)
  (cond
    [(and (pair? v) (list? v)) v]
    [(and (vector? v) (positive? (vector-length v))) (vector->list v)]
    [else #f]))

;; The kind of the labels xs, a list, when its first entry is a label of one
;; of the `kinds`, or #f.
(define (labels-kind xs kinds)
  (define kind (and xs (label-kind (car xs))))
  (and (memq kind kinds) kind))

;; The position of the first entry of xs that is not a label of `kind`, or #f.
(define (labels-mismatch xs kind)
  (for/first ([x (in-list xs)] [k (in-naturals)]
              #:unless (eq? (label-kind x) kind))
    k))

(define kind-descriptions (hasheq 'text "a string or a symbol" 'boolean "a boolean"))

;; unnamed-response/c with elem, or, with `kinds` of labels, a non-empty list
;; or vector of labels of one of them: strings and symbols ('text) or
;; booleans ('boolean).
(define (labels-response/c elem kinds)
  (define numbers/c (unnamed-response/c elem))
  (define numbers-projection (get/build-late-neg-projection numbers/c))
  (define (passes? v)
    (define xs (entries v))
    (define kind (labels-kind xs kinds))
    (if kind
        (not (labels-mismatch xs kind))
        (contract-first-order-passes? numbers/c (or xs v))))
  (cond
    [(null? kinds) numbers/c]
    [else
     (make-flat-contract
      #:name `(or/c ,@(cdr (contract-name numbers/c))
                    ,@(for/list ([kind (in-list kinds)])
                        (if (eq? kind 'text)
                            '(response/c (or/c string? symbol?))
                            '(response/c boolean?))))
      #:first-order passes?
      #:late-neg-projection
      (lambda (blame)
        (define check (numbers-projection blame))
        (lambda (v neg-party)
          (define xs (entries v))
          (define kind (labels-kind xs kinds))
          (define k (and kind (labels-mismatch xs kind)))
          (cond
            [(not kind) (check (or xs v) neg-party) v]
            [k
             (define x (list-ref xs k))
             (raise-blame-error (blame-add-context blame (format "the element at position ~a of" k))
                                #:missing-party neg-party x
                                '(expected: "~a, as the first element is" given: "~e")
                                (hash-ref kind-descriptions kind)
                                x)]
            [else v]))))]))

;; Cox's y given X: for named data, the names of its time and status columns,
;; the time numeric and the status numbers or booleans; otherwise the times,
;; positive reals.
(define (survival-for/c X)
  (response-by-form/c
   X
   (lambda (X form)
     (columns/c X form '(list/c column-name/c column-name/c)
                "a list of the names of two columns of the ~a, the time and the status"
                (lambda (v what known present column)
                  (define status (and (list? v) (= (length v) 2) (column-name? (cadr v))
                                      (column-name->string (cadr v))))
                  (cond
                    [(and (list? v) (not (= (length v) 2)))
                     (format "~e, a list of ~a" v (if (= (length v) 1)
                                                      "one name"
                                                      (format "~a names" (length v))))]
                    [else
                     (column-list-problem v what known present
                                          (lambda (s)
                                            (column s (if (equal? s status) 'status 'numeric))))]))))
   (unnamed-response/c (>/c 0))))

;; Cox's statuses given X and y: for unnamed data, 0 or #f for a censored time
;; and 1 or #t for an event; named data has no such argument, since y names
;; the status column.
(define (statuses-for/c X y)
  (define form (data-form X))
  (cond
    [(named-form? form y)
     (flat-contract-with-explanation
      (lambda (v)
        (explain v "~a" "~e"
                 (format "no statuses argument, since y names the time and status columns of the ~a"
                         (data-kind form))
                 v))
      #:name 'none/c)]
    [else (labels-response/c (or/c 0 1) '(boolean))]))

;; mgaussian's Y given X: for named data, a non-empty list of names of its
;; numeric columns; otherwise unnamed data with one column per response.
(define (responses-for/c X)
  (response-by-form/c
   X
   (lambda (X form)
     (columns/c X form '(and/c (listof column-name/c) pair?)
                "a non-empty list of distinct names of numeric columns of the ~a"
                (lambda (v what known present column)
                  (column-list-problem v what known present column))))
   (flat-contract-with-explanation
    (lambda (v)
      (or (and (unnamed-data-form? (data-form v)) #t)
          (explain v "~a" "~e"
                   (string-append "unnamed data (a design matrix, a list or vector of rows "
                                  "or a math/matrix matrix), since X is unnamed")
                   v)))
    #:name 'unnamed-data/c)))

;; --- predictors --------------------------------------------------------------------

;; #:predictors given X and y: for named data, a non-empty list of distinct
;; names of its numeric columns, without the response, one column or several;
;; for unnamed data, only #f, since its predictors are all of its columns.
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

(define (response-names y)
  (cond
    [(column-name? y) (list (column-name->string y))]
    [(list? y) (map column-name->string (filter column-name? y))]
    [else '()]))

(define (predictor-columns/c X form y)
  (define responses (response-names y))
  (columns/c X form '(and/c (listof column-name/c) pair?)
             "a non-empty list of distinct names of numeric columns of the ~a, without the response"
             (lambda (v what known present column)
               (column-list-problem v what known present
                                    (lambda (s)
                                      (if (member s responses) "which is the response" (column s)))))))

;; fit/c's preconditions: named data needs #:predictors, and unnamed data a
;; Cox fit's statuses.
(define (predictors-problem X y predictors)
  (define form (data-form X))
  (or (not (named-form? form y))
      (and (not (unsupplied-arg? predictors)) predictors #t)
      (list (format (string-append "#:predictors is required when X is a ~a; it names the "
                                   "predictor columns, as in #:predictors '(\"wt\" \"hp\")")
                    (data-kind form)))))

(define (statuses-problem X y statuses)
  (or (named-form? (data-form X) y)
      (not (unsupplied-arg? statuses))
      (list (string-append "statuses is required when X is unnamed data; "
                           "y is then the times and statuses the events"))))

;; A keyword argument's contract, its ->i name the keyword itself, so that
;; blame names "the #:lambda argument".
(begin-for-syntax
  (define-splicing-syntax-class keyword-contract
    #:attributes (kw id ctc)
    (pattern (~seq kw:keyword ctc:expr)
             #:with id (format-id #'kw "#:~a" (keyword->string (syntax-e #'kw)))))

  ;; An unnamed Cox fit's statuses: their contract, and the precondition that
  ;; checks they are given.
  (define-splicing-syntax-class statuses-argument
    #:attributes (ctc problem)
    (pattern (~seq #:statuses ctc:expr) #:with problem #'statuses-problem)))

;; The X in the contracts of the response and the statuses is the fitter's X
;; argument, and the response argument is y unless #:argument names it. The
;; contract's name is the ->* form of the documented signature, its optional
;; keywords elided, so that an error does not print the whole ->i.
(define-syntax-parse-rule (fit/c response-ctc:expr
                                 (~optional (~seq #:argument response-id:id))
                                 (~optional status-arg:statuses-argument)
                                 (mandatory:keyword-contract ...)
                                 (optional:keyword-contract ...)
                                 result:expr)
  #:with X (datum->syntax #'response-ctc 'X)
  #:with response (or (attribute response-id) (datum->syntax #'response-ctc 'y))
  #:with predictors (format-id #'response-ctc "#:predictors")
  (rename-contract
   (->i ([X data/c]
         [response (X) response-ctc]
         (~@ mandatory.kw [mandatory.id mandatory.ctc]) ...)
        ((~? [statuses (X response) status-arg.ctc])
         #:predictors [predictors (X response) (predictors-for/c X response)]
         (~@ optional.kw [optional.id optional.ctc]) ...)
        #:pre/desc (X response predictors) (predictors-problem X response predictors)
        (~? (~@ #:pre/desc (X response statuses) (status-arg.problem X response statuses)))
        [_ result])
   '(->* (data/c response-ctc (~@ mandatory.kw mandatory.ctc) ...)
         ((~? status-arg.ctc) #:predictors (predictors-for/c X response) (... ...))
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

;; The predictors of a fitter's arguments as a design matrix, their names, or
;; #f for unnamed data, and X's form.
(define (fit-predictors who X y predictors)
  (define form (data-form X))
  (cond
    [(named-form? form y)
     (define names (map column-name->string predictors))
     (values (named->design-matrix who X form names #:checked? #t) names form)]
    [else (values (unnamed->design-matrix who "X" X form) #f form)]))

(define (fit-input who X y predictors [elem #f])
  (define-values (x names form) (fit-predictors who X y predictors))
  (values x
          (if names
              (named-response who X form (column-name->string y) x elem)
              (unnamed-response who y x elem))
          names))

(define (class-fit-input who X y predictors elem family)
  (define-values (x names form) (fit-predictors who X y predictors))
  (define-values (yv classes)
    (if names
        (named-classes who X form (column-name->string y) x elem family)
        (unnamed-classes who y x elem family)))
  (values x yv names classes))

(define (survival-fit-input who X y statuses predictors time/c status/c)
  (define-values (x names form) (fit-predictors who X y predictors))
  (cond
    [names
     (match-define (list time status) (map column-name->string y))
     (values x
             (named-response who X form time x time/c)
             (named-statuses who X form status x status/c)
             names)]
    [else
     (values x
             (unnamed-response who y x time/c "times")
             (unnamed-statuses who statuses x status/c)
             #f)]))

(define (responses-fit-input who X Y predictors)
  (define-values (x names form) (fit-predictors who X Y predictors))
  (define no (design-matrix-nrows x))
  (cond
    [names
     (define responses (map column-name->string Y))
     (define y (named->design-matrix who X form responses #:checked? #t))
     (check-column-length who form (car responses) (design-matrix-nrows y) no)
     (values x y names responses)]
    [else
     (define y (unnamed->design-matrix who "Y" Y (data-form Y)))
     (check-response-rows who y no)
     (values x y #f #f)]))

(define (check-response-rows who y no)
  (unless (= (design-matrix-nrows y) no)
    (raise-arguments-error who "Y does not have one row per row of X"
                           "rows of Y" (design-matrix-nrows y) "rows of X" no)))

;; The column `name` of a table X, a vector of its values as they are, with no
;; entries, one per row of the predictors.
(define (table-column who X form name no)
  (define column (car (select-table-values X (list name) who)))
  (check-column-length who form name (vector-length column) no)
  column)

;; The values of named data X's column `name` as they are, a vector with one
;; entry per row of the predictors x.
(define (named-values who X form name x)
  (define no (design-matrix-nrows x))
  (cond
    [(polars-adapter? form)
     (define column ((adapter-ref form 'dataframe-column-values) who X name))
     (check-column-length who form name (vector-length column) no)
     column]
    [else (table-column who X form name no)]))

(define (check-column-length who form name n no)
  (unless (= n no)
    (raise-arguments-error who (format "the ~a's columns have different lengths" (data-kind form))
                           "column" name "length" n "rows of the predictors" no)))

;; The response column `name` of named data X, whose predictors are x. An
;; entry that does not satisfy elem is an error naming its column and row.
(define (named-response who X form name x elem)
  (define no (design-matrix-nrows x))
  (define entries
    (if (polars-adapter? form)
        ((adapter-ref form 'dataframe-column->response) who X name #:checked? #t)
        (table-column->flvector (table-column who X form name no) name who)))
  (check-elements who form name elem (as-response entries no who (format "the response column ~s" name))))

;; The status column `name` of named data X, whose predictors are x: numbers,
;; or booleans, #t for an event.
(define (named-statuses who X form name x elem)
  (define what (string-append "the " (data-kind form)))
  (define statuses
    (for/flvector ([s (in-vector (named-values who X form name x))] [i (in-naturals)])
      (->finite-flonum (status->real s) who what i name)))
  (check-elements who form name elem
                  (as-response statuses (design-matrix-nrows x) who
                               (format "the response column ~s" name))))

;; yv, a response read from named data's column `name`, after checking that
;; each entry satisfies elem (when it is given), naming the row of one that
;; does not.
(define (check-elements who form name elem yv)
  (define elem? (and elem (flat-contract-predicate elem)))
  (define bad
    (and elem?
         (for/first ([k (in-range (f64vector-length yv))]
                     #:unless (elem? (f64vector-ref yv k)))
           k)))
  (cond
    [bad
     (element-error who (string-append "the " (data-kind form))
                    (format "not ~s" (contract-name elem)) (f64vector-ref yv bad)
                    #:row bad #:column name)]
    [else yv]))

;; The response y of unnamed predictors x, named `what` in errors. When y has
;; one entry per column of x rather than per row, the error suggests that x
;; may be transposed.
(define (unnamed-response who y x elem [what "y"] #:hint? [hint? #t])
  (define a (response-value y))
  (define entries
    (cond
      [(not a) y]
      [(polars-adapter? a) ((adapter-ref a 'series->response) who what y)]
      [else ((adapter-ref a 'array->reals) who what y)]))
  (check-response-length who what (one-dimensional-length entries) x hint?)
  (as-response entries (design-matrix-nrows x) who what elem))

;; The statuses of unnamed predictors x: numbers, or booleans, #t for an event.
(define (unnamed-statuses who statuses x elem)
  (define xs (entries statuses))
  (define labels (unnamed-labels who "statuses" statuses xs '(boolean)))
  (cond
    [labels
     (check-response-length who "statuses" (vector-length labels) x #f)
     (as-response (vector-map status->real labels) (design-matrix-nrows x) who "statuses" elem)]
    [else (unnamed-response who (or xs statuses) x elem "statuses" #:hint? #f)]))

;; Raises an error unless a response named `what` has n entries, one per row
;; of x; with hint?, one with an entry per column suggests that x is
;; transposed.
(define (check-response-length who what n x hint?)
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (cond
    [(= n no) (void)]
    [(and hint? (= n ni))
     (raise-arguments-error who (format "~a does not have one entry per row of X" what)
                            (format "length of ~a" what) n "rows of X" no "columns of X" ni
                            "hint" (unquoted-printing-string
                                    "rows are observations; is X transposed?"))]
    [else
     (raise-arguments-error who (format "~a does not have one entry per row of X" what)
                            (format "length of ~a" what) n "rows of X" no)]))

;; --- classes -----------------------------------------------------------------------

;; The class response column `name` of named data X: its class indices and
;; classes when it holds strings, symbols or booleans, and otherwise its
;; numbers and #f.
(define (named-classes who X form name x elem family)
  (define values-of-column (named-values who X form name x))
  (define-values (classes indices) (response-classes who name values-of-column))
  (cond
    [classes
     (check-classes who family classes (list "column" name))
     (values (as-response indices (design-matrix-nrows x) who "y") classes)]
    [else (values (named-response who X form name x elem) #f)]))

;; The labels, of one of the `kinds`, of an unnamed response y, named `what`
;; in errors, as a vector: y's entries when the first is such a label, whether
;; y is a list or a vector, whose entries are xs, a Polars series that is not
;; numeric or a math array; otherwise #f.
(define (unnamed-labels who what y xs kinds)
  (define a (and (not xs) (response-value y)))
  (define series-labels
    (and (polars-adapter? a) (not ((adapter-ref a 'numeric-series?) y))
         ((adapter-ref a 'series-values) who what y)))
  (define array-entries
    (and a (not (polars-adapter? a)) (vector->list ((adapter-ref a 'array->vector) y))))
  (cond
    [series-labels series-labels]
    [(labels-kind xs kinds) (list->vector xs)]
    [(labels-kind array-entries kinds) (list->vector array-entries)]
    [else #f]))

;; The class response y of unnamed predictors x: as named-classes.
(define (unnamed-classes who y x elem family)
  (define xs (entries y))
  (define labels (unnamed-labels who "y" y xs '(text boolean)))
  (cond
    [labels
     (check-response-length who "y" (vector-length labels) x #t)
     (define-values (classes indices) (response-classes who "y" labels))
     (check-classes who family classes '())
     (values (as-response indices (design-matrix-nrows x) who "y") classes)]
    [else (values (unnamed-response who (or xs y) x elem) #f)]))
