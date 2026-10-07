#lang racket/base

;; The one boundary between the user's data and the fitters (#74): predictors
;; and a response in any supported format, unnamed (a design matrix, nested
;; rows, a math/matrix matrix; a sequence, a math array or a Polars series) or
;; named (a table or a Polars dataframe, with the response and predictors
;; selected by column name), as the design matrix and f64vector response that
;; the solvers read. glmnet loads neither Polars nor math/matrix: their values
;; are recognised only once the program has loaded the library, in glmnet's
;; own module registry, and are then converted by their adapter's support
;; submodule (data/polars.rkt, data/math.rkt), loaded on first use.

(require racket/contract
         racket/list
         racket/promise
         syntax/parse/define
         (for-syntax racket/base syntax/parse racket/syntax)
         (only-in "../data.rkt" design-matrix? response/c table? table-column-names)
         (submod "../data.rkt" support))

(define column-name/c (or/c string? symbol?))

(provide
 (contract-out
  [data/c flat-contract?]
  [named-data? (-> any/c boolean?)]
  [response-for/c (-> any/c flat-contract? flat-contract?)]
  [predictors-for/c (-> any/c any/c flat-contract?)]))

;; For the family modules and core/model.rkt only; not part of the public API.
;;
;;   (fit/c elem (mandatory-kw ctc ...) (optional-kw ctc ...) result)
;;     The contract of a fitter (X y #:predictors ...): X is data/c, y
;;     (response-for/c X elem), #:predictors (predictors-for/c X y), required
;;     for named data, then the fitter's own keywords as ->* lists them.
;;   (fit-input who X y predictors [elem])
;;     The design matrix, the response as an f64vector (each entry satisfying
;;     elem as a flonum, when it is given) and the predictors' names (#f for
;;     unnamed data) of a fitter's arguments.
;;   (unnamed->design-matrix who what X)
;;     Unnamed data X as a design matrix.
;;   (named->design-matrix who X names)
;;     The columns of named data X with the given names, as a design matrix.
(module* support #f
  (provide fit/c fit-input unnamed->design-matrix named->design-matrix))

;; --- formats glmnet does not load ----------------------------------------------

;; A data format whose library glmnet does not load: present once the program
;; has declared one of `libraries`; then the exports of its adapter's support
;; submodule are found by name, and kept.
(struct lazy-format (libraries adapter [exports #:mutable]))

(define (adapter-path relative)
  (module-path-index-join `(submod ,relative support)
                          (variable-reference->module-path-index (#%variable-reference))))

(define polars-format (lazy-format '(polars) (adapter-path "../data/polars.rkt") #f))
(define math-format (lazy-format '(math/array math/matrix) (adapter-path "../data/math.rkt") #f))

;; A namespace with glmnet's module registry, where the program's libraries are
;; declared.
(define registry (delay (variable-reference->empty-namespace (#%variable-reference))))

;; Whether the program has loaded the format's library.
(define (format-loaded? fmt)
  (cond
    [(lazy-format-exports fmt) #t]
    [(parameterize ([current-namespace (force registry)])
       (for/or ([library (in-list (lazy-format-libraries fmt))])
         (module-declared? library #f)))
     (set-lazy-format-exports! fmt (make-hasheq))
     #t]
    [else #f]))

;; The adapter's export `name`, once the format is loaded.
(define (format-ref fmt name)
  (hash-ref! (lazy-format-exports fmt) name
             (lambda ()
               (parameterize ([current-namespace (force registry)])
                 (dynamic-require (lazy-format-adapter fmt) name)))))

(define ((format-predicate fmt name) v)
  (and (format-loaded? fmt) ((format-ref fmt name) v)))

(define dataframe? (format-predicate polars-format 'dataframe?))
(define series? (format-predicate polars-format 'series?))
(define math-matrix? (format-predicate math-format 'math-matrix?))
(define response-array? (format-predicate math-format 'response-array?))

(define ((polars name) . args) (apply (format-ref polars-format name) args))
(define ((math name) . args) (apply (format-ref math-format name) args))

;; --- what data is --------------------------------------------------------------

;; Named data selects its predictors and response by column name: a table that
;; is not a design matrix, or a Polars dataframe. A design matrix is unnamed
;; data even when its columns have names, as it was before named data.
(define (named-data? v)
  (or (and (table? v) (not (design-matrix? v)))
      (dataframe? v)))

(define (unnamed-data? v)
  (or (design-matrix? v) (nested-matrix? v) (math-matrix? v)))

(define data/c
  (flat-named-contract
   '(or/c design-matrix? (listof (or/c list? vector?)) (vectorof (or/c list? vector?))
          (and/c array? matrix?) table? dataframe?)
   (lambda (v) (or (unnamed-data? v) (named-data? v)))))

;; --- column names ------------------------------------------------------------------

;; The names of named data's columns, as strings, in its order.
(define (data-column-names X)
  (if (dataframe? X)
      ((polars 'column-names) X)
      (table-column-names X)))

;; Whether X's column `name` (a string, one of its columns) can be read as
;; numbers: any column of a table, whose entries are checked as they are read,
;; and a column of a dataframe with a numeric dtype.
(define (numeric-column? X name)
  (or (not (dataframe? X)) ((polars 'numeric-column?) X name)))

(define (data-kind X) (if (dataframe? X) "dataframe" "table"))

;; What is wrong with `name` as the name of a numeric column of named data X,
;; whose columns are `present` (a set of strings `known`), or #f.
(define (column-problem X name present known)
  (define s (column-name->string name))
  (cond
    [(not (hash-ref known s #f))
     (format "~s, which is not a column of the ~a; its columns are ~s" s (data-kind X) present)]
    [(not (numeric-column? X s))
     (format "~s, a column of the ~a that is not numeric" s (data-kind X))]
    [else #f]))

(define (name-set names)
  (for/hash ([name (in-list names)]) (values name #t)))

(define ((explain v expected given) blame)
  (raise-blame-error blame v (list 'expected: expected 'given: "~a") given))

;; --- responses ---------------------------------------------------------------------

;; y given X: for named data, the name of one of its numeric columns; otherwise
;; one entry per observation, as response/c with `elem`, a math array or a
;; Polars series, whose entries are checked as they are read.
(define (response-for/c X elem)
  (if (named-data? X)
      (response-column/c X)
      (unnamed-response/c elem)))

(define (response-column/c X)
  (define present (data-column-names X))
  (define known (name-set present))
  (define expected (format "the name of a numeric column of the ~a" (data-kind X)))
  (flat-contract-with-explanation
   (lambda (name)
     (define given
       (if (column-name/c name)
           (column-problem X name present known)
           (format "~e" name)))
     (or (not given) (explain name expected given)))
   #:name 'column-name/c))

(define (unnamed-response/c elem)
  (define sequence/c (response/c elem))
  (define sequence-projection (get/build-late-neg-projection sequence/c))
  (define (other? v) (or (series? v) (response-array? v)))
  (make-flat-contract
   #:name `(or/c ,(contract-name sequence/c) series? array?)
   #:first-order (lambda (v) (or (other? v) (contract-first-order-passes? sequence/c v)))
   #:late-neg-projection
   (lambda (blame)
     (define check (sequence-projection blame))
     (lambda (v neg-party)
       (if (other? v) v (check v neg-party))))))

;; --- predictors --------------------------------------------------------------------

;; #:predictors given X and y: for named data, a non-empty list of distinct
;; names of its numeric columns, without the response; for unnamed data, only
;; #f, since its predictors are all of its columns.
(define (predictors-for/c X y)
  (if (named-data? X)
      (predictor-columns/c X y)
      (flat-contract-with-explanation
       (lambda (v)
         (or (not v)
             (explain v "#f, since X is not named data (a table or a dataframe)"
                      (format "~e" v))))
       #:name 'not)))

(define (predictor-columns/c X y)
  (define present (data-column-names X))
  (define known (name-set present))
  (define response (and (column-name/c y) (column-name->string y)))
  (define expected
    (format "a non-empty list of distinct names of numeric columns of the ~a, without the response"
            (data-kind X)))
  (flat-contract-with-explanation
   (lambda (names)
     (define given
       (cond
         [(not (and (list? names) (pair? names) (andmap column-name/c names)))
          (format "~e" names)]
         [(check-duplicates names #:key column-name->string)
          => (lambda (name) (format "~s twice" (column-name->string name)))]
         [(for/first ([name (in-list names)]
                      #:when (equal? (column-name->string name) response))
            name)
          => (lambda (name) (format "~s, which is the response" (column-name->string name)))]
         [else
          (for/or ([name (in-list names)])
            (column-problem X name present known))]))
     (or (not given) (explain names expected given)))
   #:name '(and/c (listof column-name/c) pair?)))

;; fit/c's precondition: named data needs #:predictors.
(define (predictors-problem X predictors)
  (or (not (named-data? X))
      (and (not (unsupplied-arg? predictors)) predictors #t)
      (list (format (string-append "#:predictors is required when X is a ~a; it names the "
                                   "predictor columns, as in #:predictors '(\"wt\" \"hp\")")
                    (data-kind X)))))

(begin-for-syntax
  (define-splicing-syntax-class keyword-contract
    #:attributes (kw id ctc)
    (pattern (~seq kw:keyword ctc:expr)
             #:with id (format-id #'kw "~a" (keyword->string (syntax-e #'kw))))))

(define-syntax-parse-rule (fit/c elem:expr (mandatory:keyword-contract ...)
                                 (optional:keyword-contract ...)
                                 result:expr)
  (->i ([X data/c]
        [y (X) (response-for/c X elem)]
        (~@ mandatory.kw [mandatory.id mandatory.ctc]) ...)
       (#:predictors [predictors (X y) (predictors-for/c X y)]
        (~@ optional.kw [optional.id optional.ctc]) ...)
       #:pre/desc (X predictors) (predictors-problem X predictors)
       [_ result]))

;; --- conversion --------------------------------------------------------------------

(define (unnamed->design-matrix who what X)
  (cond
    [(design-matrix? X) X]
    [(nested-matrix? X) (nested->dm X 'rows #f who what)]
    [else ((math 'matrix->dm) who what X)]))

(define (named->design-matrix who X names)
  (if (dataframe? X)
      ((polars 'dataframe->design-matrix) who X names)
      (select-table-columns X names who)))

(define (fit-input who X y predictors [elem #f])
  (cond
    [(named-data? X)
     (define names (map column-name->string predictors))
     (define x (named->design-matrix who X names))
     (values x (named-response who X (column-name->string y) x elem) names)]
    [else
     (define x (unnamed->design-matrix who "X" X))
     (values x (unnamed-response who y x elem) #f)]))

;; The response column `name` of named data X, whose predictors are x.
(define (named-response who X name x elem)
  (define no (design-matrix-nrows x))
  (define entries
    (cond
      [(dataframe? X) ((polars 'dataframe-column->response) who X name)]
      [else
       (define column (car (select-table-values X (list name) who)))
       (unless (= (vector-length column) no)
         (raise-arguments-error who "the table's columns have different lengths"
                                "column" name "length" (vector-length column)
                                "rows of the predictors" no))
       (table-column->flvector column name who)]))
  (as-response entries no who (format "the response column ~s" name) elem))

;; The response y of unnamed predictors x. When y has one entry per column of
;; x rather than per row, the error suggests that x may be transposed.
(define (unnamed-response who y x elem)
  (define entries
    (cond
      [(series? y) ((polars 'series->response) who "y" y)]
      [(response-array? y) ((math 'array->reals) who "y" y)]
      [else y]))
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
