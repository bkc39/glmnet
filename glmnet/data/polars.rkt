#lang racket/base

(require racket/contract
         racket/list
         racket/match
         ffi/vector
         (only-in polars
                  dataframe dataframe? series
                  dataframe->f64vector dataframe->columns series->list in-series
                  column-names height ref dtype null-count polars-null?)
         (only-in "../data.rkt" design-matrix? design-matrix-column-names table?
                  table-column-names)
         (only-in (submod "../data.rkt" support)
                  design-matrix-data design-matrix-nrows design-matrix-ncols
                  column-name->string select-table-values flat->design-matrix
                  element-error missing-error default-column-names))

(define column-name/c (or/c string? symbol?))

(provide
 (contract-out
  [polars->design-matrix
   (->i ([df (and/c dataframe? dataframe-with-rows/c)]
         [columns (df) (frame-columns/c df numeric-dtype? "numeric")])
        [result design-matrix?])]
  [design-matrix->polars
   (->i ([dm design-matrix?])
        (#:column-names [names (dm) (column-names-for/c (design-matrix-ncols dm))])
        [result dataframe?])]
  [polars->response
   (->i ([df (and/c dataframe? dataframe-with-rows/c)]
         [column (df) (frame-column/c df numeric-dtype? "numeric")])
        [result (and/c (listof real?) pair?)])]
  [polars->table
   (->i ([df (and/c dataframe? dataframe-with-rows/c)])
        ([columns (df) (frame-columns/c df table-dtype? table-dtypes)])
        #:pre/desc (df columns) (or (not (unsupplied-arg? columns)) (whole-frame-problem df))
        [result table?])]
  [table->polars
   (->i ([t table?])
        ([columns (t) (table-columns/c t)])
        [result dataframe?])]))

;; --- dtypes --------------------------------------------------------------------

(define (numeric-dtype? d)
  (and (memq d '(int8 int16 int32 int64 uint8 uint16 uint32 uint64 float32 float64)) #t))

(define (float-dtype? d)
  (and (memq d '(float32 float64)) #t))

;; The dtypes whose values a table holds: numbers, booleans, strings, and the
;; symbols that dataframe->columns makes of a categorical or enum column.
(define (table-dtype? d)
  (match d
    [(or 'boolean 'string 'categorical (cons 'enum _)) #t]
    [_ (numeric-dtype? d)]))

(define table-dtypes "numeric, boolean, string, categorical or enum")

;; --- contracts -----------------------------------------------------------------

(define ((explain v expected given . args) blame)
  (apply raise-blame-error blame v (list 'expected: expected 'given: given) args))

(define dataframe-with-rows/c
  (flat-contract-with-explanation
   (lambda (df)
     (or (positive? (height df))
         (explain df "a dataframe with at least one row" "a dataframe with no rows")))
   #:name 'dataframe-with-rows/c))

;; The column names `present`, strings, as a set to look names up in.
(define (name-set present)
  (for/hash ([name (in-list present)]) (values name #t)))

;; What is wrong with the column name `s` (a string) of the `what` ("dataframe"
;; or "table") whose columns are `present`, with the set `known`, as a string
;; for a blame error's given: field; #f when nothing is. `problem` is #f for a
;; column it accepts, or what is wrong with it.
(define (column-problem s what known present problem)
  (cond
    [(not (hash-ref known s #f))
     (format "~s, which is not a column of the ~a; its columns are ~s" s what present)]
    [(problem s) => (lambda (p) (format "column ~s, ~a" s p))]
    [else #f]))

;; A non-empty list of distinct column names, each without a problem.
(define (column-list/c expected what present problem)
  (define known (name-set present))
  (flat-contract-with-explanation
   (lambda (names)
     (define given
       (cond
         [(not (and (list? names) (pair? names) (andmap column-name/c names)))
          (format "~e" names)]
         [(check-duplicates names #:key column-name->string)
          => (lambda (name) (format "~s twice" (column-name->string name)))]
         [else
          (for/or ([name (in-list names)])
            (column-problem (column-name->string name) what known present problem))]))
     (or (not given) (explain names "~a" "~a" expected given)))
   #:name '(and/c (listof (or/c string? symbol?)) pair?)))

;; What is wrong with column `name` of df when its dtype must satisfy
;; `accepts?`, or #f.
(define ((dtype-problem df accepts?) name)
  (define d (dtype (ref df name)))
  (and (not (accepts? d)) (format "of dtype ~s" d)))

;; A non-empty list of distinct names of columns of df whose dtypes satisfy
;; `accepts?`, which `kind` describes.
(define (frame-columns/c df accepts? kind)
  (column-list/c (format "a non-empty list of distinct names of ~a columns of the dataframe" kind)
                 "dataframe" (column-names df) (dtype-problem df accepts?)))

;; The name of one column of df whose dtype satisfies `accepts?`.
(define (frame-column/c df accepts? kind)
  (define expected (format "the name of a ~a column of the dataframe" kind))
  (define present (column-names df))
  (define known (name-set present))
  (flat-contract-with-explanation
   (lambda (name)
     (define given
       (if (column-name/c name)
           (column-problem (column-name->string name) "dataframe" known present
                           (dtype-problem df accepts?))
           (format "~e" name)))
     (or (not given) (explain name "~a" "~a" expected given)))
   #:name '(or/c string? symbol?)))

;; A non-empty list of distinct names of the table's columns.
(define (table-columns/c t)
  (column-list/c "a non-empty list of distinct names of columns of the table"
                 "table" (table-column-names t) (lambda (name) #f)))

;; `ncols` distinct names, strings or symbols.
(define (column-names-for/c ncols)
  (flat-contract-with-explanation
   (lambda (names)
     (define expected (format "~a distinct column names, strings or symbols" ncols))
     (cond
       [(not (and (list? names) (andmap column-name/c names)))
        (explain names "~a" "~e" expected names)]
       [(not (= (length names) ncols))
        (explain names "~a" "~a names: ~e" expected (length names) names)]
       [(check-duplicates names #:key column-name->string)
        => (lambda (name) (explain names "~a" "~s twice" expected (column-name->string name)))]
       [else #t]))
   #:name '(listof (or/c string? symbol?))))

;; Why df as a whole cannot be a table, for polars->table's precondition.
(define (whole-frame-problem df)
  (define names (column-names df))
  (cond
    [(null? names) (list "the dataframe has no columns")]
    [(for/first ([name (in-list names)]
                 #:unless (table-dtype? (dtype (ref df name))))
       name)
     => (lambda (name)
          (list (format "every column of the dataframe must be ~a" table-dtypes)
                (format "column ~s has dtype ~s" name (dtype (ref df name)))))]
    [else #t]))

;; --- missing and non-finite values -------------------------------------------------

(define (check-no-null who s name)
  (unless (zero? (null-count s))
    (missing-error who "the dataframe"
                   #:row (for/first ([x (in-series s)] [i (in-naturals)] #:when (polars-null? x)) i)
                   #:column name)))

;; --- polars -> glmnet ------------------------------------------------------------

(define (polars->design-matrix df columns)
  (define who 'polars->design-matrix)
  (define names (map column-name->string columns))
  (for ([name (in-list names)])
    (check-no-null who (ref df name) name))
  (define-values (v nrows ncols) (dataframe->f64vector df #:columns names #:null 'error))
  (flat->design-matrix v nrows ncols names who "the dataframe" #:adopt? #t))

(define (polars->response df column)
  (define who 'polars->response)
  (define name (column-name->string column))
  (define s (ref df name))
  (check-no-null who s name)
  (define ys (series->list s))
  (when (float-dtype? (dtype s))
    (for ([y (in-list ys)] [i (in-naturals)])
      (unless (< (abs y) +inf.0)
        (element-error who "the dataframe" "not finite" y #:row i #:column name))))
  ys)

(define (polars->table df [columns (column-names df)])
  (define who 'polars->table)
  (define names (map column-name->string columns))
  (for ([name (in-list names)])
    (check-no-null who (ref df name) name))
  (dataframe->columns df #:columns names))

;; --- glmnet -> polars ------------------------------------------------------------

(define (design-matrix->polars dm
                               #:column-names
                               [names (or (design-matrix-column-names dm)
                                          (default-column-names (design-matrix-ncols dm)))])
  (define v (design-matrix-data dm))
  (define no (design-matrix-nrows dm))
  (dataframe
   (for/list ([name (in-list names)] [j (in-naturals)])
     (define from (* j no))
     (series (for/vector #:length no ([i (in-range no)]) (f64vector-ref v (+ from i)))
             #:name (column-name->string name)
             #:dtype 'float64))))

;; What a table's value is to Polars: a kind of number, boolean, string or
;; symbol; #f for a value that no dtype holds. An exact integer is 'natural
;; (0 to 2^63 - 1), 'integer (negative, down to -2^63) or 'large (2^63 to
;; 2^64 - 1). Any other real that is not a flonum is 'real, or 'not-finite
;; when its flonum is an infinity.
(define int64-min (- (expt 2 63)))
(define uint64-min (expt 2 63))
(define uint64-limit (expt 2 64))

(define (value-kind x)
  (cond
    [(flonum? x) 'real]
    [(exact-integer? x)
     (cond
       [(fixnum? x) (if (negative? x) 'integer 'natural)]
       [(< x int64-min) #f]
       [(negative? x) 'integer]
       [(< x uint64-min) 'natural]
       [(< x uint64-limit) 'large]
       [else #f])]
    [(real? x) (if (rational? (real->double-flonum x)) 'real 'not-finite)]
    [(boolean? x) 'boolean]
    [(string? x) 'string]
    [(symbol? x) 'symbol]
    [else #f]))

;; The kind of a column with values of kinds a and b, or #f when no dtype
;; holds both: naturals widen to integers or to large ones, and symbols to
;; strings. Integers and large ones together are a 'conflict, which no integer
;; dtype holds, and any number with a real is a real. The join does not
;; depend on the order of the values.
(define (join-kinds a b)
  (match* (a b)
    [(k k) k]
    [('natural (or 'integer 'large 'conflict 'real)) b]
    [((or 'integer 'large 'conflict 'real) 'natural) a]
    [((or 'integer 'large 'conflict) (or 'integer 'large 'conflict)) 'conflict]
    [((or 'integer 'large 'conflict 'real) (or 'integer 'large 'conflict 'real)) 'real]
    [((or 'string 'symbol) (or 'string 'symbol)) 'string]
    [(_ _) #f]))

(define kind-dtypes
  (hasheq 'natural 'int64 'integer 'int64 'large 'uint64 'real 'float64
          'boolean 'boolean 'string 'string 'symbol 'categorical))

;; A table's column, its values `xs` a vector, as a series with the dtype its
;; values call for.
(define (column->series who name xs)
  (define (no-dtype i)
    (raise-arguments-error who "the table has a column whose values no Polars dtype holds"
                           "column" name "row" i "element" (vector-ref xs i)))
  (define-values (kind conflict-row)
    (for/fold ([kind (value-kind (vector-ref xs 0))]
               [conflict-row #f])
              ([x (in-vector xs)] [i (in-naturals)])
      (define k (value-kind x))
      (when (eq? k 'not-finite)
        (element-error who "the table" "not finite" x #:row i #:column name))
      (cond
        [(and k (eq? k kind)) (values kind conflict-row)]
        [else
         (define joined (or (and kind k (join-kinds kind k)) (no-dtype i)))
         (values joined (or conflict-row (and (eq? joined 'conflict) i)))])))
  (when (eq? kind 'conflict)
    (no-dtype conflict-row))
  (define entries
    (if (eq? kind 'string)
        (for/vector #:length (vector-length xs) ([x (in-vector xs)])
          (if (symbol? x) (symbol->string x) x))
        xs))
  (series entries #:name name #:dtype (hash-ref kind-dtypes kind)))

(define (table->polars t [columns (table-column-names t)])
  (define who 'table->polars)
  (define names (map column-name->string columns))
  (dataframe
   (for/list ([name (in-list names)]
              [xs (in-list (select-table-values t names who))])
     (column->series who name xs))))
