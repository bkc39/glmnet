#lang racket/base

;; The rkt-polars adapter (#40): Polars dataframes to and from design
;; matrices, responses and tables. A design matrix comes out of Polars' bulk
;; column-major export, dataframe->f64vector, and is adopted without a copy.
;; `(require glmnet)` does not load this module, so it does not load Polars.

(require racket/contract
         racket/list
         racket/match
         ffi/vector
         (only-in polars
                  dataframe dataframe? series series? series-name
                  dataframe->f64vector dataframe->columns series->list in-series
                  column-names height ref len dtype null-count polars-null?)
         (only-in "../data.rkt" design-matrix? design-matrix-column-names table?
                  table-column-names)
         (only-in (submod "../data.rkt" support)
                  design-matrix-data design-matrix-nrows design-matrix-ncols
                  column-name->string select-table-values adopt-f64vector))

(define column-name/c (or/c string? symbol?))

(provide
 (contract-out
  [polars->design-matrix
   (->i ([df (and/c dataframe? dataframe-with-rows/c)]
         [columns (df) (frame-columns/c df numeric-dtype? "numeric")])
        [result design-matrix?])]
  [design-matrix->polars (-> design-matrix? dataframe?)]
  [polars->response
   (-> (and/c series? numeric-series/c)
       (and/c (listof real?) pair?))]
  [polars->table
   (->i ([df dataframe?])
        ([columns (df) (frame-columns/c df table-dtype? "numeric, boolean, string or categorical")])
        [result table?])]
  [table->polars
   (->* (table?) ((and/c (listof column-name/c) pair?)) dataframe?)]))

;; --- dtypes --------------------------------------------------------------------

(define (numeric-dtype? d)
  (and (memq d '(int8 int16 int32 int64 uint8 uint16 uint32 uint64 float32 float64)) #t))

(define (float-dtype? d)
  (and (memq d '(float32 float64)) #t))

;; The dtypes whose values a table holds: numbers, booleans, strings, and the
;; symbols that series->vector makes of a categorical or enum column.
(define (table-dtype? d)
  (match d
    [(or 'boolean 'string 'categorical (cons 'enum _)) #t]
    [_ (numeric-dtype? d)]))

;; --- contracts -----------------------------------------------------------------

(define dataframe-with-rows/c
  (flat-contract-with-explanation
   (lambda (df)
     (or (positive? (height df))
         (lambda (blame)
           (raise-blame-error blame df '(expected: "a dataframe with at least one row"
                                         given: "a dataframe with no rows")))))
   #:name 'dataframe-with-rows/c))

(define numeric-series/c
  (flat-contract-with-explanation
   (lambda (s)
     (cond
       [(not (numeric-dtype? (dtype s)))
        (lambda (blame)
          (raise-blame-error blame s '(expected: "a numeric series" given: "~s, of dtype ~s")
                             (series-name s) (dtype s)))]
       [(zero? (len s))
        (lambda (blame)
          (raise-blame-error blame s '(expected: "a series with at least one element"
                                       given: "~s, which is empty")
                             (series-name s)))]
       [else #t]))
   #:name 'numeric-series/c))

;; What is wrong with `names` as a list of columns of df whose dtypes satisfy
;; `accepts?`, as a string for a blame error's given: field, or #f.
(define (column-list-problem df names accepts?)
  (define present (column-names df))
  (let loop ([names names] [seen '()])
    (match names
      ['() #f]
      [(cons name rest)
       (define s (column-name->string name))
       (cond
         [(not (member s present))
          (format "~s, which is not a column of the dataframe; its columns are ~s" s present)]
         [(member s seen) (format "~s twice" s)]
         [(not (accepts? (dtype (ref df s))))
          (format "column ~s, of dtype ~s" s (dtype (ref df s)))]
         [else (loop rest (cons s seen))])])))

;; A non-empty list of distinct names of columns of df, each with a dtype that
;; `accepts?`, which `kind` describes.
(define (frame-columns/c df accepts? kind)
  (define expected
    (format "a non-empty list of distinct names of ~a columns of the dataframe" kind))
  (flat-contract-with-explanation
   (lambda (names)
     (define problem
       (if (and (list? names) (pair? names) (andmap column-name/c names))
           (column-list-problem df names accepts?)
           (format "~e" names)))
     (or (not problem)
         (lambda (blame)
           (raise-blame-error blame names '(expected: "~a" given: "~a") expected problem))))
   #:name 'frame-columns/c))

;; --- nulls and non-finite values -------------------------------------------------

(define (first-null-row s)
  (for/first ([x (in-series s)] [i (in-naturals)] #:when (polars-null? x)) i))

(define (check-no-null who what s)
  (unless (zero? (null-count s))
    (raise-arguments-error who (format "~a has a null" what)
                           "column" (series-name s) "row" (first-null-row s))))

;; --- polars -> glmnet ------------------------------------------------------------

(define (polars->design-matrix df columns)
  (define who 'polars->design-matrix)
  (define names (map column-name->string columns))
  (for ([name (in-list names)])
    (check-no-null who "the dataframe" (ref df name)))
  (define-values (v nrows _ncols) (dataframe->f64vector df #:columns names #:null 'error))
  (adopt-f64vector v nrows names who "the dataframe"))

(define (polars->response s)
  (define who 'polars->response)
  (check-no-null who "the series" s)
  (define ys (series->list s))
  (when (float-dtype? (dtype s))
    (for ([y (in-list ys)] [i (in-naturals)])
      (unless (< (abs y) +inf.0)
        (raise-arguments-error who "the series has an element that is not finite"
                               "column" (series-name s) "row" i "element" y))))
  ys)

(define (polars->table df [columns #f])
  (define who 'polars->table)
  (define names (if columns (map column-name->string columns) (all-table-columns who df)))
  (for ([name (in-list names)])
    (check-no-null who "the dataframe" (ref df name)))
  (dataframe->columns df #:columns names))

;; Every column of df, the columns polars->table converts when it is given
;; none, each of which must have a dtype that a table holds.
(define (all-table-columns who df)
  (define names (column-names df))
  (when (null? names)
    (raise-arguments-error who "the dataframe has no columns"))
  (for ([name (in-list names)])
    (define d (dtype (ref df name)))
    (unless (table-dtype? d)
      (raise-arguments-error who "the dataframe has a column whose dtype a table cannot hold"
                             "column" name "dtype" d)))
  names)

;; --- glmnet -> polars ------------------------------------------------------------

(define (design-matrix->polars dm)
  (define v (design-matrix-data dm))
  (define no (design-matrix-nrows dm))
  (define names
    (or (design-matrix-column-names dm)
        (for/list ([j (in-range (design-matrix-ncols dm))])
          (format "column_~a" j))))
  (dataframe
   (for/list ([name (in-list names)] [j (in-naturals)])
     (define from (* j no))
     (series (for/vector #:length no ([i (in-range no)]) (f64vector-ref v (+ from i)))
             #:name (column-name->string name)
             #:dtype 'float64))))

;; What a table's value is to Polars, or #f for a value no dtype holds.
(define (value-kind x)
  (cond
    [(flonum? x) 'real]
    [(fixnum? x) 'integer]
    [(exact-integer? x) (if (<= (- (expt 2 63)) x (sub1 (expt 2 63))) 'integer 'real)]
    [(real? x) 'real]
    [(boolean? x) 'boolean]
    [(string? x) 'string]
    [(symbol? x) 'symbol]
    [else #f]))

;; The kind of a column with values of kinds a and b, or #f when no dtype
;; holds both: integers widen to reals, and symbols to strings.
(define (join-kinds a b)
  (match* (a b)
    [(k k) k]
    [((or 'integer 'real) (or 'integer 'real)) 'real]
    [((or 'string 'symbol) (or 'string 'symbol)) 'string]
    [(_ _) #f]))

(define kind-dtypes
  (hasheq 'integer 'int64 'real 'float64 'boolean 'boolean 'string 'string 'symbol 'categorical))

;; A table's column, its values `xs` a vector, as a series with the dtype
;; its values call for.
(define (column->series who name xs)
  (define kind
    (for/fold ([kind (value-kind (vector-ref xs 0))])
              ([x (in-vector xs)] [i (in-naturals)])
      (define k (value-kind x))
      (or (if (eq? k kind) k (and kind k (join-kinds kind k)))
          (raise-arguments-error who "the table has a column whose values no Polars dtype holds"
                                 "column" name "row" i "element" x))))
  (define entries
    (if (eq? kind 'string)
        (for/vector #:length (vector-length xs) ([x (in-vector xs)])
          (if (symbol? x) (symbol->string x) x))
        xs))
  (series entries #:name name #:dtype (hash-ref kind-dtypes kind)))

(define (table->polars t [columns (table-column-names t)])
  (define who 'table->polars)
  (define names (map column-name->string columns))
  (define dup (check-duplicates names))
  (when dup
    (raise-arguments-error who "a column is named twice" "name" dup))
  (dataframe
   (for/list ([name (in-list names)]
              [xs (in-list (select-table-values t names who))])
     (column->series who name xs))))
