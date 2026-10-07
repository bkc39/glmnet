#lang racket/base

(require racket/contract
         racket/list
         racket/match
         ffi/vector
         (only-in polars
                  dataframe dataframe? series series?
                  dataframe->f64vector dataframe->columns series->list in-series
                  column-names height ref dtype null-count polars-null?)
         (only-in "../data.rkt" design-matrix? design-matrix-column-names table?
                  table-column-names)
         (only-in (submod "../data.rkt" support)
                  design-matrix-data design-matrix-nrows design-matrix-ncols
                  column-name? column-name->string select-table-values flat->design-matrix
                  element-error missing-error default-column-names
                  name-set explain column-problem column-list-problem))

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

;; For glmnet's data boundary (core/input.rkt) only: the conversions above with
;; the name of the procedure the user called. With #:checked? #t, the caller's
;; contract has checked the column names and dtypes, as numeric-column-problem
;; does; otherwise they are checked here.
(module* support #f
  (provide dataframe? series? column-names numeric-column-problem label-column-problem numeric-series?
           dataframe->design-matrix dataframe-column->response series->response
           dataframe-column-values series-values))

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

(define dataframe-with-rows/c
  (flat-contract-with-explanation
   (lambda (df)
     (or (positive? (height df))
         (explain df "a dataframe with at least one row" "a dataframe with no rows")))
   #:name 'dataframe-with-rows/c))

;; A non-empty list of distinct column names, each without a problem.
(define (column-list/c expected what present problem)
  (define known (name-set present))
  (flat-contract-with-explanation
   (lambda (names)
     (define given (column-list-problem names what known present problem))
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
       (if (column-name? name)
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
       [(not (and (list? names) (andmap column-name? names)))
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

;; What is wrong with df's column `name`, a string, as a numeric column, or #f.
(define (numeric-column-problem df)
  (dtype-problem df numeric-dtype?))

;; What is wrong with df's column `name`, a string, as a column of values a
;; table can hold, such as the class labels of a response: numbers, booleans,
;; strings, or the symbols of a categorical or enum column; or #f.
(define (label-column-problem df)
  (dtype-problem df table-dtype?))

(define (numeric-series? s)
  (numeric-dtype? (dtype s)))

;; Checks that df has rows and, unless checked? is #t, a numeric column with
;; each of `names`, strings.
(define (check-numeric-columns who df names checked?)
  (define present (if checked? '() (column-names df)))
  (define known (name-set present))
  (define absent
    (and (not checked?) (findf (lambda (name) (not (hash-ref known name #f))) names)))
  (define text
    (and (not checked?) (not absent)
         (findf (lambda (name) (not (numeric-dtype? (dtype (ref df name))))) names)))
  (cond
    [(zero? (height df)) (raise-arguments-error who "the dataframe has no rows")]
    [absent
     (raise-arguments-error who "the dataframe has no column with this name"
                            "column" absent "columns of the dataframe" present)]
    [text
     (raise-arguments-error who "the dataframe has a column that is not numeric"
                            "column" text "dtype" (dtype (ref df text)))]
    [else (void)]))

;; The position and value of the first entry of ys, from a series of dtype d,
;; that is not finite, or #f.
(define (first-non-finite ys d)
  (and (float-dtype? d)
       (for/first ([y (in-list ys)] [k (in-naturals)] #:unless (< (abs y) +inf.0))
         (cons k y))))

;; --- polars -> glmnet ------------------------------------------------------------

(define (polars->design-matrix df columns)
  (dataframe->design-matrix 'polars->design-matrix df columns #:checked? #t))

(define (dataframe->design-matrix who df columns #:checked? [checked? #f])
  (define names (map column-name->string columns))
  (check-numeric-columns who df names checked?)
  (for ([name (in-list names)])
    (check-no-null who (ref df name) name))
  (define-values (v nrows ncols) (dataframe->f64vector df #:columns names #:null 'error))
  (flat->design-matrix v nrows ncols names who "the dataframe" #:adopt? #t))

(define (polars->response df column)
  (dataframe-column->response 'polars->response df column #:checked? #t))

(define (dataframe-column->response who df column #:checked? [checked? #f])
  (define name (column-name->string column))
  (check-numeric-columns who df (list name) checked?)
  (define s (ref df name))
  (check-no-null who s name)
  (define ys (series->list s))
  (define bad (first-non-finite ys (dtype s)))
  (cond
    [bad (element-error who "the dataframe" "not finite" (cdr bad) #:row (car bad) #:column name)]
    [else ys]))

;; The numbers of the series s, a response named `what` in errors, as a list.
(define (series->response who what s)
  (define d (dtype s))
  (define ys (and (numeric-dtype? d) (zero? (null-count s)) (series->list s)))
  (define bad (and ys (first-non-finite ys d)))
  (cond
    [(not (numeric-dtype? d))
     (raise-arguments-error who (format "~a is a series that is not numeric" what) "dtype" d)]
    [(not ys)
     (missing-error who what
                    #:position (for/first ([x (in-series s)] [k (in-naturals)] #:when (polars-null? x))
                                 k))]
    [bad (element-error who what "not finite" (cdr bad) #:position (car bad))]
    [else ys]))

;; The values of df's column `name`, a string, as a vector, as polars->table
;; reads them: class labels, for a binomial or multinomial response.
(define (dataframe-column-values who df name)
  (define s (ref df name))
  (unless (table-dtype? (dtype s))
    (raise-arguments-error who "the dataframe has a column whose values are not class labels"
                           "column" name "dtype" (dtype s)))
  (check-no-null who s name)
  (cdar (dataframe->columns df #:columns (list name))))

;; The values of the series s, a response named `what` in errors, as a vector.
(define (series-values who what s)
  (define d (dtype s))
  (unless (table-dtype? d)
    (raise-arguments-error who (format "~a is a series whose values are not class labels" what)
                           "dtype" d))
  (unless (zero? (null-count s))
    (missing-error who what
                   #:position (for/first ([x (in-series s)] [k (in-naturals)] #:when (polars-null? x))
                                k)))
  (for/vector ([x (in-series s)]) x))

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
