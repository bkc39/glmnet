#lang racket/base

;; The tabular-asa adapter (#63). A tabular-asa table is a row index, a vector
;; of positions, and an association list from each column's name to a vector
;; that the index selects from and orders; #f is its missing value.

(require racket/contract
         racket/flonum
         racket/list
         ffi/vector
         (prefix-in asa: tabular-asa)
         "../data.rkt"
         (only-in (submod "../data.rkt" support)
                  design-matrix-data column-name->string table-names select-table-values
                  flvectors->design-matrix))

(provide
 (contract-out
  [tabular-asa->design-matrix
   (->i ([df non-empty-table/c]
         [columns (df) (column-list-of/c (tabular-asa-names df))])
        [result design-matrix?])]
  [tabular-asa->response
   (->i ([df non-empty-table/c]
         [column (df) (column-of/c (tabular-asa-names df))])
        [result (and/c (listof real?) pair?)])]
  [tabular-asa->table
   (->i ([df non-empty-table/c])
        ([columns (df) (column-list-of/c (tabular-asa-names df))])
        [result table?])]
  [design-matrix->tabular-asa
   (->i ([dm design-matrix?])
        (#:column-names [names (dm) (column-names-for/c (design-matrix-ncols dm))])
        [result asa:table?])]
  [table->tabular-asa
   (->i ([t table?])
        ([columns (t) (column-list-of/c (table-names t 'table->tabular-asa))])
        [result asa:table?])]))

;; --- contracts -----------------------------------------------------------------

;; A flat contract's failure on v: `problem` is the message's first line, and
;; `args` fill the directives of `problem` and then of `expected`.
(define ((explain v problem expected . args) blame)
  (apply raise-blame-error blame v (list problem 'expected: expected 'given: "~e")
         (append args (list v))))

(define non-empty-table/c
  (flat-contract-with-explanation
   (lambda (df)
     (cond
       [(not (asa:table? df)) (explain df "not a tabular-asa table" "table?")]
       [(null? (asa:table-data df)) (explain df "the table has no columns" "a table with columns")]
       [(zero? (asa:table-length df)) (explain df "the table has no rows" "a table with rows")]
       [else #t]))
   #:name '(and/c table? (not/c table-empty?))))

(define (name? v) (or (string? v) (symbol? v)))

;; `name` as one of `names`, a table's column names as strings; #t, or the
;; failure of `v`, the argument that holds it.
(define (check-name v name names)
  (cond
    [(not (name? name))
     (explain v "a column name must be a string or a symbol, not ~e" "(or/c string? symbol?)" name)]
    [(member (column-name->string name) names) #t]
    [else (explain v "the table has no column named ~e" "one of ~e" name names)]))

(define (column-of/c names)
  (flat-contract-with-explanation
   (lambda (name) (check-name name name names))
   #:name '(or/c string? symbol?)))

(define (column-list-of/c names)
  (flat-contract-with-explanation
   (lambda (columns)
     (cond
       [(not (and (list? columns) (pair? columns)))
        (explain columns "the columns must be a non-empty list of names"
                 "(and/c (listof (or/c string? symbol?)) pair?)")]
       [(for*/first ([name (in-list columns)]
                     [checked (in-value (check-name columns name names))]
                     #:unless (eq? checked #t))
          checked)]
       [(check-duplicates columns #:key column-name->string)
        => (lambda (name) (explain columns "the column ~e is named twice" "distinct names" name))]
       [else #t]))
   #:name '(and/c (listof (or/c string? symbol?)) pair?)))

(define (column-names-for/c ncols)
  (flat-contract-with-explanation
   (lambda (names)
     (cond
       [(not (and (list? names) (andmap name? names)))
        (explain names "the column names must be a list of strings or symbols"
                 "(listof (or/c string? symbol?))")]
       [(not (= (length names) ncols))
        (explain names "the number of column names does not match the columns"
                 "~a names" ncols)]
       [(check-duplicates names #:key column-name->string)
        => (lambda (name) (explain names "the column name ~e is repeated" "distinct names" name))]
       [else #t]))
   #:name '(listof (or/c string? symbol?))))

;; --- reading tabular-asa tables --------------------------------------------------

;; tabular-asa documents its column names as symbols but does not enforce it.
(define (table-name->string name)
  (if (name? name) (column-name->string name) (format "~a" name)))

(define (tabular-asa-names df)
  (for/list ([entry (in-list (asa:table-data df))])
    (table-name->string (car entry))))

;; The data vectors of the columns of df named `names` (strings), in that
;; order.
(define (column-data df names who)
  (define by-name
    (for/fold ([by-name (hash)]) ([entry (in-list (asa:table-data df))])
      (define name (table-name->string (car entry)))
      (when (hash-ref by-name name #f)
        (raise-arguments-error who "the table has two columns with the same name" "name" name))
      (hash-set by-name name (cdr entry))))
  (for/list ([name (in-list names)]) (hash-ref by-name name)))

(define (missing-error who name i)
  (raise-arguments-error who "the table has a missing value"
                         "column" name "row" i "element" #f))

(define (element->flonum x name i who)
  (define v
    (cond
      [(flonum? x) x]
      [(real? x) (real->double-flonum x)]
      [(not x) (missing-error who name i)]
      [else (raise-arguments-error who "the table has an element that is not a real number"
                                   "column" name "row" i "element" x)]))
  (unless (fl< (flabs v) +inf.0)
    (raise-arguments-error who "the table has an element that is not finite"
                           "column" name "row" i "element" x))
  v)

(define (tabular-asa->design-matrix df columns)
  (define who 'tabular-asa->design-matrix)
  (define names (map column-name->string columns))
  (define index (asa:table-index df))
  (flvectors->design-matrix
   (for/list ([data (in-list (column-data df names who))]
              [name (in-list names)])
     (for/flvector #:length (vector-length index) ([p (in-vector index)] [i (in-naturals)])
       (element->flonum (vector-ref data p) name i who)))
   names
   who))

(define (tabular-asa->response df column)
  (define who 'tabular-asa->response)
  (define name (column-name->string column))
  (define data (car (column-data df (list name) who)))
  (for/list ([p (in-vector (asa:table-index df))]
             [i (in-naturals)])
    (define x (vector-ref data p))
    (element->flonum x name i who)
    x))

(define (tabular-asa->table df [columns (asa:table-header df)])
  (define who 'tabular-asa->table)
  (define names (map table-name->string columns))
  (define index (asa:table-index df))
  (for/list ([data (in-list (column-data df names who))]
             [name (in-list names)])
    (cons name
          (for/vector #:length (vector-length index) ([p (in-vector index)] [i (in-naturals)])
            (or (vector-ref data p) (missing-error who name i))))))

;; --- writing tabular-asa tables ----------------------------------------------------

(define (name->symbol name)
  (if (symbol? name) name (string->symbol name)))

(define (default-column-names dm)
  (or (design-matrix-column-names dm)
      (for/list ([j (in-range (design-matrix-ncols dm))])
        (format "V~a" (add1 j)))))

(define (design-matrix->tabular-asa dm #:column-names [names (default-column-names dm)])
  (define v (design-matrix-data dm))
  (define no (design-matrix-nrows dm))
  (asa:table (build-vector no values)
             (for/list ([name (in-list names)]
                        [j (in-naturals)])
               (define start (* j no))
               (cons (name->symbol name)
                     (for/vector #:length no ([i (in-range no)])
                       (f64vector-ref v (+ start i)))))))

(define (table->tabular-asa t [columns (table-names t 'table->tabular-asa)])
  (define who 'table->tabular-asa)
  (define names (map column-name->string columns))
  (cond
    [(design-matrix? t)
     (design-matrix->tabular-asa (table->design-matrix t names))]
    [else
     (define selected (select-table-values t names who))
     (asa:table (build-vector (vector-length (first selected)) values)
                (for/list ([column (in-list selected)]
                           [name (in-list names)])
                  (cons (string->symbol name)
                        (for/vector #:length (vector-length column) ([x (in-vector column)]
                                                                     [i (in-naturals)])
                          (or x (raise-arguments-error
                                 who "the table has an element that is #f, which tabular-asa reads as a missing value"
                                 "column" name "row" i))))))]))
