#lang racket/base

;; The input-data layer (#35): `design-matrix`, the one representation of a
;; predictor matrix that the Fortran solvers read, and the standalone
;; conversions into and out of it. `glmnet` re-exports this module; it does not
;; load the native library, so adapters can build on it alone. Tables (#26),
;; the named data that formulas read, convert into it here too.
;;
;; Invariant: `data` is a column-major f64vector of nrows * ncols finite
;; flonums, element (i, j) at index i + j*nrows. Every constructor validates
;; its input and copies it into a fresh vector, and no public binding hands out
;; that vector, so the invariant holds for the life of the value.

(require racket/contract
         racket/flonum
         racket/list
         ffi/vector)

(define column-names/c (or/c #f (listof (or/c string? symbol?))))
(define column-list/c (and/c (listof (or/c string? symbol?)) pair?))

(provide
 design-matrix?
 design-matrix/c
 (contract-out
  [rows->design-matrix
   (->* ((listof list?)) (#:column-names column-names/c) design-matrix?)]
  [columns->design-matrix
   (->* ((listof list?)) (#:column-names column-names/c) design-matrix?)]
  [f64vector->design-matrix
   (->* (f64vector? exact-positive-integer? exact-positive-integer?)
        (#:column-names column-names/c)
        design-matrix?)]
  [design-matrix-nrows (-> design-matrix? exact-positive-integer?)]
  [design-matrix-ncols (-> design-matrix? exact-positive-integer?)]
  [design-matrix-column-names (-> design-matrix? column-names/c)]
  [design-matrix-ref
   (-> design-matrix? exact-nonnegative-integer? exact-nonnegative-integer? flonum?)]
  [design-matrix-select-rows
   (-> design-matrix? (and/c (listof exact-nonnegative-integer?) pair?) design-matrix?)]
  [design-matrix->rows (-> design-matrix? (listof (listof flonum?)))]
  [design-matrix->columns (-> design-matrix? (listof (listof flonum?)))]
  [design-matrix->f64vector (-> design-matrix? f64vector?)]
  [response->f64vector (-> list? f64vector?)]
  [table? (-> any/c boolean?)]
  [table-column-names (-> table? (listof string?))]
  [table->design-matrix (->* (table?) (column-list/c) design-matrix?)]))

;; For the family modules only; not part of the public API.
(module* support #f
  (provide design-matrix-data
           design-matrix-nrows
           design-matrix-ncols
           as-design-matrix
           as-response
           column-name->string
           table-names
           select-table-columns))

(struct design-matrix (data nrows ncols column-names)
  #:property prop:custom-write
  (lambda (dm port mode)
    (fprintf port "#<design-matrix ~ax~a>"
             (design-matrix-nrows dm) (design-matrix-ncols dm)))
  #:property prop:equal+hash
  (list (lambda (a b recur)
          (and (= (design-matrix-nrows a) (design-matrix-nrows b))
               (= (design-matrix-ncols a) (design-matrix-ncols b))
               (recur (design-matrix-column-names a) (design-matrix-column-names b))
               (let ([u (design-matrix-data a)]
                     [v (design-matrix-data b)])
                 (for/and ([k (in-range (f64vector-length u))])
                   (eqv? (f64vector-ref u k) (f64vector-ref v k))))))
        (lambda (dm recur) (hash-shape dm recur))
        (lambda (dm recur) (hash-shape dm recur))))

(define (hash-shape dm recur)
  (recur (list (design-matrix-nrows dm)
               (design-matrix-ncols dm)
               (design-matrix-column-names dm))))

;; What the fitters and prediction helpers accept for a matrix argument: a
;; design-matrix, or a list of rows that `rows->design-matrix` converts.
(define design-matrix/c (or/c design-matrix? (listof list?)))

;; --- validation --------------------------------------------------------------

;; In errors, `who` names the public procedure and `what` its argument ("X",
;; "y", ...); `position` is the element's "row" and "column", or its "position".
(define (element-error who what problem x . position)
  (apply raise-arguments-error who (format "~a has an element that is ~a" what problem)
         (append position (list "element" x))))

(define (->finite-flonum x who what i j)
  (define v
    (if (real? x)
        (real->double-flonum x)
        (element-error who what "not a real number" x "row" i "column" j)))
  (unless (fl< (flabs v) +inf.0)
    (element-error who what "not finite" x "row" i "column" j))
  v)

(define (check-column-names names ncols who)
  (when names
    (unless (= (length names) ncols)
      (raise-arguments-error who "the number of column names does not match the columns"
                             "column names" (length names) "columns" ncols))
    (define dup (check-duplicates names))
    (when dup
      (raise-arguments-error who "the column names are not distinct" "duplicate" dup)))
  (and names
       (for/list ([name (in-list names)])
         (if (string? name) (string->immutable-string name) name))))

;; --- conversions in --------------------------------------------------------

(define (rows->dm rows names who what)
  (when (null? rows)
    (raise-arguments-error who (format "~a has no rows" what)))
  (define no (length rows))
  (define ni (length (car rows)))
  (when (zero? ni)
    (raise-arguments-error who (format "~a has no columns" what)))
  (define v (make-f64vector (* no ni)))
  (define (ragged i row)
    (raise-arguments-error who (format "~a has rows of different lengths" what)
                           "row" i "length" (length row) "length of row 0" ni))
  ;; One walk per row: element j of row i goes to i + j*no.
  (for ([row (in-list rows)]
        [i (in-naturals)])
    (let loop ([xs row] [j 0] [k i])
      (cond
        [(null? xs) (unless (= j ni) (ragged i row))]
        [(= j ni) (ragged i row)]
        [else
         (f64vector-set! v k (->finite-flonum (car xs) who what i j))
         (loop (cdr xs) (add1 j) (+ k no))])))
  (design-matrix v no ni (check-column-names names ni who)))

(define (columns->dm columns names who what)
  (when (null? columns)
    (raise-arguments-error who (format "~a has no columns" what)))
  (define ni (length columns))
  (define no (length (car columns)))
  (when (zero? no)
    (raise-arguments-error who (format "~a has no rows" what)))
  (define v (make-f64vector (* no ni)))
  (define (ragged j column)
    (raise-arguments-error who (format "~a has columns of different lengths" what)
                           "column" j "length" (length column) "length of column 0" no))
  ;; One walk per column: element i of column j goes to i + j*no.
  (for ([column (in-list columns)]
        [j (in-naturals)])
    (let loop ([xs column] [i 0])
      (cond
        [(null? xs) (unless (= i no) (ragged j column))]
        [(= i no) (ragged j column)]
        [else
         (f64vector-set! v (+ i (* j no)) (->finite-flonum (car xs) who what i j))
         (loop (cdr xs) (add1 i))])))
  (design-matrix v no ni (check-column-names names ni who)))

(define (rows->design-matrix rows #:column-names [names #f])
  (rows->dm rows names 'rows->design-matrix "the matrix"))

(define (columns->design-matrix columns #:column-names [names #f])
  (columns->dm columns names 'columns->design-matrix "the matrix"))

(define (f64vector->design-matrix v nrows ncols #:column-names [names #f])
  (define who 'f64vector->design-matrix)
  (define n (f64vector-length v))
  (unless (= n (* nrows ncols))
    (raise-arguments-error who "the vector length is not nrows * ncols"
                           "length" n "nrows" nrows "ncols" ncols))
  (define out (make-f64vector n))
  (for ([k (in-range n)])
    (f64vector-set! out k (->finite-flonum (f64vector-ref v k) who "the vector"
                                           (remainder k nrows) (quotient k nrows))))
  (design-matrix out nrows ncols (check-column-names names ncols who)))

;; --- conversions out -------------------------------------------------------

(define (design-matrix-ref dm i j)
  (define no (design-matrix-nrows dm))
  (define ni (design-matrix-ncols dm))
  (unless (< i no)
    (raise-range-error 'design-matrix-ref "design matrix" "row " i dm 0 (sub1 no)))
  (unless (< j ni)
    (raise-range-error 'design-matrix-ref "design matrix" "column " j dm 0 (sub1 ni)))
  (f64vector-ref (design-matrix-data dm) (+ i (* j no))))

;; The rows of dm at the given indices, in that order, as a new design matrix
;; with the same column names. The entries are already valid, so they are
;; copied without being checked again.
(define (design-matrix-select-rows dm rows)
  (define v (design-matrix-data dm))
  (define no (design-matrix-nrows dm))
  (define ni (design-matrix-ncols dm))
  (for ([i (in-list rows)])
    (unless (< i no)
      (raise-range-error 'design-matrix-select-rows "design matrix" "row " i dm 0 (sub1 no))))
  (define m (length rows))
  (define out (make-f64vector (* m ni)))
  (for ([j (in-range ni)])
    (for ([i (in-list rows)]
          [k (in-naturals)])
      (f64vector-set! out (+ k (* j m)) (f64vector-ref v (+ i (* j no))))))
  (design-matrix out m ni (design-matrix-column-names dm)))

(define (design-matrix->rows dm)
  (define v (design-matrix-data dm))
  (define no (design-matrix-nrows dm))
  (for/list ([i (in-range no)])
    (for/list ([j (in-range (design-matrix-ncols dm))])
      (f64vector-ref v (+ i (* j no))))))

(define (design-matrix->columns dm)
  (define v (design-matrix-data dm))
  (define no (design-matrix-nrows dm))
  (for/list ([j (in-range (design-matrix-ncols dm))])
    (for/list ([i (in-range no)])
      (f64vector-ref v (+ i (* j no))))))

(define (design-matrix->f64vector dm)
  (define v (design-matrix-data dm))
  (define n (f64vector-length v))
  (define out (make-f64vector n))
  (for ([k (in-range n)])
    (f64vector-set! out k (f64vector-ref v k)))
  out)

;; --- responses -------------------------------------------------------------

(define (list->response y who what)
  (when (null? y)
    (raise-arguments-error who (format "~a is empty" what)))
  (define v (make-f64vector (length y)))
  (for ([x (in-list y)]
        [k (in-naturals)])
    (define fx
      (if (real? x)
          (real->double-flonum x)
          (element-error who what "not a real number" x "position" k)))
    (unless (fl< (flabs fx) +inf.0)
      (element-error who what "not finite" x "position" k))
    (f64vector-set! v k fx))
  v)

(define (response->f64vector y)
  (list->response y 'response->f64vector "the response"))

;; --- the fitters' entry points (support submodule) ---------------------------

;; A fitter's matrix argument (named `what` in errors) as a design-matrix.
(define (as-design-matrix X who what)
  (if (design-matrix? X)
      X
      (rows->dm X #f who what)))

;; A fitter's response argument as an f64vector with one entry per observation.
(define (as-response y no who what)
  (unless (= (length y) no)
    (raise-arguments-error who (format "~a does not have one entry per row of X" what)
                           (format "length of ~a" what) (length y) "rows of X" no))
  (list->response y who what))

;; --- tables (#26) ------------------------------------------------------------

;; A table supplies named columns: a design matrix with column names, a hash
;; from name to column, or an association list of (name . column) pairs, where
;; a name is a string or a symbol and a column a list or vector. Names are
;; compared as strings. Only the columns a caller selects are checked for
;; numbers, so a table can carry columns that no model reads.
(define (table? v)
  (cond
    [(design-matrix? v) (and (design-matrix-column-names v) #t)]
    [(hash? v)
     (and (positive? (hash-count v))
          (for/and ([(name column) (in-hash v)])
            (and (column-name? name) (column? column))))]
    [(pair? v)
     (and (list? v)
          (for/and ([entry (in-list v)])
            (and (pair? entry) (column-name? (car entry)) (column? (cdr entry)))))]
    [else #f]))

(define (column-name? v) (or (string? v) (symbol? v)))
(define (column? v) (or (list? v) (vector? v)))

(define (column-name->string name)
  (string->immutable-string (if (symbol? name) (symbol->string name) name)))

;; The table's column names as strings, in its order; a hash has none, so its
;; names are sorted.
(define (table-names t who)
  (define names
    (cond
      [(design-matrix? t) (map column-name->string (design-matrix-column-names t))]
      [(hash? t) (sort (map column-name->string (hash-keys t)) string<?)]
      [else (for/list ([entry (in-list t)]) (column-name->string (car entry)))]))
  (define dup (check-duplicates names))
  (when dup
    (raise-arguments-error who "the table has two columns with the same name" "name" dup))
  names)

(define (table-column-names t)
  (table-names t 'table-column-names))

;; The columns of table t with the given names, in that order, as a design
;; matrix with those column names.
(define (select-table-columns t names* who)
  (define names (map column-name->string names*))
  (define available (table-names t who))
  (define position
    (for/hash ([name (in-list available)] [j (in-naturals)])
      (values name j)))
  (for ([name (in-list names)])
    (unless (hash-ref position name #f)
      (raise-arguments-error who "the table has no column with this name"
                             "column" name "columns of the table" available)))
  (cond
    [(design-matrix? t)
     (define v (design-matrix-data t))
     (define no (design-matrix-nrows t))
     (define ni (length names))
     (define out (make-f64vector (* no ni)))
     (for ([name (in-list names)]
           [j (in-naturals)])
       (define from (* no (hash-ref position name)))
       (for ([i (in-range no)])
         (f64vector-set! out (+ i (* j no)) (f64vector-ref v (+ from i)))))
     (design-matrix out no ni names)]
    [else
     (define by-name
       (if (hash? t)
           (for/hash ([(name column) (in-hash t)])
             (values (column-name->string name) column))
           (for/hash ([entry (in-list t)])
             (values (column-name->string (car entry)) (cdr entry)))))
     (named-columns->dm (for/list ([name (in-list names)]) (hash-ref by-name name))
                        names who)]))

;; Columns (lists or vectors) with the given names as a design matrix; errors
;; name the column by its name.
(define (named-columns->dm columns names who)
  (define no (column-length (car columns)))
  (when (zero? no)
    (raise-arguments-error who "the table has a column with no rows" "column" (car names)))
  (define ni (length columns))
  (define v (make-f64vector (* no ni)))
  (for ([column (in-list columns)]
        [name (in-list names)]
        [j (in-naturals)])
    (unless (= (column-length column) no)
      (raise-arguments-error who "the table's columns have different lengths"
                             "column" name "length" (column-length column)
                             (format "length of column ~s" (car names)) no))
    (define (store! x i)
      (define fx
        (if (real? x)
            (real->double-flonum x)
            (raise-arguments-error who "the table has an element that is not a real number"
                                   "column" name "row" i "element" x)))
      (unless (fl< (flabs fx) +inf.0)
        (raise-arguments-error who "the table has an element that is not finite"
                               "column" name "row" i "element" x))
      (f64vector-set! v (+ i (* j no)) fx))
    (if (vector? column)
        (for ([x (in-vector column)] [i (in-naturals)]) (store! x i))
        (for ([x (in-list column)] [i (in-naturals)]) (store! x i))))
  (design-matrix v no ni names))

(define (column-length column)
  (if (vector? column) (vector-length column) (length column)))

(define (table->design-matrix t [names (table-names t 'table->design-matrix)])
  (define dup (check-duplicates (map column-name->string names)))
  (when dup
    (raise-arguments-error 'table->design-matrix "a column is named twice" "name" dup))
  (select-table-columns t names 'table->design-matrix))
