#lang racket/base

;; The input-data layer (#35): `design-matrix`, the one representation of a
;; predictor matrix that the Fortran solvers read, and the standalone
;; conversions into and out of it. `glmnet` re-exports this module; it does not
;; load the native library, so adapters can build on it alone. Tables (#26),
;; the named data that formulas read, convert into it here too. The fitters
;; accept rows in any nesting of lists and vectors, and responses as lists,
;; vectors, flvectors or f64vectors (#36), so those conversions live here, and
;; glmnet/data/nested, which builds on this module, exports them by name.
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
  [response/c (-> flat-contract? flat-contract?)]
  [rows->design-matrix
   (->* ((listof list?)) (#:column-names column-names/c) design-matrix?)]
  [columns->design-matrix
   (->* ((listof list?)) (#:column-names column-names/c) design-matrix?)]
  [f64vector->design-matrix
   (->i ([v (nrows ncols) (f64vector-length/c nrows ncols)]
         [nrows exact-positive-integer?]
         [ncols exact-positive-integer?])
        (#:column-names [column-names column-names/c])
        [result design-matrix?])]
  [design-matrix-nrows (-> design-matrix? exact-positive-integer?)]
  [design-matrix-ncols (-> design-matrix? exact-positive-integer?)]
  [design-matrix-column-names (-> design-matrix? column-names/c)]
  [design-matrix-ref
   (->i ([dm design-matrix?]
         [i (dm) (integer-in 0 (sub1 (design-matrix-nrows dm)))]
         [j (dm) (integer-in 0 (sub1 (design-matrix-ncols dm)))])
        [result flonum?])]
  [design-matrix-select-rows
   (-> design-matrix? (and/c (listof exact-nonnegative-integer?) pair?) design-matrix?)]
  [design-matrix->rows (-> design-matrix? (listof (listof flonum?)))]
  [design-matrix->columns (-> design-matrix? (listof (listof flonum?)))]
  [design-matrix->f64vector (-> design-matrix? f64vector?)]
  [response->f64vector (-> (or/c list? vector? flvector? f64vector?) f64vector?)]
  [table? (-> any/c boolean?)]
  [table-column-names (-> table? (listof string?))]
  [table->design-matrix (->* (table?) (column-list/c) design-matrix?)]))

;; For the family modules only; not part of the public API.
(module* support #f
  (provide design-matrix-data
           design-matrix-nrows
           design-matrix-ncols
           nested-matrix?
           nested->dm
           as-design-matrix
           as-response
           column-name->string
           table-names
           select-table-columns
           select-table-values
           table-column->flvector
           flvectors->design-matrix))

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

(define (nested-matrix? v)
  (define (line? xs) (or (list? xs) (vector? xs)))
  (cond
    [(list? v) (for/and ([xs (in-list v)]) (line? xs))]
    [(vector? v) (for/and ([xs (in-vector v)]) (line? xs))]
    [else #f]))

;; What the fitters and prediction helpers accept for a matrix argument: a
;; design-matrix, or rows in any of the four nestings. A flat contract, unlike
;; vectorof, so that a vector argument reaches the conversion unwrapped.
(define design-matrix/c
  (flat-named-contract
   '(or/c design-matrix? (listof (or/c list? vector?)) (vectorof (or/c list? vector?)))
   (lambda (v) (or (design-matrix? v) (nested-matrix? v)))))

;; A one-dimensional input with at least one element, each satisfying `elem`.
;; An element that fails is blamed with its position.
(define (response/c elem)
  (define elem/c (coerce-flat-contract 'response/c elem))
  (define elem? (flat-contract-predicate elem/c))
  (define (failure k x) (and (not (elem? x)) (cons k x)))
  (define (first-failure v)
    (cond
      [(list? v) (for/or ([x (in-list v)] [k (in-naturals)]) (failure k x))]
      [(vector? v) (for/or ([x (in-vector v)] [k (in-naturals)]) (failure k x))]
      [(flvector? v) (for/or ([x (in-flvector v)] [k (in-naturals)]) (failure k x))]
      [else (for/or ([k (in-range (f64vector-length v))]) (failure k (f64vector-ref v k)))]))
  (define (shape? v)
    (and (one-dimensional? v) (positive? (one-dimensional-length v))))
  (make-flat-contract
   #:name (build-compound-type-name 'response/c elem/c)
   #:first-order (lambda (v) (and (shape? v) (not (first-failure v))))
   #:late-neg-projection
   (lambda (blame)
     (lambda (v neg-party)
       (cond
         [(not (shape? v))
          (raise-blame-error blame #:missing-party neg-party v
                             '(expected: "a non-empty list, vector, flvector or f64vector of ~a"
                               given: "~e")
                             (contract-name elem/c) v)]
         [(first-failure v)
          => (lambda (k+x)
               (define elem-blame
                 (blame-add-context blame (format "the element at position ~a of" (car k+x))))
               (((get/build-late-neg-projection elem/c) elem-blame) (cdr k+x) neg-party)
               v)]
         [else v])))))

(define (one-dimensional? v)
  (or (list? v) (vector? v) (flvector? v) (f64vector? v)))

(define (one-dimensional-length v)
  (cond
    [(list? v) (length v)]
    [(vector? v) (vector-length v)]
    [(flvector? v) (flvector-length v)]
    [else (f64vector-length v)]))

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

;; An f64vector with nrows * ncols entries: the v argument of
;; f64vector->design-matrix.
(define (f64vector-length/c nrows ncols)
  (define n (* nrows ncols))
  (flat-contract-with-explanation
   (lambda (v)
     (or (and (f64vector? v) (= (f64vector-length v) n))
         (lambda (blame)
           (raise-blame-error blame v
                              '(expected: "an f64vector of length nrows * ncols = ~a" given: "~a")
                              n
                              (if (f64vector? v)
                                  (format "an f64vector of length ~a" (f64vector-length v))
                                  (format "~e" v))))))
   #:name `(f64vector-length/c ,nrows ,ncols)))

(define (check-column-names names ncols who)
  (when names
    (unless (= (length names) ncols)
      (raise-arguments-error who "the number of column names does not match the columns"
                             "column names" (length names) "columns" ncols))
    (define dup (check-duplicates names #:key column-name->string))
    (when dup
      (raise-arguments-error who "the column names are not distinct"
                             "duplicate" dup "column names" names)))
  (and names
       (for/list ([name (in-list names)])
         (if (string? name) (string->immutable-string name) name))))

;; --- conversions in --------------------------------------------------------

;; Rows (by = 'rows) or columns (by = 'columns) in any of the four nestings as
;; a design matrix. The outer sequence runs over the rows or columns and each
;; inner one along it; entry p of outer line o goes to o*outer-stride +
;; p*inner-stride of the column-major array.
(define (nested->dm xss by names who what)
  (define rows? (eq? by 'rows))
  (define-values (line other) (if rows? (values "row" "column") (values "column" "row")))
  (define n-outer (if (vector? xss) (vector-length xss) (length xss)))
  (when (zero? n-outer)
    (raise-arguments-error who (format "~a has no ~as" what line)))
  (define n-inner (line-length (if (vector? xss) (vector-ref xss 0) (car xss))))
  (when (zero? n-inner)
    (raise-arguments-error who (format "~a has no ~as" what other)))
  (define-values (no ni) (if rows? (values n-outer n-inner) (values n-inner n-outer)))
  (define-values (outer-stride inner-stride) (if rows? (values 1 no) (values no 1)))
  (define v (make-f64vector (* n-outer n-inner)))
  (define (ragged o xs)
    (raise-arguments-error who (format "~a has ~as of different lengths" what line)
                           line o "length" (line-length xs)
                           (format "length of ~a 0" line) n-inner))
  (define (entry x o p)
    (cond
      [(and (flonum? x) (fl< (flabs x) +inf.0)) x]
      [rows? (->finite-flonum x who what o p)]
      [else (->finite-flonum x who what p o)]))
  (define (store! xs o)
    (define start (* o outer-stride))
    (cond
      [(vector? xs)
       (unless (= (vector-length xs) n-inner) (ragged o xs))
       (for ([x (in-vector xs)]
             [p (in-naturals)])
         (f64vector-set! v (+ start (* p inner-stride)) (entry x o p)))]
      [else
       (let loop ([ys xs] [p 0] [k start])
         (cond
           [(null? ys) (unless (= p n-inner) (ragged o xs))]
           [(= p n-inner) (ragged o xs)]
           [else
            (f64vector-set! v k (entry (car ys) o p))
            (loop (cdr ys) (add1 p) (+ k inner-stride))]))]))
  (if (vector? xss)
      (for ([xs (in-vector xss)] [o (in-naturals)]) (store! xs o))
      (for ([xs (in-list xss)] [o (in-naturals)]) (store! xs o)))
  (design-matrix v no ni (check-column-names names ni who)))

(define (line-length xs)
  (if (vector? xs) (vector-length xs) (length xs)))

(define (rows->design-matrix rows #:column-names [names #f])
  (nested->dm rows 'rows names 'rows->design-matrix "the matrix"))

(define (columns->design-matrix columns #:column-names [names #f])
  (nested->dm columns 'columns names 'columns->design-matrix "the matrix"))

(define (f64vector->design-matrix v nrows ncols #:column-names [names #f])
  (define who 'f64vector->design-matrix)
  (define n (f64vector-length v))
  (define out (make-f64vector n))
  (for ([k (in-range n)])
    (f64vector-set! out k (->finite-flonum (f64vector-ref v k) who "the vector"
                                           (remainder k nrows) (quotient k nrows))))
  (design-matrix out nrows ncols (check-column-names names ncols who)))

;; --- conversions out -------------------------------------------------------

(define (design-matrix-ref dm i j)
  (f64vector-ref (design-matrix-data dm) (+ i (* j (design-matrix-nrows dm)))))

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

;; A list, vector, flvector or f64vector of reals as a fresh f64vector.
(define (->response y who what)
  (define n (one-dimensional-length y))
  (when (zero? n)
    (raise-arguments-error who (format "~a is empty" what)))
  (define v (make-f64vector n))
  (define (store-flonum! fx x k)
    (unless (fl< (flabs fx) +inf.0)
      (element-error who what "not finite" x "position" k))
    (f64vector-set! v k fx))
  (define (store! x k)
    (cond
      [(flonum? x) (store-flonum! x x k)]
      [(real? x) (store-flonum! (real->double-flonum x) x k)]
      [else (element-error who what "not a real number" x "position" k)]))
  (cond
    [(list? y) (for ([x (in-list y)] [k (in-naturals)]) (store! x k))]
    [(vector? y) (for ([x (in-vector y)] [k (in-naturals)]) (store! x k))]
    [(flvector? y) (for ([x (in-flvector y)] [k (in-naturals)]) (store-flonum! x x k))]
    [else (for ([k (in-range n)])
            (define x (f64vector-ref y k))
            (store-flonum! x x k))])
  v)

(define (response->f64vector y)
  (->response y 'response->f64vector "the response"))

;; --- the fitters' entry points (support submodule) ---------------------------

;; A fitter's matrix argument (named `what` in errors) as a design-matrix.
(define (as-design-matrix X who what)
  (if (design-matrix? X)
      X
      (nested->dm X 'rows #f who what)))

;; A fitter's response argument as an f64vector with one entry per observation.
(define (as-response y no who what)
  (define n (one-dimensional-length y))
  (unless (= n no)
    (raise-arguments-error who (format "~a does not have one entry per row of X" what)
                           (format "length of ~a" what) n "rows of X" no))
  (->response y who what))

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

;; The position of each of the table's columns by name, after checking that
;; each of `names` (strings) is one of them.
(define (column-positions t names who)
  (define available (table-names t who))
  (define position
    (for/hash ([name (in-list available)] [j (in-naturals)])
      (values name j)))
  (for ([name (in-list names)])
    (unless (hash-ref position name #f)
      (raise-arguments-error who "the table has no column with this name"
                             "column" name "columns of the table" available)))
  position)

;; The columns of table t with the given names, in that order, as a design
;; matrix with those column names.
(define (select-table-columns t names* who)
  (define names (map column-name->string names*))
  (define position (column-positions t names who))
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
     (named-columns->dm (named-columns t names) names who)]))

;; The values of the columns of table t with the given names, in that order,
;; each a vector, as they are in the table: the formula front end (#53) reads
;; factors, whose values are strings, symbols or booleans, this way.
(define (select-table-values t names* who)
  (define names (map column-name->string names*))
  (define position (column-positions t names who))
  (cond
    [(design-matrix? t)
     (define v (design-matrix-data t))
     (define no (design-matrix-nrows t))
     (for/list ([name (in-list names)])
       (define from (* no (hash-ref position name)))
       (for/vector #:length no ([i (in-range no)])
         (f64vector-ref v (+ from i))))]
    [else
     (define columns (named-columns t names))
     (check-column-lengths columns names who)
     (for/list ([column (in-list columns)])
       (if (vector? column) column (list->vector column)))]))

;; The columns (lists or vectors) of a hash or association list with the given
;; names, which it has.
(define (named-columns t names)
  (define by-name
    (if (hash? t)
        (for/hash ([(name column) (in-hash t)])
          (values (column-name->string name) column))
        (for/hash ([entry (in-list t)])
          (values (column-name->string (car entry)) (cdr entry)))))
  (for/list ([name (in-list names)]) (hash-ref by-name name)))

(define (check-column-lengths columns names who)
  (define no (column-length (car columns)))
  (when (zero? no)
    (raise-arguments-error who "the table has a column with no rows" "column" (car names)))
  (for ([column (in-list columns)]
        [name (in-list names)])
    (unless (= (column-length column) no)
      (raise-arguments-error who "the table's columns have different lengths"
                             "column" name "length" (column-length column)
                             (format "length of column ~s" (car names)) no))))

;; Element i of the table's column `name` as a flonum; it must be a finite real.
(define (table-element->flonum x name i who)
  (define fx
    (if (real? x)
        (real->double-flonum x)
        (raise-arguments-error who "the table has an element that is not a real number"
                               "column" name "row" i "element" x)))
  (unless (fl< (flabs fx) +inf.0)
    (raise-arguments-error who "the table has an element that is not finite"
                           "column" name "row" i "element" x))
  fx)

;; The values of the table's column `name`, a vector from select-table-values,
;; as an flvector.
(define (table-column->flvector entries name who)
  (for/flvector #:length (vector-length entries) ([x (in-vector entries)] [i (in-naturals)])
    (table-element->flonum x name i who)))

;; Columns (lists or vectors) with the given names as a design matrix; errors
;; name the column by its name.
(define (named-columns->dm columns names who)
  (check-column-lengths columns names who)
  (define no (column-length (car columns)))
  (define ni (length columns))
  (define v (make-f64vector (* no ni)))
  (for ([column (in-list columns)]
        [name (in-list names)]
        [j (in-naturals)])
    (define (store! x i)
      (f64vector-set! v (+ i (* j no)) (table-element->flonum x name i who)))
    (if (vector? column)
        (for ([x (in-vector column)] [i (in-naturals)]) (store! x i))
        (for ([x (in-list column)] [i (in-naturals)]) (store! x i))))
  (design-matrix v no ni names))

(define (column-length column)
  (if (vector? column) (vector-length column) (length column)))

;; Columns computed from a table, such as the interactions of a formula (#53),
;; as a design matrix with the given names. Each column is an flvector of the
;; same length; an entry that is not finite, from an overflowing product, is an
;; error that names its column and row.
(define (flvectors->design-matrix columns names who)
  (define no (flvector-length (car columns)))
  (define ni (length columns))
  (define v (make-f64vector (* no ni)))
  (for ([column (in-list columns)]
        [name (in-list names)]
        [j (in-naturals)])
    (for ([x (in-flvector column)]
          [i (in-naturals)])
      (unless (fl< (flabs x) +inf.0)
        (raise-arguments-error who "a column of the design matrix has an element that is not finite"
                               "column" name "row" i "element" x))
      (f64vector-set! v (+ i (* j no)) x)))
  (design-matrix v no ni (check-column-names names ni who)))

(define (table->design-matrix t [names (table-names t 'table->design-matrix)])
  (define dup (check-duplicates (map column-name->string names)))
  (when dup
    (raise-arguments-error 'table->design-matrix "a column is named twice" "name" dup))
  (select-table-columns t names 'table->design-matrix))
