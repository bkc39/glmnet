#lang racket/base

;; The input-data layer (#35): `design-matrix`, the one representation of a
;; predictor matrix that the Fortran solvers read, and the standalone
;; conversions into and out of it. `glmnet` re-exports this module; it does not
;; load the native library, so adapters can build on it alone.
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
  [design-matrix->rows (-> design-matrix? (listof (listof flonum?)))]
  [design-matrix->columns (-> design-matrix? (listof (listof flonum?)))]
  [design-matrix->f64vector (-> design-matrix? f64vector?)]
  [response->f64vector (-> list? f64vector?)]))

;; For the family modules only; not part of the public API.
(module* support #f
  (provide design-matrix-data
           design-matrix-nrows
           design-matrix-ncols
           as-design-matrix
           as-response))

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
