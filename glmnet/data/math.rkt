#lang racket/base

(require racket/contract
         (only-in racket/flonum flvector? flvector-length make-flvector flvector-set!)
         (only-in ffi/vector f64vector-ref)
         (only-in math/array
                  array? array-shape mutable-array? mutable-array-data flarray-data)
         (only-in math/matrix matrix? matrix-shape row-matrix? col-matrix?)
         ;; math/array exports flarray-data but no predicate for flonum arrays.
         (only-in (submod math/private/array/flarray-struct defs) flarray?)
         (only-in "../data.rkt" design-matrix?)
         (only-in (submod "../data.rkt" support)
                  design-matrix-data design-matrix-nrows design-matrix-ncols
                  check-column-names flat->design-matrix))

(module typed typed/racket/base
  (require racket/flonum
           typed/racket/unsafe
           math/array)

  (unsafe-require/typed ffi/vector
                        [#:opaque F64Vector f64vector?]
                        [make-f64vector (-> Integer F64Vector)]
                        [f64vector-set! (-> F64Vector Integer Flonum Void)])
  (unsafe-require/typed (submod "../data.rkt" support)
                        [->finite-flonum (-> Any Symbol String Integer (U String Integer) Flonum)]
                        [element-error (-> Symbol String String Any #:position Integer Nothing)])

  (provide matrix->f64vector
           array->real-list
           row-major->flarray)

  (define-type Storage (U #f FlVector VectorTop))

  ;; Element (i, j) of a matrix, or element j of a one-dimensional array (i is
  ;; then 0), where n is the number of columns. A lazy array's index
  ;; transforms write to the index vector they are given, so it is mutable
  ;; unless the array is strict.
  (: element-reader (-> (Array Any) Storage Index (-> Integer Integer Any)))
  (define (element-reader a storage n)
    (cond
      [(flvector? storage) (lambda (i j) (flvector-ref storage (+ (* i n) j)))]
      [(vector? storage) (lambda (i j) (vector-ref storage (+ (* i n) j)))]
      [else
       (define proc (unsafe-array-proc a))
       (define one-dimensional? (= (array-dims a) 1))
       (define (index [k : Integer]) (assert k index?))
       (cond
         [(and one-dimensional? (array-strict? a))
          (lambda (i j) (proc (vector-immutable (index j))))]
         [one-dimensional? (lambda (i j) (proc (vector (index j))))]
         [(array-strict? a) (lambda (i j) (proc (vector-immutable (index i) (index j))))]
         [else (lambda (i j) (proc (vector (index i) (index j))))])]))

  ;; The shape of a matrix, or of a one-dimensional array read as one row.
  (: rows+columns (-> (Array Any) (Values Index Index)))
  (define (rows+columns a)
    (define ds (array-shape a))
    (if (= (vector-length ds) 1)
        (values 1 (vector-ref ds 0))
        (values (vector-ref ds 0) (vector-ref ds 1))))

  (: matrix->f64vector (-> Any Storage (U #f (Listof String)) Symbol F64Vector))
  (define (matrix->f64vector M storage names who)
    (define a (assert M array?))
    (define-values (m n) (rows+columns a))
    (define ref (element-reader a storage n))
    (define labels (and names (list->vector names)))
    (define out (make-f64vector (* m n)))
    (for* ([i (in-range m)]
           [j (in-range n)])
      (define x (ref i j))
      (f64vector-set! out (+ i (* j m))
                      (if (and (flonum? x) (fl< (flabs x) +inf.0))
                          x
                          (->finite-flonum x who "the matrix" i
                                           (if labels (vector-ref labels j) j)))))
    out)

  (: array->real-list (-> Any Storage Symbol (Listof Real)))
  (define (array->real-list A storage who)
    (define a (assert A array?))
    (define-values (m n) (rows+columns a))
    (define ref (element-reader a storage n))
    (for*/list : (Listof Real) ([i (in-range m)]
                                [j (in-range n)])
      (define x (ref i j))
      (define k (+ (* i n) j))
      (unless (real? x)
        (element-error who "the response" "not a real number" x #:position k))
      (unless (fl< (flabs (real->double-flonum x)) +inf.0)
        (element-error who "the response" "not finite" x #:position k))
      x))

  (: row-major->flarray (-> FlVector Index Index FlArray))
  (define (row-major->flarray data m n)
    (unsafe-flarray ((inst vector Index) m n) data)))

(require 'typed)

(define column-names/c (or/c #f (listof (or/c string? symbol?))))

(define matrix/c (flat-named-contract '(and/c array? matrix?) (and/c array? matrix?)))

(define (response-array? A)
  (and (array? A)
       (or (row-matrix? A)
           (col-matrix? A)
           (let ([ds (array-shape A)])
             (and (= (vector-length ds) 1) (positive? (vector-ref ds 0)))))))

(define response-array/c
  (flat-named-contract
   '(and/c array?
           (or/c row-matrix? col-matrix? (property/c array-shape (vector/c exact-positive-integer?))))
   response-array?))

(provide
 (contract-out
  [matrix->design-matrix
   (->* (matrix/c) (#:column-names column-names/c) design-matrix?)]
  [design-matrix->matrix (-> design-matrix? matrix/c)]
  [array->response (-> response-array/c (and/c (listof real?) pair?))]))

(define (array-storage a)
  (cond
    [(flarray? a) (flarray-data a)]
    [(mutable-array? a) (mutable-array-data a)]
    [else #f]))

(define (matrix->design-matrix M #:column-names [names #f])
  (define who 'matrix->design-matrix)
  (define-values (m n) (matrix-shape M))
  (define storage (array-storage M))
  (if (and (flvector? storage) (= (flvector-length storage) (* m n)))
      (flat->design-matrix storage m n names who "the matrix" #:order 'row-major)
      (flat->design-matrix (matrix->f64vector M storage (check-column-names names n who) who)
                           m n names who "the matrix" #:adopt? #t)))

(define (design-matrix->matrix dm)
  (define v (design-matrix-data dm))
  (define m (design-matrix-nrows dm))
  (define n (design-matrix-ncols dm))
  (define rows (make-flvector (* m n)))
  (for* ([i (in-range m)]
         [j (in-range n)])
    (flvector-set! rows (+ j (* i n)) (f64vector-ref v (+ i (* j m)))))
  (row-major->flarray rows m n))

(define (array->response A)
  (array->real-list A (array-storage A) 'array->response))
