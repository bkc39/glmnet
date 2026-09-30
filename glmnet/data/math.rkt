#lang racket/base

;; glmnet/data/math (#37): math/matrix matrices to and from design matrices,
;; and math arrays as responses. main.rkt does not require it, so that
;; `(require glmnet)` does not load math-lib.
;;
;; Every math array that untyped code holds carries the contract of math-lib's
;; Typed Racket exports, and so does the procedure that computes its elements,
;; which checks each call. The element loops are therefore typed, so that they
;; add no contract of their own, and they bypass that procedure when the array
;; has storage: a flonum array's flvector or a mutable array's vector, both in
;; row-major order.

(require racket/contract
         (only-in racket/flonum flvector? make-flvector flvector-set!)
         (only-in ffi/vector f64vector-ref)
         (only-in math/array array? mutable-array? mutable-array-data flarray-data)
         (only-in math/matrix matrix? matrix-shape)
         ;; math/array exports flarray-data but no predicate for flonum arrays.
         (only-in (submod math/private/array/flarray-struct defs) flarray?)
         (only-in "../data.rkt" design-matrix?)
         (only-in (submod "../data.rkt" support)
                  design-matrix-data design-matrix-nrows design-matrix-ncols
                  row-major-flvector->design-matrix))

(module typed typed/racket/base
  (require racket/flonum
           math/array
           (only-in math/matrix row-matrix? col-matrix?))

  (provide response-array?
           matrix-flonums
           array-response
           row-major->flarray)

  (define-type Storage (U #f FlVector VectorTop))

  (: response-array? (-> Any Boolean))
  (define (response-array? v)
    (and (array? v)
         (positive? (shape-size v))
         (or (= (array-dims v) 1) (row-matrix? v) (col-matrix? v))))

  ;; The number of elements a's shape has, which is what the loops read. math's
  ;; unsafe constructors can make an array whose storage, and array-size, is
  ;; longer.
  (: shape-size (-> (Array Any) Integer))
  (define (shape-size a)
    (for/fold ([size : Integer 1]) ([d (in-vector (array-shape a))])
      (* size d)))

  ;; Element k of a in row-major order. The index vectors are immutable, so the
  ;; contract on a's element procedure checks them instead of wrapping them.
  (: element-reader (-> (Array Any) Storage (-> Integer Any)))
  (define (element-reader a storage)
    (cond
      [(flvector? storage) (lambda (k) (flvector-ref storage k))]
      [(vector? storage) (lambda (k) (vector-ref storage k))]
      [else
       (define proc (unsafe-array-proc a))
       (define ds (array-shape a))
       (if (= (vector-length ds) 1)
           (lambda (k) (proc (vector-immutable (assert k index?))))
           (let ([n (vector-ref ds 1)])
             (lambda (k)
               (proc (vector-immutable (assert (quotient k n) index?)
                                       (assert (remainder k n) index?))))))]))

  (: matrix-flonums (-> Any Storage Symbol FlVector))
  (define (matrix-flonums M storage who)
    (define a (assert M array?))
    (define n (vector-ref (array-shape a) 1))
    (define ref (element-reader a storage))
    (define size (shape-size a))
    (define out (make-flvector size))
    (for ([k (in-range size)])
      (define x (ref k))
      (unless (real? x) (matrix-element-error who "not a real number" x k n))
      (define v (real->double-flonum x))
      (unless (fl< (flabs v) +inf.0) (matrix-element-error who "not finite" x k n))
      (flvector-set! out k v))
    out)

  (: matrix-element-error (-> Symbol String Any Integer Integer Nothing))
  (define (matrix-element-error who problem x k n)
    (raise-arguments-error who (string-append "the matrix has an element that is " problem)
                           "row" (quotient k n) "column" (remainder k n) "element" x))

  (: array-response (-> Any Storage Symbol (Listof Real)))
  (define (array-response A storage who)
    (define a (assert A array?))
    (define ref (element-reader a storage))
    (for/list : (Listof Real) ([k (in-range (shape-size a))])
      (define x (ref k))
      (unless (real? x) (response-element-error who "not a real number" x k))
      (define v (real->double-flonum x))
      (unless (fl< (flabs v) +inf.0) (response-element-error who "not finite" x k))
      x))

  (: response-element-error (-> Symbol String Any Integer Nothing))
  (define (response-element-error who problem x k)
    (raise-arguments-error who (string-append "the response has an element that is " problem)
                           "position" k "element" x))

  (: row-major->flarray (-> FlVector Index Index FlArray))
  (define (row-major->flarray data m n)
    (unsafe-flarray ((inst vector Index) m n) data)))

(require 'typed)

(define column-names/c (or/c #f (listof (or/c string? symbol?))))

(define matrix/c (flat-named-contract '(and/c array? matrix?) (and/c array? matrix?)))

(define response-array/c
  (flat-contract-with-explanation
   (lambda (v)
     (or (response-array? v)
         (lambda (blame)
           (raise-blame-error
            blame v
            '(expected: "a one-dimensional array, a row matrix or a column matrix, not empty"
              given: "~e")
            v))))
   #:name 'response-array/c))

(provide
 (contract-out
  [matrix->design-matrix
   (->* (matrix/c) (#:column-names column-names/c) design-matrix?)]
  [design-matrix->matrix (-> design-matrix? matrix/c)]
  [array->response (-> response-array/c (listof real?))]))

(define (array-storage a)
  (cond
    [(flarray? a) (flarray-data a)]
    [(mutable-array? a) (mutable-array-data a)]
    [else #f]))

(define (matrix->design-matrix M #:column-names [names #f])
  (define who 'matrix->design-matrix)
  (define-values (m n) (matrix-shape M))
  (define storage (array-storage M))
  (row-major-flvector->design-matrix
   (if (flvector? storage) storage (matrix-flonums M storage who))
   m n names who))

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
  (array-response A (array-storage A) 'array->response))
