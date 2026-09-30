#lang racket/base

;; glmnet/data/nested (#36): Racket's own nested data, lists and vectors of
;; lists and vectors, to and from design matrices. The conversion in is the one
;; every fitter applies to a matrix argument (data.rkt), so a nesting fits
;; exactly as the same list of lists does.

(require racket/contract
         ffi/vector
         (only-in "../data.rkt" design-matrix?)
         (submod "../data.rkt" support))

(define nested/c
  (flat-named-contract
   '(or/c (listof (or/c list? vector?)) (vectorof (or/c list? vector?)))
   nested-matrix?))

(define by/c (or/c 'rows 'columns))
(define container/c (or/c 'list 'vector))

(provide
 (contract-out
  [nested->design-matrix
   (->* (nested/c)
        (#:by by/c
         #:column-names (or/c #f (listof (or/c string? symbol?))))
        design-matrix?)]
  [design-matrix->nested
   (->* (design-matrix?)
        (#:by by/c #:outer container/c #:inner container/c)
        (or/c list? vector?))]))

(define (nested->design-matrix xss #:by [by 'rows] #:column-names [names #f])
  (nested->dm xss by names 'nested->design-matrix "the matrix"))

(define (design-matrix->nested dm
                               #:by [by 'rows]
                               #:outer [outer 'list]
                               #:inner [inner 'list])
  (define v (design-matrix-data dm))
  (define no (design-matrix-nrows dm))
  (define ni (design-matrix-ncols dm))
  (define-values (n-outer n-inner outer-stride inner-stride)
    (if (eq? by 'rows)
        (values no ni 1 no)
        (values ni no no 1)))
  (define (line o)
    (define start (* o outer-stride))
    (define (entry p) (f64vector-ref v (+ start (* p inner-stride))))
    (if (eq? inner 'vector)
        (for/vector #:length n-inner ([p (in-range n-inner)]) (entry p))
        (build-list-from-end n-inner entry)))
  (if (eq? outer 'vector)
      (for/vector #:length n-outer ([o (in-range n-outer)]) (line o))
      (build-list-from-end n-outer line)))

;; (build-list n f) consed from its last element, with no reversal.
(define (build-list-from-end n f)
  (for/fold ([acc '()]) ([k (in-range (sub1 n) -1 -1)])
    (cons (f k) acc)))
