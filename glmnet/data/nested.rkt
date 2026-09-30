#lang racket/base

(require racket/contract
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

(define (design-matrix->nested dm #:by [by 'rows] #:outer [outer 'list] #:inner [inner 'list])
  (design-matrix->nested-lines dm by outer inner))
