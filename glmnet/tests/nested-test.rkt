#lang racket/base

;; Unit tests for glmnet/data/nested (#36) on its own: every nesting of lists
;; and vectors to and from a design matrix, in both orientations, and the
;; one-dimensional inputs of response->f64vector and response/c. Neither
;; module loads the native library.

(module+ test
  (require rackunit
           racket/contract
           racket/flonum
           racket/list
           ffi/vector
           glmnet/data
           glmnet/data/nested)

  (define X '((1.0 4.0)
              (2.0 5.0)
              (3.0 6.0)))

  (define (lists->nesting xss outer inner)
    (define lines (for/list ([xs (in-list xss)]) (if (eq? inner 'vector) (list->vector xs) xs)))
    (if (eq? outer 'vector) (list->vector lines) lines))

  (define nestings '((list list) (list vector) (vector list) (vector vector)))

  (define (check-error thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract? e)
            (for/and ([p (in-list patterns)])
              (regexp-match? p (exn-message e)))))
     thunk))

  (define (check-blame thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract:blame? e)
            (for/and ([p (in-list (cons #rx"blaming: [^\n]*nested-test[.]rkt" patterns))])
              (regexp-match? p (exn-message e)))))
     thunk))

  ;; --- the four nestings, both ways ---------------------------------------------

  (test-case "every nesting of rows converts as the list of lists does"
    (define dm (rows->design-matrix X))
    (for ([shape (in-list nestings)])
      (define rows (lists->nesting X (car shape) (cadr shape)))
      (check-equal? (nested->design-matrix rows) dm (format "~a" shape))
      (check-equal? (nested->design-matrix rows #:by 'rows) dm)
      (check-equal? (design-matrix->nested dm #:outer (car shape) #:inner (cadr shape)) rows)))

  (test-case "every nesting of columns converts as columns->design-matrix does"
    (define cols (apply map list X))
    (define dm (columns->design-matrix cols))
    (check-equal? dm (rows->design-matrix X))
    (for ([shape (in-list nestings)])
      (define columns (lists->nesting cols (car shape) (cadr shape)))
      (check-equal? (nested->design-matrix columns #:by 'columns) dm (format "~a" shape))
      (check-equal? (design-matrix->nested dm #:by 'columns #:outer (car shape) #:inner (cadr shape))
                    columns)))

  (test-case "design-matrix->nested defaults to a list of rows"
    (define dm (rows->design-matrix X))
    (check-equal? (design-matrix->nested dm) X)
    (check-equal? (design-matrix->nested dm) (design-matrix->rows dm))
    (check-equal? (design-matrix->nested dm #:by 'columns) (design-matrix->columns dm)))

  (test-case "the vectors design-matrix->nested returns are fresh and mutable"
    (define dm (rows->design-matrix X))
    (define rows (design-matrix->nested dm #:outer 'vector #:inner 'vector))
    (vector-set! (vector-ref rows 0) 0 99.0)
    (check-equal? (design-matrix-ref dm 0 0) 1.0)
    (check-false (eq? rows (design-matrix->nested dm #:outer 'vector #:inner 'vector))))

  (test-case "a row can be a list or a vector within one matrix"
    (define mixed (list '(1.0 4.0) #(2.0 5.0) '(3 6)))
    (check-equal? (nested->design-matrix mixed) (rows->design-matrix X))
    (check-equal? (nested->design-matrix (list->vector mixed)) (rows->design-matrix X)))

  (test-case "exact numbers become flonums"
    (define dm (nested->design-matrix (vector #(1 1/2) (vector -3 2.5))))
    (check-equal? (design-matrix->nested dm #:outer 'vector #:inner 'vector)
                  (vector (vector 1.0 0.5) (vector -3.0 2.5)))
    (check-true (flonum? (design-matrix-ref dm 0 0))))

  (test-case "column names are carried, and must match the columns"
    (define dm (nested->design-matrix (vector #(1.0 2.0)) #:column-names '("a" b)))
    (check-equal? (design-matrix-column-names dm) '("a" b))
    (check-equal? dm (rows->design-matrix '((1.0 2.0)) #:column-names '("a" b)))
    (check-true (table? dm))
    (check-equal? (design-matrix-column-names
                   (nested->design-matrix (vector #(1.0 2.0) #(3.0 4.0)) #:by 'columns
                                          #:column-names '(u v)))
                  '(u v))
    (check-error (lambda () (nested->design-matrix (vector #(1.0 2.0)) #:column-names '(a)))
                 #rx"^nested->design-matrix: the number of column names")
    (check-error (lambda () (nested->design-matrix (vector #(1.0 2.0)) #:column-names '(a "a")))
                 #rx"not distinct"))

  ;; --- validation ---------------------------------------------------------------

  (test-case "empty input is rejected"
    (check-error (lambda () (nested->design-matrix #())) #rx"the matrix has no rows")
    (check-error (lambda () (nested->design-matrix '())) #rx"the matrix has no rows")
    (check-error (lambda () (nested->design-matrix (vector #()))) #rx"the matrix has no columns")
    (check-error (lambda () (nested->design-matrix #() #:by 'columns)) #rx"the matrix has no columns")
    (check-error (lambda () (nested->design-matrix (list #()) #:by 'columns))
                 #rx"the matrix has no rows"))

  (test-case "ragged input is rejected with the row or column and its length"
    (check-error (lambda () (nested->design-matrix (vector #(1.0 2.0) #(3.0 4.0) #(5.0))))
                 #rx"^nested->design-matrix: the matrix has rows of different lengths"
                 #rx"row: 2" #rx"length: 1" #rx"length of row 0: 2")
    (check-error (lambda () (nested->design-matrix (vector #(1.0 2.0) '(3.0 4.0 5.0))))
                 #rx"rows of different lengths" #rx"row: 1" #rx"length: 3")
    (check-error (lambda () (nested->design-matrix (list '(1.0 2.0) #(3.0))))
                 #rx"rows of different lengths" #rx"row: 1" #rx"length: 1")
    (check-error (lambda () (nested->design-matrix (vector #(1.0 2.0) #(3.0)) #:by 'columns))
                 #rx"columns of different lengths" #rx"column: 1" #rx"length of column 0: 2"))

  (test-case "a list row and a vector row with the same entries give the same error"
    (for* ([bad (in-list '((x 3.0 4.0) (x 3.0) (1.0 2.0 x) (1.0)))]
           [outer (in-list (list values list->vector))])
      (define (message row)
        (with-handlers ([exn:fail:contract? exn-message])
          (nested->design-matrix (outer (list '(1.0 2.0) row)))))
      (check-equal? (message (list->vector bad)) (message bad) (format "~a" bad)))
    (check-error (lambda () (nested->design-matrix (vector #(1.0 2.0) #(x 3.0 4.0))))
                 #rx"not a real number" #rx"row: 1" #rx"column: 0")
    (check-error (lambda () (nested->design-matrix (vector #(1.0 2.0) #(3.0 4.0 x))))
                 #rx"rows of different lengths" #rx"row: 1" #rx"length: 3"))

  (test-case "a bad entry is named by its row and column, in either orientation"
    (check-error (lambda () (nested->design-matrix (vector #(1.0 2.0) #(3.0 x))))
                 #rx"not a real number" #rx"row: 1" #rx"column: 1" #rx"element: 'x")
    (check-error (lambda () (nested->design-matrix (vector '(1.0 2.0) '(+nan.0 4.0))))
                 #rx"not finite" #rx"row: 1" #rx"column: 0" #rx"element: \\+nan\\.0")
    (check-error (lambda () (nested->design-matrix (vector #(1.0 2.0 +inf.0) #(4.0 5.0 6.0))
                                                   #:by 'columns))
                 #rx"not finite" #rx"row: 2" #rx"column: 0"))

  (test-case "the shapes are checked by contract, and the caller is blamed"
    (check-blame (lambda () (nested->design-matrix #(1.0 2.0)))
                 #rx"expected: \\(or/c \\(listof \\(or/c list\\? vector\\?\\)\\) \\(vectorof \\(or/c list\\? vector\\?\\)\\)\\)"
                 #rx"given: '#\\(1.0 2.0\\)")
    (check-blame (lambda () (nested->design-matrix (flvector 1.0 2.0))))
    (check-blame (lambda () (nested->design-matrix X #:by 'diagonal)))
    (check-blame (lambda () (design-matrix->nested (rows->design-matrix X) #:outer 'flvector)))
    (check-blame (lambda () (design-matrix->nested X))))

  ;; --- design-matrix/c ----------------------------------------------------------

  (test-case "design-matrix/c accepts the four nestings and names them"
    (for ([shape (in-list nestings)])
      (check-true (contract-first-order-passes?
                   design-matrix/c (lists->nesting X (car shape) (cadr shape)))))
    (check-true (contract-first-order-passes? design-matrix/c (rows->design-matrix X)))
    (check-false (contract-first-order-passes? design-matrix/c '(1.0 2.0)))
    (check-false (contract-first-order-passes? design-matrix/c #(1.0 2.0)))
    (check-false (contract-first-order-passes? design-matrix/c (f64vector 1.0 2.0)))
    (check-true (flat-contract? design-matrix/c))
    (check-equal? (contract-name design-matrix/c)
                  '(or/c design-matrix? (listof (or/c list? vector?)) (vectorof (or/c list? vector?)))))

  (test-case "design-matrix/c does not wrap a vector"
    (define rows (vector (vector 1.0 2.0)))
    (define/contract (f v) (-> design-matrix/c any) v)
    (check-eq? (f rows) rows))

  ;; --- one-dimensional inputs ---------------------------------------------------

  (define (one-dimensional ys)
    (define fs (map real->double-flonum ys))
    (list ys (list->vector ys) (apply flvector fs) (list->f64vector fs)))

  (test-case "response->f64vector takes a list, vector, flvector or f64vector"
    (for ([y (in-list (one-dimensional '(1 1/4 2.5)))])
      (check-equal? (f64vector->list (response->f64vector y)) '(1.0 0.25 2.5))))

  (test-case "response->f64vector copies an f64vector"
    (define v (f64vector 1.0 2.0))
    (define out (response->f64vector v))
    (f64vector-set! v 0 9.0)
    (check-equal? (f64vector->list out) '(1.0 2.0)))

  (test-case "response->f64vector names the position of a bad entry in any form"
    (for ([y (in-list (list '(1.0 +nan.0) (vector 1.0 +nan.0) (flvector 1.0 +nan.0)
                            (f64vector 1.0 +nan.0)))])
      (check-error (lambda () (response->f64vector y))
                   #rx"^response->f64vector: the response has an element that is not finite"
                   #rx"position: 1"))
    (check-error (lambda () (response->f64vector (vector 1.0 'b)))
                 #rx"not a real number" #rx"position: 1")
    (for ([y (in-list (list '() (vector) (flvector) (f64vector)))])
      (check-error (lambda () (response->f64vector y)) #rx"the response is empty"))
    (check-blame (lambda () (response->f64vector 3.0))
                 #rx"expected: \\(or/c list\\? vector\\? flvector\\? f64vector\\?\\)"))

  (test-case "response/c accepts each form, non-empty, with elements that pass"
    (define c (response/c (>/c 0)))
    (check-true (flat-contract? c))
    (check-equal? (contract-name c) '(response/c (>/c 0)))
    (for ([y (in-list (one-dimensional '(1 2.5 3)))])
      (check-true (contract-first-order-passes? c y)))
    (for ([y (in-list (one-dimensional '(1 -2.5 3)))])
      (check-false (contract-first-order-passes? c y)))
    (for ([y (in-list (list '() (vector) (flvector) (f64vector) 3 "abc" '(1 . 2)))])
      (check-false (contract-first-order-passes? c y))))

  (test-case "response/c names the shapes, or the position of the element that fails"
    (define/contract (f y) (-> (response/c (or/c 0 1)) any) y)
    (define v (vector 0 1))
    (check-eq? (f v) v)
    (check-blame (lambda () (f "01"))
                 #rx"expected: a non-empty list, vector, flvector or f64vector of \\(or/c 0 1\\)"
                 #rx"given: \"01\"")
    (check-blame (lambda () (f (flvector)))
                 #rx"expected: a non-empty list, vector, flvector or f64vector")
    (check-blame (lambda () (f (f64vector 0.0 1.0 2.0)))
                 #rx"expected: \\(or/c 0 1\\)" #rx"given: 2.0"
                 #rx"in: the element at position 2 of\n *the 1st argument of"))

  (test-case "response/c takes only real elements, though (or/c 0 1) passes 1.0+0.0i"
    (check-true ((flat-contract-predicate (or/c 0 1)) 1.0+0.0i))
    (check-false (contract-first-order-passes? (response/c (or/c 0 1)) (list 0 1.0+0.0i)))
    (define/contract (f y) (-> (response/c (or/c 0 1)) any) y)
    (check-blame (lambda () (f (vector 0 1.0+0.0i)))
                 #rx"expected: real\\?" #rx"given: 1.0\\+0.0i"
                 #rx"the element at position 1 of"))

  ;; --- randomized round trips ---------------------------------------------------

  (define (random-real)
    (case (random 4)
      [(0) (- (random 2001) 1000)]
      [(1) (/ (- (random 201) 100) (add1 (random 50)))]
      [else (* 1e3 (- (random) 0.5))]))

  (test-case "random shapes round-trip through every nesting (seeded)"
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 36)
      (for ([_ (in-range 200)])
        (define no (add1 (random 12)))
        (define ni (add1 (random 12)))
        (define rows (for/list ([i (in-range no)])
                       (for/list ([j (in-range ni)]) (random-real))))
        (define flo (for/list ([row (in-list rows)]) (map real->double-flonum row)))
        (define dm (rows->design-matrix rows))
        (define shape (list-ref nestings (random 4)))
        (define by (if (zero? (random 2)) 'rows 'columns))
        (define lines (if (eq? by 'rows) rows (apply map list rows)))
        (define flo-lines (if (eq? by 'rows) flo (apply map list flo)))
        (define nesting (lists->nesting lines (car shape) (cadr shape)))
        (check-equal? (nested->design-matrix nesting #:by by) dm)
        (check-equal? (design-matrix->nested dm #:by by #:outer (car shape) #:inner (cadr shape))
                      (lists->nesting flo-lines (car shape) (cadr shape)))))))
