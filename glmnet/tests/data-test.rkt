#lang racket/base

;; Unit tests for the design-matrix layer on its own (data.rkt, #35): the
;; conversions in both directions, the column-major layout, and validation.
;; glmnet/data does not load the native library.

(module+ test
  (require rackunit
           racket/list
           ffi/vector
           glmnet/data)

  (define X '((1.0 4.0)
              (2.0 5.0)
              (3.0 6.0)))

  (define (check-error thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract? e)
            (for/and ([p (in-list patterns)])
              (regexp-match? p (exn-message e)))))
     thunk))

  ;; --- round trips and layout -------------------------------------------------

  (test-case "rows -> design matrix -> rows"
    (define dm (rows->design-matrix X))
    (check-equal? (design-matrix-nrows dm) 3)
    (check-equal? (design-matrix-ncols dm) 2)
    (check-equal? (design-matrix->rows dm) X))

  (test-case "columns -> design matrix -> columns"
    (define cols '((1.0 2.0 3.0) (4.0 5.0 6.0)))
    (define dm (columns->design-matrix cols))
    (check-equal? (design-matrix->columns dm) cols)
    (check-equal? (design-matrix->rows dm) X))

  (test-case "rows and columns of the same matrix give equal design matrices"
    (check-equal? (rows->design-matrix X)
                  (columns->design-matrix (apply map list X)))
    (check-equal? (equal-hash-code (rows->design-matrix X))
                  (equal-hash-code (columns->design-matrix (apply map list X))))
    (check-not-equal? (rows->design-matrix X)
                      (rows->design-matrix '((1.0 4.0) (2.0 5.0) (3.0 7.0))))
    (check-not-equal? (rows->design-matrix X)
                      (rows->design-matrix X #:column-names '(a b))))

  (test-case "element (i, j) lives at index i + j*nrows"
    (define dm (rows->design-matrix X))
    (define v (design-matrix->f64vector dm))
    (check-equal? (f64vector->list v) '(1.0 2.0 3.0 4.0 5.0 6.0))
    (for* ([i (in-range 3)] [j (in-range 2)])
      (check-equal? (design-matrix-ref dm i j) (f64vector-ref v (+ i (* j 3))))
      (check-equal? (design-matrix-ref dm i j) (list-ref (list-ref X i) j))))

  (test-case "an f64vector in the Fortran layout round-trips"
    (define v (f64vector 1.0 2.0 3.0 4.0 5.0 6.0))
    (define dm (f64vector->design-matrix v 3 2))
    (check-equal? dm (rows->design-matrix X))
    (check-equal? (f64vector->list (design-matrix->f64vector dm)) (f64vector->list v)))

  (test-case "the constructor and design-matrix->f64vector copy"
    (define v (f64vector 1.0 2.0 3.0 4.0 5.0 6.0))
    (define dm (f64vector->design-matrix v 3 2))
    (f64vector-set! v 0 +nan.0)
    (check-equal? (design-matrix-ref dm 0 0) 1.0)
    (f64vector-set! (design-matrix->f64vector dm) 1 +nan.0)
    (check-equal? (design-matrix-ref dm 1 0) 2.0))

  (test-case "exact numbers become flonums"
    (define dm (rows->design-matrix '((1 1/2) (-3 2.5))))
    (check-equal? (design-matrix->rows dm) '((1.0 0.5) (-3.0 2.5)))
    (check-true (flonum? (design-matrix-ref dm 0 0)))
    (check-equal? (design-matrix->columns (columns->design-matrix '((1 2) (3/4 0))))
                  '((1.0 2.0) (0.75 0.0))))

  (test-case "a design matrix prints its dimensions"
    (check-equal? (format "~a" (rows->design-matrix X)) "#<design-matrix 3x2>"))

  (test-case "design-matrix-select-rows takes rows in the order given, names and all"
    (define dm (rows->design-matrix X #:column-names '(a b)))
    (define sub (design-matrix-select-rows dm '(2 0)))
    (check-equal? sub (rows->design-matrix '((3.0 6.0) (1.0 4.0)) #:column-names '(a b)))
    (check-equal? (design-matrix->rows (design-matrix-select-rows dm '(1 1 1)))
                  '((2.0 5.0) (2.0 5.0) (2.0 5.0)))
    (check-equal? (design-matrix-select-rows dm '(0 1 2)) dm)
    (check-error (lambda () (design-matrix-select-rows dm '(0 3)))
                 #rx"^design-matrix-select-rows: row index is out of range")
    (check-exn exn:fail:contract? (lambda () (design-matrix-select-rows dm '()))))

  ;; --- column names ------------------------------------------------------------

  (test-case "column names are carried, and default to #f"
    (check-false (design-matrix-column-names (rows->design-matrix X)))
    (check-equal? (design-matrix-column-names (rows->design-matrix X #:column-names '("a" b)))
                  '("a" b))
    (check-equal? (design-matrix-column-names
                   (columns->design-matrix '((1.0) (2.0)) #:column-names '(x1 x2)))
                  '(x1 x2))
    (check-equal? (design-matrix-column-names
                   (f64vector->design-matrix (f64vector 1.0 2.0) 1 2 #:column-names '("u" "v")))
                  '("u" "v")))

  (test-case "column names must match the columns and be distinct"
    (check-error (lambda () (rows->design-matrix X #:column-names '(a)))
                 #rx"number of column names" #rx"column names: 1" #rx"columns: 2")
    (check-error (lambda () (rows->design-matrix X #:column-names '(a a)))
                 #rx"not distinct" #rx"duplicate: 'a")
    (check-exn exn:fail:contract? (lambda () (rows->design-matrix X #:column-names '(1 2)))))

  ;; --- validation, with positions ----------------------------------------------

  (test-case "empty input is rejected"
    (check-error (lambda () (rows->design-matrix '())) #rx"has no rows")
    (check-error (lambda () (rows->design-matrix '(()))) #rx"has no columns")
    (check-error (lambda () (columns->design-matrix '())) #rx"has no columns")
    (check-error (lambda () (columns->design-matrix '(()))) #rx"has no rows")
    (check-error (lambda () (response->f64vector '())) #rx"is empty"))

  (test-case "ragged input is rejected with the row or column"
    (check-error (lambda () (rows->design-matrix '((1.0 2.0) (3.0 4.0) (5.0))))
                 #rx"rows of different lengths" #rx"row: 2" #rx"length: 1" #rx"length of row 0: 2")
    (check-error (lambda () (columns->design-matrix '((1.0 2.0) (3.0))))
                 #rx"columns of different lengths" #rx"column: 1" #rx"length: 1"))

  (test-case "non-real input is rejected with its row and column"
    (check-error (lambda () (rows->design-matrix '((1.0 2.0) (3.0 x))))
                 #rx"not a real number" #rx"row: 1" #rx"column: 1" #rx"element: 'x")
    (check-error (lambda () (rows->design-matrix '((1.0 2.0) (3.0 1+2i))))
                 #rx"not a real number" #rx"row: 1" #rx"column: 1")
    (check-error (lambda () (columns->design-matrix '((1.0 2.0) (3.0 "4"))))
                 #rx"not a real number" #rx"row: 1" #rx"column: 1")
    (check-error (lambda () (response->f64vector '(1.0 #f)))
                 #rx"not a real number" #rx"position: 1"))

  (test-case "non-finite input is rejected with its row and column"
    (check-error (lambda () (rows->design-matrix '((1.0 2.0) (2.0 +nan.0) (3.0 4.0))))
                 #rx"not finite" #rx"row: 1" #rx"column: 1" #rx"element: \\+nan\\.0")
    (check-error (lambda () (rows->design-matrix '((1.0 -inf.0))))
                 #rx"not finite" #rx"row: 0" #rx"column: 1")
    (check-error (lambda () (columns->design-matrix '((1.0 2.0 +inf.0) (3.0 4.0 5.0))))
                 #rx"not finite" #rx"row: 2" #rx"column: 0")
    (check-error (lambda () (f64vector->design-matrix (f64vector 1.0 2.0 3.0 +nan.0) 2 2))
                 #rx"not finite" #rx"row: 1" #rx"column: 1")
    (check-error (lambda () (response->f64vector '(1.0 4.0 +inf.0 6.0)))
                 #rx"not finite" #rx"position: 2"))

  (test-case "an exact number too large for a flonum is not finite"
    (check-error (lambda () (rows->design-matrix (list (list 1 (expt 10 400)))))
                 #rx"not finite" #rx"column: 1"))

  (test-case "the f64vector length must be nrows * ncols"
    (check-error (lambda () (f64vector->design-matrix (f64vector 1.0 2.0 3.0) 2 2))
                 #rx"length: 3" #rx"nrows: 2" #rx"ncols: 2"))

  (test-case "design-matrix-ref checks its indices"
    (define dm (rows->design-matrix X))
    (check-error (lambda () (design-matrix-ref dm 3 0)) #rx"row index is out of range")
    (check-error (lambda () (design-matrix-ref dm 0 2)) #rx"column index is out of range"))

  (test-case "response->f64vector converts exact numbers"
    (check-equal? (f64vector->list (response->f64vector '(1 1/4 2.5))) '(1.0 0.25 2.5)))

  ;; --- randomized round trips --------------------------------------------------

  (define (random-real)
    (case (random 4)
      [(0) (- (random 2001) 1000)]
      [(1) (/ (- (random 201) 100) (add1 (random 50)))]
      [else (* 1e3 (- (random) 0.5))]))

  (test-case "random shapes round-trip through every conversion (seeded)"
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 35)
      (for ([_ (in-range 200)])
        (define no (add1 (random 12)))
        (define ni (add1 (random 12)))
        (define rows (for/list ([i (in-range no)])
                       (for/list ([j (in-range ni)]) (random-real))))
        (define flo (for/list ([row (in-list rows)]) (map real->double-flonum row)))
        (define cols (apply map list flo))
        (define dm (rows->design-matrix rows))
        (check-equal? (design-matrix->rows dm) flo)
        (check-equal? (design-matrix->columns dm) cols)
        (check-equal? (columns->design-matrix cols) dm)
        (define v (design-matrix->f64vector dm))
        (check-equal? (f64vector->list v) (append* cols))
        (check-equal? (f64vector->design-matrix v no ni) dm)
        (define i (random no))
        (define j (random ni))
        (check-equal? (design-matrix-ref dm i j) (list-ref (list-ref flo i) j))))))
