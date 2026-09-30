#lang racket/base

;; Unit tests for the design-matrix layer on its own (data.rkt, #35): the
;; conversions in both directions, the column-major layout, and validation.
;; glmnet/data does not load the native library.

(module+ test
  (require rackunit
           racket/contract
           racket/list
           racket/flonum
           ffi/vector
           glmnet/data
           (only-in (submod glmnet/data support)
                    flat->design-matrix design-matrix-data element-error missing-error
                    ->finite-flonum default-column-names))

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

  ;; A contract violation that blames this module, the caller.
  (define (check-blame thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract:blame? e)
            (for/and ([p (in-list (cons #rx"blaming: [^\n]*data-test[.]rkt" patterns))])
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

  (test-case "column names are carried as strings, and default to #f"
    (check-false (design-matrix-column-names (rows->design-matrix X)))
    (check-equal? (design-matrix-column-names (rows->design-matrix X #:column-names '("a" b)))
                  '("a" "b"))
    (check-equal? (design-matrix-column-names
                   (columns->design-matrix '((1.0) (2.0)) #:column-names '(x1 x2)))
                  '("x1" "x2"))
    (check-equal? (rows->design-matrix X #:column-names '(a b))
                  (rows->design-matrix X #:column-names '("a" "b")))
    (check-equal? (design-matrix-column-names
                   (f64vector->design-matrix (f64vector 1.0 2.0) 1 2 #:column-names '("u" "v")))
                  '("u" "v")))

  (test-case "column names must match the columns and be distinct"
    (check-error (lambda () (rows->design-matrix X #:column-names '(a)))
                 #rx"number of column names" #rx"column names: 1" #rx"columns: 2")
    (check-error (lambda () (rows->design-matrix X #:column-names '(a a)))
                 #rx"not distinct" #rx"duplicate: 'a")
    (check-exn exn:fail:contract? (lambda () (rows->design-matrix X #:column-names '(1 2)))))

  (test-case "column names are compared as strings"
    (check-error (lambda () (rows->design-matrix X #:column-names '("x" x)))
                 #rx"not distinct" #rx"duplicate: 'x" #rx"column names: '\\(\"x\" x\\)")
    (check-error (lambda () (columns->design-matrix '((1.0) (2.0)) #:column-names '(b "b")))
                 #rx"not distinct" #rx"duplicate: \"b\"")
    (check-error (lambda () (f64vector->design-matrix (f64vector 1.0 2.0) 1 2
                                                      #:column-names '("u" u)))
                 #rx"not distinct"))

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

  (test-case "the f64vector length must be nrows * ncols, and the caller is blamed"
    (check-blame (lambda () (f64vector->design-matrix (f64vector 1.0 2.0 3.0) 2 2))
                 #rx"expected: an f64vector of length nrows \\* ncols = 4"
                 #rx"given: an f64vector of length 3"
                 #rx"the v argument")
    (check-blame (lambda () (f64vector->design-matrix '(1.0 2.0) 1 2))
                 #rx"given: '\\(1.0 2.0\\)" #rx"the v argument"))

  (test-case "design-matrix-ref checks its indices, and the caller is blamed"
    (define dm (rows->design-matrix X))
    (check-blame (lambda () (design-matrix-ref dm 3 0))
                 #rx"expected: \\(integer-in 0 2\\)" #rx"given: 3" #rx"the i argument")
    (check-blame (lambda () (design-matrix-ref dm 0 2))
                 #rx"expected: \\(integer-in 0 1\\)" #rx"given: 2" #rx"the j argument"))

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
        (check-equal? (design-matrix-ref dm i j) (list-ref (list-ref flo i) j)))))

  ;; --- the data formats' support set (#41) -----------------------------------

  (test-case "flat->design-matrix reads column-major or row-major data, copied or adopted"
    (define cm (f64vector 1.0 2.0 3.0 4.0 5.0 6.0))
    (define dm (flat->design-matrix cm 2 3 #f 'test "the data"))
    (check-equal? (design-matrix->rows dm) '((1.0 3.0 5.0) (2.0 4.0 6.0)))
    (check-equal? (flat->design-matrix (flvector 1.0 3.0 5.0 2.0 4.0 6.0) 2 3 #f 'test "the data"
                                       #:order 'row-major)
                  dm)
    (check-equal? (flat->design-matrix (flvector 1.0 2.0 3.0 4.0 5.0 6.0) 2 3 #f 'test "the data")
                  dm)
    (define adopted (flat->design-matrix cm 2 3 '(a "b" c) 'test "the data" #:adopt? #t))
    (check-eq? (design-matrix-data adopted) cm)
    (check-equal? (design-matrix-column-names adopted) '("a" "b" "c"))
    (f64vector-set! cm 0 99.0)
    (check-eqv? (design-matrix-ref dm 0 0) 1.0))

  (test-case "flat->design-matrix checks the length and what it may adopt by contract"
    (check-blame (lambda () (flat->design-matrix (f64vector 1.0 2.0 3.0) 2 2 #f 'test "the data"))
                 #rx"does not have nrows \\* ncols entries")
    (check-blame (lambda () (flat->design-matrix (flvector 1.0) 1 1 #f 'test "the data" #:adopt? #t))
                 #rx"only a column-major f64vector can be adopted")
    (check-blame (lambda () (flat->design-matrix (f64vector 1.0 2.0) 1 2 #f 'test "the data"
                                                 #:order 'row-major #:adopt? #t))
                 #rx"only a column-major f64vector can be adopted")
    (check-error (lambda () (flat->design-matrix (f64vector 1.0 2.0) 1 2 '(a) 'test "the data"))
                 #rx"^test: the number of column names"))

  (test-case "flat->design-matrix names a non-finite entry by its row and its column"
    (check-error (lambda () (flat->design-matrix (f64vector 1.0 2.0 +nan.0 4.0) 2 2 '(a b)
                                                 'polars->design-matrix "the dataframe"))
                 #rx"^polars->design-matrix: the dataframe has an element that is not finite\n  column: \"b\"\n  row: 0\n  element: \\+nan.0$")
    (check-error (lambda () (flat->design-matrix (flvector 1.0 2.0 +inf.0 4.0) 2 2 #f
                                                 'matrix->design-matrix "the matrix" #:order 'row-major))
                 #rx"not finite\n  column: 0\n  row: 1\n")
    (check-error (lambda () (f64vector->design-matrix (f64vector 1.0 2.0 3.0 -inf.0) 2 2
                                                      #:column-names '(u v)))
                 #rx"^f64vector->design-matrix: the vector has an element that is not finite\n  column: \"v\"\n  row: 1"))

  (test-case "the shared errors, conversion and default names"
    (check-error (lambda () (element-error 'f "the table" "not a real number" 'x #:row 2 #:column "g"))
                 #rx"^f: the table has an element that is not a real number\n  column: \"g\"\n  row: 2\n  element: 'x$")
    (check-error (lambda () (element-error 'f "y" "not finite" +inf.0 #:position 3))
                 #rx"^f: y has an element that is not finite\n  position: 3\n  element: \\+inf.0$")
    (check-error (lambda () (missing-error 'f "the dataframe" #:row 1 #:column "a"))
                 #rx"^f: the dataframe has a missing value\n  column: \"a\"\n  row: 1$")
    (check-error (lambda () (missing-error 'f "the input" #:row 0 #:column "a" #:details '("line" 2)))
                 #rx"has a missing value\n  column: \"a\"\n  row: 0\n  line: 2$")
    (check-error (lambda () (missing-error 'f "the response" #:position 4 #:element #f))
                 #rx"^f: the response has a missing value\n  position: 4\n  element: #f$")
    (check-eqv? (->finite-flonum 1/2 'f "the matrix" 0 0) 0.5)
    (check-error (lambda () (->finite-flonum (expt 10 400) 'f "the matrix" 1 "c"))
                 #rx"not finite\n  column: \"c\"\n  row: 1")
    (check-error (lambda () (->finite-flonum "1" 'f "the matrix" 1 0))
                 #rx"not a real number\n  column: 0\n  row: 1")
    (check-equal? (default-column-names 3) '("V1" "V2" "V3")))

  (test-case "response/c contracts on equivalent elements are contract-equivalent?"
    (check-true (contract-equivalent? (response/c real?) (response/c real?)))
    (check-true (contract-equivalent? (response/c (or/c 0 1)) (response/c (or/c 0 1))))
    (check-true (contract-equivalent? (response/c (>/c 0)) (response/c (>/c 0))))
    (check-false (contract-equivalent? (response/c (>/c 0)) (response/c (>=/c 0))))
    (check-true (contract-stronger? (response/c (>/c 0)) (response/c (>=/c 0))))
    (check-false (contract-stronger? (response/c real?) (response/c (>/c 0))))
    (check-true (flat-contract? (response/c real?)))
    (check-equal? (contract-name (response/c (or/c 0 1))) '(response/c (or/c 0 1)))))
