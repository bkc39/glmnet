#lang racket/base

;; glmnet/data/math (#37): math/matrix matrices and math arrays in and out of
;; the design-matrix layer. Every kind of math array converts to the design
;; matrix of the same rows, fits from them are `equal?` to fits from the rows as
;; lists, and `(require glmnet)` does not load math-lib.

(module+ test
  (require rackunit
           racket/contract
           racket/flonum
           racket/list
           math/array
           math/matrix
           glmnet
           glmnet/data/math
           (file "../private/demo-utils.rkt"))

  (define rows '((1.0 4.0)
                 (2.0 5.0)
                 (3.0 6.0)))

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
            (for/and ([p (in-list (cons #rx"blaming: [^\n]*math-test[.]rkt" patterns))])
              (regexp-match? p (exn-message e)))))
     thunk))

  ;; --- matrices in ----------------------------------------------------------------

  (test-case "every kind of math matrix gives the design matrix of its rows"
    (define dm (rows->design-matrix rows))
    (for ([M (in-list
              (list (matrix [[1.0 4.0] [2.0 5.0] [3.0 6.0]])
                    (matrix [[1 4] [2 5] [3 6]])
                    (list*->matrix rows)
                    (list->matrix 3 2 (append* rows))
                    (build-matrix 3 2 (lambda (i j) (list-ref (list-ref rows i) j)))
                    (vector->matrix 3 2 (list->vector (append* rows)))
                    (vector*->matrix #(#(1.0 4.0) #(2.0 5.0) #(3.0 6.0)))
                    (flarray #[#[1.0 4.0] #[2.0 5.0] #[3.0 6.0]])
                    (array->flarray (list*->matrix rows))
                    (matrix-transpose (matrix [[1 2 3] [4 5 6]]))
                    (matrix* (list*->matrix rows) (identity-matrix 2))
                    (parameterize ([array-strictness #f])
                      (array-map exact->inexact (list*->matrix rows)))))])
      (check-equal? (matrix->design-matrix M) dm (format "~a" M))))

  (test-case "exact entries become flonums"
    (define dm (matrix->design-matrix (matrix [[1 1/2] [-3 5/4]])))
    (check-equal? (design-matrix->rows dm) '((1.0 0.5) (-3.0 1.25)))
    (check-true (flonum? (design-matrix-ref dm 0 0))))

  (test-case "a one-row and a one-column matrix are design matrices too"
    (check-equal? (design-matrix->rows (matrix->design-matrix (matrix [[1 2 3]])))
                  '((1.0 2.0 3.0)))
    (check-equal? (design-matrix->rows (matrix->design-matrix (->col-matrix '(1 2 3))))
                  '((1.0) (2.0) (3.0))))

  (test-case "column names are carried and checked"
    (define M (list*->matrix rows))
    (check-false (design-matrix-column-names (matrix->design-matrix M)))
    (check-equal? (design-matrix-column-names (matrix->design-matrix M #:column-names '(a "b")))
                  '("a" "b"))
    (check-equal? (matrix->design-matrix M #:column-names '(x1 x2))
                  (rows->design-matrix rows #:column-names '(x1 x2)))
    (check-error (lambda () (matrix->design-matrix M #:column-names '(a)))
                 #rx"^matrix->design-matrix: the number of column names does not match"
                 #rx"column names: 1" #rx"columns: 2")
    (check-error (lambda () (matrix->design-matrix M #:column-names '("a" a)))
                 #rx"^matrix->design-matrix: the column names are not distinct")
    (check-blame (lambda () (matrix->design-matrix M #:column-names '(1 2)))))

  (test-case "the design matrix is a copy"
    (define data (vector 1.0 2.0 3.0 4.0))
    (define M (vector->matrix 2 2 data))
    (define F (flarray #[#[1.0 2.0] #[3.0 4.0]]))
    (define dm (matrix->design-matrix M))
    (define df (matrix->design-matrix F))
    (vector-set! data 0 99.0)
    (array-set! F #(0 0) 99.0)
    (check-equal? (design-matrix-ref dm 0 0) 1.0)
    (check-equal? (design-matrix-ref df 0 0) 1.0))

  ;; --- validation -------------------------------------------------------------------

  (test-case "a non-real element is an error naming its row and column"
    (for ([M (in-list (list (matrix [[1 2] [3 'x]])
                            (vector->matrix 2 2 (vector 1 2 3 'x))))])
      (check-error (lambda () (matrix->design-matrix M))
                   #rx"^matrix->design-matrix: the matrix has an element that is not a real number"
                   #rx"row: 1" #rx"column: 1" #rx"element: 'x"))
    (check-error (lambda () (matrix->design-matrix (matrix [[1 2+1i]])))
                 #rx"not a real number" #rx"row: 0" #rx"column: 1")
    (check-error (lambda () (matrix->design-matrix (fcarray #[#[1.0 2.0]])))
                 #rx"not a real number" #rx"row: 0" #rx"column: 0"))

  (test-case "a non-finite element is an error naming its row and column"
    (for ([M (in-list (list (matrix [[1.0 2.0] [3.0 +nan.0] [5.0 6.0]])
                            (vector->matrix 3 2 (vector 1.0 2.0 3.0 +nan.0 5.0 6.0))
                            (flarray #[#[1.0 2.0] #[3.0 +nan.0] #[5.0 6.0]])))])
      (check-error (lambda () (matrix->design-matrix M))
                   #rx"^matrix->design-matrix: the matrix has an element that is not finite"
                   #rx"row: 1" #rx"column: 1" #rx"element: \\+nan\\.0"))
    (check-error (lambda () (matrix->design-matrix (matrix [[-inf.0 1.0]])))
                 #rx"not finite" #rx"row: 0" #rx"column: 0")
    (check-error (lambda () (matrix->design-matrix (matrix [[1 (expt 10 400)]])))
                 #rx"not finite" #rx"column: 1"))

  (test-case "a value that is not a matrix is a contract error blaming the caller"
    (for ([v (in-list (list rows
                            (array #[1.0 2.0])
                            (array 1.0)
                            (array #[#[#[1.0]]])
                            (array #[#[] #[]])))])
      (check-blame (lambda () (matrix->design-matrix v))
                   #rx"expected: \\(and/c array\\? matrix\\?\\)")))

  ;; --- matrices out ------------------------------------------------------------------

  (test-case "design-matrix->matrix gives a flonum matrix of the rows"
    (define M (design-matrix->matrix (rows->design-matrix rows #:column-names '(a b))))
    (check-true (matrix? M))
    (check-equal? (array-shape M) #(3 2))
    (check-equal? (matrix->list* M) rows)
    (check-equal? M (flarray #[#[1.0 4.0] #[2.0 5.0] #[3.0 6.0]]))
    (check-equal? (flarray-data M) (flarray-data (array->flarray (list*->matrix rows)))))

  (test-case "design-matrix->matrix takes a design matrix, and the caller is blamed"
    (check-blame (lambda () (design-matrix->matrix rows)) #rx"expected: design-matrix\\?"))

  (test-case "storage longer than the shape is read by the shape"
    (define v (flvector 1.0 2.0 3.0))
    (check-equal? (design-matrix->rows (matrix->design-matrix (unsafe-flarray #(1 2) v)))
                  '((1.0 2.0)))
    (check-equal? (array->response (unsafe-flarray #(2) v)) '(1.0 2.0))
    (check-equal? (design-matrix->rows
                   (matrix->design-matrix (unsafe-vector->array #(1 2) (vector 1 2 3))))
                  '((1.0 2.0))))

  (test-case "design-matrix->matrix copies"
    (define dm (rows->design-matrix rows))
    (define M (design-matrix->matrix dm))
    (array-set! M #(0 0) 99.0)
    (check-equal? (design-matrix-ref dm 0 0) 1.0)
    (check-equal? (matrix->design-matrix (design-matrix->matrix dm)) dm))

  (define (random-real)
    (case (random 4)
      [(0) (- (random 2001) 1000)]
      [(1) (/ (- (random 201) 100) (add1 (random 50)))]
      [else (* 1e3 (- (random) 0.5))]))

  (test-case "random matrices round-trip both ways (seeded)"
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 37)
      (for ([_ (in-range 100)])
        (define no (add1 (random 12)))
        (define ni (add1 (random 12)))
        (define xs (for/list ([i (in-range no)])
                     (for/list ([j (in-range ni)]) (random-real))))
        (define M (list*->matrix xs))
        (define dm (matrix->design-matrix M))
        (check-equal? dm (rows->design-matrix xs))
        (check-equal? (design-matrix->matrix dm) (array-map real->double-flonum M))
        (check-equal? (matrix->design-matrix (design-matrix->matrix dm)) dm)
        (check-equal? (matrix->design-matrix (array->mutable-array M)) dm)
        (check-equal? (matrix->design-matrix (array->flarray M)) dm))))

  ;; --- responses ----------------------------------------------------------------------

  (test-case "a one-dimensional array, a row matrix and a column matrix are responses"
    (define y '(1 1/2 2.5))
    (for ([A (in-list (list (list->array y)
                            (->col-matrix y)
                            (->row-matrix y)
                            (vector->array (list->vector y))
                            (->col-matrix (list->vector y))))])
      (check-equal? (array->response A) y (format "~a" A)))
    (check-equal? (array->response (flarray #[1.0 0.5 2.5])) '(1.0 0.5 2.5))
    (check-equal? (array->response (matrix [[7]])) '(7)))

  (test-case "a response keeps exact class labels"
    (define labels (array->response (->col-matrix '(0 2 1 1))))
    (check-equal? labels '(0 2 1 1))
    (check-true (andmap exact-nonnegative-integer? labels)))

  (test-case "a bad response element is an error naming its position"
    (check-error (lambda () (array->response (array #[1.0 +inf.0])))
                 #rx"^array->response: the response has an element that is not finite"
                 #rx"position: 1")
    (check-error (lambda () (array->response (->col-matrix '(1 2 x))))
                 #rx"not a real number" #rx"position: 2" #rx"element: 'x")
    (check-error (lambda () (array->response (flarray #[1.0 +nan.0 3.0])))
                 #rx"not finite" #rx"position: 1")
    (check-error (lambda () (array->response (vector->array (vector 1 'x 3))))
                 #rx"not a real number" #rx"position: 1")
    (check-error (lambda () (array->response (array #[1 #e1e400])))
                 #rx"not finite" #rx"position: 1"))

  (test-case "a lazy array is a response"
    (define lazy
      (parameterize ([array-strictness #f])
        (array-map add1 (array #[0 1 2]))))
    (check-false (array-strict? lazy))
    (check-equal? (array->response lazy) '(1 2 3)))

  (test-case "a response of the wrong shape is a contract error blaming the caller"
    (for ([v (in-list (list '(1 2 3)
                            (matrix [[1 2] [3 4]])
                            (array #[])
                            (array 1)
                            (array #[#[#[1]]])))])
      (check-blame (lambda () (array->response v))
                   #rx"expected: a one-dimensional array, a row matrix or a column matrix")))

  ;; --- fits equal to the list path -------------------------------------------------

  (define-values (Xg yg) (load-longley))
  (define-values (Xb yb) (load-wdbc))
  (define-values (Xm ym) (load-iris))
  (define-values (Xc tc sc) (load-veteran))
  (define-values (Xp yp) (load-warpbreaks))
  (define-values (Xr Yr) (load-linnerud))

  (define (dm-of X) (matrix->design-matrix (list*->matrix X)))
  (define (col-of y) (array->response (->col-matrix y)))

  (define (folds n) (for/list ([i (in-range n)]) (modulo i 3)))

  (test-case "the responses read back as the lists"
    (check-equal? (col-of yg) yg)
    (check-equal? (col-of yb) yb)
    (check-equal? (col-of ym) ym)
    (check-equal? (col-of tc) tc)
    (check-equal? (col-of sc) sc)
    (check-equal? (col-of yp) yp))

  (test-case "Gaussian fits, paths, CV and predictions"
    (define X (dm-of Xg))
    (define y (col-of yg))
    (check-equal? (lasso X y #:lambda 0.1) (lasso Xg yg #:lambda 0.1))
    (check-equal? (elnet-fit X y #:lambda 0.1 #:alpha 0.5) (elnet-fit Xg yg #:lambda 0.1 #:alpha 0.5))
    (check-equal? (ols X y) (ols Xg yg))
    (define path (elnet-path X y))
    (check-equal? path (elnet-path Xg yg))
    (check-equal? (elnet-cv X y #:fold-ids (folds 16)) (elnet-cv Xg yg #:fold-ids (folds 16)))
    (check-equal? (predict path X #:lambda '(0.1 0.01)) (predict path Xg #:lambda '(0.1 0.01)))
    (check-equal? (coef path #:lambda 0.1) (coef (elnet-path Xg yg) #:lambda 0.1)))

  (test-case "binomial fits, paths, CV and predictions"
    (define X (dm-of Xb))
    (define y (col-of yb))
    (define r (logistic-fit X y #:lambda 0.02))
    (check-equal? r (logistic-fit Xb yb #:lambda 0.02))
    (check-equal? (logistic-path X y) (logistic-path Xb yb))
    (check-equal? (logistic-cv X y #:fold-ids (folds 569)) (logistic-cv Xb yb #:fold-ids (folds 569)))
    (check-equal? (logistic-predict-proba r X) (logistic-predict-proba r Xb))
    (check-equal? (predict r X #:type 'class) (predict r Xb #:type 'class)))

  (test-case "multinomial fits, paths, CV and predictions"
    (define X (dm-of Xm))
    (define y (col-of ym))
    (define r (multinomial-fit X y #:lambda 0.01))
    (check-equal? r (multinomial-fit Xm ym #:lambda 0.01))
    (check-equal? (multinomial-path X y) (multinomial-path Xm ym))
    (check-equal? (multinomial-cv X y #:fold-ids (folds 150))
                  (multinomial-cv Xm ym #:fold-ids (folds 150)))
    (check-equal? (multinomial-predict r X) (multinomial-predict r Xm))
    (check-equal? (coef r) (coef (multinomial-fit Xm ym #:lambda 0.01))))

  (test-case "Cox fits, paths, CV and predictions"
    (define X (dm-of Xc))
    (define t (col-of tc))
    (define s (col-of sc))
    (define r (cox-fit X t s #:lambda 0.05))
    (check-equal? r (cox-fit Xc tc sc #:lambda 0.05))
    (check-equal? (cox-path X t s) (cox-path Xc tc sc))
    (check-equal? (cox-cv X t s #:fold-ids (folds 137)) (cox-cv Xc tc sc #:fold-ids (folds 137)))
    (check-equal? (cox-relative-risk r X) (cox-relative-risk r Xc)))

  (test-case "Poisson fits, paths, CV and predictions"
    (define X (dm-of Xp))
    (define y (col-of yp))
    (define r (poisson-fit X y #:lambda 0.1))
    (check-equal? r (poisson-fit Xp yp #:lambda 0.1))
    (check-equal? (poisson-path X y) (poisson-path Xp yp))
    (check-equal? (poisson-cv X y #:fold-ids (folds 54)) (poisson-cv Xp yp #:fold-ids (folds 54)))
    (check-equal? (poisson-predict-mean r X) (poisson-predict-mean r Xp)))

  (test-case "multi-response fits take Y as a math matrix"
    (define X (dm-of Xr))
    (define Y (dm-of Yr))
    (define r (mgaussian-fit X Y #:lambda 1.0))
    (check-equal? r (mgaussian-fit Xr Yr #:lambda 1.0))
    (check-equal? (mgaussian-path X Y) (mgaussian-path Xr Yr))
    (check-equal? (mgaussian-cv X Y #:fold-ids (folds 20)) (mgaussian-cv Xr Yr #:fold-ids (folds 20)))
    (check-equal? (mgaussian-predict r X) (mgaussian-predict r Xr)))

  (test-case "a math matrix with column names is a table for formulas"
    (define names '("deflator" "gnp" "unemployed" "armed" "pop" "year" "employed"))
    (define M (list*->matrix (map (lambda (x y) (append x (list y))) Xg yg)))
    (define table (matrix->design-matrix M #:column-names names))
    (check-true (table? table))
    (define fit (formula-fit (~ employed gnp unemployed) table #:lambda 0.1))
    (define list-table
      (for/list ([name (in-list names)]
                 [column (in-list (apply map list (map (lambda (x y) (append x (list y))) Xg yg)))])
        (cons name column)))
    (check-equal? (coef fit) (coef (formula-fit (~ employed gnp unemployed) list-table #:lambda 0.1)))
    (check-equal? (predict fit table) (predict fit list-table)))

  ;; --- glmnet does not load math-lib --------------------------------------------------

  (test-case "(require glmnet) loads neither math-lib nor this adapter"
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (namespace-require 'glmnet)
      (check-true (module-declared? 'glmnet/data #f))
      (check-false (module-declared? 'glmnet/data/math #f))
      (check-false (module-declared? 'math/array #f))
      (check-false (module-declared? 'math/matrix #f))
      (check-false (module-declared? 'typed/racket/base #f))
      (namespace-require 'glmnet/data/math)
      (check-true (module-declared? 'math/array #f))
      (check-true (module-declared? 'math/matrix #f)))))
