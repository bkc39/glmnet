#lang racket/base

;; R glmnet's example datasets and R's mtcars and iris (#61): each loader's
;; values have R's shape and names, are its CSV's values, and are what its
;; family's fitter takes. parity-test.rkt checks every value against R's.

(module+ test
  (require rackunit
           racket/list
           glmnet
           glmnet/data/csv
           glmnet/datasets)

  (define (dataset-table name)
    (csv-file->table (collection-file-path (string-append name ".csv") "glmnet" "datasets")))

  (define (v-names p) (for/list ([j (in-range 1 (add1 p))]) (format "V~a" j)))

  ;; x: n rows, V1 ... Vp, the file's columns of those names.
  (define (check-x x name n p)
    (define table (dataset-table name))
    (check-equal? (design-matrix-nrows x) n)
    (check-equal? (design-matrix-column-names x) (v-names p))
    (check-equal? x (table->design-matrix table (v-names p))))

  (define (column name column-name) (vector->list (cdr (assoc column-name (dataset-table name)))))

  (test-case "QuickStartExample: 100 x 20 and a numeric response"
    (define-values (x y) (quick-start-example))
    (check-x x "QuickStartExample" 100 20)
    (check-equal? y (column "QuickStartExample" "y"))
    (check-true (andmap flonum? y)))

  (test-case "BinomialExample: 100 x 30 and 0/1 labels"
    (define-values (x y) (binomial-example))
    (check-x x "BinomialExample" 100 30)
    (check-equal? (remove-duplicates (sort y <)) '(0 1))
    (check-equal? (map exact->inexact y) (column "BinomialExample" "y")))

  (test-case "MultinomialExample: 500 x 30 and R's classes 1, 2, 3 as 0, 1, 2"
    (define-values (x y) (multinomial-example))
    (check-x x "MultinomialExample" 500 30)
    (check-equal? (remove-duplicates (sort y <)) '(0 1 2))
    (check-equal? (map (lambda (k) (exact->inexact (add1 k))) y) (column "MultinomialExample" "y")))

  (test-case "PoissonExample: 500 x 20 and counts"
    (define-values (x y) (poisson-example))
    (check-x x "PoissonExample" 500 20)
    (check-true (andmap exact-nonnegative-integer? y))
    (check-equal? (map exact->inexact y) (column "PoissonExample" "y")))

  (test-case "CoxExample: 1000 x 30, times and statuses"
    (define-values (x time status) (cox-example))
    (check-x x "CoxExample" 1000 30)
    (check-equal? time (column "CoxExample" "time"))
    (check-equal? (map exact->inexact status) (column "CoxExample" "status"))
    (check-equal? (remove-duplicates (sort status <)) '(0 1)))

  (test-case "MultiGaussianExample: 100 x 20 and four responses, y1 ... y4"
    (define-values (x y) (multi-gaussian-example))
    (check-x x "MultiGaussianExample" 100 20)
    (check-equal? (design-matrix-column-names y) '("y1" "y2" "y3" "y4"))
    (check-equal? y (table->design-matrix (dataset-table "MultiGaussianExample")
                                          '("y1" "y2" "y3" "y4"))))

  (test-case "SparseExample: 100 x 20, dense, mostly zeros"
    (define-values (x y) (sparse-example))
    (check-x x "SparseExample" 100 20)
    (check-equal? (length y) 100)
    (define zeros (for*/sum ([row (in-list (design-matrix->rows x))] [v (in-list row)])
                    (if (zero? v) 1 0)))
    (check-true (> zeros 1500)))

  (test-case "a loader reads its file once"
    (define-values (x1 y1) (quick-start-example))
    (define-values (x2 y2) (quick-start-example))
    (check-eq? x1 x2)
    (check-eq? y1 y2))

  (test-case "each loader's values are its family's fitter's arguments"
    (define (fits? fit loader) (call-with-values loader (lambda args (apply fit args))))
    (check-pred elnet-result? (fits? (lambda (x y) (lasso x y #:lambda 0.1)) quick-start-example))
    (check-pred logistic-result?
                (fits? (lambda (x y) (logistic-fit x y #:lambda 0.05)) binomial-example))
    (check-pred multinomial-result?
                (fits? (lambda (x y) (multinomial-fit x y #:lambda 0.05)) multinomial-example))
    (check-pred poisson-result? (fits? (lambda (x y) (poisson-fit x y #:lambda 1)) poisson-example))
    (check-pred cox-result? (fits? (lambda (x t s) (cox-fit x t s #:lambda 0.05)) cox-example))
    (check-pred mgaussian-result?
                (fits? (lambda (x y) (mgaussian-fit x y #:lambda 0.1)) multi-gaussian-example))
    (check-pred elnet-result? (fits? (lambda (x y) (lasso x y #:lambda 0.1)) sparse-example)))

  (test-case "mtcars: R's eleven columns of 32 cars, each a vector"
    (check-equal? (table-column-names mtcars)
                  '("mpg" "cyl" "disp" "hp" "drat" "wt" "qsec" "vs" "am" "gear" "carb"))
    (check-true (andmap (lambda (column) (vector? (cdr column))) mtcars))
    (check-equal? (vector-length (cdr (assoc "mpg" mtcars))) 32)
    (check-equal? (take (column "mtcars" "wt") 3) '(2.62 2.875 2.32))
    (check-equal? mtcars (dataset-table "mtcars")))

  (test-case "iris: four measurements and the species as strings, each a vector"
    (check-equal? (table-column-names iris)
                  '("Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width" "Species"))
    (check-true (andmap (lambda (column) (vector? (cdr column))) iris))
    (check-equal? (remove-duplicates (column "iris" "Species"))
                  '("setosa" "versicolor" "virginica"))
    (check-equal? (vector-length (cdr (assoc "Species" iris))) 150))

  (test-case "mtcars and iris are shared, so their columns cannot be changed"
    (for* ([table (in-list (list mtcars iris))]
           [column (in-list table)])
      (check-true (immutable? (cdr column)) (car column)))
    (check-exn exn:fail:contract? (lambda () (vector-set! (cdr (assoc "mpg" mtcars)) 0 1000.0)))
    (check-exn exn:fail:contract? (lambda () (vector-set! (cdr (assoc "Species" iris)) 0 "rose")))
    (define fresh (dataset-table "mtcars"))
    (check-false (immutable? (cdr (assoc "mpg" fresh))))
    (vector-set! (cdr (assoc "mpg" fresh)) 0 1000.0)
    (check-eqv? (vector-ref (cdr (assoc "mpg" mtcars)) 0) 21.0)))
