#lang racket/base

;; The Gaussian family on the user's data directly (#74): every entry point and
;; every format gives what the explicit adapter route gives, fits from named
;; data predict from named data by column name, bad data is an error naming
;; its column and row, and (require glmnet) still loads neither Polars nor
;; math/matrix.

(module+ test
  (require rackunit
           racket/vector
           (only-in racket/contract exn:fail:contract:blame?)
           (only-in math/array list->array)
           (only-in math/matrix list*->matrix)
           (only-in polars dataframe series polars-null)
           glmnet
           glmnet/data/math
           glmnet/data/nested
           glmnet/data/polars
           (only-in glmnet/datasets mtcars))

  ;; --- data -----------------------------------------------------------------------

  (define predictors '("wt" "hp" "qsec"))
  (define (column name) (vector->list (cdr (assoc name mtcars))))
  (define rows (apply map list (map column predictors)))
  (define mpg (column "mpg"))

  ;; mtcars as a dataframe, built with Polars' own series, its columns in the
  ;; table's order.
  (define cars
    (dataframe (for/list ([entry (in-list mtcars)])
                 (series (cdr entry) #:name (car entry)))))
  (define cars-hash (for/hash ([entry (in-list mtcars)]) (values (car entry) (cdr entry))))
  ;; Another car, its columns in another order, with an extra text column and
  ;; no response.
  (define new-cars
    (dataframe (list (series '("Volvo 142E" "Lotus Europa") #:name "model")
                     (series '(110.0 113.0) #:name "hp")
                     (series '(18.6 16.9) #:name "qsec")
                     (series '(2.78 1.513) #:name "wt"))))
  (define new-rows '((2.78 110.0 18.6) (1.513 113.0 16.9)))
  (define new-table (list (cons "qsec" '(18.6 16.9)) (cons 'wt #(2.78 1.513)) (cons "hp" '(110 113))))

  (define folds (for/list ([i (in-range 32)]) (modulo i 4)))

  ;; Each Gaussian entry point at fixed options, as a procedure of X, y and
  ;; #:predictors.
  (define entry-points
    (list (cons 'ols (lambda (X y [predictors #f]) (ols X y #:predictors predictors)))
          (cons 'ridge (lambda (X y [predictors #f])
                         (ridge X y #:predictors predictors #:lambda 0.5)))
          (cons 'lasso (lambda (X y [predictors #f])
                         (lasso X y #:predictors predictors #:lambda 0.1)))
          (cons 'elastic-net (lambda (X y [predictors #f])
                               (elastic-net X y #:predictors predictors #:alpha 0.5 #:lambda 0.2)))
          (cons 'elnet-fit (lambda (X y [predictors #f])
                             (elnet-fit X y #:predictors predictors #:lambda 0.3)))
          (cons 'elnet-path (lambda (X y [predictors #f]) (elnet-path X y #:predictors predictors)))
          (cons 'elnet-cv (lambda (X y [predictors #f])
                            (elnet-cv X y #:predictors predictors #:fold-ids folds)))))

  (define (check-same-model direct explicit [new-data new-rows] [direct-new-data new-data])
    (check-equal? direct explicit)
    (check-equal? (coef direct) (coef explicit))
    (check-equal? (deviance-ratio direct) (deviance-ratio explicit))
    (check-equal? (predict direct direct-new-data) (predict explicit new-data)))

  ;; --- every entry point and format matches the explicit route ------------------------

  (test-case "a dataframe, its response and predictors named"
    (define dm (polars->design-matrix cars predictors))
    (define y (polars->response cars "mpg"))
    (for ([entry (in-list entry-points)])
      (define fit (cdr entry))
      (check-same-model (fit cars "mpg" predictors) (fit dm y)
                        new-rows new-cars)
      (check-same-model (fit cars 'mpg '(wt hp qsec)) (fit dm y))))

  (test-case "tables: association lists and hashes"
    (define dm (table->design-matrix mtcars predictors))
    (for* ([entry (in-list entry-points)]
           [table (in-list (list mtcars cars-hash))])
      (define fit (cdr entry))
      (check-same-model (fit table "mpg" predictors) (fit dm mpg)
                        new-rows new-table)))

  (test-case "unnamed data: nested rows, math matrices, arrays and series"
    (define dm (nested->design-matrix rows))
    (define M (list*->matrix rows))
    (for ([entry (in-list entry-points)])
      (define fit (cdr entry))
      (define explicit (fit dm mpg))
      (check-same-model (fit rows mpg) explicit)
      (check-same-model (fit (list->vector (map list->vector rows)) (list->vector mpg)) explicit)
      (check-same-model (fit M mpg) explicit new-rows (list*->matrix new-rows))
      (check-same-model (fit (matrix->design-matrix M) (array->response (list->array mpg))) explicit)
      (check-same-model (fit M (list->array mpg)) explicit)
      (check-same-model (fit rows (series mpg #:name "mpg")) explicit)))

  ;; --- prediction ------------------------------------------------------------------------

  (test-case "a fit from named data predicts named data by name, and unnamed data by position"
    (define fit (lasso cars "mpg" #:predictors predictors #:lambda 0.1))
    (define expected (predict fit new-rows))
    (check-equal? (predict fit new-cars) expected)
    (check-equal? (predict fit new-table) expected)
    (check-equal? (elnet-predict fit new-cars) expected)
    (check-equal? (predict fit (hash "wt" '(2.78 1.513) "hp" '(110 113) "qsec" '(18.6 16.9)))
                  expected)
    (check-equal? (predict fit cars) (predict fit rows))
    (check-equal? (predict (lasso mtcars "mpg" #:predictors predictors #:lambda 0.1) new-cars)
                  expected))

  (test-case "coef keeps R's layout and a fit from named data is the plain result struct"
    (define fit (ols cars "mpg" #:predictors '("wt" "hp")))
    (check-pred elnet-result? fit)
    (check-equal? (vector-length (coef fit)) 3)
    (check-false (glmnet-model-predictor-names fit))
    (check-equal? (vector-append (vector (elnet-result-intercept fit)) (elnet-result-coefficients fit))
                  (coef fit))
    (define p (elnet-path cars "mpg" #:predictors predictors))
    (check-pred glmnet-path? p)
    (check-equal? (predict p new-cars #:lambda 0.5) (predict p new-rows #:lambda 0.5))
    (define cv (elnet-cv cars "mpg" #:predictors predictors #:fold-ids folds))
    (check-equal? (predict cv new-cars #:lambda 'lambda-min) (predict cv new-rows #:lambda 'lambda-min)))

  ;; --- errors -------------------------------------------------------------------------------

  (define ((blame-matching . patterns) e)
    (and (exn:fail:contract:blame? e)
         (for/and ([p (in-list (cons #rx"blaming: [^\n]*direct-data-test[.]rkt" patterns))])
           (regexp-match? p (exn-message e)))))

  (define ((error-matching . patterns) e)
    (and (exn:fail:contract? e)
         (not (exn:fail:contract:blame? e))
         (for/and ([p (in-list patterns)])
           (regexp-match? p (exn-message e)))))

  (test-case "named data needs #:predictors, and unnamed data takes none"
    (check-exn (blame-matching #rx"^ols: contract violation"
                               #rx"#:predictors is required when X is a dataframe")
               (lambda () (ols cars "mpg")))
    (check-exn (blame-matching #rx"#:predictors is required when X is a table")
               (lambda () (lasso mtcars "mpg" #:lambda 0.1)))
    (check-exn (blame-matching #rx"expected: a non-empty list of distinct names of numeric columns of the table"
                               #rx"given: #f")
               (lambda () (elnet-cv mtcars "mpg" #:predictors #f)))
    (check-exn (blame-matching #rx"expected: #f, since X is not named data"
                               #rx"in: the predictors argument of")
               (lambda () (ols rows mpg #:predictors '("wt")))))

  (test-case "unknown names, the response among the predictors, and repeated names"
    (check-exn (blame-matching #rx"expected: the name of a numeric column of the dataframe"
                               #rx"given: \"mpgg\", which is not a column of the dataframe"
                               #rx"in: the y argument of")
               (lambda () (ols cars "mpgg" #:predictors predictors)))
    (check-exn (blame-matching #rx"given: \"weight\", which is not a column of the table"
                               #rx"in: the predictors argument of")
               (lambda () (ridge mtcars "mpg" #:predictors '("wt" "weight") #:lambda 1.0)))
    (check-exn (blame-matching #rx"given: \"mpg\", which is the response")
               (lambda () (elnet-path cars "mpg" #:predictors '("wt" mpg))))
    (check-exn (blame-matching #rx"given: \"wt\" twice")
               (lambda () (ols cars "mpg" #:predictors '("wt" wt))))
    (check-exn (blame-matching #rx"expected: the name of a numeric column of the table"
                               #rx"given: '\\(\"mpg\"\\)")
               (lambda () (ols mtcars '("mpg") #:predictors predictors))))

  (define text-cars
    (dataframe (list (series '("a" "b" "c" "d") #:name "model")
                     (series '(1.0 2.0 3.0 5.0) #:name "x")
                     (series '(2.0 1.0 4.0 3.0) #:name "z")
                     (series '(1.0 3.0 2.0 5.0) #:name "y"))))

  (test-case "a text column is never read as numbers"
    (check-exn (blame-matching #rx"given: \"model\", a column of the dataframe that is not numeric")
               (lambda () (ols text-cars "y" #:predictors '("x" "model"))))
    (check-exn (blame-matching #rx"given: \"model\", a column of the dataframe that is not numeric"
                               #rx"in: the y argument of")
               (lambda () (ols text-cars "model" #:predictors '("x"))))
    (define table (list (cons "model" '("a" "b" "c" "d")) (cons "x" '(1 2 3 5)) (cons "y" '(1 3 2 5))))
    (check-exn (error-matching #rx"^ols: the table has an element that is not a real number"
                               #rx"column: \"model\"\n  row: 0")
               (lambda () (ols table "y" #:predictors '("x" "model"))))
    (check-exn (error-matching #rx"^ols: the table has an element that is not a real number"
                               #rx"column: \"model\"\n  row: 0")
               (lambda () (ols table "model" #:predictors '("x")))))

  (test-case "missing values and non-finite numbers name their column and row"
    (define (frame x y)
      (dataframe (list (series x #:name "x") (series '(2.0 1.0 4.0 3.0) #:name "z")
                       (series y #:name "y"))))
    (define good '(1.0 2.0 3.0 5.0))
    (check-exn (error-matching #rx"^lasso: the dataframe has a missing value"
                               #rx"column: \"x\"\n  row: 2$")
               (lambda () (lasso (frame (list 1.0 2.0 polars-null 5.0) good) "y"
                                 #:predictors '("x" "z") #:lambda 0.1)))
    (check-exn (error-matching #rx"^lasso: the dataframe has a missing value"
                               #rx"column: \"y\"\n  row: 1$")
               (lambda () (lasso (frame good (list 1.0 polars-null 3.0 4.0)) "y"
                                 #:predictors '("x" "z") #:lambda 0.1)))
    (check-exn (error-matching #rx"^ols: the dataframe has an element that is not finite"
                               #rx"column: \"x\"\n  row: 1\n  element: [+]nan[.]0$")
               (lambda () (ols (frame '(1.0 +nan.0 3.0 5.0) good) "y" #:predictors '("x" "z"))))
    (check-exn (error-matching #rx"^ols: the dataframe has an element that is not finite"
                               #rx"column: \"y\"\n  row: 3\n  element: [+]inf[.]0$")
               (lambda () (ols (frame good '(1.0 2.0 3.0 +inf.0)) "y" #:predictors '("x" "z"))))
    (check-exn (error-matching #rx"^ols: the table has an element that is not finite"
                               #rx"column: \"y\"\n  row: 2")
               (lambda () (ols (list (cons "x" good) (cons "y" '(1.0 2.0 -inf.0 4.0))) "y"
                               #:predictors '("x"))))
    (check-exn (error-matching #rx"^ols: y has a missing value" #rx"position: 1$")
               (lambda () (ols '((1.0) (2.0) (3.0)) (series (list 1.0 polars-null 2.0) #:name "y"))))
    (check-exn (error-matching #rx"^ols: y has an element that is not finite" #rx"position: 2")
               (lambda () (ols '((1.0) (2.0) (3.0)) (series '(1.0 2.0 +nan.0) #:name "y"))))
    (check-exn (error-matching #rx"^ols: X has an element that is not finite"
                               #rx"column: 1\n  row: 0")
               (lambda () (ols (list*->matrix '((1.0 +inf.0) (2.0 1.0) (3.0 2.0))) '(1 2 3)))))

  (test-case "dimensions and orientation"
    (check-exn (error-matching #rx"^ols: y does not have one entry per row of X"
                               #rx"length of y: 3\n  rows of X: 2\n  columns of X: 3"
                               #rx"hint: rows are observations; is X transposed[?]")
               (lambda () (ols '((1 2 3) (4 5 7)) '(1 2 3))))
    (check-exn (error-matching #rx"^ols: y does not have one entry per row of X"
                               #rx"length of y: 4\n  rows of X: 3$")
               (lambda () (ols '((1 2) (4 5) (7 9)) '(1 2 3 4))))
    (check-exn (error-matching #rx"^ols: the table's columns have different lengths"
                               #rx"column: \"y\"")
               (lambda () (ols (list (cons "x" '(1 2 3)) (cons "y" '(1 2))) "y" #:predictors '("x"))))
    (define fit (ols rows mpg))
    (check-exn (error-matching #rx"^predict: X does not have one column per coefficient"
                               #rx"hint: rows are observations; is X transposed[?]")
               (lambda () (predict fit '((1 2) (3 4) (5 6)))))
    (check-exn (error-matching #rx"^elnet-predict: X does not have one column per coefficient")
               (lambda () (elnet-predict fit '((1 2))))))

  (test-case "prediction from named data needs a fit from named data, and its columns"
    (define unnamed (ols rows mpg))
    (check-exn (error-matching #rx"^predict: the model's predictors are not named, so X must be a design matrix, rows or a matrix")
               (lambda () (predict unnamed new-cars)))
    (check-exn (error-matching #rx"^elnet-predict: the model's predictors are not named")
               (lambda () (elnet-predict unnamed new-table)))
    (define named (ols cars "mpg" #:predictors predictors))
    (check-exn (error-matching #rx"^predict: the dataframe has no column with this name"
                               #rx"column: \"qsec\"")
               (lambda () (predict named (dataframe (list (series '(1.0) #:name "wt")
                                                          (series '(1.0) #:name "hp"))))))
    (check-exn (error-matching #rx"^predict: the table has no column with this name"
                               #rx"column: \"wt\"")
               (lambda () (predict named (list (cons "hp" '(1)) (cons "qsec" '(2))))))
    (check-exn (error-matching #rx"^predict: the dataframe has a column that is not numeric"
                               #rx"column: \"wt\"\n  dtype: 'string")
               (lambda () (predict named (dataframe (list (series '("1") #:name "wt")
                                                          (series '(1.0) #:name "hp")
                                                          (series '(1.0) #:name "qsec")))))))

  ;; --- loading ---------------------------------------------------------------------------

  (test-case "(require glmnet) and a fit from plain data load neither Polars nor math/matrix"
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (namespace-require 'racket/base)
      (namespace-require 'glmnet)
      (eval '(ols '((1 2) (3 4) (5 7)) '(1 2 4)))
      (eval '(predict (lasso (list (cons "x" '(1 2 4)) (cons "y" '(1 2 3))) "y"
                             #:predictors '("x") #:lambda 0.1)
                      (hash "x" '(3))))
      (for ([mod (in-list '(polars glmnet/data/polars math/array math/matrix glmnet/data/math
                                   typed/racket/base plot glmnet/plot))])
        (check-false (module-declared? mod #f) (format "~a is declared" mod))))))
