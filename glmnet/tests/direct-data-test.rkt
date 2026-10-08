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

  (define ((blame-matching . patterns) e)
    (and (exn:fail:contract:blame? e)
         (for/and ([p (in-list (cons #rx"blaming: [^\n]*direct-data-test[.]rkt" patterns))])
           (regexp-match? p (exn-message e)))))

  (define ((error-matching . patterns) e)
    (and (exn:fail:contract? e)
         (not (exn:fail:contract:blame? e))
         (for/and ([p (in-list patterns)])
           (regexp-match? p (exn-message e)))))

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
    (check-equal? (glmnet-model-predictor-names fit) '("wt" "hp"))
    (check-false (glmnet-model-predictor-names (ols rows mpg)))
    (check-false (glmnet-model-class-labels fit))
    (check-equal? (vector-append (vector (elnet-result-intercept fit)) (elnet-result-coefficients fit))
                  (coef fit))
    (define p (elnet-path cars "mpg" #:predictors predictors))
    (check-pred glmnet-path? p)
    (check-equal? (predict p new-cars #:lambda 0.5) (predict p new-rows #:lambda 0.5))
    (define cv (elnet-cv cars "mpg" #:predictors predictors #:fold-ids folds))
    (check-equal? (predict cv new-cars #:lambda 'lambda-min) (predict cv new-rows #:lambda 'lambda-min)))

  (test-case "a fit from named data reads a design matrix with column names by name"
    (define fit (ols cars "mpg" #:predictors predictors))
    (define expected (predict fit new-rows))
    (check-equal? (predict fit (polars->design-matrix new-cars '("qsec" "wt" "hp"))) expected)
    (check-equal? (elnet-predict fit (rows->design-matrix '((18.6 110.0 2.78) (16.9 113.0 1.513))
                                                          #:column-names '(qsec hp wt)))
                  expected)
    (check-equal? (predict fit (nested->design-matrix new-rows)) expected)
    (check-exn (error-matching #rx"^predict: the table has no column with this name"
                               #rx"column: \"qsec\"")
               (lambda () (predict fit (rows->design-matrix '((2.78 110.0 18.6))
                                                            #:column-names '(wt hp sec))))))

  (test-case "the path inside a cross-validated fit from named data predicts by name too"
    (define cv (elnet-cv cars "mpg" #:predictors predictors #:fold-ids folds))
    (define p (glmnet-cv-path cv))
    (check-equal? p (glmnet-cv-path (elnet-cv rows mpg #:fold-ids folds)))
    (check-equal? (predict p new-cars #:lambda 0.5) (predict p new-rows #:lambda 0.5))
    (check-equal? (predict p new-table #:lambda 0.5) (predict p new-rows #:lambda 0.5)))

  (test-case "a struct-copy of a fit from named data is a fit from unnamed data"
    (define fit (ols cars "mpg" #:predictors predictors))
    (define copy (struct-copy elnet-result fit))
    (check-equal? copy fit)
    (check-equal? (predict copy new-rows) (predict fit new-rows))
    (check-exn (error-matching #rx"^predict: the model's predictors are not named")
               (lambda () (predict copy new-cars))))

  ;; --- errors -------------------------------------------------------------------------------

  (test-case "named data needs #:predictors, and unnamed data takes none"
    (check-exn (blame-matching #rx"^ols: contract violation"
                               #rx"#:predictors is required when X is a dataframe")
               (lambda () (ols cars "mpg")))
    (check-exn (blame-matching #rx"#:predictors is required when X is a table")
               (lambda () (lasso mtcars "mpg" #:lambda 0.1)))
    (check-exn (blame-matching #rx"expected: a non-empty list of distinct names of numeric columns of the table"
                               #rx"given: #f")
               (lambda () (elnet-cv mtcars "mpg" #:predictors #f)))
    (check-exn (blame-matching #rx"expected: #f, since X is unnamed data: rows, a matrix or a design matrix, even one whose columns have names"
                               #rx"in: the #:predictors argument of")
               (lambda () (ols rows mpg #:predictors '("wt")))))

  (test-case "a contract error names a fitter's contract by its short signature"
    (define (short-signature e)
      (and (regexp-match? #rx"in: the #:lambda argument of\n *[(]->[*]" (exn-message e))
           (not (regexp-match? #rx"->i|#:pre/desc|predictors-problem" (exn-message e)))))
    (check-exn (blame-matching #rx"expected: [(]>=/c 0[)]\n  given: -1") (lambda () (lasso rows mpg #:lambda -1)))
    (check-exn short-signature (lambda () (lasso rows mpg #:lambda -1)))
    (check-exn (blame-matching #rx"expected: unnamed data [(]a design matrix" #rx"given: 5")
               (lambda () (ols 5 mpg))))

  (test-case "unknown names, the response among the predictors, and repeated names"
    (check-exn (blame-matching #rx"expected: the name of a numeric column of the dataframe"
                               #rx"given: \"mpgg\", which is not a column of the dataframe"
                               #rx"in: the y argument of")
               (lambda () (ols cars "mpgg" #:predictors predictors)))
    (check-exn (blame-matching #rx"given: \"weight\", which is not a column of the table"
                               #rx"in: the #:predictors argument of")
               (lambda () (ridge mtcars "mpg" #:predictors '("wt" "weight") #:lambda 1.0)))
    (check-exn (blame-matching #rx"given: column \"mpg\", which is the response")
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

  (test-case "two columns of a table with one name are an error in the caller's name"
    (check-exn (error-matching #rx"^ols: the table has two columns with the same name"
                               #rx"name: \"x\"")
               (lambda () (ols (list (cons "x" #(1 2 3)) (cons 'x #(2 3 5)) (cons "y" #(1 2 4)))
                               "y" #:predictors '("x"))))
    (check-exn (error-matching #rx"^lasso: the table has two columns with the same name"
                               #rx"name: \"x\"")
               (lambda () (lasso (hash "x" '(1 2 3) 'x '(2 3 5) "y" '(1 2 4)) "y"
                                 #:predictors '("x") #:lambda 0.1))))

  (test-case "the exported contract builders take data as X"
    (check-exn #rx"^response-for/c: contract violation.*given: 1"
               (lambda () (response-for/c 1 real?)))
    (check-exn #rx"^predictors-for/c: contract violation.*given: 1"
               (lambda () (predictors-for/c 1 '(1 2)))))

  (test-case "rows that start with text are rows, unless y names a column"
    (check-exn (error-matching #rx"^ols: X has an element that is not a real number"
                               #rx"column: 0\n  row: 0\n  element: \"a\"")
               (lambda () (ols '(("a" 1 2) ("b" 3 4) ("c" 5 7)) '(1 2 4))))
    (define table '(("x" 1 2 3 5) ("z" 2 1 4 3) ("y" 1 3 2 5)))
    (define fit (ols table "y" #:predictors '("x" "z")))
    (check-equal? fit (ols '((1 2) (2 1) (3 4) (5 3)) '(1 3 2 5)))
    (check-equal? (predict fit '(("z" 1) ("x" 2))) (predict fit '((2 1))))
    (check-exn (error-matching #rx"^predict: the model's predictors are not named")
               (lambda () (predict (ols '((1 2) (2 1) (3 4) (5 3)) '(1 3 2 5)) table))))

  (test-case "a text column is never read as numbers"
    (check-exn (blame-matching #rx"given: column \"model\", of dtype string")
               (lambda () (ols text-cars "y" #:predictors '("x" "model"))))
    (check-exn (blame-matching #rx"given: column \"model\", of dtype string"
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

  ;; A fresh namespace with glmnet, where `declarations` are required, and the
  ;; forms evaluated there.
  (define (glmnet-namespace declarations)
    (define ns (make-base-empty-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/base)
      (namespace-require 'glmnet)
      (for-each namespace-require declarations))
    ns)

  (define (declared? ns mod)
    (parameterize ([current-namespace ns]) (module-declared? mod #f)))

  ;; Every plain form of data, fitted and predicted from.
  (define plain-fits
    '((define X '((1 2) (3 4) (5 7) (2 2)))
      (define y '(1 2 4 3))
      (predict (ols X y) X)
      (predict (ols (list->vector (map list->vector X)) (list->vector y)) #(#(1 2)))
      (predict (ols (rows->design-matrix X) y) (rows->design-matrix X))
      (define table (list (cons "a" #(1 3 5 2)) (cons "b" #(2 4 7 2)) (cons "y" y)))
      (predict (lasso table "y" #:predictors '("a" "b") #:lambda 0.1) (hash "b" '(1) "a" '(2)))
      (predict (elnet-path '(("a" 1 3 5 2) ("b" 2 4 7 2) ("y" 1 2 4 3)) "y" #:predictors '(a b))
               table #:lambda 0.1)
      (with-handlers ([exn:fail? void]) (ols '(("a" 1 2) ("b" 3 4)) '(1 2)))))

  (test-case "a plain value never consults a library: declared, its adapter stays unloaded"
    (define ns (glmnet-namespace '((for-label polars) math/array)))
    (parameterize ([current-namespace ns])
      (for-each eval plain-fits))
    (check-false (declared? ns 'glmnet/data/polars))
    (check-false (declared? ns 'glmnet/data/math))
    (check-false (declared? ns 'math/matrix))
    ;; Only a value that is no plain form asks, and loads both adapters.
    (parameterize ([current-namespace ns])
      (eval '(with-handlers ([exn:fail:contract? void]) (ols (box 1) '(1 2)))))
    (check-true (declared? ns 'glmnet/data/polars))
    (check-true (declared? ns 'glmnet/data/math)))

  (test-case "a program that requires math/matrix alone fits its matrices"
    (define ns (glmnet-namespace '(math/matrix)))
    (check-false (declared? ns 'math/array))
    (parameterize ([current-namespace ns])
      (check-equal? (eval '(coef (ols (matrix [[1 2] [3 4] [5 7] [2 2]]) '(1 2 4 3))))
                    (coef (ols '((1 2) (3 4) (5 7) (2 2)) '(1 2 4 3))))))

  ;; Formulas on every plain table: an association list, a hash and a design
  ;; matrix with column names, fitted, cross-validated and predicted from.
  (define plain-formulas
    '((define table (list (cons "a" #(1 3 5 2 4 6)) (cons "b" #(2 4 7 2 1 3))
                          (cons "k" #("p" "q" "p" "q" "q" "p")) (cons "y" #(1 2 4 3 5 4))))
      (define m (formula-fit (y . ~ . a * b + (log a) + (factor k)) table #:lambda 0.1))
      (predict m (hash "a" '(2) "b" '(1) "k" '("q")))
      (predict (formula-path (~ y all) (hash "a" '(1 3 5 2) "z" '("p" "q" "p" "q") "y" '(1 2 4 3)))
               (hash "a" '(2) "z" '("q")) #:lambda 0.1)
      (predict (formula-cv (~ k a b) table #:family 'binomial #:fold-ids '(0 1 2 0 1 2) #:nlambda 5)
               table #:type 'class)
      (formula-predictor-names (~ y a (factor k)) table)
      (formula-design-matrix (~ y (: a b))
                             (columns->design-matrix '((1 3 5) (2 4 7) (1 2 4))
                                                     #:column-names '("a" "b" "y")))
      (with-handlers ([exn:fail? void]) (formula-fit (~ y nope) table #:lambda 0.1))))

  (test-case "a formula on a plain table never consults a library, though it is declared"
    (define ns (glmnet-namespace '((for-label polars) math/array)))
    (parameterize ([current-namespace ns])
      (for-each eval plain-formulas))
    (check-false (declared? ns 'glmnet/data/polars))
    (check-false (declared? ns 'glmnet/data/math))
    (check-false (declared? ns 'math/matrix)))

  (test-case "glmnet attached to another namespace reads the dataframes of that namespace"
    (define outer (glmnet-namespace '()))
    (define inner (make-base-namespace))
    (namespace-attach-module outer 'glmnet inner)
    (parameterize ([current-namespace inner])
      (namespace-require 'glmnet)
      (namespace-require '(only polars dataframe series))
      (check-equal?
       (eval '(let* ([df (dataframe (list (series '(1.0 2.0 3.0 5.0) #:name "x")
                                          (series '(1.0 3.0 2.0 5.0) #:name "y")))]
                     [fit (ols df "y" #:predictors '("x"))])
                (list (coef fit) (predict fit df))))
       (eval '(let ([fit (ols '((1.0) (2.0) (3.0) (5.0)) '(1.0 3.0 2.0 5.0))])
                (list (coef fit) (predict fit '((1.0) (2.0) (3.0) (5.0)))))))))

  (test-case "(require glmnet), fits and formulas from plain data load neither Polars nor math"
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (namespace-require 'racket/base)
      (namespace-require 'glmnet)
      (eval '(ols '((1 2) (3 4) (5 7)) '(1 2 4)))
      (eval '(predict (lasso (list (cons "x" '(1 2 4)) (cons "y" '(1 2 3))) "y"
                             #:predictors '("x") #:lambda 0.1)
                      (hash "x" '(3))))
      (eval '(predict (formula-fit (y . ~ . x + (log x)) (list (cons "x" '(1 2 4)) (cons "y" '(1 2 3)))
                                   #:lambda 0.1)
                      (hash "x" '(3))))
      (eval '(formula-path (~ y all) (hash "x" '(1 2 4 3) "z" '("a" "b" "a" "b") "y" '(1 2 3 3))))
      (for ([mod (in-list '(polars glmnet/data/polars math/array math/matrix glmnet/data/math
                                   typed/racket/base plot glmnet/plot))])
        (check-false (module-declared? mod #f) (format "~a is declared" mod))))))
