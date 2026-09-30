#lang racket/base

;; The rkt-polars adapter (#40), glmnet/data/polars. Every family fits,
;; predicts and cross-validates `equal?` from a Polars dataframe and from the
;; same data as lists; formulas fit from a dataframe as from the table it came
;; from; conversions keep values, names and dtypes; a null, a non-numeric
;; column or a non-finite value is an error naming the column and the row; and
;; `(require glmnet)` does not load Polars. The frames are built with Polars'
;; own `series`, not with the adapter.

(module+ test
  (require rackunit
           racket/list
           (only-in racket/contract exn:fail:contract:blame?)
           ffi/vector
           (only-in polars
                    dataframe series ref dtype cast head column-names height series->list
                    polars-null)
           glmnet
           glmnet/data/polars
           (only-in glmnet/datasets mtcars [iris iris-species])
           (file "../private/demo-utils.rkt"))

  ;; A table's columns, in its order, as a dataframe.
  (define (table->frame table)
    (dataframe (for/list ([entry (in-list table)])
                 (series (cdr entry) #:name (car entry)))))

  (define (rows-of table names)
    (apply map list (for/list ([name (in-list names)]) (cdr (assoc name table)))))

  (define (folds n) (for/list ([i (in-range n)]) (modulo i 3)))

  (define ((blame-matching . patterns) e)
    (and (exn:fail:contract:blame? e)
         (for/and ([p (in-list (cons #rx"blaming: [^\n]*polars-test[.]rkt" patterns))])
           (regexp-match? p (exn-message e)))))

  (define ((error-matching . patterns) e)
    (and (exn:fail:contract? e)
         (not (exn:fail:contract:blame? e))
         (for/and ([p (in-list patterns)])
           (regexp-match? p (exn-message e)))))

  ;; --- polars->design-matrix ------------------------------------------------------

  (define mixed
    (dataframe (list (series '(1 2 3) #:name "i")
                     (series '(0.5 1.5 2.5) #:name "f")
                     (series '(4 5 6) #:name "u" #:dtype 'uint16)
                     (series '(7.25 8.5 9.75) #:name "s" #:dtype 'float32)
                     (series '("a" "b" "a") #:name "g")
                     (series '(#t #f #t) #:name "b"))))

  (test-case "numeric columns, in the order given, column-major, with their names"
    (define dm (polars->design-matrix mixed '("f" "i" "u" "s")))
    (check-equal? (design-matrix-nrows dm) 3)
    (check-equal? (design-matrix-ncols dm) 4)
    (check-equal? (design-matrix-column-names dm) '("f" "i" "u" "s"))
    (check-equal? (design-matrix->rows dm)
                  '((0.5 1.0 4.0 7.25) (1.5 2.0 5.0 8.5) (2.5 3.0 6.0 9.75)))
    (check-equal? (f64vector->list (design-matrix->f64vector dm))
                  '(0.5 1.5 2.5 1.0 2.0 3.0 4.0 5.0 6.0 7.25 8.5 9.75)))

  (test-case "names are strings or symbols, compared as strings"
    (check-equal? (polars->design-matrix mixed '(i f))
                  (rows->design-matrix '((1 0.5) (2 1.5) (3 2.5)) #:column-names '("i" "f"))))

  (test-case "a null is an error naming its column and row"
    (define gappy (dataframe (list (series '(1.0 2.0 3.0) #:name "a")
                                   (series (list 1 2 polars-null) #:name "b"))))
    (check-exn (error-matching #rx"^polars->design-matrix: the dataframe has a null"
                               #rx"column: \"b\"" #rx"row: 2")
               (lambda () (polars->design-matrix gappy '("a" "b"))))
    (check-equal? (design-matrix->rows (polars->design-matrix gappy '("a")))
                  '((1.0) (2.0) (3.0))))

  (test-case "a non-finite value is an error naming its column and row"
    (define bad (dataframe (list (series '(1.0 2.0 3.0) #:name "a")
                                 (series '(1.0 +inf.0 +nan.0) #:name "b"))))
    (check-exn (error-matching #rx"^polars->design-matrix: the dataframe has an element that is not finite"
                               #rx"column: \"b\"" #rx"row: 1" #rx"element: [+]inf[.]0")
               (lambda () (polars->design-matrix bad '("a" "b")))))

  (test-case "a column that is not numeric breaks the contract, naming it and its dtype"
    (check-exn (blame-matching #rx"numeric columns" #rx"column \"g\", of dtype string")
               (lambda () (polars->design-matrix mixed '("i" "g"))))
    (check-exn (blame-matching #rx"column \"b\", of dtype boolean")
               (lambda () (polars->design-matrix mixed '("b")))))

  (test-case "unknown, repeated and missing columns break the contract"
    (check-exn (blame-matching #rx"\"z\", which is not a column of the dataframe")
               (lambda () (polars->design-matrix mixed '("i" "z"))))
    (check-exn (blame-matching #rx"\"i\" twice")
               (lambda () (polars->design-matrix mixed '("i" i))))
    (check-exn (blame-matching #rx"a non-empty list")
               (lambda () (polars->design-matrix mixed '())))
    (check-exn (blame-matching #rx"at least one row")
               (lambda ()
                 (polars->design-matrix (head mixed 0) '("f")))))

  ;; --- design-matrix->polars ------------------------------------------------------

  (test-case "a design matrix as float64 columns with its names, and back"
    (define dm (rows->design-matrix '((1 2) (3 4) (5 6)) #:column-names '(x "y")))
    (define df (design-matrix->polars dm))
    (check-equal? (column-names df) '("x" "y"))
    (check-equal? (dtype (ref df "x")) 'float64)
    (check-equal? (series->list (ref df "y")) '(2.0 4.0 6.0))
    (check-equal? (polars->design-matrix df '("x" "y"))
                  (rows->design-matrix '((1 2) (3 4) (5 6)) #:column-names '("x" "y"))))

  (test-case "an unnamed design matrix gets Polars' default column names"
    (define df (design-matrix->polars (columns->design-matrix '((1 2) (3 4) (5 6)))))
    (check-equal? (column-names df) '("column_0" "column_1" "column_2"))
    (check-equal? (height df) 2))

  ;; --- polars->response -----------------------------------------------------------

  (test-case "a response keeps integers exact and floats as flonums"
    (check-equal? (polars->response (series '(0 1 2) #:name "y")) '(0 1 2))
    (check-equal? (polars->response (series '(1 2) #:name "y" #:dtype 'uint8)) '(1 2))
    (check-equal? (polars->response (series '(0.5 1.5) #:name "y")) '(0.5 1.5)))

  (test-case "a null or non-finite response element is an error naming the row"
    (check-exn (error-matching #rx"^polars->response: the series has a null"
                               #rx"column: \"y\"" #rx"row: 1")
               (lambda () (polars->response (series (list 1.0 polars-null) #:name "y"))))
    (check-exn (error-matching #rx"not finite" #rx"column: \"y\"" #rx"row: 2")
               (lambda () (polars->response (series '(1.0 2.0 +nan.0) #:name "y")))))

  (test-case "a response that is not numeric, or empty, breaks the contract"
    (check-exn (blame-matching #rx"a numeric series" #rx"\"y\", of dtype string")
               (lambda () (polars->response (series '("a") #:name "y"))))
    (check-exn (blame-matching #rx"at least one element")
               (lambda () (polars->response (head (series '(1) #:name "y") 0)))))

  ;; --- tables ---------------------------------------------------------------------

  (test-case "a dataframe as a table: numbers, strings, booleans, categoricals"
    (define with-cat
      (dataframe (list (series '(1 2) #:name "n")
                       (series '("a" "b") #:name "s")
                       (series '(#t #f) #:name "b")
                       (series '(x y) #:name "c" #:dtype 'categorical))))
    (define t (polars->table with-cat))
    (check-true (table? t))
    (check-equal? t '(("n" . #(1 2)) ("s" . #("a" "b")) ("b" . #(#t #f)) ("c" . #(x y))))
    (check-equal? (polars->table with-cat '(c "n")) '(("c" . #(x y)) ("n" . #(1 2)))))

  (test-case "a table refuses nulls and dtypes it cannot hold"
    (check-exn (error-matching #rx"^polars->table: the dataframe has a null"
                               #rx"column: \"s\"" #rx"row: 0")
               (lambda ()
                 (polars->table (dataframe (list (series (list polars-null "b") #:name "s"))))))
    (define dated
      (dataframe (list (series '(1 2) #:name "n")
                       (cast (series '(1 2) #:name "d" #:dtype 'int32) 'date))))
    (check-exn (error-matching #rx"^polars->table: the dataframe has a column whose dtype a table cannot hold"
                               #rx"column: \"d\"" #rx"dtype: 'date")
               (lambda () (polars->table dated)))
    (check-exn (blame-matching #rx"column \"d\", of dtype date")
               (lambda () (polars->table dated '("n" "d"))))
    (check-equal? (polars->table dated '("n")) '(("n" . #(1 2)))))

  (test-case "a table as a dataframe: dtypes follow the values"
    (define t
      (list (cons "i" '(1 2 3))
            (cons 'r #(1 2.5 1/2))
            (cons "s" '("a" "b" "a"))
            (list "m" "a" 'b "c")
            (cons "c" '(x y x))
            (cons "b" '(#t #f #t))))
    (define df (table->polars t))
    (check-equal? (column-names df) '("i" "r" "s" "m" "c" "b"))
    (check-equal? (map (lambda (name) (dtype (ref df name))) (column-names df))
                  '(int64 float64 string string categorical boolean))
    (check-equal? (series->list (ref df "r")) '(1.0 2.5 0.5))
    (check-equal? (series->list (ref df "m")) '("a" "b" "c"))
    (check-equal? (polars->table df '("i" "s" "c" "b"))
                  '(("i" . #(1 2 3)) ("s" . #("a" "b" "a")) ("c" . #(x y x)) ("b" . #(#t #f #t))))
    (check-equal? (column-names (table->polars t '("b" s))) '("b" "s")))

  (test-case "a table column Polars cannot hold is an error naming its column and row"
    (check-exn (error-matching #rx"^table->polars: the table has a column whose values no Polars dtype holds"
                               #rx"column: \"x\"" #rx"row: 1" #rx"element: \"b\"")
               (lambda () (table->polars (list (cons "x" '(1 "b"))))))
    (check-exn (error-matching #rx"a column is named twice")
               (lambda () (table->polars (list (cons "x" '(1 2))) '("x" x))))
    (check-exn (error-matching #rx"no column with this name")
               (lambda () (table->polars (list (cons "x" '(1 2))) '("y")))))

  (test-case "a design matrix with names is a table, and converts"
    (define dm (rows->design-matrix '((1 2) (3 4)) #:column-names '("a" "b")))
    (check-equal? (polars->design-matrix (table->polars dm) '("a" "b")) dm))

  ;; --- fits equal to the list path -------------------------------------------------

  (define-values (Xg yg) (load-longley))
  (define-values (Xb yb) (load-wdbc))
  (define-values (Xm ym) (load-iris))
  (define-values (Xc tc sc) (load-veteran))
  (define-values (Xp yp) (load-warpbreaks))
  (define-values (Xr Yr) (load-linnerud))

  (define longley (table->frame (load-table "longley")))
  (define wdbc (table->frame (load-table "wdbc")))
  (define iris (table->frame (load-table "iris")))
  (define veteran (table->frame (load-table "veteran")))
  (define warpbreaks (table->frame (load-table "warpbreaks")))
  (define linnerud (table->frame (load-table "linnerud")))

  (define (predictors df k) (polars->design-matrix df (take (column-names df) k)))
  (define (response df name) (polars->response (ref df name)))

  (test-case "Gaussian: fit, path, cv, predict and coef"
    (define X (predictors longley 6))
    (define y (response longley "Employed"))
    (check-equal? y yg)
    (check-equal? (elnet-fit X y #:lambda 0.1 #:alpha 0.5) (elnet-fit Xg yg #:lambda 0.1 #:alpha 0.5))
    (check-equal? (ols X y) (ols Xg yg))
    (check-equal? (elnet-path X y) (elnet-path Xg yg))
    (define fold-ids (folds (length yg)))
    (check-equal? (elnet-cv X y #:fold-ids fold-ids) (elnet-cv Xg yg #:fold-ids fold-ids))
    (define fit (lasso Xg yg #:lambda 0.1))
    (check-equal? (elnet-predict fit X) (elnet-predict fit Xg))
    (check-equal? (predict fit X) (predict fit Xg))
    (check-equal? (coef (lasso X y #:lambda 0.1)) (coef fit)))

  (test-case "binomial: fit, path, cv and predictions"
    (define X (polars->design-matrix wdbc (cdr (column-names wdbc))))
    (define y (response wdbc "diagnosis"))
    (check-equal? y yb)
    (check-equal? (logistic-fit X y #:lambda 0.02) (logistic-fit Xb yb #:lambda 0.02))
    (check-equal? (logistic-path X y) (logistic-path Xb yb))
    (define fold-ids (folds (length yb)))
    (check-equal? (logistic-cv X y #:fold-ids fold-ids #:nlambda 10)
                  (logistic-cv Xb yb #:fold-ids fold-ids #:nlambda 10))
    (define fit (logistic-fit Xb yb #:lambda 0.02))
    (check-equal? (logistic-predict-proba fit X) (logistic-predict-proba fit Xb))
    (check-equal? (logistic-predict fit X) (logistic-predict fit Xb)))

  (test-case "multinomial: fit, path, cv and predictions"
    (define X (predictors iris 4))
    (define y (response iris "class"))
    (check-equal? y ym)
    (check-equal? (multinomial-fit X y #:lambda 0.01) (multinomial-fit Xm ym #:lambda 0.01))
    (check-equal? (multinomial-path X y) (multinomial-path Xm ym))
    (define fold-ids (folds (length ym)))
    (check-equal? (multinomial-cv X y #:fold-ids fold-ids #:nlambda 10)
                  (multinomial-cv Xm ym #:fold-ids fold-ids #:nlambda 10))
    (define fit (multinomial-fit Xm ym #:lambda 0.01))
    (check-equal? (multinomial-predict-proba fit X) (multinomial-predict-proba fit Xm))
    (check-equal? (multinomial-predict fit X) (multinomial-predict fit Xm)))

  (test-case "Cox: fit, path, cv and predictions"
    (define X (predictors veteran 5))
    (define times (response veteran "time"))
    (define status (response veteran "status"))
    (check-equal? (cox-fit X times status #:lambda 0.05) (cox-fit Xc tc sc #:lambda 0.05))
    (check-equal? (cox-path X times status) (cox-path Xc tc sc))
    (define fold-ids (folds (length tc)))
    (check-equal? (cox-cv X times status #:fold-ids fold-ids #:nlambda 10)
                  (cox-cv Xc tc sc #:fold-ids fold-ids #:nlambda 10))
    (define fit (cox-fit Xc tc sc #:lambda 0.05))
    (check-equal? (cox-linear-predictor fit X) (cox-linear-predictor fit Xc))
    (check-equal? (cox-relative-risk fit X) (cox-relative-risk fit Xc)))

  (test-case "Poisson: fit, path, cv and predictions"
    (define X (predictors warpbreaks 3))
    (define y (response warpbreaks "breaks"))
    (check-equal? (poisson-fit X y #:lambda 0.1) (poisson-fit Xp yp #:lambda 0.1))
    (check-equal? (poisson-path X y) (poisson-path Xp yp))
    (define fold-ids (folds (length yp)))
    (check-equal? (poisson-cv X y #:fold-ids fold-ids) (poisson-cv Xp yp #:fold-ids fold-ids))
    (define fit (poisson-fit Xp yp #:lambda 0.1))
    (check-equal? (poisson-predict-mean fit X) (poisson-predict-mean fit Xp)))

  (test-case "multi-response Gaussian: Y from the dataframe too"
    (define X (predictors linnerud 3))
    (define Y (polars->design-matrix linnerud (drop (column-names linnerud) 3)))
    (check-equal? (mgaussian-fit X Y #:lambda 1.0) (mgaussian-fit Xr Yr #:lambda 1.0))
    (check-equal? (mgaussian-path X Y) (mgaussian-path Xr Yr))
    (define fold-ids (folds (length Yr)))
    (check-equal? (mgaussian-cv X Y #:fold-ids fold-ids) (mgaussian-cv Xr Yr #:fold-ids fold-ids))
    (define fit (mgaussian-fit Xr Yr #:lambda 1.0))
    (check-equal? (mgaussian-predict fit X) (mgaussian-predict fit Xr)))

  ;; --- formulas ------------------------------------------------------------------

  (test-case "a formula fits from a dataframe as from the table it came from"
    (define cars (polars->table (table->frame mtcars)))
    (define f (~ mpg wt hp (factor cyl)))
    (check-equal? (formula-fit f cars #:lambda 0.1) (formula-fit f mtcars #:lambda 0.1))
    (check-equal? (formula-path f cars) (formula-path f mtcars))
    (define fold-ids (folds 32))
    (check-equal? (formula-cv f cars #:fold-ids fold-ids) (formula-cv f mtcars #:fold-ids fold-ids))
    (define model (formula-fit f mtcars #:lambda 0.1))
    (check-equal? (predict model cars) (predict model mtcars)))

  (test-case "a string column is a factor, as in the table"
    (define flowers (polars->table (table->frame iris-species)))
    (check-equal? (dtype (ref (table->frame iris-species) "Species")) 'string)
    (define f (Sepal.Length . ~ . Petal.Width + Species))
    (check-equal? (formula-fit f flowers #:lambda 0.01) (formula-fit f iris-species #:lambda 0.01))
    (define model (formula-fit (~ Species all) flowers #:family 'multinomial #:lambda 0.01))
    (check-equal? model (formula-fit (~ Species all) iris-species #:family 'multinomial #:lambda 0.01))
    (check-equal? (predict model flowers #:type 'class) (predict model iris-species #:type 'class)))

  ;; --- loading --------------------------------------------------------------------

  (define (loads-polars? mod)
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (namespace-require mod)
      (module-declared? 'polars #f)))

  (test-case "(require glmnet) and glmnet/data do not load Polars; the adapter does"
    (check-false (loads-polars? 'glmnet))
    (check-false (loads-polars? 'glmnet/data))
    (check-true (loads-polars? 'glmnet/data/polars))))
