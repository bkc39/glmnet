#lang racket/base

;; glmnet/data/polars. The frames are built with Polars' own `series`, not
;; with the adapter.

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
           (only-in (submod glmnet/data support) design-matrix-data)
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
    (check-exn (error-matching #rx"^polars->design-matrix: the dataframe has a missing value"
                               #rx"column: \"b\"\n  row: 2$")
               (lambda () (polars->design-matrix gappy '("a" "b"))))
    (check-equal? (design-matrix->rows (polars->design-matrix gappy '("a")))
                  '((1.0) (2.0) (3.0))))

  (test-case "a non-finite value is an error naming its column and row"
    (define bad (dataframe (list (series '(1.0 2.0 3.0) #:name "a")
                                 (series '(1.0 +inf.0 +nan.0) #:name "b"))))
    (check-exn (error-matching #rx"^polars->design-matrix: the dataframe has an element that is not finite"
                               #rx"column: \"b\"\n  row: 1\n  element: [+]inf[.]0$")
               (lambda () (polars->design-matrix bad '("a" "b")))))

  (test-case "extreme values and -0.0 convert bit for bit, both ways"
    (define xs (list -0.0 0.0 5e-324 -5e-324 2.2250738585072014e-308 1.7976931348623157e308
                     -1.7976931348623157e308 0.1 (/ 1.0 3.0)))
    (define df (dataframe (list (series xs #:name "x") (series (reverse xs) #:name "y"))))
    (define dm (polars->design-matrix df '(y x)))
    (check-equal? dm (rows->design-matrix (map list (reverse xs) xs) #:column-names '("y" "x")))
    (check-equal? (polars->design-matrix (design-matrix->polars dm) '("y" "x")) dm)
    (check-eqv? (car (series->list (ref (design-matrix->polars dm) "x"))) -0.0))

  (test-case "the design matrix does not share memory with the dataframe"
    (define df (dataframe (list (series '(1.0 2.0 3.0) #:name "a")
                                (series '(4 5 6) #:name "b"))))
    (define dm (polars->design-matrix df '("a" "b")))
    (f64vector-set! (design-matrix-data dm) 0 99.0)
    (f64vector-set! (design-matrix-data dm) 3 99.0)
    (check-equal? (series->list (ref df "a")) '(1.0 2.0 3.0))
    (check-equal? (series->list (ref df "b")) '(4 5 6))
    (check-equal? (design-matrix->rows (polars->design-matrix df '("a" "b")))
                  '((1.0 4.0) (2.0 5.0) (3.0 6.0))))

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

  (test-case "an unnamed design matrix gets R's column names, V1 to Vn"
    (define dm (columns->design-matrix '((1 2) (3 4) (5 6))))
    (define df (design-matrix->polars dm))
    (check-equal? (column-names df) '("V1" "V2" "V3"))
    (check-equal? (height df) 2))

  (test-case "#:column-names names the columns, and is checked by contract"
    (define dm (rows->design-matrix '((1 2) (3 4)) #:column-names '("a" "b")))
    (check-equal? (column-names (design-matrix->polars dm #:column-names '(x "y"))) '("x" "y"))
    (check-exn (blame-matching #rx"2 distinct column names" #rx"1 names")
               (lambda () (design-matrix->polars dm #:column-names '("x"))))
    (check-exn (blame-matching #rx"\"x\" twice")
               (lambda () (design-matrix->polars dm #:column-names '("x" x))))
    (check-exn (blame-matching #rx"2 distinct column names")
               (lambda () (design-matrix->polars dm #:column-names '(1 2)))))

  ;; --- polars->response -----------------------------------------------------------

  (define (frame-of . columns) (dataframe columns))

  (test-case "a response keeps integers exact and floats as flonums"
    (define df (frame-of (series '(0 1 2) #:name "i")
                         (series '(1 2 3) #:name "u" #:dtype 'uint8)
                         (series '(0.5 1.5 2.5) #:name "f")))
    (check-equal? (polars->response df "i") '(0 1 2))
    (check-equal? (polars->response df 'u) '(1 2 3))
    (check-equal? (polars->response df "f") '(0.5 1.5 2.5)))

  (test-case "a missing or non-finite response element is an error naming column and row"
    (check-exn (error-matching #rx"^polars->response: the dataframe has a missing value"
                               #rx"column: \"y\"\n  row: 1$")
               (lambda () (polars->response (frame-of (series (list 1.0 polars-null) #:name "y"))
                                            "y")))
    (check-exn (error-matching #rx"^polars->response: the dataframe has an element that is not finite"
                               #rx"column: \"y\"\n  row: 2\n  element: [+]nan[.]0$")
               (lambda () (polars->response (frame-of (series '(1.0 2.0 +nan.0) #:name "y")) "y"))))

  (test-case "a response column that is not numeric, unknown, or in no rows breaks the contract"
    (define df (frame-of (series '("a") #:name "s") (series '(1.0) #:name "y")))
    (check-exn (blame-matching #rx"the name of a numeric column" #rx"column \"s\", of dtype string")
               (lambda () (polars->response df "s")))
    (check-exn (blame-matching #rx"\"z\", which is not a column of the dataframe")
               (lambda () (polars->response df "z")))
    (check-exn (blame-matching #rx"at least one row")
               (lambda () (polars->response (head df 0) "y")))
    (check-exn (blame-matching #rx"the name of a numeric column")
               (lambda () (polars->response df 1))))

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

  (test-case "a table refuses missing values, and dtypes it cannot hold by contract"
    (check-exn (error-matching #rx"^polars->table: the dataframe has a missing value"
                               #rx"column: \"s\"\n  row: 0$")
               (lambda ()
                 (polars->table (dataframe (list (series (list polars-null "b") #:name "s"))))))
    (define dated
      (dataframe (list (series '(1 2) #:name "n")
                       (cast (series '(1 2) #:name "d" #:dtype 'int32) 'date))))
    (check-exn (blame-matching #rx"every column of the dataframe must be numeric, boolean"
                               #rx"column \"d\" has dtype date")
               (lambda () (polars->table dated)))
    (check-exn (blame-matching #rx"column \"d\", of dtype date")
               (lambda () (polars->table dated '("n" "d"))))
    (check-equal? (polars->table dated '("n")) '(("n" . #(1 2))))
    (check-exn (blame-matching #rx"at least one row")
               (lambda () (polars->table (head dated 0) '("n")))))

  (test-case "an enum's levels are not kept: the formula front end sorts them as strings"
    (define dose
      (cast (series '("low" "high" "mid" "low" "mid" "high" "low" "mid") #:name "dose")
            '(enum low mid high)))
    (check-equal? (dtype dose) '(enum low mid high))
    (define t (polars->table
               (dataframe (list (series '(1.0 3.2 2.1 0.9 2.0 3.1 1.2 1.9) #:name "y") dose))))
    (check-equal? (cdr (assoc "dose" t)) #(low high mid low mid high low mid))
    (check-equal? (map car (coef (formula-fit (~ y dose) t #:lambda 0)))
                  '("(Intercept)" "doselow" "dosemid")))

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
                               #rx"column: \"x\"\n  row: 1\n  element: \"b\"$")
               (lambda () (table->polars (list (cons "x" '(1 "b"))))))
    (check-exn (error-matching #rx"^table->polars: the table has an element that is not finite\n"
                               #rx"column: \"x\"\n  row: 1\n  element: ")
               (lambda () (table->polars (list (list "x" 0.5 (/ (expt 10 400) 3))))))
    (check-exn (error-matching #rx"not finite" #rx"column: \"y\"\n  row: 0\n")
               (lambda () (table->polars (list (list "y" (/ (expt -10 401) 7) 1/2)))))
    (check-equal? (series->list (ref (table->polars (list (list "x" (/ (expt 10 300) 3)))) "x"))
                  (list (real->double-flonum (/ (expt 10 300) 3))))
    (check-exn (blame-matching #rx"\"x\" twice")
               (lambda () (table->polars (list (cons "x" '(1 2))) '("x" x))))
    (check-exn (blame-matching #rx"\"y\", which is not a column of the table")
               (lambda () (table->polars (list (cons "x" '(1 2))) '("y"))))
    (check-exn (blame-matching #rx"a non-empty list of distinct names of columns of the table")
               (lambda () (table->polars (list (cons "x" '(1 2))) '()))))

  (test-case "integers take int64, or uint64 above its range; no other integer converts"
    (define big (sub1 (expt 2 64)))
    (define t (list (list "i" (- (expt 2 63)) 0 (sub1 (expt 2 63)))
                    (list "u" big (expt 2 63) 0)
                    (list "r" big 0.5 1)))
    (define df (table->polars t))
    (check-equal? (map (lambda (name) (dtype (ref df name))) (column-names df))
                  '(int64 uint64 float64))
    (check-equal? (series->list (ref df "u")) (list big (expt 2 63) 0))
    (check-equal? (polars->table df '("i" "u")) (list (cons "i" (list->vector (cdr (assoc "i" t))))
                                                      (cons "u" (list->vector (cdr (assoc "u" t))))))
    (check-equal? (polars->response df "u") (list big (expt 2 63) 0))
    (for ([column (in-list (list (list 1 (expt 2 64))
                                 (list 1 (- -1 (expt 2 63)))
                                 (list -1 (expt 2 63))
                                 (list (expt 2 63) -1)))])
      (check-exn (error-matching #rx"no Polars dtype holds" #rx"column: \"x\"\n  row: 1\n")
                 (lambda () (table->polars (list (cons "x" column))))
                 (format "~a" column)))
    (for ([column (in-list (list (list -1 (expt 2 63) 0.5)
                                 (list 0.5 -1 (expt 2 63))
                                 (list -1 0.5 (expt 2 63))))])
      (check-equal? (dtype (ref (table->polars (list (cons "x" column))) "x")) 'float64
                    (format "~a" column))))

  (test-case "a design matrix with names is a table, and converts"
    (define dm (rows->design-matrix '((1 2) (3 4)) #:column-names '("a" "b")))
    (check-equal? (polars->design-matrix (table->polars dm) '("a" "b")) dm))

  (test-case "a wide table's column list is checked in time linear in its length"
    (define (wide-table nc)
      (for/list ([j (in-range nc)]) (cons (format "x~a" j) (vector j (+ j 0.5)))))
    (define (elapsed-ms t names)
      (collect-garbage)
      (define t0 (current-inexact-monotonic-milliseconds))
      (table->polars t names)
      (- (current-inexact-monotonic-milliseconds) t0))
    (define small (wide-table 10000))
    (define large (wide-table 40000))
    (define small-names (map car small))
    (define large-names (map car large))
    (define-values (small-ms large-ms)
      (for/fold ([small-ms +inf.0] [large-ms +inf.0]) ([k (in-range 3)])
        (values (min small-ms (elapsed-ms small small-names))
                (min large-ms (elapsed-ms large large-names)))))
    (check-equal? (series->list (ref (table->polars large large-names) "x39999"))
                  (list 39999.0 39999.5))
    (check < (/ large-ms small-ms) 8
           (format "~a ms at 40000 columns, ~a ms at 10000" large-ms small-ms)))

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
  (define (response df name) (polars->response df name))

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
