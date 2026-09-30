#lang racket/base

;; The tabular-asa adapter (#63). Its conversions follow the table's row
;; index; a missing value (#f), a non-numeric cell where numbers are read and
;; a non-finite number are errors naming the column and the row. Fitting
;; through a tabular-asa table is `equal?` to fitting the same data as lists,
;; for every family, with the matrix procedures and with formulas. The
;; fixtures are the committed parity datasets, read with tabular-asa's own CSV
;; reader, and R's mtcars and iris. `(require glmnet)` does not load
;; tabular-asa.

(module+ test
  (require rackunit
           (only-in racket/list take)
           racket/runtime-path
           (only-in racket/contract exn:fail:contract:blame?)
           glmnet
           glmnet/data/tabular-asa
           (prefix-in asa: tabular-asa)
           (only-in glmnet/datasets mtcars [iris iris-species])
           (file "../private/demo-utils.rkt"))

  (define-runtime-path data-dir "../private/data")

  (define (read-csv name)
    (call-with-input-file (build-path data-dir (string-append name ".csv")) asa:table-read/csv))

  (define (check-error thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract? e)
            (not (exn:fail:contract:blame? e))
            (for/and ([p (in-list patterns)])
              (regexp-match? p (exn-message e)))))
     thunk))

  ;; A contract violation that blames this module, the caller.
  (define (check-blame thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract:blame? e)
            (for/and ([p (in-list (cons #rx"blaming: [^\n]*tabular-asa-test[.]rkt" patterns))])
              (regexp-match? p (exn-message e)))))
     thunk))

  (define df
    (asa:table-read/columns '((1 2 3 4 5)
                              #(2.0 1.0 4.0 3.0 6.0)
                              (1 4 3 6 5)
                              ("a" "b" "a" "b" "a"))
                            '(x1 x2 y g)))

  ;; --- to glmnet --------------------------------------------------------------------

  (test-case "tabular-asa->design-matrix: the columns named, in that order, with their names"
    (define dm (tabular-asa->design-matrix df '(x2 x1)))
    (check-equal? (design-matrix-column-names dm) '("x2" "x1"))
    (check-equal? (design-matrix->columns dm) '((2.0 1.0 4.0 3.0 6.0) (1.0 2.0 3.0 4.0 5.0)))
    (check-equal? (tabular-asa->design-matrix df '("x1" x2))
                  (columns->design-matrix '((1 2 3 4 5) (2 1 4 3 6)) #:column-names '("x1" "x2"))))

  (test-case "names are compared as strings, in the table as in the column list"
    (define named (asa:table-read/columns '((1 2) (3 4)) '("p" q)))
    (check-equal? (tabular-asa->design-matrix named '(p "q"))
                  (columns->design-matrix '((1 2) (3 4)) #:column-names '("p" "q")))
    (check-equal? (map car (tabular-asa->table named)) '("p" "q")))

  (test-case "conversions follow the table's index: filtered, sorted and reversed tables"
    (define picked (asa:table-reverse (asa:table-filter df (lambda (x) (> x 2)) '(x1))))
    (check-equal? (asa:table-index picked) #(4 3 2))
    (check-equal? (design-matrix->rows (tabular-asa->design-matrix picked '(x1 x2)))
                  '((5.0 6.0) (4.0 3.0) (3.0 4.0)))
    (check-equal? (tabular-asa->response picked 'y) '(5 6 3))
    (check-equal? (tabular-asa->table picked '(g)) (list (cons "g" #("a" "b" "a"))))
    (define sorted (asa:table-sort df '(x2)))
    (check-equal? (tabular-asa->response sorted 'x2) '(1.0 2.0 3.0 4.0 6.0)))

  (test-case "tabular-asa->response: the column's values as they are, in the table's order"
    (check-equal? (tabular-asa->response df 'y) '(1 4 3 6 5))
    (check-equal? (tabular-asa->response df "x2") '(2.0 1.0 4.0 3.0 6.0))
    (check-true (andmap exact-integer? (tabular-asa->response df 'y))))

  (test-case "tabular-asa->table: an association list of vectors, every column by default"
    (define t (tabular-asa->table df))
    (check-true (table? t))
    (check-equal? (table-column-names t) '("x1" "x2" "y" "g"))
    (check-equal? t (list (cons "x1" #(1 2 3 4 5))
                          (cons "x2" #(2.0 1.0 4.0 3.0 6.0))
                          (cons "y" #(1 4 3 6 5))
                          (cons "g" #("a" "b" "a" "b" "a"))))
    (check-equal? (tabular-asa->table df '(g "y"))
                  (list (cons "g" #("a" "b" "a" "b" "a")) (cons "y" #(1 4 3 6 5)))))

  (test-case "the table's vectors are copied, not shared"
    (define data (cdr (assq 'x1 (asa:table-data df))))
    (check-not-eq? (cdr (car (tabular-asa->table df '(x1)))) data))

  ;; --- back to tabular-asa ---------------------------------------------------------------

  (test-case "design-matrix->tabular-asa: symbols for names, flonums for values"
    (define back (design-matrix->tabular-asa (tabular-asa->design-matrix df '(x1 x2))))
    (check-equal? (asa:table-header back) '(x1 x2))
    (check-equal? (asa:table-index back) #(0 1 2 3 4))
    (check-equal? (tabular-asa->response back 'x1) '(1.0 2.0 3.0 4.0 5.0))
    (check-equal? (tabular-asa->design-matrix back '(x1 x2))
                  (tabular-asa->design-matrix df '(x1 x2))))

  (test-case "a design matrix without names gets R's V1, V2, ...; #:column-names overrides"
    (define dm (rows->design-matrix '((1 2) (3 4) (5 6))))
    (check-equal? (asa:table-header (design-matrix->tabular-asa dm)) '(V1 V2))
    (define named (design-matrix->tabular-asa dm #:column-names '("a" b)))
    (check-equal? (asa:table-header named) '(a b))
    (check-equal? (tabular-asa->response named 'b) '(2.0 4.0 6.0)))

  (test-case "table->tabular-asa: every column, or those named, with their values"
    (define t (list (cons "y" '(1 2 3)) (cons 'x #(4.5 5.5 6.5)) (cons "s" '("u" "v" "u"))))
    (define df2 (table->tabular-asa t))
    (check-equal? (asa:table-header df2) '(y x s))
    (check-equal? (tabular-asa->table df2) (list (cons "y" #(1 2 3))
                                                 (cons "x" #(4.5 5.5 6.5))
                                                 (cons "s" #("u" "v" "u"))))
    (check-equal? (asa:table-header (table->tabular-asa t '(s y))) '(s y))
    (check-equal? (asa:table-header (table->tabular-asa (hash "b" '(1) 'a '(2)))) '(a b))
    (define dm (rows->design-matrix '((1 2) (3 4)) #:column-names '(p q)))
    (check-equal? (tabular-asa->table (table->tabular-asa dm '(q)))
                  (list (cons "q" #(2.0 4.0)))))

  (test-case "mtcars and iris survive a round trip"
    (check-equal? (tabular-asa->table (table->tabular-asa mtcars)) mtcars)
    (check-equal? (tabular-asa->table (table->tabular-asa iris-species)) iris-species))

  ;; --- errors ---------------------------------------------------------------------------------

  (define na (asa:table-read/csv (open-input-string "a,b,c\n1,2,x\nna,3,y\n4,+inf.0,z\n")))

  (test-case "a missing value is an error naming the column and the row"
    (check-error (lambda () (tabular-asa->design-matrix na '(a)))
                 #rx"^tabular-asa->design-matrix: the table has a missing value"
                 #rx"column: \"a\"" #rx"row: 1")
    (check-error (lambda () (tabular-asa->response na 'a))
                 #rx"^tabular-asa->response: the table has a missing value" #rx"row: 1")
    (check-error (lambda () (tabular-asa->table na))
                 #rx"^tabular-asa->table: the table has a missing value"
                 #rx"column: \"a\"" #rx"row: 1")
    (check-equal? (tabular-asa->table na '(c)) (list (cons "c" #("x" "y" "z")))))

  (test-case "the row is the position in the table's order, not in its data"
    (define first-missing (asa:table-read/csv (open-input-string "a,b\nna,x\n2,y\n3,z\n")))
    (define reversed (asa:table-reverse first-missing))
    (check-error (lambda () (tabular-asa->design-matrix reversed '(a)))
                 #rx"column: \"a\"" #rx"row: 2")
    (check-error (lambda () (tabular-asa->response reversed 'a)) #rx"row: 2")
    (check-error (lambda () (tabular-asa->table reversed)) #rx"row: 2")
    (check-error (lambda () (tabular-asa->design-matrix (asa:table-tail na 2) '(a)))
                 #rx"row: 0"))

  (test-case "a non-numeric cell or a non-finite number is an error naming the column and the row"
    (check-error (lambda () (tabular-asa->design-matrix na '(c)))
                 #rx"not a real number" #rx"column: \"c\"" #rx"row: 0" #rx"element: \"x\"")
    (check-error (lambda () (tabular-asa->design-matrix na '(b)))
                 #rx"not finite" #rx"column: \"b\"" #rx"row: 2")
    (check-error (lambda () (tabular-asa->response na 'b))
                 #rx"not finite" #rx"column: \"b\"" #rx"row: 2")
    (check-error (lambda () (tabular-asa->response df 'g))
                 #rx"not a real number" #rx"column: \"g\"" #rx"row: 0")
    (check-error (lambda ()
                   (tabular-asa->design-matrix (asa:table-read/columns (list (list (expt 10 400)))
                                                                       '(big))
                                               '(big)))
                 #rx"not finite" #rx"column: \"big\""))

  (test-case "a #f in a glmnet table is an error, since tabular-asa reads it as missing"
    (check-error (lambda () (table->tabular-asa (list (cons "ok" '(#t #f)))))
                 #rx"^table->tabular-asa: the table has an element that is #f"
                 #rx"column: \"ok\"" #rx"row: 1"))

  (test-case "columns of different lengths are an error"
    (check-error (lambda () (table->tabular-asa (list (cons "a" '(1 2)) (cons "b" '(3)))))
                 #rx"different lengths" #rx"column: \"b\""))

  (test-case "arguments are checked by contracts that blame the caller"
    (check-blame (lambda () (tabular-asa->design-matrix df '(x1 z)))
                 #rx"the table has no column named 'z")
    (check-blame (lambda () (tabular-asa->design-matrix df '(x1 "x1")))
                 #rx"the column \"x1\" is named twice")
    (check-blame (lambda () (tabular-asa->design-matrix df '())))
    (check-blame (lambda () (tabular-asa->design-matrix df '(1))) #rx"string or a symbol")
    (check-blame (lambda () (tabular-asa->response df 'z)) #rx"no column named 'z")
    (check-blame (lambda () (tabular-asa->table df '(x1 w))) #rx"no column named 'w")
    (check-blame (lambda () (tabular-asa->design-matrix mtcars '("mpg")))
                 #rx"not a tabular-asa table")
    (check-blame (lambda () (tabular-asa->table asa:empty-table)) #rx"the table has no columns")
    (check-blame (lambda () (tabular-asa->design-matrix (asa:table-head df 0) '(x1)))
                 #rx"the table has no rows")
    (define dm (rows->design-matrix '((1 2))))
    (check-blame (lambda () (design-matrix->tabular-asa dm #:column-names '(a)))
                 #rx"number of column names")
    (check-blame (lambda () (design-matrix->tabular-asa dm #:column-names '(a "a")))
                 #rx"the column name \"a\" is repeated")
    (check-blame (lambda () (table->tabular-asa mtcars '("mpg" "speed")))
                 #rx"no column named \"speed\"")
    (check-blame (lambda () (table->tabular-asa '((1 2)))) #rx"table\\?"))

  (test-case "two columns of the same name are an error, naming the procedure called"
    (define twice (asa:table #(0) (list (cons 'a #(1)) (cons "a" #(2)))))
    (check-error (lambda () (tabular-asa->design-matrix twice '(a)))
                 #rx"^tabular-asa->design-matrix: the table has two columns with the same name")
    (check-error (lambda () (tabular-asa->table twice))
                 #rx"^tabular-asa->table: the table has two columns with the same name")
    (check-error (lambda () (table->tabular-asa (list (cons "a" '(1 2)) (cons 'a '(3 4)))))
                 #rx"^table->tabular-asa: the table has two columns with the same name")
    (check-error (lambda () (table->tabular-asa (hash "a" '(1) 'a '(2)) '(a)))
                 #rx"^table->tabular-asa: the table has two columns with the same name"))

  (test-case "a column named by neither a string nor a symbol converts by default"
    (define numbered (asa:table-read/columns '((1 2) (3 4)) '(1 b)))
    (check-equal? (tabular-asa->table numbered) (list (cons "1" #(1 2)) (cons "b" #(3 4)))))

  ;; --- the same fits as the list path --------------------------------------------------

  (define (folds n) (for/list ([i (in-range n)]) (modulo i 3)))

  (define longley (read-csv "longley"))
  (define wdbc (read-csv "wdbc"))
  (define iris (read-csv "iris"))
  (define veteran (read-csv "veteran"))
  (define warpbreaks (read-csv "warpbreaks"))
  (define linnerud (read-csv "linnerud"))

  (define-values (Xg yg) (load-longley))
  (define-values (Xb yb) (load-wdbc))
  (define-values (Xm ym) (load-iris))
  (define-values (Xc tc sc) (load-veteran))
  (define-values (Xp yp) (load-warpbreaks))
  (define-values (Xr Yr) (load-linnerud))

  (define (all-but df . names)
    (for/list ([name (in-list (asa:table-header df))]
               #:unless (memq name names))
      name))

  (test-case "tabular-asa's CSV reader gives the same data as the list loaders"
    (check-equal? (design-matrix->rows (tabular-asa->design-matrix longley (all-but longley 'Employed)))
                  (design-matrix->rows (rows->design-matrix Xg)))
    (check-equal? (tabular-asa->response longley 'Employed) yg)
    (check-equal? (tabular-asa->response iris 'class) ym))

  (test-case "Gaussian"
    (define X (tabular-asa->design-matrix longley (all-but longley 'Employed)))
    (define y (tabular-asa->response longley 'Employed))
    (check-equal? (elnet-fit X y #:lambda 0.1 #:alpha 0.5) (elnet-fit Xg yg #:lambda 0.1 #:alpha 0.5))
    (check-equal? (ols X y) (ols Xg yg))
    (check-equal? (lasso X y #:lambda 0.1) (lasso Xg yg #:lambda 0.1))
    (check-equal? (elnet-path X y) (elnet-path Xg yg))
    (check-equal? (elnet-cv X y #:fold-ids (folds 16)) (elnet-cv Xg yg #:fold-ids (folds 16)))
    (define fit (lasso X y #:lambda 0.1))
    (check-equal? (predict fit X) (predict fit Xg))
    (check-equal? (coef fit) (coef (lasso Xg yg #:lambda 0.1))))

  (test-case "binomial"
    (define X (tabular-asa->design-matrix wdbc (all-but wdbc 'diagnosis)))
    (define y (tabular-asa->response wdbc 'diagnosis))
    (define fit (logistic-fit X y #:lambda 0.02))
    (check-equal? fit (logistic-fit Xb yb #:lambda 0.02))
    (check-equal? (logistic-path X y #:nlambda 20) (logistic-path Xb yb #:nlambda 20))
    (check-equal? (logistic-cv X y #:fold-ids (folds 569) #:nlambda 10)
                  (logistic-cv Xb yb #:fold-ids (folds 569) #:nlambda 10))
    (check-equal? (logistic-predict-proba fit X) (logistic-predict-proba fit Xb))
    (check-equal? (logistic-predict fit X) (logistic-predict fit Xb)))

  (test-case "multinomial"
    (define X (tabular-asa->design-matrix iris (all-but iris 'class)))
    (define y (tabular-asa->response iris 'class))
    (define fit (multinomial-fit X y #:lambda 0.01))
    (check-equal? fit (multinomial-fit Xm ym #:lambda 0.01))
    (check-equal? (multinomial-path X y #:nlambda 20) (multinomial-path Xm ym #:nlambda 20))
    (check-equal? (multinomial-cv X y #:fold-ids (folds 150) #:nlambda 10)
                  (multinomial-cv Xm ym #:fold-ids (folds 150) #:nlambda 10))
    (check-equal? (multinomial-predict-proba fit X) (multinomial-predict-proba fit Xm))
    (check-equal? (multinomial-predict fit X) (multinomial-predict fit Xm)))

  (test-case "Cox"
    (define X (tabular-asa->design-matrix veteran (all-but veteran 'time 'status)))
    (define times (tabular-asa->response veteran 'time))
    (define statuses (tabular-asa->response veteran 'status))
    (define fit (cox-fit X times statuses #:lambda 0.05))
    (check-equal? fit (cox-fit Xc tc sc #:lambda 0.05))
    (check-equal? (cox-path X times statuses #:nlambda 20) (cox-path Xc tc sc #:nlambda 20))
    (check-equal? (cox-cv X times statuses #:fold-ids (folds 137) #:nlambda 10)
                  (cox-cv Xc tc sc #:fold-ids (folds 137) #:nlambda 10))
    (check-equal? (cox-linear-predictor fit X) (cox-linear-predictor fit Xc))
    (check-equal? (cox-relative-risk fit X) (cox-relative-risk fit Xc)))

  (test-case "Poisson"
    (define X (tabular-asa->design-matrix warpbreaks (all-but warpbreaks 'breaks)))
    (define y (tabular-asa->response warpbreaks 'breaks))
    (define fit (poisson-fit X y #:lambda 0.1))
    (check-equal? fit (poisson-fit Xp yp #:lambda 0.1))
    (check-equal? (poisson-path X y) (poisson-path Xp yp))
    (check-equal? (poisson-cv X y #:fold-ids (folds 54)) (poisson-cv Xp yp #:fold-ids (folds 54)))
    (check-equal? (poisson-predict-mean fit X) (poisson-predict-mean fit Xp)))

  (test-case "multi-response Gaussian, with Y from the table too"
    (define X (tabular-asa->design-matrix linnerud '(chins situps jumps)))
    (define Y (tabular-asa->design-matrix linnerud '(weight waist pulse)))
    (define fit (mgaussian-fit X Y #:lambda 1.0))
    (check-equal? fit (mgaussian-fit Xr Yr #:lambda 1.0))
    (check-equal? (mgaussian-path X Y) (mgaussian-path Xr Yr))
    (check-equal? (mgaussian-cv X Y #:fold-ids (folds 20)) (mgaussian-cv Xr Yr #:fold-ids (folds 20)))
    (check-equal? (mgaussian-predict fit X) (mgaussian-predict fit Xr)))

  ;; --- formulas ------------------------------------------------------------------------------

  (test-case "formulas fit a tabular-asa table as they fit the association list"
    (define cars (table->tabular-asa mtcars))
    (define t (tabular-asa->table cars))
    (define f (~ mpg wt hp (: wt hp)))
    (check-equal? (formula-fit f t #:lambda 0.1) (formula-fit f mtcars #:lambda 0.1))
    (check-equal? (formula-path f t) (formula-path f mtcars))
    (check-equal? (formula-cv f t #:fold-ids (folds 32)) (formula-cv f mtcars #:fold-ids (folds 32)))
    (define model (formula-fit (~ am wt hp) t #:family 'binomial #:lambda 0.05))
    (check-equal? model (formula-fit (~ am wt hp) mtcars #:family 'binomial #:lambda 0.05))
    (check-equal? (predict model (tabular-asa->table (asa:table-head cars 3)) #:type 'response)
                  (predict model (for/list ([c (in-list mtcars)]) (cons (car c) (for/list ([x (in-vector (cdr c) 0 3)]) x)))
                           #:type 'response)))

  (test-case "every family's formula fit is the same through tabular-asa as through the list"
    (define (same fit df name)
      (check-equal? (fit (tabular-asa->table df)) (fit (load-table name))))
    (same (lambda (t) (formula-fit (~ Employed all) t #:lambda 0.1)) longley "longley")
    (same (lambda (t) (formula-fit (~ diagnosis all) t #:family 'binomial #:lambda 0.02))
          wdbc "wdbc")
    (same (lambda (t) (formula-fit (~ class all) t #:family 'multinomial #:lambda 0.01))
          iris "iris")
    (same (lambda (t) (formula-fit (~ (surv time status) all) t #:family 'cox #:lambda 0.05))
          veteran "veteran")
    (same (lambda (t) (formula-fit (~ breaks all) t #:family 'poisson #:lambda 0.1))
          warpbreaks "warpbreaks")
    (same (lambda (t) (formula-fit (~ (weight waist pulse) all) t #:family 'mgaussian #:lambda 1.0))
          linnerud "linnerud"))

  (test-case "string columns are factors"
    (define flowers (table->tabular-asa iris-species))
    (define t (tabular-asa->table flowers))
    (define f (~ Sepal.Length Petal.Width Species))
    (define model (formula-fit f t #:lambda 0.01))
    (check-equal? model (formula-fit f iris-species #:lambda 0.01))
    (check-equal? (formula-model-levels model)
                  '(("Species" "setosa" "versicolor" "virginica")))
    (check-equal? (formula-cv (~ Species all) t #:family 'multinomial #:fold-ids (folds 150)
                              #:nlambda 10)
                  (formula-cv (~ Species all) iris-species #:family 'multinomial
                              #:fold-ids (folds 150) #:nlambda 10)))

  (test-case "a string column from a CSV is a factor, and a missing value in it is an error"
    (define csv
      (asa:table-read/csv
       (open-input-string "y,x,g\n1.0,2,a\n2.5,1,b\n2.0,4,a\n4.5,3,b\n5.0,6,c\n6.5,5,c\n")))
    (check-equal? (formula-predictor-names (~ y x g) (tabular-asa->table csv)) '("x" "gb" "gc"))
    (define with-na
      (asa:table-read/csv (open-input-string "y,x,g\n1.0,2,a\n2.5,1,\n2.0,4,a\n")))
    (check-error (lambda () (tabular-asa->table with-na)) #rx"missing value" #rx"column: \"g\""
                 #rx"row: 1"))

  ;; --- (require glmnet) does not load tabular-asa ------------------------------------------

  (test-case "(require glmnet) loads neither tabular-asa nor this adapter"
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (dynamic-require 'glmnet #f)
      (check-false (module-declared? 'tabular-asa #f))
      (check-false (module-declared? 'csv-reading #f))
      (check-false (module-declared? 'glmnet/data/tabular-asa #f))
      (dynamic-require 'glmnet/data/tabular-asa #f)
      (check-true (module-declared? 'tabular-asa #f)))))
