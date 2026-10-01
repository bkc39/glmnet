#lang racket/base

;; glmnet/data/tabular-asa. The fixtures are the committed parity datasets,
;; read with tabular-asa's own CSV reader, and R's mtcars and iris.

(module+ test
  (require rackunit
           racket/runtime-path
           (only-in racket/contract exn:fail:contract:blame? value-contract contract-name)
           (only-in racket/list last)
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
                 #rx"column: \"a\"\n  row: 1$")
    (check-error (lambda () (tabular-asa->response na 'a))
                 #rx"^tabular-asa->response: the table has a missing value"
                 #rx"column: \"a\"\n  row: 1$")
    (check-error (lambda () (tabular-asa->table na))
                 #rx"^tabular-asa->table: the table has a missing value"
                 #rx"column: \"a\"\n  row: 1$")
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
                 #rx"the table has an element that is not a real number"
                 #rx"column: \"c\"\n  row: 0\n  element: \"x\"$")
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
                 #rx"column: \"ok\"\n  row: 1\n  element: #f$"))

  (test-case "-0.0 and extreme values convert bit for bit"
    (define xs (list -0.0 5e-324 -1.7976931348623157e308 1/3 7))
    (define t (asa:table-read/columns (list xs) '(x)))
    (define dm (tabular-asa->design-matrix t '(x)))
    (check-equal? dm (rows->design-matrix (map list xs) #:column-names '("x")))
    (check-eqv? (design-matrix-ref dm 0 0) -0.0)
    (check-equal? (tabular-asa->design-matrix (design-matrix->tabular-asa dm) '(x)) dm))

  (test-case "columns of different lengths are an error"
    (check-error (lambda () (table->tabular-asa (list (cons "a" '(1 2)) (cons "b" '(3)))))
                 #rx"different lengths" #rx"column: \"b\""))

  (test-case "a glmnet table with no rows makes a tabular-asa table with no rows"
    (define none (table->tabular-asa (list (cons "x" '()) (cons 'y #()))))
    (check-equal? (asa:table-header none) '(x y))
    (check-equal? (asa:table-index none) #())
    (check-true (asa:table-empty? none))
    (check-equal? (asa:table-header (table->tabular-asa (list (cons "x" '()) (cons "y" '())) '(y)))
                  '(y))
    (check-error (lambda () (table->tabular-asa (list (cons "x" '()) (cons "y" '(1)))))
                 #rx"different lengths" #rx"column: \"y\""))

  (test-case "a wide table converts in time linear in its number of columns"
    (define (wide-tables nc)
      (define names (for/list ([j (in-range nc)]) (string->symbol (format "x~a" j))))
      (define columns (for/list ([j (in-range nc)]) (vector j (+ j 0.5))))
      (list names
            (asa:table #(0 1) (map cons names columns))
            (for/list ([name (in-list names)] [column (in-list columns)])
              (cons (symbol->string name) column))))
    (define small (wide-tables 10000))
    (define large (wide-tables 40000))
    (define (elapsed-ms thunk)
      (collect-garbage)
      (define t0 (current-inexact-monotonic-milliseconds))
      (thunk)
      (- (current-inexact-monotonic-milliseconds) t0))
    (define (check-linear who convert)
      (define-values (small-ms large-ms)
        (for/fold ([small-ms +inf.0] [large-ms +inf.0]) ([k (in-range 3)])
          (values (min small-ms (elapsed-ms (lambda () (apply convert small))))
                  (min large-ms (elapsed-ms (lambda () (apply convert large)))))))
      (check < (/ large-ms small-ms) 8
             (format "~a: ~a ms at 40000 columns, ~a ms at 10000" who large-ms small-ms)))
    (check-linear 'tabular-asa->design-matrix
                  (lambda (names wide t) (tabular-asa->design-matrix wide names)))
    (check-linear 'tabular-asa->table
                  (lambda (names wide t) (tabular-asa->table wide (reverse names))))
    (check-linear 'table->tabular-asa
                  (lambda (names wide t) (table->tabular-asa t names)))
    (define-values (names wide t) (apply values large))
    (define dm (tabular-asa->design-matrix wide names))
    (check-equal? (design-matrix-ncols dm) 40000)
    (check-eqv? (design-matrix-ref dm 1 39999) 39999.5)
    (check-equal? (car (tabular-asa->table wide (reverse names)))
                  (cons "x39999" (vector 39999 39999.5)))
    (check-equal? (asa:table-header (table->tabular-asa t names)) names))

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
                 #rx"not a tabular-asa table" #rx"expected: asa:table[?]")
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

  (test-case "the contracts name tabular-asa's table? asa:table?, apart from glmnet's table?"
    (define (result-of f) (last (contract-name (value-contract f))))
    (check-equal? (result-of design-matrix->tabular-asa) '(result asa:table?))
    (check-equal? (result-of table->tabular-asa) '(result asa:table?))
    (check-equal? (result-of tabular-asa->table) '(result table?))
    (check-equal? (cadr (car (cadr (contract-name (value-contract tabular-asa->response)))))
                  '(and/c asa:table? (not/c asa:table-empty?))))

  (test-case "a tabular-asa table with two columns of the same name breaks the contract"
    (define twice (asa:table #(0) (list (cons 'a #(1)) (cons "a" #(2)))))
    (check-blame (lambda () (tabular-asa->design-matrix twice '(a)))
                 #rx"the table has two columns named \"a\"")
    (check-blame (lambda () (tabular-asa->response twice 'a))
                 #rx"the table has two columns named \"a\"")
    (check-blame (lambda () (tabular-asa->table twice))
                 #rx"the table has two columns named \"a\"")
    (check-equal? (tabular-asa->table (asa:table #(0) (list (cons 'a #(1)) (cons "b" #(2)))))
                  (list (cons "a" #(1)) (cons "b" #(2)))))

  (test-case "a glmnet table with two columns of the same name is an error naming the procedure"
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

  (test-case "class labels read as flonums fit as the exact labels do"
    (define labels (asa:table-read/columns (list (map exact->inexact ym)) '(class)))
    (define y (tabular-asa->response labels 'class))
    (check-true (andmap flonum? y))
    (check-equal? (multinomial-fit Xm y #:lambda 0.01) (multinomial-fit Xm ym #:lambda 0.01)))

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
                  (predict model (for/list ([c (in-list mtcars)])
                                   (cons (car c) (for/list ([x (in-vector (cdr c) 0 3)]) x)))
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
