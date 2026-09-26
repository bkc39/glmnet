#lang racket/base

;; The formula front end (#26). A formula fit is the matrix fit of the columns
;; it selects, `equal?` to it for every family and for single fits, paths and
;; cross-validation. The formula language selects columns by name, `all` and
;; exclusions, and rejects unknown columns and a response used as a predictor.
;; `coef` is keyed by name, and `predict` reads a table by name, whatever the
;; order of its columns. The fixtures are the committed parity datasets, read
;; as tables with the CSV's column names.

(module+ test
  (require rackunit
           racket/list
           syntax/macro-testing
           glmnet
           (file "../private/demo-utils.rkt"))

  (define longley (load-table "longley"))
  (define wdbc (load-table "wdbc"))
  (define iris (load-table "iris"))
  (define veteran (load-table "veteran"))
  (define warpbreaks (load-table "warpbreaks"))
  (define linnerud (load-table "linnerud"))

  (define-values (Xg yg) (load-longley))
  (define-values (Xb yb) (load-wdbc))
  (define-values (Xm ym) (load-iris))
  (define-values (Xc tc sc) (load-veteran))
  (define-values (Xp yp) (load-warpbreaks))
  (define-values (Xr Yr) (load-linnerud))

  ;; The columns of a table, in the order given, as a list of rows.
  (define (rows-of table names)
    (apply map list (for/list ([name (in-list names)]) (cdr (assoc name table)))))

  (define (column-names table) (map car table))

  ;; Three fold ids per observation cycle, for cross-validation.
  (define (folds n) (for/list ([i (in-range n)]) (modulo i 3)))

  ;; --- equal to the matrix fits ------------------------------------------------

  (define-syntax-rule (check-same formula-call matrix-call)
    (check-equal? (formula-model-fit formula-call) matrix-call))

  (test-case "Gaussian: the fit, path and CV of the matrix procedures"
    (define f (~ Employed all))
    (check-same (formula-fit f longley #:lambda 0.5) (elnet-fit Xg yg #:lambda 0.5))
    (check-same (formula-fit f longley #:lambda 0.1 #:alpha 0.3 #:intercept? #f)
                (elnet-fit Xg yg #:lambda 0.1 #:alpha 0.3 #:intercept? #f))
    (check-same (formula-path f longley) (elnet-path Xg yg))
    (check-same (formula-path f longley #:lambda '(1.0 0.1) #:standardize? #f)
                (elnet-path Xg yg #:lambda '(1.0 0.1) #:standardize? #f))
    (check-same (formula-cv f longley #:fold-ids (folds 16))
                (elnet-cv Xg yg #:fold-ids (folds 16)))
    (check-same (formula-cv f longley #:fold-ids (folds 16) #:type-measure 'mae)
                (elnet-cv Xg yg #:fold-ids (folds 16) #:type-measure 'mae)))

  (test-case "binomial"
    (define f (~ diagnosis all))
    (check-same (formula-fit f wdbc #:family 'binomial #:lambda 0.02)
                (logistic-fit Xb yb #:lambda 0.02))
    (check-same (formula-path f wdbc #:family 'binomial #:nlambda 20)
                (logistic-path Xb yb #:nlambda 20))
    (check-same (formula-cv f wdbc #:family 'binomial #:fold-ids (folds 569) #:nlambda 10)
                (logistic-cv Xb yb #:fold-ids (folds 569) #:nlambda 10))
    (check-same (formula-cv f wdbc #:family 'binomial #:fold-ids (folds 569) #:nlambda 10
                            #:type-measure 'class)
                (logistic-cv Xb yb #:fold-ids (folds 569) #:nlambda 10 #:type-measure 'class)))

  (test-case "multinomial"
    (define f (~ class all))
    (check-same (formula-fit f iris #:family 'multinomial #:lambda 0.02)
                (multinomial-fit Xm ym #:lambda 0.02))
    (check-same (formula-path f iris #:family 'multinomial #:nlambda 20)
                (multinomial-path Xm ym #:nlambda 20))
    (check-same (formula-cv f iris #:family 'multinomial #:fold-ids (folds 150) #:nlambda 10)
                (multinomial-cv Xm ym #:fold-ids (folds 150) #:nlambda 10)))

  (test-case "Cox, from a (surv time status) response"
    (define f (~ (surv time status) all))
    (check-same (formula-fit f veteran #:family 'cox #:lambda 0.05) (cox-fit Xc tc sc #:lambda 0.05))
    (check-same (formula-fit f veteran #:family 'cox #:lambda 0.05 #:intercept? #f)
                (cox-fit Xc tc sc #:lambda 0.05))
    (check-same (formula-path f veteran #:family 'cox #:nlambda 20) (cox-path Xc tc sc #:nlambda 20))
    (check-same (formula-cv f veteran #:family 'cox #:fold-ids (folds 137) #:nlambda 10)
                (cox-cv Xc tc sc #:fold-ids (folds 137) #:nlambda 10))
    (check-same (formula-cv f veteran #:family 'cox #:fold-ids (folds 137) #:nlambda 10
                            #:type-measure 'C)
                (cox-cv Xc tc sc #:fold-ids (folds 137) #:nlambda 10 #:type-measure 'C)))

  (test-case "Poisson"
    (define f (~ breaks all))
    (check-same (formula-fit f warpbreaks #:family 'poisson #:lambda 0.05)
                (poisson-fit Xp yp #:lambda 0.05))
    (check-same (formula-path f warpbreaks #:family 'poisson) (poisson-path Xp yp))
    (check-same (formula-cv f warpbreaks #:family 'poisson #:fold-ids (folds 54))
                (poisson-cv Xp yp #:fold-ids (folds 54))))

  (test-case "multi-response Gaussian, from several response columns"
    (define f (~ (weight waist pulse) all))
    (check-same (formula-fit f linnerud #:family 'mgaussian #:lambda 1.0)
                (mgaussian-fit Xr Yr #:lambda 1.0))
    (check-same (formula-path f linnerud #:family 'mgaussian) (mgaussian-path Xr Yr))
    (check-same (formula-cv f linnerud #:family 'mgaussian #:fold-ids (folds 20))
                (mgaussian-cv Xr Yr #:fold-ids (folds 20)))
    (check-same (formula-fit (~ (pulse) chins jumps) linnerud #:family 'mgaussian #:lambda 1.0)
                (mgaussian-fit (rows-of linnerud '("chins" "jumps"))
                               (rows-of linnerud '("pulse")) #:lambda 1.0))
    (check-same (formula-fit (~ pulse chins jumps) linnerud #:family 'mgaussian #:lambda 1.0)
                (mgaussian-fit (rows-of linnerud '("chins" "jumps"))
                               (rows-of linnerud '("pulse")) #:lambda 1.0)))

  (test-case "a hash, a design matrix and an association list give the same fit"
    (define f (~ Employed GNP Population))
    (define as-hash (for/hash ([column (in-list longley)]) (values (car column) (cdr column))))
    (define as-matrix (table->design-matrix longley))
    (define fit (formula-fit f longley #:lambda 0.1))
    (check-equal? (formula-fit f as-hash #:lambda 0.1) fit)
    (check-equal? (formula-fit f as-matrix #:lambda 0.1) fit)
    (check-same fit (elnet-fit (rows-of longley '("GNP" "Population")) yg #:lambda 0.1)))

  ;; --- the formula language ------------------------------------------------------

  (test-case "~ quotes its names and makes a formula that prints as itself"
    (define f (~ y (- all x3) "a b"))
    (check-true (formula? f))
    (check-equal? (formula-response f) 'y)
    (check-equal? (formula-terms f) '((- all x3) "a b"))
    (check-equal? f (make-formula 'y '(- all x3) "a b"))
    (check-equal? (format "~a" f) "(~ y (- all x3) \"a b\")")
    (check-equal? (format "~s" (~ (surv time status) all)) "(~ (surv time status) all)")
    (check-false (formula? '(~ y all))))

  (define letters
    (list (cons "y" '(1 2 3)) (cons "a" '(1 0 1)) (cons "b" '(0 1 1))
          (cons "c" '(2 2 1)) (cons "d" '(5 4 3))))

  (test-case "predictors by name, all, + and -"
    (define (names f) (formula-predictor-names f letters))
    (check-equal? (names (~ y all)) '("a" "b" "c" "d"))
    (check-equal? (names (~ y c a)) '("c" "a"))
    (check-equal? (names (~ y (+ c a))) '("c" "a"))
    (check-equal? (names (~ y "d" b)) '("d" "b"))
    (check-equal? (names (~ y c a c (+ a b))) '("c" "a" "b"))
    (check-equal? (names (~ y (- all b))) '("a" "c" "d"))
    (check-equal? (names (~ y (- all b "d"))) '("a" "c"))
    (check-equal? (names (~ y (- all (+ a b)))) '("c" "d"))
    (check-equal? (names (~ y (- all y))) '("a" "b" "c" "d"))
    (check-equal? (names (~ y (- (+ d c b) c))) '("d" "b"))
    (check-equal? (names (~ y (- all (- all a)))) '("a"))
    (check-equal? (names (~ y (- all a) a)) '("b" "c" "d" "a"))
    (check-equal? (names (~ (surv a b) all)) '("y" "c" "d"))
    (check-equal? (names (~ (y d) all)) '("a" "b" "c")))

  (test-case "a table's symbol names match a formula's strings, and back"
    (define symbols (for/list ([column (in-list letters)])
                      (cons (string->symbol (car column)) (cdr column))))
    (check-equal? (formula-predictor-names (~ "y" "a" b) symbols) '("a" "b"))
    (check-equal? (formula-predictor-names (make-formula "y" "c" 'a) letters) '("c" "a")))

  (test-case "a column named after a word of the language is written as a string"
    (define t (list (cons "all" '(1 2 3)) (cons "surv" '(3 1 2)) (cons "+" '(0 1 0))))
    (check-equal? (formula-predictor-names (~ "all" "surv" "+") t) '("surv" "+"))
    (check-equal? (formula-predictor-names (~ "all" all) t) '("surv" "+")))

  (test-case "errors: unknown columns, the response as a predictor, nothing selected"
    (define (names f) (formula-predictor-names f letters))
    (check-exn #rx"no column with this name.*column: \"z\".*formula: \\(~ y a z\\)"
               (lambda () (names (~ y a z))))
    (check-exn #rx"no column with this name.*column: \"z\""
               (lambda () (names (~ y (- all z)))))
    (check-exn #rx"no column with this name.*column: \"w\""
               (lambda () (names (~ w all))))
    (check-exn #rx"a response column is listed as a predictor.*column: \"y\""
               (lambda () (names (~ y a y))))
    (check-exn #rx"a response column is listed as a predictor.*column: \"b\""
               (lambda () (names (~ (surv a b) (+ c b)))))
    (check-exn #rx"selects no predictors" (lambda () (names (~ y (- all a b c d)))))
    (check-exn #rx"names a column twice.*column: \"a\"" (lambda () (names (~ (a a) all))))
    (check-exn #rx"no column with this name.*column: \"z\""
               (lambda () (formula-fit (~ y a z) letters #:lambda 0.1))))

  (test-case "the formula language is checked when it is expanded"
    (check-exn #rx"~: expected more terms" (lambda () (convert-compile-time-error (~ y))))
    (check-exn #rx"expected one of these literal symbols"
               (lambda () (convert-compile-time-error (~ y (* a b)))))
    (check-exn #rx"expected more terms" (lambda () (convert-compile-time-error (~ y (- all)))))
    (check-exn #rx"expected a column name" (lambda () (convert-compile-time-error (~ all x))))
    (check-exn #rx"expected more terms starting with a column name"
               (lambda () (convert-compile-time-error (~ (surv t) all))))
    (check-exn #rx"expected a predictor term"
               (lambda () (convert-compile-time-error (~ y 3))))
    (check-exn exn:fail:contract? (lambda () (make-formula 'y '(* a b))))
    (check-exn exn:fail:contract? (lambda () (make-formula 'all 'x)))
    (check-exn exn:fail:contract? (lambda () (make-formula '(surv t) 'x)))
    (check-exn exn:fail:contract? (lambda () (make-formula 'y))))

  (test-case "the response must suit the family"
    (check-exn #rx"Cox family needs a \\(surv time status\\) response"
               (lambda () (formula-fit (~ time all) veteran #:family 'cox #:lambda 0.1)))
    (check-exn #rx"\\(surv time status\\) response is for the Cox family"
               (lambda () (formula-fit (~ (surv time status) all) veteran #:lambda 0.1)))
    (check-exn #rx"\\(surv time status\\) response is for the Cox family"
               (lambda () (formula-fit (~ (surv time status) all) veteran #:family 'mgaussian
                                       #:lambda 0.1)))
    (check-exn #rx"several columns is for the mgaussian family"
               (lambda () (formula-fit (~ (weight waist) all) linnerud #:lambda 0.1)))
    (check-exn #rx"binomial response must be 0 or 1.*column: \"y\".*row: 1.*value: 2.0"
               (lambda () (formula-fit (~ y a b) letters #:family 'binomial #:lambda 0.1)))
    (check-exn #rx"multinomial response must be a class label.*column: \"x\""
               (lambda () (formula-fit (~ x a) (list (cons "x" '(0 1 1.5)) (cons "a" '(1 2 3)))
                                       #:family 'multinomial #:lambda 0.1)))
    (check-exn #rx"Poisson response must be non-negative.*row: 2"
               (lambda () (formula-fit (~ x a) (list (cons "x" '(0 1 -1)) (cons "a" '(1 2 3)))
                                       #:family 'poisson #:lambda 0.1)))
    (check-exn #rx"survival time must be positive.*column: \"a\""
               (lambda () (formula-fit (~ (surv a b) c) letters #:family 'cox #:lambda 0.1)))
    (check-exn #rx"event status must be 0 or 1.*column: \"c\""
               (lambda () (formula-fit (~ (surv y c) a) letters #:family 'cox #:lambda 0.1)))
    (check-exn #rx"different lengths"
               (lambda () (formula-fit (~ y a) (list (cons "y" '(1 2)) (cons "a" '(1 2 3)))
                                       #:lambda 0.1)))
    (check-exn #rx"no such type measure.*'auc"
               (lambda () (formula-cv (~ Employed all) longley #:type-measure 'auc)))
    (check-exn exn:fail:contract?
               (lambda () (formula-fit (~ y a) letters #:family 'logistic #:lambda 0.1))))

  ;; --- name-keyed results -----------------------------------------------------------

  (define gfit (formula-fit (~ Employed all) longley #:lambda 0.5))
  (define gpath (formula-path (~ Employed (- all Year)) longley #:lambda '(1.0 0.1 0.01)))

  (test-case "coef is keyed by name, (Intercept) first, as R names it"
    (define c (coef gfit))
    (check-equal? (map car c) (cons "(Intercept)" (remove "Employed" (column-names longley))))
    (check-equal? (list->vector (map cdr c)) (coef (formula-model-fit gfit)))
    (check-equal? (map cdr (coef gpath #:lambda 0.1))
                  (vector->list (coef (formula-model-fit gpath) #:lambda 0.1)))
    (check-equal? (map car (coef gpath #:lambda 0.1))
                  '("(Intercept)" "GNP.deflator" "GNP" "Unemployed" "Armed.Forces" "Population"))
    (define per-lambda (coef gpath #:lambda '(1.0 0.05)))
    (check-equal? (length per-lambda) 2)
    (check-equal? (map car (second per-lambda)) (map car (coef gpath #:lambda 0.1))))

  (test-case "coef of the other families: no intercept for Cox, a list per class or response"
    (define cox (formula-fit (~ (surv time status) (- all trt)) veteran #:family 'cox #:lambda 0.05))
    (check-equal? (map car (coef cox)) '("karno" "diagtime" "age" "prior"))
    (define multi (formula-fit (~ class f2 f1) iris #:family 'multinomial #:lambda 0.02))
    (define mc (coef multi))
    (check-equal? (map car mc) '(0 1 2))
    (check-equal? (map car (cdr (assv 2 mc))) '("(Intercept)" "f2" "f1"))
    (check-equal? (for/vector ([class (in-list mc)]) (list->vector (map cdr (cdr class))))
                  (coef (formula-model-fit multi)))
    (define mg (formula-fit (~ (pulse weight) all) linnerud #:family 'mgaussian #:lambda 1.0))
    (check-equal? (map car (coef mg)) '("pulse" "weight"))
    (check-equal? (map car (cdr (assoc "weight" (coef mg))))
                  '("(Intercept)" "chins" "situps" "jumps" "waist")))

  (test-case "coef of a cross-validated formula model takes the named lambdas"
    (define cv (formula-cv (~ Employed all) longley #:fold-ids (folds 16)))
    (check-equal? (map cdr (coef cv #:lambda 'lambda-min))
                  (vector->list (coef (formula-model-fit cv) #:lambda 'lambda-min)))
    (check-equal? (coef cv) (coef cv #:lambda 'lambda-1se))
    (check-equal? (deviance-ratio cv) (deviance-ratio (formula-model-fit cv)))
    (check-equal? (glmnet-model-default-lambda cv)
                  (glmnet-cv-lambda-1se (formula-model-fit cv))))

  (test-case "predict reads a table by name, in any order and with extra columns"
    (define reversed (reverse longley))
    (define expected (predict (formula-model-fit gfit) (rows-of longley (remove "Employed"
                                                                                 (column-names longley)))))
    (check-equal? (predict gfit longley) expected)
    (check-equal? (predict gfit reversed) expected)
    (check-equal? (predict gfit (cons (cons "extra" (make-list 16 "text")) reversed)) expected)
    (check-equal? (predict gfit (for/hash ([column (in-list longley)])
                                  (values (string->symbol (car column)) (cdr column))))
                  expected)
    (check-equal? (predict gfit (table->design-matrix reversed)) expected)
    (check-equal? (predict gpath longley #:lambda '(0.5 0.05))
                  (predict (formula-model-fit gpath)
                           (rows-of longley '("GNP.deflator" "GNP" "Unemployed" "Armed.Forces"
                                              "Population"))
                           #:lambda '(0.5 0.05))))

  (test-case "predict of the other families, by #:type"
    (define b (formula-fit (~ diagnosis x3 x1) wdbc #:family 'binomial #:lambda 0.02))
    (define rows (rows-of wdbc '("x3" "x1")))
    (for ([type (in-list '(link response class))])
      (check-equal? (predict b (reverse wdbc) #:type type)
                    (predict (formula-model-fit b) rows #:type type)))
    (define cox (formula-fit (~ (surv time status) karno age) veteran #:family 'cox #:lambda 0.05))
    (check-equal? (predict cox (list (cons "age" '(60 70)) (cons "karno" '(50 90))) #:type 'response)
                  (predict (formula-model-fit cox) '((50 60) (90 70)) #:type 'response)))

  (test-case "predict names a missing column; a named model needs a table, another a matrix"
    (check-exn #rx"predict: the table has no column with this name.*column: \"Year\""
               (lambda () (predict gfit (remove (assoc "Year" longley) longley))))
    (check-exn #rx"the model's predictors are named, so X must be a table"
               (lambda () (predict gfit Xg)))
    (check-exn #rx"X must be a table"
               (lambda () (predict gfit (rows->design-matrix Xg))))
    (check-exn #rx"predictors are not named, so X must be a design matrix"
               (lambda () (predict (formula-model-fit gfit) longley)))
    (check-equal? (predict (formula-model-fit gfit)
                           (table->design-matrix longley (take (column-names longley) 6)))
                  (predict gfit longley)))

  (test-case "the name methods of gen:glmnet-model"
    (check-equal? (glmnet-model-predictor-names gfit)
                  '("GNP.deflator" "GNP" "Unemployed" "Armed.Forces" "Population" "Year"))
    (check-equal? (glmnet-model-response-names gfit) '("Employed"))
    (check-equal? (glmnet-model-response-names
                   (formula-fit (~ (surv time status) all) veteran #:family 'cox #:lambda 0.1))
                  '("time" "status"))
    (check-false (glmnet-model-predictor-names (formula-model-fit gfit)))
    (check-false (glmnet-model-response-names (formula-model-fit gpath)))
    (check-false (glmnet-model-predictor-names (elnet-cv Xg yg #:fold-ids (folds 16)))))

  (test-case "a formula model prints as its fit, with the formula after the family"
    (check-regexp-match #rx"^#<glmnet:gaussian \\(~ Employed all\\) λ=0.5 dev=0.9[0-9]+ nz=[0-9]/6>$"
                        (format "~a" gfit))
    (check-regexp-match #rx"^#<glmnet-path:gaussian \\(~ Employed \\(- all Year\\)\\)\n  Df"
                        (format "~a" gpath))
    (check-regexp-match #rx"^#<glmnet-cv:gaussian \\(~ Employed all\\) Mean-Squared Error\n"
                        (format "~a" (formula-cv (~ Employed all) longley
                                                 #:fold-ids (folds 16))))))
