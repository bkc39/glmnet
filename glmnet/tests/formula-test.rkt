#lang racket/base

;; The formula front end (#26, #53). A formula fit is the matrix fit of its
;; design matrix, `equal?` to it for every family and for single fits, paths
;; and cross-validation. The formula language is R's algebra: terms expand and
;; order as R's terms() does, prefix and infix, and the design matrix holds
;; their products; transforms are Racket functions of columns, whose names
;; are the table's columns and then Racket bindings; columns of strings,
;; symbols or booleans and (factor x) are factors, coded as R's model.matrix
;; codes them; unknown columns are errors, and a response on the right-hand
;; side is dropped with a warning. `coef` is keyed by name, and `predict`
;; rebuilds the design matrix from a table, whatever the order of its
;; columns, with the factors' levels of the fit. The fixtures are the
;; committed parity datasets, read as tables with the CSV's column names, and
;; R's mtcars and iris. parity-test.rkt checks the algebra against R itself.

(module+ test
  (require rackunit
           (only-in racket/contract exn:fail:contract:blame?)
           racket/generic
           racket/list
           racket/match
           syntax/macro-testing
           (only-in racket/math sqr)
           (for-syntax racket/base syntax/parse)
           glmnet
           (only-in glmnet/datasets mtcars [iris iris-species])
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

  ;; A column of an association-list table as a list: glmnet/datasets' tables
  ;; hold vectors, the committed datasets' lists.
  (define (table-column table name)
    (define column (cdr (assoc name table)))
    (if (vector? column) (vector->list column) column))

  ;; The columns of a table, in the order given, as a list of rows.
  (define (rows-of table names)
    (apply map list (for/list ([name (in-list names)]) (table-column table name))))

  (define (column-names table) (map car table))

  ;; Three fold ids per observation cycle, for cross-validation.
  (define (folds n) (for/list ([i (in-range n)]) (modulo i 3)))

  ;; A contract error that blames the caller, with a message matching rx.
  (define ((blame-matching rx) e)
    (and (exn:fail:contract:blame? e) (regexp-match? rx (exn-message e))))

  ;; --- equal to the matrix fits ------------------------------------------------

  (define (check-same model matrix-fit)
    (check-equal? (formula-model-fit model) matrix-fit))

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

  (test-case "an infix formula keeps its operators, and prints as the reader's form"
    (define f (y . ~ . 1 + x + z + (* x z)))
    (check-equal? f (~ y 1 + x + z + (* x z)))
    (check-equal? (formula-terms f) '(1 + x + z + (* x z)))
    (check-equal? f (make-formula 'y 1 '+ 'x '+ 'z '+ '(* x z)))
    (check-equal? (format "~a" f) "(~ y 1 + x + z + (* x z))")
    (check-equal? (format "~a" (~ y (x + z) ^ 2 - x : z)) "(~ y (x + z) ^ 2 - x : z)")
    (check-equal? (format "~a" (~ y)) "(~ y)"))

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
    (check-equal? (formula-predictor-names (~ "all" all) t) '("surv" "+"))
    (check-exn #rx"~: all is a word of the formula language; write a column with this name as a string, \"all\""
               (lambda () (convert-compile-time-error (~ all x))))
    (check-exn #rx"~: surv is a word of the formula language.*\"surv\""
               (lambda () (convert-compile-time-error (~ y a surv))))
    (check-exn #rx"~: [+] is a word of the formula language.*\"[+]\""
               (lambda () (convert-compile-time-error (~ + x)))))

  (test-case "errors: unknown columns, a response column twice"
    (define (names f) (formula-predictor-names f letters))
    (check-exn #rx"no column with this name.*column: \"z\".*formula: \\(~ y a z\\)"
               (lambda () (names (~ y a z))))
    (check-exn #rx"no column with this name.*column: \"z\""
               (lambda () (names (~ y (- all z)))))
    (check-exn #rx"no column with this name.*column: \"w\""
               (lambda () (names (~ w all))))
    (check-exn #rx"names a column twice.*column: \"a\"" (lambda () (names (~ (a a) all))))
    (define with-colon (cons (cons "a:b" '(0 1 0)) letters))
    (check-exn #rx"^formula-predictor-names: two columns of the formula's design matrix have the same name\n  name: \"a:b\""
               (lambda () (formula-predictor-names (~ y "a:b" (: a b)) with-colon)))
    (check-exn #rx"^formula-fit: two columns of the formula's design matrix have the same name"
               (lambda () (formula-fit (~ y "a:b" (: a b)) with-colon #:lambda 0.1)))
    (define with-intercept-name (cons (cons "(Intercept)" '(2 0 1)) letters))
    (check-exn #rx"^formula-fit: a column of the formula's design matrix has the intercept's name\n  name: \"\\(Intercept\\)\"\n  formula: \\(~ y \"\\(Intercept\\)\" a\\)"
               (lambda () (formula-fit (~ y "(Intercept)" a) with-intercept-name #:lambda 0.1)))
    (check-exn #rx"^formula-predictor-names: a column of the formula's design matrix has the intercept's name"
               (lambda () (formula-predictor-names (~ y all) with-intercept-name)))
    (check-equal? (formula-predictor-names (~ y (: "(Intercept)" a)) with-intercept-name)
                  '("(Intercept):a"))
    (check-exn #rx"no column with this name.*column: \"z\""
               (lambda () (formula-fit (~ y a z) letters #:lambda 0.1))))

  ;; --- R's algebra -------------------------------------------------------------------

  ;; The names formula f gives mtcars's predictors.
  (define (mtcars-names f) (formula-predictor-names f mtcars))

  ;; Each expected list is what R 4.5.3 gives for the formula's terms() on
  ;; mtcars (the R spelling is in the comment).
  (test-case "terms expand and order as R's terms() does"
    ;; mpg ~ wt * hp
    (check-equal? (mtcars-names (~ mpg (* wt hp))) '("wt" "hp" "wt:hp"))
    ;; mpg ~ hp * wt
    (check-equal? (mtcars-names (~ mpg (* hp wt))) '("hp" "wt" "hp:wt"))
    ;; mpg ~ hp:wt + wt: degree first, variables in the order they appear
    (check-equal? (mtcars-names (~ mpg (: hp wt) wt)) '("wt" "hp:wt"))
    ;; mpg ~ wt:hp + hp:wt: one term
    (check-equal? (mtcars-names (~ mpg (: wt hp) (: hp wt))) '("wt:hp"))
    ;; mpg ~ (wt + hp + qsec)^2
    (check-equal? (mtcars-names (~ mpg (^ (+ wt hp qsec) 2)))
                  '("wt" "hp" "qsec" "wt:hp" "wt:qsec" "hp:qsec"))
    ;; mpg ~ (wt + hp + qsec)^5: at most every variable
    (check-equal? (mtcars-names (~ mpg (^ (+ wt hp qsec) 5)))
                  '("wt" "hp" "qsec" "wt:hp" "wt:qsec" "hp:qsec" "wt:hp:qsec"))
    ;; mpg ~ wt^2, mpg ~ wt:wt and mpg ~ wt*wt are all mpg ~ wt
    (check-equal? (mtcars-names (~ mpg (^ wt 2))) '("wt"))
    (check-equal? (mtcars-names (~ mpg (: wt wt))) '("wt"))
    (check-equal? (mtcars-names (~ mpg (* wt wt))) '("wt"))
    ;; mpg ~ (wt + hp) * (qsec + drat)
    (check-equal? (mtcars-names (~ mpg (* (+ wt hp) (+ qsec drat))))
                  '("wt" "hp" "qsec" "drat" "wt:qsec" "wt:drat" "hp:qsec" "hp:drat"))
    ;; mpg ~ qsec * (wt + hp)
    (check-equal? (mtcars-names (~ mpg (* qsec (+ wt hp)))) '("qsec" "wt" "hp" "qsec:wt" "qsec:hp"))
    ;; mpg ~ wt:hp:qsec + wt + hp:wt
    (check-equal? (mtcars-names (~ mpg (: wt hp qsec) wt (: hp wt))) '("wt" "wt:hp" "wt:hp:qsec"))
    ;; mpg ~ .:wt
    (check-equal? (mtcars-names (~ mpg (: all wt)))
                  '("wt" "cyl:wt" "disp:wt" "hp:wt" "drat:wt" "wt:qsec" "wt:vs" "wt:am" "wt:gear"
                    "wt:carb")))

  ;; R crosses (a + b + c)^n n - 1 times: R 4.5.3 takes 1.7 s for terms(y ~
  ;; (a + b + c)^1000000), whose terms are those of (a + b + c)^3. Expanding
  ;; stops once a crossing adds nothing, so an exponent of a billion is no
  ;; slower than one of 4; it used to cross a billion times.
  (test-case "a power past the number of terms stops crossing, with R's terms"
    (define result (box #f))
    (define worker
      (thread (lambda ()
                (set-box! result
                          (list (mtcars-names (~ mpg (^ (+ wt hp qsec) 1000000000)))
                                (mtcars-names (~ mpg (^ wt 1000000000))))))))
    (check-not-false (sync/timeout 10 worker) "expanding a power of a billion takes over 10 s")
    (kill-thread worker)
    (check-equal? (unbox result)
                  (list (mtcars-names (~ mpg (^ (+ wt hp qsec) 3))) '("wt")))
    (check-equal? (mtcars-names (~ mpg (^ (+ wt hp qsec) 3)))
                  '("wt" "hp" "qsec" "wt:hp" "wt:qsec" "hp:qsec" "wt:hp:qsec")))

  (test-case "- removes terms that are equal, and ignores an absent one, as R does"
    ;; mpg ~ wt * hp - hp
    (check-equal? (mtcars-names (~ mpg (- (* wt hp) hp))) '("wt" "wt:hp"))
    ;; mpg ~ wt * hp - drat
    (check-equal? (mtcars-names (~ mpg (- (* wt hp) drat))) '("wt" "hp" "wt:hp"))
    ;; mpg ~ wt * hp * qsec - wt:hp:qsec
    (check-equal? (mtcars-names (~ mpg (- (* wt hp qsec) (: wt hp qsec))))
                  '("wt" "hp" "qsec" "wt:hp" "wt:qsec" "hp:qsec"))
    ;; mpg ~ -wt + hp
    (check-equal? (mtcars-names (~ mpg - wt + hp)) '("hp"))
    ;; mpg ~ wt + (hp - wt): the removal is inside the group
    (check-equal? (mtcars-names (~ mpg wt (- hp wt))) '("wt" "hp"))
    (check-equal? (mtcars-names (~ mpg (- wt wt))) '()))

  (test-case "* with an empty left operand is empty, as R's CrossTerms makes it"
    ;; R: mpg ~ 0*wt + hp is mpg ~ hp - 1; mpg ~ wt*0 is mpg ~ wt - 1;
    ;; mpg ~ 1*wt*hp has no terms.
    (check-equal? (mtcars-names (mpg . ~ . 0 * wt + hp)) '("hp"))
    (check-equal? (mtcars-names (~ mpg (* wt 0))) '("wt"))
    (check-equal? (mtcars-names (~ mpg (* 1 wt hp))) '())
    (check-equal? (formula-model-fit (formula-fit (mpg . ~ . 0 * wt + hp + qsec) mtcars #:lambda 0.1))
                  (elnet-fit (rows-of mtcars '("hp" "qsec")) (table-column mtcars "mpg") #:lambda 0.1
                             #:intercept? #f)))

  (test-case "infix operators take R's precedence, left to right, and mix with prefix groups"
    (define (same? a b) (equal? (mtcars-names a) (mtcars-names b)))
    (check-true (same? (mpg . ~ . wt * hp) (~ mpg (* wt hp))))
    (check-true (same? (mpg . ~ . wt + hp : qsec) (~ mpg wt (: hp qsec))))
    (check-true (same? (mpg . ~ . wt : hp * qsec) (~ mpg (* (: wt hp) qsec))))
    (check-true (same? (mpg . ~ . (wt + hp) ^ 2) (~ mpg (^ (+ wt hp) 2))))
    (check-true (same? (mpg . ~ . wt + hp ^ 2) (~ mpg wt hp)))
    (check-true (same? (mpg . ~ . wt * hp - hp) (~ mpg (- (* wt hp) hp))))
    (check-true (same? (mpg . ~ . wt - hp + hp) (~ mpg wt hp)))
    (check-true (same? (mpg . ~ . wt + (* hp qsec) - qsec) (~ mpg wt hp (: hp qsec))))
    (check-true (same? (~ mpg (wt + hp) : qsec) (~ mpg (: (+ wt hp) qsec))))
    (check-true (same? (~ mpg (- 1 + wt)) (~ mpg 0 wt))))

  (test-case "a model predicts from tables keyed by symbols, whose names become strings"
    (define m (formula-fit (mpg . ~ . wt + hp) mtcars #:lambda 0.1))
    (define symbols (for/list ([column (in-list mtcars)])
                      (cons (string->symbol (car column)) (cdr column))))
    (check-equal? (predict m symbols) (predict m mtcars))
    (define named (rows->design-matrix (rows-of mtcars '("hp" "wt")) #:column-names '(hp wt)))
    (check-equal? (design-matrix-column-names named) '("hp" "wt"))
    (check-equal? (predict m named) (predict m mtcars))
    (check-equal? (formula-model-fit (formula-fit (mpg . ~ . wt + hp) symbols #:lambda 0.1))
                  (formula-model-fit m)))

  (test-case "a symbol table's names and a formula's strings match"
    (define symbols (for/list ([column (in-list mtcars)])
                      (cons (string->symbol (car column)) (cdr column))))
    (check-equal? (formula-predictor-names (~ "mpg" "wt" * hp) symbols) '("wt" "hp" "wt:hp")))

  ;; The columns of a design matrix, by name.
  (define (columns-of dm)
    (for/hash ([name (in-list (design-matrix-column-names dm))]
               [column (in-list (design-matrix->columns dm))])
      (values name column)))

  (test-case "an interaction column is the product of its variables' columns"
    (define x (columns-of (formula-design-matrix (~ mpg wt * hp + (: wt hp qsec)) mtcars)))
    (define (column name) (map exact->inexact (table-column mtcars name)))
    (check-equal? (hash-ref x "wt") (column "wt"))
    (check-equal? (hash-ref x "wt:hp") (map * (column "wt") (column "hp")))
    (check-equal? (hash-ref x "wt:hp:qsec") (map * (column "wt") (column "hp") (column "qsec")))
    (check-equal? (design-matrix-column-names (formula-design-matrix (~ mpg (* hp wt)) mtcars))
                  '("hp" "wt" "hp:wt")))

  (test-case "the design matrix is what a formula fit fits"
    (define f (~ mpg (^ (+ wt hp qsec) 2)))
    (define x (formula-design-matrix f mtcars))
    (define y (table-column mtcars "mpg"))
    (check-same (formula-path f mtcars #:lambda '(1.0 0.1)) (elnet-path x y #:lambda '(1.0 0.1)))
    (check-equal? (formula-model-predictor-names (formula-fit f mtcars #:lambda 0.1))
                  (design-matrix-column-names x)))

  ;; --- the intercept -------------------------------------------------------------------

  (define mtcars-rows (rows-of mtcars '("wt" "hp")))
  (define mpg (table-column mtcars "mpg"))

  (test-case "0 and - 1 fit without an intercept; 1 keeps it; the last one wins"
    (define without (elnet-fit mtcars-rows mpg #:lambda 0.1 #:intercept? #f))
    (define with (elnet-fit mtcars-rows mpg #:lambda 0.1))
    (for ([f (list (~ mpg 0 wt hp) (mpg . ~ . wt + hp - 1) (mpg . ~ . - 1 + wt + hp)
                   (~ mpg (- (+ wt hp) 1)) (mpg . ~ . 1 + wt + hp - 1) (~ mpg wt hp (- 1)))])
      (check-same (formula-fit f mtcars #:lambda 0.1) without)
      (check-same (formula-fit f mtcars #:lambda 0.1 #:intercept? #f) without))
    (for ([f (list (~ mpg wt hp) (~ mpg 1 wt hp) (mpg . ~ . 0 + wt + hp + 1)
                   (mpg . ~ . wt - 0 + hp) (~ mpg (- (+ wt hp) (+ 0 qsec))))])
      (check-same (formula-fit f mtcars #:lambda 0.1) with))
    (check-equal? (assoc "(Intercept)" (coef (formula-fit (~ mpg 0 wt hp) mtcars #:lambda 0.1)))
                  '("(Intercept)" . 0.0))
    (check-same (formula-fit (~ mpg wt hp) mtcars #:lambda 0.1 #:intercept? #f) without))

  ;; R 4.5.3's attr(terms(f), "intercept"): 1 for mpg ~ wt + hp + -0, whose
  ;; unary minus flips what 0 means, and 0 for mpg ~ wt + hp - -0, which flips
  ;; it twice; 0 for + +0 and 1 for - +0. The reader reads -0 and +0 as 0,
  ;; which would invert two of the four, so the message names no sign.
  (test-case "a sign glued to 0 or 1 is a syntax error; (- 0) is R's -0, both ways"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (define glued-0
      #rx"~: 0 is written with more than its digit, and the reader reads the rest away, as it does a sign glued to it, which R reads as an operator; write 0, \\(\\+ 0\\) or \\(- 0\\)\n  at: 0")
    (check-regexp-match glued-0
                        (message-of (lambda () (convert-compile-time-error (mpg . ~ . wt + hp + -0)))))
    (check-regexp-match glued-0
                        (message-of (lambda () (convert-compile-time-error (mpg . ~ . wt + hp - -0)))))
    (check-regexp-match glued-0
                        (message-of (lambda () (convert-compile-time-error (~ mpg wt hp -0)))))
    (check-regexp-match glued-0
                        (message-of (lambda () (convert-compile-time-error (~ mpg (- (+ wt hp) +0))))))
    (check-regexp-match glued-0
                        (message-of (lambda () (convert-compile-time-error (mpg . ~ . wt + hp + +0)))))
    (check-regexp-match glued-0
                        (message-of (lambda () (convert-compile-time-error (~ mpg wt hp 00)))))
    (define glued-1
      #rx"~: 1 is written with more than its digit, and the reader reads the rest away, as it does a sign glued to it, which R reads as an operator; write 1, \\(\\+ 1\\) or \\(- 1\\)\n  at: 1")
    (check-regexp-match glued-1
                        (message-of (lambda () (convert-compile-time-error (mpg . ~ . +1 + wt + hp)))))
    (check-regexp-match glued-1
                        (message-of (lambda () (convert-compile-time-error (~ mpg #e1 wt hp)))))
    (check-same (formula-fit (mpg . ~ . wt + hp + (+ 0)) mtcars #:lambda 0.1)
                (elnet-fit mtcars-rows mpg #:lambda 0.1 #:intercept? #f))
    (check-same (formula-fit (mpg . ~ . wt + hp - (+ 0)) mtcars #:lambda 0.1)
                (elnet-fit mtcars-rows mpg #:lambda 0.1))
    (define without (elnet-fit mtcars-rows mpg #:lambda 0.1 #:intercept? #f))
    (define with (elnet-fit mtcars-rows mpg #:lambda 0.1))
    (for ([f (list (mpg . ~ . wt + hp + (- 0)) (~ mpg wt hp (- 0)) (mpg . ~ . - 0 + wt + hp))])
      (check-same (formula-fit f mtcars #:lambda 0.1) with))
    (for ([f (list (mpg . ~ . wt + hp - (- 0)) (~ mpg (- (+ wt hp) (- 0))))])
      (check-same (formula-fit f mtcars #:lambda 0.1) without)))

  (define-syntax (intercept-formula stx)
    (syntax-parse stx
      [(_ keep?:boolean)
       #:with n (datum->syntax #'keep? (if (syntax-e #'keep?) 1 0))
       #'(~ mpg n wt hp)]))
  (define-syntax (intercept-formula/borrowed stx)
    (syntax-parse stx
      [(_ keep?:boolean)
       #:with n (datum->syntax #'keep? (if (syntax-e #'keep?) 1 0) #'keep?)
       #'(~ mpg n wt hp)]))

  (test-case "a 0 or 1 that a macro builds has a location of its own, or none"
    (check-equal? (intercept-formula #f) (~ mpg 0 wt hp))
    (check-equal? (intercept-formula #t) (~ mpg 1 wt hp))
    (check-exn #rx"~: 0 is written with more than its digit"
               (lambda () (convert-compile-time-error (intercept-formula/borrowed #f)))))

  (test-case "an #:intercept? that contradicts the formula's 1, 0 or - 1 blames the caller"
    (check-exn (blame-matching
                #rx"^formula-fit: contract violation;\n the intercept\\? argument contradicts the formula's intercept\n  expected: #f, since the formula \\(~ mpg 0 wt hp\\) has no intercept\n  given: #t")
               (lambda () (formula-fit (~ mpg 0 wt hp) mtcars #:lambda 0.1 #:intercept? #t)))
    (check-exn (blame-matching
                #rx"^formula-path: .*expected: #t, since the formula \\(~ mpg 1 \\+ wt\\) has an intercept term\n  given: #f")
               (lambda () (formula-path (~ mpg 1 + wt) mtcars #:intercept? #f)))
    (check-exn (blame-matching #rx"^formula-cv: .*in: the intercept\\? argument")
               (lambda () (formula-cv (mpg . ~ . wt - 1) mtcars #:intercept? #t)))
    (check-exn (blame-matching #rx"expected: boolean\\?")
               (lambda () (formula-fit (~ mpg wt) mtcars #:lambda 0.1 #:intercept? 'yes))))

  (test-case "the Cox family fits no intercept, and 0 changes only how a factor is coded, as in R"
    (define fit (formula-fit (~ (surv time status) karno age) veteran #:family 'cox #:lambda 0.05))
    (check-same (formula-fit (~ (surv time status) 0 karno age) veteran #:family 'cox #:lambda 0.05)
                (formula-model-fit fit))
    (check-same (formula-fit (~ (surv time status) 1 + karno + age) veteran #:family 'cox
                             #:lambda 0.05 #:intercept? #f)
                (formula-model-fit fit))
    ;; R: model.matrix(Surv(time, status) ~ 0 + karno + factor(trt), veteran)
    ;; has karno factor(trt)1 factor(trt)2, and without the 0, (Intercept)
    ;; karno factor(trt)2.
    (check-equal? (formula-predictor-names (~ (surv time status) 0 karno (factor trt)) veteran)
                  '("karno" "(factor trt)1" "(factor trt)2"))
    (check-equal? (formula-predictor-names (~ (surv time status) karno (factor trt)) veteran)
                  '("karno" "(factor trt)2"))
    (define trt (for/list ([t (in-list (cdr (assoc "trt" veteran)))]) (if (= t 1) '(1 0) '(0 1))))
    (check-same (formula-fit (~ (surv time status) 0 karno (factor trt)) veteran
                             #:family 'cox #:lambda 0.05)
                (cox-fit (map cons (cdr (assoc "karno" veteran)) trt)
                         (cdr (assoc "time" veteran)) (cdr (assoc "status" veteran))
                         #:lambda 0.05)))

  (test-case "a formula without predictors is an error when it is fitted"
    (check-equal? (formula-predictor-names (~ mpg 1) mtcars) '())
    (check-equal? (formula-predictor-names (~ mpg) mtcars) '())
    (for ([f (list (~ mpg) (~ mpg 1) (~ mpg 0) (mpg . ~ . - 1) (~ mpg (- wt wt)) (~ mpg (: 1 wt)))])
      (check-exn #rx"^formula-fit: the formula has no predictors, and glmnet needs at least one\n  formula: "
                 (lambda () (formula-fit f mtcars #:lambda 0.1))))
    (check-exn #rx"^formula-cv: the formula has no predictors"
               (lambda () (formula-cv (~ y (- all a b c d)) letters)))
    (check-exn #rx"^formula-design-matrix: the formula has no predictors"
               (lambda () (formula-design-matrix (~ mpg 1) mtcars))))

  ;; --- the response on the right-hand side ---------------------------------------------

  (define (glmnet-warnings thunk)
    (define receiver (make-log-receiver (current-logger) 'warning 'glmnet))
    (define result (thunk))
    (values result
            (let loop ()
              (match (sync/timeout 0 receiver)
                [#f '()]
                [(vector _ message _ _) (cons message (loop))]))))

  (test-case "a path that stops early is logged in the formula procedure's name"
    (define-values (path path-warnings)
      (glmnet-warnings
       (lambda () (formula-path (~ mpg wt hp) mtcars #:lambda '(100.0 0.1 0.001) #:max-iters 1))))
    (check-equal? (glmnet-path-lambda (formula-model-fit path)) #(100.0))
    (check-match path-warnings
                 (list (regexp #rx"^glmnet: formula-path: the path stops after 1 lambda: convergence")))
    (define-values (cv cv-warnings)
      (glmnet-warnings
       (lambda () (formula-cv (~ mpg wt hp) mtcars #:lambda '(100.0 0.1 0.001) #:max-iters 1
                              #:fold-ids (for/list ([i (in-range 32)]) (modulo i 3))))))
    (check-match cv-warnings
                 (list (regexp #rx"^glmnet: formula-cv: fitting all the data: the path stops after 1")
                       (regexp #rx"^glmnet: formula-cv: fitting the training data of held-out fold 0: ")
                       (regexp #rx"^glmnet: formula-cv: fitting the training data of held-out fold 1: ")
                       (regexp #rx"^glmnet: formula-cv: fitting the training data of held-out fold 2: "))))

  (test-case "a response column alone on the right-hand side is dropped with a warning, as in R"
    (define-values (names warnings)
      (glmnet-warnings (lambda () (mtcars-names (mpg . ~ . 1 + wt + mpg + (* wt mpg))))))
    ;; R: mpg ~ 1 + wt + mpg + wt*mpg gives the columns wt and mpg:wt.
    (check-equal? names '("wt" "mpg:wt"))
    (check-equal? warnings
                  '("glmnet: formula-predictor-names: the response column \"mpg\" appeared on the right-hand side and was dropped"))
    (define-values (fit fit-warnings)
      (glmnet-warnings (lambda () (formula-fit (~ mpg wt hp mpg) mtcars #:lambda 0.1))))
    (check-equal? (formula-model-predictor-names fit) '("wt" "hp"))
    (check-regexp-match #rx"^glmnet: formula-fit: the response column \"mpg\"" (car fit-warnings))
    (check-same fit (elnet-fit mtcars-rows mpg #:lambda 0.1))
    (define-values (none none-warnings) (glmnet-warnings (lambda () (mtcars-names (~ mpg all)))))
    (check-equal? none-warnings '())
    (check-exn #rx"formula-fit: the formula has no predictors"
               (lambda () (formula-fit (~ mpg mpg) mtcars #:lambda 0.1))))

  (test-case "every column of a Cox or multi-response response is dropped in the same way"
    (define-values (names warnings)
      (glmnet-warnings
       (lambda () (formula-predictor-names (~ (surv a b) c b (: b d)) letters))))
    (check-equal? names '("c" "b:d"))
    (check-equal? (length warnings) 1)
    (check-equal? (formula-predictor-names (~ (y d) a y d) letters) '("a")))

  ;; --- the grammar -----------------------------------------------------------------------

  (test-case "a column named after a word of the language is written as a string"
    (define t (list (cons "all" '(1 2 3)) (cons "surv" '(3 1 2)) (cons "+" '(0 1 0))
                    (cons "a:b" '(1 1 0))))
    (check-equal? (formula-predictor-names (~ "all" "surv" "+") t) '("surv" "+"))
    (check-equal? (formula-predictor-names (~ "all" all) t) '("surv" "+" "a:b"))
    (check-equal? (formula-predictor-names (~ "all" "a:b" * "+") t) '("a:b" "+" "a:b:+"))
    (check-exn #rx"~: all is a word of the formula language; write a column with this name as a string, \"all\""
               (lambda () (convert-compile-time-error (~ all x))))
    (check-exn #rx"~: surv is a word of the formula language.*\"surv\""
               (lambda () (convert-compile-time-error (~ y a surv))))
    (check-exn #rx"~: [+] is a word of the formula language.*\"[+]\""
               (lambda () (convert-compile-time-error (~ + x)))))

  (test-case "an operator written without spaces is a syntax error that says so"
    (check-exn #rx"~: wt:hp reads as one name; put spaces around an operator, as in wt : hp, or write a column with this name as a string, \"wt:hp\"\n  at: wt:hp"
               (lambda () (convert-compile-time-error (~ mpg wt:hp))))
    (check-exn #rx"~: x\\^2 reads as one name"
               (lambda () (convert-compile-time-error (~ y x^2))))
    (check-exn #rx"~: a\\*b reads as one name"
               (lambda () (convert-compile-time-error (~ y (+ c a*b)))))
    (check-exn #rx"~: -wt reads as one name"
               (lambda () (convert-compile-time-error (~ mpg -wt + hp))))
    (check-equal? (formula-response (~ blood-pressure age)) 'blood-pressure))

  (test-case "R's operators that the language lacks are errors that say so"
    (check-exn #rx"~: / is an operator of R's formulas that this formula language does not have; for a ratio, write \\(I \\(/ x z\\)\\)\n  at: /"
               (lambda () (convert-compile-time-error (y . ~ . a / b))))
    (check-exn #rx"~: %in% is an operator of R's formulas"
               (lambda () (convert-compile-time-error (~ y (a %in% b)))))
    (check-exn #rx"~: / is an operator"
               (lambda () (convert-compile-time-error (~ y a /))))
    (check-exn #rx"~: / is an operator of R's formulas that this formula language does not have; for a ratio, write \\(I \\(/ a b\\)\\)\n  at: /"
               (lambda () (convert-compile-time-error (~ y (/ a b)))))
    (check-exn #rx"~: %in% is an operator of R's formulas that this formula language does not have\n  at: %in%"
               (lambda () (convert-compile-time-error (~ y a (+ b (%in% a b))))))
    (check-exn #rx"~: a/b reads as one name"
               (lambda () (convert-compile-time-error (~ y a/b))))
    (check-exn exn:fail:contract? (lambda () (make-formula 'y 'a '/ 'b))))

  (test-case "a malformed term is an error at the form that is wrong"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (check-regexp-match
     #rx"~: \\(1 x \\(sqr x\\)\\) is not a term: a group of terms starts with an operator, as \\(\\+ x z\\) does, or has operators between its terms, as \\(x \\+ z\\) does\n  at: \\(1 x \\(sqr x\\)\\)"
     (message-of (lambda () (convert-compile-time-error (y . ~ . (+ (1 x (sqr x))))))))
    (check-regexp-match
     #rx"~: \\(squared x\\) is not a term: squared is not bound, so it is not a transform, and a group of terms starts with an operator, as \\(\\+ x z\\) does, or has operators between its terms, as \\(x \\+ z\\) does\n  at: \\(squared x\\)"
     (message-of (lambda () (convert-compile-time-error (~ y x (squared x))))))
    (check-regexp-match #rx"~: \\(a b\\) is not a term: a is not bound"
                        (message-of (lambda () (convert-compile-time-error (~ y (+ x (a b)))))))
    (check-regexp-match #rx"~: expected an infix operator \\(\\+, -, \\*, : or \\^\\) between two terms\n  at: b"
                        (message-of (lambda () (convert-compile-time-error (~ y a b + c)))))
    (check-regexp-match #rx"~: expected a power, an exact integer of at least 2\n  at: 1"
                        (message-of (lambda () (convert-compile-time-error (~ y (^ (+ a b) 1))))))
    (check-regexp-match #rx"~: expected a power.*\n  at: z"
                        (message-of (lambda () (convert-compile-time-error (~ y x ^ z)))))
    (check-regexp-match #rx"~: expected more terms"
                        (message-of (lambda () (convert-compile-time-error (~ y (* a))))))
    (check-regexp-match #rx"~: expected a term.*\n  at: \\+"
                        (message-of (lambda () (convert-compile-time-error (~ y x * + z)))))
    (check-regexp-match #rx"~: 3 is not a term; a formula's numbers are 1, 0 and a power after \\^\n  at: 3"
                        (message-of (lambda () (convert-compile-time-error (~ y 3)))))
    (check-regexp-match #rx"~: -1 is a number; to remove a term, put a space after the sign, as in - 1\n  at: -1"
                        (message-of (lambda () (convert-compile-time-error (y . ~ . -1 + x)))))
    (check-regexp-match #rx"~: a power cannot be raised again; R reads x \\^ 2 \\^ 3 as x \\^ \\(2 \\^ 3\\), which is not a power\n  at: \\^"
                        (message-of (lambda () (convert-compile-time-error (y . ~ . (a + b) ^ 2 ^ 3)))))
    (check-equal? (format "~a" (y . ~ . ((a + b) ^ 2) ^ 3)) "(~ y ((a + b) ^ 2) ^ 3)")
    (check-regexp-match #rx"~: the right-hand side of a formula is a list of terms, and this one has a dot before hp\n  at: hp"
                        (message-of (lambda () (convert-compile-time-error (~ mpg wt . hp)))))
    (check-regexp-match #rx"~: the right-hand side .* a dot before hp\n"
                        (message-of (lambda () (convert-compile-time-error (~ mpg wt + . hp)))))
    (check-regexp-match #rx"~: the right-hand side .* a dot before hp\n"
                        (message-of (lambda () (convert-compile-time-error (~ mpg . hp)))))
    (check-regexp-match #rx"~: expected a term.*\n  at: \\(wt \\. hp\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (wt . hp))))))
    (check-regexp-match #rx"~: expected more terms starting with a column name"
                        (message-of (lambda () (convert-compile-time-error (~ (surv t) all))))))

  (test-case "make-formula takes the same data, and its contract checks it"
    (check-equal? (make-formula 'y '(* a b)) (~ y (* a b)))
    (check-equal? (make-formula 'y) (~ y))
    (check-equal? (make-formula 'y '- 1 '+ '(a + b) '^ 2) (~ y - 1 + (a + b) ^ 2))
    (check-equal? (make-formula 'y '(- 1 + a)) (~ y (- 1 + a)))
    (for ([bad (list '((* a)) '(a b + c) '((^ a 1)) '(a ^ b) '((sqr x)) '(a:b) '(3) '(+)
                     '(a +) '((1 x)) '(()) '(a ^ 2 ^ 3) '(-a) '(-1 + a) '(a / b))])
      (check-exn (blame-matching #rx"make-formula: contract violation.*expected: formula-rhs/c")
                 (lambda () (apply make-formula 'y bad))
                 (format "~s" bad)))
    (check-exn exn:fail:contract? (lambda () (make-formula 'all 'x)))
    (check-exn exn:fail:contract? (lambda () (make-formula '(surv t) 'x))))

  (test-case "the form of the response must suit #:family, a contract on the formula"
    (check-exn (blame-matching
                #rx"formula-fit: contract violation;\n the Cox family needs a \\(surv time status\\) response\n  expected: a formula with a \\(surv time status\\) response, for the cox family\n  given: \\(~ time all\\)\n  in: the f argument")
               (lambda () (formula-fit (~ time all) veteran #:family 'cox #:lambda 0.1)))
    (check-exn (blame-matching
                #rx"formula-path: .*\\(surv time status\\) response is for the Cox family.*for the gaussian family")
               (lambda () (formula-path (~ (surv time status) all) veteran)))
    (check-exn (blame-matching #rx"formula-cv: .*\\(surv time status\\) response is for the Cox family")
               (lambda () (formula-cv (~ (surv time status) all) veteran #:family 'mgaussian)))
    (check-exn (blame-matching
                #rx"several columns is for the mgaussian family.*for the binomial family")
               (lambda () (formula-fit (~ (weight waist) all) linnerud #:family 'binomial
                                       #:lambda 0.1)))
    (check-exn (blame-matching #rx"expected: formula\\?\n  given: '\\(~ y all\\)")
               (lambda () (formula-fit '(~ y all) linnerud #:lambda 0.1)))
    (check-exn (blame-matching #rx"in: the family argument")
               (lambda () (formula-fit (~ y a) letters #:family 'logistic #:lambda 0.1))))

  (test-case "the type measure must be one of the family's, a contract on #:type-measure"
    (check-exn (blame-matching
                #rx"formula-cv: contract violation;\n the gaussian family has no such type measure\n  expected: #f or one of '\\(mse deviance mae\\)\n  given: 'auc\n  in: the type-measure argument")
               (lambda () (formula-cv (~ Employed all) longley #:type-measure 'auc)))
    (check-exn (blame-matching #rx"the binomial family has no such type measure.*given: 'C")
               (lambda () (formula-cv (~ diagnosis all) wdbc #:family 'binomial #:type-measure 'C)))
    (check-exn (blame-matching #rx"the cox family has no such type measure.*given: 'auc")
               (lambda () (formula-cv (~ (surv time status) all) veteran #:family 'cox
                                      #:type-measure 'auc))))

  (test-case "the response values must suit the family"
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
                                       #:lambda 0.1))))

  (define (labelled labels)
    (list (cons "k" labels) (cons "a" '(1 2 3 4 5 6)) (cons "b" '(2 1 4 3 6 5))))

  (test-case "multinomial class labels run from 0 with none missing, checked by name"
    (check-exn #rx"formula-fit: a multinomial response must use every class label from 0 to its largest\n  column: \"k\"\n  missing label: 0\n  largest label: 3"
               (lambda () (formula-fit (~ k a b) (labelled '(1 2 3 1 2 3))
                                       #:family 'multinomial #:lambda 0.1)))
    (check-exn #rx"formula-path: .*every class label.*missing label: 1\n  largest label: 2"
               (lambda () (formula-path (~ k a b) (labelled '(0 2 0 2 0 2)) #:family 'multinomial)))
    (check-exn #rx"formula-cv: .*every class label.*missing label: 2\n  largest label: 1000000000000"
               (lambda () (formula-cv (~ k a b) (labelled '(0 1 0 1 0 1e12)) #:family 'multinomial)))
    (check-exn #rx"formula-fit: a multinomial response needs at least two classes\n  column: \"k\""
               (lambda () (formula-fit (~ k a b) (labelled '(0 0 0 0 0 0))
                                       #:family 'multinomial #:lambda 0.1))))

  (test-case "an error from the family's procedure names the formula procedure"
    (check-exn #rx"^formula-cv: fold-ids does not have one entry per row of the table\n  length of fold-ids: 3\n  rows of the table: 6"
               (lambda () (formula-cv (~ k a b) (labelled '(0 1 0 1 1 1)) #:fold-ids '(0 1 2))))
    ;; More folds than rows, the default of 10 included, is the caller's error,
    ;; named for formula-cv; it must not blame formula.rkt.
    (define ((too-many-folds? n) e)
      (and
       (exn:fail:contract? e)
       (not (exn:fail:contract:blame? e))
       (regexp-match?
        (pregexp
         (format "^formula-cv: there are more folds than observations\n  folds: ~a\n  observations: 6"
                 n))
        (exn-message e))))
    (check-exn (too-many-folds? 10) (lambda () (formula-cv (~ k a b) (labelled '(0 1 0 1 1 1)))))
    (check-exn (too-many-folds? 7)
               (lambda () (formula-cv (~ k a b) (labelled '(0 1 0 1 1 1)) #:nfolds 7)))
    (check-exn #rx"^formula-fit: at least one observation must be an event"
               (lambda () (formula-fit (~ (surv a k) b) (labelled '(0 0 0 0 0 0))
                                       #:family 'cox #:lambda 0.1)))
    (define constant (list (cons "y" '(1 2 3 4 5 6)) (cons "k" '(1 1 1 1 1 1))))
    (check-exn #rx"^formula-path: all used predictors have zero variance"
               (lambda () (formula-path (~ y k) constant #:lambda '(0.1))))
    (check-exn #rx"^formula-cv: .*all used predictors have zero variance"
               (lambda () (formula-cv (~ y k) constant #:nfolds 3))))

  ;; --- transforms --------------------------------------------------------------------

  ;; A column of mtcars, as flonums, as R holds it.
  (define (mtcars-column name) (map exact->inexact (table-column mtcars name)))

  (test-case "a transform is a function of columns, named by its source, and joins the algebra"
    ;; R: mpg ~ log(hp) * wt gives log(hp) wt log(hp):wt
    (check-equal? (mtcars-names (mpg . ~ . (log hp) * wt)) '("(log hp)" "wt" "(log hp):wt"))
    ;; R: mpg ~ (log(hp) + wt + qsec)^2
    (check-equal? (mtcars-names (~ mpg (^ (+ (log hp) wt qsec) 2)))
                  '("(log hp)" "wt" "qsec" "(log hp):wt" "(log hp):qsec" "wt:qsec"))
    ;; R: mpg ~ log(hp):wt + qsec
    (check-equal? (mtcars-names (~ mpg (: (log hp) wt) qsec)) '("qsec" "(log hp):wt"))
    (define x (columns-of (formula-design-matrix
                           (mpg . ~ . (log hp) * wt + (sqrt disp) + (exp wt) + (log hp 2)) mtcars)))
    (define log-hp (map log (mtcars-column "hp")))
    (check-equal? (hash-ref x "(log hp)") log-hp)
    (check-equal? (hash-ref x "(log hp):wt") (map * log-hp (mtcars-column "wt")))
    (check-equal? (hash-ref x "(sqrt disp)") (map sqrt (mtcars-column "disp")))
    (check-equal? (hash-ref x "(exp wt)") (map exp (mtcars-column "wt")))
    (check-equal? (hash-ref x "(log hp 2)") (map (lambda (v) (log v 2)) (mtcars-column "hp"))))

  (test-case "(I expr) is Racket arithmetic, where the operators are Racket's, as in R's I()"
    (define x (columns-of (formula-design-matrix
                           (~ mpg (I (expt hp 2)) (sqr hp) (I (* wt hp)) (: wt hp) (I (/ hp wt))
                              (I (+ hp 1)) (log (+ hp 1)) (I hp))
                           mtcars)))
    (define hp (mtcars-column "hp"))
    (define wt (mtcars-column "wt"))
    (check-equal? (hash-ref x "(I (expt hp 2))") (map * hp hp))
    (check-equal? (hash-ref x "(sqr hp)") (map * hp hp))
    (check-equal? (hash-ref x "(I (* wt hp))") (hash-ref x "wt:hp"))
    (check-equal? (hash-ref x "(I (/ hp wt))") (map / hp wt))
    (check-equal? (hash-ref x "(I (+ hp 1))") (map add1 hp))
    (check-equal? (hash-ref x "(log (+ hp 1))") (map (lambda (v) (log (add1 v))) hp))
    (check-equal? (hash-ref x "(I hp)") hp))

  (test-case "R's x^2 is crossing, x with itself, and a square is a transform"
    ;; R: mpg ~ hp + hp^2 is mpg ~ hp; mpg ~ hp + I(hp^2) has two columns.
    (check-equal? (mtcars-names (mpg . ~ . hp + hp ^ 2)) '("hp"))
    (check-equal? (mtcars-names (mpg . ~ . hp + (sqr hp))) '("hp" "(sqr hp)"))
    (check-equal? (mtcars-names (mpg . ~ . hp + (I (expt hp 2)))) '("hp" "(I (expt hp 2))")))

  (test-case "a name in a transform is the table's column if it has one, else the Racket binding"
    ;; mpg is also this module's variable, a list; the column wins, as in R.
    (define x (columns-of (formula-design-matrix (~ wt (I (* 2 mpg))) mtcars)))
    (check-equal? (hash-ref x "(I (* 2 mpg))") (map (lambda (v) (* 2 v)) (mtcars-column "mpg")))
    (define hp 'not-a-number)
    (check-equal? (hash-ref (columns-of (formula-design-matrix (~ mpg (log hp)) mtcars)) "(log hp)")
                  (map log (mtcars-column "hp")))
    (define k 100)
    (check-equal? (hash-ref (columns-of (formula-design-matrix (~ mpg (I (/ disp k))) mtcars))
                            "(I (/ disp k))")
                  (map (lambda (v) (/ v 100)) (mtcars-column "disp")))
    ;; A function in argument position, and names that the transform binds itself.
    (check-equal? (hash-ref (columns-of (formula-design-matrix
                                         (~ mpg (I (apply max (map abs (list hp (- wt)))))) mtcars))
                            "(I (apply max (map abs (list hp (- wt)))))")
                  (mtcars-column "hp"))
    (check-equal? (hash-ref (columns-of (formula-design-matrix
                                         (~ mpg (I (let ([z (* 2 hp)]) (+ z 1)))) mtcars))
                            "(I (let ((z (* 2 hp))) (+ z 1)))")
                  (map (lambda (v) (+ (* 2 v) 1)) (mtcars-column "hp")))
    (check-equal? (hash-ref (columns-of (formula-design-matrix
                                         (~ mpg (I (for/sum ([i (in-range 3)]) (* i hp)))) mtcars))
                            "(I (for/sum ((i (in-range 3))) (* i hp)))")
                  (map (lambda (v) (* 3 v)) (mtcars-column "hp")))
    ;; A column named after a macro, such as racket/base's time.
    (define timed (list (cons "y" '(1 2 3)) (cons "time" '(1 2 4))))
    (check-equal? (hash-ref (columns-of (formula-design-matrix (~ y (log time)) timed)) "(log time)")
                  (list 0.0 (log 2.0) (log 4.0))))

  (test-case "a column named like a function is the column as an argument and the function at the head"
    ;; R: y ~ pmax(max, x) is 4 5 9: R looks up a call's function by name,
    ;; skipping the column max.
    (define t (list (cons "y" '(1 2 3)) (cons "max" '(1 5 9)) (cons "x" '(4 4 4))))
    (check-equal? (columns-of (formula-design-matrix (~ y (max max x)) t))
                  (hash "(max max x)" '(4.0 5.0 9.0)))
    (check-equal? (columns-of (formula-design-matrix (~ y (I (+ 1 (max max x)))) t))
                  (hash "(I (+ 1 (max max x)))" '(5.0 6.0 10.0)))
    (check-equal? (mtcars-names (~ mpg (I (abs (- hp (min hp wt)))))) '("(I (abs (- hp (min hp wt))))")))

  (test-case "a transform reads the names that it does not bind itself, and needs only those columns"
    ;; The lambda's hp is its own, so the transform reads wt alone, and
    ;; predict needs no hp.
    (define m (formula-fit (~ mpg (I ((lambda (hp) (* 2 hp)) wt))) mtcars #:lambda 0.1))
    (define wt (mtcars-column "wt"))
    (check-equal? (predict m (list (cons "wt" '(3.0))))
                  (predict (formula-model-fit m) '((6.0))))
    (check-equal? (formula-model-fit m)
                  (elnet-fit (map (lambda (v) (list (* 2 v))) wt) mpg #:lambda 0.1))
    ;; A transform whose let binds every name it uses reads no column.
    (check-exn #rx"^formula-predictor-names: a transform must read a column of the table\n  transform: \"\\(I \\(let \\(\\(hp 5.0\\)\\) hp\\)\\)\""
               (lambda () (mtcars-names (~ mpg (I (let ([hp 5.0]) hp)) wt))))
    ;; Quoted data is data: its names are not read.
    (check-equal? (hash-ref (columns-of (formula-design-matrix
                                         (~ mpg (I (+ hp (length '(wt * qsec ^ 2))))) mtcars))
                            "(I (+ hp (length '(wt * qsec ^ 2))))")
                  (map (lambda (v) (+ v 5)) (mtcars-column "hp"))))

  (test-case "=>, else, unquote and the pattern literals keep their Racket meaning in a transform"
    (define f (~ mpg (I (cond [(memv hp '(110.0)) => (lambda (l) 1.0)] [else 0.0]))
                 (I (cdr `(,hp . ,wt)))
                 (I (match (list hp wt) [(list _ _) 1.0]))))
    (define x (columns-of (formula-design-matrix f mtcars)))
    (check-equal? (hash-ref x "(I (cond ((memv hp '(110.0)) => (lambda (l) 1.0)) (else 0.0)))")
                  (for/list ([v (in-list (mtcars-column "hp"))]) (if (= v 110.0) 1.0 0.0)))
    (check-equal? (hash-ref x "(I (cdr `(,hp . ,wt)))") (mtcars-column "wt"))
    (check-equal? (hash-ref x "(I (match (list hp wt) ((list _ _) 1.0)))")
                  (for/list ([v (in-list (mtcars-column "hp"))]) 1.0)))

  (test-case "a function or ^ that a transform binds itself is Racket's, not R's infix arithmetic"
    (define x (columns-of (formula-design-matrix
                           (~ mpg (I (let ([combine (lambda (f a b) (f a b))]) (combine * hp wt)))
                              (I (let ([^ expt]) (^ hp 2))))
                           mtcars)))
    (check-equal? (hash-ref x "(I (let ((combine (lambda (f a b) (f a b)))) (combine * hp wt)))")
                  (map * (mtcars-column "hp") (mtcars-column "wt")))
    (check-equal? (hash-ref x "(I (let ((^ expt)) (^ hp 2)))")
                  (map sqr (mtcars-column "hp"))))

  (test-case "a quoted name is a syntax error that says to write the name"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (check-regexp-match #rx"~: 'hp is quoted, and ~ quotes the names of a formula itself: write the column as hp or \"hp\"\n  at: \\(quote hp\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg 'hp 'wt)))))
    (check-regexp-match #rx"~: `hp is quoted.*write the column as hp or \"hp\""
                        (message-of (lambda () (convert-compile-time-error (mpg . ~ . `hp + wt)))))
    (check-regexp-match #rx"~: '\"blood pressure\" is quoted.*write the column as \"blood pressure\""
                        (message-of (lambda () (convert-compile-time-error (~ bp '"blood pressure")))))
    (check-regexp-match #rx"~: '\\(log hp\\) is quoted.*write \\(log hp\\) without the quote"
                        (message-of (lambda () (convert-compile-time-error (~ mpg '(log hp))))))
    (check-regexp-match #rx"~: 'mpg is quoted.*write the column as mpg or \"mpg\""
                        (message-of (lambda () (convert-compile-time-error (~ 'mpg hp))))))

  (test-case "R's / is a syntax error that says how to write a ratio"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (check-regexp-match #rx"~: / is an operator of R's formulas that this formula language does not have; for a ratio, write \\(I \\(/ hp wt\\)\\)\n  at: /"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (/ hp wt))))))
    (check-regexp-match #rx"~: / is an operator.*; for a ratio, write \\(I \\(/ hp wt\\)\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (hp . / . wt))))))
    (check-regexp-match #rx"~: / is an operator.*; for a ratio, write \\(I \\(/ x z\\)\\)\n  at: /"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (/ hp wt qsec))))))
    (check-regexp-match #rx"~: %in% is an operator of R's formulas that this formula language does not have\n  at: %in%"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (%in% hp wt)))))))

  (test-case "a transformed response is a syntax error, and a response of columns is not"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (define transformed #rx"~: a transformed response is not supported; add the transformed column to the table, or write the columns of a response of several columns as strings")
    (check-regexp-match transformed (message-of (lambda () (convert-compile-time-error (~ (I mpg) wt)))))
    (check-regexp-match transformed
                        (message-of (lambda () (convert-compile-time-error (~ (log (+ mpg 1)) wt)))))
    ;; Whether log is a function is known when the formula is made.
    (check-regexp-match transformed (message-of (lambda () (~ (log mpg) wt))))
    (check-regexp-match transformed (message-of (lambda () (~ (sqrt mpg) (log hp)))))
    ;; A response of columns whose first name is bound, not to a function.
    (define y1 '(1 2 3 5 4 6))
    (define y2 '(2 4 7 9 8 13))
    (define two (list (cons "y1" y1) (cons "y2" y2) (cons "x" '(1 2 4 5 5 7))))
    (check-equal? (formula-response (~ (y1 y2) x)) '(y1 y2))
    (check-equal? (formula-response (~ ("log" "mpg") wt)) '("log" "mpg"))
    (check-equal? (glmnet-model-response-names
                   (formula-fit (~ (y1 y2) x) two #:family 'mgaussian #:lambda 0.1))
                  '("y1" "y2")))

  (test-case "a name that is neither a column nor bound is an error when the transform runs"
    (define f (~ mpg wt (I (* hp scale))))
    (check-true (formula? f))
    (check-exn #rx"^formula-fit: a name in a transform is neither a column of the table nor a defined variable\n  name: \"scale\"\n  transform: \"\\(I \\(\\* hp scale\\)\\)\""
               (lambda () (formula-fit f mtcars #:lambda 0.1)))
    ;; The names run the transform too, since its values decide whether it is
    ;; a factor, as R's model.matrix evaluates it.
    (check-exn #rx"^formula-predictor-names: a name in a transform is neither"
               (lambda () (mtcars-names f)))
    (check-exn #rx"^formula-fit: a transform must read a column of the table\n  transform: \"\\(log horsepower\\)\"\n  formula: \\(~ mpg \\(log horsepower\\)\\)"
               (lambda () (formula-fit (~ mpg (log horsepower)) mtcars #:lambda 0.1))))

  (test-case "a transform's value must be a finite real, and an error names the transform and the row"
    (check-exn #rx"^formula-fit: a transform's value is not finite\n  transform: \"\\(log \\(- hp 110\\)\\)\"\n  row: 0\n  value: -inf.0"
               (lambda () (formula-fit (~ mpg (log (- hp 110))) mtcars #:lambda 0.1)))
    (check-exn #rx"^formula-design-matrix: a transform's value is not a real number\n  transform: \"\\(sqrt \\(- hp 100\\)\\)\"\n  row: 2\n  value: 0.0\\+2.6457513110645907i"
               (lambda () (formula-design-matrix (~ mpg (sqrt (- hp 100))) mtcars)))
    (check-exn #rx"^formula-path: a transform raised an exception\n  transform: \"\\(I \\(/ hp 0\\)\\)\"\n  row: 0\n  exception: /: division by zero"
               (lambda () (formula-path (~ mpg (I (/ hp 0))) mtcars))))

  (test-case "a transform is one variable: written twice it is one term, and R keeps a transformed response"
    (check-equal? (mtcars-names (~ mpg (log hp) (log  hp) (: (log hp) wt) (: wt (log hp))))
                  '("(log hp)" "(log hp):wt"))
    ;; R: mpg ~ wt + log(hp) - log(hp)
    (check-equal? (mtcars-names (mpg . ~ . wt + (log hp) - (log hp))) '("wt"))
    (define-values (names warnings)
      (glmnet-warnings (lambda () (mtcars-names (~ mpg (log mpg) wt)))))
    (check-equal? names '("(log mpg)" "wt"))
    (check-equal? warnings '())
    (define with-name (cons (cons "(log hp)" (mtcars-column "hp")) mtcars))
    (check-exn #rx"two columns of the formula's design matrix have the same name\n  name: \"\\(log hp\\)\""
               (lambda () (formula-predictor-names (~ mpg "(log hp)" (log hp)) with-name))))

  (test-case "a formula with transforms prints as written, and compares as written"
    (define f (mpg . ~ . (log hp) * wt + (I (expt hp 2))))
    (check-equal? (format "~a" f) "(~ mpg (log hp) * wt + (I (expt hp 2)))")
    (check-equal? (format "~a" (~ mpg (* (log hp) wt))) "(~ mpg (* (log hp) wt))")
    (define t (car (formula-terms (~ mpg (log hp)))))
    (check-true (transform-term? t))
    (check-true (formula-term/c t))
    (check-false (formula-term/c '(log hp)))
    (check-equal? (format "~a" t) "#<transform-term (log hp)>")
    (check-equal? (~ mpg (log hp)) (~ mpg (log hp)))
    (check-not-equal? (~ mpg (log hp)) (~ mpg (log wt)))
    (check-equal? (make-formula 'mpg (transform-term "(log hp)" '(hp) log) '* 'wt)
                  (mpg . ~ . (log hp) * wt)))

  (test-case "a transform's name writes quoted data with the reader's abbreviations"
    (define f (~ mpg wt (I (if (memv cyl '(4.0 6.0)) 1 0)) (I (length `(,hp x ,@(list wt))))))
    (check-equal? (mtcars-names f)
                  '("wt" "(I (if (memv cyl '(4.0 6.0)) 1 0))" "(I (length `(,hp x ,@(list wt))))"))
    (check-equal? (format "~a" f)
                  "(~ mpg wt (I (if (memv cyl '(4.0 6.0)) 1 0)) (I (length `(,hp x ,@(list wt)))))")
    (check-equal? (map car (coef (formula-fit f mtcars #:lambda 0.1)))
                  '("(Intercept)" "wt" "(I (if (memv cyl '(4.0 6.0)) 1 0))"
                    "(I (length `(,hp x ,@(list wt))))"))
    (check-equal? (hash-ref (columns-of (formula-design-matrix f mtcars))
                            "(I (if (memv cyl '(4.0 6.0)) 1 0))")
                  (for/list ([c (in-list (mtcars-column "cyl"))]) (if (= c 8.0) 0.0 1.0))))

  (test-case "transform-term makes a transform from a procedure, its name and its columns"
    (define ratio (transform-term "hp/wt" '("hp" wt) /))
    (define f (make-formula 'mpg ratio '+ 'qsec))
    (check-equal? (format "~a" f) "(~ mpg hp/wt + qsec)")
    (check-equal? (hash-ref (columns-of (formula-design-matrix f mtcars)) "hp/wt")
                  (map / (mtcars-column "hp") (mtcars-column "wt")))
    (check-exn #rx"^formula-fit: the table has no column with this name\n  column: \"power\""
               (lambda () (formula-fit (make-formula 'mpg (transform-term "(log power)" '(power) log))
                                       mtcars #:lambda 0.1)))
    (check-exn (blame-matching #rx"transform-term: contract violation.*procedure-arity-includes/c 2")
               (lambda () (transform-term "bad" '(hp wt) sqrt)))
    (check-exn (blame-matching #rx"transform-term: contract violation.*in: the columns argument")
               (lambda () (transform-term "none" '() (lambda () 1)))))

  (test-case "^ and R's infix arithmetic inside a transform are syntax errors that say what to write"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (check-regexp-match #rx"~: \\^ is not a Racket function; inside a transform, write a power as \\(expt x 2\\), or a square as \\(sqr x\\)\n  at: \\^"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (I (^ hp 2)))))))
    (check-regexp-match #rx"~: \\^ is not a Racket function"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (I (hp ^ 2)))))))
    (check-regexp-match #rx"~: hp is not bound, and inside a transform the operators are Racket's, which come first: write \\(\\* hp wt\\)\n  at: \\(hp \\* wt\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (I (hp * wt)))))))
    (check-regexp-match #rx"~: hp is not bound.*write \\(\\+ hp 1\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (log (hp + 1)))))))
    (check-regexp-match #rx"~: I takes one Racket expression, as in \\(I \\(expt x 2\\)\\)\n  at: \\(I hp wt\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (I hp wt)))))))

  (test-case "an error after an (I expr) term is that error, not one about I"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (check-regexp-match #rx"~: wt:hp reads as one name; put spaces around an operator, as in wt : hp, or write a column with this name as a string, \"wt:hp\"\n  at: wt:hp"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (I (expt hp 2)) wt:hp)))))
    (check-regexp-match #rx"~: expected a power, an exact integer of at least 2\n  at: 1"
                        (message-of (lambda () (convert-compile-time-error
                                                (mpg . ~ . hp + (I (expt hp 2)) + wt ^ 1)))))
    (check-regexp-match #rx"~: \\(1 x z\\) is not a term: a group of terms starts with an operator, as \\(\\+ x z\\) does, or has operators between its terms, as \\(x \\+ z\\) does\n  at: \\(1 x z\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (I (expt hp 2)) (1 x z))))))
    (check-regexp-match #rx"~: 'wt is quoted, and ~ quotes the names of a formula itself: write the column as wt or \"wt\"\n  at: \\(quote wt\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (I (expt hp 2)) 'wt)))))
    (check-regexp-match #rx"~: / is an operator of R's formulas that this formula language does not have; for a ratio, write \\(I \\(/ hp wt\\)\\)\n  at: /"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (I (expt hp 2)) (/ hp wt))))))
    (check-regexp-match #rx"~: \\(squared hp\\) is not a term: squared is not bound, so it is not a transform, and a group of terms starts with an operator.*\n  at: \\(squared hp\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (I (* wt hp)) (squared hp))))))
    (define two #rx"~: 2 is not a term; a formula's numbers are 1, 0 and a power after \\^\n  at: 2")
    (check-regexp-match two (message-of (lambda () (convert-compile-time-error (~ mpg (log hp) (I (* wt hp)) 2)))))
    (check-regexp-match two (message-of (lambda () (convert-compile-time-error (~ mpg (I (* wt hp)) (log hp) 2))))))

  (test-case "a formula fit with transforms is the matrix fit of the transformed columns, for any family"
    (define f (mpg . ~ . hp + (sqr hp) + (log wt)))
    (define x (map list (mtcars-column "hp") (map sqr (mtcars-column "hp")) (map log (mtcars-column "wt"))))
    (check-same (formula-fit f mtcars #:lambda 0.1) (elnet-fit x mpg #:lambda 0.1))
    (check-same (formula-path f mtcars #:lambda '(1.0 0.1)) (elnet-path x mpg #:lambda '(1.0 0.1)))
    (define cox (formula-fit (~ (surv time status) (log karno) age) veteran #:family 'cox #:lambda 0.05))
    (check-same cox (cox-fit (for/list ([k (in-list (cdr (assoc "karno" veteran)))]
                                        [a (in-list (cdr (assoc "age" veteran)))])
                               (list (log k) a))
                             tc sc #:lambda 0.05))
    (check-equal? (map car (coef cox)) '("(log karno)" "age")))

  (test-case "predict evaluates the transforms again on the new table's columns"
    (define m (formula-path (mpg . ~ . (log hp) * wt) mtcars #:lambda '(1.0 0.1)))
    (define new (list (cons "wt" '(2.5 3.5)) (cons "hp" '(100 200))))
    (check-equal? (predict m new #:lambda 0.1)
                  (predict (formula-model-fit m)
                           (list (list (log 100.0) 2.5 (* (log 100.0) 2.5))
                                 (list (log 200.0) 3.5 (* (log 200.0) 3.5)))
                           #:lambda 0.1))
    (check-exn #rx"^predict: the table has no column with this name\n  column: \"hp\""
               (lambda () (predict m (list (cons "wt" '(2.5))))))
    (check-exn #rx"^predict: a transform's value is not finite\n  transform: \"\\(log hp\\)\"\n  row: 1\n  value: -inf.0"
               (lambda () (predict m (list (cons "wt" '(2.5 3.5)) (cons "hp" '(100 0)))))))

  (test-case "at the top level, a transform's name can be defined after the formula"
    (define ns (make-base-namespace))
    (namespace-attach-module (variable-reference->namespace (#%variable-reference)) 'glmnet ns)
    (define t (list (cons "y" '(1 2 3)) (cons "x" '(1 2 4))))
    (define (column-of expr)
      (parameterize ([current-namespace ns])
        (design-matrix->columns (eval `(formula-design-matrix ,expr ',t)))))
    (parameterize ([current-namespace ns])
      (namespace-require 'glmnet)
      (eval '(define x "a string, not the column"))
      (eval '(define f (~ y (I (* x scale))))))
    (check-exn #rx"^formula-design-matrix: a name in a transform is neither a column of the table nor a defined variable\n  name: \"scale\""
               (lambda () (column-of 'f)))
    (parameterize ([current-namespace ns])
      (eval '(define scale 10)))
    (check-equal? (column-of 'f) '((10.0 20.0 40.0))))

  (test-case "at the top level, a transform's function must be defined first, and ~ checks it as in a module"
    (define ns (make-base-namespace))
    (namespace-attach-module (variable-reference->namespace (#%variable-reference)) 'glmnet ns)
    (define (run expr)
      (parameterize ([current-namespace ns]) (eval expr)))
    (define (message-of expr)
      (with-handlers ([exn:fail:syntax? exn-message]) (run expr) #f))
    (run '(require glmnet))
    (check-regexp-match #rx"~: \\(squared x\\) is not a term: squared is not bound"
                        (message-of '(~ y (squared x))))
    (check-regexp-match #rx"~: x is not bound, and inside a transform the operators are Racket's, which come first: write \\(\\+ x 1\\)"
                        (message-of '(~ y (log (x + 1)))))
    (check-regexp-match #rx"~: \\^ is not a Racket function"
                        (message-of '(~ y (I (x ^ 2)))))
    (run '(define (squared v) (* v v)))
    (check-equal? (run '(design-matrix->columns
                         (formula-design-matrix (~ y (squared x)) (list (cons "y" '(1 2 3))
                                                                        (cons "x" '(1 2 4))))))
                  '((1.0 4.0 16.0))))

  ;; R's model.frame evaluates each variable once, and model.matrix codes the
  ;; values it holds; a factor's levels and its columns come from that one
  ;; evaluation. It used to be evaluated twice, once for the levels.
  (test-case "a transform is evaluated once for each row of the table it is fitted to"
    (define t (list (cons "y" '(1.0 3.0 2.0 4.0)) (cons "x" '(1.0 2.0 3.0 4.0))))
    (define calls 0)
    (define (tick x) (set! calls (add1 calls)) (+ x (* 100 calls)))
    (check-equal? (design-matrix->columns (formula-design-matrix (~ y (tick x)) t))
                  '((101.0 202.0 303.0 404.0)))
    (check-equal? calls 4)
    (set! calls 0)
    (define m (formula-fit (~ y (tick x)) t #:lambda 0))
    (check-equal? calls 4)
    (check-equal? (formula-model-fit m) (elnet-fit '((101.0) (202.0) (303.0) (404.0)) '(1.0 3.0 2.0 4.0)
                                                   #:lambda 0))
    (set! calls 0)
    (formula-cv (~ y (tick x)) t #:fold-ids '(0 1 2 0) #:lambda '(0.1 0.01))
    (check-equal? calls 4)
    ;; A factor whose labels change from one evaluation to the next: the
    ;; levels are those of the evaluation that gives the columns.
    (define draws 0)
    (define (draw g) (set! draws (add1 draws)) (if (<= draws 4) g (string-append g "'")))
    (define groups (list (cons "y" '(1.0 3.0 2.0 4.0)) (cons "g" '("a" "b" "a" "b"))))
    (define dm (formula-design-matrix (~ y (draw g)) groups))
    (check-equal? (design-matrix-column-names dm) '("(draw g)b"))
    (check-equal? (design-matrix->columns dm) '((0.0 1.0 0.0 1.0)))
    (check-equal? draws 4))

  (test-case "a transform reads its Racket bindings each time, as R does"
    (define k 2)
    (define m (formula-fit (~ mpg (I (* k hp)) wt) mtcars #:lambda 0.1))
    (define new (list (cons "hp" '(100)) (cons "wt" '(3))))
    (define before (predict m new))
    (define doubled (predict m (list (cons "hp" '(200)) (cons "wt" '(3)))))
    (set! k 4)
    (check-equal? (predict m new) doubled)
    (check-not-equal? doubled before))

  ;; --- factors ----------------------------------------------------------------------

  ;; A small table with a column of each kind that makes a factor, and a
  ;; column of numbers.
  (define kinds
    (list (cons "y" '(1.0 3.0 2.0 5.0 4.0 6.0))
          (cons "x" '(0.5 1.5 1.0 2.5 2.0 3.0))
          (cons "group" '("b" "a" "c" "a" "b" "c"))
          (cons "tag" '(b a c a b c))
          (cons "mixed" #("b" a "c" a "b" c))
          (cons "flag" '(#t #f #t #t #f #f))
          (cons "code" '(3 1 2 1 3 2))))

  (define (kinds-names f) (formula-predictor-names f kinds))
  (define (kinds-columns f) (columns-of (formula-design-matrix f kinds)))

  ;; The rows of a table at the indices `rows`.
  (define (table-rows table rows)
    (for/list ([column (in-list table)])
      (define entries (table-column table (car column)))
      (cons (car column) (for/list ([i (in-list rows)]) (list-ref entries i)))))

  (test-case "a column of strings, symbols or booleans is a factor, coded by treatment contrasts"
    ;; R: y ~ group + x, with group a character column
    (check-equal? (kinds-names (~ y group x)) '("groupb" "groupc" "x"))
    (define x (kinds-columns (~ y group x)))
    (check-equal? (hash-ref x "groupb") '(1.0 0.0 0.0 0.0 1.0 0.0))
    (check-equal? (hash-ref x "groupc") '(0.0 0.0 1.0 0.0 0.0 1.0))
    ;; Symbols, and a string and a symbol with the same text, are one level.
    (check-equal? (kinds-names (~ y tag)) '("tagb" "tagc"))
    (check-equal? (hash-ref (kinds-columns (~ y tag)) "tagc") (hash-ref x "groupc"))
    (check-equal? (hash-ref (kinds-columns (~ y mixed)) "mixedc") (hash-ref x "groupc"))
    ;; R codes a logical as a factor with the levels FALSE and TRUE, both even
    ;; when one is absent.
    (check-equal? (hash-ref (kinds-columns (~ y flag)) "flagTRUE") '(1.0 0.0 1.0 1.0 0.0 0.0))
    (check-equal? (formula-predictor-names (~ y on) (list (cons "y" '(1 2 3)) (cons "on" '(#t #t #t))))
                  '("onTRUE"))
    ;; Numbers are a number, as in R, and all takes a factor column as it is.
    (check-equal? (kinds-names (~ y code)) '("code"))
    (check-equal? (kinds-names (~ y (- all tag mixed))) '("x" "groupb" "groupc" "flagTRUE" "code")))

  (test-case "(factor x) makes numbers a factor, its levels sorted by value, as R's factor()"
    ;; R: mpg ~ wt + factor(cyl)
    (check-equal? (mtcars-names (mpg . ~ . wt + (factor cyl))) '("wt" "(factor cyl)6" "(factor cyl)8"))
    (check-equal? (mtcars-names (~ mpg wt (factor "cyl"))) '("wt" "(factor cyl)6" "(factor cyl)8"))
    (check-equal? (hash-ref (columns-of (formula-design-matrix (~ mpg (factor cyl)) mtcars))
                            "(factor cyl)6")
                  (for/list ([c (in-list (mtcars-column "cyl"))]) (if (= c 6.0) 1.0 0.0)))
    ;; 9 comes before 10, 6 and 6.0 are one level, and a level is named as
    ;; Racket prints its number, without a trailing .0.
    (define codes (list (cons "y" '(1 2 3 4 5 6)) (cons "k" '(10 9 2.5 6 6.0 -1))))
    (check-equal? (formula-predictor-names (~ y (factor k)) codes)
                  '("(factor k)2.5" "(factor k)6" "(factor k)9" "(factor k)10"))
    ;; Of strings, the levels of the strings; of booleans, those present.
    (check-equal? (kinds-names (~ y (factor group))) '("(factor group)b" "(factor group)c"))
    (check-equal? (formula-predictor-names (~ y (factor on)) (list (cons "y" '(1 2)) (cons "on" '(#f #t))))
                  '("(factor on)TRUE"))
    ;; A column whose name is not an identifier is named as a string.
    (check-equal? (formula-predictor-names (~ y (factor "n k"))
                                           (list (cons "y" '(1 2 3)) (cons "n k" '(1 2 1))))
                  '("(factor \"n k\")2")))

  (test-case "string levels sort by string<?, as R's sort() does in the C locale"
    ;; R with LC_COLLATE=C: 10 9 A B Z _z a b. An en_US session sorts them
    ;; _z 10 9 a A b B Z.
    (define letters* (list (cons "y" '(1 2 3 4 5 6 7 8))
                           (cons "g" '("b" "B" "a" "A" "Z" "_z" "10" "9"))))
    (check-equal? (formula-predictor-names (~ y g) letters*)
                  '("g9" "gA" "gB" "gZ" "g_z" "ga" "gb")))

  (test-case "without an intercept, the first factor is coded by dummies, as R's model.matrix does"
    ;; R: mpg ~ 0 + wt + factor(cyl)
    (check-equal? (mtcars-names (mpg . ~ . 0 + wt + (factor cyl)))
                  '("wt" "(factor cyl)4" "(factor cyl)6" "(factor cyl)8"))
    ;; R: mpg ~ 0 + factor(cyl) + factor(gear): only the first factor
    (check-equal? (mtcars-names (mpg . ~ . 0 + (factor cyl) + (factor gear)))
                  '("(factor cyl)4" "(factor cyl)6" "(factor cyl)8" "(factor gear)4" "(factor gear)5"))
    ;; R: mpg ~ 0 + wt + wt:factor(cyl): in the first term that has a factor
    (check-equal? (mtcars-names (mpg . ~ . 0 + wt + wt : (factor cyl)))
                  '("wt" "wt:(factor cyl)4" "wt:(factor cyl)6" "wt:(factor cyl)8"))
    ;; The dummies add up to the intercept's column of ones.
    (define x (kinds-columns (~ y 0 group)))
    (check-equal? (map + (hash-ref x "groupa") (hash-ref x "groupb") (hash-ref x "groupc"))
                  (make-list 6 1.0))
    ;; R: y ~ 0 + flag + x, with flag a logical column
    (check-equal? (kinds-names (~ y 0 flag x)) '("flagFALSE" "flagTRUE" "x")))

  (test-case "in an interaction, a factor is coded by R's marginality rule"
    ;; R: mpg ~ wt * factor(cyl): contrasts, as the main effect is there
    (check-equal? (mtcars-names (mpg . ~ . wt * (factor cyl)))
                  '("wt" "(factor cyl)6" "(factor cyl)8" "wt:(factor cyl)6" "wt:(factor cyl)8"))
    ;; R: mpg ~ wt:factor(cyl): without the main effect, dummies
    (check-equal? (mtcars-names (mpg . ~ . wt : (factor cyl)))
                  '("wt:(factor cyl)4" "wt:(factor cyl)6" "wt:(factor cyl)8"))
    ;; R: mpg ~ wt:hp + wt:factor(cyl): contrasts, as wt is contained in wt:hp,
    ;; an earlier term, though it is not a term itself
    (check-equal? (mtcars-names (mpg . ~ . wt : hp + wt : (factor cyl)))
                  '("wt:hp" "wt:(factor cyl)6" "wt:(factor cyl)8"))
    ;; R: mpg ~ factor(cyl) + wt:factor(cyl): a slope for each level
    (check-equal? (mtcars-names (mpg . ~ . (factor cyl) + wt : (factor cyl)))
                  '("(factor cyl)6" "(factor cyl)8" "(factor cyl)4:wt" "(factor cyl)6:wt"
                    "(factor cyl)8:wt"))
    ;; R: mpg ~ factor(cyl) * factor(gear): the first factor varies fastest
    (check-equal? (mtcars-names (mpg . ~ . (factor cyl) * (factor gear)))
                  '("(factor cyl)6" "(factor cyl)8" "(factor gear)4" "(factor gear)5"
                    "(factor cyl)6:(factor gear)4" "(factor cyl)8:(factor gear)4"
                    "(factor cyl)6:(factor gear)5" "(factor cyl)8:(factor gear)5"))
    (define x (kinds-columns (~ y (* group x))))
    (check-equal? (hash-ref x "groupb:x") (map * (hash-ref x "groupb") (hash-ref x "x"))))

  (test-case "a factor needs two levels, and a column holds one kind of value; errors name it"
    (define one (list (cons "y" '(1 2 3)) (cons "g" '("a" "a" "a")) (cons "k" '(4 4 4))))
    (check-exn #rx"^formula-fit: a factor must have at least two levels\n  column: \"g\"\n  level: \"a\""
               (lambda () (formula-fit (~ y g) one #:lambda 0.1)))
    (check-exn #rx"^formula-predictor-names: a factor must have at least two levels\n  factor: \"\\(factor k\\)\"\n  level: \"4\""
               (lambda () (formula-predictor-names (~ y (factor k)) one)))
    (define y '(1 2 3))
    (check-exn #rx"^formula-fit: a column mixes numbers with strings or symbols\n  column: \"g\"\n  row: 2\n  element: \"b\""
               (lambda () (formula-fit (~ y g) (list (cons "y" y) (cons "g" '(1 2 "b"))) #:lambda 0.1)))
    (check-exn #rx"^formula-fit: a column mixes strings or symbols with numbers\n  column: \"g\"\n  row: 1\n  element: 2"
               (lambda () (formula-fit (~ y g) (list (cons "y" y) (cons "g" '("a" 2 "b"))) #:lambda 0.1)))
    (check-exn #rx"a column mixes booleans with strings or symbols\n  column: \"h\""
               (lambda () (formula-fit (~ y h) (list (cons "y" y) (cons "h" '(#t "a" #f))) #:lambda 0.1)))
    ;; Among numbers, another value is not a real number, as before.
    (check-exn #rx"the table has an element that is not a real number\n  column: \"g\"\n  row: 1"
               (lambda () (formula-fit (~ y g) (list (cons "y" y) (cons "g" '(1 #\a 2))) #:lambda 0.1)))
    (check-exn #rx"^formula-design-matrix: a transform's values mix numbers with strings or symbols\n  transform: \"\\(if \\(> hp 150\\) \\\\\"high\\\\\" 0\\)\"\n  row: 4\n  value: \"high\""
               (lambda () (formula-design-matrix (~ mpg (if (> hp 150) "high" 0)) mtcars))))

  (test-case "a transform whose values are strings or booleans is a factor, as in R"
    ;; R: mpg ~ ifelse(hp > 150, "high", "low") + wt
    (check-equal? (mtcars-names (mpg . ~ . (if (> hp 150) "high" "low") + wt))
                  '("(if (> hp 150) \"high\" \"low\")low" "wt"))
    ;; R: mpg ~ I(hp > 150) + wt
    (check-equal? (mtcars-names (mpg . ~ . (> hp 150) + wt)) '("(> hp 150)TRUE" "wt"))
    ;; R: mpg ~ factor(gear > 3) * wt
    (check-equal? (mtcars-names (mpg . ~ . (factor (> gear 3)) * wt))
                  '("(factor (> gear 3))TRUE" "wt" "(factor (> gear 3))TRUE:wt"))
    ;; R: mpg ~ factor(hp %/% 100)
    (check-equal? (mtcars-names (~ mpg (factor (quotient hp 100))))
                  '("(factor (quotient hp 100))1" "(factor (quotient hp 100))2"
                    "(factor (quotient hp 100))3")))

  (test-case "a transform reads a column of strings, symbols or booleans as its values, as R's calls do"
    ;; R: y ~ I(group == "a"), with group a character column
    (check-equal? (kinds-names (~ y (equal? group "a"))) '("(equal? group \"a\")TRUE"))
    (check-equal? (hash-ref (kinds-columns (~ y (equal? group "a"))) "(equal? group \"a\")TRUE")
                  '(0.0 1.0 0.0 1.0 0.0 0.0))
    ;; R: y ~ I(!flag) and y ~ I(flag * x), with flag a logical column
    (check-equal? (hash-ref (kinds-columns (~ y (not flag))) "(not flag)TRUE")
                  '(0.0 1.0 0.0 0.0 1.0 1.0))
    (check-equal? (hash-ref (kinds-columns (~ y (if flag x 0))) "(if flag x 0)")
                  '(0.5 0.0 1.0 2.5 0.0 0.0))
    ;; R: y ~ factor(toupper(group))
    (check-equal? (kinds-names (~ y (factor (string-upcase group))))
                  '("(factor (string-upcase group))B" "(factor (string-upcase group))C"))
    ;; Symbols are symbols, and a column of numbers is flonums.
    (check-equal? (kinds-names (~ y (symbol->string tag))) '("(symbol->string tag)b" "(symbol->string tag)c"))
    (define seen '())
    (define peek (transform-term "peek" '(code group) (lambda (c g) (set! seen (cons (list c g) seen)) c)))
    (formula-design-matrix (make-formula 'y peek) kinds)
    (check-equal? (reverse seen)
                  '((3.0 "b") (1.0 "a") (2.0 "c") (1.0 "a") (3.0 "b") (2.0 "c")))
    ;; predict reads the new table's columns in the same way.
    (define m (formula-fit (~ y x (not flag)) kinds #:lambda 0.01))
    (check-equal? (predict m (list (cons "x" '(1.0 2.0)) (cons "flag" '(#f #t))))
                  (predict (formula-model-fit m) '((1.0 1) (2.0 0))))
    ;; Strings and symbols are two kinds for a transform, which tells them apart.
    (check-exn #rx"^formula-design-matrix: a transform reads a column that mixes strings with symbols\n  transform: \"\\(equal\\? mixed \\\\\"a\\\\\"\\)\"\n  column: \"mixed\"\n  row: 1\n  element: 'a"
               (lambda () (formula-design-matrix (~ y (equal? mixed "a")) kinds)))
    ;; A column of one kind of value, and numbers finite, or an error names the
    ;; transform and the column.
    (define (with-g g) (list (cons "y" '(1 2 3)) (cons "g" g)))
    (check-exn #rx"^formula-fit: a transform reads a column that mixes numbers with strings\n  transform: \"\\(abs g\\)\"\n  column: \"g\"\n  row: 2\n  element: \"b\""
               (lambda () (formula-fit (~ y (abs g)) (with-g '(1 2 "b")) #:lambda 0.1)))
    (check-exn #rx"^formula-design-matrix: a transform reads a column with an element that is not a real number, a string, a symbol or a boolean\n  transform: \"\\(abs g\\)\"\n  column: \"g\"\n  row: 1\n  element: #\\\\a"
               (lambda () (formula-design-matrix (~ y (abs g)) (with-g '(1 #\a 2)))))
    (check-exn #rx"^formula-design-matrix: a transform reads a column with an element that is not finite\n  transform: \"\\(abs g\\)\"\n  column: \"g\"\n  row: 1\n  element: \\+inf.0"
               (lambda () (formula-design-matrix (~ y (abs g)) (with-g '(1 +inf.0 2)))))
    (check-exn #rx"^formula-design-matrix: a transform raised an exception\n  transform: \"\\(log group\\)\"\n  row: 0\n  exception: \n   log: contract violation\n     expected: number\\?\n     given: \"b\""
               (lambda () (formula-design-matrix (~ y (log group)) kinds))))

  (test-case "predict needs each column a transform reads to hold the kind of value it was fitted with"
    ;; R's ifelse(b, wt, 0) is the same for TRUE/FALSE and 1/0, but 0 is true
    ;; in Racket, so (if flag x 0) would silently change on 1/0.
    (define m (formula-fit (~ y x (if flag x 0)) kinds #:lambda 0.01))
    (check-exn #rx"^predict: a transform reads a column whose values are of another kind than when the model was fitted\n  transform: \"\\(if flag x 0\\)\"\n  column: \"flag\"\n  fitted with: booleans\n  given: numbers"
               (lambda () (predict m (list (cons "x" '(1.0 2.0)) (cons "flag" '(1 0))))))
    (check-equal? (predict m (list (cons "x" '(1.0 2.0)) (cons "flag" '(#t #f))))
                  (predict (formula-model-fit m) '((1.0 1.0) (2.0 0.0))))
    ;; (equal? group "a") is #f for the symbol a.
    (define g (formula-fit (~ y x (equal? group "a")) kinds #:lambda 0.01))
    (check-exn #rx"^predict: a transform reads a column whose values are of another kind than when the model was fitted\n  transform: \"\\(equal\\? group \\\\\"a\\\\\"\\)\"\n  column: \"group\"\n  fitted with: strings\n  given: symbols"
               (lambda () (predict g (list (cons "x" '(1.0)) (cons "group" '(a))))))
    (check-exn #rx"fitted with: numbers\n  given: booleans"
               (lambda () (predict (formula-fit (~ y (sqr x) code) kinds #:lambda 0.01)
                                   (list (cons "x" '(#t)) (cons "code" '(1))))))
    ;; A factor column's levels match by label, so symbols stand for strings.
    (define f (formula-fit (~ y x group) kinds #:lambda 0.01))
    (check-equal? (predict f (list (cons "x" '(1.0 2.0)) (cons "group" '(a c))))
                  (predict f (list (cons "x" '(1.0 2.0)) (cons "group" '("a" "c"))))))

  (test-case "the model keeps its factors' levels, and predict codes new data with them"
    (define m (formula-fit (mpg . ~ . wt + (factor cyl) + (> hp 150)) mtcars #:lambda 0.1))
    (check-equal? (formula-model-levels m)
                  '(("(factor cyl)" "4" "6" "8") ("(> hp 150)" "FALSE" "TRUE")))
    (check-equal? (formula-model-levels (formula-fit (~ mpg wt) mtcars #:lambda 0.1)) '())
    ;; R's xlevels leave out the response, even in an interaction: R's
    ;; .getXlevels of Species ~ Sepal.Length:Species is an empty list.
    (define response-in-interaction
      (formula-fit (~ Species (: Sepal.Length Species)) iris-species
                   #:family 'multinomial #:lambda 0.05))
    (check-equal? (formula-model-predictor-names response-in-interaction)
                  '("Speciessetosa:Sepal.Length" "Speciesversicolor:Sepal.Length"
                    "Speciesvirginica:Sepal.Length"))
    (check-equal? (formula-model-levels response-in-interaction) '())
    (check-equal? (formula-model-levels
                   (formula-fit (flag . ~ . x + x : flag) kinds #:family 'binomial #:lambda 0.1))
                  '())
    ;; New data with some of the levels, 8.0 the level 8.
    (define new (list (cons "cyl" '(8.0 4)) (cons "hp" '(100 200)) (cons "wt" '(3.0 2.5))))
    (check-equal? (predict m new) (predict (formula-model-fit m) '((3.0 0 1 0) (2.5 0 0 1))))
    (check-exn #rx"^predict: a factor has levels that the model was not fitted with\n  factor: \"\\(factor cyl\\)\"\n  new levels: '\\(\"5\" \"7\"\\)\n  levels: '\\(\"4\" \"6\" \"8\"\\)"
               (lambda () (predict m (list (cons "cyl" '(5 4 7 5)) (cons "hp" '(1 2 3 4))
                                           (cons "wt" '(1 2 3 4))))))
    ;; A column of strings: symbols with the same text are its levels.
    (define g (formula-fit (~ y group x) kinds #:lambda 0.01))
    (check-equal? (formula-model-levels g) '(("group" "a" "b" "c")))
    (define rows (list (cons "x" '(1.0 2.0)) (cons "group" '("c" "a"))))
    (check-equal? (predict g rows) (predict (formula-model-fit g) '((0 1 1.0) (0 0 2.0))))
    (check-equal? (predict g (list (cons "x" '(1.0 2.0)) (cons "group" '(c a)))) (predict g rows))
    (check-exn #rx"^predict: a factor has levels that the model was not fitted with\n  column: \"group\"\n  new levels: '\\(\"d\"\\)"
               (lambda () (predict g (list (cons "x" '(1.0)) (cons "group" '("d"))))))
    ;; A column that was numbers must still be numbers.
    (check-exn #rx"^predict: the table has an element that is not a real number\n  column: \"wt\""
               (lambda () (predict m (list (cons "cyl" '(4)) (cons "hp" '(1)) (cons "wt" '("heavy")))))))

  (test-case "a binomial or multinomial response of strings or booleans has its levels as classes"
    (define measures '("Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width"))
    (define species (table-column iris-species "Species"))
    (define m (formula-fit (~ Species all) iris-species #:family 'multinomial #:lambda 0.05))
    (check-same m (multinomial-fit (rows-of iris-species measures)
                                   (for/list ([s (in-list species)])
                                     (index-of '("setosa" "versicolor" "virginica") s))
                                   #:lambda 0.05))
    (check-equal? (map car (coef m)) '("setosa" "versicolor" "virginica"))
    (define three (table-rows iris-species '(0 50 100)))
    ;; R: predict(glmnet(x, y, "multinomial", lambda = 0.05), x[c(1, 51, 101), ], type = "class")
    (check-equal? (predict m three #:type 'class) '("setosa" "versicolor" "virginica"))
    ;; R: glmnet(x[, 2:4], iris$Sepal.Length > 5.8, "binomial", lambda = 0.05):
    ;; FALSE is the baseline, and the class is the level's name.
    (define long (cons (cons "long" (for/list ([v (in-list (table-column iris-species "Sepal.Length"))])
                                      (> v 5.8)))
                       iris-species))
    (define b (formula-fit (~ long Sepal.Width Petal.Length Petal.Width) long
                           #:family 'binomial #:lambda 0.05))
    (check-within (cdr (assoc "(Intercept)" (coef b))) -5.363383 1e-6)
    (check-within (cdr (assoc "Petal.Length" (coef b))) 1.289253 1e-6)
    (check-equal? (predict b three #:type 'class) '("FALSE" "TRUE" "TRUE"))
    (check-exn #rx"^formula-fit: a binomial response has two classes; the multinomial family takes more\n  column: \"Species\"\n  classes: '\\(\"setosa\" \"versicolor\" \"virginica\"\\)"
               (lambda () (formula-fit (~ Species all) iris-species #:family 'binomial #:lambda 0.05)))
    (define setosa (table-rows iris-species (range 50)))
    (check-exn #rx"^formula-fit: a binomial response needs two classes\n  column: \"Species\""
               (lambda () (formula-fit (~ Species all) setosa #:family 'binomial #:lambda 0.05)))
    (check-exn #rx"^formula-cv: a multinomial response needs at least two classes\n  column: \"Species\""
               (lambda () (formula-cv (~ Species all) setosa #:family 'multinomial)))
    ;; A Gaussian response is numbers.
    (check-exn #rx"^formula-fit: the table has an element that is not a real number\n  column: \"Species\""
               (lambda () (formula-fit (~ Species all) iris-species #:lambda 0.05))))

  (test-case "factor takes one column or Racket expression, and says so where it is written"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (check-regexp-match #rx"~: factor takes one column or Racket expression, as in \\(factor cyl\\)\n  at: \\(factor cyl gear\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor cyl gear))))))
    (check-regexp-match #rx"~: factor takes one column or Racket expression"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor))))))
    (check-regexp-match #rx"~: factor takes one column or Racket expression, as in \\(factor cyl\\)\n  at: \\(factor \\. cyl\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor . cyl))))))
    (check-regexp-match #rx"~: factor takes one column or Racket expression"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor cyl . gear))))))
    (check-regexp-match #rx"~: expected a column, or a Racket expression such as \\(> hp 150\\)\n  at: \\(log \\. hp\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor (log . hp)))))))
    (check-regexp-match #rx"~: expected a column, or a Racket expression such as \\(> hp 150\\)\n  at: 3"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor 3))))))
    (check-regexp-match #rx"~: \\(wt hp\\) is not a Racket expression: wt is not bound\n  at: \\(wt hp\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor (wt hp)))))))
    (check-regexp-match #rx"~: hp is not bound, and inside a transform the operators are Racket's, which come first: write \\(\\* hp wt\\)\n  at: \\(hp \\* wt\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor (hp * wt)))))))
    (check-regexp-match #rx"~: \\^ is not a Racket function; inside a transform, write a power as \\(expt x 2\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor (hp ^ 2)))))))
    (check-regexp-match #rx"~: \\^ is not a Racket function; inside a transform, write a power as \\(expt x 2\\), or a square as \\(sqr x\\)\n  at: \\^"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor (^ hp 2)))))))
    (check-regexp-match #rx"~: 'cyl is quoted, and ~ quotes the names of a formula itself: write the column as cyl or \"cyl\"\n  at: \\(quote cyl\\)"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor 'cyl))))))
    (check-regexp-match #rx"~: cyl:gear reads as one name"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (factor cyl:gear)))))))

  (test-case "an error after a (factor (I expr)) term is that error, not one about I"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (check-regexp-match #rx"~: wt:hp reads as one name.*\n  at: wt:hp"
                        (message-of (lambda () (convert-compile-time-error
                                                (~ mpg (factor (I (> hp 150))) wt:hp)))))
    (check-regexp-match #rx"~: \\(1 x z\\) is not a term: a group of terms starts with an operator.*\n  at: \\(1 x z\\)"
                        (message-of (lambda () (convert-compile-time-error
                                                (~ mpg (factor (I (> hp 150))) (1 x z))))))
    (check-regexp-match #rx"~: expected a power, an exact integer of at least 2\n  at: 1"
                        (message-of (lambda () (convert-compile-time-error
                                                (mpg . ~ . (factor (I (> hp 150))) + wt ^ 1)))))
    (check-regexp-match #rx"~: / is an operator of R's formulas that this formula language does not have; for a ratio, write \\(I \\(/ hp wt\\)\\)\n  at: /"
                        (message-of (lambda () (convert-compile-time-error
                                                (~ mpg (factor (I (> hp 150))) (/ hp wt))))))
    (check-regexp-match #rx"~: 2 is not a term; a formula's numbers are 1, 0 and a power after \\^\n  at: 2"
                        (message-of (lambda () (convert-compile-time-error
                                                (~ mpg (factor (I (> hp 150))) 2))))))

  (test-case "(factor x) is data, printed and compared as written"
    (define f (mpg . ~ . wt * (factor cyl)))
    (check-equal? (format "~a" f) "(~ mpg wt * (factor cyl))")
    (check-equal? f (make-formula 'mpg 'wt '* '(factor cyl)))
    (check-true (formula-term/c '(factor cyl)))
    (define gear>3 (transform-term "(> gear 3)" '(gear) (lambda (g) (> g 3))))
    (check-true (formula-term/c (list 'factor gear>3)))
    (check-false (formula-term/c '(factor cyl gear)))
    (check-false (formula-term/c '(factor (> gear 3))))
    (check-equal? (format "~a" (~ mpg (factor (> gear 3)))) "(~ mpg (factor (> gear 3)))")
    (check-equal? (formula-predictor-names (make-formula 'mpg (list 'factor gear>3)) mtcars)
                  '("(factor (> gear 3))TRUE")))

  (test-case "a formula fit with factors is the matrix fit of its dummies, for any family"
    (define f (mpg . ~ . wt + (factor cyl)))
    (define x (for/list ([w (in-list (mtcars-column "wt"))] [c (in-list (mtcars-column "cyl"))])
                (list w (if (= c 6.0) 1 0) (if (= c 8.0) 1 0))))
    (check-same (formula-fit f mtcars #:lambda 0.1) (elnet-fit x mpg #:lambda 0.1))
    (check-same (formula-path f mtcars #:lambda '(1.0 0.1)) (elnet-path x mpg #:lambda '(1.0 0.1)))
    (check-same (formula-fit (am . ~ . wt + (factor cyl)) mtcars #:family 'binomial #:lambda 0.05)
                (logistic-fit x (mtcars-column "am") #:lambda 0.05))
    ;; A design matrix as the table: (factor x) of its numbers.
    (define dm (table->design-matrix mtcars '("mpg" "wt" "cyl")))
    (check-equal? (formula-design-matrix f dm) (formula-design-matrix f mtcars)))

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

  (test-case "predict rebuilds the design matrix of the fitted terms from a new table"
    (define f (mpg . ~ . (wt + hp + qsec) ^ 2 - wt : hp))
    (define path (formula-path f mtcars #:lambda '(1.0 0.1)))
    (define x (formula-design-matrix f mtcars))
    (define expected (predict (formula-model-fit path) x #:lambda 0.3))
    (check-equal? (predict path mtcars #:lambda 0.3) expected)
    (check-equal? (predict path (reverse mtcars) #:lambda 0.3) expected)
    (define needed (for/list ([name '("qsec" "hp" "wt")]) (assoc name mtcars)))
    (check-equal? (predict path needed #:lambda 0.3) expected)
    (check-equal? (predict path (cons (cons "name" (make-list 32 "car")) needed) #:lambda 0.3)
                  expected)
    (check-exn #rx"^predict: the table has no columns with these names\n  columns: '\\(\"wt\" \"qsec\"\\)"
               (lambda () (predict path (list (assoc "hp" mtcars)))))
    (check-exn #rx"^predict: the table has no column with this name\n  column: \"qsec\""
               (lambda () (predict path (list (assoc "hp" mtcars) (assoc "wt" mtcars))))))

  (test-case "a model fitted with all keeps the columns all stood for"
    (define m (formula-fit (~ mpg (- all cyl)) mtcars #:lambda 0.1))
    (define extra (cons (cons "extra" (make-list 32 1.0)) (reverse mtcars)))
    (check-equal? (predict m extra) (predict m mtcars))
    (check-equal? (length (formula-model-predictor-names m)) 9))

  ;; A model that gives its fit the predictor and response names it holds.
  (struct named (fit predictors responses)
    #:methods gen:glmnet-model
    [(define/generic ->path glmnet-model->path)
     (define (glmnet-model->path m) (->path (named-fit m)))
     (define (glmnet-model-predictor-names m) (named-predictors m))
     (define (glmnet-model-response-names m) (named-responses m))])

  (test-case "coef and predict need one distinct name per predictor, and per response"
    (define fit (formula-model-fit gfit))
    (define names (glmnet-model-predictor-names gfit))
    (check-equal? (coef (named fit names #f)) (coef gfit))
    (check-equal? (predict (named fit names #f) longley) (predict gfit longley))
    (check-exn #rx"coef: the model does not have one name per predictor\n  predictor names: '\\(\\)\n  predictors: 6"
               (lambda () (coef (named fit '() #f))))
    (check-exn #rx"predict: the model does not have one name per predictor"
               (lambda () (predict (named fit '() #f) longley)))
    (check-exn #rx"coef: the model does not have one name per predictor"
               (lambda () (coef (named fit (take names 5) #f))))
    (check-exn #rx"predict: the model does not have one name per predictor"
               (lambda () (predict (named fit (cons "Employed" names) #f) longley)))
    (define twice (cons "GNP" (cdr names)))
    (check-exn #rx"coef: the model gives two predictors the same name\n  name: \"GNP\""
               (lambda () (coef (named fit twice #f))))
    (check-exn #rx"predict: the model gives two predictors the same name"
               (lambda () (predict (named fit twice #f) longley)))
    (define mg (formula-fit (~ (pulse weight) all) linnerud #:family 'mgaussian #:lambda 1.0))
    (define mg-names (glmnet-model-predictor-names mg))
    (check-equal? (coef (named (formula-model-fit mg) mg-names '("pulse" "weight"))) (coef mg))
    (check-exn #rx"coef: the model does not have one name per response\n  response names: '\\(\"pulse\"\\)\n  responses: 2"
               (lambda () (coef (named (formula-model-fit mg) mg-names '("pulse"))))))

  (test-case "a formula model is made only by the formula procedures"
    (check-exn #rx"formula-model: unbound identifier"
               (lambda () (convert-compile-time-error formula-model))))

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
    (check-regexp-match #rx"^#<glmnet-path:gaussian \\(~ Employed \\(- all Year\\)\\)\n +Df"
                        (format "~a" gpath))
    (check-regexp-match #rx"^#<glmnet-cv:gaussian \\(~ Employed all\\) Mean-Squared Error\n"
                        (format "~a" (formula-cv (~ Employed all) longley
                                                 #:fold-ids (folds 16))))))
