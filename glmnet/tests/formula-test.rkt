#lang racket/base

;; The formula front end (#26, #53). A formula fit is the matrix fit of its
;; design matrix, `equal?` to it for every family and for single fits, paths
;; and cross-validation. The formula language is R's algebra: terms expand and
;; order as R's terms() does, prefix and infix, and the design matrix holds
;; their products; unknown columns are errors, and a response on the
;; right-hand side is dropped with a warning. `coef` is keyed by name, and
;; `predict` rebuilds the design matrix from a table, whatever the order of
;; its columns. The fixtures are the committed parity datasets, read as tables
;; with the CSV's column names, and mtcars. parity-test.rkt checks the algebra
;; against R itself.

(module+ test
  (require rackunit
           (only-in racket/contract exn:fail:contract:blame?)
           racket/generic
           racket/list
           racket/match
           syntax/macro-testing
           glmnet
           glmnet/examples/data/mtcars
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
                  (elnet-fit (rows-of mtcars '("hp" "qsec")) (cdr (assoc "mpg" mtcars)) #:lambda 0.1
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
    (define (column name) (map exact->inexact (cdr (assoc name mtcars))))
    (check-equal? (hash-ref x "wt") (column "wt"))
    (check-equal? (hash-ref x "wt:hp") (map * (column "wt") (column "hp")))
    (check-equal? (hash-ref x "wt:hp:qsec") (map * (column "wt") (column "hp") (column "qsec")))
    (check-equal? (design-matrix-column-names (formula-design-matrix (~ mpg (* hp wt)) mtcars))
                  '("hp" "wt" "hp:wt")))

  (test-case "the design matrix is what a formula fit fits"
    (define f (~ mpg (^ (+ wt hp qsec) 2)))
    (define x (formula-design-matrix f mtcars))
    (define y (cdr (assoc "mpg" mtcars)))
    (check-same (formula-path f mtcars #:lambda '(1.0 0.1)) (elnet-path x y #:lambda '(1.0 0.1)))
    (check-equal? (formula-model-predictor-names (formula-fit f mtcars #:lambda 0.1))
                  (design-matrix-column-names x)))

  ;; --- the intercept -------------------------------------------------------------------

  (define mtcars-rows (rows-of mtcars '("wt" "hp")))
  (define mpg (cdr (assoc "mpg" mtcars)))

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
  ;; it twice. The reader reads -0 as 0, which would invert both.
  (test-case "a sign glued to 0 or 1 is a syntax error; (- 0) is R's -0, both ways"
    (define (message-of thunk)
      (with-handlers ([exn:fail:syntax? exn-message]) (thunk) #f))
    (check-regexp-match
     #rx"~: 0 has a sign glued to it, which the reader drops, reading -0 as 0; put a space after the sign, as in \\(- 0\\)\n  at: 0"
     (message-of (lambda () (convert-compile-time-error (mpg . ~ . wt + hp + -0)))))
    (check-regexp-match #rx"~: 0 has a sign glued to it"
                        (message-of (lambda () (convert-compile-time-error (mpg . ~ . wt + hp - -0)))))
    (check-regexp-match #rx"~: 0 has a sign glued to it"
                        (message-of (lambda () (convert-compile-time-error (~ mpg wt hp -0)))))
    (check-regexp-match #rx"~: 0 has a sign glued to it"
                        (message-of (lambda () (convert-compile-time-error (~ mpg (- (+ wt hp) +0))))))
    (check-regexp-match
     #rx"~: 1 has a sign glued to it, which the reader drops, reading \\+1 as 1; put a space after the sign, as in \\(\\+ 1\\)\n  at: 1"
     (message-of (lambda () (convert-compile-time-error (mpg . ~ . +1 + wt + hp)))))
    (define without (elnet-fit mtcars-rows mpg #:lambda 0.1 #:intercept? #f))
    (define with (elnet-fit mtcars-rows mpg #:lambda 0.1))
    (for ([f (list (mpg . ~ . wt + hp + (- 0)) (~ mpg wt hp (- 0)) (mpg . ~ . - 0 + wt + hp))])
      (check-same (formula-fit f mtcars #:lambda 0.1) with))
    (for ([f (list (mpg . ~ . wt + hp - (- 0)) (~ mpg (- (+ wt hp) (- 0))))])
      (check-same (formula-fit f mtcars #:lambda 0.1) without)))

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

  (test-case "the Cox family accepts intercept terms and ignores them"
    (define fit (formula-fit (~ (surv time status) karno age) veteran #:family 'cox #:lambda 0.05))
    (check-same (formula-fit (~ (surv time status) 0 karno age) veteran #:family 'cox #:lambda 0.05)
                (formula-model-fit fit))
    (check-same (formula-fit (~ (surv time status) 1 + karno + age) veteran #:family 'cox
                             #:lambda 0.05 #:intercept? #f)
                (formula-model-fit fit)))

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
    (check-exn #rx"~: / is an operator of R's formulas that this formula language does not have\n  at: /"
               (lambda () (convert-compile-time-error (y . ~ . a / b))))
    (check-exn #rx"~: %in% is an operator of R's formulas"
               (lambda () (convert-compile-time-error (~ y (a %in% b)))))
    (check-exn #rx"~: / is an operator"
               (lambda () (convert-compile-time-error (~ y a /))))
    (check-exn #rx"~: / is an operator of R's formulas that this formula language does not have\n  at: /"
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
     #rx"~: \\(sqr x\\) is a function call, a transform, which the formula language does not support yet\n  at: \\(sqr x\\)"
     (message-of (lambda () (convert-compile-time-error (~ y x (sqr x))))))
    (check-regexp-match #rx"~: \\(I \\(\\* x x\\)\\) is a function call"
                        (message-of (lambda () (convert-compile-time-error (~ y (+ x (I (* x x))))))))
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
    (define (too-many-folds? n)
      (lambda (e)
        (and (exn:fail:contract? e)
             (not (exn:fail:contract:blame? e))
             (regexp-match?
              (pregexp (format "^formula-cv: there are more folds than observations\n  folds: ~a\n  observations: 6" n))
              (exn-message e)))))
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
