#lang racket/base

;; Every fitter, path, cross-validation and prediction helper accepts a matrix
;; in any of the four nestings of lists and vectors, and a one-dimensional
;; input as a list, vector, flvector or f64vector (#36), with results `equal?`
;; to the same data as lists. The fixtures are the committed parity datasets,
;; whose CSVs mix exact integers and decimals.

(module+ test
  (require rackunit
           (only-in racket/contract exn:fail:contract:blame?)
           racket/flonum
           racket/list
           ffi/vector
           glmnet
           glmnet/data/nested
           (file "../private/demo-utils.rkt"))

  (define-values (Xg yg) (load-longley))
  (define-values (Xb yb) (load-wdbc))
  (define-values (Xm ym) (load-iris))
  (define-values (Xc tc sc) (load-veteran))
  (define-values (Xp yp) (load-warpbreaks))
  (define-values (Xr Yr) (load-linnerud))

  ;; The same rows as a list of vectors, a vector of lists and a vector of
  ;; vectors.
  (define (other-nestings X)
    (list (map list->vector X)
          (list->vector X)
          (list->vector (map list->vector X))))

  (define (vector-of-vectors X) (list->vector (map list->vector X)))

  ;; The same entries as a vector, an flvector and an f64vector.
  (define (other-forms y)
    (define fs (map real->double-flonum y))
    (list (list->vector y) (apply flvector fs) (list->f64vector fs)))

  (define (as-flvector y) (apply flvector (map real->double-flonum y)))

  ;; (apply fit X ys) with X in each other nesting, with each one-dimensional
  ;; argument in each other form, and with all of them as vectors at once, is
  ;; `equal?` to the fit on lists.
  (define (check-same fit X . ys)
    (define expected (apply fit X ys))
    (for ([X* (in-list (other-nestings X))])
      (check-equal? (apply fit X* ys) expected))
    (for* ([k (in-range (length ys))]
           [y* (in-list (other-forms (list-ref ys k)))])
      (check-equal? (apply fit X (list-set ys k y*)) expected))
    (check-equal? (apply fit (vector-of-vectors X) (map as-flvector ys)) expected))

  ;; A prediction on new data in each other nesting is `equal?` to the one on
  ;; the list.
  (define (check-same-prediction predict-with X)
    (define expected (predict-with X))
    (for ([X* (in-list (other-nestings X))])
      (check-equal? (predict-with X*) expected)))

  ;; Three folds, fixed, so that every cross-validation below is reproducible.
  (define (folds n) (for/list ([i (in-range n)]) (modulo i 3)))

  (test-case "Gaussian fits, path and cross-validation"
    (check-same (lambda (X y) (elnet-fit X y #:lambda 0.1 #:alpha 0.5)) Xg yg)
    (check-same ols Xg yg)
    (check-same (lambda (X y) (ridge X y #:lambda 0.1)) Xg yg)
    (check-same (lambda (X y) (lasso X y #:lambda 0.1)) Xg yg)
    (check-same (lambda (X y) (elastic-net X y #:alpha 0.3 #:lambda 0.1)) Xg yg)
    (check-same elnet-path Xg yg)
    (check-same (lambda (X y) (elnet-cv X y #:nlambda 10 #:fold-ids (folds 16))) Xg yg))

  (test-case "Gaussian predictions"
    (define r (lasso Xg yg #:lambda 0.1))
    (define p (elnet-path Xg yg))
    (check-same-prediction (lambda (X) (elnet-predict r X)) Xg)
    (check-same-prediction (lambda (X) (predict r X)) Xg)
    (check-same-prediction (lambda (X) (predict p X #:lambda '(0.5 0.05))) Xg))

  (test-case "binomial fits, path and cross-validation"
    (check-same (lambda (X y) (logistic-fit X y #:lambda 0.02)) Xb yb)
    (check-same (lambda (X y) (logistic-path X y #:nlambda 10)) Xb yb)
    (check-same (lambda (X y) (logistic-cv X y #:nlambda 10 #:fold-ids (folds 569)
                                           #:type-measure 'class))
                Xb yb))

  (test-case "binomial predictions"
    (define r (logistic-fit Xb yb #:lambda 0.02))
    (check-same-prediction (lambda (X) (logistic-predict-proba r X)) Xb)
    (check-same-prediction (lambda (X) (logistic-predict r X #:threshold 0.3)) Xb)
    (check-same-prediction (lambda (X) (predict r X #:type 'class)) Xb))

  (test-case "multinomial fits, path and cross-validation, with inexact labels"
    (check-same (lambda (X y) (multinomial-fit X y #:lambda 0.01)) Xm ym)
    (check-same (lambda (X y) (multinomial-path X y #:nlambda 10)) Xm ym)
    (check-same (lambda (X y) (multinomial-cv X y #:nlambda 10 #:fold-ids (folds 150)
                                              #:type-measure 'class))
                Xm ym)
    (check-equal? (multinomial-fit Xm (map exact->inexact ym) #:lambda 0.01)
                  (multinomial-fit Xm ym #:lambda 0.01)))

  (test-case "multinomial predictions"
    (define r (multinomial-fit Xm ym #:lambda 0.01))
    (check-same-prediction (lambda (X) (multinomial-predict-proba r X)) Xm)
    (check-same-prediction (lambda (X) (multinomial-predict r X)) Xm))

  (test-case "Cox fits, path and cross-validation, times and statuses in every form"
    (check-same (lambda (X t s) (cox-fit X t s #:lambda 0.05)) Xc tc sc)
    (check-same (lambda (X t s) (cox-path X t s #:nlambda 10)) Xc tc sc)
    (check-same (lambda (X t s) (cox-cv X t s #:nlambda 10 #:fold-ids (folds 137))) Xc tc sc))

  (test-case "Cox predictions"
    (define r (cox-fit Xc tc sc #:lambda 0.05))
    (check-same-prediction (lambda (X) (cox-linear-predictor r X)) Xc)
    (check-same-prediction (lambda (X) (cox-relative-risk r X)) Xc))

  (test-case "Poisson fits, path and cross-validation"
    (check-same (lambda (X y) (poisson-fit X y #:lambda 0.1)) Xp yp)
    (check-same (lambda (X y) (poisson-path X y #:nlambda 10)) Xp yp)
    (check-same (lambda (X y) (poisson-cv X y #:nlambda 10 #:fold-ids (folds 54))) Xp yp))

  (test-case "Poisson predictions"
    (define r (poisson-fit Xp yp #:lambda 0.1))
    (check-same-prediction (lambda (X) (poisson-predict-mean r X)) Xp))

  (test-case "multi-response fits, path and cross-validation, Y in every nesting"
    (define (check-same-mgaussian fit)
      (define expected (fit Xr Yr))
      (for* ([X* (in-list (cons Xr (other-nestings Xr)))]
             [Y* (in-list (cons Yr (other-nestings Yr)))])
        (check-equal? (fit X* Y*) expected)))
    (check-same-mgaussian (lambda (X Y) (mgaussian-fit X Y #:lambda 1.0)))
    (check-same-mgaussian (lambda (X Y) (mgaussian-path X Y #:nlambda 10)))
    (check-same-mgaussian (lambda (X Y) (mgaussian-cv X Y #:nlambda 10 #:fold-ids (folds 20)))))

  (test-case "multi-response predictions"
    (define r (mgaussian-fit Xr Yr #:lambda 1.0))
    (check-same-prediction (lambda (X) (mgaussian-predict r X)) Xr))

  (test-case "a nesting converted once fits as the nesting does"
    (define rows (vector-of-vectors Xg))
    (define dm (nested->design-matrix rows))
    (check-equal? dm (rows->design-matrix Xg))
    (check-equal? (lasso dm (as-flvector yg) #:lambda 0.1) (lasso rows yg #:lambda 0.1))
    (define columns (list->vector (map list->vector (apply map list Xg))))
    (check-equal? (nested->design-matrix columns #:by 'columns) dm))

  (test-case "a nesting with column names is a table for the formula front end"
    (define names '("GNP.deflator" "GNP" "Unemployed" "Armed.Forces" "Population" "Year" "Employed"))
    (define rows
      (for/vector ([x (in-list Xg)] [y (in-list yg)])
        (list->vector (append x (list y)))))
    (define table (nested->design-matrix rows #:column-names names))
    (define f (~ Employed all))
    (check-equal? (coef (formula-fit f table #:lambda 0.1))
                  (coef (formula-fit f (load-table "longley") #:lambda 0.1))))

  ;; --- contract errors name the shapes ---------------------------------------------

  (define (check-blame thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract:blame? e)
            (for/and ([p (in-list (cons #rx"blaming: [^\n]*nested-fit-test[.]rkt" patterns))])
              (regexp-match? p (exn-message e)))))
     thunk))

  (test-case "a matrix of the wrong shape names the accepted shapes"
    (check-blame (lambda () (ols (flvector 1.0 2.0) yg))
                 #rx"^ols: contract violation"
                 #rx"expected: unnamed data \\(a design matrix, a list or vector of rows or a math/matrix matrix\\)"
                 #rx"or named data \\(a table or a Polars dataframe\\)"
                 #rx"in: the X argument of")
    (check-blame (lambda () (mgaussian-fit Xr #(1.0 2.0) #:lambda 1.0))
                 #rx"expected: unnamed data \\(a design matrix, a list or vector of rows or a math/matrix matrix\\), since X is unnamed"
                 #rx"in: the Y argument of"))

  (test-case "new data of the wrong shape names the accepted shapes"
    (define r (lasso Xg yg #:lambda 0.1))
    (check-blame (lambda () (elnet-predict r (flvector 1.0 2.0)))
                 #rx"^elnet-predict: contract violation"
                 #rx"a list or vector of rows" #rx"given: \\(flvector 1.0 2.0\\)")
    (check-blame (lambda () (predict r #(1.0 2.0)))
                 #rx"^predict: contract violation"
                 #rx"a list or vector of rows" #rx"named data \\(a table or a Polars dataframe\\)"))

  (test-case "a response of the wrong shape names the accepted shapes"
    (check-blame (lambda () (ols Xg "y"))
                 #rx"expected: a non-empty list, vector, flvector or f64vector of real\\?")
    (check-blame (lambda () (cox-fit Xc tc (vector) #:lambda 0.05))
                 #rx"expected: a non-empty list, vector, flvector or f64vector of \\(or/c 0 1\\)"
                 #rx"in: the statuses argument of"))

  (test-case "a response entry that fails is named by its position"
    (check-blame (lambda () (logistic-fit Xb (list->vector (list-set yb 7 2)) #:lambda 0.02))
                 #rx"expected: \\(or/c 0 1\\)" #rx"given: 2" #rx"the element at position 7 of")
    (check-blame (lambda () (poisson-fit Xp (as-flvector (list-set yp 3 -1)) #:lambda 0.1))
                 #rx"given: -1.0" #rx"the element at position 3 of")
    (check-blame (lambda () (multinomial-fit Xm (as-flvector (list-set ym 0 1.5)) #:lambda 0.01))
                 #rx"expected: integer\\?" #rx"given: 1.5" #rx"the element at position 0 of"))

  (test-case "multinomial labels are checked before the classes are tallied"
    (check-exn #rx"y does not have one entry per row of X"
               (lambda () (multinomial-fit Xm (vector 0 1) #:lambda 0.01)))
    (check-exn #rx"^multinomial-fit: class 3 has no observations; labels must cover 0\\.\\.999999999"
               (lambda () (multinomial-fit Xm (as-flvector (list-set ym 0 999999999))
                                           #:lambda 0.01)))
    (check-exn #rx"^multinomial-path: class 2 has no observations"
               (lambda () (multinomial-path '((1.0) (2.0) (3.0)) (flvector 0.0 1.0 1e300)))))

  (test-case "the response checks read the values the Fortran gets"
    (check-exn #rx"y is constant"
               (lambda () (ols '((1.0) (2.0) (3.0)) (list 1/3 (+ 1/3 (expt 10 -30)) 1/3))))
    (check-exn #rx"no positive count"
               (lambda () (poisson-fit '((1.0) (2.0)) (list 0 (expt 10 -400)) #:lambda 0.1))))

  (test-case "non-finite entries and mismatched lengths in vectors are errors"
    (check-exn #rx"^ols: X has an element that is not finite\n  column: 1\n  row: 1"
               (lambda () (ols (vector #(1.0 2.0) (vector 2.0 +nan.0) #(3.0 1.0)) '(1.0 2.0 3.0))))
    (check-exn #rx"^ols: y has an element that is not finite\n  position: 2"
               (lambda () (ols (vector-of-vectors Xg) (as-flvector (list-set yg 2 +inf.0)))))
    (check-exn #rx"y does not have one entry per row of X\n  length of y: 2\n  rows of X: 16"
               (lambda () (ols (vector-of-vectors Xg) (f64vector 1.0 2.0))))
    (check-exn #rx"statuses does not have one entry per row of X"
               (lambda () (cox-fit Xc tc (vector 1 0) #:lambda 0.05))))

  (test-case "a response is checked again on its copy, which the caller cannot change"
    ;; A vector whose entries read as 2 once the contract has read each of them.
    (define reads 0)
    (define labels
      (impersonate-vector (vector 0 1 0 1 1 0)
                          (lambda (v i x) (set! reads (add1 reads)) (if (> reads 6) 2 x))
                          (lambda (v i x) x)))
    (check-exn #rx"^logistic-fit: y has an element that is not \\(or/c 0 1\\) as a flonum\n  position: 0\n  element: 2$"
               (lambda () (logistic-fit '((1.0) (2.0) (3.0) (4.0) (5.0) (6.0)) labels #:lambda 0.1))))

  (test-case "a positive Cox time that rounds to 0.0 is an error"
    (check-exn #rx"^cox-fit: times has an element that is not \\(>/c 0\\) as a flonum\n  position: 1\n"
               (lambda () (cox-fit '((1.0 0.0) (0.0 1.0) (1.0 1.0) (0.0 0.0))
                                   (list 1 (expt 10 -400) 3 4) '(1 1 0 1) #:lambda 0.1)))
    (check-exn #rx"^cox-path: times has an element that is not \\(>/c 0\\) as a flonum"
               (lambda () (cox-path '((1.0 0.0) (0.0 1.0) (1.0 1.0) (0.0 0.0))
                                    (vector 1 2 (expt 10 -400) 4) '(1 1 0 1))))))
