#lang racket/base

;; Every fitter and prediction helper reads its input through the design-matrix
;; layer (#35): a design-matrix argument gives results `equal?` to the same list
;; of rows, one design matrix can be reused across fits, and non-finite input is
;; an error rather than a silent wrong answer. The fixtures are the committed
;; parity datasets, whose CSVs mix exact integers and decimals.

(module+ test
  (require rackunit
           ffi/vector
           glmnet
           (submod glmnet/core/path support)
           (file "../private/demo-utils.rkt"))

  (define-values (Xg yg) (load-longley))
  (define-values (Xb yb) (load-wdbc))
  (define-values (Xm ym) (load-iris))
  (define-values (Xc tc sc) (load-veteran))
  (define-values (Xp yp) (load-warpbreaks))
  (define-values (Xr Yr) (load-linnerud))

  (define (check-same-fit fit X . args)
    (check-equal? (apply fit (rows->design-matrix X) args) (apply fit X args)))

  ;; --- equal? results on lists and on design matrices ---------------------------

  (test-case "Gaussian fits"
    (check-same-fit (lambda (X y) (elnet-fit X y #:lambda 0.1 #:alpha 0.5)) Xg yg)
    (check-same-fit ols Xg yg)
    (check-same-fit (lambda (X y) (ridge X y #:lambda 0.1)) Xg yg)
    (check-same-fit (lambda (X y) (lasso X y #:lambda 0.1)) Xg yg)
    (check-same-fit (lambda (X y) (elastic-net X y #:alpha 0.3 #:lambda 0.1)) Xg yg)
    (check-same-fit elnet-path Xg yg))

  (test-case "binomial fits and predictions"
    (define dm (rows->design-matrix Xb))
    (check-same-fit (lambda (X y) (logistic-fit X y #:lambda 0.02)) Xb yb)
    (check-same-fit logistic-path Xb yb)
    (define r (logistic-fit Xb yb #:lambda 0.02))
    (check-equal? (logistic-predict-proba r dm) (logistic-predict-proba r Xb))
    (check-equal? (logistic-predict r dm #:threshold 0.3) (logistic-predict r Xb #:threshold 0.3)))

  (test-case "multinomial fits and predictions"
    (define dm (rows->design-matrix Xm))
    (check-same-fit (lambda (X y) (multinomial-fit X y #:lambda 0.01)) Xm ym)
    (check-same-fit multinomial-path Xm ym)
    (define r (multinomial-fit Xm ym #:lambda 0.01))
    (check-equal? (multinomial-predict-proba r dm) (multinomial-predict-proba r Xm))
    (check-equal? (multinomial-predict r dm) (multinomial-predict r Xm)))

  (test-case "Cox fits and predictions"
    (define dm (rows->design-matrix Xc))
    (check-same-fit (lambda (X t s) (cox-fit X t s #:lambda 0.05)) Xc tc sc)
    (check-same-fit cox-path Xc tc sc)
    (define r (cox-fit Xc tc sc #:lambda 0.05))
    (check-equal? (cox-linear-predictor r dm) (cox-linear-predictor r Xc))
    (check-equal? (cox-relative-risk r dm) (cox-relative-risk r Xc)))

  (test-case "Poisson fits and predictions"
    (define dm (rows->design-matrix Xp))
    (check-same-fit (lambda (X y) (poisson-fit X y #:lambda 0.1)) Xp yp)
    (check-same-fit poisson-path Xp yp)
    (define r (poisson-fit Xp yp #:lambda 0.1))
    (check-equal? (poisson-predict-mean r dm) (poisson-predict-mean r Xp)))

  (test-case "multi-response fits and predictions, with Y as a design matrix too"
    (define dm (rows->design-matrix Xr))
    (define Ydm (rows->design-matrix Yr))
    (define r (mgaussian-fit Xr Yr #:lambda 1.0))
    (check-equal? (mgaussian-fit dm Ydm #:lambda 1.0) r)
    (check-equal? (mgaussian-fit Xr Ydm #:lambda 1.0) r)
    (check-equal? (mgaussian-path dm Ydm) (mgaussian-path Xr Yr))
    (check-equal? (mgaussian-predict r dm) (mgaussian-predict r Xr)))

  (test-case "column names do not change a fit"
    (define dm (rows->design-matrix Xg #:column-names '(deflator gnp unemployed armed pop year)))
    (check-equal? (lasso dm yg #:lambda 0.1) (lasso Xg yg #:lambda 0.1)))

  ;; --- one design matrix, many fits ---------------------------------------------

  (test-case "a design matrix is reused across fits unchanged"
    (define dm (rows->design-matrix Xg))
    (define before (design-matrix->rows dm))
    (define fits
      (for/list ([lam (in-list '(1.0 0.1 0.01 0.1))])
        (lasso dm yg #:lambda lam)))
    (check-equal? (design-matrix->rows dm) before)
    (check-equal? (list-ref fits 1) (list-ref fits 3))
    (for ([lam (in-list '(1.0 0.1 0.01))] [fit (in-list fits)])
      (check-equal? fit (lasso Xg yg #:lambda lam)))
    (check-equal? (elnet-path dm yg) (elnet-path Xg yg))
    (check-equal? (ridge dm yg #:lambda 0.1) (ridge Xg yg #:lambda 0.1))
    (check-equal? (design-matrix->rows dm) before))

  ;; --- non-finite input (regressions: these used to return coefficients) --------

  (define X '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0) (5.0 6.0)))
  (define y '(1.0 4.0 3.0 6.0 5.0))

  (define (check-error thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract? e)
            (for/and ([p (in-list patterns)])
              (regexp-match? p (exn-message e)))))
     thunk))

  (test-case "a NaN in X is an error naming its row and column"
    (check-error (lambda () (ols '((1.0 2.0) (2.0 +nan.0) (3.0 4.0) (4.0 3.0) (5.0 6.0)) y))
                 #rx"^ols: X has an element that is not finite" #rx"row: 1" #rx"column: 1"))

  (test-case "an infinity in y is an error naming its position"
    (check-error (lambda () (ols X '(1.0 4.0 +inf.0 6.0 5.0)))
                 #rx"^ols: y has an element that is not finite" #rx"position: 2"))

  (test-case "every family rejects non-finite input"
    (check-error (lambda () (elnet-path '((1.0 2.0) (+inf.0 1.0)) '(1.0 2.0))) #rx"row: 1" #rx"column: 0")
    (check-error (lambda () (logistic-fit '((1.0 +nan.0) (2.0 1.0)) '(0 1) #:lambda 0.1)) #rx"row: 0")
    (check-error (lambda () (multinomial-path '((1.0 2.0) (2.0 -inf.0)) '(0 1))) #rx"column: 1")
    (check-error (lambda () (cox-fit X '(1.0 2.0 +inf.0 4.0 5.0) '(1 1 1 1 1) #:lambda 0.1))
                 #rx"times has an element that is not finite" #rx"position: 2")
    (check-error (lambda () (poisson-fit X '(1 2 +inf.0 4 5) #:lambda 0.1))
                 #rx"y has an element that is not finite")
    (check-error (lambda () (mgaussian-fit X '((1.0 2.0) (2.0 1.0) (3.0 +nan.0) (4.0 3.0) (5.0 6.0))
                                           #:lambda 0.1))
                 #rx"Y has an element that is not finite" #rx"row: 2" #rx"column: 1"))

  (test-case "prediction helpers reject non-finite and mis-shaped input"
    (define r (poisson-fit X '(1 2 2 3 5) #:lambda 0.1))
    (check-error (lambda () (poisson-predict-mean r '((1.0 +nan.0))))
                 #rx"^poisson-predict-mean: X has an element that is not finite")
    (check-error (lambda () (poisson-predict-mean r '((1.0 2.0 3.0))))
                 #rx"one column per coefficient" #rx"columns of X: 3" #rx"coefficients: 2")
    (check-error (lambda () (poisson-predict-mean r (rows->design-matrix '((1.0)))))
                 #rx"one column per coefficient"))

  (test-case "shape mismatches name both arguments"
    (check-error (lambda () (lasso X '(1.0 2.0) #:lambda 0.1))
                 #rx"y does not have one entry per row of X" #rx"length of y: 2" #rx"rows of X: 5")
    (check-error (lambda () (mgaussian-fit X '((1.0) (2.0)) #:lambda 0.1))
                 #rx"Y does not have one row per row of X" #rx"rows of Y: 2"))

  ;; --- the output direction: dense f64vector -> vectors ---------------------------

  (test-case "unpack-vector reads the first n entries"
    (check-equal? (unpack-vector (f64vector 1.0 2.0 3.0) 2) #(1.0 2.0))
    (check-equal? (unpack-vector (f64vector 1.0 2.0 3.0) 0) #()))

  (test-case "unpack-columns reads column-major (and class-major) blocks"
    (define v (f64vector 1.0 2.0 3.0 4.0 5.0 6.0))
    (check-equal? (unpack-columns v 3 2) #(#(1.0 2.0 3.0) #(4.0 5.0 6.0)))
    (check-equal? (unpack-columns v 2 3) #(#(1.0 2.0) #(3.0 4.0) #(5.0 6.0)))
    (check-equal? (unpack-columns v 2 1) #(#(1.0 2.0)))))
