#lang racket/base

;; Unit tests for the generic model interface (#25): `predict`, `coef` and
;; `deviance-ratio` on every result type, R's lambda interpolation along a path,
;; the family prediction helpers as wrappers over `predict`, and how results
;; print. R parity for predict and coef lives in parity-test.rkt.

(module+ test
  (require rackunit
           racket/generic
           racket/list
           racket/match
           racket/vector
           glmnet)

  (define X '((1.0 2.0  1.0)
              (2.0 1.0  4.0)
              (3.0 4.0  9.0)
              (4.0 3.0 16.0)
              (5.0 6.0 25.0)
              (6.0 5.0 36.0)))
  (define y '(1.0 4.0 3.0 6.0 5.0 8.0))
  (define new-rows '((7.0 6.0 49.0) (0.5 1.0 0.25)))

  (define Xl '((1.0 5.0 2.0) (2.0 6.0 1.0) (2.0 5.0 3.0) (1.0 4.0 1.0) (3.0 6.0 2.0) (2.0 4.0 2.0)
               (6.0 2.0 2.0) (5.0 1.0 1.0) (6.0 1.0 3.0) (5.0 2.0 1.0) (4.0 1.0 2.0) (6.0 3.0 2.0)))
  (define yl '(0 0 0 0 0 0 1 1 1 1 1 1))
  (define Xm '((1.0 1.0) (2.0 1.0) (1.0 2.0) (2.0 2.0) (5.0 1.0) (6.0 1.0)
               (5.0 2.0) (6.0 2.0) (3.0 5.0) (4.0 5.0) (3.0 6.0) (4.0 6.0)))
  (define ym '(0 0 0 0 1 1 1 1 2 2 2 2))
  (define Xc '((0.5 1.0) (1.0 2.0) (1.5 1.0) (2.0 2.0) (2.5 1.0) (3.0 2.0) (3.5 1.0) (4.0 2.0)))
  (define tc '(12.0 10.0 11.0 8.0 6.0 7.0 4.0 3.0))
  (define sc '(0 0 0 1 1 1 1 1))
  (define Xp '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0) (5.0 2.0) (6.0 1.0) (7.0 2.0) (8.0 1.0)))
  (define yp '(1 2 2 3 4 6 8 11))
  (define Xg '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0) (5.0 2.0) (6.0 1.0)))
  (define Yg '((3.0 9.0) (5.0 8.0) (7.0 7.0) (9.0 6.0) (11.0 5.0) (13.0 4.0)))

  (define gaussian (lasso X y #:lambda 0.05))
  (define binomial (logistic-fit Xl yl #:lambda 0.04))
  (define multinomial (multinomial-fit Xm ym #:lambda 0.01))
  (define cox (cox-fit Xc tc sc #:lambda 0.1))
  (define poisson (poisson-fit Xp yp #:lambda 0.2))
  (define mgaussian (mgaussian-fit Xg Yg #:lambda 0.1))

  (define (check-list= got want [tol 1e-12])
    (check-equal? (length got) (length want))
    (for ([g (in-list got)] [w (in-list want)])
      (if (list? w) (check-list= g w tol) (check-= g w tol))))

  (define (dot row beta)
    (for/sum ([x (in-list row)] [b (in-vector beta)]) (* x b)))

  (define (check-contract-error thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract? e)
            (for/and ([p (in-list patterns)])
              (regexp-match? p (exn-message e)))))
     thunk))

  ;; --- the interface -----------------------------------------------------------

  (test-case "every result type, and a path, is a glmnet-model"
    (for ([m (list gaussian binomial multinomial cox poisson mgaussian (elnet-path X y))])
      (check-true (glmnet-model? m)))
    (check-false (glmnet-model? X)))

  (test-case "a single fit is a path with one lambda"
    (define p (glmnet-model->path gaussian))
    (check-eq? (glmnet-path-family p) 'gaussian)
    (check-equal? (glmnet-path-lambda p) (vector (elnet-result-lambda gaussian)))
    (check-equal? (glmnet-path-intercepts p) (vector (elnet-result-intercept gaussian)))
    (check-equal? (glmnet-path-coefficients p) (vector (elnet-result-coefficients gaussian)))
    (check-equal? (glmnet-path-dev-ratio p) (vector (elnet-result-r-squared gaussian)))
    (check-equal? (glmnet-path-df p) #(2))
    (check-false (glmnet-path-intercepts (glmnet-model->path cox)))
    (check-equal? (glmnet-path-df (glmnet-model->path multinomial)) #(2))
    (check-eq? (glmnet-path-family (glmnet-model->path mgaussian)) 'mgaussian))

  (test-case "a path is its own path view"
    (define p (elnet-path X y #:nlambda 5))
    (check-eq? (glmnet-model->path p) p)
    (check-equal? (glmnet-model-default-lambda p) (vector->list (glmnet-path-lambda p))))

  (test-case "the default lambda of a single fit is its lambda"
    (check-equal? (glmnet-model-default-lambda gaussian) 0.05)
    (check-equal? (glmnet-model-default-lambda cox) 0.1))

  (test-case "deviance-ratio: a real for a single fit, a vector for a path"
    (check-equal? (deviance-ratio gaussian) (elnet-result-r-squared gaussian))
    (check-equal? (deviance-ratio binomial) (logistic-result-dev-ratio binomial))
    (check-equal? (deviance-ratio multinomial) (multinomial-result-dev-ratio multinomial))
    (check-equal? (deviance-ratio cox) (cox-result-dev-ratio cox))
    (check-equal? (deviance-ratio poisson) (poisson-result-dev-ratio poisson))
    (check-equal? (deviance-ratio mgaussian) (mgaussian-result-r-squared mgaussian))
    (define p (elnet-path X y))
    (check-eq? (deviance-ratio p) (glmnet-path-dev-ratio p)))

  ;; --- predict, per family ---------------------------------------------------------

  (test-case "Gaussian: link and response are the fitted value a0 + x.beta"
    (define want
      (for/list ([row (in-list new-rows)])
        (+ (elnet-result-intercept gaussian) (dot row (elnet-result-coefficients gaussian)))))
    (check-list= (predict gaussian new-rows) want)
    (check-equal? (predict gaussian new-rows #:type 'response) (predict gaussian new-rows))
    (check-equal? (elnet-predict gaussian new-rows) (predict gaussian new-rows)))

  (test-case "binomial: log-odds, probability, and class 1 where the log-odds are positive"
    (define eta (predict binomial Xl))
    (define p (predict binomial Xl #:type 'response))
    (check-list= p (for/list ([e (in-list eta)]) (/ 1.0 (+ 1.0 (exp (- e))))))
    (check-equal? (logistic-predict-proba binomial Xl) p)
    (check-equal? (predict binomial Xl #:type 'class)
                  (for/list ([e (in-list eta)]) (if (> e 0) 1 0)))
    (check-equal? (predict binomial Xl #:type 'class) yl)
    (check-equal? (logistic-predict binomial Xl) yl))

  (test-case "multinomial: per-class linear predictors, their softmax, and the argmax"
    (define etas (predict multinomial Xm))
    (define probs (predict multinomial Xm #:type 'response))
    (check-equal? (length (first etas)) 3)
    (for ([e (in-list etas)] [p (in-list probs)])
      (define z (for/sum ([v (in-list e)]) (exp v)))
      (check-list= p (for/list ([v (in-list e)]) (/ (exp v) z)))
      (check-= (apply + p) 1.0 1e-12))
    (check-equal? (multinomial-predict-proba multinomial Xm) probs)
    (define classes (predict multinomial Xm #:type 'class))
    (check-equal? classes
                  (for/list ([e (in-list etas)]) (index-of e (apply max e))))
    (check-equal? (multinomial-predict multinomial Xm) classes)
    (check-equal? classes ym))

  (test-case "Cox: x.beta with no intercept, and exp(x.beta)"
    (define eta (predict cox Xc))
    (check-list= eta (for/list ([row (in-list Xc)]) (dot row (cox-result-coefficients cox))))
    (check-list= (predict cox Xc #:type 'response) (map exp eta))
    (check-equal? (cox-linear-predictor cox Xc) eta)
    (check-equal? (cox-relative-risk cox Xc) (predict cox Xc #:type 'response)))

  (test-case "Poisson: log mean and mean"
    (define eta (predict poisson Xp))
    (check-list= (predict poisson Xp #:type 'response) (map exp eta))
    (check-equal? (poisson-predict-mean poisson Xp) (predict poisson Xp #:type 'response)))

  (test-case "multi-response: one prediction per response, link and response alike"
    (define preds (predict mgaussian Xg))
    (check-equal? (length (first preds)) 2)
    (check-equal? (predict mgaussian Xg #:type 'response) preds)
    (check-equal? (mgaussian-predict mgaussian Xg) preds))

  (test-case "'class is only for the binomial and multinomial families"
    (for ([m (list gaussian cox poisson mgaussian (elnet-path X y))])
      (check-contract-error (lambda () (predict m X #:type 'class))
                            #rx"^predict: type 'class is only for the binomial and multinomial"))
    (check-contract-error (lambda () (predict gaussian X #:type 'probability)) #rx"predict"))

  (test-case "predict takes a design matrix or a list of rows"
    (check-equal? (predict binomial (rows->design-matrix Xl) #:type 'response)
                  (predict binomial Xl #:type 'response)))

  (test-case "errors name the procedure that was called"
    (check-contract-error (lambda () (predict gaussian '((1.0 2.0))))
                          #rx"^predict: X does not have one column per coefficient")
    (check-contract-error (lambda () (elnet-predict gaussian '((1.0 2.0))))
                          #rx"^elnet-predict: X does not have one column per coefficient")
    (check-contract-error (lambda () (logistic-predict binomial '((1.0 +nan.0 2.0))))
                          #rx"^logistic-predict: X has an element that is not finite")
    (check-contract-error (lambda () (predict gaussian X #:lambda -1.0)) #rx"predict")
    (check-contract-error (lambda () (coef gaussian #:lambda '())) #rx"coef"))

  ;; --- coef ------------------------------------------------------------------------

  (test-case "coef puts the intercept first, as R does"
    (check-equal? (coef gaussian)
                  (vector-append (vector (elnet-result-intercept gaussian))
                                 (elnet-result-coefficients gaussian)))
    (check-equal? (coef poisson)
                  (vector-append (vector (poisson-result-intercept poisson))
                                 (poisson-result-coefficients poisson))))

  (test-case "Cox coefficients have no intercept"
    (check-equal? (coef cox) (cox-result-coefficients cox))
    (check-not-eq? (coef cox) (cox-result-coefficients cox)))

  (test-case "multinomial and multi-response coef: one vector per class or response"
    (define mc (coef multinomial))
    (check-equal? (vector-length mc) 3)
    (for ([v (in-vector mc)]
          [a (in-vector (multinomial-result-intercepts multinomial))]
          [b (in-vector (multinomial-result-coefficients multinomial))])
      (check-equal? v (vector-append (vector a) b)))
    (define gc (coef mgaussian))
    (check-equal? (vector-length gc) 2)
    (check-equal? (vector-ref (vector-ref gc 1) 0)
                  (vector-ref (mgaussian-result-intercepts mgaussian) 1)))

  (test-case "multinomial intercepts are centred, as R centres them"
    (check-= (for/sum ([a (in-vector (multinomial-result-intercepts multinomial))]) a) 0.0 1e-12)
    (define p (multinomial-path Xm ym #:nlambda 10))
    (for ([a0 (in-vector (glmnet-path-intercepts p))])
      (check-= (for/sum ([a (in-vector a0)]) a) 0.0 1e-12)))

  ;; --- lambda ------------------------------------------------------------------------

  (test-case "a single fit ignores #:lambda, as R does for a one-lambda fit"
    (check-equal? (coef gaussian #:lambda 3.0) (coef gaussian))
    (check-equal? (predict binomial Xl #:lambda 0.0 #:type 'response)
                  (predict binomial Xl #:type 'response))
    (check-equal? (coef gaussian #:lambda '(0.05 1.0 0.0)) (make-list 3 (coef gaussian))))

  (define path (elnet-path X y #:nlambda 20))
  (define lams (glmnet-path-lambda path))
  (define L (vector-length lams))

  (define (stored m)
    (vector-append (vector (vector-ref (glmnet-path-intercepts path) m))
                   (vector-ref (glmnet-path-coefficients path) m)))

  (test-case "on the path, coef is the fitted point exactly"
    (for ([lam (in-vector lams)] [m (in-naturals)])
      (check-equal? (coef path #:lambda lam) (stored m))))

  (test-case "without #:lambda, a path gives one entry per fitted lambda"
    (check-equal? (coef path) (for/list ([m (in-range L)]) (stored m)))
    (define preds (predict path new-rows))
    (check-equal? (length preds) L)
    (check-equal? (list-ref preds 3) (predict path new-rows #:lambda (vector-ref lams 3))))

  (test-case "between two fitted lambdas, coef interpolates linearly in lambda"
    (define-values (hi lo) (values (vector-ref lams 4) (vector-ref lams 5)))
    (define s (+ (* 0.3 hi) (* 0.7 lo)))
    (define frac (/ (- s lo) (- hi lo)))
    (define want
      (for/vector ([a (in-vector (stored 4))] [b (in-vector (stored 5))])
        (+ (* frac a) (* (- 1 frac) b))))
    (for ([got (in-vector (coef path #:lambda s))] [w (in-vector want)])
      (check-= got w 1e-12)))

  (test-case "predict at s is the linear predictor of coef at s"
    (define s (* 0.5 (+ (vector-ref lams 2) (vector-ref lams 3))))
    (define c (coef path #:lambda s))
    (check-list= (predict path new-rows #:lambda s)
                 (for/list ([row (in-list new-rows)])
                   (+ (vector-ref c 0) (dot row (vector-drop c 1))))))

  (test-case "outside the path, lambda is clamped to its ends"
    (check-equal? (coef path #:lambda (* 10 (vector-ref lams 0))) (stored 0))
    (check-equal? (coef path #:lambda 0.0) (stored (sub1 L)))
    (check-equal? (coef path #:lambda +inf.0) (stored 0)))

  (test-case "a list of lambdas gives one entry per lambda, in the order given"
    (define s (list (vector-ref lams 7) 100.0 (vector-ref lams 2) 0.0))
    (check-equal? (coef path #:lambda s)
                  (list (stored 7) (stored 0) (stored 2) (stored (sub1 L)))))

  (test-case "a user path interpolates between its own lambdas"
    (define p (elnet-path X y #:lambda '(1.0 0.1)))
    (define c1 (coef p #:lambda 1.0))
    (define c2 (coef p #:lambda 0.1))
    (for ([got (in-vector (coef p #:lambda 0.4))] [a (in-vector c1)] [b (in-vector c2)])
      (check-= got (+ (* 1/3 a) (* 2/3 b)) 1e-12)))

  (test-case "a one-lambda path behaves as a single fit"
    (define p (elnet-path X y #:lambda '(0.05)))
    (check-equal? (coef p #:lambda 5.0) (coef p #:lambda 0.05))
    (check-equal? (coef p) (list (coef p #:lambda 0.05))))

  (test-case "a repeated lambda in a user path selects its fitted point"
    (define p (elnet-path X y #:lambda '(1.0 0.1 0.1 0.01)))
    (check-equal? (coef p #:lambda 0.1)
                  (vector-append (vector (vector-ref (glmnet-path-intercepts p) 1))
                                 (vector-ref (glmnet-path-coefficients p) 1))))

  (test-case "multinomial and Cox paths interpolate every class and have the right shape"
    (define mp (multinomial-path Xm ym #:nlambda 10))
    (define s (* 0.5 (+ (vector-ref (glmnet-path-lambda mp) 3) (vector-ref (glmnet-path-lambda mp) 4))))
    (define mc (coef mp #:lambda s))
    (check-equal? (vector-length mc) 3)
    (check-equal? (length (first (predict mp Xm #:lambda s))) 3)
    (define cp (cox-path Xc tc sc #:nlambda 10))
    (check-equal? (vector-length (coef cp #:lambda 0.05)) 2)
    (check-list= (predict cp Xc #:lambda 0.05 #:type 'response)
                 (map exp (predict cp Xc #:lambda 0.05))))

  ;; --- printing and equality ------------------------------------------------------------

  (test-case "a single fit prints its family, lambda, deviance ratio and nonzero count"
    (check-equal? (format "~a" gaussian) "#<glmnet:gaussian λ=0.05 dev=0.9941 nz=2/3>")
    (check-regexp-match #rx"^#<glmnet:binomial λ=0.04 dev=[0-9.]+ nz=2/3>$" (format "~s" binomial))
    (check-regexp-match #rx"^#<glmnet:multinomial λ=0.01 .* nz=2/2>$" (format "~v" multinomial))
    (check-regexp-match #rx"^#<glmnet:cox λ=0.1 " (format "~a" cox))
    (check-equal? (format "~a" (ols X y)) "#<glmnet:gaussian λ=0 dev=1.0000 nz=3/3>"))

  (test-case "a path prints R's Df, %Dev and Lambda table"
    (define p (multinomial-path Xm ym #:nlambda 5))
    (define lines (regexp-split #rx"\n" (format "~a" p)))
    (check-equal? (first lines) "#<glmnet-path:multinomial")
    (check-regexp-match #rx"^ +Df +%Dev +Lambda$" (second lines))
    (check-equal? (length lines) (+ 2 (vector-length (glmnet-path-lambda p))))
    (check-regexp-match #rx"^ +0 +0[.]00 +0[.]4557$" (third lines))
    (check-regexp-match #rx"e-05>$" (last lines)))

  (test-case "results are still transparent: equal? and match see their fields"
    (check-equal? (lasso X y #:lambda 0.05) gaussian)
    (check-not-equal? (lasso X y #:lambda 0.5) gaussian)
    (check-equal? (elnet-path X y #:nlambda 20) path)
    (match-define (elnet-result a0 _ _ lam _) gaussian)
    (check-equal? lam 0.05)
    (check-equal? a0 (elnet-result-intercept gaussian)))

  ;; --- implementing the interface -----------------------------------------------------

  ;; A path with a chosen lambda, as a cross-validation result would be.
  (struct chosen (path index)
    #:methods gen:glmnet-model
    [(define (glmnet-model->path m) (chosen-path m))
     (define (glmnet-model-default-lambda m)
       (vector-ref (glmnet-path-lambda (chosen-path m)) (chosen-index m)))
     (define (deviance-ratio m)
       (vector-ref (glmnet-path-dev-ratio (chosen-path m)) (chosen-index m)))])

  (test-case "a new type that implements gen:glmnet-model gets predict and coef"
    (define m (chosen path 6))
    (check-true (glmnet-model? m))
    (check-equal? (coef m) (stored 6))
    (check-equal? (predict m new-rows) (predict path new-rows #:lambda (vector-ref lams 6)))
    (check-equal? (coef m #:lambda (vector-ref lams 2)) (stored 2))
    (check-equal? (deviance-ratio m) (vector-ref (glmnet-path-dev-ratio path) 6)))

  (test-case "a type that implements only glmnet-model->path is a single fit"
    (struct wrapped (fit)
      #:methods gen:glmnet-model
      [(define/generic ->path glmnet-model->path)
       (define (glmnet-model->path m) (->path (wrapped-fit m)))])
    (define w (wrapped poisson))
    (check-equal? (glmnet-model-default-lambda w) 0.2)
    (check-equal? (deviance-ratio w) (poisson-result-dev-ratio poisson))
    (check-equal? (predict w Xp #:type 'response) (poisson-predict-mean poisson Xp))))
