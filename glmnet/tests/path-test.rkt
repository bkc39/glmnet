#lang racket/base

;; Unit tests for the regularization-path fitters (#10). R parity for every
;; family's default path lives in parity-test.rkt.

(module+ test
  (require rackunit
           racket/list
           racket/match
           glmnet)

  (define X '((1.0 2.0  1.0)
              (2.0 1.0  4.0)
              (3.0 4.0  9.0)
              (4.0 3.0 16.0)
              (5.0 6.0 25.0)
              (6.0 5.0 36.0)))
  (define y '(1.0 4.0 3.0 6.0 5.0 8.0))

  (define (decreasing? v)
    (for/and ([a (in-vector v)] [b (in-vector v 1)]) (> a b)))

  (define (check-vector= got want tol)
    (for ([g (in-vector got)] [w (in-vector want)])
      (check-= g w tol)))

  ;; A path warm-starts each lambda from the previous solution, a single fit
  ;; starts from zero; both converge to the same optimum within the solver's
  ;; tolerance, the bound parity-test.rkt also uses against R.
  (define warm-start-tol 1e-4)

  ;; --- the automatic sequence ----------------------------------------------------

  (test-case "an automatic path decreases from lambda_max, where every coefficient is zero"
    (define p (elnet-path X y))
    (define lams (glmnet-path-lambda p))
    (check-eq? (glmnet-path-family p) 'gaussian)
    (check-true (< 2 (vector-length lams) 101))
    (check-true (decreasing? lams))
    (check-true (for/and ([b (in-vector (vector-ref (glmnet-path-coefficients p) 0))]) (zero? b)))
    (check-equal? (vector-ref (glmnet-path-df p) 0) 0))

  (test-case "the first automatic lambda is R's fix.lam extrapolation"
    (define lams (glmnet-path-lambda (elnet-path X y)))
    (check-= (vector-ref lams 0)
             (exp (- (* 2 (log (vector-ref lams 1))) (log (vector-ref lams 2))))
             1e-12))

  (test-case "every per-lambda field has one entry per fitted lambda"
    (define p (elnet-path X y))
    (define n (vector-length (glmnet-path-lambda p)))
    (for ([field (list glmnet-path-intercepts glmnet-path-coefficients
                       glmnet-path-dev-ratio glmnet-path-df)])
      (check-equal? (vector-length (field p)) n)))

  (test-case "#:nlambda bounds the number of lambdas"
    (check-true (<= (vector-length (glmnet-path-lambda (elnet-path X y #:nlambda 10))) 10)))

  ;; --- a user sequence -----------------------------------------------------------

  (test-case "a user sequence is fitted largest first, as given"
    (define p (elnet-path X y #:lambda '(0.05 1.0 0.2)))
    (check-equal? (glmnet-path-lambda p) #(1.0 0.2 0.05)))

  (test-case "each point of a user path agrees with the single fit at that lambda"
    (define lams '(0.5 0.1 0.01))
    (define p (elnet-path X y #:lambda lams #:alpha 0.5 #:thresh 1e-12))
    (for ([lam (in-list lams)] [m (in-naturals)])
      (define r (elastic-net X y #:alpha 0.5 #:lambda lam #:thresh 1e-12))
      (check-= (vector-ref (glmnet-path-intercepts p) m) (elnet-result-intercept r) warm-start-tol)
      (check-vector= (vector-ref (glmnet-path-coefficients p) m) (elnet-result-coefficients r) warm-start-tol)
      (check-= (vector-ref (glmnet-path-dev-ratio p) m) (elnet-result-r-squared r) warm-start-tol)))

  ;; --- the other families --------------------------------------------------------

  (define Xl '((1.0 5.0 2.0) (2.0 6.0 1.0) (2.0 5.0 3.0) (1.0 4.0 1.0) (3.0 6.0 2.0) (2.0 4.0 2.0)
               (6.0 2.0 2.0) (5.0 1.0 1.0) (6.0 1.0 3.0) (5.0 2.0 1.0) (4.0 1.0 2.0) (6.0 3.0 2.0)))
  (define yl '(0 0 0 0 0 0 1 1 1 1 1 1))

  (test-case "binomial: a path point agrees with the single fit"
    (define p (logistic-path Xl yl #:lambda '(0.2 0.04) #:thresh 1e-12))
    (define r (logistic-fit Xl yl #:lambda 0.04 #:thresh 1e-12))
    (check-eq? (glmnet-path-family p) 'binomial)
    (check-vector= (vector-ref (glmnet-path-coefficients p) 1) (logistic-result-coefficients r) warm-start-tol))

  (test-case "multinomial: K coefficient vectors and K intercepts per lambda"
    (define Xm '((1.0 1.0) (2.0 1.0) (1.0 2.0) (2.0 2.0) (5.0 1.0) (6.0 1.0)
                 (5.0 2.0) (6.0 2.0) (3.0 5.0) (4.0 5.0) (3.0 6.0) (4.0 6.0)))
    (define ym '(0 0 0 0 1 1 1 1 2 2 2 2))
    (define p (multinomial-path Xm ym #:lambda '(0.1 0.01) #:thresh 1e-12))
    (define r (multinomial-fit Xm ym #:lambda 0.01 #:thresh 1e-12))
    (check-equal? (vector-length (vector-ref (glmnet-path-intercepts p) 0)) 3)
    (for ([got (in-vector (vector-ref (glmnet-path-coefficients p) 1))]
          [want (in-vector (multinomial-result-coefficients r))])
      (check-vector= got want warm-start-tol)))

  (test-case "cox: no intercepts, and a path point agrees with the single fit"
    (define Xc '((0.5 1.0) (1.0 2.0) (1.5 1.0) (2.0 2.0) (2.5 1.0) (3.0 2.0) (3.5 1.0) (4.0 2.0)))
    (define tc '(12.0 10.0 11.0 8.0 6.0 7.0 4.0 3.0))
    (define sc '(0 0 0 1 1 1 1 1))
    (define p (cox-path Xc tc sc #:lambda '(0.5 0.1) #:thresh 1e-12))
    (define r (cox-fit Xc tc sc #:lambda 0.1 #:thresh 1e-12))
    (check-false (glmnet-path-intercepts p))
    (check-vector= (vector-ref (glmnet-path-coefficients p) 1) (cox-result-coefficients r) warm-start-tol))

  (test-case "poisson: a path point agrees with the single fit"
    (define Xp '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0) (5.0 2.0) (6.0 1.0) (7.0 2.0) (8.0 1.0)))
    (define yp '(1 2 2 3 4 6 8 11))
    (define p (poisson-path Xp yp #:lambda '(0.5 0.2) #:thresh 1e-12))
    (define r (poisson-fit Xp yp #:lambda 0.2 #:thresh 1e-12))
    (check-= (vector-ref (glmnet-path-intercepts p) 1) (poisson-result-intercept r) warm-start-tol)
    (check-vector= (vector-ref (glmnet-path-coefficients p) 1) (poisson-result-coefficients r) warm-start-tol))

  (test-case "mgaussian: one coefficient vector per response, grouped df"
    (define Xg '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0) (5.0 2.0) (6.0 1.0)))
    (define Yg '((3.0 9.0) (5.0 8.0) (7.0 7.0) (9.0 6.0) (11.0 5.0) (13.0 4.0)))
    (define p (mgaussian-path Xg Yg #:lambda '(1.0 0.1) #:thresh 1e-12))
    (define r (mgaussian-fit Xg Yg #:lambda 0.1 #:thresh 1e-12))
    (check-equal? (vector-ref (glmnet-path-df p) 1) 1)
    (for ([got (in-vector (vector-ref (glmnet-path-coefficients p) 1))]
          [want (in-vector (mgaussian-result-coefficients r))])
      (check-vector= got want warm-start-tol)))

  (test-case "multinomial: intercepts are centred at every lambda, as R's coef() reports them"
    (define Xm '((1.0 1.0) (2.0 1.0) (1.0 2.0) (2.0 2.0) (5.0 1.0) (6.0 1.0)
                 (5.0 2.0) (6.0 2.0) (3.0 5.0) (4.0 5.0) (3.0 6.0) (4.0 6.0)))
    (define ym '(0 0 0 0 1 1 1 1 2 2 2 2))
    (define p (multinomial-path Xm ym #:lambda '(0.1 0.01) #:thresh 1e-12))
    (for ([a0 (in-vector (glmnet-path-intercepts p))])
      (check-= (for/sum ([a (in-vector a0)]) a) 0.0 1e-12))
    (define r (multinomial-fit Xm ym #:lambda 0.01 #:thresh 1e-12))
    (check-vector= (vector-ref (glmnet-path-intercepts p) 1) (multinomial-result-intercepts r)
                   warm-start-tol))

  ;; --- the lambda-min-ratio ------------------------------------------------------

  ;; Five lambdas are too few for glmnet to stop early, so the automatic
  ;; sequence runs geometrically from lambda_max to ratio * lambda_max, and the
  ;; last lambda is ratio^(3/4) times the second.
  (define (last-over-second p)
    (define lams (glmnet-path-lambda p))
    (/ (vector-ref lams 4) (vector-ref lams 1)))

  (test-case "an explicit #:lambda-min-ratio sets the end of the automatic sequence"
    (check-= (last-over-second (elnet-path X y #:nlambda 5 #:lambda-min-ratio 0.1))
             (expt 0.1 3/4) 1e-9))

  (test-case "the default ratio is 1e-4, or 0.01 with fewer observations than predictors"
    (check-= (last-over-second (elnet-path X y #:nlambda 5)) (expt 1e-4 3/4) 1e-9)
    (define Xw '((1.0 2.0 1.0 0.5 3.0) (2.0 1.0 4.0 1.5 2.0) (3.0 4.0 9.0 0.2 1.0)))
    (check-= (last-over-second (elnet-path Xw '(1.0 4.0 3.0) #:nlambda 5)) (expt 0.01 3/4) 1e-9))

  (test-case "a ratio of 0 is glmnet's floor of 1e-6, as in R"
    (check-equal? (glmnet-path-lambda (elnet-path X y #:nlambda 5 #:lambda-min-ratio 0))
                  (glmnet-path-lambda (elnet-path X y #:nlambda 5 #:lambda-min-ratio 1e-6))))

  ;; --- when glmnet stops -------------------------------------------------------

  (define Yg '((1.0 2.0) (3.0 1.0) (2.0 5.0) (6.0 4.0) (5.0 5.0) (8.0 9.0)))
  (define Xc '((0.5 1.0) (1.0 2.0) (1.5 1.0) (2.0 2.0) (2.5 1.0) (3.0 2.0) (3.5 1.0) (4.0 2.0)))
  (define tc '(12.0 10.0 11.0 8.0 6.0 7.0 4.0 3.0))
  (define sc '(0 0 0 1 1 1 1 1))
  (define yp '(1 2 2 3 4 6 8 11 1 2 3 4))
  (define ym3 '(0 0 1 1 2 2 0 1 2 0 1 2))

  ;; With #:max-iters 1 every family gives up at the first lambda, so glmnet fits
  ;; none; each fitter raises, naming itself. Without the shim's lmu = 0, the
  ;; path fitters read past their buffers ("invalid memory reference").
  (define no-lambda-cases
    (list (list 'elnet-path (lambda () (elnet-path X y #:lambda '(0.001) #:max-iters 1)))
          (list 'logistic-path (lambda () (logistic-path Xl yl #:lambda '(0.001) #:max-iters 1)))
          (list 'multinomial-path (lambda () (multinomial-path Xl ym3 #:lambda '(0.001) #:max-iters 1)))
          (list 'cox-path (lambda () (cox-path Xc tc sc #:lambda '(0.001) #:max-iters 1)))
          (list 'poisson-path (lambda () (poisson-path Xl yp #:lambda '(0.001) #:max-iters 1)))
          (list 'mgaussian-path (lambda () (mgaussian-path X Yg #:lambda '(0.001) #:max-iters 1)))
          (list 'elnet-fit (lambda () (elnet-fit X y #:lambda 0.001 #:max-iters 1)))
          (list 'lasso (lambda () (lasso X y #:lambda 0.001 #:max-iters 1)))
          (list 'logistic-fit (lambda () (logistic-fit Xl yl #:lambda 0.001 #:max-iters 1)))
          (list 'multinomial-fit (lambda () (multinomial-fit Xl ym3 #:lambda 0.001 #:max-iters 1)))
          (list 'cox-fit (lambda () (cox-fit Xc tc sc #:lambda 0.001 #:max-iters 1)))
          (list 'poisson-fit (lambda () (poisson-fit Xl yp #:lambda 0.001 #:max-iters 1)))
          (list 'mgaussian-fit (lambda () (mgaussian-fit X Yg #:lambda 0.001 #:max-iters 1)))))

  (for ([c (in-list no-lambda-cases)])
    (define who (car c))
    (test-case (format "~a raises when glmnet fits no lambda" who)
      (check-exn (regexp (format "^~a: glmnet fitted no lambda: convergence was not reached at the first lambda.*#:max-iters" who))
                 (cadr c))))

  (define (glmnet-warnings thunk)
    (define receiver (make-log-receiver (current-logger) 'warning 'glmnet))
    (define result (thunk))
    (values result
            (let loop ()
              (match (sync/timeout 0 receiver)
                [#f '()]
                [(vector _ message _ _) (cons message (loop))]))))

  (test-case "a path that fails partway keeps the lambdas before it and logs a warning"
    (define-values (p warnings)
      (glmnet-warnings (lambda () (elnet-path X y #:lambda '(0.5 0.1 0.001) #:max-iters 1))))
    (check-equal? (glmnet-path-lambda p) #(0.5))
    (check-equal? (vector-length (glmnet-path-coefficients p)) 1)
    (check-match warnings
                 (list (regexp #rx"elnet-path: the path stops after 1 lambda: convergence was not reached at lambda number 2"))))

  (test-case "a fatal glmnet error raises from a path"
    (check-exn #rx"^elnet-path: all used predictors have zero variance"
               (lambda () (elnet-path '((1.0 2.0) (1.0 2.0) (1.0 2.0)) '(1.0 2.0 4.0)))))

  (test-case "a constant Gaussian response is rejected, as R rejects it"
    (define msg #rx"y is constant; gaussian glmnet fails at standardization step")
    (check-exn msg (lambda () (elnet-path X (make-list 6 2.0))))
    (check-exn msg (lambda () (elnet-path X (make-list 6 0.0) #:intercept? #f)))
    (check-exn msg (lambda () (mgaussian-path X (make-list 6 '(2.0 3.0)))))
    (check-exn #rx"^elnet-path: " (lambda () (elnet-path X (make-list 6 2.0))))
    (check-exn #rx"^mgaussian-path: " (lambda () (mgaussian-path X (make-list 6 '(2.0 3.0))))))

  (test-case "a constant response without an intercept, or one constant column of Y, still fits"
    (check-true (< 2 (vector-length (glmnet-path-lambda (elnet-path X (make-list 6 2.0) #:intercept? #f)))))
    (define p (mgaussian-path X (for/list ([v (in-list y)]) (list v 3.0)) #:lambda '(1.0 0.1)))
    (for ([a0 (in-vector (glmnet-path-intercepts p))]
          [coefs (in-vector (glmnet-path-coefficients p))])
      (check-= (vector-ref a0 1) 3.0 1e-12)
      (check-true (for/and ([b (in-vector (vector-ref coefs 1))]) (zero? b)))))

  (test-case "an all-zero Poisson response is rejected up front"
    (check-exn #rx"^poisson-path: the response has no positive count"
               (lambda () (poisson-path Xl (make-list 12 0))))
    (check-exn #rx"^poisson-path: the response has no positive count"
               (lambda () (poisson-path Xl (make-list 12 0) #:lambda '(1.0 0.1)))))

  ;; --- contracts -----------------------------------------------------------------

  (test-case "a negative lambda is a contract error"
    (check-exn exn:fail:contract? (lambda () (elnet-path X y #:lambda '(0.5 -0.1)))))

  (test-case "an empty lambda list is a contract error"
    (check-exn exn:fail:contract? (lambda () (elnet-path X y #:lambda '()))))

  (test-case "lambda-min-ratio must lie in [0, 1)"
    (check-exn exn:fail:contract? (lambda () (elnet-path X y #:lambda-min-ratio 1.0)))
    (check-exn exn:fail:contract? (lambda () (elnet-path X y #:lambda-min-ratio -0.1)))))
