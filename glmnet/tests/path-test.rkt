#lang racket/base

;; Unit tests for the regularization-path fitters (#10). R parity for every
;; family's default path lives in parity-test.rkt.

(module+ test
  (require rackunit
           racket/list
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

  ;; --- contracts -----------------------------------------------------------------

  (test-case "a negative lambda is a contract error"
    (check-exn exn:fail:contract? (lambda () (elnet-path X y #:lambda '(0.5 -0.1)))))

  (test-case "an empty lambda list is a contract error"
    (check-exn exn:fail:contract? (lambda () (elnet-path X y #:lambda '()))))

  (test-case "lambda-min-ratio must lie strictly between 0 and 1"
    (check-exn exn:fail:contract? (lambda () (elnet-path X y #:lambda-min-ratio 1.0)))
    (check-exn exn:fail:contract? (lambda () (elnet-path X y #:lambda-min-ratio 0)))))
