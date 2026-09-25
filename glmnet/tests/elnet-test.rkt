#lang racket/base

;; Unit tests for the high-level Gaussian elastic-net API (core/elnet.rkt).

(module+ test
  (require rackunit
           glmnet)

  ;; Noise-free fixture: y = 1 + 2*x1 - x2, so OLS recovers it exactly.
  (define X '((1.0 2.0)
              (2.0 1.0)
              (3.0 4.0)
              (4.0 3.0)
              (5.0 6.0)))
  (define y '(1.0 4.0 3.0 6.0 5.0))

  (test-case "OLS recovers the exact linear fit at lambda 0"
    (define r (ols X y #:standardize? #f))
    (check-true (elnet-result? r))
    (check-= (elnet-result-intercept r) 1.0 1e-4)
    (define b (elnet-result-coefficients r))
    (check-= (vector-ref b 0) 2.0 1e-4)
    (check-= (vector-ref b 1) -1.0 1e-4)
    (check-= (elnet-result-r-squared r) 1.0 1e-6))

  (test-case "elnet-fit at lambda 0 equals ols"
    (define r (elnet-fit X y #:lambda 0.0 #:standardize? #f #:thresh 1e-10))
    (check-= (elnet-result-intercept r) 1.0 1e-4)
    (check-= (vector-ref (elnet-result-coefficients r) 0) 2.0 1e-4))

  (test-case "standardized OLS recovers the fit too (no penalty at lambda 0)"
    (define r (ols X y #:standardize? #t))
    (check-= (elnet-result-intercept r) 1.0 1e-4)
    (check-= (vector-ref (elnet-result-coefficients r) 0) 2.0 1e-4)
    (check-= (vector-ref (elnet-result-coefficients r) 1) -1.0 1e-4))

  ;; --- ridge (alpha = 0) ---
  ;; Fixture with an irrelevant predictor x3 = x1^2; mean(y) = 4.5.
  (define X3 '((1.0 2.0  1.0)
               (2.0 1.0  4.0)
               (3.0 4.0  9.0)
               (4.0 3.0 16.0)
               (5.0 6.0 25.0)
               (6.0 5.0 36.0)))
  (define y3 '(1.0 4.0 3.0 6.0 5.0 8.0))

  (test-case "ridge keeps every coefficient nonzero"
    (define b (elnet-result-coefficients (ridge X3 y3 #:lambda 0.1)))
    (check-true (for/and ([bi (in-vector b)]) (not (zero? bi)))))

  (test-case "ridge shrinkage grows with lambda"
    (define b-small (elnet-result-coefficients (ridge X3 y3 #:lambda 0.1)))
    (define b-large (elnet-result-coefficients (ridge X3 y3 #:lambda 2.0)))
    (check-true (< (abs (vector-ref b-large 0)) (abs (vector-ref b-small 0)))
                "leading coefficient shrinks further at larger lambda"))

  (test-case "ridge at huge lambda collapses to the mean"
    (define r (ridge X3 y3 #:lambda 1e6))
    (check-= (elnet-result-intercept r) 4.5 1e-2)
    (check-true (for/and ([bi (in-vector (elnet-result-coefficients r))])
                  (< (abs bi) 1e-2))))

  ;; --- lasso (alpha = 1) ---
  (define (num-zeros v) (for/sum ([x (in-vector v)] #:when (zero? x)) 1))

  (test-case "lasso selects the irrelevant predictor out"
    (define b (elnet-result-coefficients (lasso X3 y3 #:lambda 0.05)))
    (check-equal? (vector-ref b 2) 0.0)
    (check-true (not (zero? (vector-ref b 0))))
    (check-true (not (zero? (vector-ref b 1)))))

  (test-case "lasso sparsity grows with lambda"
    (define b-small (elnet-result-coefficients (lasso X3 y3 #:lambda 0.05)))
    (define b-large (elnet-result-coefficients (lasso X3 y3 #:lambda 0.5)))
    (check-true (> (num-zeros b-large) (num-zeros b-small))))

  (test-case "lasso at huge lambda is intercept-only at the mean"
    (define r (lasso X3 y3 #:lambda 1e6))
    (check-= (elnet-result-intercept r) 4.5 1e-6)
    (check-equal? (num-zeros (elnet-result-coefficients r)) 3))

  ;; --- elastic net (0 < alpha < 1) ---
  (test-case "elastic net at alpha 0 equals ridge"
    (check-equal?
     (vector->list (elnet-result-coefficients (elastic-net X3 y3 #:alpha 0.0 #:lambda 0.3)))
     (vector->list (elnet-result-coefficients (ridge X3 y3 #:lambda 0.3)))))

  (test-case "elastic net at alpha 1 equals lasso"
    (check-equal?
     (vector->list (elnet-result-coefficients (elastic-net X3 y3 #:alpha 1.0 #:lambda 0.3)))
     (vector->list (elnet-result-coefficients (lasso X3 y3 #:lambda 0.3)))))

  (test-case "elastic-net sparsity sits between ridge and lasso"
    (define (nz r) (for/sum ([x (in-vector (elnet-result-coefficients r))] #:when (zero? x)) 1))
    (define nz-ridge (nz (ridge X3 y3 #:lambda 0.5)))
    (define nz-enet  (nz (elastic-net X3 y3 #:alpha 0.5 #:lambda 0.5)))
    (define nz-lasso (nz (lasso X3 y3 #:lambda 0.5)))
    (check-equal? nz-ridge 0)
    (check-true (>= nz-lasso 1))
    (check-true (<= nz-ridge nz-enet))
    (check-true (<= nz-enet nz-lasso)))

  ;; --- no intercept (#33) ---
  ;; Without an intercept y is not centered, so r-squared is relative to
  ;; sum(y^2). Reference values: R glmnet 4.1.8, glmnet(X, y, intercept = FALSE).

  (define (uncentered-r-squared r)
    (define rss
      (for/sum ([row (in-list X)] [yi (in-list y)])
        (define fitted
          (for/sum ([b (in-vector (elnet-result-coefficients r))] [x (in-list row)])
            (* b x)))
        (expt (- yi fitted) 2)))
    (- 1.0 (/ rss (for/sum ([yi (in-list y)]) (* yi yi)))))

  (test-case "no-intercept r-squared is 1 - RSS/sum(y^2)"
    (for ([r (list (ols X y #:intercept? #f)
                   (lasso X y #:lambda 0.1 #:intercept? #f)
                   (lasso X y #:lambda 0.1 #:intercept? #f #:standardize? #f))])
      (check-equal? (elnet-result-intercept r) 0.0)
      (check-= (elnet-result-r-squared r) (uncentered-r-squared r) 1e-6)))

  (test-case "no-intercept fits match R glmnet"
    (define (check-r r beta dev-ratio)
      (check-= (elnet-result-r-squared r) dev-ratio 1e-4)
      (for ([b (in-vector (elnet-result-coefficients r))] [rb (in-list beta)])
        (check-= b rb 1e-4)))
    (check-r (ols X y #:intercept? #f) '(2.2329448 -0.9622848) 0.9896292)
    (check-r (lasso X y #:lambda 0.1 #:intercept? #f) '(1.8656870 -0.6265096) 0.9832474)
    (check-r (lasso X y #:lambda 0.1 #:intercept? #f #:standardize? #f)
             '(1.9955260 -0.7460684) 0.9869714))

  ;; --- input validation / contracts ---

  (test-case "ragged predictor matrix is rejected"
    (check-exn exn:fail? (lambda () (ols '((1.0 2.0) (3.0)) '(1.0 2.0)))))

  (test-case "response length must match observation count"
    (check-exn exn:fail? (lambda () (ols '((1.0 2.0) (3.0 4.0)) '(1.0)))))

  (test-case "negative lambda is a contract error"
    (check-exn exn:fail:contract? (lambda () (elnet-fit X y #:lambda -1.0))))

  (test-case "alpha outside [0,1] is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (elnet-fit X y #:lambda 0.1 #:alpha 2.0)))))
