#lang racket/base

;; Unit tests for the high-level Poisson API (core/poisson.rkt).

(module+ test
  (require rackunit
           glmnet)

  ;; Count data with a clean log-linear trend: x1 drives the expected count
  ;; upward; x2 is an uninformative noise predictor. mean(y) = 37/8.
  (define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0)
              (5.0 2.0) (6.0 1.0) (7.0 2.0) (8.0 1.0)))
  (define y '(1 2 2 3 4 6 8 11))

  ;; --- coefficients ----------------------------------------------------------

  (test-case "the rate driver gets a positive log-mean coefficient"
    (check-true (> (vector-ref (poisson-result-coefficients
                                (poisson-fit X y #:lambda 0.05)) 0)
                   0.0)))

  (test-case "lasso zeros the noise predictor; ridge keeps it"
    (check-equal? (vector-ref (poisson-result-coefficients
                               (poisson-fit X y #:lambda 0.2)) 1)
                  0.0)
    (check-true (for/and ([bi (in-vector (poisson-result-coefficients
                                          (poisson-fit X y #:lambda 0.2 #:alpha 0.0)))])
                  (not (zero? bi)))))

  ;; --- dev-ratio -------------------------------------------------------------

  (test-case "dev-ratio is a fraction of null deviance in (0, 1]"
    (define d (poisson-result-dev-ratio (poisson-fit X y #:lambda 0.05)))
    (check-true (< 0.0 d))
    (check-true (<= d 1.0)))

  (test-case "huge lambda collapses to the log(mean) null model"
    (define r (poisson-fit X y #:lambda 1e6))
    (check-true (for/and ([bi (in-vector (poisson-result-coefficients r))]) (zero? bi))
                "all coefficients zero")
    (check-= (poisson-result-intercept r) (log (/ 37 8)) 1e-3 "intercept = log(mean(y))")
    (check-= (poisson-result-dev-ratio r) 0.0 1e-3))

  ;; --- prediction ------------------------------------------------------------

  (test-case "predict-mean is positive and increases with the rate driver"
    (define mu (poisson-predict-mean (poisson-fit X y #:lambda 0.2) X))
    (check-true (for/and ([m (in-list mu)]) (> m 0.0)))
    (check-true (for/and ([a (in-list mu)] [b (in-list (cdr mu))]) (<= a b))))

  (test-case "predict-mean applies the log link (exp of the linear predictor)"
    (define r (poisson-fit X y #:lambda 0.2))
    ;; a zero-feature row maps to exp(intercept)
    (check-= (car (poisson-predict-mean r '((0.0 0.0))))
             (exp (poisson-result-intercept r)) 1e-9))

  ;; --- contracts / input validation -----------------------------------------

  (test-case "a negative count is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (poisson-fit '((1.0) (2.0)) '(1 -1) #:lambda 0.05))))

  (test-case "alpha outside [0,1] is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (poisson-fit X y #:lambda 0.05 #:alpha 2.0))))

  (test-case "negative lambda is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (poisson-fit X y #:lambda -0.1))))

  (test-case "ragged predictor matrix is rejected"
    (check-exn exn:fail? (lambda () (poisson-fit '((1.0 2.0) (3.0)) '(1 2) #:lambda 0.05))))

  (test-case "predict rejects rows with the wrong number of features"
    (define r (poisson-fit X y #:lambda 0.2))
    (check-exn exn:fail? (lambda () (poisson-predict-mean r '((1.0 2.0 3.0)))))))
