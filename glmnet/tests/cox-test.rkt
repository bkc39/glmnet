#lang racket/base

;; Unit tests for the high-level Cox proportional-hazards API (core/cox.rkt).

(module+ test
  (require rackunit
           glmnet
           racket/list)

  ;; Survival fixture with a clean risk gradient: x1 = 1..12 is a risk factor
  ;; (higher x1 => earlier event => higher hazard); times decrease with x1; x2 is
  ;; noise; every subject has an observed event.
  (define X (for/list ([i (in-range 1 13)])
              (list (exact->inexact i) (if (even? i) 1.0 2.0))))
  (define times (for/list ([i (in-range 1 13)]) (- 13.0 i)))
  (define statuses (for/list ([_ (in-range 12)]) 1))

  ;; --- coefficients ----------------------------------------------------------

  (test-case "the risk factor gets a positive log-hazard coefficient"
    (define b (cox-result-coefficients (cox-fit X times statuses #:lambda 0.05)))
    (check-true (> (vector-ref b 0) 0.0)))

  (test-case "lasso zeros the noise predictor; ridge keeps it"
    (check-equal? (vector-ref (cox-result-coefficients
                               (cox-fit X times statuses #:lambda 0.05)) 1)
                  0.0)
    (check-true (for/and ([bi (in-vector (cox-result-coefficients
                                          (cox-fit X times statuses #:lambda 0.1 #:alpha 0.0)))])
                  (not (zero? bi)))))

  (test-case "dev-ratio is a fraction of null deviance in (0, 1]"
    (define d (cox-result-dev-ratio (cox-fit X times statuses #:lambda 0.05)))
    (check-true (< 0.0 d))
    (check-true (<= d 1.0)))

  (test-case "huge lambda collapses to the null model"
    (define r (cox-fit X times statuses #:lambda 1e6))
    (check-true (for/and ([bi (in-vector (cox-result-coefficients r))]) (zero? bi)))
    (check-= (cox-result-dev-ratio r) 0.0 1e-3))

  ;; --- prediction ------------------------------------------------------------

  (test-case "relative risk increases with the risk factor"
    (define r (cox-fit X times statuses #:lambda 0.05))
    (define rr (cox-relative-risk r X))
    (check-true (for/and ([a (in-list rr)] [b (in-list (cdr rr))]) (<= a b))
                "monotone non-decreasing in x1"))

  (test-case "relative risk is exp of the linear predictor"
    (define r (cox-fit X times statuses #:lambda 0.05))
    (define lp (cox-linear-predictor r X))
    (define rr (cox-relative-risk r X))
    (for ([l (in-list lp)] [w (in-list rr)])
      (check-= w (exp l) 0.0)))

  (test-case "linear predictor has no intercept (a zero row maps to 0)"
    (define r (cox-fit X times statuses #:lambda 0.05))
    (check-= (car (cox-linear-predictor r '((0.0 0.0)))) 0.0 1e-12))

  ;; --- contracts / input validation -----------------------------------------

  (test-case "a status other than 0/1 is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (cox-fit '((1.0 2.0) (3.0 4.0)) '(5.0 6.0) '(1 2) #:lambda 0.05))))

  (test-case "a non-positive follow-up time is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (cox-fit '((1.0 2.0) (3.0 4.0)) '(0.0 6.0) '(1 1) #:lambda 0.05))))

  (test-case "all-censored data is rejected (need at least one event)"
    (check-exn exn:fail?
               (lambda () (cox-fit '((1.0 2.0) (3.0 4.0)) '(5.0 6.0) '(0 0) #:lambda 0.05))))

  (test-case "times/statuses length must match the observation count"
    (check-exn exn:fail?
               (lambda () (cox-fit '((1.0 2.0) (3.0 4.0)) '(5.0) '(1 1) #:lambda 0.05)))
    (check-exn exn:fail?
               (lambda () (cox-fit '((1.0 2.0) (3.0 4.0)) '(5.0 6.0) '(1) #:lambda 0.05))))

  (test-case "alpha outside [0,1] is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (cox-fit X times statuses #:lambda 0.05 #:alpha 2.0))))

  (test-case "negative lambda is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (cox-fit X times statuses #:lambda -0.1))))

  (test-case "prediction rejects rows with the wrong number of features"
    (define r (cox-fit X times statuses #:lambda 0.05))
    (check-exn exn:fail? (lambda () (cox-relative-risk r '((1.0 2.0 3.0)))))))
