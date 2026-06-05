#lang racket/base

;; Unit tests for the high-level two-class logistic (binomial) API
;; (core/lognet.rkt).

(module+ test
  (require rackunit
           glmnet)

  ;; Separable two-class fixture: class 1 has high x1 / low x2, class 0 the
  ;; reverse; x3 is noise -- identically distributed within each class, so it
  ;; carries no signal about y.
  (define X '((1.0 5.0 2.0) (2.0 6.0 1.0) (2.0 5.0 3.0) (1.0 4.0 1.0)
              (3.0 6.0 2.0) (2.0 4.0 2.0)
              (6.0 2.0 2.0) (5.0 1.0 1.0) (6.0 1.0 3.0) (5.0 2.0 1.0)
              (4.0 1.0 2.0) (6.0 3.0 2.0)))
  (define y '(0 0 0 0 0 0 1 1 1 1 1 1))

  ;; --- coefficients ----------------------------------------------------------

  (test-case "lasso logistic recovers the coefficient signs"
    (define b (logistic-result-coefficients (logistic-fit X y #:lambda 0.04)))
    (check-true (> (vector-ref b 0) 0.0) "x1 raises the log-odds of class 1")
    (check-true (< (vector-ref b 1) 0.0) "x2 lowers the log-odds of class 1"))

  (test-case "lasso logistic selects the noise predictor out exactly"
    (define b (logistic-result-coefficients (logistic-fit X y #:lambda 0.04)))
    (check-equal? (vector-ref b 2) 0.0))

  (test-case "ridge logistic keeps every coefficient nonzero"
    (define b (logistic-result-coefficients (logistic-fit X y #:lambda 0.1 #:alpha 0.0)))
    (check-true (for/and ([bi (in-vector b)]) (not (zero? bi)))))

  (test-case "lasso logistic sparsity grows with lambda"
    (define (nz l)
      (for/sum ([bi (in-vector (logistic-result-coefficients
                                (logistic-fit X y #:lambda l)))]
                #:when (zero? bi)) 1))
    (check-true (>= (nz 0.2) (nz 0.04))))

  ;; --- dev-ratio -------------------------------------------------------------

  (test-case "dev-ratio is a fraction of null deviance in (0, 1]"
    (define d (logistic-result-dev-ratio (logistic-fit X y #:lambda 0.04)))
    (check-true (< 0.0 d))
    (check-true (<= d 1.0)))

  (test-case "huge lambda collapses to the balanced null model"
    (define r (logistic-fit X y #:lambda 1e6))
    (check-true (for/and ([bi (in-vector (logistic-result-coefficients r))]) (zero? bi))
                "all coefficients zero")
    (check-= (logistic-result-intercept r) 0.0 1e-2 "balanced 6/6 => logit(0.5) = 0")
    (check-= (logistic-result-dev-ratio r) 0.0 1e-3))

  ;; --- prediction ------------------------------------------------------------

  (test-case "predict classifies every separable training point correctly"
    (define r (logistic-fit X y #:lambda 0.04))
    (check-equal? (logistic-predict r X) y))

  (test-case "predict-proba returns [0,1] probabilities consistent with predict"
    (define r (logistic-fit X y #:lambda 0.04))
    (define ps (logistic-predict-proba r X))
    (check-true (for/and ([p (in-list ps)]) (and (>= p 0.0) (<= p 1.0)))
                "every probability in [0,1]")
    ;; class-1 rows sit at/above 0.5, class-0 rows below
    (for ([p (in-list ps)] [t (in-list y)])
      (if (= t 1) (check-true (>= p 0.5)) (check-true (< p 0.5))))
    ;; predict is exactly predict-proba thresholded at 0.5
    (check-equal? (logistic-predict r X)
                  (for/list ([p (in-list ps)]) (if (>= p 0.5) 1 0))))

  (test-case "raising the threshold predicts no more class-1 labels"
    (define r (logistic-fit X y #:lambda 0.04))
    (define (count1 t) (for/sum ([p (in-list (logistic-predict r X #:threshold t))]) p))
    (check-equal? (count1 0.0) (length y) "threshold 0 => every row is class 1")
    (check-true (>= (count1 0.5) (count1 0.95)) "monotone non-increasing in threshold"))

  ;; --- contracts / input validation -----------------------------------------

  (test-case "non-binary response is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (logistic-fit X '(0 1 2 0 1 0 1 0 1 0 1 0) #:lambda 0.04))))

  (test-case "alpha outside [0,1] is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (logistic-fit X y #:lambda 0.04 #:alpha 2.0))))

  (test-case "negative lambda is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (logistic-fit X y #:lambda -0.1))))

  (test-case "ragged predictor matrix is rejected"
    (check-exn exn:fail? (lambda () (logistic-fit '((1.0 2.0) (3.0)) '(0 1) #:lambda 0.04))))

  (test-case "response length must match observation count"
    (check-exn exn:fail? (lambda () (logistic-fit '((1.0 2.0) (3.0 4.0)) '(0) #:lambda 0.04))))

  (test-case "predict rejects rows with the wrong number of features"
    (define r (logistic-fit X y #:lambda 0.04))
    (check-exn exn:fail? (lambda () (logistic-predict-proba r '((1.0 2.0)))))))
