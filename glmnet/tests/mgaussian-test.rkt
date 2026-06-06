#lang racket/base

;; Unit tests for the high-level multi-response Gaussian API (core/mgaussian.rkt).

(module+ test
  (require rackunit
           glmnet)

  ;; Two responses that both depend on x1 with opposite signs (y1 = 1 + 2*x1,
  ;; y2 = 10 - x1); x2 is an uninformative noise predictor. mean(y1) = 8,
  ;; mean(y2) = 6.5.
  (define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0) (5.0 2.0) (6.0 1.0)))
  (define Y '((3.0 9.0) (5.0 8.0) (7.0 7.0) (9.0 6.0) (11.0 5.0) (13.0 4.0)))

  ;; --- shape + coefficients --------------------------------------------------

  (test-case "fit returns nr intercepts and nr coefficient vectors"
    (define r (mgaussian-fit X Y #:lambda 0.1))
    (check-equal? (vector-length (mgaussian-result-intercepts r)) 2)
    (check-equal? (vector-length (mgaussian-result-coefficients r)) 2)
    (for ([br (in-vector (mgaussian-result-coefficients r))])
      (check-equal? (vector-length br) 2)))

  (test-case "each response loads on x1 with the expected sign"
    (define b (mgaussian-result-coefficients (mgaussian-fit X Y #:lambda 0.1)))
    (check-true (> (vector-ref (vector-ref b 0) 0) 0.0) "x1 raises response 1")
    (check-true (< (vector-ref (vector-ref b 1) 0) 0.0) "x1 lowers response 2"))

  (test-case "grouped lasso shares support: the noise row is zero for all responses"
    (define b (mgaussian-result-coefficients (mgaussian-fit X Y #:lambda 0.1)))
    (check-equal? (vector-ref (vector-ref b 0) 1) 0.0)
    (check-equal? (vector-ref (vector-ref b 1) 1) 0.0))

  (test-case "ridge keeps every coefficient nonzero"
    (define b (mgaussian-result-coefficients (mgaussian-fit X Y #:lambda 0.1 #:alpha 0.0)))
    (check-true (for*/and ([br (in-vector b)] [bi (in-vector br)]) (not (zero? bi)))))

  ;; --- r-squared -------------------------------------------------------------

  (test-case "r-squared is a fraction of variance in (0, 1]"
    (define r2 (mgaussian-result-r-squared (mgaussian-fit X Y #:lambda 0.1)))
    (check-true (< 0.0 r2))
    (check-true (<= r2 1.0)))

  (test-case "huge lambda collapses to the per-response means"
    (define r (mgaussian-fit X Y #:lambda 1e6))
    (check-true (for*/and ([br (in-vector (mgaussian-result-coefficients r))]
                           [bi (in-vector br)]) (zero? bi)))
    (check-= (vector-ref (mgaussian-result-intercepts r) 0) 8.0 1e-2)
    (check-= (vector-ref (mgaussian-result-intercepts r) 1) 6.5 1e-2))

  ;; --- prediction ------------------------------------------------------------

  (test-case "predict returns one nr-vector per row"
    (define preds (mgaussian-predict (mgaussian-fit X Y #:lambda 0.1) X))
    (check-equal? (length preds) (length X))
    (for ([p (in-list preds)]) (check-equal? (length p) 2)))

  (test-case "a zero feature row predicts the intercepts"
    (define r (mgaussian-fit X Y #:lambda 0.1))
    (check-equal? (car (mgaussian-predict r '((0.0 0.0))))
                  (list (vector-ref (mgaussian-result-intercepts r) 0)
                        (vector-ref (mgaussian-result-intercepts r) 1))))

  ;; --- contracts / input validation -----------------------------------------

  (test-case "a ragged response matrix is rejected"
    (check-exn exn:fail?
               (lambda () (mgaussian-fit '((1.0 2.0) (3.0 4.0)) '((1.0 2.0) (3.0)) #:lambda 0.1))))

  (test-case "the response matrix must have one row per observation"
    (check-exn exn:fail?
               (lambda () (mgaussian-fit X '((1.0 2.0)) #:lambda 0.1))))

  (test-case "alpha outside [0,1] is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (mgaussian-fit X Y #:lambda 0.1 #:alpha 2.0))))

  (test-case "negative lambda is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (mgaussian-fit X Y #:lambda -0.1))))

  (test-case "predict rejects rows with the wrong number of features"
    (define r (mgaussian-fit X Y #:lambda 0.1))
    (check-exn exn:fail? (lambda () (mgaussian-predict r '((1.0 2.0 3.0)))))))
