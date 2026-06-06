#lang racket/base

;; Runner + tests for the literate example ../08-mgaussian.rkt.

(require glmnet
         "../08-mgaussian.rkt")

(module+ main
  (define r (run-example))
  (printf "mgaussian intercepts   = ~a\n" (mgaussian-result-intercepts r))
  (printf "mgaussian coefficients = ~a\n" (mgaussian-result-coefficients r))
  (printf "r-squared              = ~a\n" (mgaussian-result-r-squared r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (define b (mgaussian-result-coefficients r))
  ;; Two responses, two predictors each.
  (check-equal? (vector-length (mgaussian-result-intercepts r)) 2)
  (check-equal? (vector-length b) 2)
  (for ([br (in-vector b)]) (check-equal? (vector-length br) 2))
  ;; x1 drives both responses with opposite signs.
  (check-true (> (vector-ref (vector-ref b 0) 0) 0.0) "x1 raises response 1")
  (check-true (< (vector-ref (vector-ref b 1) 0) 0.0) "x1 lowers response 2")
  ;; the grouped lasso zeros the x2 row across both responses.
  (check-equal? (vector-ref (vector-ref b 0) 1) 0.0 "x2 dropped from response 1")
  (check-equal? (vector-ref (vector-ref b 1) 1) 0.0 "x2 dropped from response 2")
  ;; r-squared in (0, 1].
  (check-true (< 0.0 (mgaussian-result-r-squared r)))
  (check-true (<= (mgaussian-result-r-squared r) 1.0)))
