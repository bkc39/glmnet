#lang racket/base

;; Runner + tests for the literate example ../03-elastic-net.rkt.

(require glmnet
         "../03-elastic-net.rkt")

(module+ main
  (define r (run-example))
  (printf "elastic-net intercept    = ~a\n" (elnet-result-intercept r))
  (printf "elastic-net coefficients = ~a\n" (elnet-result-coefficients r))
  (printf "R^2                      = ~a\n" (elnet-result-r-squared r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (define b (elnet-result-coefficients r))
  ;; The blend both selects (some coefficient is zeroed) and shrinks (it is a
  ;; penalized fit, so R^2 < 1).
  (define zeros (for/sum ([x (in-vector b)] #:when (zero? x)) 1))
  (check-true (>= zeros 1) "elastic net selects at least one coefficient out")
  (check-true (< (elnet-result-r-squared r) 1.0))
  (check-true (> (elnet-result-r-squared r) 0.0)))
