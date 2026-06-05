#lang racket/base

;; Runner + tests for the literate example ../05-multinomial.rkt.

(require glmnet
         "../05-multinomial.rkt")

(module+ main
  (define r (run-example))
  (printf "multinomial intercepts   = ~a\n" (multinomial-result-intercepts r))
  (printf "multinomial coefficients = ~a\n" (multinomial-result-coefficients r))
  (printf "dev.ratio                = ~a\n" (multinomial-result-dev-ratio r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (define b (multinomial-result-coefficients r))
  ;; Three classes, two features each.
  (check-equal? (vector-length (multinomial-result-intercepts r)) 3)
  (check-equal? (vector-length b) 3)
  (for ([bk (in-vector b)]) (check-equal? (vector-length bk) 2))
  ;; Each class loads on its discriminating feature with the expected sign.
  (check-true (< (vector-ref (vector-ref b 0) 0) 0.0) "class 0: low x1")
  (check-true (> (vector-ref (vector-ref b 1) 0) 0.0) "class 1: high x1")
  (check-true (> (vector-ref (vector-ref b 2) 1) 0.0) "class 2: high x2")
  ;; dev.ratio in (0, 1].
  (check-true (< 0.0 (multinomial-result-dev-ratio r)))
  (check-true (<= (multinomial-result-dev-ratio r) 1.0)))
