#lang racket/base

;; Unit tests for the high-level K-class multinomial API (core/multinomial.rkt).

(module+ test
  (require rackunit
           glmnet)

  ;; Three separable clusters in 2-D: class 0 at low x1, class 1 at high x1,
  ;; class 2 at high x2.
  (define X '((1.0 1.0) (2.0 1.0) (1.0 2.0) (2.0 2.0)
              (5.0 1.0) (6.0 1.0) (5.0 2.0) (6.0 2.0)
              (3.0 5.0) (4.0 5.0) (3.0 6.0) (4.0 6.0)))
  (define y '(0 0 0 0 1 1 1 1 2 2 2 2))

  (define (num-zeros r)
    (for*/sum ([bk (in-vector (multinomial-result-coefficients r))]
               [bi (in-vector bk)] #:when (zero? bi)) 1))

  ;; --- shape + coefficients --------------------------------------------------

  (test-case "fit returns K intercepts and K coefficient vectors"
    (define r (multinomial-fit X y #:lambda 0.01))
    (check-equal? (vector-length (multinomial-result-intercepts r)) 3)
    (check-equal? (vector-length (multinomial-result-coefficients r)) 3)
    (for ([bk (in-vector (multinomial-result-coefficients r))])
      (check-equal? (vector-length bk) 2)))

  (test-case "each class loads on its discriminating feature with the right sign"
    (define b (multinomial-result-coefficients (multinomial-fit X y #:lambda 0.01)))
    (check-true (< (vector-ref (vector-ref b 0) 0) 0.0) "class 0: low x1")
    (check-true (> (vector-ref (vector-ref b 1) 0) 0.0) "class 1: high x1")
    (check-true (> (vector-ref (vector-ref b 2) 1) 0.0) "class 2: high x2"))

  (test-case "lasso multinomial is sparse; ridge keeps every coefficient"
    (check-true (> (num-zeros (multinomial-fit X y #:lambda 0.01)) 0))
    (check-equal? (num-zeros (multinomial-fit X y #:lambda 0.1 #:alpha 0.0)) 0))

  (test-case "sparsity grows with lambda"
    (check-true (>= (num-zeros (multinomial-fit X y #:lambda 0.3))
                    (num-zeros (multinomial-fit X y #:lambda 0.01)))))

  ;; --- dev-ratio -------------------------------------------------------------

  (test-case "dev-ratio is a fraction of null deviance in (0, 1]"
    (define d (multinomial-result-dev-ratio (multinomial-fit X y #:lambda 0.01)))
    (check-true (< 0.0 d))
    (check-true (<= d 1.0)))

  (test-case "huge lambda collapses to the null model"
    (define r (multinomial-fit X y #:lambda 1e6))
    (check-equal? (num-zeros r) 6 "all coefficients zero")
    (check-= (multinomial-result-dev-ratio r) 0.0 1e-3))

  ;; --- prediction ------------------------------------------------------------

  (test-case "predict classifies every separable training point correctly"
    (define r (multinomial-fit X y #:lambda 0.01))
    (check-equal? (multinomial-predict r X) y))

  (test-case "predict labels are in 0..K-1"
    (define preds (multinomial-predict (multinomial-fit X y #:lambda 0.01) X))
    (check-true (for/and ([p (in-list preds)]) (and (>= p 0) (< p 3)))))

  (test-case "predict-proba rows are probability distributions"
    (define r (multinomial-fit X y #:lambda 0.01))
    (for ([ps (in-list (multinomial-predict-proba r X))])
      (check-equal? (length ps) 3)
      (check-true (for/and ([p (in-list ps)]) (and (>= p 0.0) (<= p 1.0))))
      (check-= (apply + ps) 1.0 1e-9))
    ;; predict is the argmax of predict-proba
    (check-equal? (multinomial-predict r X)
                  (for/list ([ps (in-list (multinomial-predict-proba r X))])
                    (let loop ([rest (cdr ps)] [i 1] [best (car ps)] [bi 0])
                      (cond [(null? rest) bi]
                            [(> (car rest) best) (loop (cdr rest) (add1 i) (car rest) i)]
                            [else (loop (cdr rest) (add1 i) best bi)])))))

  ;; --- contracts / input validation -----------------------------------------

  (test-case "non-integer class label is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (multinomial-fit X '(0 0 0 0 1 1 1 1 2 2 2 1.5) #:lambda 0.01))))

  (test-case "negative class label is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (multinomial-fit X '(0 0 0 0 1 1 1 1 2 2 2 -1) #:lambda 0.01))))

  (test-case "a single class is rejected (need >= 2)"
    (check-exn exn:fail?
               (lambda () (multinomial-fit '((1.0 2.0) (3.0 4.0)) '(0 0) #:lambda 0.01))))

  (test-case "non-contiguous labels (a class with no observations) are rejected"
    (check-exn exn:fail?
               (lambda () (multinomial-fit '((1.0 2.0) (3.0 4.0)) '(0 2) #:lambda 0.01))))

  (test-case "alpha outside [0,1] is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (multinomial-fit X y #:lambda 0.01 #:alpha 2.0))))

  (test-case "negative lambda is a contract error"
    (check-exn exn:fail:contract?
               (lambda () (multinomial-fit X y #:lambda -0.1))))

  (test-case "predict rejects rows with the wrong number of features"
    (define r (multinomial-fit X y #:lambda 0.01))
    (check-exn exn:fail? (lambda () (multinomial-predict-proba r '((1.0 2.0 3.0)))))))
