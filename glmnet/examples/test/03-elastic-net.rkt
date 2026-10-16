#lang racket/base

;; Runner + tests for the literate example ../03-elastic-net.rkt.

(require racket/dict
         glmnet
         "../03-elastic-net.rkt")

(module+ main
  (println fit)
  (println (coef fit))
  (println zero-counts)
  (for ([(α cv) (in-dict alpha-cvs)])
    (printf "α = ~a: ~a\n" α cv)))

(module+ test
  (require racket/match
           rackunit)
  (match-define (list ridge-zeros enet-zeros lasso-zeros) zero-counts)
  (check-equal? ridge-zeros 0 "ridge zeroes none")
  (check-true (<= 1 enet-zeros lasso-zeros)
              "the elastic net selects at least one out, and no more than the lasso")
  (check-true (< 0.0 (deviance-ratio fit) 1.0) "a penalized fit, so 0 < R² < 1")
  (match-define (vector _ lasso-copy₁ lasso-copy₂ _) (coef lasso-dup))
  (check-true (> lasso-copy₁ 1.0) "the lasso puts x₁'s weight on the first copy")
  (check-true (< (abs lasso-copy₂) 1e-12) "and leaves the second within rounding of zero")
  (match-define (vector _ enet-copy₁ enet-copy₂ _) (coef enet-dup))
  (check-= enet-copy₁ enet-copy₂ 0.05 "the elastic net splits it almost equally")

  (define (best cv)
    (for/first ([λ (in-vector (glmnet-cv-lambda cv))]
                [err (in-vector (glmnet-cv-cvm cv))]
                [se (in-vector (glmnet-cv-cvsd cv))]
                #:when (= λ (glmnet-cv-lambda-min cv)))
      (list err se)))
  (define ridge-cv (dict-ref alpha-cvs 0.0))
  (match-define (list ridge-err _) (best ridge-cv))
  (match-define (list longer-ridge-err _) (best longer-ridge-cv))
  (match-define (list lasso-err lasso-se) (best (dict-ref alpha-cvs 1.0)))
  (check-equal? (glmnet-cv-index-min ridge-cv) (sub1 (vector-length (glmnet-cv-lambda ridge-cv)))
                "ridge's best λ is the last of its default path")
  (check-true (> ridge-err (+ lasso-err lasso-se))
              "on its default path, ridge is more than one standard error worse than the lasso")
  (check-true (< (+ (glmnet-cv-index-min longer-ridge-cv) 25) (length longer-grid))
              "on a longer grid ridge's error bottoms out at least 25 points before its end")
  (check-true (<= longer-ridge-err (+ lasso-err lasso-se))
              "and is within one standard error of the lasso")
  (for* ([(α cv) (in-dict alpha-cvs)]
         [(α′ cv′) (in-dict alpha-cvs)]
         #:when (and (positive? α) (positive? α′)))
    (match-define (list err se) (best cv))
    (match-define (list err′ _) (best cv′))
    (check-true (<= (abs (- err err′)) se)
                (format "α = ~a is within one standard error of α = ~a" α α′)))
  (define best-errs
    (for/list ([(α cv) (in-dict alpha-cvs)] #:when (positive? α))
      (match-define (list err _) (best cv))
      err))
  (define α-spread (- (apply max best-errs) (apply min best-errs)))
  (for ([(α cv) (in-dict alpha-cvs)] #:when (positive? α))
    (define cvm (vector->list (glmnet-cv-cvm cv)))
    (check-true (> (- (apply max cvm) (apply min cvm)) (* 100 α-spread))
                (format "at α = ~a, λ moves the error 100 times as far as α does" α))))
