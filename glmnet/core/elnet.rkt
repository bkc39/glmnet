#lang racket/base

;; High-level Gaussian elastic-net fitting on top of the raw FFI.
;;
;; OLS, ridge, lasso, and elastic net are all `elnet-fit` with different #:alpha
;; and #:lambda; thin convenience wrappers name the common cases. The result is
;; an `elnet-result` carrying a dense coefficient vector.

(require racket/contract
         ffi/vector
         "marshal.rkt"
         "../foreign/raw/elnet.rkt")

(provide
 (struct-out elnet-result)
 (contract-out
  [elnet-fit
   (->* (matrix/c response/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        elnet-result?)]
  [ols
   (->* (matrix/c response/c)
        (#:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        elnet-result?)]
  [ridge
   (->* (matrix/c response/c #:lambda (>=/c 0))
        (#:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        elnet-result?)]
  [lasso
   (->* (matrix/c response/c #:lambda (>=/c 0))
        (#:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        elnet-result?)]
  [elastic-net
   (->* (matrix/c response/c #:alpha (real-in 0 1) #:lambda (>=/c 0))
        (#:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        elnet-result?)]))

;; A fitted model. `coefficients` is a vector of length ni on the original
;; predictor scale; `lambda` is the penalty actually used; `r-squared` is the
;; fraction of null deviance explained; `num-passes` is glmnet's pass count.
(struct elnet-result (intercept coefficients r-squared lambda num-passes)
  #:transparent)

;; --- public API ------------------------------------------------------------

(define (elnet-fit X y
                   #:lambda lambda
                   #:alpha [alpha 1.0]
                   #:standardize? [standardize? #t]
                   #:intercept? [intercept? #t]
                   #:thresh [thresh 1e-7]
                   #:max-iters [max-iters 100000])
  (define-values (no ni) (rows->dims X 'elnet-fit))
  (define xcol (matrix->colmajor X no ni))
  (define yv (response->f64vector y no 'elnet-fit))
  (define beta (make-f64vector ni 0.0))
  (define-values (intercept rsq lam nlp jerr)
    (glmnet-elnet-solo/raw (exact->inexact alpha) no ni xcol yv
                           (exact->inexact lambda)
                           (if standardize? 1 0)
                           (if intercept? 1 0)
                           (exact->inexact thresh)
                           max-iters
                           beta))
  (check-jerr jerr 'elnet-fit)
  (elnet-result intercept
                (for/vector ([i (in-range ni)]) (f64vector-ref beta i))
                rsq lam nlp))

;; Ordinary least squares = elastic net at lambda 0 (alpha then irrelevant).
;; Coordinate descent approaches the OLS solution from above as `thresh`
;; tightens, so `ols` uses a tighter default than the penalized fits where
;; glmnet's 1e-7 is conventional.
(define (ols X y
             #:standardize? [standardize? #t]
             #:intercept? [intercept? #t]
             #:thresh [thresh 1e-10]
             #:max-iters [max-iters 100000])
  (elnet-fit X y
             #:alpha 1.0
             #:lambda 0.0
             #:standardize? standardize?
             #:intercept? intercept?
             #:thresh thresh
             #:max-iters max-iters))

;; Ridge regression = elastic net at alpha 0 (pure L2 penalty). Shrinks all
;; coefficients smoothly toward zero; none are driven exactly to zero.
(define (ridge X y
               #:lambda lambda
               #:standardize? [standardize? #t]
               #:intercept? [intercept? #t]
               #:thresh [thresh 1e-7]
               #:max-iters [max-iters 100000])
  (elnet-fit X y
             #:alpha 0.0
             #:lambda lambda
             #:standardize? standardize?
             #:intercept? intercept?
             #:thresh thresh
             #:max-iters max-iters))

;; Lasso = elastic net at alpha 1 (pure L1 penalty). Performs variable
;; selection: coefficients are driven exactly to zero, more of them as lambda
;; grows.
(define (lasso X y
               #:lambda lambda
               #:standardize? [standardize? #t]
               #:intercept? [intercept? #t]
               #:thresh [thresh 1e-7]
               #:max-iters [max-iters 100000])
  (elnet-fit X y
             #:alpha 1.0
             #:lambda lambda
             #:standardize? standardize?
             #:intercept? intercept?
             #:thresh thresh
             #:max-iters max-iters))

;; Elastic net at an explicit alpha in [0,1]: blends the lasso's selection with
;; the ridge's shrinkage. alpha 0 reduces to `ridge`, alpha 1 to `lasso`.
(define (elastic-net X y
                     #:alpha alpha
                     #:lambda lambda
                     #:standardize? [standardize? #t]
                     #:intercept? [intercept? #t]
                     #:thresh [thresh 1e-7]
                     #:max-iters [max-iters 100000])
  (elnet-fit X y
             #:alpha alpha
             #:lambda lambda
             #:standardize? standardize?
             #:intercept? intercept?
             #:thresh thresh
             #:max-iters max-iters))
