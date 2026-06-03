#lang racket/base

;; High-level Gaussian elastic-net fitting on top of the raw FFI.
;;
;; OLS, ridge, lasso, and elastic net are all `elnet-fit` with different #:alpha
;; and #:lambda; thin convenience wrappers name the common cases. The result is
;; an `elnet-result` carrying a dense coefficient vector.

(require racket/contract
         ffi/vector
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

;; --- input contracts -------------------------------------------------------

(define matrix/c (and/c (listof (listof real?)) pair?))
(define response/c (and/c (listof real?) pair?))

;; --- marshalling -----------------------------------------------------------

(define (rows->dims X who)
  (define no (length X))
  (define ni (length (car X)))
  (when (zero? ni)
    (error who "predictor rows must be non-empty"))
  (unless (andmap (lambda (r) (= (length r) ni)) X)
    (error who "all predictor rows must have the same length (got ragged rows)"))
  (values no ni))

;; Pack the row-major matrix X (list of rows) into the column-major f64vector
;; the Fortran expects: element (i, j) lives at index i + j*no.
(define (matrix->colmajor X no ni)
  (define v (make-f64vector (* no ni)))
  (for ([row (in-list X)]
        [i (in-naturals)])
    (for ([xij (in-list row)]
          [j (in-naturals)])
      (f64vector-set! v (+ i (* j no)) (exact->inexact xij))))
  v)

(define (response->f64vector y no who)
  (unless (= (length y) no)
    (error who "response length ~a does not match ~a observations" (length y) no))
  (list->f64vector (map exact->inexact y)))

;; --- error handling --------------------------------------------------------

;; Map glmnet's jerr flag (documented in fortran/vendor/glmnet5.f90) to a Racket
;; error (fatal) or a logged warning (non-fatal partial result).
(define (check-jerr jerr who)
  (cond
    [(zero? jerr) (void)]
    [(positive? jerr)
     (error who
            (cond
              [(= jerr 7777) "all used predictors have zero variance"]
              [(= jerr 10000) "no penalized predictors (penalty factors are all <= 0)"]
              [(< jerr 7777) (format "glmnet memory allocation error (jerr=~a)" jerr)]
              [else (format "glmnet fatal error (jerr=~a)" jerr)]))]
    [else
     (log-warning
      "~a: glmnet did not fully converge for this lambda (jerr=~a); coefficients are partial"
      who jerr)]))

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
