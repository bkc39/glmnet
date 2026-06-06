#lang racket/base

;; High-level Poisson elastic-net fitting on top of the raw FFI.
;;
;; `poisson-fit` mirrors `elnet-fit`: the same #:alpha / #:lambda knobs, but the
;; response is a list of non-negative counts and the model uses a log link, so
;; the fitted mean is  exp(intercept + x . beta). A positive coefficient raises
;; the expected count. As elsewhere #:alpha 1.0 is the lasso (sparse) fit and
;; #:alpha 0.0 the ridge fit. `poisson-predict-mean` returns the fitted rate for
;; new predictors.

(require racket/contract
         ffi/vector
         "marshal.rkt"
         "../foreign/raw/fishnet.rkt")

(provide
 (struct-out poisson-result)
 (contract-out
  [poisson-fit
   (->* (matrix/c count-response/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        poisson-result?)]
  [poisson-predict-mean
   (-> poisson-result? matrix/c (listof (>/c 0)))]))

;; A fitted Poisson model. `intercept` and `coefficients` (a dense vector of
;; length ni on the original predictor scale) are on the log-mean scale.
;; `dev-ratio` is the fraction of null deviance explained; `lambda` the penalty
;; used; `num-passes` glmnet's coordinate-descent pass count.
(struct poisson-result (intercept coefficients dev-ratio lambda num-passes)
  #:transparent)

;; --- input contract --------------------------------------------------------

;; The response is a non-empty list of non-negative counts (or rates).
(define count-response/c (and/c (listof (>=/c 0)) pair?))

;; Poisson adds the 8888 (negative response counts) fatal code on top of the
;; shared cases in `check-jerr`.
(define (check-poisson-jerr jerr who)
  (cond
    [(= jerr 8888)
     (error who "response counts must be non-negative (jerr=8888)")]
    [else (check-jerr jerr who)]))

;; --- public API ------------------------------------------------------------

(define (poisson-fit X y
                     #:lambda lambda
                     #:alpha [alpha 1.0]
                     #:standardize? [standardize? #t]
                     #:intercept? [intercept? #t]
                     #:thresh [thresh 1e-7]
                     #:max-iters [max-iters 100000])
  (define-values (no ni) (rows->dims X 'poisson-fit))
  (define xcol (matrix->colmajor X no ni))
  (define yv (response->f64vector y no 'poisson-fit))
  (define beta (make-f64vector ni 0.0))
  (define-values (intercept dev-ratio lam nlp jerr)
    (glmnet-fishnet-solo/raw (exact->inexact alpha) no ni xcol yv
                             (exact->inexact lambda)
                             (if standardize? 1 0)
                             (if intercept? 1 0)
                             (exact->inexact thresh)
                             max-iters
                             beta))
  (check-poisson-jerr jerr 'poisson-fit)
  (poisson-result intercept
                  (for/vector ([i (in-range ni)]) (f64vector-ref beta i))
                  dev-ratio lam nlp))

;; --- prediction ------------------------------------------------------------

;; The linear predictor intercept + x . beta (log mean) for one predictor row.
(define (eta-row result row)
  (define coefs (poisson-result-coefficients result))
  (unless (= (length row) (vector-length coefs))
    (error 'poisson-predict-mean
           "row has ~a features, expected ~a" (length row) (vector-length coefs)))
  (for/fold ([acc (poisson-result-intercept result)])
            ([b (in-vector coefs)]
             [xj (in-list row)])
    (+ acc (* b (exact->inexact xj)))))

;; The fitted Poisson mean exp(intercept + x . beta) for each row of X.
(define (poisson-predict-mean result X)
  (for/list ([row (in-list X)]) (exp (eta-row result row))))
