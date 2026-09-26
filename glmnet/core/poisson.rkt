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
         "model.rkt"
         (submod "model.rkt" support)
         "../foreign/raw/fishnet.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "../foreign/raw/path.rkt")

(provide
 (struct-out poisson-result)
 (contract-out
  [poisson-fit
   (->* (design-matrix/c count-response/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        poisson-result?)]
  [poisson-predict-mean
   (-> poisson-result? design-matrix/c (listof (>/c 0)))]))

(provide
 (contract-out
  [poisson-path
   (->* (design-matrix/c count-response/c)
           (#:lambda lambda-sequence/c
            #:nlambda exact-positive-integer?
            #:lambda-min-ratio lambda-min-ratio/c
            #:alpha (real-in 0 1)
            #:standardize? boolean?
            #:intercept? boolean?
            #:thresh (>/c 0)
            #:max-iters exact-positive-integer?)
        glmnet-path?)]))

;; A fitted Poisson model. `intercept` and `coefficients` (a dense vector of
;; length ni on the original predictor scale) are on the log-mean scale.
;; `dev-ratio` is the fraction of null deviance explained; `lambda` the penalty
;; used; `num-passes` glmnet's coordinate-descent pass count.
(struct poisson-result (intercept coefficients dev-ratio lambda num-passes)
  #:transparent
  #:property prop:custom-write write-fit
  #:methods gen:glmnet-model
  [(define (glmnet-model->path r)
     (single-fit-path 'poisson (poisson-result-lambda r) (poisson-result-intercept r)
                      (poisson-result-coefficients r) (poisson-result-dev-ratio r)
                      (poisson-result-num-passes r)))])

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
  (define x (as-design-matrix X 'poisson-fit "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define yv (as-response y no 'poisson-fit "y"))
  (define beta (make-f64vector ni 0.0))
  (define-values (intercept dev-ratio lam nlp jerr)
    (glmnet-fishnet-solo/raw (exact->inexact alpha) no ni (design-matrix-data x) yv
                             (exact->inexact lambda)
                             (if standardize? 1 0)
                             (if intercept? 1 0)
                             (exact->inexact thresh)
                             max-iters
                             beta))
  (check-poisson-jerr jerr 'poisson-fit)
  (poisson-result intercept (unpack-vector beta ni) dev-ratio lam nlp))

;; --- prediction ------------------------------------------------------------

;; The fitted Poisson mean exp(intercept + x . beta) for each row of X.
(define (poisson-predict-mean result X)
  (predict-as 'poisson-predict-mean result X 'response))

;; --- regularization path (#10) ---------------------------------------------

(define (poisson-path X y
                      #:lambda [lambda #f]
                      #:nlambda [nlambda 100]
                      #:lambda-min-ratio [lambda-min-ratio #f]
                      #:alpha [alpha 1.0]
                      #:standardize? [standardize? #t]
                      #:intercept? [intercept? #t]
                      #:thresh [thresh 1e-7]
                      #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'poisson-path "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define yv (as-response y no 'poisson-path "y"))
  (define-values (nlam flmin ulam)
    (path-lambdas lambda nlambda lambda-min-ratio no ni))
  (define a0 (make-f64vector nlam 0.0))
  (define beta (make-f64vector (* ni nlam) 0.0))
  (define dev (make-f64vector nlam 0.0))
  (define alm (make-f64vector nlam 0.0))
  (define-values (lmu nlp jerr)
    (glmnet-fishnet-path/raw (exact->inexact alpha) no ni (design-matrix-data x) yv
                             nlam flmin ulam
                             (if standardize? 1 0) (if intercept? 1 0)
                             (exact->inexact thresh) max-iters
                             a0 beta dev alm))
  (check-poisson-jerr jerr 'poisson-path)
  (define coefficients (unpack-columns beta ni lmu))
  (glmnet-path 'poisson (finish-lambdas alm lmu (not lambda))
               (unpack-vector a0 lmu) coefficients (unpack-vector dev lmu)
               (count-nonzero coefficients) nlp))
