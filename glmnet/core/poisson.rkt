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
         racket/flonum
         ffi/vector
         "marshal.rkt"
         "model.rkt"
         (submod "model.rkt" support)
         "../foreign/raw/fishnet.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "../foreign/raw/path.rkt"
         (only-in "../data.rkt" design-matrix-select-rows)
         "cv.rkt"
         (submod "cv.rkt" support))

(provide
 (struct-out poisson-result)
 (contract-out
  [poisson-fit
   (->* (design-matrix/c (response/c count/c) #:lambda (>=/c 0))
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
   (->* (design-matrix/c (response/c count/c))
           (#:lambda lambda-sequence/c
            #:nlambda exact-positive-integer?
            #:lambda-min-ratio lambda-min-ratio/c
            #:alpha (real-in 0 1)
            #:standardize? boolean?
            #:intercept? boolean?
            #:thresh (>/c 0)
            #:max-iters exact-positive-integer?)
        glmnet-path?)]
  [poisson-cv
   (->* (design-matrix/c (response/c count/c))
        (#:type-measure (or/c 'deviance 'mse 'mae)
         #:nfolds nfolds/c
         #:fold-ids fold-ids/c
         #:grouped? boolean?
         #:lambda cv-lambda-sequence/c
         #:nlambda exact-positive-integer?
         #:lambda-min-ratio lambda-min-ratio/c
         #:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        glmnet-cv?)]))

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

;; Poisson adds the 8888 (negative response counts) fatal code on top of the
;; shared cases in `check-jerr`.
(define (check-poisson-jerr jerr who [lmu #f])
  (when (= jerr 8888)
    (error who "response counts must be non-negative (jerr=8888)"))
  (check-jerr jerr who lmu))

;; A count, or a rate: a non-negative real.
(define count/c (>=/c 0))

;; With no positive count the null model's log mean is -inf and glmnet cannot
;; converge (R warns and returns an empty model).
(define (check-some-count yv who)
  (unless (for/or ([k (in-range (f64vector-length yv))]) (fl> (f64vector-ref yv k) 0.0))
    (error who "the response has no positive count; Poisson needs at least one y > 0")))

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
  (define yv (as-response y no 'poisson-fit "y" count/c))
  (check-some-count yv 'poisson-fit)
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
  (define yv (as-response y no 'poisson-path "y" count/c))
  (check-some-count yv 'poisson-path)
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
  (check-poisson-jerr jerr 'poisson-path lmu)
  (define coefficients (unpack-columns beta ni lmu))
  (glmnet-path 'poisson (finish-lambdas alm lmu (not lambda))
               (unpack-vector a0 lmu) coefficients (unpack-vector dev lmu)
               (count-nonzero coefficients) nlp))

;; --- cross-validation (#27) ------------------------------------------------

(define (poisson-cv X y
                    #:type-measure [measure 'deviance]
                    #:nfolds [nfolds 10]
                    #:fold-ids [fold-ids #f]
                    #:grouped? [grouped? #t]
                    #:lambda [lambda #f]
                    #:nlambda [nlambda 100]
                    #:lambda-min-ratio [lambda-min-ratio #f]
                    #:alpha [alpha 1.0]
                    #:standardize? [standardize? #t]
                    #:intercept? [intercept? #t]
                    #:thresh [thresh 1e-7]
                    #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'poisson-cv "X"))
  (define ys
    (response->vector (as-response y (design-matrix-nrows x) 'poisson-cv "y" count/c)))
  (define (fit x y)
    (poisson-path x y
                  #:lambda lambda #:nlambda nlambda #:lambda-min-ratio lambda-min-ratio
                  #:alpha alpha #:standardize? standardize? #:intercept? intercept?
                  #:thresh thresh #:max-iters max-iters))
  (define (fit-all) (fit x y))
  (define (fit-rows rows) (fit (design-matrix-select-rows x rows) (select ys rows)))
  (cross-validate 'poisson-cv x ys fit-all fit-rows
                  #:measure measure #:nfolds nfolds #:fold-ids fold-ids #:grouped? grouped?))
