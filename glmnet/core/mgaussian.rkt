#lang racket/base

;; High-level multi-response Gaussian ("mgaussian") elastic-net fitting on top of
;; the raw FFI.
;;
;; `mgaussian-fit` fits several Gaussian responses jointly: `Y` is a matrix (one
;; column per response) and the model uses a GROUPED lasso across responses, so a
;; predictor enters or leaves for all responses together -- the fitted coefficient
;; rows share support. The result carries per-response intercepts and per-response
;; coefficient vectors; `mgaussian-predict` gives the per-response linear
;; predictions. As elsewhere #:alpha 1.0 is the (grouped) lasso and #:alpha 0.0
;; the ridge.

(require racket/contract
         ffi/vector
         "marshal.rkt"
         "model.rkt"
         (submod "model.rkt" support)
         "../foreign/raw/mgaussian.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "../foreign/raw/path.rkt"
         (only-in "../data.rkt" design-matrix-select-rows design-matrix->rows)
         "cv.rkt"
         (submod "cv.rkt" support))

(provide
 (struct-out mgaussian-result)
 (contract-out
  [mgaussian-fit
   (->* (design-matrix/c design-matrix/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        mgaussian-result?)]
  [mgaussian-predict
   (-> mgaussian-result? design-matrix/c (listof (listof real?)))]))

(provide
 (contract-out
  [mgaussian-path
   (->* (design-matrix/c design-matrix/c)
           (#:lambda lambda-sequence/c
            #:nlambda exact-positive-integer?
            #:lambda-min-ratio lambda-min-ratio/c
            #:alpha (real-in 0 1)
            #:standardize? boolean?
            #:intercept? boolean?
            #:thresh (>/c 0)
            #:max-iters exact-positive-integer?)
        glmnet-path?)]
  [mgaussian-cv
   (->* (design-matrix/c design-matrix/c)
        (#:type-measure (or/c 'mse 'deviance 'mae)
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

;; A fitted multi-response Gaussian model. `intercepts` is a vector of nr reals;
;; `coefficients` is a vector of nr coefficient vectors (each length ni), one per
;; response, on the original predictor scale. `r-squared` is the fraction of
;; (multi-response) variance explained; `lambda` the penalty used; `num-passes`
;; glmnet's pass count.
(struct mgaussian-result (intercepts coefficients r-squared lambda num-passes)
  #:transparent
  #:property prop:custom-write write-fit
  #:methods gen:glmnet-model
  [(define (glmnet-model->path r)
     (single-fit-path 'mgaussian (mgaussian-result-lambda r)
                      (mgaussian-result-intercepts r) (mgaussian-result-coefficients r)
                      (mgaussian-result-r-squared r) (mgaussian-result-num-passes r)))])

;; The response matrix Y as a design matrix with one row per observation.
(define (response-matrix Y no who)
  (define y (as-design-matrix Y who "Y"))
  (unless (= (design-matrix-nrows y) no)
    (raise-arguments-error who "Y does not have one row per row of X"
                           "rows of Y" (design-matrix-nrows y) "rows of X" no))
  y)

;; --- public API ------------------------------------------------------------

(define (mgaussian-fit X Y
                       #:lambda lambda
                       #:alpha [alpha 1.0]
                       #:standardize? [standardize? #t]
                       #:intercept? [intercept? #t]
                       #:thresh [thresh 1e-7]
                       #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'mgaussian-fit "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define y (response-matrix Y no 'mgaussian-fit))
  (define nr (design-matrix-ncols y))
  (define intercepts (make-f64vector nr 0.0))
  (define beta (make-f64vector (* ni nr) 0.0))
  (define-values (r-squared lam nlp jerr)
    (glmnet-mgaussian-solo/raw (exact->inexact alpha) no ni nr
                               (design-matrix-data x) (design-matrix-data y)
                               (exact->inexact lambda)
                               (if standardize? 1 0)
                               (if intercept? 1 0)
                               (exact->inexact thresh)
                               max-iters
                               intercepts beta))
  (check-jerr jerr 'mgaussian-fit)
  ;; beta is response-major: response r's predictor j at r*ni + j.
  (mgaussian-result (unpack-vector intercepts nr) (unpack-columns beta ni nr)
                    r-squared lam nlp))

;; --- prediction ------------------------------------------------------------

;; The per-response predictions a0_r + x . beta_r for each row of X (one inner
;; list per row, nr entries each).
(define (mgaussian-predict result X)
  (predict-as 'mgaussian-predict result X 'link))

;; --- regularization path (#10) ---------------------------------------------

(define (mgaussian-path X Y
                        #:lambda [lambda #f]
                        #:nlambda [nlambda 100]
                        #:lambda-min-ratio [lambda-min-ratio #f]
                        #:alpha [alpha 1.0]
                        #:standardize? [standardize? #t]
                        #:intercept? [intercept? #t]
                        #:thresh [thresh 1e-7]
                        #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'mgaussian-path "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define y (response-matrix Y no 'mgaussian-path))
  (define k (design-matrix-ncols y))
  (define-values (nlam flmin ulam)
    (path-lambdas lambda nlambda lambda-min-ratio no ni))
  (define a0 (make-f64vector (* k nlam) 0.0))
  (define beta (make-f64vector (* ni k nlam) 0.0))
  (define dev (make-f64vector nlam 0.0))
  (define alm (make-f64vector nlam 0.0))
  (define-values (lmu nlp jerr)
    (glmnet-mgaussian-path/raw (exact->inexact alpha) no ni k
                               (design-matrix-data x) (design-matrix-data y)
                               nlam flmin ulam
                               (if standardize? 1 0) (if intercept? 1 0)
                               (exact->inexact thresh) max-iters
                               a0 beta dev alm))
  (check-jerr jerr 'mgaussian-path)
  (define coefficients (unpack-column-groups beta ni k lmu))
  (glmnet-path 'mgaussian (finish-lambdas alm lmu (not lambda))
               (unpack-intercept-groups a0 k lmu) coefficients (unpack-vector dev lmu)
               (count-nonzero-groups coefficients) nlp))

;; --- cross-validation (#27) ------------------------------------------------

(define (mgaussian-cv X Y
                      #:type-measure [measure 'mse]
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
  (define x (as-design-matrix X 'mgaussian-cv "X"))
  (define y (response-matrix Y (design-matrix-nrows x) 'mgaussian-cv))
  (define (fit x y)
    (mgaussian-path x y
                    #:lambda lambda #:nlambda nlambda #:lambda-min-ratio lambda-min-ratio
                    #:alpha alpha #:standardize? standardize? #:intercept? intercept?
                    #:thresh thresh #:max-iters max-iters))
  (define (fit-all) (fit x y))
  (define (fit-rows rows)
    (fit (design-matrix-select-rows x rows) (design-matrix-select-rows y rows)))
  (cross-validate 'mgaussian-cv x (for/vector ([row (in-list (design-matrix->rows y))])
                                    (list->vector row))
                  fit-all fit-rows
                  #:measure measure #:nfolds nfolds #:fold-ids fold-ids #:grouped? grouped?))
