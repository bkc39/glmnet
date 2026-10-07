#lang racket/base

;; High-level two-class logistic (binomial) elastic-net fitting on top of the
;; raw FFI.
;;
;; `logistic-fit` is the binomial-family analogue of `elnet-fit`: the same
;; #:alpha / #:lambda knobs, but the response is a 0/1 class label and the fit
;; models the log-odds of class 1. As with the Gaussian models, #:alpha 1.0 is
;; the lasso (sparse) logistic, #:alpha 0.0 the ridge logistic, and values in
;; between the elastic net. `logistic-predict-proba` / `logistic-predict` turn a
;; fit plus new predictors into class-1 probabilities and hard 0/1 labels.

(require racket/contract
         ffi/vector
         "marshal.rkt"
         (only-in "input.rkt" data/c response-for/c)
         (submod "input.rkt" support)
         "model.rkt"
         (submod "model.rkt" support)
         "../foreign/raw/lognet.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "../foreign/raw/path.rkt"
         (only-in "../data.rkt" design-matrix-select-rows)
         "cv.rkt"
         (submod "cv.rkt" support))

(provide
 (struct-out logistic-result)
 (contract-out
  [logistic-fit
   (fit/c (response-for/c X (or/c 0 1) #:classes? #t)
          (#:lambda (>=/c 0))
          (#:alpha (real-in 0 1)
           #:standardize? boolean?
           #:intercept? boolean?
           #:thresh (>/c 0)
           #:max-iters exact-positive-integer?)
          logistic-result?)]
  [logistic-predict-proba
   (-> logistic-result? data/c (listof (real-in 0 1)))]
  [logistic-predict
   (->* (logistic-result? data/c) (#:threshold (real-in 0 1))
        (listof (or/c 0 1 string?)))]))

(provide
 (contract-out
  [logistic-path
   (fit/c (response-for/c X (or/c 0 1) #:classes? #t)
          ()
          (#:lambda lambda-sequence/c
           #:nlambda exact-positive-integer?
           #:lambda-min-ratio lambda-min-ratio/c
           #:alpha (real-in 0 1)
           #:standardize? boolean?
           #:intercept? boolean?
           #:thresh (>/c 0)
           #:max-iters exact-positive-integer?)
          glmnet-path?)]
  [logistic-cv
   (fit/c (response-for/c X (or/c 0 1) #:classes? #t)
          ()
          (#:type-measure (or/c 'deviance 'class 'auc 'mse 'mae)
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

;; A fitted two-class logistic model. `coefficients` is a vector of length ni on
;; the original predictor scale; `intercept` and the coefficients are on the
;; log-odds (logit) scale for class 1. `lambda` is the penalty actually used;
;; `dev-ratio` is the fraction of null deviance explained (the logistic analogue
;; of R^2); `num-passes` is glmnet's coordinate-descent pass count.
(struct logistic-result (intercept coefficients dev-ratio lambda num-passes)
  #:transparent
  #:property prop:custom-write write-fit
  #:methods gen:glmnet-model
  [(define (glmnet-model->path r)
     (single-fit-path 'binomial (logistic-result-lambda r) (logistic-result-intercept r)
                      (logistic-result-coefficients r) (logistic-result-dev-ratio r)
                      (logistic-result-num-passes r)))])

;; A class label, 0 or 1.
(define label/c (or/c 0 1))

;; Cross-validation fits every fold to data with both classes, so the data must
;; have both.
(define (check-both-classes ys who)
  (for ([c (in-list '(0 1))])
    (unless (for/or ([v (in-vector ys)]) (= v c))
      (error who "class ~a has no observations; y needs both 0 and 1" c))))

;; Logistic adds the 8000/9000 (a class probability collapsed -- e.g. perfect
;; separation) and 90000 (coefficient-bound non-convergence) fatal codes on top
;; of the shared cases in `check-jerr`.
(define (check-logistic-jerr jerr who [lmu #f])
  (when (and (>= jerr 8000) (< jerr 9000))
    (error who
           (format (string-append
                    "a class probability collapsed (perfect separation or a "
                    "degenerate class); try a larger lambda (jerr=~a)")
                   jerr)))
  (when (and (>= jerr 9000) (< jerr 10000))
    (error who (format "a class has a degenerate null probability (jerr=~a)" jerr)))
  (when (= jerr 90000)
    (error who "coefficient-bound adjustment failed to converge (jerr=90000)"))
  (check-jerr jerr who lmu))

;; --- public API ------------------------------------------------------------

(define (logistic-fit X y
                      #:predictors [predictors #f]
                      #:lambda lambda
                      #:alpha [alpha 1.0]
                      #:standardize? [standardize? #t]
                      #:intercept? [intercept? #t]
                      #:thresh [thresh 1e-7]
                      #:max-iters [max-iters 100000])
  (define-values (x yv names classes) (class-fit-input 'logistic-fit X y predictors label/c 'binomial))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define beta (make-f64vector ni 0.0))
  (define-values (intercept dev-ratio lam nlp jerr)
    (glmnet-lognet-solo/raw (exact->inexact alpha) no ni (design-matrix-data x) yv
                            (exact->inexact lambda)
                            (if standardize? 1 0)
                            (if intercept? 1 0)
                            (exact->inexact thresh)
                            max-iters
                            beta))
  (check-logistic-jerr jerr 'logistic-fit)
  (attach-data-names (logistic-result intercept (unpack-vector beta ni) dev-ratio lam nlp)
                        names #:classes classes))

;; --- prediction ------------------------------------------------------------

;; P(y = 1 | x) for each row of X.
(define (logistic-predict-proba result X)
  (predict-as 'logistic-predict-proba result X 'response))

;; Hard prediction: class 1 when P(y=1) >= threshold (default 0.5), else
;; class 0, by label for a fit that names its classes.
(define (logistic-predict result X #:threshold [threshold 0.5])
  (define labels (or (model-class-labels result) '(0 1)))
  (for/list ([p (in-list (predict-as 'logistic-predict result X 'response))])
    (if (>= p threshold) (cadr labels) (car labels))))

;; --- regularization path (#10) ---------------------------------------------

(define (logistic-path X y
                       #:predictors [predictors #f]
                       #:lambda [lambda #f]
                       #:nlambda [nlambda 100]
                       #:lambda-min-ratio [lambda-min-ratio #f]
                       #:alpha [alpha 1.0]
                       #:standardize? [standardize? #t]
                       #:intercept? [intercept? #t]
                       #:thresh [thresh 1e-7]
                       #:max-iters [max-iters 100000])
  (define-values (x yv names classes)
    (class-fit-input 'logistic-path X y predictors label/c 'binomial))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define-values (nlam flmin ulam)
    (path-lambdas lambda nlambda lambda-min-ratio no ni))
  (define a0 (make-f64vector nlam 0.0))
  (define beta (make-f64vector (* ni nlam) 0.0))
  (define dev (make-f64vector nlam 0.0))
  (define alm (make-f64vector nlam 0.0))
  (define-values (lmu nlp jerr)
    (glmnet-lognet-path/raw (exact->inexact alpha) no ni (design-matrix-data x) yv
                            nlam flmin ulam
                            (if standardize? 1 0) (if intercept? 1 0)
                            (exact->inexact thresh) max-iters
                            a0 beta dev alm))
  (check-logistic-jerr jerr 'logistic-path lmu)
  (define coefficients (unpack-columns beta ni lmu))
  (attach-data-names
   (glmnet-path 'binomial (finish-lambdas alm lmu (not lambda))
                (unpack-vector a0 lmu) coefficients (unpack-vector dev lmu)
                (count-nonzero coefficients) nlp)
   names #:classes classes))

;; --- cross-validation (#27) ------------------------------------------------

(define (logistic-cv X y
                     #:predictors [predictors #f]
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
  (define-values (x yv names classes)
    (class-fit-input 'logistic-cv X y predictors label/c 'binomial))
  (define ys (response->vector yv))
  (check-both-classes ys 'logistic-cv)
  (define (fit x y)
    (logistic-path x y
                   #:lambda lambda #:nlambda nlambda #:lambda-min-ratio lambda-min-ratio
                   #:alpha alpha #:standardize? standardize? #:intercept? intercept?
                   #:thresh thresh #:max-iters max-iters))
  (define (fit-all) (fit x ys))
  (define (fit-rows rows) (fit (design-matrix-select-rows x rows) (select ys rows)))
  (attach-data-names
   (cross-validate 'logistic-cv x ys fit-all fit-rows
                   #:measure measure #:nfolds nfolds #:fold-ids fold-ids #:grouped? grouped?)
   names #:classes classes))
