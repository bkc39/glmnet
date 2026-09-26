#lang racket/base

;; High-level Cox proportional-hazards elastic-net fitting on top of the raw FFI.
;;
;; Cox is a survival model: the response is a follow-up `time` plus a 0/1 `status`
;; indicator (1 = event observed, 0 = right-censored), and there is NO intercept
;; (the unspecified baseline hazard absorbs it). The fit models the log relative
;; hazard  x . beta -- a positive coefficient raises the hazard, i.e. shortens
;; survival. As with the other families #:alpha 1.0 is the lasso (sparse) fit and
;; #:alpha 0.0 the ridge fit. `cox-linear-predictor` / `cox-relative-risk` turn a
;; fit plus new predictors into the log relative hazard and the relative risk.

(require racket/contract
         ffi/vector
         "marshal.rkt"
         "model.rkt"
         (submod "model.rkt" support)
         "../foreign/raw/coxnet.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "../foreign/raw/path.rkt"
         (only-in "../data.rkt" design-matrix-select-rows)
         "cv.rkt"
         (submod "cv.rkt" support))

(provide
 (struct-out cox-result)
 (contract-out
  [cox-fit
   (->* (design-matrix/c cox-times/c cox-status/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        cox-result?)]
  [cox-linear-predictor (-> cox-result? design-matrix/c (listof real?))]
  [cox-relative-risk    (-> cox-result? design-matrix/c (listof (>/c 0)))]))

(provide
 (contract-out
  [cox-path
   (->* (design-matrix/c cox-times/c cox-status/c)
           (#:lambda lambda-sequence/c
            #:nlambda exact-positive-integer?
            #:lambda-min-ratio lambda-min-ratio/c
            #:alpha (real-in 0 1)
            #:standardize? boolean?
            #:thresh (>/c 0)
            #:max-iters exact-positive-integer?)
        glmnet-path?)]
  [cox-cv
   (->* (design-matrix/c cox-times/c cox-status/c)
        (#:type-measure (or/c 'deviance 'C)
         #:nfolds nfolds/c
         #:fold-ids fold-ids/c
         #:grouped? boolean?
         #:lambda cv-lambda-sequence/c
         #:nlambda exact-positive-integer?
         #:lambda-min-ratio lambda-min-ratio/c
         #:alpha (real-in 0 1)
         #:standardize? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        glmnet-cv?)]))

;; A fitted Cox model. `coefficients` is a dense vector of length ni on the
;; original predictor scale, on the log relative-hazard scale -- there is no
;; intercept. `dev-ratio` is the fraction of null (partial-likelihood) deviance
;; explained; `lambda` is the penalty used; `num-passes` glmnet's pass count.
(struct cox-result (coefficients dev-ratio lambda num-passes)
  #:transparent
  #:property prop:custom-write write-fit
  #:methods gen:glmnet-model
  [(define (glmnet-model->path r)
     (single-fit-path 'cox (cox-result-lambda r) #f (cox-result-coefficients r)
                      (cox-result-dev-ratio r) (cox-result-num-passes r)))])

;; --- input contracts -------------------------------------------------------

;; Follow-up times must be positive; status is a 0/1 event indicator.
(define cox-times/c (and/c (listof (>/c 0)) pair?))
(define (event-flag? s) (and (real? s) (or (= s 0) (= s 1))))
(define cox-status/c (and/c (listof event-flag?) pair?))

;; Cox adds the 8888 (all observations censored -> no events) and 20000/30000
;; (initialization numerical error) fatal codes on top of the shared cases in
;; `check-jerr`.
(define (check-cox-jerr jerr who)
  (cond
    [(= jerr 8888)
     (error who
            "all observations are censored; Cox needs at least one event (status = 1)")]
    [(or (= jerr 20000) (= jerr 30000))
     (error who
            (format (string-append
                     "Cox initialization numerical error (jerr=~a); check the data "
                     "or try a larger lambda")
                    jerr))]
    [else (check-jerr jerr who)]))

(define (check-events statuses who)
  (unless (for/or ([s (in-list statuses)]) (= s 1))
    (error who "at least one observation must be an event (status = 1)")))

;; --- public API ------------------------------------------------------------

(define (cox-fit X times statuses
                 #:lambda lambda
                 #:alpha [alpha 1.0]
                 #:standardize? [standardize? #t]
                 #:thresh [thresh 1e-7]
                 #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'cox-fit "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define tv (as-response times no 'cox-fit "times"))
  (define sv (as-response statuses no 'cox-fit "statuses"))
  (check-events statuses 'cox-fit)
  (define beta (make-f64vector ni 0.0))
  (define-values (dev-ratio lam nlp jerr)
    (glmnet-coxnet-solo/raw (exact->inexact alpha) no ni (design-matrix-data x) tv sv
                            (exact->inexact lambda)
                            (if standardize? 1 0)
                            (exact->inexact thresh)
                            max-iters
                            beta))
  (check-cox-jerr jerr 'cox-fit)
  (cox-result (unpack-vector beta ni) dev-ratio lam nlp))

;; --- prediction ------------------------------------------------------------

;; The log relative hazard x . beta (no intercept) for each row of X.
(define (cox-linear-predictor result X)
  (predict-as 'cox-linear-predictor result X 'link))

;; The relative risk exp(x . beta) for each row of X -- the multiplicative effect
;; on the baseline hazard.
(define (cox-relative-risk result X)
  (predict-as 'cox-relative-risk result X 'response))

;; --- regularization path (#10) ---------------------------------------------

(define (cox-path X times statuses
                  #:lambda [lambda #f]
                  #:nlambda [nlambda 100]
                  #:lambda-min-ratio [lambda-min-ratio #f]
                  #:alpha [alpha 1.0]
                  #:standardize? [standardize? #t]
                  #:thresh [thresh 1e-7]
                  #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'cox-path "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define tv (as-response times no 'cox-path "times"))
  (define sv (as-response statuses no 'cox-path "statuses"))
  (check-events statuses 'cox-path)
  (define-values (nlam flmin ulam)
    (path-lambdas lambda nlambda lambda-min-ratio no ni))
  (define beta (make-f64vector (* ni nlam) 0.0))
  (define dev (make-f64vector nlam 0.0))
  (define alm (make-f64vector nlam 0.0))
  (define-values (lmu nlp jerr)
    (glmnet-coxnet-path/raw (exact->inexact alpha) no ni (design-matrix-data x) tv sv
                            nlam flmin ulam
                            (if standardize? 1 0)
                            (exact->inexact thresh) max-iters
                            beta dev alm))
  (check-cox-jerr jerr 'cox-path)
  (define coefficients (unpack-columns beta ni lmu))
  (glmnet-path 'cox (finish-lambdas alm lmu (not lambda))
               #f coefficients (unpack-vector dev lmu)
               (count-nonzero coefficients) nlp))

;; --- cross-validation (#27) ------------------------------------------------

(define (cox-cv X times statuses
                #:type-measure [measure 'deviance]
                #:nfolds [nfolds 10]
                #:fold-ids [fold-ids #f]
                #:grouped? [grouped? #t]
                #:lambda [lambda #f]
                #:nlambda [nlambda 100]
                #:lambda-min-ratio [lambda-min-ratio #f]
                #:alpha [alpha 1.0]
                #:standardize? [standardize? #t]
                #:thresh [thresh 1e-7]
                #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'cox-cv "X"))
  (define no (design-matrix-nrows x))
  (define tv (as-response times no 'cox-cv "times"))
  (define sv (as-response statuses no 'cox-cv "statuses"))
  (check-events statuses 'cox-cv)
  (define ts (list->vector (f64vector->list tv)))
  (define ds (list->vector (f64vector->list sv)))
  (define (fit x times statuses)
    (cox-path x times statuses
              #:lambda lambda #:nlambda nlambda #:lambda-min-ratio lambda-min-ratio
              #:alpha alpha #:standardize? standardize?
              #:thresh thresh #:max-iters max-iters))
  (define (fit-all) (fit x times statuses))
  (define (fit-rows rows)
    (fit (design-matrix-select-rows x rows) (select ts rows) (select ds rows)))
  (cross-validate 'cox-cv x (for/vector ([t (in-vector ts)] [d (in-vector ds)]) (cons t d))
                  fit-all fit-rows
                  #:measure measure #:nfolds nfolds #:fold-ids fold-ids #:grouped? grouped?))
