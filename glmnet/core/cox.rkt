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
         "../foreign/raw/coxnet.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "../foreign/raw/path.rkt")

(provide
 (struct-out cox-result)
 (contract-out
  [cox-fit
   (->* (matrix/c cox-times/c cox-status/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        cox-result?)]
  [cox-linear-predictor (-> cox-result? matrix/c (listof real?))]
  [cox-relative-risk    (-> cox-result? matrix/c (listof (>/c 0)))]))

(provide
 (contract-out
  [cox-path
   (->* (matrix/c cox-times/c cox-status/c)
           (#:lambda lambda-sequence/c
            #:nlambda exact-positive-integer?
            #:lambda-min-ratio lambda-min-ratio/c
            #:alpha (real-in 0 1)
            #:standardize? boolean?
            #:thresh (>/c 0)
            #:max-iters exact-positive-integer?)
        glmnet-path?)]))

;; A fitted Cox model. `coefficients` is a dense vector of length ni on the
;; original predictor scale, on the log relative-hazard scale -- there is no
;; intercept. `dev-ratio` is the fraction of null (partial-likelihood) deviance
;; explained; `lambda` is the penalty used; `num-passes` glmnet's pass count.
(struct cox-result (coefficients dev-ratio lambda num-passes)
  #:transparent)

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

(define (check-survival times statuses no who)
  (unless (= (length times) no)
    (error who "times length ~a does not match ~a observations" (length times) no))
  (unless (= (length statuses) no)
    (error who "statuses length ~a does not match ~a observations"
           (length statuses) no))
  (unless (for/or ([s (in-list statuses)]) (= s 1))
    (error who "at least one observation must be an event (status = 1)")))

;; --- public API ------------------------------------------------------------

(define (cox-fit X times statuses
                 #:lambda lambda
                 #:alpha [alpha 1.0]
                 #:standardize? [standardize? #t]
                 #:thresh [thresh 1e-7]
                 #:max-iters [max-iters 100000])
  (define-values (no ni) (rows->dims X 'cox-fit))
  (check-survival times statuses no 'cox-fit)
  (define xcol (matrix->colmajor X no ni))
  (define tv (response->f64vector times no 'cox-fit))
  (define sv (response->f64vector statuses no 'cox-fit))
  (define beta (make-f64vector ni 0.0))
  (define-values (dev-ratio lam nlp jerr)
    (glmnet-coxnet-solo/raw (exact->inexact alpha) no ni xcol tv sv
                            (exact->inexact lambda)
                            (if standardize? 1 0)
                            (exact->inexact thresh)
                            max-iters
                            beta))
  (check-cox-jerr jerr 'cox-fit)
  (cox-result
   (for/vector ([i (in-range ni)]) (f64vector-ref beta i))
   dev-ratio lam nlp))

;; --- prediction ------------------------------------------------------------

;; Log relative hazard x . beta for one predictor row (no intercept).
(define (lp-row result row)
  (define coefs (cox-result-coefficients result))
  (unless (= (length row) (vector-length coefs))
    (error 'cox-linear-predictor
           "row has ~a features, expected ~a" (length row) (vector-length coefs)))
  (for/fold ([acc 0.0])
            ([b (in-vector coefs)]
             [xj (in-list row)])
    (+ acc (* b (exact->inexact xj)))))

;; The log relative hazard (linear predictor) for each row of X.
(define (cox-linear-predictor result X)
  (for/list ([row (in-list X)]) (lp-row result row)))

;; The relative risk exp(x . beta) for each row of X -- the multiplicative effect
;; on the baseline hazard.
(define (cox-relative-risk result X)
  (for/list ([row (in-list X)]) (exp (lp-row result row))))

;; --- regularization path (#10) ---------------------------------------------

(define (cox-path X times statuses
                  #:lambda [lambda #f]
                  #:nlambda [nlambda 100]
                  #:lambda-min-ratio [lambda-min-ratio #f]
                  #:alpha [alpha 1.0]
                  #:standardize? [standardize? #t]
                  #:thresh [thresh 1e-7]
                  #:max-iters [max-iters 100000])
  (define-values (no ni) (rows->dims X 'cox-path))
  (check-survival times statuses no 'cox-path)
  (define tv (response->f64vector times no 'cox-path))
  (define sv (response->f64vector statuses no 'cox-path))
  (define xcol (matrix->colmajor X no ni))
  (define-values (nlam flmin ulam)
    (path-lambdas lambda nlambda lambda-min-ratio no ni))
  (define beta (make-f64vector (* ni nlam) 0.0))
  (define dev (make-f64vector nlam 0.0))
  (define alm (make-f64vector nlam 0.0))
  (define-values (lmu nlp jerr)
    (glmnet-coxnet-path/raw (exact->inexact alpha) no ni xcol tv sv
                            nlam flmin ulam
                            (if standardize? 1 0)
                            (exact->inexact thresh) max-iters
                            beta dev alm))
  (check-cox-jerr jerr 'cox-path)
  (define coefficients (unpack-columns beta ni lmu))
  (glmnet-path 'cox (finish-lambdas alm lmu (not lambda))
               #f coefficients (unpack-vector dev lmu)
               (count-nonzero coefficients) nlp))
