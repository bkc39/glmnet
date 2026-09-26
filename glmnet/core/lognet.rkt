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
         "model.rkt"
         (submod "model.rkt" support)
         "../foreign/raw/lognet.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "../foreign/raw/path.rkt")

(provide
 (struct-out logistic-result)
 (contract-out
  [logistic-fit
   (->* (design-matrix/c binary-response/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        logistic-result?)]
  [logistic-predict-proba
   (-> logistic-result? design-matrix/c (listof (real-in 0 1)))]
  [logistic-predict
   (->* (logistic-result? design-matrix/c) (#:threshold (real-in 0 1))
        (listof (or/c 0 1)))]))

(provide
 (contract-out
  [logistic-path
   (->* (design-matrix/c binary-response/c)
           (#:lambda lambda-sequence/c
            #:nlambda exact-positive-integer?
            #:lambda-min-ratio lambda-min-ratio/c
            #:alpha (real-in 0 1)
            #:standardize? boolean?
            #:intercept? boolean?
            #:thresh (>/c 0)
            #:max-iters exact-positive-integer?)
        glmnet-path?)]))

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

;; --- input contract --------------------------------------------------------

;; The response is a non-empty list of 0/1 class labels (exact or inexact).
(define (binary-label? v) (and (real? v) (or (= v 0) (= v 1))))
(define binary-response/c (and/c (listof binary-label?) pair?))

;; Logistic adds the 8000/9000 (a class probability collapsed -- e.g. perfect
;; separation) and 90000 (coefficient-bound non-convergence) fatal codes on top
;; of the shared cases in `check-jerr`.
(define (check-logistic-jerr jerr who)
  (cond
    [(and (>= jerr 8000) (< jerr 9000))
     (error who
            (format (string-append
                     "a class probability collapsed (perfect separation or a "
                     "degenerate class); try a larger lambda (jerr=~a)")
                    jerr))]
    [(and (>= jerr 9000) (< jerr 10000))
     (error who (format "a class has a degenerate null probability (jerr=~a)" jerr))]
    [(= jerr 90000)
     (error who "coefficient-bound adjustment failed to converge (jerr=90000)")]
    [else (check-jerr jerr who)]))

;; --- public API ------------------------------------------------------------

(define (logistic-fit X y
                      #:lambda lambda
                      #:alpha [alpha 1.0]
                      #:standardize? [standardize? #t]
                      #:intercept? [intercept? #t]
                      #:thresh [thresh 1e-7]
                      #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'logistic-fit "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define yv (as-response y no 'logistic-fit "y"))
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
  (logistic-result intercept (unpack-vector beta ni) dev-ratio lam nlp))

;; --- prediction ------------------------------------------------------------

;; P(y = 1 | x) for each row of X.
(define (logistic-predict-proba result X)
  (predict-as 'logistic-predict-proba result X 'response))

;; Hard 0/1 prediction: class 1 when P(y=1) >= threshold (default 0.5).
(define (logistic-predict result X #:threshold [threshold 0.5])
  (for/list ([p (in-list (predict-as 'logistic-predict result X 'response))])
    (if (>= p threshold) 1 0)))

;; --- regularization path (#10) ---------------------------------------------

(define (logistic-path X y
                       #:lambda [lambda #f]
                       #:nlambda [nlambda 100]
                       #:lambda-min-ratio [lambda-min-ratio #f]
                       #:alpha [alpha 1.0]
                       #:standardize? [standardize? #t]
                       #:intercept? [intercept? #t]
                       #:thresh [thresh 1e-7]
                       #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'logistic-path "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define yv (as-response y no 'logistic-path "y"))
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
  (check-logistic-jerr jerr 'logistic-path)
  (define coefficients (unpack-columns beta ni lmu))
  (glmnet-path 'binomial (finish-lambdas alm lmu (not lambda))
               (unpack-vector a0 lmu) coefficients (unpack-vector dev lmu)
               (count-nonzero coefficients) nlp))
