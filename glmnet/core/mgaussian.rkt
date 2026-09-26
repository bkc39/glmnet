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
         "../foreign/raw/mgaussian.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "../foreign/raw/path.rkt")

(provide
 (struct-out mgaussian-result)
 (contract-out
  [mgaussian-fit
   (->* (matrix/c matrix/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        mgaussian-result?)]
  [mgaussian-predict
   (-> mgaussian-result? matrix/c (listof (listof real?)))]))

(provide
 (contract-out
  [mgaussian-path
   (->* (matrix/c matrix/c)
           (#:lambda lambda-sequence/c
            #:nlambda exact-positive-integer?
            #:lambda-min-ratio lambda-min-ratio/c
            #:alpha (real-in 0 1)
            #:standardize? boolean?
            #:intercept? boolean?
            #:thresh (>/c 0)
            #:max-iters exact-positive-integer?)
        glmnet-path?)]))

;; A fitted multi-response Gaussian model. `intercepts` is a vector of nr reals;
;; `coefficients` is a vector of nr coefficient vectors (each length ni), one per
;; response, on the original predictor scale. `r-squared` is the fraction of
;; (multi-response) variance explained; `lambda` the penalty used; `num-passes`
;; glmnet's pass count.
(struct mgaussian-result (intercepts coefficients r-squared lambda num-passes)
  #:transparent)

;; Validate the response matrix Y against the observation count; return nr.
(define (response-dims Y no who)
  (unless (= (length Y) no)
    (error who "response matrix has ~a rows, expected ~a" (length Y) no))
  (define nr (length (car Y)))
  (when (zero? nr)
    (error who "response rows must be non-empty"))
  (unless (andmap (lambda (r) (= (length r) nr)) Y)
    (error who "all response rows must have the same length (got ragged responses)"))
  nr)

;; --- public API ------------------------------------------------------------

(define (mgaussian-fit X Y
                       #:lambda lambda
                       #:alpha [alpha 1.0]
                       #:standardize? [standardize? #t]
                       #:intercept? [intercept? #t]
                       #:thresh [thresh 1e-7]
                       #:max-iters [max-iters 100000])
  (define-values (no ni) (rows->dims X 'mgaussian-fit))
  (define nr (response-dims Y no 'mgaussian-fit))
  (define xcol (matrix->colmajor X no ni))
  (define ycol (matrix->colmajor Y no nr))
  (define intercepts (make-f64vector nr 0.0))
  (define beta (make-f64vector (* ni nr) 0.0))
  (define-values (r-squared lam nlp jerr)
    (glmnet-mgaussian-solo/raw (exact->inexact alpha) no ni nr xcol ycol
                               (exact->inexact lambda)
                               (if standardize? 1 0)
                               (if intercept? 1 0)
                               (exact->inexact thresh)
                               max-iters
                               intercepts beta))
  (check-jerr jerr 'mgaussian-fit)
  (mgaussian-result
   (for/vector ([r (in-range nr)]) (f64vector-ref intercepts r))
   ;; beta is response-major: response r's predictor j at r*ni + j.
   (for/vector ([r (in-range nr)])
     (for/vector ([j (in-range ni)]) (f64vector-ref beta (+ (* r ni) j))))
   r-squared lam nlp))

;; --- prediction ------------------------------------------------------------

;; The nr linear predictions a0_r + x . beta_r for one predictor row.
(define (predict-row result row)
  (define intercepts (mgaussian-result-intercepts result))
  (define coefs (mgaussian-result-coefficients result))
  (define ni (vector-length (vector-ref coefs 0)))
  (unless (= (length row) ni)
    (error 'mgaussian-predict
           "row has ~a features, expected ~a" (length row) ni))
  (for/list ([r (in-range (vector-length intercepts))])
    (for/fold ([acc (vector-ref intercepts r)])
              ([b (in-vector (vector-ref coefs r))]
               [xj (in-list row)])
      (+ acc (* b (exact->inexact xj))))))

;; The per-response predictions for each row of X (one inner list per row, nr
;; entries each).
(define (mgaussian-predict result X)
  (for/list ([row (in-list X)]) (predict-row result row)))

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
  (define-values (no ni) (rows->dims X 'mgaussian-path))
  (define k (response-dims Y no 'mgaussian-path))
  (define yv (matrix->colmajor Y no k))
  (define xcol (matrix->colmajor X no ni))
  (define-values (nlam flmin ulam)
    (path-lambdas lambda nlambda lambda-min-ratio no ni))
  (define a0 (make-f64vector (* k nlam) 0.0))
  (define beta (make-f64vector (* ni k nlam) 0.0))
  (define dev (make-f64vector nlam 0.0))
  (define alm (make-f64vector nlam 0.0))
  (define-values (lmu nlp jerr)
    (glmnet-mgaussian-path/raw (exact->inexact alpha) no ni k xcol yv
                               nlam flmin ulam
                               (if standardize? 1 0) (if intercept? 1 0)
                               (exact->inexact thresh) max-iters
                               a0 beta dev alm))
  (check-jerr jerr 'mgaussian-path)
  (define coefficients (unpack-column-groups beta ni k lmu))
  (glmnet-path 'mgaussian (finish-lambdas alm lmu (not lambda))
               (unpack-intercept-groups a0 k lmu) coefficients (unpack-vector dev lmu)
               (count-nonzero-groups coefficients) nlp))
