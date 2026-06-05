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
         "../foreign/raw/lognet.rkt")

(provide
 (struct-out logistic-result)
 (contract-out
  [logistic-fit
   (->* (matrix/c binary-response/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        logistic-result?)]
  [logistic-predict-proba
   (-> logistic-result? matrix/c (listof (real-in 0 1)))]
  [logistic-predict
   (->* (logistic-result? matrix/c) (#:threshold (real-in 0 1))
        (listof (or/c 0 1)))]))

;; A fitted two-class logistic model. `coefficients` is a vector of length ni on
;; the original predictor scale; `intercept` and the coefficients are on the
;; log-odds (logit) scale for class 1. `lambda` is the penalty actually used;
;; `dev-ratio` is the fraction of null deviance explained (the logistic analogue
;; of R^2); `num-passes` is glmnet's coordinate-descent pass count.
(struct logistic-result (intercept coefficients dev-ratio lambda num-passes)
  #:transparent)

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
  (define-values (no ni) (rows->dims X 'logistic-fit))
  (define xcol (matrix->colmajor X no ni))
  (define yv (response->f64vector y no 'logistic-fit))
  (define beta (make-f64vector ni 0.0))
  (define-values (intercept dev-ratio lam nlp jerr)
    (glmnet-lognet-solo/raw (exact->inexact alpha) no ni xcol yv
                            (exact->inexact lambda)
                            (if standardize? 1 0)
                            (if intercept? 1 0)
                            (exact->inexact thresh)
                            max-iters
                            beta))
  (check-logistic-jerr jerr 'logistic-fit)
  (logistic-result intercept
                   (for/vector ([i (in-range ni)]) (f64vector-ref beta i))
                   dev-ratio lam nlp))

;; --- prediction ------------------------------------------------------------

;; Linear predictor (log-odds of class 1) for one predictor row.
(define (logit-row result row ni)
  (unless (= (length row) ni)
    (error 'logistic-predict
           "row has ~a features, expected ~a" (length row) ni))
  (for/fold ([acc (logistic-result-intercept result)])
            ([b (in-vector (logistic-result-coefficients result))]
             [xj (in-list row)])
    (+ acc (* b (exact->inexact xj)))))

(define (sigmoid z) (/ 1.0 (+ 1.0 (exp (- z)))))

;; P(y = 1 | x) for each row of X.
(define (logistic-predict-proba result X)
  (define ni (vector-length (logistic-result-coefficients result)))
  (for/list ([row (in-list X)])
    (sigmoid (logit-row result row ni))))

;; Hard 0/1 prediction: class 1 when P(y=1) >= threshold (default 0.5).
(define (logistic-predict result X #:threshold [threshold 0.5])
  (for/list ([p (in-list (logistic-predict-proba result X))])
    (if (>= p threshold) 1 0)))
