#lang racket/base

;; Shared plumbing between the Racket front end and the column-major,
;; double-precision Fortran ABI, used by every model family: the input
;; contracts, the design-matrix layer's entry points for fitters (data.rkt), the
;; linear predictor the prediction helpers evaluate, and the jerr handling
;; common to every family. Each family layers its own family-specific jerr cases
;; on top of `check-jerr`.

(require racket/contract
         ffi/vector
         (only-in "../data.rkt" design-matrix/c)
         (submod "../data.rkt" support))

(provide design-matrix/c response/c
         design-matrix-data design-matrix-nrows design-matrix-ncols
         as-design-matrix as-response
         prediction-matrix linear-predictor
         check-jerr)

;; --- input contracts -------------------------------------------------------

(define response/c (and/c (listof real?) pair?))

;; --- prediction ------------------------------------------------------------

;; The new-data argument of a prediction helper, as a design matrix with one
;; column per coefficient.
(define (prediction-matrix X ni who)
  (define x (as-design-matrix X who "X"))
  (unless (= (design-matrix-ncols x) ni)
    (raise-arguments-error who "X does not have one column per coefficient"
                           "columns of X" (design-matrix-ncols x) "coefficients" ni))
  x)

;; intercept + x_i . beta for row i of the design matrix x.
(define (linear-predictor x i intercept beta)
  (define v (design-matrix-data x))
  (define no (design-matrix-nrows x))
  (for/fold ([acc intercept])
            ([b (in-vector beta)]
             [j (in-naturals)])
    (+ acc (* b (f64vector-ref v (+ i (* j no)))))))

;; --- error handling --------------------------------------------------------

;; Map glmnet's jerr flag (documented in R glmnet's R/jerr.R) to a Racket
;; error (fatal) or a logged warning (non-fatal partial result). Handles the
;; codes shared across model families; family-specific positive codes (e.g. the
;; logistic 8000/9000 range) are intercepted by the caller before delegating
;; here.
(define (check-jerr jerr who)
  (cond
    [(zero? jerr) (void)]
    [(positive? jerr)
     (error who
            (cond
              [(= jerr 7777) "all used predictors have zero variance"]
              [(= jerr 10000) "no penalized predictors (penalty factors are all <= 0)"]
              [(< jerr 7777) (format "glmnet memory allocation error (jerr=~a)" jerr)]
              [else (format "glmnet fatal error (jerr=~a)" jerr)]))]
    [else
     (log-warning
      "~a: glmnet did not fully converge for this lambda (jerr=~a); coefficients are partial"
      who jerr)]))
