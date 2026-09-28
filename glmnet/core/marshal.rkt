#lang racket/base

;; Shared marshalling between the row-major Racket front end and the
;; column-major, double-precision Fortran ABI, plus the jerr handling common to
;; every model family. core/elnet.rkt (Gaussian) and core/lognet.rkt (binomial)
;; build on this; each family layers its own family-specific jerr cases on top
;; of `check-jerr`.

(require racket/contract
         ffi/vector)

(provide matrix/c response/c
         rows->dims matrix->colmajor response->f64vector
         check-response-varies
         check-jerr)

;; --- input contracts -------------------------------------------------------

(define matrix/c (and/c (listof (listof real?)) pair?))
(define response/c (and/c (listof real?) pair?))

;; --- shape + marshalling ---------------------------------------------------

(define (rows->dims X who)
  (define no (length X))
  (define ni (length (car X)))
  (when (zero? ni)
    (error who "predictor rows must be non-empty"))
  (unless (andmap (lambda (r) (= (length r) ni)) X)
    (error who "all predictor rows must have the same length (got ragged rows)"))
  (values no ni))

;; Pack the row-major matrix X (list of rows) into the column-major f64vector
;; the Fortran expects: element (i, j) lives at index i + j*no.
(define (matrix->colmajor X no ni)
  (define v (make-f64vector (* no ni)))
  (for ([row (in-list X)]
        [i (in-naturals)])
    (for ([xij (in-list row)]
          [j (in-naturals)])
      (f64vector-set! v (+ i (* j no)) (exact->inexact xij))))
  v)

(define (response->f64vector y no who)
  (unless (= (length y) no)
    (error who "response length ~a does not match ~a observations" (length y) no))
  (list->f64vector (map exact->inexact y)))

;; R's Gaussian wrapper stops when the null deviance is zero: the response is
;; constant or, without an intercept, all zero. `columns` holds one response per
;; list; for mgaussian that is every column of Y, all of which must be constant
;; for the null deviance to be zero.
(define (check-response-varies columns intercept? who)
  (when (for/and ([ys (in-list columns)])
          (define center (if intercept? (car ys) 0))
          (for/and ([v (in-list ys)]) (= v center)))
    (error who "y is constant; gaussian glmnet fails at standardization step")))

;; --- error handling --------------------------------------------------------

(define-logger glmnet)

;; Map glmnet's jerr flag (documented in R glmnet's R/jerr.R) to a Racket
;; error. Handles the codes shared across model families; family-specific
;; positive codes (e.g. the logistic 8000/9000 range) are intercepted by the
;; caller before delegating here.
;;
;; A negative jerr is not fatal: glmnet stopped at one lambda and kept the ones
;; before it. `lmu` is the number of lambdas fitted, or #f for a single fit,
;; which has fitted its one lambda unless jerr < 0. With no lambda fitted this
;; raises (R warns and returns an empty model); a path that kept some is
;; truncated and logs a warning, as R warns.
(define (check-jerr jerr who [lmu #f])
  (define fitted (or lmu (if (negative? jerr) 0 1)))
  (cond
    [(positive? jerr)
     (error who
            (cond
              [(= jerr 7777) "all used predictors have zero variance"]
              [(= jerr 10000) "no penalized predictors (penalty factors are all <= 0)"]
              [(< jerr 7777) (format "glmnet memory allocation error (jerr=~a)" jerr)]
              [else (format "glmnet fatal error (jerr=~a)" jerr)]))]
    [(zero? fitted)
     (error who "glmnet fitted no lambda: ~a" (stop-reason jerr))]
    [(negative? jerr)
     (log-glmnet-warning "~a: the path stops after ~a ~a: ~a"
                         who fitted (if (= fitted 1) "lambda" "lambdas") (stop-reason jerr))]
    [else (void)]))

;; glmnet's reason for stopping, from a non-fatal jerr: -m, or -(code + m) for
;; codes 10000, 20000 and 30000, at the m-th lambda (R glmnet's R/jerr.R).
(define (stop-reason jerr)
  (define n (- jerr))
  (define (at m)
    (if (= m 1) "the first lambda" (format "lambda number ~a" m)))
  (cond
    [(zero? n) "glmnet returned no lambdas"]
    [(< n 10000)
     (format (string-append "convergence was not reached at ~a within #:max-iters passes; "
                            "try a larger #:max-iters (jerr=~a)")
             (at n) jerr)]
    [(< n 20000)
     (format "the number of nonzero coefficients exceeded its maximum at ~a (jerr=~a)"
             (at (- n 10000)) jerr)]
    [(< n 30000)
     (format "the fitted probabilities saturated at ~a (jerr=~a)" (at (- n 20000)) jerr)]
    [else
     (format "a numerical error occurred at ~a (jerr=~a)" (at (- n 30000)) jerr)]))
