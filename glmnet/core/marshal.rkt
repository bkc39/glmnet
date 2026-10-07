#lang racket/base

;; Shared plumbing between the Racket front end and the column-major,
;; double-precision Fortran ABI, used by every model family: the input
;; contracts, the design-matrix layer's entry points for fitters (data.rkt), the
;; linear predictor the prediction helpers evaluate, and the jerr handling
;; common to every family. Each family layers its own family-specific jerr cases
;; on top of `check-jerr`.

(require racket/contract
         racket/flonum
         ffi/vector
         (only-in "../data.rkt" design-matrix/c response/c)
         (submod "../data.rkt" support)
         (only-in (submod "input.rkt" support) unnamed->design-matrix))

(provide design-matrix/c response/c
         design-matrix-data design-matrix-nrows design-matrix-ncols
         as-design-matrix as-response response->vector
         prediction-matrix linear-predictor
         check-response-varies
         check-jerr
         log-fit-warning
         current-warning-who
         current-warning-what)

;; --- responses ---------------------------------------------------------------

;; The entries of a response the fitter has converted with `as-response`, as a
;; vector, which cross-validation selects folds from.
(define (response->vector yv)
  (for/vector #:length (f64vector-length yv) ([k (in-range (f64vector-length yv))])
    (f64vector-ref yv k)))

;; --- prediction ------------------------------------------------------------

;; The new-data argument of a prediction helper, unnamed data, as a design
;; matrix with one column per coefficient.
(define (prediction-matrix X ni who)
  (define x (unnamed->design-matrix who "X" X))
  (define no (design-matrix-nrows x))
  (define nc (design-matrix-ncols x))
  (cond
    [(= nc ni) x]
    [(= no ni)
     (raise-arguments-error who "X does not have one column per coefficient"
                            "columns of X" nc "coefficients" ni "rows of X" no
                            "hint" (unquoted-printing-string
                                    "rows are observations; is X transposed?"))]
    [else
     (raise-arguments-error who "X does not have one column per coefficient"
                            "columns of X" nc "coefficients" ni)]))

;; intercept + x_i . beta for row i of the design matrix x.
(define (linear-predictor x i intercept beta)
  (define v (design-matrix-data x))
  (define no (design-matrix-nrows x))
  (for/fold ([acc intercept])
            ([b (in-vector beta)]
             [j (in-naturals)])
    (+ acc (* b (f64vector-ref v (+ i (* j no)))))))

;; R's Gaussian wrapper stops when the null deviance is zero: the response is
;; constant or, without an intercept, all zero. `data` holds ncols responses of
;; nrows entries, column-major, as converted for the Fortran; for mgaussian
;; every column of Y must be constant for the null deviance to be zero.
(define (check-response-varies data nrows ncols intercept? who)
  (when (for/and ([j (in-range ncols)])
          (define start (* j nrows))
          (define center (if intercept? (f64vector-ref data start) 0.0))
          (for/and ([i (in-range nrows)]) (fl= (f64vector-ref data (+ start i)) center)))
    (error who "y is constant; gaussian glmnet fails at standardization step")))

;; --- error handling --------------------------------------------------------

(define-logger glmnet)

;; The procedure the user called and what it was fitting, when that is not the
;; path fitter that logs a warning: a *-cv procedure (cv.rkt's `fitting`) or a
;; formula procedure (formula.rkt's `as-formula-procedure`).
(define current-warning-who (make-parameter #f))
(define current-warning-what (make-parameter #f))

;; Logs (format fmt arg ...) as a warning on the glmnet topic, in the name of
;; the procedure the user called; nothing is formatted unless a receiver listens.
(define (log-fit-warning who fmt . args)
  (log-glmnet-warning "~a: ~a" (or (current-warning-who) who) (apply format fmt args)))

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
     (log-fit-warning who "~athe path stops after ~a ~a: ~a"
                      (if (current-warning-what) (format "~a: " (current-warning-what)) "")
                      fitted (if (= fitted 1) "lambda" "lambdas") (stop-reason jerr))]
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
