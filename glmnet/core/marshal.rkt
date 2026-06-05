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

;; --- error handling --------------------------------------------------------

;; Map glmnet's jerr flag (documented in fortran/vendor/glmnet5.f90) to a Racket
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
