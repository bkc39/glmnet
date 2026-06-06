#lang racket/base

;; Raw FFI binding to the dense Cox proportional-hazards "solo" fit
;; (fortran/glmnet_capi.f90 :: glmnet_coxnet_solo).
;;
;; Pure call, and -- unlike the other families -- there is NO intercept (Cox
;; absorbs it into the unspecified baseline hazard). Inputs are SRFI-4 f64vectors
;; (`x` column-major; `time` and `status` length no). `beta-out` is a
;; caller-supplied f64vector of length ni filled in place; the scalar outputs come
;; back via `values`.

(require ffi/unsafe
         ffi/vector
         "library.rkt")

(provide glmnet-coxnet-solo/raw)

;; void glmnet_coxnet_solo(double alpha, int no, int ni, double* x, double* time,
;;   double* status, double lambda, int standardize, double thresh, int maxit,
;;   double* beta_out, double* dev_ratio_out, double* lambda_out, int* nlp_out,
;;   int* jerr_out);
;;
;; time is the follow-up time, status the 0/1 event indicator. Returns
;; (values dev-ratio lambda nlp jerr); beta-out is mutated in place.
(define-glmnet glmnet-coxnet-solo/raw
  (_fun (alpha no ni x time status lambda standardize thresh maxit beta-out)
        ::
        (alpha       : _double)
        (no          : _int)
        (ni          : _int)
        (x           : _f64vector)
        (time        : _f64vector)
        (status      : _f64vector)
        (lambda      : _double)
        (standardize : _int)
        (thresh      : _double)
        (maxit       : _int)
        (beta-out    : _f64vector)
        (dev-ratio-o : (_ptr o _double))
        (lambda-o    : (_ptr o _double))
        (nlp-o       : (_ptr o _int))
        (jerr-o      : (_ptr o _int))
        -> _void
        -> (values dev-ratio-o lambda-o nlp-o jerr-o))
  #:c-id glmnet_coxnet_solo)
