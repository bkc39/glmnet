#lang racket/base

;; Raw FFI binding to the dense Gaussian elastic-net "solo" fit
;; (fortran/glmnet_capi.f90 :: glmnet_elnet_solo).
;;
;; This is a pure call -- no C-owned handle, no finalizer. Inputs are passed as
;; SRFI-4 f64vectors (non-moving, malloc-backed); `beta-out` is a caller-supplied
;; f64vector of length ni that the call fills in place, and the scalar outputs
;; come back via `values`.

(require ffi/unsafe
         ffi/vector
         "library.rkt")

(provide glmnet-elnet-solo/raw)

;; void glmnet_elnet_solo(double alpha, int no, int ni, double* x, double* y,
;;   double lambda, int standardize, int intercept, double thresh, int maxit,
;;   double* intercept_out, double* beta_out, double* rsq_out,
;;   double* lambda_out, int* nlp_out, int* jerr_out);
;;
;; x is column-major, length no*ni. Returns (values intercept rsq lambda nlp jerr);
;; beta-out is mutated in place.
(define-glmnet glmnet-elnet-solo/raw
  (_fun (alpha no ni x y lambda standardize intercept thresh maxit beta-out)
        ::
        (alpha       : _double)
        (no          : _int)
        (ni          : _int)
        (x           : _f64vector)
        (y           : _f64vector)
        (lambda      : _double)
        (standardize : _int)
        (intercept   : _int)
        (thresh      : _double)
        (maxit       : _int)
        (intercept-o : (_ptr o _double))
        (beta-out    : _f64vector)
        (rsq-o       : (_ptr o _double))
        (lambda-o    : (_ptr o _double))
        (nlp-o       : (_ptr o _int))
        (jerr-o      : (_ptr o _int))
        -> _void
        -> (values intercept-o rsq-o lambda-o nlp-o jerr-o))
  #:c-id glmnet_elnet_solo)
