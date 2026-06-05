#lang racket/base

;; Raw FFI binding to the dense two-class logistic (binomial) "solo" fit
;; (fortran/glmnet_capi.f90 :: glmnet_lognet_solo).
;;
;; Like `glmnet-elnet-solo/raw` this is a pure call -- no C-owned handle, no
;; finalizer. Inputs are SRFI-4 f64vectors (`x` column-major, `y` the 0/1 class
;; labels); `beta-out` is a caller-supplied f64vector of length ni that the call
;; fills in place, and the scalar outputs come back via `values`.

(require ffi/unsafe
         ffi/vector
         "library.rkt")

(provide glmnet-lognet-solo/raw)

;; void glmnet_lognet_solo(double alpha, int no, int ni, double* x, double* y,
;;   double lambda, int standardize, int intercept, double thresh, int maxit,
;;   double* intercept_out, double* beta_out, double* dev_ratio_out,
;;   double* lambda_out, int* nlp_out, int* jerr_out);
;;
;; x is column-major, length no*ni; y is the 0/1 label vector, length no.
;; Returns (values intercept dev-ratio lambda nlp jerr); beta-out is mutated in
;; place. dev-ratio is the fraction of null deviance explained (logistic "R^2").
(define-glmnet glmnet-lognet-solo/raw
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
        (dev-ratio-o : (_ptr o _double))
        (lambda-o    : (_ptr o _double))
        (nlp-o       : (_ptr o _int))
        (jerr-o      : (_ptr o _int))
        -> _void
        -> (values intercept-o dev-ratio-o lambda-o nlp-o jerr-o))
  #:c-id glmnet_lognet_solo)
