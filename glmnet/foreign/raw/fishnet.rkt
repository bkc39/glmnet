#lang racket/base

;; Raw FFI binding to the dense Poisson "solo" fit
;; (fortran/glmnet_capi.f90 :: glmnet_fishnet_solo).
;;
;; Pure call, the same shape as the Gaussian/binomial fitters: `x` column-major,
;; `y` the non-negative counts; `beta-out` a caller-supplied f64vector of length
;; ni filled in place, and the scalar outputs (intercept, dev-ratio, lambda, nlp,
;; jerr) come back via `values`.

(require ffi/unsafe
         ffi/vector
         "library.rkt")

(provide glmnet-fishnet-solo/raw)

;; void glmnet_fishnet_solo(double alpha, int no, int ni, double* x, double* y,
;;   double lambda, int standardize, int intercept, double thresh, int maxit,
;;   double* intercept_out, double* beta_out, double* dev_ratio_out,
;;   double* lambda_out, int* nlp_out, int* jerr_out);
;;
;; Returns (values intercept dev-ratio lambda nlp jerr); beta-out is mutated in
;; place. dev-ratio is the fraction of null deviance explained (Poisson "R^2").
(define-glmnet glmnet-fishnet-solo/raw
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
  #:c-id glmnet_fishnet_solo)
