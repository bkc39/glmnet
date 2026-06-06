#lang racket/base

;; Raw FFI binding to the dense multi-response Gaussian "solo" fit
;; (fortran/glmnet_capi.f90 :: glmnet_mgaussian_solo).
;;
;; Pure call. Inputs are SRFI-4 f64vectors (`x` and `y` both column-major; `y` is
;; the no-by-nr response matrix). Both `intercept-out` (length nr) and `beta-out`
;; (length ni*nr, response-major) are caller-supplied output arrays filled in
;; place; the remaining scalar outputs come back via `values`.

(require ffi/unsafe
         ffi/vector
         "library.rkt")

(provide glmnet-mgaussian-solo/raw)

;; void glmnet_mgaussian_solo(double alpha, int no, int ni, int nr, double* x,
;;   double* y, double lambda, int standardize, int intercept, double thresh,
;;   int maxit, double* intercept_out, double* beta_out, double* rsq_out,
;;   double* lambda_out, int* nlp_out, int* jerr_out);
;;
;; beta-out is response-major: response r (0-based), predictor j at r*ni + j.
;; intercept-out has length nr. Returns (values rsq lambda nlp jerr);
;; intercept-out and beta-out are mutated in place.
(define-glmnet glmnet-mgaussian-solo/raw
  (_fun (alpha no ni nr x y lambda standardize intercept thresh maxit intercept-out beta-out)
        ::
        (alpha         : _double)
        (no            : _int)
        (ni            : _int)
        (nr            : _int)
        (x             : _f64vector)
        (y             : _f64vector)
        (lambda        : _double)
        (standardize   : _int)
        (intercept     : _int)
        (thresh        : _double)
        (maxit         : _int)
        (intercept-out : _f64vector)
        (beta-out      : _f64vector)
        (rsq-o         : (_ptr o _double))
        (lambda-o      : (_ptr o _double))
        (nlp-o         : (_ptr o _int))
        (jerr-o        : (_ptr o _int))
        -> _void
        -> (values rsq-o lambda-o nlp-o jerr-o))
  #:c-id glmnet_mgaussian_solo)
