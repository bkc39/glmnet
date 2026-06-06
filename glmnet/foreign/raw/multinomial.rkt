#lang racket/base

;; Raw FFI binding to the dense K-class multinomial "solo" fit
;; (fortran/glmnet_capi.f90 :: glmnet_multinomial_solo).
;;
;; Pure call. Inputs are SRFI-4 f64vectors (`x` column-major; `y` the integer
;; class labels 0..K-1 carried as doubles). Unlike the binary fitters, BOTH the
;; intercepts and the coefficients are caller-supplied output arrays: `intercept-out`
;; (length nc) and `beta-out` (length ni*nc, class-major) are filled in place. The
;; remaining scalar outputs come back via `values`.

(require ffi/unsafe
         ffi/vector
         "library.rkt")

(provide glmnet-multinomial-solo/raw)

;; void glmnet_multinomial_solo(double alpha, int no, int ni, int nc, double* x,
;;   double* y, double lambda, int standardize, int intercept, double thresh,
;;   int maxit, double* intercept_out, double* beta_out, double* dev_ratio_out,
;;   double* lambda_out, int* nlp_out, int* jerr_out);
;;
;; beta-out is class-major: class ic (0-based), predictor j lives at ic*ni + j.
;; intercept-out has length nc. Returns (values dev-ratio lambda nlp jerr);
;; intercept-out and beta-out are mutated in place.
(define-glmnet glmnet-multinomial-solo/raw
  (_fun (alpha no ni nc x y lambda standardize intercept thresh maxit intercept-out beta-out)
        ::
        (alpha         : _double)
        (no            : _int)
        (ni            : _int)
        (nc            : _int)
        (x             : _f64vector)
        (y             : _f64vector)
        (lambda        : _double)
        (standardize   : _int)
        (intercept     : _int)
        (thresh        : _double)
        (maxit         : _int)
        (intercept-out : _f64vector)
        (beta-out      : _f64vector)
        (dev-ratio-o   : (_ptr o _double))
        (lambda-o      : (_ptr o _double))
        (nlp-o         : (_ptr o _int))
        (jerr-o        : (_ptr o _int))
        -> _void
        -> (values dev-ratio-o lambda-o nlp-o jerr-o))
  #:c-id glmnet_multinomial_solo)
