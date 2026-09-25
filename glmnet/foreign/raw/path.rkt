#lang racket/base

;; Raw FFI bindings to the regularization-path entry points
;; (fortran/glmnet_capi.f90 :: glmnet_<family>_path).
;;
;; Pure calls, no C-owned handles. Every per-lambda output is a caller-supplied
;; f64vector sized for `nlam` lambdas and filled in place; only the first `lmu`
;; entries (lmu comes back via `values`) are meaningful. Matrices are
;; column-major: for coefficients, predictor j of lambda m lives at j + ni*m, and
;; for the K-column families at j + ni*(k + K*m). Integer scalars go by reference
;; (see the path section of glmnet_capi.f90 for the arm64 macOS reason).

(require ffi/unsafe
         ffi/vector
         "library.rkt")

(provide glmnet-elnet-path/raw
         glmnet-lognet-path/raw
         glmnet-multinomial-path/raw
         glmnet-coxnet-path/raw
         glmnet-fishnet-path/raw
         glmnet-mgaussian-path/raw)

;; Gaussian, binomial and Poisson share one shape: one intercept and one
;; coefficient column per lambda. Returns (values lmu nlp jerr).
(define-syntax-rule (define-single-response-path name c-id)
  (define-glmnet name
    (_fun (alpha no ni x y nlam flmin ulam standardize intercept thresh maxit
                 intercept-out beta-out dev-out lambda-out)
          ::
          (alpha         : _double)
          (no            : (_ptr i _int))
          (ni            : (_ptr i _int))
          (x             : _f64vector)
          (y             : _f64vector)
          (nlam          : (_ptr i _int))
          (flmin         : _double)
          (ulam          : _f64vector)
          (standardize   : (_ptr i _int))
          (intercept     : (_ptr i _int))
          (thresh        : _double)
          (maxit         : (_ptr i _int))
          (lmu-o         : (_ptr o _int))
          (intercept-out : _f64vector)
          (beta-out      : _f64vector)
          (dev-out       : _f64vector)
          (lambda-out    : _f64vector)
          (nlp-o         : (_ptr o _int))
          (jerr-o        : (_ptr o _int))
          -> _void
          -> (values lmu-o nlp-o jerr-o))
    #:c-id c-id))

(define-single-response-path glmnet-elnet-path/raw glmnet_elnet_path)
(define-single-response-path glmnet-lognet-path/raw glmnet_lognet_path)
(define-single-response-path glmnet-fishnet-path/raw glmnet_fishnet_path)

;; Multinomial and multi-response Gaussian: K intercepts and K coefficient
;; columns per lambda (K = classes or responses). y is the K-column response for
;; mgaussian, the 0..K-1 labels for multinomial. Returns (values lmu nlp jerr).
(define-syntax-rule (define-multi-response-path name c-id)
  (define-glmnet name
    (_fun (alpha no ni k x y nlam flmin ulam standardize intercept thresh maxit
                 intercept-out beta-out dev-out lambda-out)
          ::
          (alpha         : _double)
          (no            : (_ptr i _int))
          (ni            : (_ptr i _int))
          (k             : (_ptr i _int))
          (x             : _f64vector)
          (y             : _f64vector)
          (nlam          : (_ptr i _int))
          (flmin         : _double)
          (ulam          : _f64vector)
          (standardize   : (_ptr i _int))
          (intercept     : (_ptr i _int))
          (thresh        : _double)
          (maxit         : (_ptr i _int))
          (lmu-o         : (_ptr o _int))
          (intercept-out : _f64vector)
          (beta-out      : _f64vector)
          (dev-out       : _f64vector)
          (lambda-out    : _f64vector)
          (nlp-o         : (_ptr o _int))
          (jerr-o        : (_ptr o _int))
          -> _void
          -> (values lmu-o nlp-o jerr-o))
    #:c-id c-id))

(define-multi-response-path glmnet-multinomial-path/raw glmnet_multinomial_path)
(define-multi-response-path glmnet-mgaussian-path/raw glmnet_mgaussian_path)

;; Cox: no intercept, and the response is a time plus a status. Returns
;; (values lmu nlp jerr).
(define-glmnet glmnet-coxnet-path/raw
  (_fun (alpha no ni x time status nlam flmin ulam standardize thresh maxit
               beta-out dev-out lambda-out)
        ::
        (alpha       : _double)
        (no          : (_ptr i _int))
        (ni          : (_ptr i _int))
        (x           : _f64vector)
        (time        : _f64vector)
        (status      : _f64vector)
        (nlam        : (_ptr i _int))
        (flmin       : _double)
        (ulam        : _f64vector)
        (standardize : (_ptr i _int))
        (thresh      : _double)
        (maxit       : (_ptr i _int))
        (lmu-o       : (_ptr o _int))
        (beta-out    : _f64vector)
        (dev-out     : _f64vector)
        (lambda-out  : _f64vector)
        (nlp-o       : (_ptr o _int))
        (jerr-o      : (_ptr o _int))
        -> _void
        -> (values lmu-o nlp-o jerr-o))
  #:c-id glmnet_coxnet_path)
