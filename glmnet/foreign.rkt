#lang racket/base

;; Contracted, Racket-friendly wrappers over the raw FFI layer (foreign/raw/).
;; This is the seam the high-level API (main.rkt) and the examples build on.

(require racket/contract
         "foreign/raw/capi.rkt")

(provide
 (contract-out
  ;; Native-library self-check surface.
  [glmnet-capi-abi-version   (-> exact-positive-integer?)]
  [glmnet-default-real-bytes (-> exact-positive-integer?)]))

;; Load-time precision guard. The vendored glmnet Fortran declares its arrays as
;; single `real`; the whole elnet ABI is double precision only because the
;; native library is built with -fdefault-real-8. If that flag was dropped,
;; every numeric result would be silently wrong, so fail loudly here instead.
(let ([bytes (glmnet-default-real-bytes)])
  (unless (= bytes 8)
    (error 'glmnet
           (string-append
            "native library libglmnetcompat was built without -fdefault-real-8 "
            "(default Fortran real = ~a bytes, need 8); rebuild the native "
            "library -- see fortran/CMakeLists.txt")
           bytes)))
