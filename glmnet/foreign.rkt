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

;; ABI guard. The native library must export the current set of C entry points
;; (all six families). A prebuilt platform candidate that predates a family would
;; otherwise fail with a cryptic missing-symbol error on first use of that family;
;; fail loudly here instead. Bump in lockstep with glmnet_capi_abi_version
;; (fortran/glmnet_capi.f90).
(let ([abi (glmnet-capi-abi-version)])
  (unless (= abi 2)
    (error 'glmnet
           (string-append
            "native library libglmnetcompat is ABI version ~a, but this package "
            "expects 2 -- the loaded shared object is stale (likely a committed "
            "platform candidate that predates the current families). Rebuild it "
            "with scripts/build-so.sh <platform> or `nix run .#copy-native-libs`.")
           abi)))
