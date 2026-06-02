#lang racket/base

;; Native library handle and the FFI definer shared by the raw layer.
;;
;; libglmnetcompat is our C-ABI shim (fortran/glmnet_capi.f90) statically linked
;; with the vendored glmnet Fortran (fortran/vendor/glmnet5.f90). It exports
;; clean bind(C) symbols, so `define-glmnet` maps hyphenated Racket names to the
;; underscored C names (glmnet-hello -> glmnet_hello).
;;
;; `native-libs-dir` is resolved relative to this file, which lives at
;; glmnet/foreign/raw/ -- two directories below the collection root -- so the
;; path to glmnet/native-libs/ climbs two levels. In Nix builds
;; GLMNET_NATIVE_LIB_PATH points at the native derivation, whose library lives
;; under lib/.

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/define/conventions
         racket/runtime-path)

(provide define-glmnet)

(define-runtime-path native-libs-dir "../../native-libs")

(define (glmnet-lib-dir)
  (define env (getenv "GLMNET_NATIVE_LIB_PATH"))
  (if env (build-path env "lib") native-libs-dir))

(define libglmnet (ffi-lib (build-path (glmnet-lib-dir) "libglmnetcompat")))

(define-ffi-definer define-glmnet libglmnet
  #:make-c-id convention:hyphen->underscore)
