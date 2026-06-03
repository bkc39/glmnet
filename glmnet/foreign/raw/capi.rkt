#lang racket/base

;; Raw FFI bindings to the shim's connectivity / self-check entry points
;; (fortran/glmnet_capi.f90). These prove the gfortran -> C-ABI -> ffi-lib
;; pipeline end to end and pin the -fdefault-real-8 precision contract.
;;
;; Model entry points (elnet & friends) live alongside this file in elnet.rkt.

(require ffi/unsafe
         "library.rkt")

(provide (all-defined-out))

;; int glmnet_capi_abi_version(void);
(define-glmnet glmnet-capi-abi-version
  (_fun -> _int))

;; int glmnet_default_real_bytes(void);  -- MUST be 8 (see library precision contract)
(define-glmnet glmnet-default-real-bytes
  (_fun -> _int))
