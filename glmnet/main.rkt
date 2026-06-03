#lang racket/base

;; Public API for the `glmnet` collection: `(require glmnet)`.
;;
;; The four core Gaussian models (OLS, ridge, lasso, elastic net) are all
;; `elnet-fit` with different #:alpha / #:lambda; convenience wrappers name the
;; common cases. `foreign.rkt` also re-exports the Phase 0 connectivity checks.

(require "core/elnet.rkt"
         "foreign.rkt")

(provide (all-from-out "core/elnet.rkt")
         (all-from-out "foreign.rkt"))
