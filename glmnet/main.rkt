#lang racket/base

;; Public API for the `glmnet` collection: `(require glmnet)`.
;;
;; The four core Gaussian models (OLS, ridge, lasso, elastic net) are all
;; `elnet-fit` with different #:alpha / #:lambda; convenience wrappers name the
;; common cases. The binomial family adds `logistic-fit` (two-class logistic
;; elastic net) and the multinomial family `multinomial-fit` (K-class), each with
;; prediction helpers. `foreign.rkt` also re-exports the Phase 0 connectivity
;; checks.

(require "core/elnet.rkt"
         "core/lognet.rkt"
         "core/multinomial.rkt"
         "foreign.rkt")

(provide (all-from-out "core/elnet.rkt")
         (all-from-out "core/lognet.rkt")
         (all-from-out "core/multinomial.rkt")
         (all-from-out "foreign.rkt"))
