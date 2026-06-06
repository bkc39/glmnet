#lang racket/base

;; Public API for the `glmnet` collection: `(require glmnet)`.
;;
;; The four core Gaussian models (OLS, ridge, lasso, elastic net) are all
;; `elnet-fit` with different #:alpha / #:lambda; convenience wrappers name the
;; common cases. The other families add `logistic-fit` (two-class), `multinomial-fit`
;; (K-class), `cox-fit` (proportional-hazards survival), and `poisson-fit` (counts,
;; log link), each with prediction helpers. `foreign.rkt` also re-exports the
;; Phase 0 connectivity checks.

(require "core/elnet.rkt"
         "core/lognet.rkt"
         "core/multinomial.rkt"
         "core/cox.rkt"
         "core/poisson.rkt"
         "foreign.rkt")

(provide (all-from-out "core/elnet.rkt")
         (all-from-out "core/lognet.rkt")
         (all-from-out "core/multinomial.rkt")
         (all-from-out "core/cox.rkt")
         (all-from-out "core/poisson.rkt")
         (all-from-out "foreign.rkt"))
