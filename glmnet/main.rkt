#lang racket/base

;; Public API for the `glmnet` collection: `(require glmnet)`.
;;
;; As models land (OLS, ridge, lasso, elastic-net) their fitting procedures and
;; result accessors are re-exported here from foreign.rkt / core modules. For
;; now this surfaces the Phase 0 connectivity checks.

(require "foreign.rkt")

(provide (all-from-out "foreign.rkt"))
