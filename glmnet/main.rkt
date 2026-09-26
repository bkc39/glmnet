#lang racket/base

;; Public API for the `glmnet` collection: `(require glmnet)`.
;;
;; The four core Gaussian models (OLS, ridge, lasso, elastic net) are all
;; `elnet-fit` with different #:alpha / #:lambda; convenience wrappers name the
;; common cases. The other families add `logistic-fit` (two-class), `multinomial-fit`
;; (K-class), `cox-fit` (proportional-hazards survival), `poisson-fit` (counts, log
;; link), and `mgaussian-fit` (multi-response Gaussian), each with prediction
;; helpers, a regularization path (`*-path`) and cross-validation (`*-cv`, whose
;; shared machinery is core/cv.rkt). Every result type implements
;; `gen:glmnet-model` (core/model.rkt), so `predict`, `coef` and
;; `deviance-ratio` work on any of them. core/formula.rkt is the formula front
;; end, which fits any family from a table by column name. `data.rkt` is the
;; design-matrix layer every fitter reads its input through, and `foreign.rkt`
;; also re-exports the Phase 0 connectivity checks.

(require "data.rkt"
         "core/elnet.rkt"
         "core/lognet.rkt"
         "core/multinomial.rkt"
         "core/cox.rkt"
         "core/poisson.rkt"
         "core/mgaussian.rkt"
         "core/path.rkt"
         "core/model.rkt"
         "core/cv.rkt"
         "core/formula.rkt"
         "foreign.rkt")

(provide (all-from-out "data.rkt")
         (all-from-out "core/elnet.rkt")
         (all-from-out "core/lognet.rkt")
         (all-from-out "core/multinomial.rkt")
         (all-from-out "core/cox.rkt")
         (all-from-out "core/poisson.rkt")
         (all-from-out "core/mgaussian.rkt")
         (all-from-out "core/path.rkt")
         (all-from-out "core/model.rkt")
         (all-from-out "core/cv.rkt")
         (all-from-out "core/formula.rkt")
         (all-from-out "foreign.rkt"))
