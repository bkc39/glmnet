#lang info

;; raco review lints this as a normal module and flags every `info`
;; definition as unused; #lang info has no value-level uses to detect.
#|review: ignore|#

;; This package adds glmnet/plot to the `glmnet` collection. It is a separate
;; package so that plot-lib, which is Typed Racket and pulls in the drawing
;; stack, stays out of glmnet's dependencies.
(define collection "glmnet")
(define version "0.1")
(define deps '("base" "draw-lib" "glmnet" "pict-lib" "plot-lib"))
;; at-exp-lib provides scribble/reader, which tests/plot-docs-coverage-test.rkt
;; uses to read the manual's sources. plot-gui-lib provides `plot`, the module
;; plot's documentation is written against, which the manual imports for-label.
(define build-deps
  '("at-exp-lib" "pict-doc" "plot-doc" "plot-gui-lib" "racket-doc"
    "rackunit-lib" "sandbox-lib" "scribble-lib"))
(define scribblings '(("scribblings/glmnet-plot.scrbl" ())))
(define pkg-desc
  "Plots for glmnet: coefficient paths and cross-validation curves, as R's glmnet draws them")
(define pkg-authors '(bkc))
;; It links glmnet, whose vendored R glmnet Fortran is GPL-2, so it has
;; glmnet's license. See the project root LICENSE.
(define license 'GPL-2.0-or-later)
(define pkg-tags '("machine-learning" "statistics" "glmnet" "plot" "lasso" "elastic-net"))
