#lang info

;; raco review lints this as a normal module and flags every `info`
;; definition as unused; #lang info has no value-level uses to detect.
#|review: ignore|#

(define collection "glmnet")
(define version "0.1")
;; net-lib provides net/url, used by private/demo-utils.rkt (the parity/demo
;; dataset loaders). It ships with the main Racket distribution, so the catalog
;; resolves it trivially; main.rkt itself stays base-only.
;; scribble-lib is a RUN dependency, not build-only: the examples/NN-*.rkt files
;; are #lang scribble/lp2 literate programs that ship as compiled collection
;; modules (lp-included by the docs, required by examples/test/*.rkt), so their
;; .zo files import scribble's lp2 runtime (scribble/lp/lang/lang2.rkt). The
;; catalog's `raco setup --check-pkg-deps` flags it under deps, not build-deps.
;; draw-lib, pict-lib and plot-lib are for glmnet/plot (plot.rkt), which main.rkt
;; does not re-export, so `(require glmnet)` does not load them.
(define deps '("base" "draw-lib" "net-lib" "pict-lib" "plot-lib" "scribble-lib"))
;; at-exp-lib provides scribble/reader, which tests/docs-coverage-test.rkt uses
;; to read the manual's sources. pict-doc and plot-doc are for the manual's
;; links into their documentation; plot-gui-lib provides `plot`, the module
;; plot's documentation is written against, which the manual imports for-label.
(define build-deps
  '("at-exp-lib" "pict-doc" "plot-doc" "plot-gui-lib" "racket-doc" "rackunit-lib"
    "sandbox-lib"))
(define scribblings '(("scribblings/glmnet.scrbl" (multi-page))))
(define pkg-desc
  (string-append
   "Racket FFI bindings to glmnet: lasso, ridge, and elastic-net regularized GLMs"
   " -- linear, logistic, multinomial, Poisson, Cox, and multi-response --"
   " with R's coefficient-path and cross-validation plots"))
(define pkg-authors '(bkc))
;; The vendored R glmnet Fortran (fortran/vendor/glmnet5dpclean.f) is GPL-2, so this
;; binding is distributed under GPL-2.0-or-later. See the project root LICENSE.
(define license 'GPL-2.0-or-later)
(define pkg-tags
  '("machine-learning" "statistics" "data-science" "glmnet"
    "regression" "classification" "regularization"
    "lasso" "ridge" "elastic-net"
    "generalized-linear-models" "survival-analysis" "plot"))
(define pre-install-collection "private/install-glmnet-native.rkt")
