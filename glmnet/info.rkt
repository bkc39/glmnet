#lang info

;; raco review lints this as a normal module and flags every `info`
;; definition as unused; #lang info has no value-level uses to detect.
#|review: ignore|#

(define collection "glmnet")
(define version "0.1")
;; net-lib provides net/url, used by private/demo-utils.rkt (the parity/demo
;; dataset loaders). It ships with the main Racket distribution, so the catalog
;; resolves it trivially; main.rkt itself stays base-only.
(define deps '("base" "net-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/glmnet.scrbl" (multi-page))))
(define pkg-desc
  (string-append
   "Racket FFI bindings to glmnet: lasso, ridge, and elastic-net regularized GLMs"
   " -- linear, logistic, multinomial, Poisson, Cox, and multi-response"))
(define pkg-authors '(bkc))
;; The vendored glmnet Fortran (fortran/vendor/glmnet5.f90) is GPL-2.0, so this
;; binding is distributed under GPL-2.0-or-later. See the project root LICENSE.
(define license 'GPL-2.0-or-later)
(define pkg-tags
  '("machine-learning" "statistics" "data-science" "glmnet"
    "regression" "classification" "regularization"
    "lasso" "ridge" "elastic-net"
    "generalized-linear-models" "survival-analysis"))
(define pre-install-collection "private/install-glmnet-native.rkt")
