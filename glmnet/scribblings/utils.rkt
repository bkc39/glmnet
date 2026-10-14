#lang racket/base

;; Shared documentation helpers, modelled on racket-doc's
;; scribblings/reference/mz.rkt and scribblings/guide/guide-utils.rkt (and on
;; rkt-polars' polars/scribblings/utils.rkt). Each chapter's preamble is
;;
;;   #lang scribble/manual
;;   @(require "../utils.rkt")
;;   @(define ev (make-glmnet-eval))
;;
;; Examples run against the real bindings at documentation-build time, so a
;; printed coefficient cannot drift from what the library produces, and a broken
;; example fails the build. They evaluate in `racket`, as the Racket Guide's do,
;; with glmnet required (the two export no common name). The library is an FFI
;; wrapper, so the sandbox runs with the ambient security guard and without
;; memory or time limits.

(require scribble/manual
         scribble/example
         scribble/core
         scribble/decode
         racket/sandbox
         syntax/parse/define)

(require (for-label glmnet
                    (only-in datasets load-dataset)
                    glmnet/data/nested
                    glmnet/plot
                    glmnet/data/csv
                    glmnet/data/math
                    glmnet/data/polars
                    (only-in polars
                             dataframe? series? dataframe series read-csv ref column-names
                             dtype polars-null dataframe->f64vector cast)
                    (prefix-in pl: (only-in polars head))
                    glmnet/datasets
                    racket
                    racket/flonum
                    ffi/vector
                    math/array
                    math/matrix
                    math/distributions
                    (only-in pict pict?)
                    (only-in plot
                             plot-pict plot-width plot-height plot-title plot-font-size vrule
                             points hrule function)
                    (only-in plot/utils renderer2d?)))

(provide (all-from-out scribble/manual)
         (all-from-out scribble/example)
         (for-label (all-from-out glmnet
                                  datasets
                                  glmnet/data/nested
                                  glmnet/plot
                                  glmnet/data/csv
                                  glmnet/data/math
                                  glmnet/data/polars
                                  polars
                                  glmnet/datasets
                                  racket
                                  racket/flonum
                                  ffi/vector
                                  math/array
                                  math/matrix
                                  math/distributions
                                  pict
                                  plot
                                  plot/utils))
         make-glmnet-eval
         see-reference
         exnraise)

(define (make-glmnet-eval)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f]
                 [sandbox-eval-limits #f]
                 [sandbox-security-guard current-security-guard]
                 [sandbox-path-permissions '((exists "/"))])
    (make-base-eval #:lang 'racket '(require glmnet))))

;; "the @exnraise[exn:fail:contract]" => "the `exn:fail:contract` exception is
;; raised", after mz.rkt.
(define (*exnraise s)
  (make-element #f (list s " exception is raised")))
(define-syntax-parse-rule (exnraise s:id)
  (*exnraise (racket s)))

;; A margin note pointing from a guide chapter into the reference, after
;; guide-utils.rkt's `refdetails`.
(define (see-reference tag . what)
  (apply margin-note
         (decode-content (append (list "See " (secref tag) " for ")
                                 what
                                 (list ".")))))
