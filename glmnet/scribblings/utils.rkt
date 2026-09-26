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
;; example fails the build. The library is an FFI wrapper, so the sandbox runs
;; with the ambient security guard and without memory or time limits.

(require scribble/manual
         scribble/example
         scribble/core
         scribble/decode
         racket/sandbox
         (for-syntax racket/base))

(require (for-label glmnet
                    racket/base
                    racket/contract
                    racket/match
                    ffi/vector))

(provide (all-from-out scribble/manual)
         (all-from-out scribble/example)
         (for-label (all-from-out glmnet
                                  racket/base
                                  racket/contract
                                  racket/match
                                  ffi/vector))
         make-glmnet-eval
         see-reference
         exnraise
         plot-manual)

(define (make-glmnet-eval)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f]
                 [sandbox-eval-limits #f]
                 [sandbox-security-guard current-security-guard]
                 [sandbox-path-permissions '((exists "/"))])
    (make-base-eval '(require glmnet))))

;; "the @exnraise[exn:fail:contract]" => "the `exn:fail:contract` exception is
;; raised", after mz.rkt.
(define (*exnraise s)
  (make-element #f (list s " exception is raised")))
(define-syntax exnraise
  (syntax-rules ()
    [(_ s) (*exnraise (racket s))]))

;; A margin note pointing from a guide chapter into the reference, after
;; guide-utils.rkt's `refdetails`.
(define (see-reference tag . what)
  (apply margin-note
         (decode-content (append (list "See " (secref tag) " for ")
                                 what
                                 (list ".")))))

;; "the glmnet-plot documentation", linking to the manual of the separate
;; glmnet-plot package. The link is indirect, resolved when it is followed, so
;; that this manual neither depends on that package nor warns when it is not
;; installed.
(define plot-manual
  (other-doc '(lib "glmnet/scribblings/glmnet-plot.scrbl") #:indirect "glmnet-plot"))
