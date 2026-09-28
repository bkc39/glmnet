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
                    glmnet/plot
                    racket/base
                    racket/contract
                    racket/file
                    racket/match
                    ffi/vector
                    (only-in pict pict?)
                    (only-in plot
                             plot-pict plot-width plot-height plot-title plot-font-size vrule)
                    (only-in plot/utils renderer2d?)))

(provide (all-from-out scribble/manual)
         (all-from-out scribble/example)
         (for-label (all-from-out glmnet
                                  glmnet/plot
                                  racket/base
                                  racket/contract
                                  racket/file
                                  racket/match
                                  ffi/vector
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
