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
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

(require (for-label glmnet
                    (only-in datasets load-dataset)
                    glmnet/data/nested
                    glmnet/plot
                    glmnet/data/csv
                    glmnet/data/math
                    glmnet/data/polars
                    (only-in polars
                             dataframe? series? dataframe series read-csv ref column-names
                             dtype polars-null dataframe->f64vector cast series->list)
                    (prefix-in pl: (only-in polars head))
                    glmnet/datasets
                    (except-in racket λ)
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
         page-helpers-block
         noisy-fixture
         see-reference
         exnraise)

(define-syntax-parse-rule (define-page-helpers forms:id block:id form:expr ...)
  (begin
    (define forms '(form ...))
    (define block (racketblock form ...))))

(define-page-helpers page-helpers page-helpers-block
  (define (fmt x [digits 3])
    (cond
      [(real? x) (~r x #:precision (list '= digits))]
      [else (for/list ([v x]) (fmt v digits))]))
  (define (coef-line coefs [digits 3] #:intercept? [intercept? #t])
    (match* (intercept? coefs)
      [(#t (vector β₀ β ...)) (format "β₀ = ~a, β = ~a" (fmt β₀ digits) (fmt β digits))]
      [(#f β) (format "β = ~a" (fmt β digits))])))

(define (make-glmnet-eval)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f]
                 [sandbox-eval-limits #f]
                 [sandbox-security-guard current-security-guard]
                 [sandbox-path-permissions '((exists "/"))])
    (apply make-base-eval #:lang 'racket '(require glmnet) page-helpers)))

(begin-for-syntax
  (define-splicing-syntax-class fixture-option
    #:description "an examples option"
    (pattern (~seq #:label label:expr) #:with (form ...) #'(#:label label))
    (pattern (~seq #:hidden) #:with (form ...) #'(#:hidden))))

(define-syntax-parse-rule (noisy-fixture ev:expr option:fixture-option ...)
  (examples #:eval ev option.form ... ...
   (random-seed 3)
   (define (uniform a b) (+ a (* (- b a) (random))))
   (define-values (x₁ x₂)
     (for/lists (x₁ x₂) ([_ (in-range 40)])
       (values (uniform 0 6) (uniform 0 6))))
   (define X40 (map list x₁ x₂ (map sqr x₁)))
   (define y40
     (for/list ([a (in-list x₁)] [b (in-list x₂)])
       (+ 1.0 (* 2 a) (- b) (uniform -1 1))))))

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
