#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-cox"]{Cox proportional hazards (survival)}

The @deftech{Cox family} fits a survival model: instead of a label or a number,
each observation carries a follow-up @emph{time} and a 0/1 @emph{event indicator}
(1 = the event was observed, 0 = right-censored). Cox has @bold{no intercept} ---
the baseline hazard is left unspecified --- so the fit returns coefficients only,
on the log relative-hazard scale, where a positive coefficient @emph{raises} the
hazard (shortens survival). The elastic-net knobs (@math{α}, @math{λ}) are the
same as every other family.

In the synthetic cohort below @math{x₁} is a risk factor: subjects with higher
@math{x₁} have the event sooner, while the low-risk subjects are censored;
@math{x₂} is noise. A lasso-penalized fit recovers a positive @math{β₁} and drops
@math{x₂} to exactly @racket[0.0].

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide run-example)]

@chunk[<data>
(define X '((0.5 1.0) (1.0 2.0) (1.5 1.0) (2.0 2.0)
            (2.5 1.0) (3.0 2.0) (3.5 1.0) (4.0 2.0)))
(code:comment "follow-up times (higher-risk subjects fail sooner)")
(define times '(12.0 10.0 11.0 8.0 6.0 7.0 4.0 3.0))
(code:comment "1 = event observed, 0 = right-censored")
(define statuses '(0 0 0 1 1 1 1 1))]

@racket[cox-fit] takes the predictor matrix, the times, and the 0/1 statuses.
There is no intercept, so @racket[cox-result-coefficients] is the whole model;
@math{β₁} comes back positive (risk rises with @math{x₁}) and the noise @math{β₂}
is exactly @racket[0.0].

@chunk[<fit>
(define result (cox-fit X times statuses #:lambda 0.1))]

@racket[cox-relative-risk] turns a fit into @math{exp(x·β)}, the hazard relative
to the baseline, so subjects can be ranked by risk; @racket[cox-linear-predictor]
returns the raw log relative hazard @math{x·β}:

@racketblock[
(cox-relative-risk result X)
(code:comment "=> relative hazard per subject; larger = higher risk")
(cox-linear-predictor result X)]

@chunk[<run-example>
(define (run-example)
  <data>
  <fit>
  result)]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
