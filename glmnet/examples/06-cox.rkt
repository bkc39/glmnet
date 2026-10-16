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
(provide X times statuses fit log-risk risk)]

The higher-risk subjects have the shorter follow-up times; a status of 1 is an
observed event, 0 a right-censored time:

@chunk[<data>
(define X '((0.5 1.0) (1.0 2.0) (1.5 1.0) (2.0 2.0)
            (2.5 1.0) (3.0 2.0) (3.5 1.0) (4.0 2.0)))
(define times '(12.0 10.0 11.0 8.0 6.0 7.0 4.0 3.0))
(define statuses '(0 0 0 1 1 1 1 1))]

@racket[cox-fit] takes the predictor matrix, the follow-up times, and the 0/1
statuses. There is no intercept, so @racket[coef] is the coefficients alone;
@math{β₁} comes back positive (risk rises with @math{x₁}) and the noise
@math{β₂} is exactly @racket[0.0].

@chunk[<fit>
(define fit (cox-fit X times statuses #:lambda 0.1))]

@racket[predict] with @racket[#:type 'link] returns the log relative hazard
@math{x·β} of each subject, and @racket[#:type 'response] returns
@math{exp(x·β)}, the hazard relative to the baseline, which ranks the subjects
by risk:

@chunk[<risk>
(define log-risk (predict fit X #:type 'link))
(define risk (predict fit X #:type 'response))]

@racket[cox-linear-predictor] and @racket[cox-relative-risk] are the same two
calls.

@chunk[<*>
  <require>
  <provide>
  <data>
  <fit>
  <risk>]
