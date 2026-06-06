#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-poisson"]{Poisson regression (counts)}

The @deftech{Poisson family} models @bold{count} responses --- non-negative
numbers like events per interval. It uses a @emph{log link}, so the fitted mean
is @math{μ = exp(β₀ + x·β)} and a positive coefficient @emph{multiplies} the
expected count. Poisson has an intercept (unlike Cox), and the elastic-net knobs
(@math{α}, @math{λ}) are the same as every other family.

In the synthetic data below the expected count rises with @math{x₁}, while
@math{x₂} is noise. A lasso-penalized fit recovers a positive @math{β₁} and drops
@math{x₂} to exactly @racket[0.0].

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide run-example)]

@chunk[<data>
(define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0)
            (5.0 2.0) (6.0 1.0) (7.0 2.0) (8.0 1.0)))
(code:comment "counts roughly following exp(0.3 * x1)")
(define y '(1 2 2 3 4 6 8 11))]

@racket[poisson-fit] takes the predictor matrix and the count response. The
@math{x₁} coefficient comes back positive (the rate rises with @math{x₁}) and the
noise @math{β₂} is exactly @racket[0.0]; @racket[poisson-result-dev-ratio] is the
fraction of null deviance explained.

@chunk[<fit>
(define result (poisson-fit X y #:lambda 0.2))]

@racket[poisson-predict-mean] applies the log link to return the fitted rate
@math{exp(β₀ + x·β)} for each row --- here it tracks the observed counts:

@racketblock[
(poisson-predict-mean result X)
(code:comment "=> fitted means, increasing with x1")]

@chunk[<run-example>
(define (run-example)
  <data>
  <fit>
  result)]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
