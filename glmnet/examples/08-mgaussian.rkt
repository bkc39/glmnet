#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-mgaussian"]{Multi-response Gaussian (grouped)}

The @deftech{multi-response Gaussian family} fits several numeric responses at
once. The response @racket[_Y] is a @emph{matrix} (one column per response), and
the model applies a @bold{grouped lasso} across the responses: a predictor enters
or leaves for @emph{all} responses together, so the fitted coefficient rows share
support. Each response keeps its own intercept, and the fit is a plain
identity-link Gaussian, so prediction is @math{ŷ_r = a0_r + x·β_r}.

In the data below both responses depend on @math{x₁} (response 1 rises with it,
response 2 falls) while @math{x₂} carries no signal. The grouped lasso recovers
the opposite signs and drives the entire @math{x₂} row to @racket[0.0] across both
responses.

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide X Y fit predictions)]

The two responses are exactly @math{y₁ = 1 + 2x₁} and @math{y₂ = 10 − x₁}:

@chunk[<data>
(define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0) (5.0 2.0) (6.0 1.0)))
(define Y '((3.0 9.0) (5.0 8.0) (7.0 7.0) (9.0 6.0) (11.0 5.0) (13.0 4.0)))]

@racket[mgaussian-fit] takes the predictor matrix and the response matrix. Its
@racket[coef] is a vector of per-response vectors, each an intercept followed by
the coefficients. The @math{x₁} coefficient is positive for response 1 and
negative for response 2; the @math{x₂} coefficients are exactly @racket[0.0]
for both.

@chunk[<fit>
(define fit (mgaussian-fit X Y #:lambda 0.1))]

@racket[predict] returns the per-response predictions for each row, one
@math{(y₁ y₂)} pair per row, tracking @racket[Y]:

@chunk[<predict>
(define predictions (predict fit X))]

@racket[mgaussian-predict] is the same call.

@chunk[<*>
  <require>
  <provide>
  <data>
  <fit>
  <predict>]
