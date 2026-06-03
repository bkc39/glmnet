#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-lasso"]{Lasso (L1, @math{α = 1})}

The @deftech{lasso} is the elastic net at @math{α = 1}: a pure L1 penalty. Where
ridge shrinks every coefficient but keeps them all, the lasso performs
@emph{variable selection} --- it drives coefficients @bold{exactly} to zero, and
more of them as @math{λ} grows. It is the model you reach for when you believe
only a few predictors matter and you want the fit to say which.

Using the same fixture --- relevant @math{x₁}, @math{x₂} and the irrelevant
@math{x₃ = x₁²} --- a modest @math{λ} selects @math{x₃} out entirely (its
coefficient is @racket[0.0]) while keeping the two real predictors. Push
@math{λ} higher and even @math{x₂} drops, until at large @math{λ} only the
intercept survives.

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide run-example)]

@chunk[<data>
(define X '((1.0 2.0  1.0)
            (2.0 1.0  4.0)
            (3.0 4.0  9.0)
            (4.0 3.0 16.0)
            (5.0 6.0 25.0)
            (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))]

@racket[lasso] is @racket[elnet-fit] with @racket[#:alpha 1.0]. The third
coefficient comes back exactly @racket[0.0] --- the irrelevant predictor has been
selected out.

@chunk[<fit>
(define result (lasso X y #:lambda 0.05))]

@chunk[<run-example>
(define (run-example)
  <data>
  <fit>
  result)]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
