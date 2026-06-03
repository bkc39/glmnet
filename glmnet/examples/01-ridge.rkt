#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-ridge"]{Ridge regression (L2, @math{α = 0})}

@deftech{Ridge regression} is the elastic net at @math{α = 0}: a pure L2 penalty.
Unlike the lasso it never sets a coefficient exactly to zero --- it shrinks every
coefficient smoothly toward zero, more so as @math{λ} grows, until at very large
@math{λ} they all vanish and only the intercept (the mean of the response)
remains.

We reuse the OLS fixture but add an irrelevant third predictor
@math{x₃ = x₁²}: the response is still exactly @math{y = 1 + 2x₁ − x₂}, so
@math{x₃} carries no signal. Ordinary least squares gives it coefficient
@racket[0]; ridge instead keeps every coefficient @emph{nonzero} but shrunken ---
including a small value on the irrelevant predictor. That is the characteristic
ridge behaviour: shrink, never select.

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

@racket[ridge] is @racket[elnet-fit] with @racket[#:alpha 0.0]. With a modest
@math{λ} the leading coefficient shrinks from its OLS value of @racket[2.0] while
staying nonzero.

@chunk[<fit>
(define result (ridge X y #:lambda 0.1))]

@chunk[<run-example>
(define (run-example)
  <data>
  <fit>
  result)]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
