#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-ridge"]{Ridge regression (L2, @math{α = 0})}

@deftech{Ridge regression} is the elastic net at @math{α = 0}: a pure L2
penalty. It shrinks every coefficient smoothly toward zero, more as @math{λ}
grows, but never sets one exactly to zero. At very large @math{λ} all the
coefficients vanish and only the intercept, the mean of the response, remains.

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide X y fit)]

The fixture from @secref["ex-ols"], extended by one row and one irrelevant
predictor, @math{x₃ = x₁²}. The response is still exactly
@math{y = 1 + 2x₁ − x₂}, so @math{x₃} carries no signal, but it is strongly
correlated with @math{x₁}:

@chunk[<data>
(define X '((1.0 2.0  1.0)
            (2.0 1.0  4.0)
            (3.0 4.0  9.0)
            (4.0 3.0 16.0)
            (5.0 6.0 25.0)
            (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))]

@racket[ridge] is @racket[elnet-fit] with @racket[#:alpha 0.0]. Least squares
would give @math{x₃} a coefficient of zero. Ridge gives it a small nonzero
weight instead, and pulls @math{x₁} down from @racket[2.0]: the penalty prefers
to spread weight across correlated predictors rather than concentrate it. That
is the characteristic ridge behaviour: shrink, never select.

@chunk[<fit>
(define fit (ridge X y #:lambda 0.1))]

@chunk[<*>
  <require>
  <provide>
  <data>
  <fit>]
