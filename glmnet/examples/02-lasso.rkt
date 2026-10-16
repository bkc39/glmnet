#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-lasso"]{Lasso (L1, @math{α = 1})}

The @deftech{lasso} is the elastic net at @math{α = 1}: a pure L1 penalty.
Where ridge shrinks every coefficient but keeps them all, the lasso performs
@emph{variable selection}: it drives coefficients exactly to zero, more of them
as @math{λ} grows. It is the model to reach for when you believe only a few
predictors matter and you want the fit to say which.

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide X y fit kept)]

The same fixture as @secref["ex-ridge"]: the response is exactly
@math{y = 1 + 2x₁ − x₂}, and @math{x₃ = x₁²} is irrelevant.

@chunk[<data>
(define X '((1.0 2.0  1.0)
            (2.0 1.0  4.0)
            (3.0 4.0  9.0)
            (4.0 3.0 16.0)
            (5.0 6.0 25.0)
            (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))]

@racket[lasso] is @racket[elnet-fit] with @racket[#:alpha 1.0]. The
coefficient of @math{x₃} comes back exactly @racket[0.0]: the irrelevant
predictor has been selected out, while @math{x₁} and @math{x₂} keep their
signs, a little shrunk.

@chunk[<fit>
(define fit (lasso X y #:lambda 0.05))]

The predictors a fit kept are those with a nonzero coefficient. @racket[coef]
gives the intercept first, so the coefficients start at index 1, which is also
the number of their predictor:

@chunk[<kept>
(define kept
  (for/list ([b (in-vector (coef fit) 1)]
             [j (in-naturals 1)]
             #:unless (zero? b))
    j))]

@chunk[<*>
  <require>
  <provide>
  <data>
  <fit>
  <kept>]
