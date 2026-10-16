#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-ols"]{Ordinary least squares (@math{λ = 0})}

With the penalty strength set to @math{λ = 0}, the elastic net is
@deftech{ordinary least squares}: there is no penalty, so the mixing parameter
@math{α} drops out and glmnet returns the unregularized fit. It is the natural
first model because its answer can be checked exactly.

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide X y fit fitted)]

The response is an exact linear function of two predictors,
@math{y = 1 + 2x₁ − x₂}, with no error term, so least squares should recover the
intercept @racket[1.0] and the coefficients @racket[2.0] and @racket[-1.0]. The
design matrix is a list of rows, and the response the matching list:

@chunk[<data>
(define X '((1.0 2.0)
            (2.0 1.0)
            (3.0 4.0)
            (4.0 3.0)
            (5.0 6.0)))
(define y '(1.0 4.0 3.0 6.0 5.0))]

@racket[ols] is @racket[elnet-fit] with @racket[#:lambda 0.0]. @racket[coef]
gives the intercept and then the coefficients, and @racket[deviance-ratio] the
@math{R²}, which is 1 up to the solver's tolerance:

@chunk[<fit>
(define fit (ols X y))]

A fitted value is the intercept plus the dot product of a row with the
coefficients. @racket[predict] computes it for each row, and on the training
rows the fitted values reproduce @racket[y]:

@chunk[<fitted>
(define fitted (predict fit X))]

@chunk[<*>
  <require>
  <provide>
  <data>
  <fit>
  <fitted>]
