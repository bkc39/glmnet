#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-ols"]{Ordinary least squares (@math{λ = 0})}

The elastic net at penalty strength @math{λ = 0} is just @deftech{ordinary least
squares}: with no penalty, the mixing parameter @math{α} drops out and glmnet
returns the unregularized fit. It is the natural first model --- a known answer
we can check exactly.

We use a noise-free fixture so the answer is unambiguous. Take two predictors
and build the response as an exact linear function of them,
@math{y = 1 + 2 x₁ − x₂}, with no error term. Ordinary least squares then
recovers the intercept @racket[1.0] and coefficients @racket[(2.0 -1.0)] --- up
to the coordinate-descent solver's tolerance --- with @math{R² ≈ 1}.

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide run-example)]

The design matrix is a list of rows; the response is the matching vector. (Rows
here, columns there: the binding marshals to the column-major layout the Fortran
expects.)

@chunk[<data>
(define X '((1.0 2.0)
            (2.0 1.0)
            (3.0 4.0)
            (4.0 3.0)
            (5.0 6.0)))
(define y '(1.0 4.0 3.0 6.0 5.0))]

@racket[ols] is @racket[elnet-fit] with @racket[#:lambda 0.0]. The result is an
@racket[elnet-result] carrying the intercept, the dense coefficient vector, the
achieved @math{R²}, and the @math{λ} actually used.

@chunk[<fit>
(define result (ols X y))]

@chunk[<run-example>
(define (run-example)
  <data>
  <fit>
  result)]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
