#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-elastic-net"]{Elastic net (@math{0 < α < 1})}

The @deftech{elastic net} blends the two penalties: with @math{0 < α < 1} the
objective mixes the lasso's L1 term and the ridge's L2 term. It keeps the lasso's
ability to select variables while borrowing the ridge's stability --- in
particular it tends to keep or drop @emph{groups} of correlated predictors
together, where the pure lasso would arbitrarily pick one. Setting @math{α = 0}
recovers @secref["ex-ridge"] exactly and @math{α = 1} recovers
@secref["ex-lasso"]; the interesting models live in between.

On the running fixture, an @math{α = 0.5} fit at a moderate @math{λ} both shrinks
its coefficients (ridge-like) and drives at least one exactly to zero
(lasso-like). At a shared @math{λ}, the number of coefficients it zeros sits
between ridge (which zeros none) and lasso (which zeros the most) --- the
characteristic in-between behaviour of the blend.

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

@racket[elastic-net] takes an explicit @racket[#:alpha]. Compare its fit to the
@racket[ridge] and @racket[lasso] fits at the same @math{λ} to see it land
between them.

@chunk[<fit>
(define result (elastic-net X y #:alpha 0.5 #:lambda 0.5))]

@chunk[<run-example>
(define (run-example)
  <data>
  <fit>
  result)]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
