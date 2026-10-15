#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-elastic-net"]{Elastic net (@math{0 < α < 1})}

The @deftech{elastic net} blends the two penalties: with @math{0 < α < 1} the
objective mixes the lasso's L1 term with the ridge's L2 term. It keeps the
lasso's ability to select predictors and borrows the ridge's stability. In
particular it tends to keep or drop @emph{groups} of correlated predictors
together, where the lasso would pick one of them arbitrarily. @math{α = 0}
recovers @secref["ex-ridge"] and @math{α = 1} recovers @secref["ex-lasso"].

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide X y fit zero-counts)]

The running fixture again, with the irrelevant @math{x₃ = x₁²}:

@chunk[<data>
(define X '((1.0 2.0  1.0)
            (2.0 1.0  4.0)
            (3.0 4.0  9.0)
            (4.0 3.0 16.0)
            (5.0 6.0 25.0)
            (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))]

@racket[elastic-net] takes an explicit @racket[#:alpha]. The fit both shrinks
its coefficients, as ridge does, and sets one exactly to zero, as the lasso
does.

@chunk[<fit>
(define fit (elastic-net X y #:alpha 0.5 #:lambda 0.5))]

At a shared @math{λ}, the number of coefficients the elastic net zeroes lies
between ridge, which zeroes none, and the lasso, which zeroes the most.
@racket[coef] gives the intercept first, so the coefficients start at index 1:

@chunk[<zeros>
(define (zeros model)
  (for/sum ([b (in-vector (coef model) 1)] #:when (zero? b))
    1))
(define zero-counts
  (map zeros (list (ridge X y #:lambda 0.5)
                   fit
                   (lasso X y #:lambda 0.5))))]

@chunk[<*>
  <require>
  <provide>
  <data>
  <fit>
  <zeros>]
