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
(require racket/list
         racket/match
         racket/math
         glmnet)]

@chunk[<provide>
(provide X y fit zero-counts X-dup lasso-dup enet-dup X40 y40 folds alpha-cvs
         longer-grid longer-ridge-cv)]

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
@racket[coef] gives the intercept first, and the count skips it:

@chunk[<zeros>
(define (zeros model)
  (match-define (vector _ β ...) (coef model))
  (count zero? β))
(define zero-counts
  (map zeros (list (ridge X y #:lambda 0.5)
                   fit
                   (lasso X y #:lambda 0.5))))]

The case the elastic net was designed for is a group of predictors that carry
the same signal. Take the extreme: duplicate @math{x₁}, so the first two
columns are identical, and drop @math{x₃}. The lasso puts all of
@math{x₁}'s weight on one copy and leaves the other within rounding of zero,
because the two identical columns tie exactly; the elastic net's L2 term makes
splitting the weight cheaper than concentrating it, so the two copies get
almost equal coefficients.

@chunk[<grouping>
(define X-dup
  (map (match-lambda [(list x₁ x₂ _) (list x₁ x₁ x₂)]) X))
(define lasso-dup (lasso X-dup y #:lambda 0.1))
(define enet-dup (elastic-net X-dup y #:alpha 0.5 #:lambda 0.1))]

@racket[elnet-cv] chooses @math{λ} for one @math{α}. To choose @math{α} too,
cross-validate each candidate on the same folds, so that the errors differ only
because the models do. The data are the 40 noisy observations of
@secref["ex-lasso-cv"]:

@chunk[<noisy>
(random-seed 3)
(define (uniform a b) (+ a (* (- b a) (random))))
(define-values (x₁ x₂)
  (for/lists (x₁ x₂) ([_ (in-range 40)])
    (values (uniform 0 6) (uniform 0 6))))
(define X40 (map list x₁ x₂ (map sqr x₁)))
(define y40
  (for/list ([a (in-list x₁)] [b (in-list x₂)])
    (+ 1.0 (* 2 a) (- b) (uniform -1 1))))]

Ridge's default path stops before its error bottoms out, so its best @math{λ}
is the last one; a longer @racket[#:lambda] grid closes the gap. Every
@math{α} above 0 reaches nearly the same error, within one standard error of
the others.

@chunk[<choosing>
(define folds (random-fold-ids 40))
(define alpha-cvs
  (for/list ([α (in-list '(0.0 0.25 0.5 0.75 1.0))])
    (cons α (elnet-cv X40 y40 #:alpha α #:fold-ids folds))))
(define longer-grid
  (for/list ([i (in-range 160)])
    (expt 10.0 (- 3.5 (* 0.05 i)))))
(define longer-ridge-cv
  (elnet-cv X40 y40 #:alpha 0.0 #:fold-ids folds #:lambda longer-grid))]

@chunk[<*>
  <require>
  <provide>
  <data>
  <fit>
  <zeros>
  <grouping>
  <noisy>
  <choosing>]
