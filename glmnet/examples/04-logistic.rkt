#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-logistic"]{Binomial logistic regression (classification)}

The @deftech{binomial family} fits a two-class @emph{classifier}: instead of a
numeric response, @racket[_y] is a 0/1 class label and the model returns the
@emph{log-odds} of class 1. Under the hood this is glmnet's @tt{lognet}
coordinate-descent solver rather than the Gaussian @tt{elnet}, but the
elastic-net knobs are identical --- @math{α} mixes the L1/L2 penalty and
@math{λ} sets its strength --- so @racket[#:alpha 1.0] is the @emph{lasso}
(sparse) logistic and @racket[#:alpha 0.0] the @emph{ridge} logistic.

The synthetic data below is linearly separable: class 1 has high @math{x₁} and
low @math{x₂}, class 0 the reverse, and @math{x₃} is pure noise --- identically
distributed in both classes. A lasso-penalized fit recovers the signs
(@math{β₁ > 0}, @math{β₂ < 0}), drives the noise coefficient @bold{exactly} to
@racket[0.0], and classifies every training point correctly.

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide X y fit probabilities labels)]

@chunk[<data>
(define X '((1.0 5.0 2.0)
            (2.0 6.0 1.0)
            (2.0 5.0 3.0)
            (1.0 4.0 1.0)
            (3.0 6.0 2.0)
            (2.0 4.0 2.0)
            (6.0 2.0 2.0)
            (5.0 1.0 1.0)
            (6.0 1.0 3.0)
            (5.0 2.0 1.0)
            (4.0 1.0 2.0)
            (6.0 3.0 2.0)))
(define y '(0 0 0 0 0 0 1 1 1 1 1 1))]

@racket[logistic-fit] with @racket[#:alpha 1.0] is the sparse (lasso) logistic.
The third coefficient --- the noise predictor @math{x₃} --- comes back exactly
@racket[0.0], while @math{x₁} and @math{x₂} keep their (opposite) signs.
@racket[deviance-ratio] gives the fraction of null deviance explained (the
logistic analogue of @math{R²}).

@chunk[<fit>
(define fit (logistic-fit X y #:lambda 0.04))]

@racket[predict] with @racket[#:type 'response] returns the class-1 probability
for each row, low for class 0 and high for class 1, and @racket[#:type 'class]
the label, which here is right for every training point:

@chunk[<predict>
(define probabilities (predict fit X #:type 'response))
(define labels (predict fit X #:type 'class))]

@racket[logistic-predict-proba] is the first call. @racket[logistic-predict] is
the second with a @racket[#:threshold] on the probability, @racket[0.5] unless
given.

@chunk[<*>
  <require>
  <provide>
  <data>
  <fit>
  <predict>]
