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
(provide run-example)]

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
@racket[0.0], while @math{x₁} and @math{x₂} keep their (opposite) signs. The
@racket[logistic-result] also carries @racket[logistic-result-dev-ratio], the
fraction of null deviance explained (the logistic analogue of @math{R²}).

@chunk[<fit>
(define result (logistic-fit X y #:lambda 0.04))]

Given a fit, @racket[logistic-predict-proba] returns the class-1 probability for
each row and @racket[logistic-predict] thresholds it into a hard 0/1 label:

@racketblock[
(logistic-predict-proba result X)
(code:comment "=> (0.02 0.05 ... 0.97 0.99)  -- low for class 0, high for class 1")
(logistic-predict result X)
(code:comment "=> (0 0 0 0 0 0 1 1 1 1 1 1)   -- every training point correct")
]

@chunk[<run-example>
(define (run-example)
  <data>
  <fit>
  result)]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
