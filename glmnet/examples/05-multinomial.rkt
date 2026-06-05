#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-multinomial"]{Multinomial classification (K classes)}

The @deftech{multinomial family} generalizes the binomial classifier to
@math{K > 2} classes. It is the same glmnet @tt{lognet} solver with the number of
classes set above 1, so the elastic-net knobs (@math{α}, @math{λ}) are unchanged:
the response is now an integer class label in @math{{0, …, K−1}} and the fit
returns @math{K} intercepts and @math{K} coefficient vectors. Class probabilities
are the @emph{softmax} over the @math{K} linear predictors
@math{η_k = a0_k + x·β_k}, and the predicted class is the @racket[argmax].

The synthetic data below is three separable clusters: class 0 at low @math{x₁},
class 1 at high @math{x₁}, and class 2 at high @math{x₂}. A lasso-penalized fit
gives each class a sparse coefficient vector that picks out its discriminating
feature, and the argmax classifies every training point correctly.

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide run-example)]

@chunk[<data>
(define X '((1.0 1.0) (2.0 1.0) (1.0 2.0) (2.0 2.0)
            (5.0 1.0) (6.0 1.0) (5.0 2.0) (6.0 2.0)
            (3.0 5.0) (4.0 5.0) (3.0 6.0) (4.0 6.0)))
(define y '(0 0 0 0 1 1 1 1 2 2 2 2))]

@racket[multinomial-fit] takes integer class labels and returns a
@racket[multinomial-result] whose @racket[multinomial-result-coefficients] is a
vector of @math{K} per-class coefficient vectors. Here each class keeps only the
feature that distinguishes it (class 0 and 1 split on @math{x₁}, class 2 on
@math{x₂}); the others come back exactly @racket[0.0].

@chunk[<fit>
(define result (multinomial-fit X y #:lambda 0.01))]

@racket[multinomial-predict-proba] gives the per-class softmax probabilities for
each row (they sum to 1), and @racket[multinomial-predict] takes the argmax:

@racketblock[
(multinomial-predict-proba result X)
(code:comment "=> per-row probabilities, each summing to 1")
(multinomial-predict result X)
(code:comment "=> (0 0 0 0 1 1 1 1 2 2 2 2)  -- every training point correct")
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
