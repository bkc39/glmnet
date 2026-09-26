#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-logistic"]{Binomial logistic regression (classification)}

@margin-note{Source: @filepath{glmnet/examples/04-logistic.rkt}}

The @tech{binomial family} fits a two-class classifier. The response is a 0/1
class label and the model is linear in the @emph{log-odds} of class 1. Under the
hood this is glmnet's @tt{lognet} solver rather than the Gaussian @tt{elnet},
but the penalty is the same: @racket[#:alpha 1.0] is the sparse (lasso)
logistic and @racket[#:alpha 0.0] the ridge logistic.

@section[#:tag "ex-logistic-data"]{The data}

Class 1 has high @math{x₁} and low @math{x₂}, class 0 the reverse, and
@math{x₃} is noise, identically distributed in both classes:

@examples[#:eval ev #:label #f
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
(define y '(0 0 0 0 0 0 1 1 1 1 1 1))
]

@section[#:tag "ex-logistic-fit"]{Fitting}

@examples[#:eval ev #:label #f
(define fit (logistic-fit X y #:lambda 0.04))
fit
(coef fit)
]

@racket[coef] lists the intercept and then the coefficients, all on the
log-odds scale. @math{x₁} raises the odds of class 1 and @math{x₂} lowers
them; the noise predictor is exactly @racket[0.0].
Exponentiating a coefficient gives an @emph{odds ratio}, the factor by which
one more unit of the predictor multiplies the odds of class 1:

@examples[#:eval ev #:label #f
(for/list ([b (in-vector (logistic-result-coefficients fit))])
  (exp b))
]

@racket[logistic-result-dev-ratio] is the fraction of null deviance explained,
the logistic analogue of @math{R²}.

@section[#:tag "ex-logistic-predict"]{Predicting}

@racket[logistic-predict-proba] returns the class-1 probability for each row;
@racket[logistic-predict] thresholds it into a label. The first is
@racket[predict] with @racket[#:type 'response]; at its default threshold, the
second agrees with @racket[#:type 'class]:

@examples[#:eval ev #:label #f
(logistic-predict-proba fit X)
(equal? (logistic-predict fit X) y)
(equal? (predict fit X #:type 'class) y)
]

Every training point is classified correctly. The threshold defaults to
@racket[0.5]; for a point near the boundary, where it sits decides the label:

@examples[#:eval ev #:label #f
(define borderline '((3.5 3.5 2.0)))
(logistic-predict-proba fit borderline)
(logistic-predict fit borderline)
(logistic-predict fit borderline #:threshold 0.4)
]

Lowering the threshold trades false negatives for false positives, which is
what you want when missing a class-1 case costs more than a false alarm.

@section[#:tag "ex-logistic-separation"]{Separation and the penalty}

These two classes are @emph{linearly separable}: a line in the
@math{(x₁, x₂)} plane splits them perfectly. Unpenalized logistic regression has
no finite solution on such data, because making the coefficients larger always
fits a little better. The penalty is what keeps the fit well defined:

@examples[#:eval ev #:label #f
(define (round3 x)
  (/ (round (* 1000 x)) 1000))
(define (rounded v)
  (for/list ([b (in-vector v)])
    (round3 b)))
(for ([lam (in-list '(0.0 0.001 0.01 0.04 0.1 0.3))])
  (define fit (logistic-fit X y #:lambda lam))
  (printf "λ = ~a: β = ~a, dev-ratio = ~a\n"
          lam
          (rounded (logistic-result-coefficients fit))
          (round3 (logistic-result-dev-ratio fit))))
]

At @math{λ = 0} the solver returns large coefficients, the noise predictor among
them, with a deviance ratio of essentially 1: the model has memorized the
training labels. Even a small penalty removes the noise and keeps the
coefficients finite. On harder data, glmnet can instead fail with an error
saying a class probability collapsed; a larger @racket[#:lambda] fixes that too.

@section[#:tag "ex-logistic-when"]{When to use it}

Use the binomial family for any yes/no outcome: default or not, spam or not,
malignant or benign. The lasso logistic is a standard tool for building a
sparse, interpretable classifier from many candidate features. For more than
two classes, see @secref["ex-multinomial"].

@(close-eval ev)
