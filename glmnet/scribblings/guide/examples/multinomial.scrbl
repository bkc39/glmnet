#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-multinomial"]{Multinomial classification (@math{K} classes)}

@margin-note{Source: @filepath{glmnet/examples/05-multinomial.rkt}}

The @tech{multinomial family} generalizes the binomial classifier to
@math{K > 2} classes. It is the same @tt{lognet} solver with the class count set
above one, and the penalty is unchanged. The response is an integer class label
in @math{0, …, K−1}; the fit returns one intercept and one coefficient vector
per class, and the class probabilities are the softmax of the @math{K} linear
predictors @math{η_k = a0_k + xβ_k}.

@section[#:tag "ex-multinomial-data"]{The data}

Three separable clusters: class 0 at low @math{x₁}, class 1 at high @math{x₁},
class 2 at high @math{x₂}:

@examples[#:eval ev #:label #f
(define X '((1.0 1.0) (2.0 1.0) (1.0 2.0) (2.0 2.0)
            (5.0 1.0) (6.0 1.0) (5.0 2.0) (6.0 2.0)
            (3.0 5.0) (4.0 5.0) (3.0 6.0) (4.0 6.0)))
(define y '(0 0 0 0 1 1 1 1 2 2 2 2))
]

@section[#:tag "ex-multinomial-fit"]{Fitting}

@examples[#:eval ev #:label #f
(define fit (multinomial-fit X y #:lambda 0.01))
(multinomial-result-intercepts fit)
(multinomial-result-coefficients fit)
]

Each class keeps only the predictor that sets it apart. Classes 0 and 1 split on
@math{x₁}, with opposite signs; class 2 is picked out by @math{x₂}; every other
coefficient is exactly @racket[0.0].

Only differences between the classes' linear predictors matter: adding the same
vector to every class's coefficients leaves the softmax unchanged. glmnet
resolves that ambiguity with the penalty, which prefers the smallest
coefficients, so the class-0 and class-1 weights on @math{x₁} come out
nearly equal and opposite rather than, say, zero and twice as large.

@section[#:tag "ex-multinomial-predict"]{Predicting}

@racket[multinomial-predict-proba] returns one probability per class for each
row, summing to 1, and @racket[multinomial-predict] returns the most probable
class:

@examples[#:eval ev #:label #f
(define new-points '((1.5 1.5) (5.5 1.5) (3.5 5.5) (3.5 1.5)))
(multinomial-predict-proba fit new-points)
(multinomial-predict fit new-points)
(equal? (multinomial-predict fit X) y)
]

The first three points sit at the cluster centres and are classified with
confidence. The fourth lies between clusters 0 and 1; its probabilities show
how close the call is.

@section[#:tag "ex-multinomial-labels"]{Class labels}

The labels must be the integers @racket[0] to @math{K−1}, with every class
present. A gap is an error, not an empty class:

@examples[#:eval ev #:label #f
(eval:error (multinomial-fit X '(0 0 0 0 2 2 2 2 2 2 2 2) #:lambda 0.01))
]

@section[#:tag "ex-multinomial-when"]{When to use it}

Use the multinomial family for a categorical outcome with more than two
unordered levels: species, document topics, product categories. For two classes,
use @secref["ex-logistic"].

@(close-eval ev)
