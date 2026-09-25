#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-mgaussian"]{Multi-response Gaussian (grouped)}

@margin-note{Source: @filepath{glmnet/examples/08-mgaussian.rkt}}

The @tech{multi-response Gaussian family} fits several numeric responses at
once. The response is a matrix with one column per response, and the penalty is
a @emph{grouped} lasso across responses: a predictor enters for every response
or for none, so all the responses share one set of selected predictors. Each
response keeps its own intercept and its own coefficients, and prediction is
@math{ŷ_r = a0_r + xβ_r}.

@section[#:tag "ex-mgaussian-data"]{The data}

Both responses depend on @math{x₁}, the first rising with it
(@math{y₁ = 1 + 2x₁}) and the second falling (@math{y₂ = 10 − x₁}); @math{x₂}
carries no signal:

@examples[#:eval ev #:label #f
(define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0) (5.0 2.0) (6.0 1.0)))
(define Y '((3.0 9.0) (5.0 8.0) (7.0 7.0) (9.0 6.0) (11.0 5.0) (13.0 4.0)))
]

@section[#:tag "ex-mgaussian-fit"]{Fitting}

@examples[#:eval ev #:label #f
(define fit (mgaussian-fit X Y #:lambda 0.1))
(mgaussian-result-intercepts fit)
(mgaussian-result-coefficients fit)
(mgaussian-result-r-squared fit)
]

There is one coefficient vector per response. The @math{x₁} coefficient is
positive for the first and negative for the second, and @math{x₂} is exactly
@racket[0.0] in both. @racket[mgaussian-result-r-squared] is a single number
covering all the responses together.

@section[#:tag "ex-mgaussian-predict"]{Predicting}

@racket[mgaussian-predict] returns one list per row, with one prediction per
response:

@examples[#:eval ev #:label #f
(mgaussian-predict fit '((7.0 1.0) (8.0 2.0)))
]

@section[#:tag "ex-mgaussian-grouped"]{Grouped versus separate fits}

What the grouping buys shows when a predictor matters for some responses and not
others. Give the first response a weak dependence on @math{x₂} and leave the
second without one:

@examples[#:eval ev #:label #f
(define Y2
  (for/list ([row (in-list X)])
    (define x1 (car row))
    (define x2 (cadr row))
    (list (+ 1 (* 2 x1) (* 0.5 x2))
          (- 10 x1))))
]

Fitting each response on its own with the lasso keeps @math{x₂} for the first
and drops it for the second. The grouped fit keeps @math{x₂} for both, with a
small coefficient where it does not belong:

@examples[#:eval ev #:label #f
(define (round3 x)
  (/ (round (* 1000 x)) 1000))
(define (rounded v)
  (for/list ([b (in-vector v)])
    (round3 b)))
(rounded (elnet-result-coefficients (lasso X (map car Y2) #:lambda 0.05)))
(rounded (elnet-result-coefficients (lasso X (map cadr Y2) #:lambda 0.05)))
(map rounded
     (vector->list
      (mgaussian-result-coefficients (mgaussian-fit X Y2 #:lambda 0.05))))
]

Raise @math{λ} and the grouped fit drops @math{x₂} from both responses at once:

@examples[#:eval ev #:label #f
(map rounded
     (vector->list
      (mgaussian-result-coefficients (mgaussian-fit X Y2 #:lambda 0.2))))
]

@section[#:tag "ex-mgaussian-when"]{When to use it}

Use the multi-response family when several outcomes are measured on the same
units and are expected to depend on the same predictors: several physiological
measurements per subject, or returns on related assets. The shared selection
borrows strength across responses. If the responses depend on different
predictors, fit them separately.

@(close-eval ev)
