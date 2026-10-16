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
fit
(coef fit)
(deviance-ratio fit)
]

@racket[coef] gives one vector per response, the intercept first. The
@math{x₁} coefficient is positive for the first response and negative for the
second, and @math{x₂} is exactly @racket[0.0] in both. @racket[deviance-ratio]
is a single @math{R²} covering all the responses together.

@section[#:tag "ex-mgaussian-predict"]{Predicting}

@racket[predict] returns one list per row, with one prediction per response;
@racket[mgaussian-predict] is the same call:

@examples[#:eval ev #:label #f
(predict fit '((7.0 1.0) (8.0 2.0)))
]

@section[#:tag "ex-mgaussian-grouped"]{Grouped versus separate fits}

What the grouping buys shows when a predictor matters for some responses and not
others. Give the first response a weak dependence on @math{x₂} and leave the
second without one:

@examples[#:eval ev #:label #f
(define Y2
  (map (match-lambda
         [(list x₁ x₂)
          (define y₁ (+ 1 (* 2 x₁) (* 0.5 x₂)))
          (list y₁ (- 10 x₁))])
       X))
]

Fitting each response on its own with the lasso keeps @math{x₂} for the first
and drops it for the second. The grouped fit keeps @math{x₂} for both, with a
small coefficient where it does not belong. Each fit's coefficients are shown
to three decimals, the intercept @math{β₀} first and then @math{β}:

@examples[#:eval ev #:label #f
(define (show-coefs coefs)
  (for-each (compose displayln coef-line) (sequence->list coefs)))
(match-define (list y₁ y₂) (apply map list Y2))
(define separate (list (lasso X y₁ #:lambda 0.05) (lasso X y₂ #:lambda 0.05)))
(show-coefs (map coef separate))
]

@examples[#:eval ev #:label #f
(define grouped (mgaussian-fit X Y2 #:lambda 0.05))
(show-coefs (coef grouped))
]

Raise @math{λ} and the grouped fit drops @math{x₂} from both responses at once:

@examples[#:eval ev #:label #f
(define grouped-0.2 (mgaussian-fit X Y2 #:lambda 0.2))
(show-coefs (coef grouped-0.2))
]

@section[#:tag "ex-mgaussian-when"]{When to use it}

Use the multi-response family when several outcomes are measured on the same
units and are expected to depend on the same predictors: several physiological
measurements per subject, or returns on related assets. The shared selection
borrows strength across responses. If the responses depend on different
predictors, fit them separately.

@(close-eval ev)
