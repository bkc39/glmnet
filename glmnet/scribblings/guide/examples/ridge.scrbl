#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-ridge"]{Ridge regression (L2, @math{α = 0})}

@margin-note{Source: @filepath{glmnet/examples/01-ridge.rkt}}

Ridge regression is the elastic net at @math{α = 0}: a pure L2 penalty. It
shrinks every coefficient smoothly toward zero, more as @math{λ} grows, but
never sets one exactly to zero. At very large @math{λ} all the coefficients
vanish and only the intercept, the mean of the response, remains.

@section[#:tag "ex-ridge-data"]{The data}

The fixture from @secref["ex-ols"], extended by one row and one irrelevant
predictor, @math{x₃ = x₁²}. The response is still exactly
@math{y = 1 + 2x₁ − x₂}, so @math{x₃} carries no signal, but it is strongly
correlated with @math{x₁}:

@examples[#:eval ev #:label #f
(define X '((1.0 2.0  1.0)
            (2.0 1.0  4.0)
            (3.0 4.0  9.0)
            (4.0 3.0 16.0)
            (5.0 6.0 25.0)
            (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))
]

@section[#:tag "ex-ridge-fit"]{Fitting}

@racket[ridge] is @racket[elnet-fit] with @racket[#:alpha 0.0]:

@examples[#:eval ev #:label #f
(define fit (ridge X y #:lambda 0.1))
(elnet-result-coefficients fit)
(elnet-result-r-squared fit)
]

Least squares would give @math{x₃} a coefficient of zero. Ridge gives it a small
nonzero weight instead, and pulls @math{x₁} down from @racket[2.0]: the penalty
prefers to spread weight across correlated predictors rather than concentrate
it. That is the characteristic ridge behaviour: shrink, never select.

@section[#:tag "ex-ridge-path"]{The shrinkage path}

Refitting over a range of @math{λ} traces how the coefficients shrink:

@examples[#:eval ev #:label #f
(define (round3 x)
  (/ (round (* 1000 x)) 1000))
(define (rounded v)
  (for/list ([b (in-vector v)])
    (round3 b)))
(for ([lam (in-list '(0.01 0.1 1.0 10.0 100.0))])
  (define fit (ridge X y #:lambda lam))
  (printf "λ = ~a: β₀ = ~a, β = ~a\n"
          lam
          (round3 (elnet-result-intercept fit))
          (rounded (elnet-result-coefficients fit))))
]

Every coefficient heads toward zero and none reaches it, while the intercept
heads toward the mean of @racket[y], which is @racket[4.5]. Along the way the
coefficient on @math{x₂} changes sign. As @math{λ} grows, each ridge coefficient
approaches a scaled-down copy of its predictor's covariance with the response,
taken on its own; @math{x₂} rises with @math{x₁}, so on its own it is
positively correlated with @racket[y], even though its coefficient in the
generating equation is negative.

@section[#:tag "ex-ridge-when"]{When to use it}

Reach for ridge when many predictors each carry a little signal, or when
predictors are strongly correlated, and you want a stable fit rather than a
short list of predictors. It never drops a predictor; when you want the fit to
say which predictors matter, use @secref["ex-lasso"] or
@secref["ex-elastic-net"].

@(close-eval ev)
