#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-lasso"]{Lasso (L1, @math{α = 1})}

@margin-note{Source: @filepath{glmnet/examples/02-lasso.rkt}}

The lasso is the elastic net at @math{α = 1}: a pure L1 penalty. Where ridge
shrinks every coefficient but keeps them all, the lasso performs
@emph{variable selection}: it drives coefficients exactly to zero, more of them
as @math{λ} grows. It is the model to reach for when you believe only a few
predictors matter and you want the fit to say which.

@section[#:tag "ex-lasso-data"]{The data}

The same fixture as @secref["ex-ridge"]: the response is exactly
@math{y = 1 + 2x₁ − x₂}, and @math{x₃ = x₁²} is irrelevant.

@examples[#:eval ev #:label #f
(define X '((1.0 2.0  1.0)
            (2.0 1.0  4.0)
            (3.0 4.0  9.0)
            (4.0 3.0 16.0)
            (5.0 6.0 25.0)
            (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))
]

@section[#:tag "ex-lasso-fit"]{Fitting}

@racket[lasso] is @racket[elnet-fit] with @racket[#:alpha 1.0]:

@examples[#:eval ev #:label #f
(define fit (lasso X y #:lambda 0.05))
(coef fit)
]

The coefficient of @math{x₃} is exactly @racket[0.0]: the irrelevant predictor
has been selected out, while @math{x₁} and @math{x₂} keep their signs, a little
shrunk. The predictors a fit kept are those with a nonzero coefficient.
@racket[coef] gives the intercept first, so the coefficients start at index 1,
which is also the number of their predictor:

@examples[#:eval ev #:label #f
(define kept
  (for/list ([b (in-vector (coef fit) 1)]
             [j (in-naturals 1)]
             #:unless (zero? b))
    j))
kept
]

@section[#:tag "ex-lasso-path"]{The selection path}

@racket[elnet-path] fits a list of @math{λ} in one call, largest first
(@secref["concepts-path"]). @racket[in-path] walks it, giving each @math{λ}
with its coefficients, the intercept @math{β₀} first and then @math{β}, and
@racket[deviance-ratio] gives the path's @math{R²} at each @math{λ}, all shown
to three decimals. Read upward, raising @math{λ} removes predictors one at a
time:

@examples[#:eval ev #:label #f
(define (~r3 x) (~r x #:precision '(= 3)))
(define (coef-line coefs)
  (match-define (vector β₀ β ...) coefs)
  (format "β₀ = ~a, β = ~a" (~r3 β₀) (map ~r3 β)))
(define path (elnet-path X y #:lambda '(0.01 0.05 0.2 0.5 1.0 2.5)))
(for ([(λ coefs) (in-path path)]
      [r² (in-vector (deviance-ratio path))])
  (printf "λ = ~a: ~a, R² = ~a\n" λ (coef-line coefs) (~r3 r²)))
]

At @math{λ = 0.01} all three predictors are in (the noise just barely);
by @math{λ = 0.05} @math{x₃} is out; by @math{λ = 0.5} @math{x₂} is out; and
at @math{λ = 2.5} nothing is left. Once every coefficient is zero the fit is the
intercept alone, which is the mean of the response:

@examples[#:eval ev #:label #f
(coef (lasso X y #:lambda 2.5))
]

@racket[plot-coefficient-path], from @racketmodname[glmnet/plot], draws the
same path as R's @tt{plot(fit, label = TRUE)} does (@secref["plot-path"]):
one curve per predictor against @math{−log λ}, so that the largest @math{λ} is
at the left, with the count of nonzero coefficients along the top:

@examples[#:eval ev #:label #f
(require glmnet/plot)
(plot-coefficient-path path #:label #t)
]

Read from the left, the plot is the table read from the top: @math{x₁} enters
at @math{λ = 1}, @math{x₂} at @math{λ = 0.2}, and @math{x₃} only at the
smallest @math{λ}, @math{0.01}, with a coefficient too small to lift its curve
visibly off zero, although the count along the top reaches 3.

@section[#:tag "ex-lasso-cv"]{Choosing λ by cross-validation}

Six observations without noise are no test of @math{λ}: with no noise to
overfit, the smallest @math{λ} predicts best. With noisy data, cross-validation
chooses @math{λ} by how well each fit predicts observations it did not see
(@secref["concepts-cv"]). Here are 40 observations of the same model, with
uniform noise on @math{[−1, 1]} added to @math{y}, generated as named columns:

@noisy-fixture[ev #:label #f]

@racket[elnet-cv] cross-validates the lasso path on them, and @racket[coef]
reads the coefficients at either λ it chose:

@examples[#:eval ev #:label #f
(define cv (elnet-cv X40 y40))
cv
(printf "lambda-min: ~a\n" (coef-line (coef cv #:lambda 'lambda-min)))
(printf "lambda-1se: ~a\n" (coef-line (coef cv)))
]

Both choices keep @math{x₁} and @math{x₂} and drop @math{x₃}. @racket[coef]
defaults to @tech{lambda-1se}, the larger @math{λ}, whose coefficients are
shrunk further from the true @math{2} and @math{−1} in exchange for a model the
data cannot tell apart from the best one.

@racket[plot-cv] draws the cross-validated error as R's @tt{plot(cvfit)} does
(@secref["plot-cv"]):

@examples[#:eval ev #:label #f
(plot-cv cv)
]

The red points are the error at each @math{λ}, with bars one standard error
either side, and the dotted lines mark @tech{lambda-min}, at the right, and
@tech{lambda-1se}, at the left. From @tech{lambda-1se} rightwards every
point lies within one standard error of the smallest error.

@section[#:tag "ex-lasso-when"]{When to use it}

Use the lasso when you expect a sparse answer and want an interpretable list of
predictors. Its weakness is correlated predictors: among a group that carries
the same signal it tends to keep one arbitrarily and drop the rest.
@secref["ex-elastic-net"] shows the problem and the fix.

@(close-eval ev)
