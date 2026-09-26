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
(elnet-result-coefficients fit)
]

The third coefficient is exactly @racket[0.0]: the irrelevant predictor has been
selected out, while @math{x₁} and @math{x₂} keep their signs, a little shrunk.
To list the predictors a fit kept:

@examples[#:eval ev #:label #f
(for/list ([b (in-vector (elnet-result-coefficients fit))]
           [j (in-naturals 1)]
           #:unless (zero? b))
  j)
]

@section[#:tag "ex-lasso-path"]{The selection path}

@racket[elnet-path] fits a list of @math{λ} in one call, largest first
(@secref["concepts-path"]). Read upward, each larger @math{λ} removes
another predictor:

@examples[#:eval ev #:label #f
(define (round3 x)
  (/ (round (* 1000 x)) 1000))
(define (rounded v)
  (for/list ([b (in-vector v)])
    (round3 b)))
(define path (elnet-path X y #:lambda '(0.01 0.05 0.2 0.5 1.0 2.5)))
(for ([lam (in-vector (glmnet-path-lambda path))]
      [beta (in-vector (glmnet-path-coefficients path))]
      [r2 (in-vector (glmnet-path-dev-ratio path))])
  (printf "λ = ~a: β = ~a, R² = ~a\n" lam (rounded beta) (round3 r2)))
]

At @math{λ = 0.01} all three predictors are in (the noise just barely);
by @math{λ = 0.05} @math{x₃} is out; by @math{λ = 0.5} @math{x₂} is out; and
at @math{λ = 2.5} nothing is left. Once every coefficient is zero the fit is the
intercept alone, which is the mean of the response:

@examples[#:eval ev #:label #f
(elnet-result-intercept (lasso X y #:lambda 2.5))
]

Choosing @math{λ} well is a job for cross-validation, which the bindings do not
provide yet (@hyperlink["https://github.com/bkc39/glmnet/issues/27"]{#27}).

@section[#:tag "ex-lasso-when"]{When to use it}

Use the lasso when you expect a sparse answer and want an interpretable list of
predictors. Its weakness is correlated predictors: among a group that carries
the same signal it tends to keep one arbitrarily and drop the rest.
@secref["ex-elastic-net"] shows the problem and the fix.

@(close-eval ev)
