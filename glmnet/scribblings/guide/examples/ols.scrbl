#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-ols"]{Ordinary least squares (@math{λ = 0})}

@margin-note{Source: @filepath{glmnet/examples/00-ols.rkt}}

With the penalty strength set to @math{λ = 0}, the elastic net is ordinary
least squares: there is no penalty, so the mixing parameter @math{α} drops out
and glmnet returns the unregularized fit. It is the natural first model because
its answer can be checked exactly.

@section[#:tag "ex-ols-data"]{The data}

The response is an exact linear function of two predictors,
@math{y = 1 + 2x₁ − x₂}, with no error term, so least squares should recover the
intercept @racket[1.0] and the coefficients @racket[2.0] and @racket[-1.0]. The
design matrix is a list of rows, and the response the matching list:

@examples[#:eval ev #:label #f
(define X '((1.0 2.0)
            (2.0 1.0)
            (3.0 4.0)
            (4.0 3.0)
            (5.0 6.0)))
(define y '(1.0 4.0 3.0 6.0 5.0))
]

@section[#:tag "ex-ols-fit"]{Fitting}

@racket[ols] is @racket[elnet-fit] with @racket[#:lambda 0.0]. The fit prints
as a summary; @racket[coef] gives the intercept @math{β₀} and then the
coefficients @math{β}, here shown to five decimals, and @racket[deviance-ratio]
the @math{R²}, here to ten:

@examples[#:eval ev #:label #f
(define fit (ols X y))
fit
(define (~r5 x) (~r x #:precision '(= 5)))
(define (coef-line coefs)
  (match-define (vector β₀ β ...) coefs)
  (format "β₀ = ~a, β = ~a" (~r5 β₀) (map ~r5 β)))
(displayln (coef-line (coef fit)))
(~r (deviance-ratio fit) #:precision '(= 10))
]

The intercept and coefficients match the generating equation to about five
decimal places, and @math{R²} is 1 to within @racket[1e-10]. They are not exact
because coordinate descent stops once a pass changes the objective by less than
@racket[#:thresh]. A tighter threshold costs more passes and gets closer:

@examples[#:eval ev #:label #f
(define tight (ols X y #:thresh 1e-14))
(displayln (coef-line (coef tight)))
(map elnet-result-num-passes (list fit tight))
]

@section[#:tag "ex-ols-fitted"]{Fitted values}

A fitted value is the intercept plus the dot product of a row with the
coefficients. @racket[predict] computes it for each row, here the training
rows, whose fitted values reproduce @racket[y] to the same five decimal
places:

@examples[#:eval ev #:label #f
(map ~r5 (predict fit X))
]

@racket[elnet-predict] is the same computation under the Gaussian family's own
name. On new rows the fit extrapolates the plane it found:

@examples[#:eval ev #:label #f
(map ~r5 (elnet-predict fit '((6.0 5.0) (0.0 0.0))))
]

@section[#:tag "ex-ols-when"]{When to use it}

Least squares is the baseline the penalized models are measured against. It
has no answer to overfitting or to collinear predictors: when two predictors
carry the same information, or when there are more predictors than rows, the
least-squares coefficients are not unique. @secref["ex-ridge"] handles that
case, and @secref["ex-lasso"] handles the case where most predictors are noise.

@(close-eval ev)
