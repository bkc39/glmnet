#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-poisson"]{Poisson regression (counts)}

@margin-note{Source: @filepath{glmnet/examples/07-poisson.rkt}}

The @tech{Poisson family} models @emph{counts}: non-negative responses such as
events per interval. It uses a log link, so the fitted mean is
@math{μ = exp(β₀ + xβ)} and each predictor @emph{multiplies} the expected count
rather than adding to it. Unlike Cox, Poisson fits an intercept, and the
penalty is the same as for every other family.

@section[#:tag "ex-poisson-data"]{The data}

The counts grow roughly like @math{exp(0.3 x₁)}; @math{x₂} is noise:

@examples[#:eval ev #:label #f
(define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0)
            (5.0 2.0) (6.0 1.0) (7.0 2.0) (8.0 1.0)))
(define y '(1 2 2 3 4 6 8 11))
]

@section[#:tag "ex-poisson-fit"]{Fitting}

@examples[#:eval ev #:label #f
(define fit (poisson-fit X y #:lambda 0.2))
fit
(coef fit)
]

After the intercept, @math{β₁} is close to the @racket[0.3] the counts were
built around, and the noise coefficient is exactly @racket[0.0]. The coefficients are on the log
scale; exponentiated, @math{β₁} is a @emph{rate ratio}, the factor by which one
more unit of @math{x₁} multiplies the expected count:

@examples[#:eval ev #:label #f
(exp (vector-ref (poisson-result-coefficients fit) 0))
]

@section[#:tag "ex-poisson-predict"]{Predicting}

@racket[poisson-predict-mean] applies the log link and returns the fitted mean
for each row; it is @racket[predict] with @racket[#:type 'response]. Next to
the observed counts:

@examples[#:eval ev #:label #f
(for/list ([mu (in-list (poisson-predict-mean fit X))]
           [count (in-list y)])
  (list count mu))
]

@section[#:tag "ex-poisson-response"]{The response}

The response must be non-negative, but it need not be an integer, so rates such
as events per hour fit too:

@examples[#:eval ev #:label #f
(poisson-result-coefficients
 (poisson-fit X '(0.5 1.0 1.2 1.5 2.0 3.1 4.0 5.6) #:lambda 0.1))
]

When observations cover different exposures (different lengths of time,
different population sizes), R's glmnet takes an @tt{offset} of
@math{log(exposure)}. The bindings do not support offsets yet
(@hyperlink["https://github.com/bkc39/glmnet/issues/12"]{#12}).

@section[#:tag "ex-poisson-when"]{When to use it}

Use the Poisson family for counts: defects per batch, visits per day, claims per
policy. The model assumes a count's variance equals its mean; counts that vary
much more than that (overdispersion) are a poor fit for it.

@(close-eval ev)
