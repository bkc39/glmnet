#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-cox"]{Cox proportional hazards (survival)}

@margin-note{Source: @filepath{glmnet/examples/06-cox.rkt}}

The @tech{Cox family} fits a survival model. Each observation carries a
follow-up @emph{time} and a 0/1 @emph{event indicator}: @racket[1] means the
event (death, failure, churn) was observed at that time, @racket[0] means the
subject was still event-free when observation stopped, so the time is only a
lower bound. That second case is @emph{right-censoring}, and handling it is the
reason not to regress the times directly.

The model is @math{h(t | x) = h₀(t) exp(xβ)}: every subject's hazard is the
same baseline @math{h₀(t)} scaled by @math{exp(xβ)}. The baseline absorbs any
intercept, so Cox fits have none, and @racket[cox-fit] has no
@racket[#:intercept?] keyword. The penalty is the same as for every other
family.

@section[#:tag "ex-cox-data"]{The data}

In this small cohort @math{x₁} is a risk factor: subjects with higher
@math{x₁} have the event sooner, and the three lowest-risk subjects are
censored. @math{x₂} is noise.

@examples[#:eval ev #:label #f
(define X '((0.5 1.0) (1.0 2.0) (1.5 1.0) (2.0 2.0)
            (2.5 1.0) (3.0 2.0) (3.5 1.0) (4.0 2.0)))
(define times '(12.0 10.0 11.0 8.0 6.0 7.0 4.0 3.0))
(define statuses '(0 0 0 1 1 1 1 1))
]

@section[#:tag "ex-cox-fit"]{Fitting}

@examples[#:eval ev #:label #f
(define fit (cox-fit X times statuses #:lambda 0.1))
fit
(coef fit)
]

A Cox fit has no intercept, so @racket[coef] holds only the coefficients.
@math{β₁} is positive, so risk rises with @math{x₁}, and the noise coefficient
is exactly @racket[0.0]. Exponentiated, a coefficient is a @emph{hazard ratio}:
one more unit of @math{x₁} multiplies the hazard at every time by

@examples[#:eval ev #:label #f
(exp (vector-ref (cox-result-coefficients fit) 0))
]

@section[#:tag "ex-cox-risk"]{Ranking by risk}

@racket[cox-linear-predictor] returns @math{xβ} for each row and
@racket[cox-relative-risk] returns @math{exp(xβ)}, the hazard relative to the
baseline. They are @racket[predict] with @racket[#:type 'link] and
@racket[#:type 'response]:

@examples[#:eval ev #:label #f
(cox-linear-predictor fit '((1.0 1.0) (2.0 1.0) (3.0 1.0)))
(cox-relative-risk fit '((1.0 1.0) (2.0 1.0) (3.0 1.0)))
]

Each step of one unit in @math{x₁} multiplies the relative risk by the hazard
ratio above. Because the baseline hazard is not estimated, these numbers rank
subjects against each other; they do not predict a survival time or a survival
curve.

@section[#:tag "ex-cox-path"]{Varying @math{λ}}

@examples[#:eval ev #:label #f
(define (round3 x)
  (/ (round (* 1000 x)) 1000))
(define (rounded v)
  (for/list ([b (in-vector v)])
    (round3 b)))
(for ([lam (in-list '(0.01 0.1 0.5 1.0))])
  (define fit (cox-fit X times statuses #:lambda lam))
  (printf "λ = ~a: β = ~a, dev-ratio = ~a\n"
          lam
          (rounded (cox-result-coefficients fit))
          (round3 (cox-result-dev-ratio fit))))
]

With eight subjects and five events, a small penalty lets the noise predictor
in with a sizeable coefficient; the lasso removes it by @math{λ = 0.1}, and by
@math{λ = 1} it has removed @math{x₁} as well.

@section[#:tag "ex-cox-events"]{Censoring and events}

The partial likelihood is built from the event times, so at least one
observation must be an event:

@examples[#:eval ev #:label #f
(eval:error (cox-fit X times '(0 0 0 0 0 0 0 0) #:lambda 0.1))
]

@section[#:tag "ex-cox-when"]{When to use it}

Use the Cox family for time-to-event outcomes with censoring: patient survival,
machine failure, customer churn. The lasso Cox model is a standard way to pick
a few prognostic factors from many candidates.

@(close-eval ev)
