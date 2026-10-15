#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-elastic-net"]{Elastic net (@math{0 < α < 1})}

@margin-note{Source: @filepath{glmnet/examples/03-elastic-net.rkt}}

The elastic net blends the two penalties: with @math{0 < α < 1} the objective
mixes the lasso's L1 term with the ridge's L2 term. It keeps the lasso's ability
to select predictors and borrows the ridge's stability. In particular it tends
to keep or drop @emph{groups} of correlated predictors together, where the lasso
would pick one of them arbitrarily. @math{α = 0} recovers @secref["ex-ridge"]
and @math{α = 1} recovers @secref["ex-lasso"].

@section[#:tag "ex-elastic-net-fit"]{Fitting}

The running fixture again, with the irrelevant @math{x₃ = x₁²}:

@examples[#:eval ev #:label #f
(define X '((1.0 2.0  1.0)
            (2.0 1.0  4.0)
            (3.0 4.0  9.0)
            (4.0 3.0 16.0)
            (5.0 6.0 25.0)
            (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))
]

@racket[elastic-net] takes an explicit @racket[#:alpha]:

@examples[#:eval ev #:label #f
(define fit (elastic-net X y #:alpha 0.5 #:lambda 0.5))
(coef fit)
]

The fit both shrinks its coefficients, as ridge does, and sets one exactly to
zero, as the lasso does.

@section[#:tag "ex-elastic-net-between"]{Between ridge and lasso}

At a shared @math{λ}, the number of coefficients the elastic net zeroes lies
between ridge, which zeroes none, and the lasso, which zeroes the most.
@racket[coef] gives the intercept first, and the count skips it:

@examples[#:eval ev #:label #f
(define (zeros model)
  (match-define (vector _ β ...) (coef model))
  (count zero? β))
(map zeros (list (ridge X y #:lambda 0.5)
                 fit
                 (lasso X y #:lambda 0.5)))
]

Sweeping @math{α} at that @math{λ} moves between the two. Each line shows the
intercept @math{β₀} and then @math{β}, to three decimals:

@examples[#:eval ev #:label #f
(define (~r3 x) (~r x #:precision '(= 3)))
(define (coef-line coefs)
  (match-define (vector β₀ β ...) coefs)
  (format "β₀ = ~a, β = ~a" (~r3 β₀) (map ~r3 β)))
(for ([α (in-list '(0.0 0.25 0.5 0.75 1.0))])
  (printf "α = ~a: ~a\n" α (coef-line (coef (elastic-net X y #:alpha α #:lambda 0.5)))))
]

@section[#:tag "ex-elastic-net-grouping"]{Correlated predictors}

The case the elastic net was designed for is a group of predictors that carry
the same signal. Take the extreme: duplicate @math{x₁}, so the first two columns
are identical, and drop @math{x₃}:

@examples[#:eval ev #:label #f
(define X-dup
  (map (match-lambda [(list x₁ x₂ _) (list x₁ x₁ x₂)]) X))
(printf "lasso:       ~a\n" (coef-line (coef (lasso X-dup y #:lambda 0.1))))
(printf "elastic net: ~a\n"
        (coef-line (coef (elastic-net X-dup y #:alpha 0.5 #:lambda 0.1))))
]

The lasso puts all of @math{x₁}'s weight on one copy and leaves the other
within rounding of zero, about @racket[1e-14]: the two identical columns tie
exactly at this @math{λ}, and which copy wins is an accident of the column
order. The elastic net's L2 term makes
splitting the weight cheaper than concentrating it, so the two copies get
almost equal coefficients. On real data the copies are merely correlated, but
the effect is the same.

@section[#:tag "ex-elastic-net-cv"]{Choosing α and λ}

@racket[elnet-cv] chooses @math{λ} for one @math{α} (@secref["concepts-cv"]).
To choose @math{α} too, cross-validate each candidate on the same folds, so
that the errors differ only because the models do. Take the 40 noisy
observations @racket[X40] and @racket[y40] of @secref["ex-lasso-cv"]:

@noisy-fixture[ev #:hidden]

@examples[#:eval ev #:label #f
(define folds (random-fold-ids 40))
(for ([α (in-list '(0.0 0.25 0.5 0.75 1.0))])
  (define cv (elnet-cv X40 y40 #:alpha α #:fold-ids folds))
  (define best (glmnet-cv-index-min cv))
  (printf "α = ~a: λ-min = ~a, error = ~a ± ~a\n"
          α
          (~r3 (glmnet-cv-lambda-min cv))
          (~r3 (vector-ref (glmnet-cv-cvm cv) best))
          (~r3 (vector-ref (glmnet-cv-cvsd cv) best))))
]

Ridge, which keeps every predictor, the irrelevant @math{x₃} among them,
predicts clearly worse. Every @math{α} above 0 reaches nearly the same error,
well within one standard error of each other, so on these data the choice
among them matters much less than the choice of @math{λ}.

@section[#:tag "ex-elastic-net-when"]{When to use it}

The elastic net is a good default when you want selection but expect groups of
correlated predictors, as with gene expression, sensor arrays or
technical indicators. @math{α} is a second tuning parameter alongside
@math{λ}; values around @racket[0.5] are a common starting point, and
cross-validation on shared folds chooses between candidates.

@(close-eval ev)
