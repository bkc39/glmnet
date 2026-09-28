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
(define fit (elastic-net X y #:alpha 0.5 #:lambda 0.5))
(elnet-result-coefficients fit)
]

The fit both shrinks its coefficients, as ridge does, and sets one exactly to
zero, as the lasso does.

@section[#:tag "ex-elastic-net-between"]{Between ridge and lasso}

At a shared @math{λ}, the number of coefficients the elastic net zeroes lies
between ridge, which zeroes none, and the lasso, which zeroes the most:

@examples[#:eval ev #:label #f
(define (zeros fit)
  (for/sum ([b (in-vector (elnet-result-coefficients fit))])
    (if (zero? b) 1 0)))
(map zeros (list (ridge X y #:lambda 0.5)
                 (elastic-net X y #:alpha 0.5 #:lambda 0.5)
                 (lasso X y #:lambda 0.5)))
]

Sweeping @math{α} at that @math{λ} moves between the two:

@examples[#:eval ev #:label #f
(define (round3 x)
  (/ (round (* 1000 x)) 1000))
(define (rounded v)
  (for/list ([b (in-vector v)])
    (round3 b)))
(for ([alpha (in-list '(0.0 0.25 0.5 0.75 1.0))])
  (printf "α = ~a: β = ~a\n"
          alpha
          (rounded (elnet-result-coefficients
                    (elastic-net X y #:alpha alpha #:lambda 0.5)))))
]

@section[#:tag "ex-elastic-net-grouping"]{Correlated predictors}

The case the elastic net was designed for is a group of predictors that carry
the same signal. Take the extreme: duplicate @math{x₁}, so the first two columns
are identical, and drop @math{x₃}:

@examples[#:eval ev #:label #f
(define X-dup
  (for/list ([row (in-list X)])
    (list (car row) (car row) (cadr row))))
(rounded (elnet-result-coefficients (lasso X-dup y #:lambda 0.1)))
(rounded (elnet-result-coefficients (elastic-net X-dup y #:alpha 0.5 #:lambda 0.1)))
]

The lasso puts all of @math{x₁}'s weight on one copy and zeroes the other; which
copy wins is an accident of the column order. The elastic net's L2 term makes
splitting the weight cheaper than concentrating it, so the two copies get
almost equal coefficients. On real data the copies are merely correlated, but
the effect is the same.

@section[#:tag "ex-elastic-net-cv"]{Choosing α and λ}

@racket[elnet-cv] chooses @math{λ} for one @math{α} (@secref["concepts-cv"]).
To choose @math{α} too, cross-validate each candidate on the same folds, so
that the errors differ only because the models do. Take the 40 noisy
observations of @secref["ex-lasso-cv"]:

@examples[#:eval ev #:label #f
(random-seed 3)
(define (uniform a b)
  (+ a (* (- b a) (random))))
(define X40
  (for/list ([i (in-range 40)])
    (define x1 (uniform 0 6))
    (list x1 (uniform 0 6) (* x1 x1))))
(define y40
  (for/list ([row (in-list X40)])
    (+ 1.0 (* 2 (car row)) (- (cadr row)) (uniform -1 1))))
(define folds (random-fold-ids 40))
(for ([alpha (in-list '(0.0 0.25 0.5 0.75 1.0))])
  (define cv (elnet-cv X40 y40 #:alpha alpha #:fold-ids folds))
  (define best (glmnet-cv-index-min cv))
  (printf "α = ~a: λ-min = ~a, error = ~a ± ~a\n"
          alpha
          (round3 (glmnet-cv-lambda-min cv))
          (round3 (vector-ref (glmnet-cv-cvm cv) best))
          (round3 (vector-ref (glmnet-cv-cvsd cv) best))))
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
