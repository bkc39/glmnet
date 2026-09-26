#lang scribble/manual
@(require "utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "reference"]{Reference}

@declare-exporting[glmnet]

Every binding below is provided by @racketmodname[glmnet]. Each model family
has a fit procedure, a transparent result struct and, for all but the Gaussian
family, prediction helpers; @secref["concepts"] explains how they fit together.

@section[#:tag "ref-common"]{Common arguments}

The fit procedures share their argument conventions:

@itemlist[
 @item{@racket[X] is a @tech{design matrix}: a non-empty list of equal-length
       rows of reals, one row per observation. Prediction helpers take new rows
       in the same layout, each with as many entries as the fit has
       coefficients.}
 @item{@racket[#:lambda] is the penalty strength @math{λ ≥ 0}. It is
       required.}
 @item{@racket[#:alpha] is the mixing parameter @math{α ∈ [0, 1]}:
       @racket[0.0] is the ridge penalty, @racket[1.0] (the default) the
       lasso, and values in between the elastic net.}
 @item{@racket[#:standardize?] scales each predictor to unit variance before
       fitting; coefficients are always reported on the original scale.}
 @item{@racket[#:intercept?] fits an unpenalized intercept; @racket[#f] fixes
       it at zero.}
 @item{@racket[#:thresh] is the coordinate-descent convergence threshold and
       @racket[#:max-iters] the maximum number of passes.}
]

Before the solver runs, an argument of the wrong type raises
@racket[exn:fail:contract], and rows or responses of mismatched length raise
@racket[exn:fail]. A fatal condition reported by glmnet
raises @racket[exn:fail] with a readable message. If the solver reaches
@racket[#:max-iters] without converging, the fit returns partial coefficients
and logs a warning.

@section[#:tag "ref-gaussian"]{Gaussian models}

The @tech{Gaussian family}. The four named models are @racket[elnet-fit] with a
fixed @racket[#:alpha]. See @secref["ex-ols"], @secref["ex-ridge"],
@secref["ex-lasso"] and @secref["ex-elastic-net"].

@defstruct*[elnet-result ([intercept real?]
                          [coefficients (vectorof real?)]
                          [r-squared real?]
                          [lambda real?]
                          [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted Gaussian model. @racket[coefficients] has one entry per column of
  @racket[_X], on the original predictor scale; @racket[r-squared] is the
  fraction of variance explained; @racket[lambda] is the penalty the solver
  used; @racket[num-passes] is the number of coordinate-descent passes.

  @examples[#:eval ev
  (define X '((1.0 2.0 1.0) (2.0 1.0 4.0) (3.0 4.0 9.0)
              (4.0 3.0 16.0) (5.0 6.0 25.0) (6.0 5.0 36.0)))
  (define y '(1.0 4.0 3.0 6.0 5.0 8.0))
  (define fit (lasso X y #:lambda 0.05))
  (elnet-result-intercept fit)
  (elnet-result-coefficients fit)]}

@defproc[(elnet-fit [X (and/c (listof (listof real?)) pair?)]
                    [y (and/c (listof real?) pair?)]
                    [#:lambda lambda (>=/c 0)]
                    [#:alpha alpha (real-in 0 1) 1.0]
                    [#:standardize? standardize? boolean? #t]
                    [#:intercept? intercept? boolean? #t]
                    [#:thresh thresh (>/c 0) 1e-7]
                    [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Fits a Gaussian elastic-net model of the response @racket[y], one real per
  row of @racket[X], at a single @racket[lambda].

  @examples[#:eval ev
  (elnet-fit X y #:alpha 0.5 #:lambda 0.5)]}

@defproc[(ols [X (and/c (listof (listof real?)) pair?)]
              [y (and/c (listof real?) pair?)]
              [#:standardize? standardize? boolean? #t]
              [#:intercept? intercept? boolean? #t]
              [#:thresh thresh (>/c 0) 1e-10]
              [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Ordinary least squares: @racket[elnet-fit] with @racket[#:lambda 0.0], where
  @racket[#:alpha] has no effect. The default @racket[thresh] is tighter than
  the penalized fits' because coordinate descent approaches the unpenalized
  solution slowly.

  @examples[#:eval ev
  (elnet-result-coefficients
   (ols '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0) (5.0 6.0))
        '(1.0 4.0 3.0 6.0 5.0)))]}

@defproc[(ridge [X (and/c (listof (listof real?)) pair?)]
                [y (and/c (listof real?) pair?)]
                [#:lambda lambda (>=/c 0)]
                [#:standardize? standardize? boolean? #t]
                [#:intercept? intercept? boolean? #t]
                [#:thresh thresh (>/c 0) 1e-7]
                [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Ridge regression: @racket[elnet-fit] with @racket[#:alpha 0.0]. Shrinks
  every coefficient toward zero as @racket[lambda] grows, but sets none exactly
  to zero.

  @examples[#:eval ev
  (elnet-result-coefficients (ridge X y #:lambda 0.1))]}

@defproc[(lasso [X (and/c (listof (listof real?)) pair?)]
                [y (and/c (listof real?) pair?)]
                [#:lambda lambda (>=/c 0)]
                [#:standardize? standardize? boolean? #t]
                [#:intercept? intercept? boolean? #t]
                [#:thresh thresh (>/c 0) 1e-7]
                [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  The lasso: @racket[elnet-fit] with @racket[#:alpha 1.0]. Sets coefficients
  exactly to zero, more of them as @racket[lambda] grows.

  @examples[#:eval ev
  (elnet-result-coefficients (lasso X y #:lambda 0.5))]}

@defproc[(elastic-net [X (and/c (listof (listof real?)) pair?)]
                      [y (and/c (listof real?) pair?)]
                      [#:alpha alpha (real-in 0 1)]
                      [#:lambda lambda (>=/c 0)]
                      [#:standardize? standardize? boolean? #t]
                      [#:intercept? intercept? boolean? #t]
                      [#:thresh thresh (>/c 0) 1e-7]
                      [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  The elastic net at an explicit @racket[alpha], which is required here.
  @racket[#:alpha 0.0] is @racket[ridge] and @racket[#:alpha 1.0] is
  @racket[lasso].

  @examples[#:eval ev
  (elnet-result-coefficients (elastic-net X y #:alpha 0.5 #:lambda 0.5))]}

@section[#:tag "ref-binomial"]{Binomial models}

The @tech{binomial family}: two-class logistic regression on 0/1 labels. See
@secref["ex-logistic"].

@defstruct*[logistic-result ([intercept real?]
                             [coefficients (vectorof real?)]
                             [dev-ratio real?]
                             [lambda real?]
                             [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted two-class model. @racket[intercept] and @racket[coefficients] are
  on the log-odds scale for class 1; @racket[dev-ratio] is the fraction of null
  deviance explained.

  @examples[#:eval ev
  (define X '((1.0 5.0 2.0) (2.0 6.0 1.0) (2.0 5.0 3.0) (1.0 4.0 1.0)
              (6.0 2.0 2.0) (5.0 1.0 1.0) (6.0 1.0 3.0) (5.0 2.0 1.0)))
  (define y '(0 0 0 0 1 1 1 1))
  (define fit (logistic-fit X y #:lambda 0.05))
  (logistic-result-coefficients fit)
  (logistic-result-dev-ratio fit)]}

@defproc[(logistic-fit [X (and/c (listof (listof real?)) pair?)]
                       [y (and/c (listof (or/c 0 1)) pair?)]
                       [#:lambda lambda (>=/c 0)]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? #t]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         logistic-result?]{
  Fits a two-class logistic elastic-net model of the labels @racket[y]. If a
  class probability collapses, typically under perfect separation, the
  @exnraise[exn:fail]; a larger @racket[lambda] usually fixes it.

  @examples[#:eval ev
  (logistic-fit X y #:lambda 0.05)]}

@defproc[(logistic-predict-proba [fit logistic-result?]
                                 [X (and/c (listof (listof real?)) pair?)])
         (listof (real-in 0 1))]{
  The class-1 probability @math{1 / (1 + exp(−(β₀ + xβ)))} for each row of
  @racket[X].

  @examples[#:eval ev
  (logistic-predict-proba fit '((2.0 5.0 2.0) (5.0 2.0 2.0)))]}

@defproc[(logistic-predict [fit logistic-result?]
                           [X (and/c (listof (listof real?)) pair?)]
                           [#:threshold threshold (real-in 0 1) 0.5])
         (listof (or/c 0 1))]{
  Hard labels: @racket[1] where @racket[logistic-predict-proba] is at least
  @racket[threshold], otherwise @racket[0].

  @examples[#:eval ev
  (logistic-predict fit '((2.0 5.0 2.0) (5.0 2.0 2.0)))]}

@section[#:tag "ref-multinomial"]{Multinomial models}

The @tech{multinomial family}: @math{K}-class logistic regression on integer
labels. See @secref["ex-multinomial"].

@defstruct*[multinomial-result ([intercepts (vectorof real?)]
                                [coefficients (vectorof (vectorof real?))]
                                [dev-ratio real?]
                                [lambda real?]
                                [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted @math{K}-class model. @racket[intercepts] holds one real per class
  and @racket[coefficients] one coefficient vector per class, each with one
  entry per predictor. @racket[dev-ratio] is the fraction of null deviance
  explained.

  @examples[#:eval ev
  (define X '((1.0 1.0) (2.0 1.0) (5.0 1.0) (6.0 1.0) (3.0 5.0) (4.0 6.0)))
  (define y '(0 0 1 1 2 2))
  (define fit (multinomial-fit X y #:lambda 0.05))
  (multinomial-result-coefficients fit)]}

@defproc[(multinomial-fit [X (and/c (listof (listof real?)) pair?)]
                          [y (and/c (listof exact-nonnegative-integer?) pair?)]
                          [#:lambda lambda (>=/c 0)]
                          [#:alpha alpha (real-in 0 1) 1.0]
                          [#:standardize? standardize? boolean? #t]
                          [#:intercept? intercept? boolean? #t]
                          [#:thresh thresh (>/c 0) 1e-7]
                          [#:max-iters max-iters exact-positive-integer? 100000])
         multinomial-result?]{
  Fits a @math{K}-class multinomial elastic-net model. The labels @racket[y]
  must cover @racket[0] to @math{K−1} with every class present; otherwise the
  @exnraise[exn:fail]. As with @racket[logistic-fit], a collapsed class
  probability raises @racket[exn:fail].

  @examples[#:eval ev
  (multinomial-result-intercepts (multinomial-fit X y #:lambda 0.05))
  (eval:error (multinomial-fit X '(0 0 2 2 2 2) #:lambda 0.05))]}

@defproc[(multinomial-predict-proba [fit multinomial-result?]
                                    [X (and/c (listof (listof real?)) pair?)])
         (listof (listof (real-in 0 1)))]{
  The softmax class probabilities for each row of @racket[X]: one list of
  @math{K} entries, summing to 1, per row.

  @examples[#:eval ev
  (multinomial-predict-proba fit '((1.5 1.0) (3.5 5.5)))]}

@defproc[(multinomial-predict [fit multinomial-result?]
                              [X (and/c (listof (listof real?)) pair?)])
         (listof exact-nonnegative-integer?)]{
  The most probable class for each row of @racket[X].

  @examples[#:eval ev
  (multinomial-predict fit '((1.5 1.0) (5.5 1.0) (3.5 5.5)))]}

@section[#:tag "ref-cox"]{Cox models}

The @tech{Cox family}: proportional-hazards survival models. There is no
intercept, and so no @racket[#:intercept?] keyword. See @secref["ex-cox"].

@defstruct*[cox-result ([coefficients (vectorof real?)]
                        [dev-ratio real?]
                        [lambda real?]
                        [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted Cox model. @racket[coefficients] are on the log-hazard-ratio scale;
  @racket[dev-ratio] is the fraction of null partial-likelihood deviance
  explained.

  @examples[#:eval ev
  (define X '((0.5 1.0) (1.0 2.0) (1.5 1.0) (2.0 2.0)
              (2.5 1.0) (3.0 2.0) (3.5 1.0) (4.0 2.0)))
  (define times '(12.0 10.0 11.0 8.0 6.0 7.0 4.0 3.0))
  (define statuses '(0 0 0 1 1 1 1 1))
  (define fit (cox-fit X times statuses #:lambda 0.1))
  (cox-result-coefficients fit)]}

@defproc[(cox-fit [X (and/c (listof (listof real?)) pair?)]
                  [times (and/c (listof (>/c 0)) pair?)]
                  [statuses (and/c (listof (or/c 0 1)) pair?)]
                  [#:lambda lambda (>=/c 0)]
                  [#:alpha alpha (real-in 0 1) 1.0]
                  [#:standardize? standardize? boolean? #t]
                  [#:thresh thresh (>/c 0) 1e-7]
                  [#:max-iters max-iters exact-positive-integer? 100000])
         cox-result?]{
  Fits a Cox proportional-hazards elastic-net model. @racket[times] are
  positive follow-up times and @racket[statuses] the matching event indicators:
  @racket[1] for an observed event, @racket[0] for right-censoring. If no
  status is @racket[1], the @exnraise[exn:fail].

  @examples[#:eval ev
  (cox-fit X times statuses #:lambda 0.5)
  (eval:error (cox-fit X times '(0 0 0 0 0 0 0 0) #:lambda 0.1))]}

@defproc[(cox-linear-predictor [fit cox-result?]
                               [X (and/c (listof (listof real?)) pair?)])
         (listof real?)]{
  The log relative hazard @math{xβ} for each row of @racket[X].

  @examples[#:eval ev
  (cox-linear-predictor fit '((1.0 1.0) (3.0 1.0)))]}

@defproc[(cox-relative-risk [fit cox-result?]
                            [X (and/c (listof (listof real?)) pair?)])
         (listof (>/c 0))]{
  The relative risk @math{exp(xβ)} for each row of @racket[X]: the factor by
  which the row's hazard exceeds the baseline hazard.

  @examples[#:eval ev
  (cox-relative-risk fit '((1.0 1.0) (3.0 1.0)))]}

@section[#:tag "ref-poisson"]{Poisson models}

The @tech{Poisson family}: counts with a log link. See @secref["ex-poisson"].

@defstruct*[poisson-result ([intercept real?]
                            [coefficients (vectorof real?)]
                            [dev-ratio real?]
                            [lambda real?]
                            [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted Poisson model. @racket[intercept] and @racket[coefficients] are on
  the log-mean scale; @racket[dev-ratio] is the fraction of null deviance
  explained.

  @examples[#:eval ev
  (define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0)
              (5.0 2.0) (6.0 1.0) (7.0 2.0) (8.0 1.0)))
  (define y '(1 2 2 3 4 6 8 11))
  (define fit (poisson-fit X y #:lambda 0.2))
  (poisson-result-intercept fit)
  (poisson-result-coefficients fit)]}

@defproc[(poisson-fit [X (and/c (listof (listof real?)) pair?)]
                      [y (and/c (listof (>=/c 0)) pair?)]
                      [#:lambda lambda (>=/c 0)]
                      [#:alpha alpha (real-in 0 1) 1.0]
                      [#:standardize? standardize? boolean? #t]
                      [#:intercept? intercept? boolean? #t]
                      [#:thresh thresh (>/c 0) 1e-7]
                      [#:max-iters max-iters exact-positive-integer? 100000])
         poisson-result?]{
  Fits a Poisson elastic-net model of the non-negative response @racket[y],
  which need not be integral.

  @examples[#:eval ev
  (poisson-fit X y #:lambda 0.5)]}

@defproc[(poisson-predict-mean [fit poisson-result?]
                               [X (and/c (listof (listof real?)) pair?)])
         (listof (>/c 0))]{
  The fitted mean @math{exp(β₀ + xβ)} for each row of @racket[X].

  @examples[#:eval ev
  (poisson-predict-mean fit '((2.0 1.0) (9.0 1.0)))]}

@section[#:tag "ref-mgaussian"]{Multi-response Gaussian models}

The @tech{multi-response Gaussian family}: several numeric responses fitted
jointly under a grouped penalty. See @secref["ex-mgaussian"].

@defstruct*[mgaussian-result ([intercepts (vectorof real?)]
                              [coefficients (vectorof (vectorof real?))]
                              [r-squared real?]
                              [lambda real?]
                              [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted multi-response model. @racket[intercepts] holds one real per
  response and @racket[coefficients] one coefficient vector per response, each
  with one entry per predictor. @racket[r-squared] is the fraction of variance
  explained across all responses.

  @examples[#:eval ev
  (define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0) (5.0 2.0) (6.0 1.0)))
  (define Y '((3.0 9.0) (5.0 8.0) (7.0 7.0) (9.0 6.0) (11.0 5.0) (13.0 4.0)))
  (define fit (mgaussian-fit X Y #:lambda 0.1))
  (mgaussian-result-intercepts fit)
  (mgaussian-result-coefficients fit)]}

@defproc[(mgaussian-fit [X (and/c (listof (listof real?)) pair?)]
                        [Y (and/c (listof (listof real?)) pair?)]
                        [#:lambda lambda (>=/c 0)]
                        [#:alpha alpha (real-in 0 1) 1.0]
                        [#:standardize? standardize? boolean? #t]
                        [#:intercept? intercept? boolean? #t]
                        [#:thresh thresh (>/c 0) 1e-7]
                        [#:max-iters max-iters exact-positive-integer? 100000])
         mgaussian-result?]{
  Fits a multi-response Gaussian elastic-net model. @racket[Y] is a matrix with
  one row per observation and one column per response. With @racket[alpha]
  above zero, the grouped lasso keeps or drops each predictor for every
  response at once.

  @examples[#:eval ev
  (mgaussian-fit X Y #:lambda 0.5)]}

@defproc[(mgaussian-predict [fit mgaussian-result?]
                            [X (and/c (listof (listof real?)) pair?)])
         (listof (listof real?))]{
  The predictions @math{a0_r + xβ_r} for each row of @racket[X]: one list per
  row, with one entry per response.

  @examples[#:eval ev
  (mgaussian-predict fit '((7.0 1.0) (8.0 2.0)))]}

@section[#:tag "ref-path"]{Regularization paths}

A @tech{regularization path} fits a decreasing sequence of @math{λ} in one call.
Every family has a path fitter, and they all return a @racket[glmnet-path]. Each
fitter takes its family's data arguments and keywords, with @racket[#:lambda]
made optional:

@itemlist[
 @item{@racket[#:lambda] is a list of penalties, fitted largest first. When it
       is @racket[#f] (the default), glmnet chooses the sequence as R does:
       @racket[#:nlambda] values from @math{λ_max}, where every coefficient is
       zero, down to @racket[#:lambda-min-ratio] times @math{λ_max}. The
       ratio defaults to @racket[0.01] when there are fewer observations than
       predictors, and to @racket[1e-4] otherwise.}
 @item{Like R, the path stops early once the deviance ratio improves by less
       than @racket[1e-5] or passes @racket[0.999], so it can hold fewer than
       @racket[#:nlambda] values. The first value of an automatic sequence is
       the extrapolation R's @tt{glmnet} reports.}
]

@defstruct*[glmnet-path ([family (or/c 'gaussian 'binomial 'multinomial 'cox 'poisson 'mgaussian)]
                         [lambda (vectorof real?)]
                         [intercepts (or/c #f vector?)]
                         [coefficients vector?]
                         [dev-ratio (vectorof real?)]
                         [df (vectorof exact-nonnegative-integer?)]
                         [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted path. @racket[lambda] holds the fitted penalties, decreasing, and
  every other per-@math{λ} field has one entry for each of them:

  @itemlist[
   @item{@racket[intercepts] holds a real per @math{λ}, or a vector of one real
         per class or response for the multinomial and multi-response families.
         It is @racket[#f] for Cox, which has no intercept.}
   @item{@racket[coefficients] holds a dense vector per @math{λ} with one entry
         per predictor, or a vector of such vectors (one per class or response).}
   @item{@racket[dev-ratio] is the fraction of null deviance explained.}
   @item{@racket[df] counts the predictors with a nonzero coefficient, in any
         class or response.}
  ]

  @racket[num-passes] counts the coordinate-descent passes over the whole path.

  @examples[#:eval ev
  (define X '((1.0 2.0 1.0) (2.0 1.0 4.0) (3.0 4.0 9.0)
              (4.0 3.0 16.0) (5.0 6.0 25.0) (6.0 5.0 36.0)))
  (define y '(1.0 4.0 3.0 6.0 5.0 8.0))
  (define path (elnet-path X y #:lambda '(1.0 0.1 0.01)))
  (glmnet-path-lambda path)
  (glmnet-path-coefficients path)
  (glmnet-path-df path)]}

@defproc[(elnet-path [X (and/c (listof (listof real?)) pair?)]
                     [y (and/c (listof real?) pair?)]
                     [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                     [#:nlambda nlambda exact-positive-integer? 100]
                     [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                     [#:alpha alpha (real-in 0 1) 1.0]
                     [#:standardize? standardize? boolean? #t]
                     [#:intercept? intercept? boolean? #t]
                     [#:thresh thresh (>/c 0) 1e-7]
                     [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The Gaussian path, as @racket[elnet-fit] fits one point of it.

  @examples[#:eval ev
  (vector-length (glmnet-path-lambda (elnet-path X y)))
  (glmnet-path-df (elnet-path X y #:alpha 0.0 #:nlambda 5))]}

@defproc[(logistic-path [X (and/c (listof (listof real?)) pair?)]
                        [y (and/c (listof (or/c 0 1)) pair?)]
                        [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                        [#:nlambda nlambda exact-positive-integer? 100]
                        [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                        [#:alpha alpha (real-in 0 1) 1.0]
                        [#:standardize? standardize? boolean? #t]
                        [#:intercept? intercept? boolean? #t]
                        [#:thresh thresh (>/c 0) 1e-7]
                        [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The binomial path, as @racket[logistic-fit] fits one point of it.

  @examples[#:eval ev
  (glmnet-path-df (logistic-path X '(0 0 0 1 1 1) #:lambda '(0.3 0.1 0.03)))]}

@defproc[(multinomial-path [X (and/c (listof (listof real?)) pair?)]
                           [y (and/c (listof exact-nonnegative-integer?) pair?)]
                           [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                           [#:nlambda nlambda exact-positive-integer? 100]
                           [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                           [#:alpha alpha (real-in 0 1) 1.0]
                           [#:standardize? standardize? boolean? #t]
                           [#:intercept? intercept? boolean? #t]
                           [#:thresh thresh (>/c 0) 1e-7]
                           [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The multinomial path, as @racket[multinomial-fit] fits one point of it.

  @examples[#:eval ev
  (define mpath (multinomial-path X '(0 0 1 1 2 2) #:lambda '(0.3 0.03)))
  (vector-ref (glmnet-path-coefficients mpath) 1)]}

@defproc[(cox-path [X (and/c (listof (listof real?)) pair?)]
                   [times (and/c (listof (>/c 0)) pair?)]
                   [statuses (and/c (listof (or/c 0 1)) pair?)]
                   [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                   [#:nlambda nlambda exact-positive-integer? 100]
                   [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                   [#:alpha alpha (real-in 0 1) 1.0]
                   [#:standardize? standardize? boolean? #t]
                   [#:thresh thresh (>/c 0) 1e-7]
                   [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The Cox path, as @racket[cox-fit] fits one point of it. There is no
  intercept, so @racket[glmnet-path-intercepts] is @racket[#f].

  @examples[#:eval ev
  (define cpath (cox-path X '(12.0 10.0 8.0 6.0 4.0 3.0) '(0 1 1 1 1 1)
                          #:lambda '(0.5 0.05)))
  (glmnet-path-coefficients cpath)]}

@defproc[(poisson-path [X (and/c (listof (listof real?)) pair?)]
                       [y (and/c (listof (>=/c 0)) pair?)]
                       [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                       [#:nlambda nlambda exact-positive-integer? 100]
                       [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? #t]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The Poisson path, as @racket[poisson-fit] fits one point of it.

  @examples[#:eval ev
  (glmnet-path-df (poisson-path X '(1 2 2 3 5 8) #:lambda '(0.5 0.05)))]}

@defproc[(mgaussian-path [X (and/c (listof (listof real?)) pair?)]
                         [Y (and/c (listof (listof real?)) pair?)]
                         [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                         [#:nlambda nlambda exact-positive-integer? 100]
                         [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                         [#:alpha alpha (real-in 0 1) 1.0]
                         [#:standardize? standardize? boolean? #t]
                         [#:intercept? intercept? boolean? #t]
                         [#:thresh thresh (>/c 0) 1e-7]
                         [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The multi-response Gaussian path, as @racket[mgaussian-fit] fits one point of
  it.

  @examples[#:eval ev
  (define gpath (mgaussian-path X '((3.0 9.0) (5.0 8.0) (7.0 7.0)
                                    (9.0 6.0) (11.0 5.0) (13.0 4.0))
                                #:lambda '(1.0 0.1)))
  (glmnet-path-intercepts gpath)]}

@section[#:tag "ref-native"]{Native library}

These call straight into @tt{libglmnetcompat}. Loading @racketmodname[glmnet]
checks both and raises @racket[exn:fail] if either is wrong. See
@secref["concepts-precision"].

@defproc[(glmnet-default-real-bytes) exact-positive-integer?]{
  The width in bytes of the Fortran default @tt{real} in the loaded library.
  It is @racket[8] for a correctly built library, which the numeric API
  requires.

  @examples[#:eval ev
  (glmnet-default-real-bytes)]}

@defproc[(glmnet-capi-abi-version) exact-positive-integer?]{
  The version of the C entry points the library exports. It changes whenever an
  entry point is added or changed incompatibly.

  @examples[#:eval ev
  (glmnet-capi-abi-version)]}

@(close-eval ev)
