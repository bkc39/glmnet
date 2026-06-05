#lang scribble/manual

@(require (for-label racket/base
                     glmnet))

@title[#:tag "reference"]{API reference}

@declare-exporting[glmnet]

The reference documents every public procedure. It grows as each model lands.

@section[#:tag "ref-fitting"]{Fitting Gaussian models}

The four core models are one routine, @racket[elnet-fit], called with different
@racket[#:alpha] and @racket[#:lambda]; convenience wrappers name the common
cases.

@defstruct*[elnet-result ([intercept real?]
                          [coefficients (vectorof real?)]
                          [r-squared real?]
                          [lambda real?]
                          [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted model. @racket[coefficients] is a dense vector on the original
  predictor scale (one entry per column of @racket[_X]); @racket[lambda] is the
  penalty actually used; @racket[r-squared] is the fraction of null deviance
  explained; @racket[num-passes] is glmnet's coordinate-descent pass count.
}

@defproc[(elnet-fit [X (and/c (listof (listof real?)) pair?)]
                    [y (and/c (listof real?) pair?)]
                    [#:lambda lambda (>=/c 0)]
                    [#:alpha alpha (real-in 0 1) 1.0]
                    [#:standardize? standardize? boolean? #t]
                    [#:intercept? intercept? boolean? #t]
                    [#:thresh thresh (>/c 0) 1e-7]
                    [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Fits a single dense Gaussian elastic-net model. @racket[X] is a non-empty list
  of equal-length rows; @racket[y] is the matching response. @racket[alpha] is
  the mixing parameter (@racket[0] ridge, @racket[1] lasso, in between elastic
  net) and @racket[lambda] is the penalty strength. With @racket[standardize?]
  the predictors are scaled to unit variance before fitting (coefficients are
  reported on the original scale). Raises an error if glmnet reports a fatal
  condition (e.g. zero-variance predictors).
}

@defproc[(ols [X (and/c (listof (listof real?)) pair?)]
              [y (and/c (listof real?) pair?)]
              [#:standardize? standardize? boolean? #t]
              [#:intercept? intercept? boolean? #t]
              [#:thresh thresh (>/c 0) 1e-10]
              [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Ordinary least squares: @racket[elnet-fit] with @racket[#:lambda 0.0] (the
  mixing parameter is then irrelevant). Uses a tighter default @racket[thresh]
  than the penalized fits, since coordinate descent approaches the unpenalized
  solution as the threshold tightens. See @secref["ex-ols"].
}

@defproc[(ridge [X (and/c (listof (listof real?)) pair?)]
                [y (and/c (listof real?) pair?)]
                [#:lambda lambda (>=/c 0)]
                [#:standardize? standardize? boolean? #t]
                [#:intercept? intercept? boolean? #t]
                [#:thresh thresh (>/c 0) 1e-7]
                [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Ridge regression: @racket[elnet-fit] with @racket[#:alpha 0.0] (pure L2
  penalty). Shrinks every coefficient smoothly toward zero as @racket[lambda]
  grows but never sets one exactly to zero. See @secref["ex-ridge"].
}

@defproc[(lasso [X (and/c (listof (listof real?)) pair?)]
                [y (and/c (listof real?) pair?)]
                [#:lambda lambda (>=/c 0)]
                [#:standardize? standardize? boolean? #t]
                [#:intercept? intercept? boolean? #t]
                [#:thresh thresh (>/c 0) 1e-7]
                [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Lasso: @racket[elnet-fit] with @racket[#:alpha 1.0] (pure L1 penalty). Performs
  variable selection --- drives coefficients exactly to zero, more of them as
  @racket[lambda] grows. See @secref["ex-lasso"].
}

@defproc[(elastic-net [X (and/c (listof (listof real?)) pair?)]
                      [y (and/c (listof real?) pair?)]
                      [#:alpha alpha (real-in 0 1)]
                      [#:lambda lambda (>=/c 0)]
                      [#:standardize? standardize? boolean? #t]
                      [#:intercept? intercept? boolean? #t]
                      [#:thresh thresh (>/c 0) 1e-7]
                      [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Elastic net at an explicit @racket[alpha] in @racket[(real-in 0 1)]: blends the
  lasso's selection with the ridge's shrinkage. @racket[#:alpha 0.0] reduces to
  @racket[ridge] and @racket[#:alpha 1.0] to @racket[lasso]. See
  @secref["ex-elastic-net"].
}

@section[#:tag "ref-binomial"]{Fitting binomial (logistic) models}

The binomial family fits a two-class logistic model: the response is a 0/1 class
label and the fit models the log-odds of class 1. @racket[logistic-fit] mirrors
@racket[elnet-fit]'s keywords, and prediction helpers turn a fit into class-1
probabilities or hard labels.

@defstruct*[logistic-result ([intercept real?]
                             [coefficients (vectorof real?)]
                             [dev-ratio real?]
                             [lambda real?]
                             [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted two-class logistic model. @racket[intercept] and the dense
  @racket[coefficients] (on the original predictor scale) are on the log-odds
  scale for class 1; @racket[lambda] is the penalty actually used;
  @racket[dev-ratio] is the fraction of null deviance explained (the logistic
  analogue of @racket[elnet-result]'s @racket[r-squared]); @racket[num-passes] is
  glmnet's coordinate-descent pass count.
}

@defproc[(logistic-fit [X (and/c (listof (listof real?)) pair?)]
                       [y (and/c (listof (or/c 0 1)) pair?)]
                       [#:lambda lambda (>=/c 0)]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? #t]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         logistic-result?]{
  Fits a single dense two-class logistic elastic-net model. @racket[X] is a
  non-empty list of equal-length rows and @racket[y] a matching list of 0/1 class
  labels. @racket[alpha] mixes the penalty (@racket[0.0] ridge logistic,
  @racket[1.0] lasso logistic, in between elastic net) and @racket[lambda] sets
  its strength. Raises an error if glmnet reports a fatal condition --- including
  a class probability collapsing under perfect separation, which a larger
  @racket[lambda] usually fixes. See @secref["ex-logistic"].
}

@defproc[(logistic-predict-proba [fit logistic-result?]
                                 [X (and/c (listof (listof real?)) pair?)])
         (listof (real-in 0 1))]{
  The class-1 probability @math{1 / (1 + e^(-(β₀ + xβ)))} for each row of
  @racket[X]. Each row must have as many features as @racket[fit] has
  coefficients.
}

@defproc[(logistic-predict [fit logistic-result?]
                           [X (and/c (listof (listof real?)) pair?)]
                           [#:threshold threshold (real-in 0 1) 0.5])
         (listof (or/c 0 1))]{
  Hard class labels: @racket[1] where @racket[logistic-predict-proba] is at least
  @racket[threshold], otherwise @racket[0].
}

@section[#:tag "ref-multinomial"]{Fitting multinomial (multiclass) models}

The multinomial family fits a K-class classifier: the response is a list of
integer class labels @math{0..K-1} and the fit returns K intercepts and K
coefficient vectors.

@defstruct*[multinomial-result ([intercepts (vectorof real?)]
                                [coefficients (vectorof (vectorof real?))]
                                [dev-ratio real?]
                                [lambda real?]
                                [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted K-class model. @racket[intercepts] is a vector of K reals;
  @racket[coefficients] is a vector of K coefficient vectors (each of length
  @racket[_ni]), one per class, on the original predictor scale.
  @racket[dev-ratio] is the fraction of null deviance explained (the multiclass
  analogue of @racket[elnet-result]'s @racket[r-squared]); @racket[lambda] is the
  penalty used; @racket[num-passes] is glmnet's pass count.
}

@defproc[(multinomial-fit [X (and/c (listof (listof real?)) pair?)]
                          [y (and/c (listof exact-nonnegative-integer?) pair?)]
                          [#:lambda lambda (>=/c 0)]
                          [#:alpha alpha (real-in 0 1) 1.0]
                          [#:standardize? standardize? boolean? #t]
                          [#:intercept? intercept? boolean? #t]
                          [#:thresh thresh (>/c 0) 1e-7]
                          [#:max-iters max-iters exact-positive-integer? 100000])
         multinomial-result?]{
  Fits a single dense K-class multinomial elastic-net model. @racket[y] is a list
  of integer class labels that must cover @racket[0]..@racket[(sub1 K)]
  contiguously (every class present). @racket[alpha] mixes the penalty
  (@racket[0.0] ridge, @racket[1.0] lasso) and @racket[lambda] sets its strength.
  Raises an error on a fatal glmnet condition (e.g. a class probability
  collapsing under perfect separation --- use a larger @racket[lambda]). See
  @secref["ex-multinomial"].
}

@defproc[(multinomial-predict-proba [fit multinomial-result?]
                                    [X (and/c (listof (listof real?)) pair?)])
         (listof (listof (real-in 0 1)))]{
  The per-class softmax probabilities for each row of @racket[X]; each inner list
  has K entries summing to 1. Each row must have as many features as @racket[fit]
  has coefficients.
}

@defproc[(multinomial-predict [fit multinomial-result?]
                              [X (and/c (listof (listof real?)) pair?)])
         (listof exact-nonnegative-integer?)]{
  The predicted class label @math{0..K-1} for each row of @racket[X] --- the
  argmax of @racket[multinomial-predict-proba].
}

@section[#:tag "ref-connectivity"]{Connectivity and self-checks}

These entry points call directly into the C-ABI shim, for confirming the native
library loaded and was built correctly.

@defproc[(glmnet-default-real-bytes) exact-positive-integer?]{
  The byte width of the Fortran default @tt{real} in the loaded native library.
  This is @racket[8] when the library was compiled with @tt{-fdefault-real-8},
  which the numeric API requires. The package raises an error at load time if it
  is not @racket[8].
}

@defproc[(glmnet-capi-abi-version) exact-positive-integer?]{
  The ABI version of the C-ABI shim. Bumped on any breaking change to a C entry
  point.
}
