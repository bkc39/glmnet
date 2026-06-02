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

@section[#:tag "ref-connectivity"]{Connectivity and self-checks}

These entry points call directly into the C-ABI shim. They are used by
@secref["ex-hello"] to verify the binding is wired correctly.

@defproc[(glmnet-hello [a real?] [b real?]) real?]{
  Returns @racket[(+ a b)], computed in Fortran. A by-value @tt{double}
  round-trip across the C ABI --- a smoke test that the native library loads and
  marshals correctly.
}

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
