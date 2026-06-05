#lang scribble/manual

@(require (for-label racket/base
                     glmnet))

@title[#:tag "guide"]{User guide}

This guide explains the model concepts behind the bindings. It grows one section
at a time as each model lands; each section pairs with a runnable example in
@secref["examples"].

@section[#:tag "guide-elastic-net"]{The elastic-net model}

glmnet fits a linear model by minimizing a penalized least-squares objective.
For a response @math{y} and predictors @math{X} (with @math{n} observations), it
solves

@centered{@math{min_{β₀,β} 1/(2n) ‖y − β₀ − Xβ‖² + λ [ α‖β‖₁ + (1−α)/2 ‖β‖₂² ]}}

over the intercept @math{β₀} and coefficients @math{β}. Two knobs control the
penalty:

@itemlist[
 @item{@bold{@math{α} (the mixing parameter), in @math{[0,1]}.} @math{α=0} is the
       pure ridge (L2) penalty; @math{α=1} is the pure lasso (L1) penalty;
       values in between blend the two (the @italic{elastic net}).}
 @item{@bold{@math{λ} (the penalty strength), @math{≥ 0}.} Larger @math{λ}
       shrinks coefficients more; @math{λ=0} recovers the unpenalized fit
       (ordinary least squares).}
]

So the four core models this package exposes are @emph{one routine} called with
different parameters:

@tabular[#:sep @hspace[2]
 (list (list @bold{Model}        @bold{@math{α}} @bold{@math{λ}})
       (list "OLS"               "(any)"          "0")
       (list "Ridge"             "0"              "> 0")
       (list "Lasso"             "1"              "> 0")
       (list "Elastic net"       "0 < α < 1"      "> 0"))]

@section[#:tag "guide-fitting"]{Fitting a model}

@racket[elnet-fit] is the one entry point behind every model; the four core
cases have convenience wrappers. A fit takes the predictor matrix as a list of
rows and the response as a list, and returns an @racket[elnet-result]:

@racketblock[
(require glmnet)
(define X '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0) (5.0 6.0)))
(define y '(1.0 4.0 3.0 6.0 5.0))
(define fit (ols X y))
(elnet-result-intercept fit)
(elnet-result-coefficients fit)]

@racket[ols] is just @racket[elnet-fit] with @racket[#:lambda 0.0]. The
penalized models pass @racket[#:alpha] and a positive @racket[#:lambda], and
each has a convenience wrapper: @racket[ridge] (@math{α = 0}), @racket[lasso]
(@math{α = 1}), and @racket[elastic-net] (an explicit @racket[#:alpha]). See
@secref["ex-ols"], @secref["ex-ridge"], @secref["ex-lasso"], and
@secref["ex-elastic-net"] for worked examples, and @secref["reference"] for the
full keyword list.

@section[#:tag "guide-standardize"]{Standardization and intercept}

By default glmnet standardizes each predictor column to unit variance before
fitting and reports coefficients on the original scale, and it fits an
unpenalized intercept. These match the conventions in the R package's vignette
and are the defaults the bindings expose.

@section[#:tag "guide-binomial"]{Classification: the binomial family}

The models above fit a numeric response with the Gaussian @tt{elnet} solver. For
@bold{binary classification}, the @deftech{binomial family} fits a two-class
logistic model with glmnet's @tt{lognet} solver instead: the response @math{y} is
a 0/1 class label, and the model predicts the @emph{log-odds} of class 1,

@centered{@math{log( P(y=1) / P(y=0) ) = β₀ + Xβ},}

minimizing the penalized binomial deviance under the same elastic-net penalty.
The @math{α} and @math{λ} knobs mean exactly what they do for the Gaussian
models, so @racket[#:alpha 1.0] is the sparse (lasso) logistic and
@racket[#:alpha 0.0] the ridge logistic.

@racketblock[
(require glmnet)
(define X '((1.0 5.0) (2.0 4.0) (5.0 1.0) (4.0 2.0)))
(define y '(0 0 1 1))
(define fit (logistic-fit X y #:lambda 0.05))
(logistic-result-coefficients fit)
(code:comment "class-1 probabilities, then hard 0/1 labels:")
(logistic-predict-proba fit X)
(logistic-predict fit X)]

@racket[logistic-fit] returns a @racket[logistic-result] whose
@racket[logistic-result-dev-ratio] is the fraction of null deviance explained ---
the logistic analogue of @math{R²}. @racket[logistic-predict-proba] maps new rows
to class-1 probabilities through the logistic function, and
@racket[logistic-predict] thresholds those (at @racket[0.5] by default) into 0/1
labels. See @secref["ex-logistic"] for a worked classification example.

@section[#:tag "guide-precision"]{The precision contract}

The vendored Fortran declares its arrays as single-precision @tt{real}, but the
entire elastic-net ABI is treated as @bold{double precision}: the native library
is compiled with @tt{-fdefault-real-8} so the default @tt{real} is 8 bytes. This
is what R's glmnet and Julia's @tt{glmnet_jll} do as well. The package asserts
the flag took effect at load time (see @secref["start"]).

@section[#:tag "guide-build"]{Rebuilding the native library}

Using the package needs no Fortran toolchain. To rebuild @tt{libglmnetcompat}
from source (for development or a new platform):

@verbatim|{
  # via Nix (runs the Fortran ctest suite too)
  nix build .#native

  # or directly with CMake + gfortran
  cmake -S fortran -B fortran/build -DBUILD_TESTING=ON
  cmake --build fortran/build
  ctest --test-dir fortran/build --output-on-failure
}|
