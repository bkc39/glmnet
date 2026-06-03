#lang scribble/manual

@(require (for-label racket/base
                     glmnet))

@title[#:tag "start"]{Getting started}

@section{Installation}

@racketblock[(require glmnet)]

The package ships a prebuilt native library (@tt{libglmnetcompat}) for each
supported platform under @filepath{glmnet/native-libs/}, staged at install time
by the pre-install hook. No Fortran toolchain is needed to @emph{use} the
package; you only need one to rebuild the native library (see @secref["guide"]).

@section{A first call}

Fit ordinary least squares and read off the result:

@racketblock[
(require glmnet)
(define fit (ols '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0)) '(1.0 4.0 3.0 6.0)))
(elnet-result-intercept fit)
(elnet-result-coefficients fit)
]

You can also confirm the native library loaded and was built with the
double-precision contract the numeric API depends on:

@racketblock[
(glmnet-default-real-bytes)    (code:comment "=> 8")
(glmnet-capi-abi-version)      (code:comment "=> 1")
]

If @racket[(glmnet-default-real-bytes)] is anything other than @racket[8], the
native library was built without @tt{-fdefault-real-8} and the numeric results
would be wrong; the package raises an error at load time in that case rather
than returning silent garbage.

Continue to @secref["guide"] for the model concepts, or jump to
@secref["examples"] for runnable code.
