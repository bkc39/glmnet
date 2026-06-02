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

The @secref["ex-hello"] example is a pure connectivity check --- it confirms the
bindings load, that values marshal correctly across the C ABI, and that the
native library was compiled with the double-precision contract the numeric API
depends on:

@racketblock[
(require glmnet)
(glmnet-hello 2.5 4.0)         (code:comment "=> 6.5")
(glmnet-default-real-bytes)    (code:comment "=> 8")
(glmnet-capi-abi-version)      (code:comment "=> 1")
]

If @racket[(glmnet-default-real-bytes)] is anything other than @racket[8], the
native library was built without @tt{-fdefault-real-8} and the numeric results
would be wrong; the package raises an error at load time in that case rather
than returning silent garbage.

Continue to @secref["guide"] for the model concepts, or jump to
@secref["examples"] for runnable code.
