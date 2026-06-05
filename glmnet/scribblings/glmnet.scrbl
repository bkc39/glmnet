#lang scribble/manual

@(require (for-label racket/base
                     glmnet))

@title[#:style '(toc)]{glmnet: lasso and elastic-net regularized models}
@author[(author+email "bkc" "bkcschemer@gmail.com")]

@defmodule[glmnet]

These are Racket bindings to @hyperlink["https://glmnet.stanford.edu/"]{glmnet},
the Friedman/Hastie/Tibshirani coordinate-descent solver for lasso and
elastic-net regularized generalized linear models. The numerics come from the
original, self-contained glmnet Fortran (vendored as
@filepath{fortran/vendor/glmnet5.f90}); a small @tt{iso_c_binding} shim exports a
clean C ABI that this package binds to through Racket's FFI.

The library is developed @emph{example-first}: every model below ships as a
runnable @tech{literate program} under @filepath{glmnet/examples/} that is also
woven into @secref["examples"]. The four core models --- ordinary least squares,
ridge, lasso, and elastic net --- are all a single call to the same elastic-net
routine with different values of the mixing parameter @math{α} and the penalty
@math{λ}.

@bold{License.} Because this package vendors and links the GPL-2.0 glmnet
Fortran, it is distributed under @bold{GPL-2.0-or-later} --- unlike the
permissively licensed bindings in the same family.

@table-of-contents[]

@include-section["start.scrbl"]
@include-section["guide.scrbl"]
@include-section["examples.scrbl"]
@include-section["reference.scrbl"]
