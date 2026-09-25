#lang scribble/manual
@(require "utils.rkt")

@title{glmnet: lasso and elastic-net regularized models}
@author{bkc}

@defmodule[glmnet]

@racketmodname[glmnet] provides Racket bindings to
@hyperlink["https://glmnet.stanford.edu/"]{glmnet}, the
Friedman/Hastie/Tibshirani coordinate-descent solver for lasso and elastic-net
regularized generalized linear models. The numerics come from the original,
self-contained glmnet Fortran (vendored as
@filepath{fortran/vendor/glmnet5.f90}); a small @tt{iso_c_binding} shim exports
a clean C ABI that this package binds to through Racket's FFI. Prebuilt shared
objects for Linux (x86-64) and macOS (arm64) ship with the package and are
staged at install time, so no Fortran toolchain is needed to use it.

The @racketmodname[glmnet] module fits six model families, each with the same
elastic-net penalty:

@itemlist[
  @item{@bold{Gaussian} --- ordinary least squares, ridge, lasso and elastic
        net for a numeric response.}
  @item{@bold{Binomial} and @bold{multinomial} --- two-class and
        @math{K}-class logistic classifiers.}
  @item{@bold{Cox} --- proportional-hazards survival models.}
  @item{@bold{Poisson} --- counts, with a log link.}
  @item{@bold{Multi-response Gaussian} --- several numeric responses fitted
        jointly under a grouped lasso.}
]

This manual has two parts: the @secref["guide"] works through the library by
example, and the @secref["reference"] documents the public API.

@bold{License.} Because this package vendors and links the GPL-2.0 glmnet
Fortran, it is distributed under @bold{GPL-2.0-or-later} --- unlike the
permissively licensed bindings in the same family.

@bold{Acknowledgements.} The solver and its algorithms are the work of Jerome
Friedman, Trevor Hastie, Rob Tibshirani and the other authors of the
@hyperlink["https://glmnet.stanford.edu/"]{R glmnet package}, whose vignettes
shaped this manual.

@local-table-of-contents[]

@include-section["guide.scrbl"]
@include-section["reference.scrbl"]
