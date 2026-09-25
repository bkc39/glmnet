#lang scribble/manual
@(require "utils.rkt")

@title{glmnet: lasso and elastic-net regularized models}
@author{bkc}

@defmodule[glmnet]

@racketmodname[glmnet] is a library for building linear statistical models in
Racket. It is a port of the reference implementation by Friedman, Hastie,
Tibshirani et al., the R package
@hyperlink["https://glmnet.stanford.edu/"]{glmnet}, and fits lasso, ridge and
elastic-net regularized models in six families:

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
