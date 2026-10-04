#lang scribble/manual
@(require "utils.rkt")

@title[#:tag "guide" #:style 'toc]{User guide}

To get started we will walk through an example of estimating a model from data and unpacking the results.

There are several worked @secref["examples"] if you prefer to jump to the specific model type you are interested in.

You can also evaluate the examples in this guide and their source code which
you can view @filepath{glmnet/examples} in the main repository on GitHub.

To find the documentation for a specific definition or procedure, see the @secref["reference"].

@local-table-of-contents[]

@include-section["guide/getting-started.scrbl"]
@include-section["guide/concepts.scrbl"]
@include-section["guide/data.scrbl"]
@include-section["guide/formulas.scrbl"]
@include-section["guide/plots.scrbl"]
@include-section["guide/examples.scrbl"]
