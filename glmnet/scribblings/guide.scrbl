#lang scribble/manual
@(require "utils.rkt")

@title[#:tag "guide" #:style 'toc]{User guide}

The guide starts with a first fit, then covers the data layout, the penalty
and the result types that every model family shares, and ends with one worked
example per family.

Every snippet is evaluated when this manual is built, so the printed results
are what the library returns. Each section in @secref["examples"] has a runnable
literate counterpart under @filepath{glmnet/examples/} in the package source,
together with a companion runner and test under @filepath{glmnet/examples/test/}:

@commandline{racket glmnet/examples/test/02-lasso.rkt}

For the definition of every binding mentioned, see the @secref["reference"].

@local-table-of-contents[]

@include-section["guide/getting-started.scrbl"]
@include-section["guide/concepts.scrbl"]
@include-section["guide/examples.scrbl"]
