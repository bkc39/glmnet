#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "examples" #:style 'toc]{Examples}

First the Quick Start of R glmnet's vignette, on R's own data, then one
section per model, then three on formulas, and one that fits the same data
from every data source. Each starts from the literate
program of the same name in @filepath{glmnet/examples/}, whose companion under
@filepath{glmnet/examples/test/} runs it and checks the result the prose
promises, then goes further: it varies @math{λ} or @math{α} to show what the
penalty does, and uses the family's prediction helpers on new data.

The small fixtures of the model sections are chosen so that the right answer is
known in advance: a response built from some predictors plus a column of pure
noise, which a good fit should leave out. Where a section prints a sweep over
@math{λ}, it rounds the numbers to three decimal places so the rows line up.
The Quick Start uses R glmnet's @tt{QuickStartExample}, and the formula and
data-source sections R's @tt{mtcars} and @tt{iris}; their numbers are R's.

@local-table-of-contents[]

@include-section["examples/quick-start.scrbl"]
@include-section["examples/ols.scrbl"]
@include-section["examples/ridge.scrbl"]
@include-section["examples/lasso.scrbl"]
@include-section["examples/elastic-net.scrbl"]
@include-section["examples/logistic.scrbl"]
@include-section["examples/multinomial.scrbl"]
@include-section["examples/cox.scrbl"]
@include-section["examples/poisson.scrbl"]
@include-section["examples/mgaussian.scrbl"]
@include-section["examples/formula-interactions.scrbl"]
@include-section["examples/formula-polynomial.scrbl"]
@include-section["examples/formula-factors.scrbl"]
@include-section["examples/data-sources.scrbl"]
