#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "examples" #:style 'toc]{Examples}

One section per model. Each starts from the literate program of the same name
in @filepath{glmnet/examples/}, whose companion under
@filepath{glmnet/examples/test/} runs it and checks the result the prose
promises, then goes further: it varies @math{λ} or @math{α} to show what the
penalty does, and uses the family's prediction helpers on new data.

The small fixtures are chosen so that the right answer is known in advance: a
response built from some predictors plus a column of pure noise, which a good
fit should leave out. Where a section prints a sweep over @math{λ}, it rounds
the numbers to three decimal places so the rows line up.

@local-table-of-contents[]

@include-section["examples/ols.scrbl"]
@include-section["examples/ridge.scrbl"]
@include-section["examples/lasso.scrbl"]
@include-section["examples/elastic-net.scrbl"]
@include-section["examples/logistic.scrbl"]
@include-section["examples/multinomial.scrbl"]
@include-section["examples/cox.scrbl"]
@include-section["examples/poisson.scrbl"]
@include-section["examples/mgaussian.scrbl"]
