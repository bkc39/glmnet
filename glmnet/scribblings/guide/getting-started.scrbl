#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "getting-started" #:style 'toc]{Getting started}

This chapter follows the @hyperlink["https://glmnet.stanford.edu/articles/glmnet.html#quick-start"]{Quick
Start} of the R package's vignette: fit a model, read its coefficients, and
predict from it.

@local-table-of-contents[]

@section[#:tag "gs-installing"]{Installing}

@commandline{raco pkg install glmnet}

@racketblock[(require glmnet)]

The package ships a prebuilt native library, @tt{libglmnetcompat}, for Linux
(x86-64) and macOS (arm64), and a pre-install hook stages it at install time.
To check that it loaded:

@examples[#:eval ev #:label #f
(glmnet-default-real-bytes)
(glmnet-capi-abi-version)
]

@racket[glmnet-default-real-bytes] must be @racket[8]: the Fortran is compiled
with double-precision default reals (see @secref["concepts-precision"]). If it
is not, or if the library's ABI version is not the one this package expects,
@racket[(require glmnet)] raises an error rather than returning wrong numbers.

@section[#:tag "gs-first-fit"]{A first fit}

The simplest form of a @tech{design matrix} is a list of rows, one per
observation; the response is a list with one entry per row (see
@secref["concepts-data"] for the other form). Here the response is exactly
@math{y = 1 + 2x₁ − x₂}, and the third column, @math{x₃ = x₁²}, carries no
signal:

@examples[#:eval ev #:label #f
(define X '((1.0 2.0  1.0)
            (2.0 1.0  4.0)
            (3.0 4.0  9.0)
            (4.0 3.0 16.0)
            (5.0 6.0 25.0)
            (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))
(define fit (lasso X y #:lambda 0.05))
fit
]

@racket[lasso] fits a Gaussian model under an L1 penalty of strength
@racket[#:lambda]. The result is an @racket[elnet-result], which prints as a
summary: the family, @math{λ}, the deviance ratio (for this family,
@math{R²}) and the number of nonzero coefficients out of three. Its fields are
read with the usual struct accessors:

@examples[#:eval ev #:label #f
(elnet-result-intercept fit)
(elnet-result-coefficients fit)
(elnet-result-r-squared fit)
]

@racket[coef] gives the same numbers in R's layout, intercept first, for a fit
of any family:

@examples[#:eval ev #:label #f
(coef fit)
]

The coefficient on @math{x₃} is exactly @racket[0.0]: the lasso has dropped it.
Raise @racket[#:lambda] and more coefficients drop out; set it to @racket[0]
(or call @racket[ols]) and the fit is ordinary least squares:

@examples[#:eval ev #:label #f
(elnet-result-coefficients (lasso X y #:lambda 0.5))
(elnet-result-coefficients (ols X y))
]

To try many values of @racket[#:lambda] at once, fit a
@tech{regularization path} (@secref["concepts-path"]). It prints as R prints
one, with the number of nonzero coefficients, the percentage of deviance
explained and @math{λ} for each fitted value:

@examples[#:eval ev #:label #f
(define path (elnet-path X y #:nlambda 12))
path
]

To choose one of those values, cross-validate: @racket[elnet-cv] estimates the
prediction error at each @math{λ} on held-out data and picks R's
@tt{lambda.min} and @tt{lambda.1se}, at which @racket[predict] and
@racket[coef] then evaluate the fit. Six observations are too few for that;
@secref["concepts-cv"] works through an example.

@section[#:tag "gs-models"]{Choosing a model}

@racket[ols], @racket[ridge], @racket[lasso] and @racket[elastic-net] are the
same solver, @racket[elnet-fit], with a different @racket[#:alpha] and
@racket[#:lambda] (see @secref["concepts-penalty"]). The other families take the
same two keywords and differ only in the response they model:

@examples[#:eval ev #:label #f
(define labels '(0 0 0 1 1 1))
(define clf (logistic-fit X labels #:lambda 0.05))
(logistic-result-coefficients clf)
]

@secref["concepts-families"] lists all six families, and
@secref["examples"] has a worked example for each.

@section[#:tag "gs-predicting"]{Predicting}

@racket[predict] evaluates a fit of any family on new rows, given in the same
layout as @racket[X]. For the Gaussian family it returns the fitted values:

@examples[#:eval ev #:label #f
(predict fit '((7.0 6.0 49.0) (1.5 1.0 2.25)))
]

For a classifier, @racket[#:type] chooses between the log-odds (the default,
as in R), the probability of class 1 and the class itself:

@examples[#:eval ev #:label #f
(define new-rows '((2.0 3.0 4.0) (5.0 4.0 25.0)))
(predict clf new-rows)
(predict clf new-rows #:type 'response)
(predict clf new-rows #:type 'class)
]

Each family also has named prediction helpers, such as
@racket[logistic-predict-proba] and @racket[logistic-predict]; they are
@racket[predict] at a fixed type. A path predicts at any @math{λ}, including
values between those it fitted; see @secref["concepts-predict"].

@section[#:tag "gs-api-gaps"]{What is not there yet}

Several R @tt{glmnet} features have no binding yet. Each has an open issue:

@itemlist[
  @item{Coefficient-path and CV-error plots
        (@hyperlink["https://github.com/bkc39/glmnet/issues/28"]{#28}).}
  @item{Observation weights, @tt{penalty.factor}, coefficient limits, offsets
        and @tt{exclude}
        (@hyperlink["https://github.com/bkc39/glmnet/issues/12"]{#12}).}
  @item{Sparse predictor matrices
        (@hyperlink["https://github.com/bkc39/glmnet/issues/11"]{#11}).}
]

@(close-eval ev)
