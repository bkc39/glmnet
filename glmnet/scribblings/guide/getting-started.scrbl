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

A @tech{design matrix} is a list of rows, one per observation; the response is
a list with one entry per row. Here the response is exactly
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
@racket[#:lambda]. The result is an @racket[elnet-result]; its fields are
read with the usual struct accessors:

@examples[#:eval ev #:label #f
(elnet-result-intercept fit)
(elnet-result-coefficients fit)
(elnet-result-r-squared fit)
]

The coefficient on @math{x₃} is exactly @racket[0.0]: the lasso has dropped it.
Raise @racket[#:lambda] and more coefficients drop out; set it to @racket[0]
(or call @racket[ols]) and the fit is ordinary least squares:

@examples[#:eval ev #:label #f
(elnet-result-coefficients (lasso X y #:lambda 0.5))
(elnet-result-coefficients (ols X y))
]

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

Each classification and count family has a prediction helper that takes new
rows in the same layout as @racket[X]:

@examples[#:eval ev #:label #f
(logistic-predict-proba clf '((2.0 3.0 4.0) (5.0 4.0 25.0)))
(logistic-predict clf '((2.0 3.0 4.0) (5.0 4.0 25.0)))
]

The Gaussian family has no prediction helper yet; the fitted value is the
intercept plus the dot product of a row with the coefficients:

@examples[#:eval ev #:label #f
(define (gaussian-predict fit row)
  (for/fold ([acc (elnet-result-intercept fit)])
            ([b (in-vector (elnet-result-coefficients fit))]
             [x (in-list row)])
    (+ acc (* b x))))
(gaussian-predict fit '(7.0 6.0 49.0))
]

@section[#:tag "gs-api-gaps"]{What is not there yet}

Several R @tt{glmnet} features have no binding yet. Each has an open issue:

@itemlist[
  @item{A whole regularization path in one call, @tt{glmnet(x, y)} with
        @tt{nlambda}: every fit here takes a single @racket[#:lambda]
        (@hyperlink["https://github.com/bkc39/glmnet/issues/10"]{#10}).}
  @item{Cross-validation, @tt{cv.glmnet}, @tt{lambda.min} and @tt{lambda.1se}
        (@hyperlink["https://github.com/bkc39/glmnet/issues/27"]{#27}).}
  @item{Generic @tt{predict} and @tt{coef}, a Gaussian predictor and printed
        summaries (@hyperlink["https://github.com/bkc39/glmnet/issues/25"]{#25}).}
  @item{Coefficient-path and CV-error plots
        (@hyperlink["https://github.com/bkc39/glmnet/issues/28"]{#28}).}
  @item{Observation weights, @tt{penalty.factor}, coefficient limits, offsets
        and @tt{exclude}
        (@hyperlink["https://github.com/bkc39/glmnet/issues/12"]{#12}).}
  @item{Sparse predictor matrices
        (@hyperlink["https://github.com/bkc39/glmnet/issues/11"]{#11}).}
]

@(close-eval ev)
