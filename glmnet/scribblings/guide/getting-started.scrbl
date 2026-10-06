#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "getting-started" #:style 'toc]{Getting started}

This chapter fits a model to the mtcars dataset, reads its coefficients,
and predicts from it. It starts with ordinary least squares, then introduces
regularization and fitting from named columns.

@local-table-of-contents[]

@section[#:tag "gs-installing"]{Installing}

@commandline{raco pkg install --auto glmnet datasets}

The @tt{datasets} package supplies the real data used in this chapter and
requires Racket 9.3 or later. Load the modelling library:

@examples[#:eval ev #:label #f
(require glmnet)
(glmnet-capi-abi-version)
]

@section[#:tag "gs-first-fit"]{A First Regression}

A @tech{fit} is the result of solving an optimization problem on a dataset.
For a Gaussian model, the objective described in the
@hyperlink["https://glmnet.stanford.edu/articles/glmnet.html"]{R glmnet vignette}
is:

@centered{@math{min}@subscript{@math{β₀,β}} @math{1/(2n) ∑ᵢ₌₁ⁿ (yᵢ − β₀ − xᵢᵀβ)² + λ [α‖β‖₁ + (1−α)/2 ‖β‖₂²]}}

The @tech{solution} gives the intercept @math{β₀} and the predictor
coefficients @math{β}. Here @math{n} is the number of observations,
@math{xᵢ} holds the features for observation @math{i}, and @math{yᵢ} is its
observed response. The parameter @math{λ ≥ 0} controls the penalty's strength,
and @math{α ∈ [0, 1]} mixes the lasso and ridge penalties. Ordinary least
squares (OLS) sets @math{λ = 0}, so only the squared-error term remains.
See @secref["concepts-penalty"] for the other choices.

A @tech{design matrix} @math{X} holds the predictors: one row per observation
and one column per feature. The response @math{y} is separate, with one
value per row. The library accepts lists or vectors of rows, or a
@racket[design-matrix?] value produced by one of its data adapters.

Start with R's @tt{mtcars} dataset: 32 cars with measurements of fuel
economy and vehicle characteristics. We will predict fuel economy
(@tt{mpg}, miles per US gallon) from weight (@tt{wt}, in thousands of pounds)
and horsepower (@tt{hp}). Load and display the Polars dataframe:

@examples[#:eval ev #:label #f
(require datasets glmnet/data/polars (prefix-in pl: polars) racket/match)
(define cars (load-dataset 'mtcars #:format 'polars))
cars
]

The dataframe includes a text column, @tt{model}, identifying each car.
Select the two numeric predictors by name with
@racket[polars->design-matrix], and extract the numeric response with
@racket[polars->response]:

@examples[#:eval ev #:label #f
(define feature-names '("wt" "hp"))
(define X (polars->design-matrix cars feature-names))
(define y (polars->response cars "mpg"))
(list (design-matrix-nrows X) (design-matrix-ncols X) (length y))
]

@racket[ols] fits a Gaussian model without a penalty. Fit it directly inside
@racket[match-define]. The @racket[and] pattern binds the whole result as
@racket[fit] and extracts its intercept, coefficients, fraction of variance
explained (@math{R²}), penalty strength, and number of solver passes:

@examples[#:eval ev #:label #f
(match-define
  (and fit (elnet-result β₀ β R² λ n-steps))
  (ols X y))
fit
(list β₀ β R² λ n-steps)
]

The intercept and coefficients describe the fitted linear relationship.
With weight held constant, the horsepower coefficient describes the change
in predicted fuel economy per additional horsepower; the weight coefficient
holds horsepower constant. @math{R²} describes the fit to these observations,
without estimating performance on new observations.

@racket[coef] gives the intercept followed by the coefficients, in predictor
column order, through the same interface for every family:

@examples[#:eval ev #:label #f
(coef fit)
]

To shrink the coefficients, fit the same data with a penalty. For example,
@racket[lasso] applies an L1 penalty and can set coefficients to zero:

@examples[#:eval ev #:label #f
(define penalized-fit (lasso X y #:lambda 1.0))
penalized-fit
(coef penalized-fit)
]

Fit a @tech{regularization path} to try many values of @math{λ} at once.
@racket[elnet-path] uses the lasso penalty by default. Its summary shows the
number of nonzero coefficients, percentage of deviance explained, and
@math{λ} for each fitted value:

@examples[#:eval ev #:label #f
(define path (elnet-path X y #:nlambda 12))
path
]

To choose a penalty using held-out observations, @racket[elnet-cv]
cross-validates a path. It estimates prediction error at each @math{λ} and
selects @tt{lambda.min} and @tt{lambda.1se}; @secref["concepts-cv"] explains
how to use those results. For plots of paths and cross-validation curves,
require @racketmodname[glmnet/plot] and see @secref["plots"].

@subsection[#:tag "gs-plots"]{Plotting the fit and path}

Load @racketmodname[glmnet/plot] for coefficient paths and Racket's
@racketmodname[plot] for general plots. These examples return picts, which
this manual and DrRacket display as images. @racket[plot-pict] draws an
image rather than opening a plot window.

@; Keep the displayed interactive require, but render picts without loading
@; the GUI in raco setup's documentation worker places and sandbox namespace.
@examples[#:eval ev #:label #f
(eval:alts (require glmnet/plot plot)
           (require glmnet/plot plot/no-gui))
]

@subsubsection[#:tag "gs-predicted-actual"]{Predicted versus actual values}

Compare the OLS predictions with the observed responses. Points on the
reference line have equal predicted and actual values:

@examples[#:eval ev #:label #f
(define fitted (predict fit X))
(plot-pict
 (list (function values #:color "gray" #:style 'short-dash)
       (points (map vector fitted y) #:alpha 0.5 #:size 3))
 #:x-label "Predicted fuel economy (mpg)"
 #:y-label "Actual fuel economy (mpg)"
 #:title "Mtcars: OLS predicted vs actual"
 #:width 640 #:height 360)
]

Points above the line are underpredictions; points below it are
overpredictions. These predictions use the training observations, so
this plot does not estimate prediction error on new data.

@subsubsection[#:tag "gs-residuals"]{Residuals versus fitted values}

For the OLS fit, a residual is the observed response minus its fitted value:
@math{rᵢ = yᵢ − ŷᵢ}. Plot all 32 residuals against their fitted values, with
a horizontal reference line at zero:

@examples[#:eval ev #:label #f
(define residuals (map - y fitted))
(plot-pict
 (list (hrule 0 #:color "gray" #:style 'short-dash)
       (points (map vector fitted residuals) #:alpha 0.5 #:size 3))
 #:x-label "Fitted fuel economy (mpg)"
 #:y-label "Residual (observed - fitted)"
 #:title "Mtcars: OLS residuals vs fitted"
 #:width 640 #:height 360)
]

Curvature can suggest a missing nonlinear relationship; a widening spread
can suggest that the residual variance changes with the fitted response.
These are residuals on the training observations, so the plot is a model
diagnostic rather than an estimate of prediction error on new data.

@subsubsection[#:tag "gs-lasso-path"]{The lasso coefficient path}

Fit a denser path to the same mtcars predictors and response, explicitly
choosing the lasso penalty with @racket[#:alpha 1.0]:

@examples[#:eval ev #:label #f
(define lasso-path (elnet-path X y #:alpha 1.0))
(plot-coefficient-path lasso-path #:label #t
                       #:width 640 #:height 360
                       #:title "Mtcars: lasso coefficient path")
]

Each curve is a predictor's coefficient as the penalty weakens from left to
right. The horizontal axis is @math{−log λ}; the top axis counts the nonzero
coefficients. The curve labels are predictor positions, starting at 1, in
@racket[feature-names] order. See @secref["plots"] for other axis choices
and cross-validation plots.

@section[#:tag "gs-formulas"]{Fitting from named columns}

A @tech{table} associates column names with their values. Convert the Polars
dataframe with @racket[polars->table], then express the same model as a
@tech{formula}. Name the response and the two predictors directly:

@examples[#:eval ev #:label #f
(define data (polars->table cars))
(define named-fit
  (formula-fit (~ mpg (+ wt hp)) data #:lambda 0 #:thresh 1e-10))
named-fit
(coef named-fit)
(equal? (predict named-fit data) (predict fit X))
]

The formula predicts @racket[mpg] from @racket[wt] and @racket[hp], leaving
the other columns out. With @racket[#:lambda 0], this fits the same OLS
model as @racket[fit].
@racket[#:thresh 1e-10] matches the tighter convergence threshold used by
@racket[ols]. The coefficients are keyed by name, with the intercept under
@racket["(Intercept)"] instead of occupying the first position in a list.
@racket[#:family] chooses among the six families; @racket[formula-path] and
@racket[formula-cv] fit a path and cross-validate one. See
@secref["formulas"] for the formula interface.

@section[#:tag "gs-models"]{Choosing a model}

@racket[ols], @racket[ridge], @racket[lasso], and @racket[elastic-net] use the
same Gaussian solver, @racket[elnet-fit], with different choices of
@racket[#:alpha] and @racket[#:lambda]. They suit a numeric response such as
the mtcars dataset's fuel economy.

The other families model different responses: binomial and multinomial
models classify observations, Poisson models describe counts, Cox models
use survival times and event indicators, and multi-response Gaussian models
fit several numeric responses together. @secref["concepts-families"] lists
the response forms, and @secref["examples"] has a worked example for each.

@section[#:tag "gs-predicting"]{Predicting}

@racket[predict] evaluates a fit on predictor rows in the same column order
as @racket[X]. For a Gaussian model it returns the fitted response values.
Here we use the first three cars to demonstrate prediction:

@examples[#:eval ev #:label #f
(define prediction-X (design-matrix-select-rows X '(0 1 2)))
(predict fit prediction-X)
]

These rows were also used for fitting, so the predictions are fitted values,
not a test of performance on unseen data. To predict new observations,
prepare their weight and horsepower in the same units as the training data
and in the same column order, and pass them to @racket[predict].

For the formula model, supply a table of predictors. Their names determine
which coefficients apply, so column order does not matter and the response
column is unnecessary:

@examples[#:eval ev #:label #f
(define prediction-data
  (polars->table (pl:head cars 3) (reverse feature-names)))
(predict named-fit prediction-data)
]

A classifier also accepts @racket[#:type] to choose its linear predictor,
probabilities (@racket['response]), or classes (@racket['class]); see
@secref["concepts-predict"]. A path predicts at selected values of
@math{λ}, including values between those it fitted.

@section[#:tag "gs-api-gaps"]{What is not there yet}

Several R @tt{glmnet} features have no binding yet. Each has an open issue:

@itemlist[
  @item{Observation weights, @tt{penalty.factor}, coefficient limits, offsets
        and @tt{exclude}
        (@hyperlink["https://github.com/bkc39/glmnet/issues/12"]{#12}).}
  @item{Sparse predictor matrices
        (@hyperlink["https://github.com/bkc39/glmnet/issues/11"]{#11}).}
]

@section[#:tag "gs-license"]{License}

This package is distributed under @bold{GPL-2.0-or-later} (see @tt{LICENSE}
at the root of the repository). It is assembled from parts under different
licences:

@itemlist[
  @item{The Racket bindings, the Fortran layer that gives the solver a C
        interface (@tt{fortran/glmnet_capi.f90}) and this manual:
        GPL-2.0-or-later.}
  @item{The solver: R glmnet 4.1's own Fortran, @tt{glmnet5dpclean.f},
        vendored byte for byte under @tt{fortran/vendor/}. R glmnet declares
        it @bold{GPL-2} only. @tt{fortran/vendor/NOTICE.md} records its
        source, commit and checksum.}
  @item{The example datasets that @racketmodname[glmnet/datasets] loads: R
        glmnet 4.1-10's data, GPL-2 only, and R's own @tt{mtcars} and
        @tt{iris}, part of R, under GPL-2 or GPL-3.}
  @item{The libraries it builds on, such as rkt-polars and Racket's plot
        library, are permissively licensed (Apache-2.0 or MIT).}
]

The solver is GPL-2 only, so whether the package as a whole can be offered
under "or later" terms is an open question, tracked in
@hyperlink["https://github.com/bkc39/glmnet/issues/62"]{#62}.

@section[#:tag "gs-acknowledgements"]{Acknowledgements}

The solver and its algorithms are the work of the authors of the
@hyperlink["https://glmnet.stanford.edu/"]{R glmnet package}: Jerome Friedman,
Trevor Hastie, Rob Tibshirani, Balasubramanian Narasimhan, Kenneth Tay and
Noah Simon, with Junyang Qian. Their vignettes shaped this manual, whose
chapters follow them closely, and their R package is the reference the test
suite checks every result against.

The example datasets come from R glmnet and from R's @tt{datasets} package,
maintained by the R Core Team. The tables in the guide are read and shaped
with rkt-polars, the Racket bindings to the Polars dataframe library, and the
plots are drawn with Racket's plot library.

@section[#:tag "gs-ai-disclosure"]{AI disclosure}

This package was built with substantial help from AI coding agents. Claude, by
Anthropic, running in Claude Code, wrote most of the code, the tests and this
manual under the maintainer's direction. As of October 2026, 30 of the 31
commits on the main branch credit Claude as a co-author.

Every change went through a pull request. AI reviewer agents also reviewed
most of them, and the maintainer decided what was merged. The numbers do not
rest on the agents' word. An automated parity suite compares the package's
results with R glmnet 4.1.10's on the same data, and it runs as part of the
package's checks.

@(close-eval ev)
