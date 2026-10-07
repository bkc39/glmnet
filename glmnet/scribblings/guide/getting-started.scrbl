#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "getting-started" #:style 'toc]{Getting started}

This chapter fits a model to R's mtcars data, reads its coefficients, chooses
a penalty by cross-validation and predicts the fuel economy of a new car.

@local-table-of-contents[]

@section[#:tag "gs-installing"]{Installing}

@commandline{raco pkg install --auto glmnet datasets}

The @tt{datasets} package supplies this chapter's data and requires Racket 9.3
or later. Load the modelling library:

@examples[#:eval ev #:label #f
(require glmnet)
]

@section[#:tag "gs-first-fit"]{A First Regression}

A @tech{fit} is the result of solving an optimization problem on a dataset.
For a Gaussian model, the objective described in the
@hyperlink["https://glmnet.stanford.edu/articles/glmnet.html"]{R glmnet vignette}
is:

@centered{@math{min}@subscript{@math{β₀,β}} @math{1/(2n) ∑ᵢ₌₁ⁿ (yᵢ − β₀ − xᵢᵀβ)² + λ [α‖β‖₁ + (1−α)/2 ‖β‖₂²]}}

The @tech{solution} gives the intercept @math{β₀} and the predictor
coefficients @math{β}. Each of the @math{n} observations has predictors
@math{xᵢ} and a response @math{yᵢ}. The penalty's strength is @math{λ ≥ 0},
and @math{α ∈ [0, 1]} mixes the lasso and ridge penalties. Ordinary least
squares (OLS) sets @math{λ = 0}; see @secref["concepts-penalty"] for the other
choices.

R's @tt{mtcars} dataset measures 32 cars. Load it as a Polars dataframe:

@examples[#:eval ev #:label #f
(require datasets)
(define cars (load-dataset 'mtcars #:format 'polars))
cars
]

Predict fuel economy (@tt{mpg}, miles per US gallon) from weight (@tt{wt}, in
thousands of pounds) and horsepower (@tt{hp}). @racket[ols] fits without a
penalty; name the response column, then the predictor columns:

@examples[#:eval ev #:label #f
(define fit (ols cars "mpg" #:predictors '("wt" "hp")))
fit
(coef fit)
]

The fit prints its family, @math{λ}, the fraction of variance explained
(@math{R²}) and how many coefficients are nonzero. @racket[coef] gives the
intercept, then one coefficient per predictor, in the order named. Name them
with @racket[match-define]:

@examples[#:eval ev #:label #f
(match-define (vector β₀ β-wt β-hp) (coef fit))
(~r (* 100 β-hp) #:precision 2)
(deviance-ratio fit)
]

@racket[(* 100 β-hp)] is the change in predicted fuel economy for 100 more
horsepower at the same weight. @racket[deviance-ratio] is @math{R²}.

A penalty shrinks the coefficients. @racket[lasso] applies the L1 penalty,
which can set coefficients to zero; at @math{λ = 4} it drops horsepower:

@examples[#:eval ev #:label #f
(define penalized-fit (lasso cars "mpg" #:predictors '("wt" "hp") #:lambda 4.0))
(coef penalized-fit)
]

A @tech{regularization path} fits many values of @math{λ} at once.
@racket[elnet-path] uses the lasso penalty by default. Its summary shows, for
each @math{λ}, the number of nonzero coefficients and the percentage of
deviance explained:

@examples[#:eval ev #:label #f
(elnet-path cars "mpg" #:predictors '("wt" "hp") #:nlambda 12)
]

@subsection[#:tag "gs-plots"]{Plotting the fit and path}

@racketmodname[glmnet/plot] draws coefficient paths and cross-validation
curves, and Racket's @racketmodname[plot] everything else. These examples
return picts, which this manual and DrRacket show as images:

@examples[#:eval ev #:label #f
(eval:alts (require glmnet/plot plot (only-in polars ref series->list))
           (require glmnet/plot plot/no-gui (only-in polars ref series->list)))
]

@subsubsection[#:tag "gs-predicted-actual"]{Predicted versus actual values}

Points on the dashed line have equal predicted and actual values; points above
it are underpredictions:

@examples[#:eval ev #:label #f
(define fitted (predict fit cars))
(define actual (series->list (ref cars "mpg")))
(plot-pict
 (list (function values #:color "gray" #:style 'short-dash)
       (points (map vector fitted actual) #:alpha 0.5 #:size 3))
 #:x-label "Predicted fuel economy (mpg)"
 #:y-label "Actual fuel economy (mpg)"
 #:title "Mtcars: OLS predicted vs actual"
 #:width 640 #:height 360)
]

These are the cars the model was fitted to, so this plot, the next and
@math{R²} describe the fit, not its error on new cars;
@secref["gs-cv"] estimates that.

@subsubsection[#:tag "gs-residuals"]{Residuals versus fitted values}

A residual is the observed response minus its fitted value,
@math{rᵢ = yᵢ − ŷᵢ}:

@examples[#:eval ev #:label #f
(plot-pict
 (list (hrule 0 #:color "gray" #:style 'short-dash)
       (points (map vector fitted (map - actual fitted)) #:alpha 0.5 #:size 3))
 #:x-label "Fitted fuel economy (mpg)"
 #:y-label "Residual (observed - fitted)"
 #:title "Mtcars: OLS residuals vs fitted"
 #:width 640 #:height 360)
]

Curvature suggests a missing nonlinear term; a widening spread suggests that
the variance changes with the fitted response.

@subsubsection[#:tag "gs-lasso-path"]{The lasso coefficient path}

@examples[#:eval ev #:label #f
(plot-coefficient-path (elnet-path cars "mpg" #:predictors '("wt" "hp"))
                       #:label #t
                       #:width 640 #:height 360
                       #:title "Mtcars: lasso coefficient path")
]

Each curve is a predictor's coefficient, labelled with its name, as the
penalty weakens from left to right. The horizontal axis is @math{−log λ}; the
top axis counts the nonzero coefficients. See @secref["plots"] for the other
plots.

@subsection[#:tag "gs-cv"]{Choosing λ by cross-validation}

@racket[elnet-cv] cross-validates a path: it fits the path without each fold
of cars in turn and measures the error of predicting the fold. Fixed fold ids
make the result repeatable:

@examples[#:eval ev #:label #f
(define cv
  (elnet-cv cars "mpg" #:predictors '("wt" "hp")
            #:fold-ids (for/list ([i 32]) (modulo i 8))))
cv
(glmnet-cv-lambda-min cv)
(glmnet-cv-lambda-1se cv)
]

@tech{lambda-min} has the smallest cross-validated error; @tech{lambda-1se},
the default for @racket[coef] and @racket[predict] on @racket[cv], is the
largest @math{λ} within one standard error of it. @racket[plot-cv] draws the
error curve, with the two marked:

@examples[#:eval ev #:label #f
(plot-cv cv #:width 640 #:height 360
         #:title "Mtcars: cross-validated error")
]

@secref["concepts-cv"] covers folds and error measures.

@section[#:tag "gs-formulas"]{Fitting from named columns}

A @tech{formula}, written with @racket[~], names the response and the
predictors in one expression:

@examples[#:eval ev #:label #f
(define named-fit (formula-fit (~ mpg (+ wt hp)) cars #:lambda 4.0))
(coef named-fit)
(equal? (predict named-fit cars) (predict penalized-fit cars))
]

It is the lasso fit above, with its coefficients keyed by name. Formulas also
build interactions, transforms and factors; @racket[#:family] chooses among
the six families, and @racket[formula-path] and @racket[formula-cv] fit a path
and cross-validate one. See @secref["formulas"].

@section[#:tag "gs-models"]{Choosing a model}

@racket[ols], @racket[ridge], @racket[lasso] and @racket[elastic-net] are one
Gaussian solver, @racket[elnet-fit], with different @racket[#:alpha] and
@racket[#:lambda]. They suit a numeric response such as fuel economy.

The other families model other responses: binomial and multinomial models
classify, Poisson models count, Cox models take survival times and event
indicators, and multi-response Gaussian models fit several numeric responses
together. @secref["concepts-families"] lists their responses, and
@secref["examples"] works through each.

@section[#:tag "gs-predicting"]{Predicting}

@racket[predict] reads new data by the predictors' names, in any column order.
Predict a car of 3,000 pounds and 150 horsepower:

@examples[#:eval ev #:label #f
(define new-car '((wt 3.0) (hp 150)))
(predict fit new-car)
(predict named-fit new-car)
(predict cv new-car #:lambda 'lambda-1se)
]

The penalized prediction at @tech{lambda-1se} is pulled toward the mean fuel
economy. A classifier's @racket[#:type] chooses its linear predictor,
probabilities or classes; see @secref["concepts-predict"].

@section[#:tag "gs-api-gaps"]{What is not there yet}

Several R @tt{glmnet} features have no binding yet. Each has an open issue:

@itemlist[
  @item{Observation weights, @tt{penalty.factor}, coefficient limits, offsets
        and @tt{exclude}
        (@hyperlink["https://github.com/bkc39/glmnet/issues/12"]{#12}).}
  @item{Sparse predictor matrices
        (@hyperlink["https://github.com/bkc39/glmnet/issues/11"]{#11}).}
]

@(close-eval ev)
