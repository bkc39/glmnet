#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "concepts" #:style 'toc]{Concepts}

Every model family in @racketmodname[glmnet] takes its data the same way, and
shares one penalty and one shape of result. This chapter covers those shared pieces; the
@secref["examples"] then take each family in turn.

A @deftech{fit} is the result of estimating a model on a dataset. Its
@deftech{solution} consists of the parameter values found by minimizing the
model's objective function.

@local-table-of-contents[]

@section[#:tag "concepts-data"]{Data}

Every fit, path and cross-validation procedure, and @racket[predict], takes
its data in one of two ways: unnamed or named.

@bold{Unnamed data} is the predictors, one row per observation and one column
per predictor, with a separate response. The predictors are a
@racketmodname[math/matrix] matrix, a list or vector of rows, each a list or
vector, or a @tech{design matrix}:

@examples[#:eval ev #:label #f
(require math/matrix)
(define X (matrix [[1.0 2.0] [2.0 1.0] [3.0 4.0] [4.0 3.0] [5.0 6.0]]))
(define y '(1 4 3 6 5))
(coef (ols X y))
(equal? (ols X y) (ols (matrix->list* X) y))
]

A @deftech{design matrix} is a @racket[design-matrix?] value: the predictors
in the layout the solver reads, which every procedure converts its data to.
Converting once yourself checks the data once, for many fits (see
@secref["data-explicit"]).

The @deftech{response} has one entry per row: a list, a vector, an
@racket[flvector], an @racket[f64vector], a @racketmodname[math/array] array
or a Polars series. Its entries depend on the family:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Family}                    @bold{Response})
       (list "Gaussian"                       "reals")
       (list "Binomial"                       @elem{@racket[0]/@racket[1], or two class labels: strings, symbols or booleans})
       (list "Multinomial"                    @elem{class numbers @math{0, …, K−1}, every class present, or class labels})
       (list "Cox"                            @elem{positive times @emph{and}, separately, @racket[0]/@racket[1] event indicators})
       (list "Poisson"                        "non-negative counts")
       (list "Multi-response Gaussian"        "a matrix with one column per response, in any form of unnamed data"))]

Data is checked before the solver runs. Every entry must be a finite real,
every row the same length, and the response one entry per row; an error names
the row and the column. R's @tt{glmnet} stops on a missing value too, but fits
some infinite ones; here each is an error:

@examples[#:eval ev #:label #f
(eval:error (ols (matrix [[1.0 2.0] [2.0 +nan.0] [3.0 4.0]]) '(1 4 3)))
(eval:error (ols X '(1.0 2.0)))
]

@subsection[#:tag "concepts-named"]{Named data}

@bold{Named data} is a @deftech{table}, an association list or a hash from
column names to columns, or a Polars dataframe. The second argument names the
response column, and @racket[#:predictors], then required, the predictor
columns. No other column is read:

@examples[#:eval ev #:label #f
(define patients
  '((age 34 51 67 45 29)
    (dose 2.5 5.0 1.0 3.5 4.0)
    (response 3.1 5.2 1.9 4.0 4.4)))
(define by-name (ols patients "response" #:predictors '("age" "dose")))
(coef by-name)
(predict by-name '((dose 3.0) (age 40)))
]

A fit from named data predicts from named data by column name, in any order.
The Cox and multi-response families name several response columns (see
@secref["ref-common-data"]).

A @deftech{formula}, written with @racket[~], names the response and the
predictors in one expression, and keys the coefficients by name:

@examples[#:eval ev #:label #f
(coef (formula-fit (~ response (+ age dose)) patients #:lambda 0))
]

@secref["formulas"] covers formulas, and @secref["data"] where data comes
from.

@section[#:tag "concepts-penalty"]{The penalty: @math{α} and @math{λ}}

For the Gaussian family, glmnet solves

@centered{@math{min_{β₀,β} 1/(2n) ‖y − β₀ − Xβ‖² + λ [ α‖β‖₁ + (1−α)/2 ‖β‖₂² ]}}

over the intercept @math{β₀} and the coefficients @math{β}. The other families
replace the squared-error term with their negative log-likelihood; the penalty
is the same for all of them. It has two knobs:

@itemlist[
 @item{@bold{@racket[#:alpha], @math{α ∈ [0, 1]}}, mixes the penalty.
       @math{α = 0} is the ridge (L2) penalty, which shrinks every coefficient
       but zeroes none; @math{α = 1} is the lasso (L1) penalty, which sets
       coefficients exactly to zero; values in between are the elastic net.
       Every fit procedure defaults to @racket[1.0].}
 @item{@bold{@racket[#:lambda], @math{λ ≥ 0}}, sets the penalty's strength.
       A single fit requires it; to fit a whole sequence of values at once, use
       a @tech{regularization path} (@secref["concepts-path"]). @math{λ = 0} is
       the unpenalized fit.}
]

The four Gaussian models are one routine, @racket[elnet-fit], with different
arguments; the named procedures only fix @racket[#:alpha]:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Procedure}           @bold{@math{α}}   @bold{@math{λ}})
       (list @racket[ols]               "(ignored)"       "0")
       (list @racket[ridge]             "0"               "> 0")
       (list @racket[lasso]             "1"               "> 0")
       (list @racket[elastic-net]       "0 < α < 1"       "> 0"))]

@examples[#:eval ev #:label #f
(equal? (lasso X y #:lambda 0.1) (elnet-fit X y #:alpha 1.0 #:lambda 0.1))
(eval:error (lasso X y))
]

@section[#:tag "concepts-families"]{Model families}

Each family pairs a response type with a glmnet solver and a link from the
linear predictor @math{η = β₀ + xβ} to the response:

@itemlist[
 @item{The @deftech{Gaussian family} (@tt{elnet}) models a numeric response
       with the identity link: @math{E[y] = η}.}
 @item{The @deftech{binomial family} (@tt{lognet}) models a 0/1 label through
       the log-odds of class 1: @math{log(P(y=1) / P(y=0)) = η}.}
 @item{The @deftech{multinomial family} (@tt{lognet} with @math{K > 2}
       classes) fits one linear predictor per class, @math{η_k = a0_k + xβ_k},
       and takes class probabilities as their softmax.}
 @item{The @deftech{Cox family} (@tt{coxnet}) models survival times through the
       proportional hazard @math{h(t | x) = h₀(t) exp(xβ)}. The baseline hazard
       @math{h₀} absorbs the intercept, so there is none.}
 @item{The @deftech{Poisson family} (@tt{fishnet}) models counts with the log
       link: @math{E[y] = exp(η)}.}
 @item{The @deftech{multi-response Gaussian family} (@tt{multelnet}) fits one
       Gaussian model per response column under a grouped penalty: each
       predictor enters for every response or for none.}
]

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Family}          @bold{Fit}                 @bold{Result}                  @bold{Prediction})
       (list "Gaussian"             @racket[elnet-fit]         @racket[elnet-result]          @racket[elnet-predict])
       (list "Binomial"             @racket[logistic-fit]      @racket[logistic-result]       @elem{@racket[logistic-predict-proba], @racket[logistic-predict]})
       (list "Multinomial"          @racket[multinomial-fit]   @racket[multinomial-result]    @elem{@racket[multinomial-predict-proba], @racket[multinomial-predict]})
       (list "Cox"                  @racket[cox-fit]           @racket[cox-result]            @elem{@racket[cox-linear-predictor], @racket[cox-relative-risk]})
       (list "Poisson"              @racket[poisson-fit]       @racket[poisson-result]        @racket[poisson-predict-mean])
       (list "Multi-response"       @racket[mgaussian-fit]     @racket[mgaussian-result]      @racket[mgaussian-predict]))]

All six fit procedures take the same keywords: @racket[#:lambda],
@racket[#:alpha], @racket[#:standardize?], @racket[#:intercept?] (not
@racket[cox-fit]), @racket[#:thresh] and @racket[#:max-iters].
Each also has a path counterpart that fits many values of @math{λ} at once
(see @secref["concepts-path"]), and a cross-validation counterpart that
chooses among them (see @secref["concepts-cv"]). @racket[predict] and
@racket[coef] work on every result (@secref["concepts-predict"]).

@section[#:tag "concepts-results"]{Results}

A fit prints as a one-line summary: its family, @math{λ}, the deviance ratio
and how many predictors have a nonzero coefficient. The generics of
@secref["concepts-predict"] read it:

@examples[#:eval ev #:label #f
(define fit (ridge X y #:lambda 0.1))
fit
(match-define (vector β₀ β₁ β₂) (coef fit))
β₁
(deviance-ratio fit)
]

@racket[coef] gives the intercept, then one coefficient per predictor, on the
original (unstandardized) scale; a predictor the penalty dropped is exactly
@racket[0.0]. Cox models have no intercept. The multinomial and
multi-response families give one such vector per class or response; as in R,
the multinomial intercepts are centred to sum to zero. @racket[deviance-ratio]
is the fraction of the null deviance explained, @math{R²} for the Gaussian
families.

Each family's result is a transparent struct, so @racket[equal?] compares
fits. Its fields, the intercept, coefficients, deviance ratio, @math{λ} and
number of solver passes, have accessors (see @secref["ref-gaussian"]).

@section[#:tag "concepts-path"]{Regularization paths}

The right @math{λ} is rarely known in advance. A @deftech{regularization path}
fits a whole decreasing sequence of @math{λ} in one call, starting each fit
from the solution before it, which is much cheaper than fitting each value
separately. It is how R's @tt{glmnet(x, y)} is normally used. Every family has
a path fitter: @racket[elnet-path], @racket[logistic-path],
@racket[multinomial-path], @racket[cox-path], @racket[poisson-path] and
@racket[mgaussian-path]. They take the same keywords as the single fits,
except that @racket[#:lambda] is optional and, when given, a list.

Without @racket[#:lambda], glmnet chooses the sequence the way R does:
@racket[#:nlambda] values (default @racket[100]) from @math{λ_max}, the
smallest @math{λ} at which every coefficient is zero, down to
@racket[#:lambda-min-ratio] times @math{λ_max} (default @racket[0.01] when there
are fewer observations than predictors, otherwise @racket[1e-4]). A path
prints as R prints one: a row for each fitted @math{λ}, with the number of
nonzero coefficients (@tt{Df}) and the percentage of the null deviance
explained (@tt{%Dev}):

@examples[#:eval ev #:label #f
(define path (elnet-path X y #:nlambda 12))
path
]

At the first @math{λ} every coefficient is zero, and the predictors enter as
the penalty relaxes, here @math{x₁} first and then @math{x₂}. The path has
fewer values than asked for because glmnet, like R, stops an automatic
sequence once another @math{λ} would barely change the fit. For the Gaussian
family that is when @math{R²} gains less than @racket[1e-5] of its own value
from one @math{λ} to the next, or passes @racket[0.999]; the reference lists
each family's rule (@secref["ref-path"]).

@racket[in-path] walks a path, giving each fitted @math{λ} with its
coefficients as @racket[coef] gives them. With @racket[#:lambda], the path
fits those values, largest first:

@examples[#:eval ev #:label #f
(define user-path (elnet-path X y #:lambda '(0.01 0.5 0.1)))
(for ([(λ β) (in-path user-path)])
  (printf "λ = ~a: ~a\n" λ β))
]

Each point agrees with the single fit at that @math{λ} to within the solver's
tolerance. The early-stopping rule does not apply to a user sequence, but any
path can still end early, for example where glmnet does not converge within
@racket[#:max-iters] passes at one of its values. It then keeps the values
before that one and logs a warning (@secref["concepts-convergence"]):

@examples[#:eval ev #:label #f
(elnet-path X y #:lambda '(0.5 0.1 0.01) #:max-iters 1)
]

@racket[predict] and @racket[coef] evaluate a path at any @math{λ}
(@secref["concepts-predict"]). Cross-validation chooses among the values on a
path (@secref["concepts-cv"]). @racket[plot-coefficient-path], from
@racketmodname[glmnet/plot], plots a path's coefficients against @math{λ}, as
R's @tt{plot} does (@secref["plot-path"]).

@section[#:tag "concepts-predict"]{Predictions and coefficients}

Every result, whether a single fit or a @tech{regularization path}, is a
@racket[glmnet-model?]. Three procedures work on all of them:

@itemlist[
 @item{@racket[predict] evaluates the model on new data, as R's @tt{predict}
       does: unnamed data with one column per predictor, or, for a model
       fitted from named data or a @tech{formula}, named data;}
 @item{@racket[coef] returns the intercept, then one coefficient per
       predictor, as R's @tt{coef} does, keyed by name for a model fitted from
       a formula;}
 @item{@racket[deviance-ratio] returns the fraction of null deviance
       explained, which R keeps in a fit's @tt{dev.ratio} field.}
]

@examples[#:eval ev #:label #f
(define fit (lasso X y #:lambda 0.1))
(predict fit (matrix [[6.0 5.0] [7.0 8.0]]))
(coef fit)
(deviance-ratio fit)
]

@subsection[#:tag "concepts-predict-type"]{What is predicted}

@racket[#:type] chooses what @racket[predict] returns, with R's names:
@racket['link], the default, is the linear predictor; @racket['response] is on
the scale of the response; and @racket['class] is the predicted class, for the
two families that have classes:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Family}                  @racket['link]                   @racket['response]                @racket['class])
       (list "Gaussian"                     @math{η = β₀ + xβ}              @math{η}                          "---")
       (list "Binomial"                     @elem{log-odds @math{η}}         @elem{@math{P(y = 1)}}            @elem{@racket[1] if @math{η > 0}, else @racket[0]})
       (list "Multinomial"                  @elem{@math{η_k}, one per class} @elem{softmax of the @math{η_k}}  @elem{the class with the largest @math{η_k}})
       (list "Cox"                          @math{xβ}                        @elem{relative risk @math{exp(xβ)}} "---")
       (list "Poisson"                      @math{log μ = η}                 @elem{mean @math{μ = exp(η)}}     "---")
       (list "Multi-response"               @elem{@math{η_r}, one per response} @math{η_r}                   "---"))]

@examples[#:eval ev #:label #f
(define labels '(0 1 0 1 1))
(define clf (logistic-fit X labels #:lambda 0.05))
(define new-X (matrix [[1.0 1.0] [5.0 5.0]]))
(predict clf new-X)
(predict clf new-X #:type 'response)
(predict clf new-X #:type 'class)
(eval:error (predict fit X #:type 'class))
]

The prediction helpers of each family are @racket[predict] at a fixed type:
@racket[logistic-predict-proba] is @racket[#:type 'response], for example, and
@racket[elnet-predict] is the Gaussian default.

@subsection[#:tag "concepts-predict-lambda"]{Predicting along a path}

For a path, @racket[#:lambda] chooses the @math{λ} at which to evaluate, as
R's @tt{s} does. It is a single @math{λ} or a list; for a list, the result has
one entry per element, in the order given. Without @racket[#:lambda], a path
gives one entry per fitted @math{λ}:

@examples[#:eval ev #:label #f
(coef user-path #:lambda 0.1)
(predict user-path (matrix [[6.0 5.0]]))
(predict user-path (matrix [[6.0 5.0]]) #:lambda '(0.01 0.5))
]

A @math{λ} that was not fitted is handled as R handles it by default: the
coefficients are interpolated linearly in @math{λ} between the two fitted
values on either side. For @math{λ_l > s > λ_r},

@centered{@math{β(s) = w β(λ_l) + (1 − w) β(λ_r),  w = (s − λ_r) / (λ_l − λ_r)},}

and the same holds for the intercept. At @math{s = 0.2}, between
@math{0.5} and @math{0.1}, the weight is @math{w = 0.25}:

@examples[#:eval ev #:label #f
(coef user-path #:lambda 0.2)
(for/vector ([hi (in-vector (coef user-path #:lambda 0.5))]
             [lo (in-vector (coef user-path #:lambda 0.1))])
  (+ (* 0.25 hi) (* 0.75 lo)))
]

A @math{λ} above the largest fitted value, or below the smallest, is clamped
to that end of the path:

@examples[#:eval ev #:label #f
(equal? (coef user-path #:lambda 5.0) (coef user-path #:lambda 0.5))
(equal? (coef user-path #:lambda 0.0) (coef user-path #:lambda 0.01))
]

Interpolated coefficients approximate the fit at @math{s}; they are not that
fit. The closer together the fitted values, the better the approximation. To
get the fit itself, fit at @math{s}: R's @tt{exact = TRUE} refits, and here a
single fit or a path through @math{s} does the same:

@examples[#:eval ev #:label #f
(coef (lasso X y #:lambda 0.2))
]

A single fit is a path with one @math{λ}, so, as in R, it gives the same
answer whatever @racket[#:lambda] says:

@examples[#:eval ev #:label #f
(equal? (coef fit #:lambda 0.5) (coef fit))
]

@section[#:tag "concepts-cv"]{Choosing λ by cross-validation}

A @tech{regularization path} offers a fit at every @math{λ}. Cross-validation
chooses among them by estimating how well each fit predicts observations it
was not fitted to. Every family has a cross-validation procedure, the
equivalent of R's @tt{cv.glmnet}: @racket[elnet-cv], @racket[logistic-cv],
@racket[multinomial-cv], @racket[cox-cv], @racket[poisson-cv] and
@racket[mgaussian-cv]. Each takes the arguments of the family's path fitter
and works as R's does:

@itemlist[#:style 'ordered
 @item{Fit the path to all the data. Its @math{λ} values are the candidates.}
 @item{Split the observations at random into @math{K} folds, 10 by default.
       For each fold, fit the path to the other @math{K − 1} folds, its
       @emph{training data}.}
 @item{Predict each observation from the path whose training data left it
       out, at every candidate @math{λ}, and measure the error of each
       prediction: by default the squared error for the Gaussian families and
       the deviance for the others.}
 @item{Average the errors within each fold. At each @math{λ}, the mean of the
       @math{K} fold averages, weighted by the folds' sizes, is the
       cross-validated error, and their spread gives its standard error.}
 @item{Choose @math{λ}: @deftech{lambda-min} is the @math{λ} with the smallest
       cross-validated error, and @deftech{lambda-1se} the largest @math{λ}
       whose error is within one standard error of that smallest error.}
]

Unless @racket[#:lambda] fixes the sequence, each fold's path chooses its own
@math{λ} values, as in R, and is evaluated at the candidates by the
interpolation of @secref["concepts-predict-lambda"].

Here are 60 observations of 8 predictors, of which only the first three carry
signal, with @math{y = 1 + 3x₁ − 2x₂ + x₃} plus noise:

@examples[#:eval ev #:label #f
(random-seed 8)
(define (noise) (- (* 2 (random)) 1))
(define X60 (build-matrix 60 8 (lambda (i j) (noise))))
(define β (col-matrix [3.0 -2.0 1.0 0.0 0.0 0.0 0.0 0.0]))
(define y60
  (for/list ([μ (in-list (matrix->list (matrix* X60 β)))])
    (+ 1.0 μ (noise))))
(define cv (elnet-cv X60 y60))
cv
]

The result is a @racket[glmnet-cv]. It prints like R's @tt{print.cv.glmnet},
with the index counted from 0: the measure, then a row for each of the two
choices, with its @math{λ}, its index among the candidates, the
cross-validated error, its standard error and the number of nonzero
coefficients (see @secref["ref-model-printing"]).

@subsection[#:tag "concepts-cv-choice"]{Reading lambda-min and lambda-1se}

The smallest cross-validated error is only an estimate, and the standard
error says how uncertain it is. @tech{lambda-min} minimizes the estimate.
@tech{lambda-1se} takes the simplest model, the one with the most
regularization, that the estimate cannot tell apart from the best, and it is
what R uses by default. Here it keeps exactly the three predictors that matter,
while @tech{lambda-min} also keeps three of the noise predictors, with small
coefficients:

@examples[#:eval ev #:label #f
(glmnet-cv-lambda-min cv)
(glmnet-cv-lambda-1se cv)
(coef cv #:lambda 'lambda-min)
(coef cv #:lambda 'lambda-1se)
]

The whole curve is in the result: @racket[glmnet-cv-lambda] holds the
candidates, and @racket[glmnet-cv-cvm], @racket[glmnet-cv-cvsd] and
@racket[glmnet-cv-nzero] the error, its standard error and the number of
nonzero coefficients at each. Every fifth candidate, from the largest:

@examples[#:eval ev #:label #f
(define (every-fifth v) (in-vector v 0 #f 5))
(for ([λ (every-fifth (glmnet-cv-lambda cv))]
      [err (every-fifth (glmnet-cv-cvm cv))]
      [se (every-fifth (glmnet-cv-cvsd cv))]
      [nz (every-fifth (glmnet-cv-nzero cv))])
  (printf "λ = ~a, error = ~a ± ~a, nonzero = ~a\n"
          (~r λ #:precision '(= 3)) (~r err #:precision '(= 3))
          (~r se #:precision '(= 3)) nz))
]

The error falls quickly as the three real predictors enter, reaches its
minimum, and then rises slowly as the noise predictors are fitted.
@racket[plot-cv], from @racketmodname[glmnet/plot], draws this curve as R's
@tt{plot} draws a @tt{cv.glmnet} result; see @secref["plot-cv"].

@subsection[#:tag "concepts-cv-predict"]{Predicting at the chosen λ}

A @racket[glmnet-cv] is a @racket[glmnet-model?] whose path is the full-data
path, so @racket[predict] and @racket[coef] work on it. As R's
@tt{predict.cv.glmnet} does, they default to @tech{lambda-1se};
@racket[#:lambda] also takes @racket['lambda-min], @racket['lambda-1se] or any
number. @racket[deviance-ratio] answers at @tech{lambda-1se}:

@examples[#:eval ev #:label #f
(define new-rows (matrix [[0.5 -0.5 0.0 0.0 0.0 0.0 0.0 0.0]
                           [0.0 0.0 1.0 0.9 -0.9 0.9 -0.9 0.9]]))
(predict cv new-rows)
(predict cv new-rows #:lambda 'lambda-min)
(equal? (coef cv)
        (coef (glmnet-cv-path cv) #:lambda (glmnet-cv-lambda-1se cv)))
(deviance-ratio cv)
]

The true means of the two rows are @math{1 + 1.5 + 1 = 3.5} and
@math{1 + 1 = 2}. Both predictions fall short of them, those at
@tech{lambda-1se}, with the stronger penalty, by more.

@subsection[#:tag "concepts-cv-folds"]{Folds}

@racket[#:nfolds] sets the number of folds, at least 3. The folds are drawn by
@racket[random-fold-ids] from @racket[current-pseudo-random-generator], so a
second call gives different folds and a slightly different curve. To repeat a
result, seed the generator, or pass the folds with @racket[#:fold-ids]: one
fold id per observation, counting from 0. A result records its folds, so they
can be reused:

@examples[#:eval ev #:label #f
(glmnet-cv-lambda-min (elnet-cv X60 y60))
(define (seeded seed)
  (parameterize ([current-pseudo-random-generator
                  (make-pseudo-random-generator)])
    (random-seed seed)
    (elnet-cv X60 y60 #:nfolds 5)))
(equal? (seeded 1) (seeded 1))
(define folds (glmnet-cv-fold-ids cv))
(equal? (elnet-cv X60 y60 #:fold-ids folds) cv)
]

Fixed folds are also how to compare models fairly: cross-validate each value of
@racket[#:alpha] on the same folds, and the differences in error come from the
models rather than from the split. @secref["ex-elastic-net-cv"] does this.

@subsection[#:tag "concepts-cv-measures"]{Measures of error}

@racket[#:type-measure] chooses how a prediction's error is measured, with R's
names and R's default for each family:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Measure}         @bold{Error of a prediction}                          @bold{Families})
       (list @racket['mse]          "squared error on the response scale"                 "all but Cox; the default for the Gaussian families")
       (list @racket['deviance]     "the family's deviance"                               "all; the default for the others")
       (list @racket['mae]          "absolute error on the response scale"                "all but Cox")
       (list @racket['class]        "misclassification rate"                              "binomial, multinomial")
       (list @racket['auc]          "area under the ROC curve, per fold"                  "binomial")
       (list @racket['C]            "Harrell's concordance index, per fold"               "Cox"))]

For the Gaussian families @racket['deviance] is the squared error, as in R.
For classifiers, @racket['mse] and @racket['mae] compare the predicted
probabilities with 0/1 indicators of the classes. @racket['auc] and
@racket['C] are better when larger, so @tech{lambda-min} maximizes them.

@examples[#:eval ev #:label #f
(define labels (for/list ([v (in-list y60)]) (if (> v 1.0) 1 0)))
(logistic-cv X60 labels #:type-measure 'class #:fold-ids folds)
]

By default the errors are averaged within each fold first, which R calls
@emph{grouped}; @racket[#:grouped? #f] computes the error and its standard
error over the individual observations instead. As in R, the folds are never
grouped when they average fewer than 3 observations, @racket['auc] needs an
average of 10 observations per fold and otherwise falls back to
@racket['deviance], and each of these changes logs a warning.

For Cox models, the deviance of a fold is computed as R computes it. Grouped,
it is the deviance of all the data less that of the fold's training data, at
the fold's coefficients. Ungrouped, it is the deviance of the held-out fold
alone, which is undefined when the fold has no event or when its first event
is among its last two observations in time order; R stops then, and so does
@racket[cox-cv], naming the fold. When the folds average fewer than 10
observations, the Cox deviance is grouped even with @racket[#:grouped? #f],
with a warning, as in R.

@section[#:tag "concepts-standardize"]{Standardization and the intercept}

By default each predictor is scaled to unit variance before fitting and the
coefficients are reported on the original scale, which is also the R package's
default. The penalty acts on the standardized coefficients, so predictors
measured in different units are penalized evenly. Pass
@racket[#:standardize? #f] to penalize the raw coefficients instead; a
penalized fit then changes:

@examples[#:eval ev #:label #f
(coef (ridge X y #:lambda 0.1))
(coef (ridge X y #:lambda 0.1 #:standardize? #f))
]

Every family except Cox fits an unpenalized intercept. Pass
@racket[#:intercept? #f] to force it to zero:

@examples[#:eval ev #:label #f
(coef (ols X y #:intercept? #f))
]

@section[#:tag "concepts-convergence"]{Convergence and errors}

Coordinate descent cycles over the coefficients until no update moves the
objective by more than @racket[#:thresh] (default @racket[1e-7]) times the null
deviance, or until @racket[#:max-iters] passes (default @racket[100000]).
@racket[ols] defaults to a tighter @racket[1e-10], because the unpenalized
solution is approached slowly. The pass count is recorded in every result:

@examples[#:eval ev #:label #f
(elnet-result-num-passes (lasso X y #:lambda 0.1))
(elnet-result-num-passes (lasso X y #:lambda 0.1 #:thresh 1e-12))
]

Problems are reported in three ways:

@itemlist[
 @item{Before the solver runs, an argument of the wrong type, a ragged or
       empty matrix, a non-finite entry, or a response of the wrong length
       raises @racket[exn:fail:contract] (see @secref["concepts-data"]).}
 @item{Data that glmnet cannot fit raises @racket[exn:fail] with a readable
       message: for example when every predictor is constant, when a class
       probability collapses under perfect separation (a larger
       @racket[#:lambda] usually fixes that), or when a Gaussian response is
       constant, as R reports too.}
 @item{When glmnet fits no @math{λ} at all, for example because the solver
       hits @racket[#:max-iters] before converging, the fit raises
       @racket[exn:fail] with glmnet's reason, naming the procedure called. R
       warns and returns an empty model instead. A @tech{regularization path}
       that fails after fitting some @math{λ} keeps those and logs a warning
       with the topic @racket['glmnet], as R warns. The warning names the
       procedure called, and for cross-validation what it was fitting: all
       the data, or the training data of a held-out fold.}
]

@examples[#:eval ev #:label #f
(eval:error (ols (matrix [[1.0 2.0] [1.0 2.0] [1.0 2.0]]) '(1.0 2.0 3.0)))
(eval:error (lasso X y #:lambda 0.01 #:max-iters 1))
]

@section[#:tag "concepts-precision"]{The native library}

The solver is R glmnet 4.1's own double-precision Fortran, the last release
with every family in Fortran, in a native library, @tt{libglmnetcompat}, that
the package installs prebuilt. When the package loads, it checks that the
library's reals are 8 bytes (@racket[glmnet-default-real-bytes]) and that it
exports the entry points this version expects
(@racket[glmnet-capi-abi-version]).

To use a library built from source instead, set the
@envvar{GLMNET_NATIVE_LIB_PATH} environment variable to the directory whose
@filepath{lib} subdirectory holds it before installing the package.

@(close-eval ev)
