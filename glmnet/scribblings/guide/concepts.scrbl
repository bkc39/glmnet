#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "concepts" #:style 'toc]{Concepts}

Every model family in @racketmodname[glmnet] shares one data layout, one
penalty and one shape of result. This chapter covers those shared pieces; the
@secref["examples"] then take each family in turn.

@local-table-of-contents[]

@section[#:tag "concepts-data"]{Data layout}

A @deftech{design matrix} holds the predictors: one row per observation and one
column per predictor. Every fit and prediction procedure accepts it in either
of two forms:

@itemlist[
 @item{a non-empty list of rows, each a list of reals of the same length; or}
 @item{a @racket[design-matrix?] value, which holds the matrix in the layout
       the Fortran reads.}
]

@examples[#:eval ev #:label #f
(define X '((1.0 2.0)
            (2.0 1.0)
            (3.0 4.0)
            (4.0 3.0)
            (5.0 6.0)))
(define y '(1 4 3 6 5))
(elnet-result-coefficients (ols X y))
]

The Fortran reads a matrix as one array of doubles stored column by column:
element @math{(i, j)} of a matrix with @math{n} rows is at index
@math{i + jn}, counting from 0. A @racket[design-matrix?] value holds exactly
that array, together with the numbers of rows and columns and, optionally,
column names. @racket[rows->design-matrix] builds one from a list of rows:

@examples[#:eval ev #:label #f
(define D (rows->design-matrix X #:column-names '(x1 x2)))
D
(design-matrix-ref D 2 1)
(design-matrix-column-names D)
(require ffi/vector)
(f64vector->list (design-matrix->f64vector D))
]

A fit on @racket[D] gives the same result as a fit on @racket[X]. The list is
converted on every call, while @racket[D] was converted once. The solver copies
its input before working on it, so one design matrix serves any number of
fits:

@examples[#:eval ev #:label #f
(equal? (ols D y) (ols X y))
(for/list ([lam (in-list '(1.0 0.1 0.01))])
  (elnet-result-coefficients (lasso D y #:lambda lam)))
]

The column names are carried along, but the results do not use them yet.
@racket[columns->design-matrix] builds a design matrix from columns, and
@racket[f64vector->design-matrix] from an array that is already in the
column-major layout. @racket[design-matrix->rows],
@racket[design-matrix->columns] and @racket[design-matrix->f64vector] convert
back:

@examples[#:eval ev #:label #f
(define C (columns->design-matrix '((1 2 3) (4 5 6))))
(design-matrix->rows C)
(define v (f64vector 1.0 2.0 3.0 4.0 5.0 6.0))
(equal? (f64vector->design-matrix v 3 2) C)
]

Every conversion checks its input once, before any solver runs. The matrix
must have at least one row and one column, every row must have the same length,
and every entry must be a real, finite number. Exact numbers become flonums.
An error names the offending row and column, counting from 0:

@examples[#:eval ev #:label #f
(design-matrix->rows (rows->design-matrix '((1 1/2) (-3 2.5))))
(eval:error (rows->design-matrix '((1.0 2.0) (3.0))))
(eval:error (ols '((1.0 2.0) (2.0 +nan.0) (3.0 4.0) (4.0 3.0) (5.0 6.0)) y))
]

Without the check, a @racket[+nan.0] or an infinity would reach the solver,
which would return meaningless coefficients and no error.

The @deftech{response} has one entry per row of the design matrix. Its shape
depends on the family:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Family}                    @bold{Response})
       (list "Gaussian"                       "a list of reals")
       (list "Binomial"                       @elem{a list of @racket[0]/@racket[1] class labels})
       (list "Multinomial"                    @elem{a list of class labels @math{0, …, K−1}, every class present})
       (list "Cox"                            @elem{a list of positive times @emph{and} a list of @racket[0]/@racket[1] event indicators})
       (list "Poisson"                        "a list of non-negative counts")
       (list "Multi-response Gaussian"        @elem{a matrix with one row per observation and one column per response, in either form of a @tech{design matrix}}))]

A response is checked in the same way as a design matrix, by
@racket[response->f64vector]: every entry must be finite, and there must be one
per row of the design matrix:

@examples[#:eval ev #:label #f
(eval:error (ols X '(1.0 4.0 +inf.0 6.0 5.0)))
(eval:error (ols X '(1.0 2.0)))
]

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
(equal? (elnet-result-coefficients (lasso X y #:lambda 0.1))
        (elnet-result-coefficients (elnet-fit X y #:alpha 1.0 #:lambda 0.1)))
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
chooses among them (see @secref["concepts-cv"]). Each prediction helper is @racket[predict] with a
fixed @racket[#:type]; @secref["concepts-predict"] covers @racket[predict] and
@racket[coef], which work on every result.

@section[#:tag "concepts-results"]{Results}

A fit returns a transparent struct. The fields follow one pattern across the
families:

@itemlist[
 @item{@bold{intercept} --- a real, or a vector of one per class or response
       (@racket[multinomial-result-intercepts],
       @racket[mgaussian-result-intercepts]). Cox results have none. As in R,
       the multinomial intercepts are centred to sum to zero; adding the same
       constant to each would not change any probability.}
 @item{@bold{coefficients} --- a dense vector with one entry per predictor, on
       the original (unstandardized) scale; a predictor the penalty dropped is
       exactly @racket[0.0]. Multinomial and multi-response results hold a
       vector of such vectors, one per class or response.}
 @item{@bold{r-squared} or @bold{dev-ratio} --- the fraction of the null
       deviance explained. For the Gaussian families this is @math{R²}.}
 @item{@bold{lambda} --- the penalty the solver used.}
 @item{@bold{num-passes} --- the number of coordinate-descent passes.}
]

A result prints as a one-line summary: its family, @math{λ}, deviance ratio,
and how many of the predictors have a nonzero coefficient. The fields are read
with the struct's accessors:

@examples[#:eval ev #:label #f
(define fit (ridge X y #:lambda 0.1))
fit
(elnet-result-r-squared fit)
]

Because the structs are transparent, @racket[equal?] compares them field by
field and @racket[match] destructures them:

@examples[#:eval ev #:label #f
(require racket/match)
(match-define (elnet-result a0 beta _ _ _) fit)
(list a0 beta)
]

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
are fewer observations than predictors, otherwise @racket[1e-4]):

@examples[#:eval ev #:label #f
(vector-length (glmnet-path-lambda (elnet-path X y)))
(define path (elnet-path X y #:nlambda 12))
(vector-ref (glmnet-path-coefficients path) 0)
(glmnet-path-df path)
]

A @racket[glmnet-path] holds one entry per fitted @math{λ} in each of its
@racket[lambda], @racket[intercepts], @racket[coefficients],
@racket[dev-ratio] and @racket[df] fields. At the first @math{λ} every
coefficient is zero; @racket[df] counts the predictors in the model as the
penalty relaxes, here @math{x₁} first and then @math{x₂}. The default path has
50 values, not 100, because glmnet, like R, stops once another @math{λ} would
barely change the fit: when the deviance ratio improves by less than
@racket[1e-5] or passes @racket[0.999].

A path prints as R prints one: for each fitted @math{λ}, the number of
nonzero coefficients (@tt{Df}), the percentage of the null deviance explained
(@tt{%Dev}) and @math{λ}:

@examples[#:eval ev #:label #f
path
]

With @racket[#:lambda], the path fits exactly those values, largest first:

@examples[#:eval ev #:label #f
(define user-path (elnet-path X y #:lambda '(0.01 0.5 0.1)))
(glmnet-path-lambda user-path)
(glmnet-path-coefficients user-path)
]

Each point agrees with the single fit at that @math{λ} to within the solver's
tolerance. @racket[predict] and @racket[coef] evaluate a path at any @math{λ}
(@secref["concepts-predict"]). Cross-validation chooses among the values on a
path (@secref["concepts-cv"]).

To plot a path's coefficients against @math{λ}, as R's @tt{plot} does, use
@racket[plot-coefficient-path] from @racketmodname[glmnet/plot]; see
@secref["plot-path"].

@section[#:tag "concepts-predict"]{Predictions and coefficients}

Every result, whether a single fit or a @tech{regularization path}, is a
@racket[glmnet-model?]. Three procedures work on all of them, as R's generics
of the same names do:

@itemlist[
 @item{@racket[predict] evaluates the model on new rows, given as a
       @tech{design matrix} with one column per predictor;}
 @item{@racket[coef] returns the intercept, then one coefficient per
       predictor;}
 @item{@racket[deviance-ratio] returns the fraction of null deviance
       explained.}
]

@examples[#:eval ev #:label #f
(define fit (lasso X y #:lambda 0.1))
(predict fit '((6.0 5.0) (7.0 8.0)))
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
(predict clf '((1.0 1.0) (5.0 5.0)))
(predict clf '((1.0 1.0) (5.0 5.0)) #:type 'response)
(predict clf '((1.0 1.0) (5.0 5.0)) #:type 'class)
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
(glmnet-path-lambda user-path)
(coef user-path #:lambda 0.1)
(predict user-path '((6.0 5.0)))
(predict user-path '((6.0 5.0)) #:lambda '(0.01 0.5))
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
(define X60
  (for/list ([i (in-range 60)])
    (for/list ([j (in-range 8)])
      (noise))))
(define beta '(3.0 -2.0 1.0 0.0 0.0 0.0 0.0 0.0))
(define y60
  (for/list ([row (in-list X60)])
    (+ 1.0
       (for/sum ([b (in-list beta)] [x (in-list row)]) (* b x))
       (noise))))
(define cv (elnet-cv X60 y60))
cv
]

The result is a @racket[glmnet-cv]. It prints as R prints one: the measure,
then a row for each of the two choices, with its @math{λ}, its index among the
candidates (counting from 0), the cross-validated error, its standard error and
the number of nonzero coefficients.

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
(define (round3 x)
  (/ (round (* 1000 x)) 1000))
(define (rounded v)
  (for/list ([b (in-vector v)])
    (round3 b)))
(rounded (coef cv #:lambda 'lambda-min))
(rounded (coef cv #:lambda 'lambda-1se))
]

The whole curve is in the result: @racket[glmnet-cv-lambda] holds the
candidates, and @racket[glmnet-cv-cvm], @racket[glmnet-cv-cvsd] and
@racket[glmnet-cv-nzero] the error, its standard error and the number of
nonzero coefficients at each. Every fifth candidate, from the largest:

@examples[#:eval ev #:label #f
(for ([lam (in-vector (glmnet-cv-lambda cv))]
      [err (in-vector (glmnet-cv-cvm cv))]
      [se (in-vector (glmnet-cv-cvsd cv))]
      [nz (in-vector (glmnet-cv-nzero cv))]
      [i (in-naturals)]
      #:when (zero? (remainder i 5)))
  (printf "~a: λ = ~a, error = ~a ± ~a, nonzero = ~a\n"
          i (round3 lam) (round3 err) (round3 se) nz))
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
(define new-rows '((0.5 -0.5 0.0 0.0 0.0 0.0 0.0 0.0)
                   (0.0 0.0 1.0 0.9 -0.9 0.9 -0.9 0.9)))
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
grouped when they have fewer than 3 observations each, @racket['auc] needs 10
observations per fold and otherwise falls back to @racket['deviance], and each
of these changes logs a warning. For Cox models, the deviance of a fold is
computed as R computes it: the deviance of all the data less that of the
fold's training data, at the fold's coefficients.

@section[#:tag "concepts-standardize"]{Standardization and the intercept}

By default each predictor is scaled to unit variance before fitting and the
coefficients are reported on the original scale, which is also the R package's
default. The penalty acts on the standardized coefficients, so predictors
measured in different units are penalized evenly. Pass
@racket[#:standardize? #f] to penalize the raw coefficients instead; a
penalized fit then changes:

@examples[#:eval ev #:label #f
(elnet-result-coefficients (ridge X y #:lambda 0.1))
(elnet-result-coefficients (ridge X y #:lambda 0.1 #:standardize? #f))
]

Every family except Cox fits an unpenalized intercept. Pass
@racket[#:intercept? #f] to force it to zero:

@examples[#:eval ev #:label #f
(elnet-result-intercept (ols X y #:intercept? #f))
(elnet-result-coefficients (ols X y #:intercept? #f))
]

@section[#:tag "concepts-convergence"]{Convergence and errors}

Coordinate descent cycles over the coefficients until no update moves the
objective by more than @racket[#:thresh] (default @racket[1e-7]) times the null
deviance, or until
@racket[#:max-iters] passes (default @racket[100000]). @racket[ols] defaults to
a tighter @racket[1e-10], because the unpenalized solution is approached slowly.
The pass count is recorded in every result:

@examples[#:eval ev #:label #f
(elnet-result-num-passes (lasso X y #:lambda 0.1))
(elnet-result-num-passes (lasso X y #:lambda 0.1 #:thresh 1e-12))
]

Problems are reported in three ways:

@itemlist[
 @item{Before the solver runs, an argument of the wrong type, a ragged or
       empty matrix, a non-finite entry, or a response of the wrong length
       raises @racket[exn:fail:contract] (see @secref["concepts-data"]).}
 @item{A fatal condition reported by glmnet raises @racket[exn:fail] with a
       readable message, for example when every predictor is constant, or when
       a class probability collapses under perfect separation (a larger
       @racket[#:lambda] usually fixes that).}
 @item{If the solver hits @racket[#:max-iters] before converging, the result
       is still returned, holding partial coefficients, and a warning is logged
       to the current logger.}
]

@examples[#:eval ev #:label #f
(eval:error (ols '((1.0 2.0) (1.0 2.0) (1.0 2.0)) '(1.0 2.0 3.0)))
]

@section[#:tag "concepts-precision"]{The native library}

@tt{libglmnetcompat} is R glmnet 4.1's own double-precision Fortran, the last
release with every family in Fortran, behind a small C-ABI shim. When the
package loads, it checks
that the library's reals are 8 bytes (@racket[glmnet-default-real-bytes]) and
that it exports the entry points this version expects
(@racket[glmnet-capi-abi-version]).

The pre-install hook stages the library from the first of these that exists:

@itemlist[#:style 'ordered
 @item{the directory named by the @envvar{GLMNET_NATIVE_LIB_PATH} environment
       variable (its @filepath{lib} subdirectory), as the Nix build sets it;}
 @item{a library already staged in @filepath{glmnet/native-libs/};}
 @item{the prebuilt candidate for the platform under
       @filepath{glmnet/native-libs/candidates/}.}
]

To rebuild the library from source you need a Fortran toolchain:

@verbatim|{
  # via Nix (also runs the Fortran ctest suite)
  nix build .#native

  # or directly with CMake and gfortran
  cmake -S fortran -B fortran/build -DBUILD_TESTING=ON
  cmake --build fortran/build
  ctest --test-dir fortran/build --output-on-failure
}|

@(close-eval ev)
