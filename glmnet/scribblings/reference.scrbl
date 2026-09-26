#lang scribble/manual
@(require "utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "reference"]{Reference}

@declare-exporting[glmnet]

Every binding below is provided by @racketmodname[glmnet]. Each model family
has a fit procedure, a path fitter, a transparent result struct and prediction
helpers. Every result type, and @racket[glmnet-path], also implements the
generic interface of @secref["ref-model"]: @racket[predict], @racket[coef] and
@racket[deviance-ratio] work on all of them, and they print as summaries.
@secref["concepts"] explains how the pieces fit together.

@section[#:tag "ref-common"]{Common arguments}

The fit procedures share their argument conventions:

@itemlist[
 @item{@racket[X] is a @tech{design matrix}, one row per observation: a
       @racket[design-matrix?] value or a non-empty list of equal-length rows
       of reals (see @racket[design-matrix/c] and @secref["ref-data"]).
       Prediction helpers take new data in the same forms, with as many columns
       as the fit has coefficients.}
 @item{@racket[#:lambda] is the penalty strength @math{λ ≥ 0}. It is
       required.}
 @item{@racket[#:alpha] is the mixing parameter @math{α ∈ [0, 1]}:
       @racket[0.0] is the ridge penalty, @racket[1.0] (the default) the
       lasso, and values in between the elastic net.}
 @item{@racket[#:standardize?] scales each predictor to unit variance before
       fitting; coefficients are always reported on the original scale.}
 @item{@racket[#:intercept?] fits an unpenalized intercept; @racket[#f] fixes
       it at zero.}
 @item{@racket[#:thresh] is the coordinate-descent convergence threshold and
       @racket[#:max-iters] the maximum number of passes.}
]

Before the solver runs, an argument of the wrong type, a ragged or empty
matrix, a non-finite entry, or a response of the wrong length raises
@racket[exn:fail:contract]; the message names the offending row and column or
position. A fatal condition reported by glmnet
raises @racket[exn:fail] with a readable message. If the solver reaches
@racket[#:max-iters] without converging, the fit returns partial coefficients
and logs a warning.

@section[#:tag "ref-data"]{Input data}

@defmodule[glmnet/data #:no-declare]

A @racket[design-matrix?] value holds a @tech{design matrix} in the layout the
Fortran reads: an array of doubles stored column by column, so that element
@math{(i, j)} of a matrix with @math{n} rows is at index @math{i + jn}, with
indices counted from 0. It also records the numbers of rows and columns, and
optional column names. See @secref["concepts-data"].

Every conversion into a design matrix copies its input and checks it: the
matrix needs at least one row and one column, every row (or column) the same
length, and every entry must be a real, finite number. Exact numbers become
flonums. A violation raises @racket[exn:fail:contract], with a message that
names the offending row and column. Because nothing can modify a design matrix
after it is built, it passes these checks for its whole life, and one design
matrix can be passed to any number of fits.

@racketmodname[glmnet] re-exports every binding in this section.
@racketmodname[glmnet/data] provides them without loading the native library.

@defproc[(design-matrix? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a design matrix, and @racket[#f]
  otherwise. Two design matrices are @racket[equal?] when they have the same
  dimensions, entries and column names. A design matrix prints with its
  dimensions only.

  @examples[#:eval ev
  (define D (rows->design-matrix '((1.0 4.0) (2.0 5.0) (3.0 6.0))))
  (design-matrix? D)
  D
  (equal? D (columns->design-matrix '((1 2 3) (4 5 6))))]}

@defthing[design-matrix/c flat-contract?]{
  The contract on the @racket[X] argument of every fit and prediction
  procedure, and on the response matrix @racket[Y] of @racket[mgaussian-fit]
  and @racket[mgaussian-path]. It accepts a @racket[design-matrix?] value, or a
  list of lists that @racket[rows->design-matrix] then converts and checks.
  Equivalent to @racket[(or/c design-matrix? (listof list?))].

  @examples[#:eval ev
  (require racket/contract)
  (contract-first-order-passes? design-matrix/c D)
  (contract-first-order-passes? design-matrix/c '((1.0 2.0) (3.0 4.0)))
  (contract-first-order-passes? design-matrix/c '(1.0 2.0))]}

@defproc[(rows->design-matrix [rows (listof list?)]
                              [#:column-names column-names
                                              (or/c #f (listof (or/c string? symbol?)))
                                              #f])
         design-matrix?]{
  Builds a design matrix from a list of rows, one per observation. Every row
  must have the same length, and every entry must be a real, finite number.
  @racket[column-names], when given, names the columns: one distinct string or
  symbol per column.

  @examples[#:eval ev
  (define named (rows->design-matrix '((1 2) (3 4)) #:column-names '(age dose)))
  (design-matrix->rows named)
  (design-matrix-column-names named)
  (eval:error (rows->design-matrix '((1.0 2.0) (3.0 4.0) (5.0))))
  (eval:error (rows->design-matrix '((1.0 2.0) (3.0 +inf.0))))
  (eval:error (rows->design-matrix '((1.0 2.0)) #:column-names '(a)))]}

@defproc[(columns->design-matrix [columns (listof list?)]
                                 [#:column-names column-names
                                                 (or/c #f (listof (or/c string? symbol?)))
                                                 #f])
         design-matrix?]{
  Builds a design matrix from a list of columns, one per predictor, under the
  same rules as @racket[rows->design-matrix].

  @examples[#:eval ev
  (design-matrix->rows (columns->design-matrix '((1 2 3) (4 5 6))))
  (eval:error (columns->design-matrix '((1.0 2.0 3.0) (4.0 +nan.0 6.0))))]}

@defproc[(f64vector->design-matrix [v f64vector?]
                                   [nrows exact-positive-integer?]
                                   [ncols exact-positive-integer?]
                                   [#:column-names column-names
                                                   (or/c #f (listof (or/c string? symbol?)))
                                                   #f])
         design-matrix?]{
  Builds a design matrix from @racket[v], an array already in the column-major
  layout: element @math{(i, j)} is at index @math{i + j · nrows}. The length of
  @racket[v] must be @racket[(* nrows ncols)] and every entry must be finite.
  The design matrix holds a copy, so later changes to @racket[v] do not
  affect it.

  @examples[#:eval ev
  (require ffi/vector)
  (define v (f64vector 1.0 2.0 3.0 4.0 5.0 6.0))
  (design-matrix->rows (f64vector->design-matrix v 3 2))
  (design-matrix->rows (f64vector->design-matrix v 2 3))
  (eval:error (f64vector->design-matrix v 4 2))]}

@deftogether[(@defproc[(design-matrix-nrows [dm design-matrix?]) exact-positive-integer?]
              @defproc[(design-matrix-ncols [dm design-matrix?]) exact-positive-integer?])]{
  The number of rows (observations) and columns (predictors) of @racket[dm].

  @examples[#:eval ev
  (design-matrix-nrows D)
  (design-matrix-ncols D)]}

@defproc[(design-matrix-column-names [dm design-matrix?])
         (or/c #f (listof (or/c string? symbol?)))]{
  The column names @racket[dm] was built with, or @racket[#f] if it has none.
  The fit results do not use them yet.

  @examples[#:eval ev
  (design-matrix-column-names D)
  (define cols '((1 2) (3 4)))
  (design-matrix-column-names
   (columns->design-matrix cols #:column-names '("x1" "x2")))]}

@defproc[(design-matrix-ref [dm design-matrix?]
                            [i exact-nonnegative-integer?]
                            [j exact-nonnegative-integer?])
         flonum?]{
  The entry in row @racket[i] and column @racket[j] of @racket[dm], counting
  from 0.

  @examples[#:eval ev
  (design-matrix-ref D 2 1)
  (eval:error (design-matrix-ref D 3 0))]}

@deftogether[(@defproc[(design-matrix->rows [dm design-matrix?]) (listof (listof flonum?))]
              @defproc[(design-matrix->columns [dm design-matrix?]) (listof (listof flonum?))])]{
  The entries of @racket[dm] as a list of rows or as a list of columns.

  @examples[#:eval ev
  (design-matrix->rows D)
  (design-matrix->columns D)]}

@defproc[(design-matrix->f64vector [dm design-matrix?]) f64vector?]{
  A fresh copy of the column-major array that @racket[dm] holds.

  @examples[#:eval ev
  (f64vector->list (design-matrix->f64vector D))]}

@defproc[(response->f64vector [y list?]) f64vector?]{
  Converts a non-empty list of real, finite numbers to an @racket[f64vector],
  the form in which a @tech{response} reaches the Fortran. The fit procedures
  apply the same conversion to their responses, after checking that the
  response has one entry per row of @racket[X]. An error names the position of
  the offending entry, counting from 0.

  @examples[#:eval ev
  (f64vector->list (response->f64vector '(1 1/2 2.5)))
  (eval:error (response->f64vector '(1.0 +nan.0 2.0)))]}

@section[#:tag "ref-gaussian"]{Gaussian models}

The @tech{Gaussian family}. The four named models are @racket[elnet-fit] with a
fixed @racket[#:alpha]. See @secref["ex-ols"], @secref["ex-ridge"],
@secref["ex-lasso"] and @secref["ex-elastic-net"].

@defstruct*[elnet-result ([intercept real?]
                          [coefficients (vectorof real?)]
                          [r-squared real?]
                          [lambda real?]
                          [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted Gaussian model. @racket[coefficients] has one entry per column of
  @racket[_X], on the original predictor scale; @racket[r-squared] is the
  fraction of variance explained; @racket[lambda] is the penalty the solver
  used; @racket[num-passes] is the number of coordinate-descent passes.

  Like every result type, it prints as a summary of its family, @math{λ},
  deviance ratio and number of nonzero coefficients (see
  @secref["ref-model-printing"]), and it implements @racket[gen:glmnet-model].

  @examples[#:eval ev
  (define X '((1.0 2.0 1.0) (2.0 1.0 4.0) (3.0 4.0 9.0)
              (4.0 3.0 16.0) (5.0 6.0 25.0) (6.0 5.0 36.0)))
  (define y '(1.0 4.0 3.0 6.0 5.0 8.0))
  (define fit (lasso X y #:lambda 0.05))
  fit
  (elnet-result-intercept fit)
  (elnet-result-coefficients fit)]}

@defproc[(elnet-fit [X design-matrix/c]
                    [y (and/c (listof real?) pair?)]
                    [#:lambda lambda (>=/c 0)]
                    [#:alpha alpha (real-in 0 1) 1.0]
                    [#:standardize? standardize? boolean? #t]
                    [#:intercept? intercept? boolean? #t]
                    [#:thresh thresh (>/c 0) 1e-7]
                    [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Fits a Gaussian elastic-net model of the response @racket[y], one real per
  row of @racket[X], at a single @racket[lambda].

  @examples[#:eval ev
  (elnet-fit X y #:alpha 0.5 #:lambda 0.5)]}

@defproc[(ols [X design-matrix/c]
              [y (and/c (listof real?) pair?)]
              [#:standardize? standardize? boolean? #t]
              [#:intercept? intercept? boolean? #t]
              [#:thresh thresh (>/c 0) 1e-10]
              [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Ordinary least squares: @racket[elnet-fit] with @racket[#:lambda 0.0], where
  @racket[#:alpha] has no effect. The default @racket[thresh] is tighter than
  the penalized fits' because coordinate descent approaches the unpenalized
  solution slowly.

  @examples[#:eval ev
  (elnet-result-coefficients
   (ols '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0) (5.0 6.0))
        '(1.0 4.0 3.0 6.0 5.0)))]}

@defproc[(ridge [X design-matrix/c]
                [y (and/c (listof real?) pair?)]
                [#:lambda lambda (>=/c 0)]
                [#:standardize? standardize? boolean? #t]
                [#:intercept? intercept? boolean? #t]
                [#:thresh thresh (>/c 0) 1e-7]
                [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Ridge regression: @racket[elnet-fit] with @racket[#:alpha 0.0]. Shrinks
  every coefficient toward zero as @racket[lambda] grows, but sets none exactly
  to zero.

  @examples[#:eval ev
  (elnet-result-coefficients (ridge X y #:lambda 0.1))]}

@defproc[(lasso [X design-matrix/c]
                [y (and/c (listof real?) pair?)]
                [#:lambda lambda (>=/c 0)]
                [#:standardize? standardize? boolean? #t]
                [#:intercept? intercept? boolean? #t]
                [#:thresh thresh (>/c 0) 1e-7]
                [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  The lasso: @racket[elnet-fit] with @racket[#:alpha 1.0]. Sets coefficients
  exactly to zero, more of them as @racket[lambda] grows.

  @examples[#:eval ev
  (elnet-result-coefficients (lasso X y #:lambda 0.5))]}

@defproc[(elastic-net [X design-matrix/c]
                      [y (and/c (listof real?) pair?)]
                      [#:alpha alpha (real-in 0 1)]
                      [#:lambda lambda (>=/c 0)]
                      [#:standardize? standardize? boolean? #t]
                      [#:intercept? intercept? boolean? #t]
                      [#:thresh thresh (>/c 0) 1e-7]
                      [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  The elastic net at an explicit @racket[alpha], which is required here.
  @racket[#:alpha 0.0] is @racket[ridge] and @racket[#:alpha 1.0] is
  @racket[lasso].

  @examples[#:eval ev
  (elnet-result-coefficients (elastic-net X y #:alpha 0.5 #:lambda 0.5))]}

@defproc[(elnet-predict [fit elnet-result?]
                        [X design-matrix/c])
         (listof real?)]{
  The fitted value @math{β₀ + xβ} for each row of @racket[X]: @racket[predict]
  with its defaults.

  @examples[#:eval ev
  (elnet-predict fit '((7.0 6.0 49.0) (0.0 0.0 0.0)))]}

@section[#:tag "ref-binomial"]{Binomial models}

The @tech{binomial family}: two-class logistic regression on 0/1 labels. See
@secref["ex-logistic"].

@defstruct*[logistic-result ([intercept real?]
                             [coefficients (vectorof real?)]
                             [dev-ratio real?]
                             [lambda real?]
                             [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted two-class model. @racket[intercept] and @racket[coefficients] are
  on the log-odds scale for class 1; @racket[dev-ratio] is the fraction of null
  deviance explained.

  @examples[#:eval ev
  (define X '((1.0 5.0 2.0) (2.0 6.0 1.0) (2.0 5.0 3.0) (1.0 4.0 1.0)
              (6.0 2.0 2.0) (5.0 1.0 1.0) (6.0 1.0 3.0) (5.0 2.0 1.0)))
  (define y '(0 0 0 0 1 1 1 1))
  (define fit (logistic-fit X y #:lambda 0.05))
  (logistic-result-coefficients fit)
  (logistic-result-dev-ratio fit)]}

@defproc[(logistic-fit [X design-matrix/c]
                       [y (and/c (listof (or/c 0 1)) pair?)]
                       [#:lambda lambda (>=/c 0)]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? #t]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         logistic-result?]{
  Fits a two-class logistic elastic-net model of the labels @racket[y]. If a
  class probability collapses, typically under perfect separation, the
  @exnraise[exn:fail]; a larger @racket[lambda] usually fixes it.

  @examples[#:eval ev
  (logistic-fit X y #:lambda 0.05)]}

@defproc[(logistic-predict-proba [fit logistic-result?]
                                 [X design-matrix/c])
         (listof (real-in 0 1))]{
  The class-1 probability @math{1 / (1 + exp(−(β₀ + xβ)))} for each row of
  @racket[X]: @racket[predict] with @racket[#:type 'response].

  @examples[#:eval ev
  (logistic-predict-proba fit '((2.0 5.0 2.0) (5.0 2.0 2.0)))]}

@defproc[(logistic-predict [fit logistic-result?]
                           [X design-matrix/c]
                           [#:threshold threshold (real-in 0 1) 0.5])
         (listof (or/c 0 1))]{
  Hard labels: @racket[1] where @racket[logistic-predict-proba] is at least
  @racket[threshold], otherwise @racket[0]. At the default threshold this is
  @racket[predict] with @racket[#:type 'class], except for a row whose
  probability is exactly @racket[0.5], which R and @racket[predict] label
  @racket[0].

  @examples[#:eval ev
  (logistic-predict fit '((2.0 5.0 2.0) (5.0 2.0 2.0)))]}

@section[#:tag "ref-multinomial"]{Multinomial models}

The @tech{multinomial family}: @math{K}-class logistic regression on integer
labels. See @secref["ex-multinomial"].

@defstruct*[multinomial-result ([intercepts (vectorof real?)]
                                [coefficients (vectorof (vectorof real?))]
                                [dev-ratio real?]
                                [lambda real?]
                                [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted @math{K}-class model. @racket[intercepts] holds one real per class
  and @racket[coefficients] one coefficient vector per class, each with one
  entry per predictor. Adding the same constant to every intercept leaves the
  probabilities unchanged, so, as R does, the intercepts are centred to sum to
  zero. @racket[dev-ratio] is the fraction of null deviance explained.

  @examples[#:eval ev
  (define X '((1.0 1.0) (2.0 1.0) (5.0 1.0) (6.0 1.0) (3.0 5.0) (4.0 6.0)))
  (define y '(0 0 1 1 2 2))
  (define fit (multinomial-fit X y #:lambda 0.05))
  (multinomial-result-coefficients fit)]}

@defproc[(multinomial-fit [X design-matrix/c]
                          [y (and/c (listof exact-nonnegative-integer?) pair?)]
                          [#:lambda lambda (>=/c 0)]
                          [#:alpha alpha (real-in 0 1) 1.0]
                          [#:standardize? standardize? boolean? #t]
                          [#:intercept? intercept? boolean? #t]
                          [#:thresh thresh (>/c 0) 1e-7]
                          [#:max-iters max-iters exact-positive-integer? 100000])
         multinomial-result?]{
  Fits a @math{K}-class multinomial elastic-net model. The labels @racket[y]
  must cover @racket[0] to @math{K−1} with every class present; otherwise the
  @exnraise[exn:fail]. As with @racket[logistic-fit], a collapsed class
  probability raises @racket[exn:fail].

  @examples[#:eval ev
  (multinomial-result-intercepts (multinomial-fit X y #:lambda 0.05))
  (eval:error (multinomial-fit X '(0 0 2 2 2 2) #:lambda 0.05))]}

@defproc[(multinomial-predict-proba [fit multinomial-result?]
                                    [X design-matrix/c])
         (listof (listof (real-in 0 1)))]{
  The softmax class probabilities for each row of @racket[X]: one list of
  @math{K} entries, summing to 1, per row. This is @racket[predict] with
  @racket[#:type 'response].

  @examples[#:eval ev
  (multinomial-predict-proba fit '((1.5 1.0) (3.5 5.5)))]}

@defproc[(multinomial-predict [fit multinomial-result?]
                              [X design-matrix/c])
         (listof exact-nonnegative-integer?)]{
  The most probable class for each row of @racket[X]: @racket[predict] with
  @racket[#:type 'class]. On a tie the lowest class wins.

  @examples[#:eval ev
  (multinomial-predict fit '((1.5 1.0) (5.5 1.0) (3.5 5.5)))]}

@section[#:tag "ref-cox"]{Cox models}

The @tech{Cox family}: proportional-hazards survival models. There is no
intercept, and so no @racket[#:intercept?] keyword. See @secref["ex-cox"].

@defstruct*[cox-result ([coefficients (vectorof real?)]
                        [dev-ratio real?]
                        [lambda real?]
                        [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted Cox model. @racket[coefficients] are on the log-hazard-ratio scale;
  @racket[dev-ratio] is the fraction of null partial-likelihood deviance
  explained.

  @examples[#:eval ev
  (define X '((0.5 1.0) (1.0 2.0) (1.5 1.0) (2.0 2.0)
              (2.5 1.0) (3.0 2.0) (3.5 1.0) (4.0 2.0)))
  (define times '(12.0 10.0 11.0 8.0 6.0 7.0 4.0 3.0))
  (define statuses '(0 0 0 1 1 1 1 1))
  (define fit (cox-fit X times statuses #:lambda 0.1))
  (cox-result-coefficients fit)]}

@defproc[(cox-fit [X design-matrix/c]
                  [times (and/c (listof (>/c 0)) pair?)]
                  [statuses (and/c (listof (or/c 0 1)) pair?)]
                  [#:lambda lambda (>=/c 0)]
                  [#:alpha alpha (real-in 0 1) 1.0]
                  [#:standardize? standardize? boolean? #t]
                  [#:thresh thresh (>/c 0) 1e-7]
                  [#:max-iters max-iters exact-positive-integer? 100000])
         cox-result?]{
  Fits a Cox proportional-hazards elastic-net model. @racket[times] are
  positive follow-up times and @racket[statuses] the matching event indicators:
  @racket[1] for an observed event, @racket[0] for right-censoring. If no
  status is @racket[1], the @exnraise[exn:fail].

  @examples[#:eval ev
  (cox-fit X times statuses #:lambda 0.5)
  (eval:error (cox-fit X times '(0 0 0 0 0 0 0 0) #:lambda 0.1))]}

@defproc[(cox-linear-predictor [fit cox-result?]
                               [X design-matrix/c])
         (listof real?)]{
  The log relative hazard @math{xβ} for each row of @racket[X]:
  @racket[predict] with its defaults.

  @examples[#:eval ev
  (cox-linear-predictor fit '((1.0 1.0) (3.0 1.0)))]}

@defproc[(cox-relative-risk [fit cox-result?]
                            [X design-matrix/c])
         (listof (>/c 0))]{
  The relative risk @math{exp(xβ)} for each row of @racket[X]: the factor by
  which the row's hazard exceeds the baseline hazard. This is @racket[predict]
  with @racket[#:type 'response].

  @examples[#:eval ev
  (cox-relative-risk fit '((1.0 1.0) (3.0 1.0)))]}

@section[#:tag "ref-poisson"]{Poisson models}

The @tech{Poisson family}: counts with a log link. See @secref["ex-poisson"].

@defstruct*[poisson-result ([intercept real?]
                            [coefficients (vectorof real?)]
                            [dev-ratio real?]
                            [lambda real?]
                            [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted Poisson model. @racket[intercept] and @racket[coefficients] are on
  the log-mean scale; @racket[dev-ratio] is the fraction of null deviance
  explained.

  @examples[#:eval ev
  (define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0)
              (5.0 2.0) (6.0 1.0) (7.0 2.0) (8.0 1.0)))
  (define y '(1 2 2 3 4 6 8 11))
  (define fit (poisson-fit X y #:lambda 0.2))
  (poisson-result-intercept fit)
  (poisson-result-coefficients fit)]}

@defproc[(poisson-fit [X design-matrix/c]
                      [y (and/c (listof (>=/c 0)) pair?)]
                      [#:lambda lambda (>=/c 0)]
                      [#:alpha alpha (real-in 0 1) 1.0]
                      [#:standardize? standardize? boolean? #t]
                      [#:intercept? intercept? boolean? #t]
                      [#:thresh thresh (>/c 0) 1e-7]
                      [#:max-iters max-iters exact-positive-integer? 100000])
         poisson-result?]{
  Fits a Poisson elastic-net model of the non-negative response @racket[y],
  which need not be integral.

  @examples[#:eval ev
  (poisson-fit X y #:lambda 0.5)]}

@defproc[(poisson-predict-mean [fit poisson-result?]
                               [X design-matrix/c])
         (listof (>/c 0))]{
  The fitted mean @math{exp(β₀ + xβ)} for each row of @racket[X]:
  @racket[predict] with @racket[#:type 'response].

  @examples[#:eval ev
  (poisson-predict-mean fit '((2.0 1.0) (9.0 1.0)))]}

@section[#:tag "ref-mgaussian"]{Multi-response Gaussian models}

The @tech{multi-response Gaussian family}: several numeric responses fitted
jointly under a grouped penalty. See @secref["ex-mgaussian"].

@defstruct*[mgaussian-result ([intercepts (vectorof real?)]
                              [coefficients (vectorof (vectorof real?))]
                              [r-squared real?]
                              [lambda real?]
                              [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted multi-response model. @racket[intercepts] holds one real per
  response and @racket[coefficients] one coefficient vector per response, each
  with one entry per predictor. @racket[r-squared] is the fraction of variance
  explained across all responses.

  @examples[#:eval ev
  (define X '((1.0 2.0) (2.0 1.0) (3.0 2.0) (4.0 1.0) (5.0 2.0) (6.0 1.0)))
  (define Y '((3.0 9.0) (5.0 8.0) (7.0 7.0) (9.0 6.0) (11.0 5.0) (13.0 4.0)))
  (define fit (mgaussian-fit X Y #:lambda 0.1))
  (mgaussian-result-intercepts fit)
  (mgaussian-result-coefficients fit)]}

@defproc[(mgaussian-fit [X design-matrix/c]
                        [Y design-matrix/c]
                        [#:lambda lambda (>=/c 0)]
                        [#:alpha alpha (real-in 0 1) 1.0]
                        [#:standardize? standardize? boolean? #t]
                        [#:intercept? intercept? boolean? #t]
                        [#:thresh thresh (>/c 0) 1e-7]
                        [#:max-iters max-iters exact-positive-integer? 100000])
         mgaussian-result?]{
  Fits a multi-response Gaussian elastic-net model. @racket[Y] is a matrix with
  one row per observation and one column per response. With @racket[alpha]
  above zero, the grouped lasso keeps or drops each predictor for every
  response at once.

  @examples[#:eval ev
  (mgaussian-fit X Y #:lambda 0.5)]}

@defproc[(mgaussian-predict [fit mgaussian-result?]
                            [X design-matrix/c])
         (listof (listof real?))]{
  The predictions @math{a0_r + xβ_r} for each row of @racket[X]: one list per
  row, with one entry per response. This is @racket[predict] with its
  defaults.

  @examples[#:eval ev
  (mgaussian-predict fit '((7.0 1.0) (8.0 2.0)))]}

@section[#:tag "ref-path"]{Regularization paths}

A @tech{regularization path} fits a decreasing sequence of @math{λ} in one call.
Every family has a path fitter, and they all return a @racket[glmnet-path]. Each
fitter takes its family's data arguments and keywords, with @racket[#:lambda]
made optional:

@itemlist[
 @item{@racket[#:lambda] is a list of penalties, fitted largest first. When it
       is @racket[#f] (the default), glmnet chooses the sequence as R does:
       @racket[#:nlambda] values from @math{λ_max}, where every coefficient is
       zero, down to @racket[#:lambda-min-ratio] times @math{λ_max}. The
       ratio defaults to @racket[0.01] when there are fewer observations than
       predictors, and to @racket[1e-4] otherwise.}
 @item{Like R, the path stops early once the deviance ratio improves by less
       than @racket[1e-5] or passes @racket[0.999], so it can hold fewer than
       @racket[#:nlambda] values. The first value of an automatic sequence is
       the extrapolation R's @tt{glmnet} reports.}
]

@defstruct*[glmnet-path ([family (or/c 'gaussian 'binomial 'multinomial 'cox 'poisson 'mgaussian)]
                         [lambda (vectorof real?)]
                         [intercepts (or/c #f vector?)]
                         [coefficients vector?]
                         [dev-ratio (vectorof real?)]
                         [df (vectorof exact-nonnegative-integer?)]
                         [num-passes exact-nonnegative-integer?])
            #:transparent]{
  A fitted path. @racket[lambda] holds the fitted penalties, decreasing, and
  every other per-@math{λ} field has one entry for each of them:

  @itemlist[
   @item{@racket[intercepts] holds a real per @math{λ}, or a vector of one real
         per class or response for the multinomial and multi-response families.
         It is @racket[#f] for Cox, which has no intercept.}
   @item{@racket[coefficients] holds a dense vector per @math{λ} with one entry
         per predictor, or a vector of such vectors (one per class or response).}
   @item{@racket[dev-ratio] is the fraction of null deviance explained.}
   @item{@racket[df] counts the predictors with a nonzero coefficient, in any
         class or response.}
  ]

  @racket[num-passes] counts the coordinate-descent passes over the whole path.

  A path prints as R prints one: a table with, for each fitted @math{λ}, the
  number of nonzero coefficients (@tt{Df}), the percentage of null deviance
  explained (@tt{%Dev}) and @math{λ} itself (see
  @secref["ref-model-printing"]). @racket[predict] and @racket[coef] evaluate
  a path at any @math{λ}.

  @examples[#:eval ev
  (define X '((1.0 2.0 1.0) (2.0 1.0 4.0) (3.0 4.0 9.0)
              (4.0 3.0 16.0) (5.0 6.0 25.0) (6.0 5.0 36.0)))
  (define y '(1.0 4.0 3.0 6.0 5.0 8.0))
  (define path (elnet-path X y #:lambda '(1.0 0.1 0.01)))
  path
  (glmnet-path-lambda path)
  (glmnet-path-coefficients path)
  (glmnet-path-df path)]}

@defproc[(elnet-path [X design-matrix/c]
                     [y (and/c (listof real?) pair?)]
                     [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                     [#:nlambda nlambda exact-positive-integer? 100]
                     [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                     [#:alpha alpha (real-in 0 1) 1.0]
                     [#:standardize? standardize? boolean? #t]
                     [#:intercept? intercept? boolean? #t]
                     [#:thresh thresh (>/c 0) 1e-7]
                     [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The Gaussian path, as @racket[elnet-fit] fits one point of it.

  @examples[#:eval ev
  (vector-length (glmnet-path-lambda (elnet-path X y)))
  (glmnet-path-df (elnet-path X y #:alpha 0.0 #:nlambda 5))]}

@defproc[(logistic-path [X design-matrix/c]
                        [y (and/c (listof (or/c 0 1)) pair?)]
                        [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                        [#:nlambda nlambda exact-positive-integer? 100]
                        [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                        [#:alpha alpha (real-in 0 1) 1.0]
                        [#:standardize? standardize? boolean? #t]
                        [#:intercept? intercept? boolean? #t]
                        [#:thresh thresh (>/c 0) 1e-7]
                        [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The binomial path, as @racket[logistic-fit] fits one point of it.

  @examples[#:eval ev
  (glmnet-path-df (logistic-path X '(0 0 0 1 1 1) #:lambda '(0.3 0.1 0.03)))]}

@defproc[(multinomial-path [X design-matrix/c]
                           [y (and/c (listof exact-nonnegative-integer?) pair?)]
                           [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                           [#:nlambda nlambda exact-positive-integer? 100]
                           [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                           [#:alpha alpha (real-in 0 1) 1.0]
                           [#:standardize? standardize? boolean? #t]
                           [#:intercept? intercept? boolean? #t]
                           [#:thresh thresh (>/c 0) 1e-7]
                           [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The multinomial path, as @racket[multinomial-fit] fits one point of it.

  @examples[#:eval ev
  (define mpath (multinomial-path X '(0 0 1 1 2 2) #:lambda '(0.3 0.03)))
  (vector-ref (glmnet-path-coefficients mpath) 1)]}

@defproc[(cox-path [X design-matrix/c]
                   [times (and/c (listof (>/c 0)) pair?)]
                   [statuses (and/c (listof (or/c 0 1)) pair?)]
                   [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                   [#:nlambda nlambda exact-positive-integer? 100]
                   [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                   [#:alpha alpha (real-in 0 1) 1.0]
                   [#:standardize? standardize? boolean? #t]
                   [#:thresh thresh (>/c 0) 1e-7]
                   [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The Cox path, as @racket[cox-fit] fits one point of it. There is no
  intercept, so @racket[glmnet-path-intercepts] is @racket[#f].

  @examples[#:eval ev
  (define cpath (cox-path X '(12.0 10.0 8.0 6.0 4.0 3.0) '(0 1 1 1 1 1)
                          #:lambda '(0.5 0.05)))
  (glmnet-path-coefficients cpath)]}

@defproc[(poisson-path [X design-matrix/c]
                       [y (and/c (listof (>=/c 0)) pair?)]
                       [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                       [#:nlambda nlambda exact-positive-integer? 100]
                       [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? #t]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The Poisson path, as @racket[poisson-fit] fits one point of it.

  @examples[#:eval ev
  (glmnet-path-df (poisson-path X '(1 2 2 3 5 8) #:lambda '(0.5 0.05)))]}

@defproc[(mgaussian-path [X design-matrix/c]
                         [Y design-matrix/c]
                         [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                         [#:nlambda nlambda exact-positive-integer? 100]
                         [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                         [#:alpha alpha (real-in 0 1) 1.0]
                         [#:standardize? standardize? boolean? #t]
                         [#:intercept? intercept? boolean? #t]
                         [#:thresh thresh (>/c 0) 1e-7]
                         [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The multi-response Gaussian path, as @racket[mgaussian-fit] fits one point of
  it.

  @examples[#:eval ev
  (define gpath (mgaussian-path X '((3.0 9.0) (5.0 8.0) (7.0 7.0)
                                    (9.0 6.0) (11.0 5.0) (13.0 4.0))
                                #:lambda '(1.0 0.1)))
  (glmnet-path-intercepts gpath)]}

@section[#:tag "ref-model"]{Generic model interface}

Every result type implements one generic interface, @racket[gen:glmnet-model]:
the six single-@math{λ} results (@racket[elnet-result],
@racket[logistic-result], @racket[multinomial-result], @racket[cox-result],
@racket[poisson-result] and @racket[mgaussian-result]) and
@racket[glmnet-path]. @racket[predict], @racket[coef] and
@racket[deviance-ratio] follow R's @tt{predict}, @tt{coef} and
@tt{dev.ratio} for @tt{glmnet} fits. See @secref["concepts-predict"].

The interface reads every model as a @tech{regularization path}: a
single-@math{λ} fit is a path with one @math{λ}. Where @racket[#:lambda] names
a @math{λ} that is not on the path, @racket[predict] and @racket[coef] follow
R's rule (its @tt{exact = FALSE}, the default):

@itemlist[
 @item{Between two fitted values @math{λ_l > s > λ_r}, the intercepts and
       coefficients are interpolated linearly in @math{λ}:
       @math{β(s) = w β(λ_l) + (1 − w) β(λ_r)}, with
       @math{w = (s − λ_r) / (λ_l − λ_r)}.}
 @item{Above the largest fitted @math{λ}, or below the smallest, @math{s} is
       clamped to that end of the path.}
 @item{A model with one @math{λ}, such as a single fit, gives that fit for
       every @math{s}.}
]

The examples in this section use a single fit and a path of the Gaussian
family:

@examples[#:eval ev #:label #f
(define X '((1.0 2.0 1.0) (2.0 1.0 4.0) (3.0 4.0 9.0)
            (4.0 3.0 16.0) (5.0 6.0 25.0) (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))
(define fit (lasso X y #:lambda 0.05))
(define path (elnet-path X y #:lambda '(1.0 0.1 0.01)))
]

@defidform[gen:glmnet-model]{
  A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{generic
  interface} for fitted models. It has three methods:

  @itemlist[
   @item{@racket[glmnet-model->path], which must be implemented;}
   @item{@racket[glmnet-model-default-lambda], which defaults to the first
         @math{λ} of the model's path;}
   @item{@racket[deviance-ratio], which defaults to the first deviance ratio of
         the model's path.}
  ]

  The defaults suit a single fit. @racket[glmnet-path] implements all three
  itself. A new type, such as a path together with a chosen @math{λ},
  implements the interface through the @racket[#:methods] option of
  @racket[struct], and then works with @racket[predict] and @racket[coef]:

  @examples[#:eval ev
  (require racket/generic)
  (struct chosen (path index)
    #:methods gen:glmnet-model
    [(define (glmnet-model->path m) (chosen-path m))
     (define (glmnet-model-default-lambda m)
       (vector-ref (glmnet-path-lambda (chosen-path m))
                   (chosen-index m)))
     (define (deviance-ratio m)
       (vector-ref (glmnet-path-dev-ratio (chosen-path m))
                   (chosen-index m)))])
  (define best (chosen path 1))
  (coef best)
  (deviance-ratio best)]}

@defproc[(glmnet-model? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] implements @racket[gen:glmnet-model]: every
  result type and @racket[glmnet-path] does.

  @examples[#:eval ev
  (glmnet-model? fit)
  (glmnet-model? path)
  (glmnet-model? X)]}

@defproc[(glmnet-model->path [model glmnet-model?]) glmnet-path?]{
  @racket[model] as a @racket[glmnet-path]. A single fit becomes a path with
  one @math{λ}, and a path is returned as it is.

  @examples[#:eval ev
  (glmnet-model->path fit)
  (glmnet-path-coefficients (glmnet-model->path fit))]}

@defproc[(glmnet-model-default-lambda [model glmnet-model?])
         (or/c (>=/c 0) (and/c (listof (>=/c 0)) pair?))]{
  The @racket[#:lambda] that @racket[predict] and @racket[coef] use when none
  is given: a single fit's @math{λ}, or the list of a path's fitted @math{λ}
  values, which is R's default @tt{s = NULL}.

  @examples[#:eval ev
  (glmnet-model-default-lambda fit)
  (glmnet-model-default-lambda path)]}

@defproc[(deviance-ratio [model glmnet-model?]) (or/c real? (vectorof real?))]{
  The fraction of null deviance explained, which is @math{R²} for the Gaussian
  families: a real for a single fit, and for a path a vector with one entry per
  fitted @math{λ}, R's @tt{dev.ratio}. It is not interpolated.

  @examples[#:eval ev
  (deviance-ratio fit)
  (deviance-ratio path)]}

@defproc[(predict [model glmnet-model?]
                  [X design-matrix/c]
                  [#:type type (or/c 'link 'response 'class) 'link]
                  [#:lambda lambda (or/c (>=/c 0) (and/c (listof (>=/c 0)) pair?))
                                   (glmnet-model-default-lambda model)])
         list?]{
  Predictions for each row of @racket[X], which needs one column per
  coefficient, as R's @tt{predict(fit, newx, s, type)}. @racket[type] chooses
  what is predicted:

  @tabular[#:style 'boxed
           #:sep @hspace[2]
           #:row-properties '(bottom-border ())
   (list (list @bold{Family}                       @racket['link]                    @racket['response]            @racket['class])
         (list "Gaussian, multi-response"          @math{η = β₀ + xβ}               @math{η}                      "---")
         (list "Binomial"                          @elem{log-odds @math{η}}          @elem{@math{P(y = 1)}}        @elem{@racket[1] if @math{η > 0}, else @racket[0]})
         (list "Multinomial"                       @elem{@math{η_k}, one per class}  @elem{softmax of the @math{η_k}}  @elem{the class with the largest @math{η_k}})
         (list "Cox"                               @math{xβ}                         @math{exp(xβ)}                "---")
         (list "Poisson"                           @math{log μ = η}                  @math{μ = exp(η)}             "---"))]

  @racket['class] is an error for the families without classes. For one
  @math{λ}, the result has one entry per row of @racket[X]: a real, a class
  label, or, for the multinomial and multi-response families, a list with one
  entry per class or response. When @racket[lambda] is a list, the result is a
  list of those, one per element of @racket[lambda], in the same order.

  @examples[#:eval ev
  (predict fit '((7.0 6.0 49.0) (0.0 0.0 0.0)))
  (predict path '((7.0 6.0 49.0)) #:lambda 0.5)
  (predict path '((7.0 6.0 49.0)) #:lambda '(2.0 0.5 0.01))
  (define clf (logistic-fit X '(0 0 0 1 1 1) #:lambda 0.05))
  (predict clf '((2.0 3.0 4.0) (5.0 4.0 25.0)))
  (predict clf '((2.0 3.0 4.0) (5.0 4.0 25.0)) #:type 'response)
  (predict clf '((2.0 3.0 4.0) (5.0 4.0 25.0)) #:type 'class)
  (eval:error (predict fit X #:type 'class))]}

@defproc[(coef [model glmnet-model?]
               [#:lambda lambda (or/c (>=/c 0) (and/c (listof (>=/c 0)) pair?))
                                (glmnet-model-default-lambda model)])
         (or/c vector? (listof vector?))]{
  The intercept and coefficients at @racket[lambda], as R's
  @tt{coef(fit, s)}: a vector holding the intercept, then one coefficient per
  predictor. Cox models have no intercept, so their vector holds only the
  coefficients. For the multinomial and multi-response families the result is a
  vector of such vectors, one per class or response. When @racket[lambda] is a
  list, the result is a list with one entry per element of @racket[lambda].

  @examples[#:eval ev
  (coef fit)
  (coef path #:lambda 0.1)
  (coef path #:lambda 0.4)
  (coef path #:lambda 5.0)]}

@subsection[#:tag "ref-model-printing"]{Printing}

A single fit prints on one line with its family, its @math{λ} (to four
significant digits), its deviance ratio (to four decimal places) and the
number of nonzero coefficients out of the number of predictors, counting a
predictor once when it is nonzero for any class or response. A path prints as
R's @tt{print.glmnet} table, with one row per fitted @math{λ}. Printing does not
change @racket[equal?], which compares results field by field.

@examples[#:eval ev
(list fit clf)
(define Xm '((1.0 1.0) (2.0 1.0) (5.0 1.0) (6.0 1.0) (3.0 5.0) (4.0 6.0)))
(multinomial-fit Xm '(0 0 1 1 2 2) #:lambda 0.05)
(multinomial-path Xm '(0 0 1 1 2 2) #:nlambda 4)
(equal? fit (lasso X y #:lambda 0.05))]

@section[#:tag "ref-native"]{Native library}

These call straight into @tt{libglmnetcompat}. Loading @racketmodname[glmnet]
checks both and raises @racket[exn:fail] if either is wrong. See
@secref["concepts-precision"].

@defproc[(glmnet-default-real-bytes) exact-positive-integer?]{
  The width in bytes of the Fortran default @tt{real} in the loaded library.
  It is @racket[8] for a correctly built library, which the numeric API
  requires.

  @examples[#:eval ev
  (glmnet-default-real-bytes)]}

@defproc[(glmnet-capi-abi-version) exact-positive-integer?]{
  The version of the C entry points the library exports. It changes whenever an
  entry point is added or changed incompatibly.

  @examples[#:eval ev
  (glmnet-capi-abi-version)]}

@(close-eval ev)
