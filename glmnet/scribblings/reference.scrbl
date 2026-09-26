#lang scribble/manual
@(require "utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "reference"]{Reference}

@declare-exporting[glmnet]

Every binding below is provided by @racketmodname[glmnet], except those of
@secref["ref-plot"], which @racketmodname[glmnet/plot] provides. Each model
family has a fit procedure, a path fitter, a cross-validation procedure, a
transparent result struct and prediction helpers. The formula front end
(@secref["ref-formula"]) fits any family from a @tech{table}. Every result
type, @racket[glmnet-path], @racket[glmnet-cv] and @racket[formula-model] also
implement the generic interface of @secref["ref-model"]: @racket[predict],
@racket[coef] and @racket[deviance-ratio] work on all of them, and they print
as summaries. @secref["concepts"] explains how the pieces fit together.

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
  A design matrix with column names is a @tech{table}, from which the formula
  front end fits by name; the fits from a matrix do not use the names.

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

@defproc[(design-matrix-select-rows [dm design-matrix?]
                                    [rows (and/c (listof exact-nonnegative-integer?) pair?)])
         design-matrix?]{
  A new design matrix of the rows of @racket[dm] at the indices
  @racket[rows], counting from 0, in the order given and with the same column
  names. An index can appear more than once. The rows are copied without being
  checked again. Cross-validation uses it to split one design matrix into
  folds.

  @examples[#:eval ev
  (design-matrix->rows (design-matrix-select-rows D '(2 0)))
  (design-matrix->rows (design-matrix-select-rows D '(1 1)))
  (eval:error (design-matrix-select-rows D '(0 3)))]}

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

@subsection[#:tag "ref-tables"]{Tables}

A @tech{table} holds named columns, and is what the formula front end
(@secref["ref-formula"]) fits from. A column name is a string or a symbol, and
names are compared as strings; a column is a list or vector of reals. A table
is one of:

@itemlist[
 @item{a non-empty association list of @racket[(name . column)] pairs, whose
       columns are in the list's order;}
 @item{a non-empty hash from name to column, whose columns are in the order of
       their names, sorted with @racket[string<?];}
 @item{a @racket[design-matrix?] with column names, in the order of its
       columns.}
]

Only the columns that are selected are checked: every entry of a selected
column must be a real, finite number, and the selected columns must have the
same length, at least 1. An error names the column, and the row of an entry
that is wrong. The guide's @secref["formulas"] chapter shows tables in use.

@defproc[(table? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{table}: an association list or
  hash whose names are strings or symbols and whose columns are lists or
  vectors, or a design matrix with column names. The entries of the columns are
  not checked.

  @examples[#:eval ev
  (define patients
    (list (cons "age" '(34 51 67))
          (cons 'dose #(2.0 5.5 1.0))
          (cons "id" '("a" "b" "c"))))
  (table? patients)
  (table? (hash "x" '(1 2)))
  (table? named)
  (table? D)
  (table? '((1 2) (3 4)))]}

@defproc[(table-column-names [table table?]) (listof string?)]{
  The names of @racket[table]'s columns, as strings, in the table's order. Two
  columns whose names are the same string are an error.

  @examples[#:eval ev
  (table-column-names patients)
  (table-column-names (hash 'b '(1) "a" '(2)))
  (eval:error (table-column-names (hash "a" '(1) 'a '(2))))]}

@defproc[(table->design-matrix [table table?]
                               [names (and/c (listof (or/c string? symbol?)) pair?)
                                      (table-column-names table)])
         design-matrix?]{
  A design matrix of the columns of @racket[table] named by @racket[names], in
  that order, with those names, as strings, as its column names. Each name must
  be a column of the table, and must appear once.

  @examples[#:eval ev
  (define P (table->design-matrix patients '(dose "age")))
  (design-matrix->rows P)
  (design-matrix-column-names P)
  (eval:error (table->design-matrix patients))
  (eval:error (table->design-matrix patients '("age" "weight")))]}

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

@section[#:tag "ref-cv"]{Cross-validation}

Every family has a cross-validation procedure, the equivalent of R's
@tt{cv.glmnet} for that family, and they all return a @racket[glmnet-cv]. Each
takes its family's data arguments and every keyword of its path fitter, which
it passes on to each fit, and these:

@itemlist[
 @item{@racket[#:type-measure] is the loss, R's @tt{type.measure}. Each family
       offers R's choices for it and defaults to R's default, the first in the
       table below.}
 @item{@racket[#:nfolds] is the number of folds, at least 3 and at most the
       number of observations. The folds are drawn by @racket[random-fold-ids]
       from @racket[current-pseudo-random-generator]. It is ignored when
       @racket[#:fold-ids] is given.}
 @item{@racket[#:fold-ids], R's @tt{foldid}, assigns the observations to folds:
       one fold id per row of @racket[X], counting from 0. Every id from 0 to
       the largest must appear, and there must be at least 3.}
 @item{@racket[#:grouped?], R's @tt{grouped}, computes the error and its
       standard error from the per-fold means when true, and from the
       per-observation losses otherwise. With fewer than 3 observations per
       fold the folds are never grouped; @racket['auc] and @racket['C] are
       always computed per fold; and the Cox deviance is grouped when a fold
       has fewer than 10 observations. These adjustments, which R also makes,
       log a warning.}
 @item{@racket[#:lambda] is as for the path fitter, but needs at least two
       values. Every fold's path is fitted at those values. Without it, each
       fold's path chooses its own sequence, as R's does, and is evaluated at
       the full-data path's values by interpolation.}
]

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Family}          @bold{Procedure}             @bold{@racket[#:type-measure]})
       (list "Gaussian"             @racket[elnet-cv]            @elem{@racket['mse], @racket['deviance], @racket['mae]})
       (list "Binomial"             @racket[logistic-cv]         @elem{@racket['deviance], @racket['class], @racket['auc], @racket['mse], @racket['mae]})
       (list "Multinomial"          @racket[multinomial-cv]      @elem{@racket['deviance], @racket['class], @racket['mse], @racket['mae]})
       (list "Cox"                  @racket[cox-cv]              @elem{@racket['deviance], @racket['C]})
       (list "Poisson"              @racket[poisson-cv]          @elem{@racket['deviance], @racket['mse], @racket['mae]})
       (list "Multi-response"       @racket[mgaussian-cv]        @elem{@racket['mse], @racket['deviance], @racket['mae]}))]

@secref["concepts-cv"] describes the procedure and each measure. The examples
in this section use 60 observations of 8 predictors, of which the first three
carry the signal:

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
]

@defstruct*[glmnet-cv ([lambda (vectorof real?)]
                       [cvm (vectorof real?)]
                       [cvsd (vectorof real?)]
                       [cvup (vectorof real?)]
                       [cvlo (vectorof real?)]
                       [nzero (vectorof exact-nonnegative-integer?)]
                       [measure (or/c 'mse 'deviance 'mae 'class 'auc 'C)]
                       [name string?]
                       [path glmnet-path?]
                       [lambda-min real?]
                       [lambda-1se real?]
                       [index-min exact-nonnegative-integer?]
                       [index-1se exact-nonnegative-integer?]
                       [fold-ids (listof exact-nonnegative-integer?)])
            #:transparent]{
  A cross-validated path, R's @tt{cv.glmnet} object. @racket[lambda] holds the
  λ values of the full-data path, and @racket[cvm], @racket[cvsd],
  @racket[cvup], @racket[cvlo] and @racket[nzero] one entry for each:

  @itemlist[
   @item{@racket[cvm] is the cross-validated error, @racket[cvsd] its standard
         error, and @racket[cvup] and @racket[cvlo] are @racket[cvm] plus and
         minus @racket[cvsd].}
   @item{@racket[nzero] counts the predictors in the model, as R counts them:
         the path's @racket[glmnet-path-df], except for the multinomial,
         where it is the median over the classes rounded up, and the
         multi-response family, where R also counts the first response's
         intercept.}
  ]

  A λ at which the standard error is undefined is left out, as R leaves it
  out; @racket[path] still holds every fitted λ.

  @racket[measure] is the loss and @racket[name] R's name for it.
  @racket[lambda-min] is the largest λ at which @racket[cvm] is smallest, or
  largest for @racket['auc] and @racket['C]. @racket[lambda-1se] is the
  largest λ whose @racket[cvm] is within @racket[cvsd] of that best value,
  with @racket[cvsd] taken at @racket[lambda-min]. @racket[index-min] and
  @racket[index-1se] are their positions in @racket[lambda], counting from 0.
  @racket[path] is the path fitted to all the data, and @racket[fold-ids] the
  fold of each observation.

  A @racket[glmnet-cv] implements @racket[gen:glmnet-model]. Its path is
  @racket[path]; @racket[predict] and @racket[coef] default to
  @racket[lambda-1se], as R's @tt{predict.cv.glmnet} does, and also accept
  @racket['lambda-min], @racket['lambda-1se] or any λ; and
  @racket[deviance-ratio] is the path's at @racket[lambda-1se]. It prints as
  R's @tt{print.cv.glmnet} does: the measure, then for each of
  @racket[lambda-min] and @racket[lambda-1se] its value, index, @racket[cvm],
  @racket[cvsd] and @racket[nzero].

  @examples[#:eval ev
  (define cv (elnet-cv X60 y60))
  cv
  (glmnet-cv-lambda-min cv)
  (vector-ref (glmnet-cv-cvm cv) (glmnet-cv-index-min cv))
  (coef cv)
  (coef cv #:lambda 'lambda-min)
  (predict cv '((0.5 -0.5 0.0 0.0 0.0 0.0 0.0 0.0)))
  (deviance-ratio cv)]}

@defproc[(elnet-cv [X design-matrix/c]
                   [y (and/c (listof real?) pair?)]
                   [#:type-measure type-measure (or/c 'mse 'deviance 'mae) 'mse]
                   [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                   [#:fold-ids fold-ids (or/c #f (and/c (listof exact-nonnegative-integer?) pair?)) #f]
                   [#:grouped? grouped? boolean? #t]
                   [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                   [#:nlambda nlambda exact-positive-integer? 100]
                   [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                   [#:alpha alpha (real-in 0 1) 1.0]
                   [#:standardize? standardize? boolean? #t]
                   [#:intercept? intercept? boolean? #t]
                   [#:thresh thresh (>/c 0) 1e-7]
                   [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-cv?]{
  Cross-validates @racket[elnet-path]. @racket['mse] and @racket['deviance]
  are both the mean squared error, as in R; @racket['mae] is the mean absolute
  error.

  @examples[#:eval ev
  (elnet-cv X60 y60 #:type-measure 'mae #:alpha 0.5)
  (define halves (for/list ([i (in-range 60)]) (modulo i 2)))
  (eval:error (elnet-cv X60 y60 #:fold-ids halves))]}

@defproc[(logistic-cv [X design-matrix/c]
                      [y (and/c (listof (or/c 0 1)) pair?)]
                      [#:type-measure type-measure (or/c 'deviance 'class 'auc 'mse 'mae) 'deviance]
                      [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                      [#:fold-ids fold-ids (or/c #f (and/c (listof exact-nonnegative-integer?) pair?)) #f]
                      [#:grouped? grouped? boolean? #t]
                      [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                      [#:nlambda nlambda exact-positive-integer? 100]
                      [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                      [#:alpha alpha (real-in 0 1) 1.0]
                      [#:standardize? standardize? boolean? #t]
                      [#:intercept? intercept? boolean? #t]
                      [#:thresh thresh (>/c 0) 1e-7]
                      [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-cv?]{
  Cross-validates @racket[logistic-path]. @racket['deviance] is the binomial
  deviance, @racket['class] the misclassification rate, @racket['auc] the area
  under the ROC curve of each fold, and @racket['mse] and @racket['mae] the
  squared and absolute differences between the 0/1 labels of both classes and
  their predicted probabilities, summed over the two classes. With fewer than
  10 observations per fold, @racket['auc] becomes @racket['deviance], as in R.

  @examples[#:eval ev
  (define labels (for/list ([v (in-list y60)]) (if (> v 1.0) 1 0)))
  (logistic-cv X60 labels #:type-measure 'class)
  (logistic-cv X60 labels #:type-measure 'auc #:nfolds 5)]}

@defproc[(multinomial-cv [X design-matrix/c]
                         [y (and/c (listof exact-nonnegative-integer?) pair?)]
                         [#:type-measure type-measure (or/c 'deviance 'class 'mse 'mae) 'deviance]
                         [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                         [#:fold-ids fold-ids (or/c #f (and/c (listof exact-nonnegative-integer?) pair?)) #f]
                         [#:grouped? grouped? boolean? #t]
                         [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                         [#:nlambda nlambda exact-positive-integer? 100]
                         [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                         [#:alpha alpha (real-in 0 1) 1.0]
                         [#:standardize? standardize? boolean? #t]
                         [#:intercept? intercept? boolean? #t]
                         [#:thresh thresh (>/c 0) 1e-7]
                         [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-cv?]{
  Cross-validates @racket[multinomial-path]. The measures are those of
  @racket[logistic-cv], summed over the @math{K} classes, without
  @racket['auc]. Every fold's training data must contain every class.

  @examples[#:eval ev
  (define classes
    (for/list ([v (in-list y60)])
      (cond [(< v -0.5) 0] [(< v 2.5) 1] [else 2])))
  (multinomial-cv X60 classes #:type-measure 'class)]}

@defproc[(cox-cv [X design-matrix/c]
                 [times (and/c (listof (>/c 0)) pair?)]
                 [statuses (and/c (listof (or/c 0 1)) pair?)]
                 [#:type-measure type-measure (or/c 'deviance 'C) 'deviance]
                 [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                 [#:fold-ids fold-ids (or/c #f (and/c (listof exact-nonnegative-integer?) pair?)) #f]
                 [#:grouped? grouped? boolean? #t]
                 [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                 [#:nlambda nlambda exact-positive-integer? 100]
                 [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                 [#:alpha alpha (real-in 0 1) 1.0]
                 [#:standardize? standardize? boolean? #t]
                 [#:thresh thresh (>/c 0) 1e-7]
                 [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-cv?]{
  Cross-validates @racket[cox-path]. @racket['deviance] is the partial
  likelihood deviance per observation: when grouped, each fold contributes the
  deviance of all the data less that of its training data, at the fold's
  coefficients, as in R; otherwise the deviance of the fold itself.
  @racket['C] is Harrell's concordance index of each fold. Every fold's
  training data must contain an event.

  @examples[#:eval ev
  (define times (for/list ([v (in-list y60)]) (exp (* -0.3 v))))
  (define statuses
    (for/list ([i (in-range 60)])
      (if (zero? (modulo i 5)) 0 1)))
  (cox-cv X60 times statuses)
  (cox-cv X60 times statuses #:type-measure 'C)]}

@defproc[(poisson-cv [X design-matrix/c]
                     [y (and/c (listof (>=/c 0)) pair?)]
                     [#:type-measure type-measure (or/c 'deviance 'mse 'mae) 'deviance]
                     [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                     [#:fold-ids fold-ids (or/c #f (and/c (listof exact-nonnegative-integer?) pair?)) #f]
                     [#:grouped? grouped? boolean? #t]
                     [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                     [#:nlambda nlambda exact-positive-integer? 100]
                     [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                     [#:alpha alpha (real-in 0 1) 1.0]
                     [#:standardize? standardize? boolean? #t]
                     [#:intercept? intercept? boolean? #t]
                     [#:thresh thresh (>/c 0) 1e-7]
                     [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-cv?]{
  Cross-validates @racket[poisson-path]. @racket['deviance] is the Poisson
  deviance; @racket['mse] and @racket['mae] compare the counts with the
  predicted means.

  @examples[#:eval ev
  (define counts
    (for/list ([v (in-list y60)])
      (inexact->exact (round (exp (* 0.3 v))))))
  (poisson-cv X60 counts)]}

@defproc[(mgaussian-cv [X design-matrix/c]
                       [Y design-matrix/c]
                       [#:type-measure type-measure (or/c 'mse 'deviance 'mae) 'mse]
                       [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                       [#:fold-ids fold-ids (or/c #f (and/c (listof exact-nonnegative-integer?) pair?)) #f]
                       [#:grouped? grouped? boolean? #t]
                       [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                       [#:nlambda nlambda exact-positive-integer? 100]
                       [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? #t]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-cv?]{
  Cross-validates @racket[mgaussian-path]. The measures are those of
  @racket[elnet-cv], summed over the responses.

  @examples[#:eval ev
  (define Y60
    (for/list ([row (in-list X60)] [v (in-list y60)])
      (list v (- (list-ref row 1) (list-ref row 3)))))
  (mgaussian-cv X60 Y60)]}

@defproc[(random-fold-ids [n exact-positive-integer?]
                          [nfolds exact-positive-integer? 10])
         (listof exact-nonnegative-integer?)]{
  Assigns @racket[n] observations to @racket[nfolds] folds at random, as R's
  @tt{sample(rep(seq(nfolds), length = n))} does: the fold ids
  @racket[0], @racket[1], ..., @racket[(- nfolds 1)], @racket[0], ... are
  shuffled with @racket[current-pseudo-random-generator], so that the folds
  differ in size by at most one. @racket[nfolds] must not exceed
  @racket[n]. The cross-validation procedures call it when they are not
  given @racket[#:fold-ids]; calling it directly gives folds to reuse across
  calls.

  @examples[#:eval ev
  (random-fold-ids 10 3)
  (define (draw)
    (parameterize ([current-pseudo-random-generator
                    (make-pseudo-random-generator)])
      (random-seed 1)
      (random-fold-ids 10 3)))
  (equal? (draw) (draw))
  (eval:error (random-fold-ids 3 5))]}

@section[#:tag "ref-formula"]{Formulas}

The formula front end fits any family from a @tech{table} (see
@secref["ref-tables"]). A @tech{formula} names the response and selects the
predictors among the table's columns; @racket[formula-fit],
@racket[formula-path] and @racket[formula-cv] then call the fit procedure,
path fitter or cross-validation procedure of the family that
@racket[#:family] names, on those columns, and return a
@racket[formula-model]. The model keeps the formula and the names of the
predictors, so that @racket[coef] keys the coefficients by name and
@racket[predict] reads a table by name (see @secref["ref-model"]).
@secref["formulas"] works through examples.

A formula selects its columns from a table as follows:

@itemlist[
 @item{The response columns must be columns of the table, and distinct.}
 @item{A column name adds that column. It must be a column of the table, and
       must not be a response column.}
 @item{@racket[all] adds every column of the table that is not a response
       column, in the table's order.}
 @item{@racket[(+ term ...)] adds the columns of each term in turn, and several
       terms after the response are joined in the same way.}
 @item{@racket[(- term excluded ...)] adds the columns of @racket[term] that
       are not columns of the @racket[excluded] terms. An excluded term may name
       a response column.}
 @item{A column added twice counts once, where it was first added. At least
       one column must be selected.}
]

Names are compared as strings. A column whose name is @racket[all],
@racket[surv], @racket[+] or @racket[-] is written as a string, such as
@racket["all"].

The examples in this section use the 60 observations of
@secref["ref-cv"] as a table, with the response @racket["y"] and the
predictors @racket["x1"] to @racket["x8"]:

@examples[#:eval ev #:label #f
(define T60
  (cons (cons "y" y60)
        (for/list ([j (in-range 8)])
          (cons (format "x~a" (add1 j))
                (for/list ([row (in-list X60)]) (list-ref row j))))))
(table-column-names T60)
]

@defform[#:literals (all surv + -)
         (~ response term ...+)
         #:grammar
         [(response column
                    (surv time-column status-column)
                    (column ...+))
          (term column
                all
                (+ term ...+)
                (- term excluded-term ...+))
          (column identifier
                  string)]]{
  A @tech{formula}: the @racket[response], then the predictor terms. The body is
  quoted, as by @racket[quote], and its shape is checked when the form is
  expanded; the result is @racket[(make-formula 'response 'term ...)]. A
  column is written as an identifier or a string.

  The response is one column, @racket[(surv time-column status-column)] for
  the Cox family, or a list of columns for the multi-response Gaussian family.

  @examples[#:eval ev
  (~ y all)
  (~ y (- all x7 x8))
  (~ (surv time status) age "blood pressure")
  (formula? (~ y x1 x2))
  (eval:error (~ y))
  (eval:error (~ y (* x1 x2)))]}

@defthing[formula-term/c flat-contract?]{
  Accepts a predictor term as data, as @racket[~] quotes it: a column name
  (a string, or a symbol other than @racket['all], @racket['surv], @racket['+]
  and @racket['-]), @racket['all], a list of @racket['+] and one or more terms,
  or a list of @racket['-] and two or more terms.

  @examples[#:eval ev
  (formula-term/c '(- all x7))
  (formula-term/c '(* x1 x2))]}

@defthing[formula-response/c flat-contract?]{
  Accepts a response as data: a column name, a list of @racket['surv] and two
  column names, or a non-empty list of column names.

  @examples[#:eval ev
  (formula-response/c '(surv time status))
  (formula-response/c '(surv time))]}

@defproc[(make-formula [response formula-response/c] [term formula-term/c] ...+)
         formula?]{
  The formula with @racket[response] and the @racket[term]s, which is what
  @racket[~] expands to. It builds a formula from names computed at run time.

  @examples[#:eval ev
  (define chosen '("x1" "x2" "x3"))
  (apply make-formula "y" chosen)
  (equal? (make-formula 'y '(- all x8)) (~ y (- all x8)))]}

@defproc[(formula? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a formula. Two formulas are
  @racket[equal?] when their responses and terms are, and a formula prints as
  the @racket[~] form that makes it.

  @examples[#:eval ev
  (formula? (~ y all))
  (formula? '(~ y all))]}

@deftogether[(@defproc[(formula-response [f formula?]) formula-response/c]
              @defproc[(formula-terms [f formula?]) (listof formula-term/c)])]{
  The response and the predictor terms of @racket[f], as written.

  @examples[#:eval ev
  (formula-response (~ (surv time status) age))
  (formula-terms (~ y x1 (- all x1 x2)))]}

@defproc[(formula-predictor-names [f formula?] [table table?]) (listof string?)]{
  The names of the predictor columns that @racket[f] selects from
  @racket[table], in the order of the model's coefficients.

  @examples[#:eval ev
  (formula-predictor-names (~ y all) T60)
  (formula-predictor-names (~ y (- all x2 x4) x2) T60)
  (eval:error (formula-predictor-names (~ y x1 x9) T60))
  (eval:error (formula-predictor-names (~ y x1 y) T60))]}

@defstruct*[formula-model ([formula formula?]
                           [predictor-names (listof string?)]
                           [fit glmnet-model?])
            #:transparent]{
  A model fitted from a formula. @racket[fit] is the result of the family's
  procedure: a single fit, a @racket[glmnet-path] or a @racket[glmnet-cv].
  @racket[predictor-names] names its predictors, in the order of its
  coefficients.

  A formula model implements @racket[gen:glmnet-model] through @racket[fit]:
  @racket[predict], @racket[coef] and @racket[deviance-ratio] give what they
  give for @racket[fit], except that @racket[coef] keys the coefficients by
  name and @racket[predict] reads the predictors from a table by name.
  @racket[glmnet-model-predictor-names] returns @racket[predictor-names], and
  @racket[glmnet-model-response-names] the formula's response columns. A
  formula model prints as @racket[fit] does, with the formula after the
  family.

  @examples[#:eval ev
  (define m (formula-fit (~ y all) T60 #:lambda 0.2))
  m
  (formula-model-predictor-names m)
  (formula-model-fit m)]}

@defproc[(formula-fit [f formula?]
                      [table table?]
                      [#:lambda lambda (>=/c 0)]
                      [#:family family
                                (or/c 'gaussian 'binomial 'multinomial 'poisson 'cox 'mgaussian)
                                'gaussian]
                      [#:alpha alpha (real-in 0 1) 1.0]
                      [#:standardize? standardize? boolean? #t]
                      [#:intercept? intercept? boolean? #t]
                      [#:thresh thresh (>/c 0) 1e-7]
                      [#:max-iters max-iters exact-positive-integer? 100000])
         formula-model?]{
  Fits @racket[f] to @racket[table] at a single @math{λ}, with the fit
  procedure of @racket[family]: @racket[elnet-fit], @racket[logistic-fit],
  @racket[multinomial-fit], @racket[poisson-fit], @racket[cox-fit] or
  @racket[mgaussian-fit]. The keywords are passed on to it; the Cox family has
  no intercept, so @racket[intercept?] does not apply to it.

  The response must suit the family: @racket[(surv time status)] for the Cox
  family, one or more columns for the multi-response family, and one column
  otherwise. Its values must be ones the family models: 0 or 1 for the
  binomial family, a class label @math{0, 1, …} for the multinomial family,
  non-negative for the Poisson family, and for the Cox family positive times
  and 0/1 statuses. An error names the column and row of a value that is not.

  @examples[#:eval ev
  (define fit (formula-fit (~ y all) T60 #:lambda 0.2 #:alpha 0.5))
  (coef fit)
  (define new-row
    (for/list ([j (in-range 1 9)])
      (cons (format "x~a" j) '(0.5))))
  (predict fit new-row)
  (define T60b
    (cons (cons "positive" (for/list ([v (in-list y60)]) (if (> v 1) 1 0)))
          T60))
  (formula-fit (~ positive (- all y)) T60b
               #:family 'binomial #:lambda 0.05)
  (eval:error (formula-fit (~ y all) T60 #:family 'binomial #:lambda 0.05))
  (eval:error (formula-fit (~ y all) T60 #:family 'cox #:lambda 0.05))]}

@defproc[(formula-path [f formula?]
                       [table table?]
                       [#:family family
                                 (or/c 'gaussian 'binomial 'multinomial 'poisson 'cox 'mgaussian)
                                 'gaussian]
                       [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                       [#:nlambda nlambda exact-positive-integer? 100]
                       [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? #t]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         formula-model?]{
  Fits the @tech{regularization path} of @racket[f] on @racket[table], with the
  path fitter of @racket[family], such as @racket[elnet-path], to which the
  keywords are passed. The response is as for @racket[formula-fit].

  @examples[#:eval ev
  (define path
    (formula-path (~ y x1 x2 x3) T60 #:lambda '(1.0 0.1 0.01)))
  path
  (coef path #:lambda 0.1)]}

@defproc[(formula-cv [f formula?]
                     [table table?]
                     [#:family family
                               (or/c 'gaussian 'binomial 'multinomial 'poisson 'cox 'mgaussian)
                               'gaussian]
                     [#:type-measure type-measure (or/c #f 'mse 'deviance 'mae 'class 'auc 'C) #f]
                     [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                     [#:fold-ids fold-ids (or/c #f (and/c (listof exact-nonnegative-integer?) pair?)) #f]
                     [#:grouped? grouped? boolean? #t]
                     [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                     [#:nlambda nlambda exact-positive-integer? 100]
                     [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1))) #f]
                     [#:alpha alpha (real-in 0 1) 1.0]
                     [#:standardize? standardize? boolean? #t]
                     [#:intercept? intercept? boolean? #t]
                     [#:thresh thresh (>/c 0) 1e-7]
                     [#:max-iters max-iters exact-positive-integer? 100000])
         formula-model?]{
  Cross-validates the path of @racket[f] on @racket[table] with the
  cross-validation procedure of @racket[family], such as @racket[elnet-cv], to
  which the keywords are passed. @racket[type-measure] must be one of the
  family's measures (see @secref["ref-cv"]); @racket[#f], the default, is the
  family's default. The response is as for @racket[formula-fit].

  @examples[#:eval ev
  (define cv-model (formula-cv (~ y all) T60 #:nfolds 5))
  cv-model
  (coef cv-model)
  (eval:error (formula-cv (~ y all) T60 #:type-measure 'auc))]}

@section[#:tag "ref-model"]{Generic model interface}

Every result type implements one generic interface, @racket[gen:glmnet-model]:
the six single-@math{λ} results (@racket[elnet-result],
@racket[logistic-result], @racket[multinomial-result], @racket[cox-result],
@racket[poisson-result] and @racket[mgaussian-result]), @racket[glmnet-path],
@racket[glmnet-cv] and @racket[formula-model]. @racket[predict], @racket[coef] and
@racket[deviance-ratio] follow R's @tt{predict}, @tt{coef} and
@tt{dev.ratio} for @tt{glmnet} and @tt{cv.glmnet} fits. See
@secref["concepts-predict"].

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

A cross-validated model also names two λ values, @racket['lambda-min] and
@racket['lambda-1se], which @racket[#:lambda] accepts in place of a number,
as R's @tt{s} accepts @tt{"lambda.min"} and @tt{"lambda.1se"}.

A model can also name its predictors, as a @racket[formula-model] does. Then
@racket[coef] keys its coefficients by name, as R's @tt{coef} names its rows,
and @racket[predict] takes a @tech{table} and reads the predictors from it by
name.

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
  interface} for fitted models. It has six methods:

  @itemlist[
   @item{@racket[glmnet-model->path], which must be implemented;}
   @item{@racket[glmnet-model-default-lambda], which defaults to the first
         @math{λ} of the model's path;}
   @item{@racket[glmnet-model-named-lambda], which defaults to naming no
         @math{λ};}
   @item{@racket[glmnet-model-predictor-names] and
         @racket[glmnet-model-response-names], which default to naming no
         predictors and no responses;}
   @item{@racket[deviance-ratio], which defaults to the first deviance ratio of
         the model's path.}
  ]

  The defaults suit a single fit. @racket[glmnet-path] and
  @racket[glmnet-cv] implement the ones that differ for them, and
  @racket[formula-model] implements all six. A new type, such as a path
  together with a chosen @math{λ},
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
  result type, @racket[glmnet-path], @racket[glmnet-cv] and
  @racket[formula-model] do.

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
  values, which is R's default @tt{s = NULL}. For a @racket[glmnet-cv] it is
  @racket[glmnet-cv-lambda-1se], R's default for @tt{cv.glmnet}.

  @examples[#:eval ev
  (glmnet-model-default-lambda fit)
  (glmnet-model-default-lambda path)]}

@defproc[(glmnet-model-named-lambda [model glmnet-model?]
                                    [name (or/c 'lambda-min 'lambda-1se)])
         (or/c #f (>=/c 0))]{
  The @math{λ} that @racket[name] stands for in @racket[model], or
  @racket[#f] if the model does not name one. A @racket[glmnet-cv] names its
  @racket[glmnet-cv-lambda-min] and @racket[glmnet-cv-lambda-1se]; single fits
  and paths name none, so @racket[predict] and @racket[coef] reject a name for
  them.

  @examples[#:eval ev
  (glmnet-model-named-lambda path 'lambda-min)
  (eval:error (coef path #:lambda 'lambda-min))]}

@defproc[(glmnet-model-predictor-names [model glmnet-model?])
         (or/c #f (listof string?))]{
  The names of @racket[model]'s predictors, in the order of its coefficients,
  or @racket[#f] if it does not name them. A @racket[formula-model] names them;
  the results of the family procedures do not. When a model names its
  predictors, @racket[coef] keys its coefficients by these names and
  @racket[predict] reads the columns with these names from a table.

  @examples[#:eval ev
  (define named-model
    (formula-fit (~ y all)
                 (list (cons "y" y) (cons "a" (map car X))
                       (cons "b" (map cadr X)) (cons "c" (map caddr X)))
                 #:lambda 0.05))
  (glmnet-model-predictor-names named-model)
  (glmnet-model-predictor-names fit)]}

@defproc[(glmnet-model-response-names [model glmnet-model?])
         (or/c #f (listof string?))]{
  The names of @racket[model]'s response columns, or @racket[#f] if it does not
  name them. For a @racket[formula-model], they are the columns of its
  formula's response: for the Cox family the time and status columns. For the
  multi-response family, @racket[coef] keys the coefficients of each response
  by its name.

  @examples[#:eval ev
  (glmnet-model-response-names named-model)
  (glmnet-model-response-names path)]}

@defproc[(deviance-ratio [model glmnet-model?]) (or/c real? (vectorof real?))]{
  The fraction of null deviance explained, which is @math{R²} for the Gaussian
  families: a real for a single fit, and for a path a vector with one entry per
  fitted @math{λ}, R's @tt{dev.ratio}. It is not interpolated.

  @examples[#:eval ev
  (deviance-ratio fit)
  (deviance-ratio path)]}

@defproc[(predict [model glmnet-model?]
                  [X (or/c design-matrix/c table?)]
                  [#:type type (or/c 'link 'response 'class) 'link]
                  [#:lambda lambda (or/c (>=/c 0) (and/c (listof (>=/c 0)) pair?)
                                         'lambda-min 'lambda-1se)
                                   (glmnet-model-default-lambda model)])
         list?]{
  Predictions for each row of @racket[X], as R's
  @tt{predict(fit, newx, s, type)}. @racket[X] needs one column per
  coefficient, in the order of the coefficients. For a model that names its
  predictors (see @racket[glmnet-model-predictor-names]), @racket[X] is instead
  a @tech{table} with a column of each of those names, in any order; its other
  columns are ignored, and a missing one is an error that names it. A model
  that does not name its predictors reads @racket[X] by position, and so does
  not take an association list or a hash. @racket[type] chooses what is
  predicted:

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
  @racket['lambda-min] and @racket['lambda-1se] stand for the λ values a
  @racket[glmnet-cv] chose (see @racket[glmnet-model-named-lambda]).

  @examples[#:eval ev
  (predict fit '((7.0 6.0 49.0) (0.0 0.0 0.0)))
  (predict path '((7.0 6.0 49.0)) #:lambda 0.5)
  (predict path '((7.0 6.0 49.0)) #:lambda '(2.0 0.5 0.01))
  (define clf (logistic-fit X '(0 0 0 1 1 1) #:lambda 0.05))
  (predict clf '((2.0 3.0 4.0) (5.0 4.0 25.0)))
  (predict clf '((2.0 3.0 4.0) (5.0 4.0 25.0)) #:type 'response)
  (predict clf '((2.0 3.0 4.0) (5.0 4.0 25.0)) #:type 'class)
  (eval:error (predict fit X #:type 'class))
  (define new-table
    (list (cons "c" '(49.0)) (cons "b" '(6.0)) (cons "a" '(7.0))))
  (predict named-model new-table)
  (eval:error (predict named-model (cdr new-table)))]}

@defproc[(coef [model glmnet-model?]
               [#:lambda lambda (or/c (>=/c 0) (and/c (listof (>=/c 0)) pair?)
                                      'lambda-min 'lambda-1se)
                                (glmnet-model-default-lambda model)])
         (or/c vector? list?)]{
  The intercept and coefficients at @racket[lambda], as R's
  @tt{coef(fit, s)}: a vector holding the intercept, then one coefficient per
  predictor. Cox models have no intercept, so their vector holds only the
  coefficients. For the multinomial and multi-response families the result is a
  vector of such vectors, one per class or response. When @racket[lambda] is a
  list, the result is a list with one entry per element of @racket[lambda]. As
  for @racket[predict], @racket[lambda] can name a λ that a @racket[glmnet-cv]
  chose.

  For a model that names its predictors, each vector is an association list
  instead, keyed as R's @tt{coef} names its rows: @racket["(Intercept)"] (not
  for Cox), then the predictor names. For the multinomial family, the
  association lists are in turn keyed by class label, and for the
  multi-response family by response name (see
  @racket[glmnet-model-response-names]; @racket["y1"], @racket["y2"], … when
  the model names no responses), as R names the elements of its lists.

  @examples[#:eval ev
  (coef fit)
  (coef path #:lambda 0.1)
  (coef path #:lambda 0.4)
  (coef path #:lambda 5.0)
  (coef named-model)]}

@subsection[#:tag "ref-model-printing"]{Printing}

A single fit prints on one line with its family, its @math{λ} (to four
significant digits), its deviance ratio (to four decimal places) and the
number of nonzero coefficients out of the number of predictors, counting a
predictor once when it is nonzero for any class or response. A path prints as
R's @tt{print.glmnet} table, with one row per fitted @math{λ}. A
@racket[glmnet-cv] prints as R's @tt{print.cv.glmnet}: the name of its measure,
then a row for each of @racket[glmnet-cv-lambda-min] and
@racket[glmnet-cv-lambda-1se] with that @math{λ}, its index, the
cross-validated error, its standard error and the number of nonzero
coefficients, the reals to four significant digits. A @racket[formula-model]
prints as the result it holds, with its formula after the family. Printing
does not change @racket[equal?], which compares results field by field.

@examples[#:eval ev
(list fit clf)
(define Xm '((1.0 1.0) (2.0 1.0) (5.0 1.0) (6.0 1.0) (3.0 5.0) (4.0 6.0)))
(multinomial-fit Xm '(0 0 1 1 2 2) #:lambda 0.05)
(multinomial-path Xm '(0 0 1 1 2 2) #:nlambda 4)
cv
named-model
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

@section[#:tag "ref-plot"]{Plots}

@defmodule[glmnet/plot]

@racketmodname[glmnet/plot] draws a @racket[glmnet-path] and a
@racket[glmnet-cv], or a @racket[formula-model] that holds one, as R's
@tt{plot} draws a @tt{glmnet} and a @tt{cv.glmnet} fit. The guide's
@secref["plots"] chapter shows every plot and how to read it.
@racketmodname[glmnet] does not re-export these bindings, so that
@racket[(require glmnet)] does not load the plot library. Each plot is a
@racket[pict?], drawn with @racketmodname[plot/no-gui], whose parameters apply
to it, and each plot procedure has a companion that returns the plot's
renderers, without the axes, to combine with other plot-lib renderers.

The examples in this section plot the lasso path of the fixture of
@secref["ref-gaussian"], a multinomial path of three separable classes, the
cross-validated fit @racket[cv] of @secref["ref-cv"], and the formula model
@racket[cv-model] of @racket[formula-cv]:

@examples[#:eval ev #:label #f
(require glmnet/plot)
(define X '((1.0 2.0 1.0) (2.0 1.0 4.0) (3.0 4.0 9.0)
            (4.0 3.0 16.0) (5.0 6.0 25.0) (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))
(define path (elnet-path X y))
(define X3 '((1.0 1.0) (2.0 1.0) (1.0 2.0) (2.0 2.0)
             (5.0 1.0) (6.0 1.0) (5.0 2.0) (6.0 2.0)
             (3.0 5.0) (4.0 5.0) (3.0 6.0) (4.0 6.0)))
(define mpath (multinomial-path X3 '(0 0 0 0 1 1 1 1 2 2 2 2)))
]

@defproc[(plot-coefficient-path [model (or/c glmnet-path? glmnet-cv? formula-model?)]
                                [#:xvar xvar (or/c 'lambda 'norm 'dev) 'lambda]
                                [#:sign-lambda sign-lambda (or/c -1 1) -1]
                                [#:label label
                                         (or/c boolean? design-matrix? (listof (or/c string? symbol?)))
                                         #f]
                                [#:type-coef type-coef (or/c 'coef '2norm) 'coef]
                                [#:width width exact-positive-integer? (plot-width)]
                                [#:height height exact-positive-integer? (plot-height)]
                                [#:title title (or/c #f string?) (plot-title)]
                                [#:out-file out-file (or/c #f path-string?) #f])
         pict?]{
  Plots the coefficients of @racket[model]'s path against @racket[xvar], as R's
  @tt{plot.glmnet} does (see @secref["plot-path"]). For a
  @racket[glmnet-cv], it plots the path fitted to all the data. For a
  @racket[formula-model], it plots the model's fit, which must be a path or a
  @racket[glmnet-cv].

  @itemlist[
   @item{@racket[xvar] is @racket['lambda] for @racket[sign-lambda] times
         @math{log λ}, @racket['norm] for the L1 norm of the coefficients, or
         @racket['dev] for the fraction of deviance explained.}
   @item{@racket[label] labels each curve at the end of the path: @racket[#f]
         for no labels, @racket[#t] for the predictor's name if the model names
         its predictors, as a @racket[formula-model] does, and otherwise its
         position counting from 1, or the names of the predictors, as a list or
         as the column names of a design matrix. A design matrix without column
         names gives positions.}
   @item{@racket[type-coef] applies to the multinomial and multi-response
         families: @racket['coef] stacks one plot per class or response,
         @racket['2norm] draws one plot of the 2-norms across them.}
   @item{@racket[width] and @racket[height] are the size of each plot in
         pixels, and @racket[title] is its title.}
   @item{@racket[out-file], when given, names a file to which the plot is also
         written, in the format its extension names: @filepath{png},
         @filepath{pdf}, @filepath{svg} or @filepath{eps}.}
  ]

  An error is raised if every coefficient is zero at every @math{λ}, as there
  is then nothing to plot.

  @examples[#:eval ev
  (plot-coefficient-path path #:xvar 'norm #:label '("x1" "x2" "x3")
                         #:width 400 #:height 300 #:title "Lasso path")
  (eval:error (plot-coefficient-path (elnet-path X y #:lambda '(10.0 5.0))))]}

@defproc[(coefficient-path-renderers [model (or/c glmnet-path? glmnet-cv? formula-model?)]
                                     [#:xvar xvar (or/c 'lambda 'norm 'dev) 'lambda]
                                     [#:sign-lambda sign-lambda (or/c -1 1) -1]
                                     [#:label label
                                              (or/c boolean? design-matrix? (listof (or/c string? symbol?)))
                                              #f]
                                     [#:type-coef type-coef (or/c 'coef '2norm) 'coef]
                                     [#:response response exact-nonnegative-integer? 0])
         (listof renderer2d?)]{
  The renderers of one plot of @racket[plot-coefficient-path]: a line for each
  predictor that is nonzero at some @math{λ}, and a label for each when
  @racket[label] asks for them. For the multinomial and multi-response
  families with @racket[type-coef] @racket['coef], @racket[response] chooses
  the class or response, counting from 0; for the others it must be 0. The
  list is empty when every coefficient is zero.

  The axes are not included. Labels start at the end of the path, where
  plot-lib cuts them off unless the x axis is widened.

  @examples[#:eval ev
  (length (coefficient-path-renderers path))
  (length (coefficient-path-renderers path #:label #t))
  (length (coefficient-path-renderers mpath #:response 2))]}

@defproc[(plot-cv [cv (or/c glmnet-cv? formula-model?)]
                  [#:sign-lambda sign-lambda (or/c -1 1) -1]
                  [#:width width exact-positive-integer? (plot-width)]
                  [#:height height exact-positive-integer? (plot-height)]
                  [#:title title (or/c #f string?) (plot-title)]
                  [#:out-file out-file (or/c #f path-string?) #f])
         pict?]{
  Plots the cross-validation curve of @racket[cv] against
  @racket[sign-lambda] times @math{log λ}, as R's @tt{plot.cv.glmnet} does (see
  @secref["plot-cv"]), with the number of nonzero coefficients along the top.
  A @racket[formula-model] must hold a @racket[glmnet-cv], from
  @racket[formula-cv].
  @racket[width], @racket[height], @racket[title] and @racket[out-file] are as
  for @racket[plot-coefficient-path].

  @examples[#:eval ev
  (plot-cv cv #:sign-lambda 1 #:width 400 #:height 300)]}

@defproc[(cv-renderers [cv (or/c glmnet-cv? formula-model?)]
                       [#:sign-lambda sign-lambda (or/c -1 1) -1])
         (listof renderer2d?)]{
  The renderers of @racket[plot-cv]: the error bars, the points, and a
  vertical line at each of @racket[glmnet-cv-lambda-min] and
  @racket[glmnet-cv-lambda-1se]. The axes are not included.

  @examples[#:eval ev
  (length (cv-renderers cv))
  (length (cv-renderers cv-model))]}

@(close-eval ev)
