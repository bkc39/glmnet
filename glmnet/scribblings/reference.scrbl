#lang scribble/manual
@(require "utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "reference"]{Reference}

@declare-exporting[glmnet]

Every binding below is provided by @racketmodname[glmnet], except those of
@secref["ref-data-nested"], @secref["ref-data-csv"], @secref["ref-data-math"],
@secref["ref-data-polars"], @secref["ref-datasets"] and @secref["ref-plot"], whose
sections name the modules that provide them. Each model
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
       @racket[design-matrix?] value, a non-empty list or vector of
       equal-length rows of reals, each row a list or a vector (see
       @racket[design-matrix/c], @secref["ref-data"] and
       @secref["ref-data-nested"]), or a @racketmodname[math/matrix] matrix.
       Prediction helpers take new data in the same forms, with as many
       columns as the fit has coefficients.}
 @item{A @tech{response} @racket[y], and an unnamed Cox fit's
       @racket[statuses], is a non-empty list, vector, @racket[flvector] or
       @racket[f64vector] with one entry per row of @racket[X] (see
       @racket[response/c]), a @racketmodname[math/array] array or a Polars
       series; a Cox status may also be a boolean, @racket[#t] for an event.}
 @item{Every procedure also takes named data, a @tech{table} or a Polars
       dataframe, whose response and predictors it selects by name (see
       @secref["ref-common-data"]).}
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
raises @racket[exn:fail] with a readable message. So does a fit for which
glmnet fits no @math{λ} at all, for example because it does not converge within
@racket[#:max-iters] passes; the message names the procedure called and gives
glmnet's reason. (R warns instead, and returns an empty model.)

@subsection[#:tag "ref-common-data"]{Data arguments}

@examples[#:eval ev #:hidden
(require racket/contract
         (only-in polars dataframe series)
         (only-in math/matrix matrix)
         (only-in math/array array)
         (only-in glmnet/datasets iris))]

The fit, path and cross-validation procedures take their data as it is, and
give the results of the explicit conversions of @secref["ref-data"],
@secref["ref-data-math"] and @secref["ref-data-polars"]. Unnamed data is as
above. Named data, a @tech{table} that is not a design matrix or a Polars
dataframe, is read by column name: @racket[y] names the response, and
@racket[#:predictors], then required, lists the predictor columns in the order
of the coefficients. Nothing is guessed: a column is used only when it is
named, no row is dropped, and a missing value, a value that is not a real
number or one that is not finite is an error naming its column and row. A
design matrix is unnamed data, even when its columns have names. An
association list whose columns are lists, such as @racket['(("x" 1 2) ("y" 3
4))], is also a list of rows: it is named data when @racket[y] names columns,
and rows otherwise.

The response is one column, except for two families:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Family}            @bold{@racket[y] for named data}               @bold{Unnamed data})
       (list "Gaussian, Poisson"      "a numeric column"                             "a response")
       (list "Binomial, multinomial"  "a column of class labels"                     "a response of class labels")
       (list "Cox"                    @elem{@racket['("time" "status")], two columns} @elem{the times, and the statuses after them})
       (list "Multi-response"         @elem{@racket['("y1" "y2")], a non-empty list}  "a matrix, one column per response"))]

A binomial or multinomial response, a column, a list, a vector, a Polars
series or a @racketmodname[math/array] array, may hold strings, symbols or
booleans in place of class numbers. Its classes are then its levels, as a formula's
factor response has them: strings and symbols sorted by @racket[string<?],
which is R's order in the C locale (R in another locale sorts mixed-case
labels differently), and @racket["FALSE"] before @racket["TRUE"]. A binomial response has exactly two.
The fit remembers them, and @racket[predict] with @racket[#:type 'class],
@racket[logistic-predict] and @racket[multinomial-predict] return them.

A fit from named data remembers its predictors' names: @racket[predict] and
the prediction helpers read new named data, and a design matrix with column
names, by them, in any order, ignoring other columns, the response included,
and other unnamed data by position. A fit from unnamed data reads only
unnamed data. The result is the same struct as from the explicit conversions,
and @racket[equal?] to it, though a copy made by @racket[struct-copy] does not
remember the names; @racket[coef] keeps its layout, and
@racket[plot-coefficient-path] labels the curves by name and a
multi-response fit's panels by its responses' names.

@racket[(require glmnet)] loads neither Polars nor @racketmodname[math/matrix];
their values are recognised once the program has loaded the library itself.

@examples[#:eval ev
(define autos
  (dataframe (list (series '(21.0 22.8 21.4 18.7 18.1 14.3) #:name "mpg")
                   (series '(2.62 2.32 3.215 3.44 3.46 3.57) #:name "wt")
                   (series '(110 93 110 175 105 245) #:name "hp"))))
(define car-fit (ols autos "mpg" #:predictors '("wt" "hp")))
(coef car-fit)
(predict car-fit (hash "hp" '(150) "wt" '(3.0) "model" '("new")))
(predict car-fit '((3.0 150)))
(equal? (ols (matrix [[2.62 110] [2.32 93] [3.215 110] [3.44 175] [3.46 105] [3.57 245]])
             (array #[21.0 22.8 21.4 18.7 18.1 14.3]))
        (ols '((2.62 110) (2.32 93) (3.215 110) (3.44 175) (3.46 105) (3.57 245))
             '(21.0 22.8 21.4 18.7 18.1 14.3)))
(define flowers (multinomial-fit iris "Species" #:lambda 0.05
                                 #:predictors '("Petal.Length" "Petal.Width")))
(multinomial-predict flowers (hash "Petal.Length" '(1.4 4.5 6.0) "Petal.Width" '(0.2 1.5 2.5)))
(eval:error (ols autos "mpg"))
(eval:error (ols autos "mpg" #:predictors '("wt" "mpg")))]

@defthing[data/c flat-contract?]{
  The contract on the @racket[X] argument of the fit, path and
  cross-validation procedures and the new data of @racket[predict] and the
  prediction helpers: unnamed data, a design matrix, rows in any of
  the nestings of @racket[design-matrix/c] or a @racketmodname[math/matrix]
  matrix, or named data, a @tech{table} or a Polars dataframe.

  @examples[#:eval ev
  (contract-first-order-passes? data/c '((1.0 2.0) (3.0 4.0)))
  (contract-first-order-passes? data/c autos)
  (contract-first-order-passes? data/c (hash "x" '(1 2)))
  (contract-first-order-passes? data/c '(1.0 2.0))]}

@defproc[(named-data? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is named data: a @tech{table} that is not
  a design matrix, or a Polars dataframe.

  @examples[#:eval ev
  (named-data? autos)
  (named-data? (list (cons "x" '(1 2))))
  (named-data? (rows->design-matrix '((1 2)) #:column-names '(a b)))]}

@defproc[(response-for/c [X data/c]
                         [elem flat-contract?]
                         [#:classes? classes? boolean? #f])
         flat-contract?]{
  The contract on the @racket[y] argument, given @racket[X]. For named data,
  @racket[y] is the name of one of its columns, a string or a symbol, with a
  numeric dtype when @racket[X] is a dataframe. Otherwise, @racket[y] is
  @racket[(response/c elem)], or a @racketmodname[math/array] array, as
  @racket[array->response] takes, or a Polars series, whose entries are
  checked as they are read. With @racket[classes?], for the binomial and
  multinomial families, the column, or the list or vector, may also hold
  strings and symbols, or booleans: class labels. The Gaussian procedures use
  @racket[(response-for/c X real?)].

  @examples[#:eval ev
  (contract-first-order-passes? (response-for/c autos real?) "mpg")
  (contract-first-order-passes? (response-for/c autos real?) "weight")
  (contract-first-order-passes? (response-for/c '((1.0) (2.0)) real?) '(3 4))
  (contract-first-order-passes? (response-for/c '((1.0) (2.0)) (or/c 0 1) #:classes? #t)
                                '(yes no))]}

@defproc[(survival-for/c [X data/c]) flat-contract?]{
  The contract on a Cox fit's @racket[y], given @racket[X]. For named data, a
  list of the names of two of its columns, the time, numeric, and then the
  status, numeric or boolean; otherwise the times,
  @racket[(response-for/c X (>/c 0))].

  @examples[#:eval ev
  (define survival (list (cons "x" '(1 2 3)) (cons "t" '(5.0 3.0 6.0)) (cons "d" '(1 0 1))))
  (contract-first-order-passes? (survival-for/c survival) '("t" "d"))
  (contract-first-order-passes? (survival-for/c survival) "t")]}

@defproc[(statuses-for/c [X data/c] [y any/c]) flat-contract?]{
  The contract on an unnamed Cox fit's @racket[statuses], given @racket[X] and
  @racket[y]: @racket[(response-for/c X (or/c 0 1))], or booleans, @racket[#t]
  for an event, as R's @tt{Surv} reads a logical status. Named data takes no
  statuses argument, since @racket[y] names the status column.

  @examples[#:eval ev
  (contract-first-order-passes? (statuses-for/c '((1.0) (2.0)) '(5.0 3.0)) '(1 0))
  (contract-first-order-passes? (statuses-for/c '((1.0) (2.0)) '(5.0 3.0)) '(#t #f))
  (contract-first-order-passes? (statuses-for/c survival '("t" "d")) '(1 0 1))]}

@defproc[(responses-for/c [X data/c]) flat-contract?]{
  The contract on a multi-response fit's @racket[Y], given @racket[X]. For
  named data, a non-empty list of distinct names of its numeric columns;
  otherwise unnamed data with one column per response.

  @examples[#:eval ev
  (contract-first-order-passes? (responses-for/c autos) '("mpg" "hp"))
  (contract-first-order-passes? (responses-for/c '((1.0) (2.0))) '((3.0 4.0) (5.0 6.0)))]}

@defproc[(predictors-for/c [X data/c] [y any/c]) flat-contract?]{
  The contract on the @racket[#:predictors] argument, given @racket[X] and
  @racket[y]. For named data, a non-empty list of distinct names of its
  columns, numeric ones for a dataframe, without the response @racket[y], or
  any of its names when it is a list; it is required. For unnamed data, a
  design matrix with column names included, only @racket[#f], since every
  column of @racket[X] is a predictor.

  @examples[#:eval ev
  (define car-predictors/c (predictors-for/c autos "mpg"))
  (contract-first-order-passes? car-predictors/c '("wt" hp))
  (contract-first-order-passes? car-predictors/c '("wt" "mpg"))
  (contract-first-order-passes? car-predictors/c '("wt" "wt"))]}

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
  procedure, and on the response matrix @racket[Y] of @racket[mgaussian-fit],
  @racket[mgaussian-path] and @racket[mgaussian-cv]. It accepts a
  @racket[design-matrix?] value, or rows in any of four nestings: a list or a
  vector of rows, each row a list or a vector. Each of those procedures
  converts and checks the rows as @racket[nested->design-matrix] does, so
  every nesting fits exactly as the same list of lists. A violation reports the contract's
  name, @racket[(or/c design-matrix? (listof (or/c list? vector?)) (vectorof (or/c list? vector?)))];
  unlike @racket[vectorof], the contract never wraps a vector.

  @examples[#:eval ev
  (contract-first-order-passes? design-matrix/c D)
  (contract-first-order-passes? design-matrix/c '((1.0 2.0) (3.0 4.0)))
  (contract-first-order-passes? design-matrix/c (vector #(1.0 2.0) '(3.0 4.0)))
  (contract-first-order-passes? design-matrix/c '(1.0 2.0))]}

@defproc[(response/c [elem flat-contract?]) flat-contract?]{
  A contract for a one-dimensional input, such as a @tech{response}: a
  non-empty list, vector, @racket[flvector] or @racket[f64vector] each of whose
  elements is a real number that satisfies @racket[elem]. The elements are
  checked with @racket[real?] first because a number contract such as
  @racket[(or/c 0 1)] compares with @racket[=], which the complex number
  @racket[1.0+0.0i] passes. The fit procedures use it for their
  responses, for example @racket[(response/c (or/c 0 1))] for the labels of
  @racket[logistic-fit], and check @racket[elem] again on the flonums they
  convert the response to, so that a positive time too small for a flonum,
  which becomes @racket[0.0], is an error. A violation names the accepted
  shapes, or the position of the first element that fails @racket[elem],
  counting from 0. Like @racket[design-matrix/c], it never wraps a vector.
  Two @racket[response/c] contracts are @racket[contract-equivalent?] when
  their element contracts are, and one is @racket[contract-stronger?] than
  another when its element contract is.

  @examples[#:eval ev
  (require racket/flonum)
  (define labels/c (response/c (or/c 0 1)))
  (contract-first-order-passes? labels/c '(0 1 1))
  (contract-first-order-passes? labels/c (flvector 0.0 1.0))
  (contract-first-order-passes? labels/c (vector 0 2))
  (contract-first-order-passes? labels/c (vector))
  (define/contract (events statuses)
    (-> (response/c (or/c 0 1)) exact-nonnegative-integer?)
    (for/sum ([s statuses]) (if (= s 1) 1 0)))
  (events (vector 1 0 1))
  (eval:error (events (flvector 1.0 0.0 0.5)))
  (contract-equivalent? (response/c (>/c 0)) (response/c (>/c 0)))]}

@defproc[(rows->design-matrix [rows (listof list?)]
                              [#:column-names column-names
                                              (or/c #f (listof (or/c string? symbol?)))
                                              #f])
         design-matrix?]{
  Builds a design matrix from a list of rows, one per observation. Every row
  must have the same length, and every entry must be a real, finite number.
  @racket[column-names], when given, names the columns: one distinct string or
  symbol per column. Names are compared as strings, so @racket["x"] and
  @racket['x] are the same name, and the design matrix keeps each as a
  string, so that the same data named with symbols or with strings gives
  @racket[equal?] design matrices, whatever it was converted from. For rows
  that are vectors, or a vector of rows, see @racket[nested->design-matrix].

  @examples[#:eval ev
  (define named (rows->design-matrix '((1 2) (3 4)) #:column-names '(age dose)))
  (design-matrix->rows named)
  (design-matrix-column-names named)
  (eval:error (rows->design-matrix '((1.0 2.0) (3.0 4.0) (5.0))))
  (eval:error (rows->design-matrix '((1.0 2.0) (3.0 +inf.0))))
  (eval:error (rows->design-matrix '((1.0 2.0)) #:column-names '(a)))
  (eval:error (rows->design-matrix '((1.0 2.0)) #:column-names '("x" x)))]}

@defproc[(columns->design-matrix [columns (listof list?)]
                                 [#:column-names column-names
                                                 (or/c #f (listof (or/c string? symbol?)))
                                                 #f])
         design-matrix?]{
  Builds a design matrix from a list of columns, one per predictor, under the
  same rules as @racket[rows->design-matrix]. For columns in the other
  nestings, see @racket[nested->design-matrix] with @racket[#:by 'columns].

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
  layout: element @math{(i, j)} is at index @math{i + j · nrows}. The contract
  requires the length of @racket[v] to be @racket[(* nrows ncols)], and every
  entry must be finite. The design matrix holds a copy, so later changes to
  @racket[v] do not affect it.

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
         (or/c #f (listof string?))]{
  The column names @racket[dm] was built with, as strings, or @racket[#f] if it
  has none.
  A design matrix with column names is a @tech{table}, from which the formula
  front end fits by name; the fits from a matrix do not use the names.

  @examples[#:eval ev
  (design-matrix-column-names D)
  (define cols '((1 2) (3 4)))
  (design-matrix-column-names
   (columns->design-matrix cols #:column-names '("x1" x2)))]}

@defproc[(design-matrix-ref [dm design-matrix?]
                            [i exact-nonnegative-integer?]
                            [j exact-nonnegative-integer?])
         flonum?]{
  The entry in row @racket[i] and column @racket[j] of @racket[dm], counting
  from 0. The contract requires @racket[i] to be less than
  @racket[(design-matrix-nrows dm)] and @racket[j] less than
  @racket[(design-matrix-ncols dm)].

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

@defproc[(response->f64vector [y (or/c list? vector? flvector? f64vector?)])
         f64vector?]{
  Converts a non-empty list, vector, @racket[flvector] or @racket[f64vector] of
  real, finite numbers to a fresh @racket[f64vector], the form in which a
  @tech{response} reaches the Fortran. The fit procedures apply the same
  conversion to their responses, after checking that the response has one
  entry per row of @racket[X]. An @racket[flvector] is always copied, since
  the FFI cannot pass one where the Fortran expects an @racket[f64vector]; so
  is an @racket[f64vector], so that the result never shares memory with
  @racket[y]. An error names the position of the offending entry, counting
  from 0.

  @examples[#:eval ev
  (f64vector->list (response->f64vector '(1 1/2 2.5)))
  (f64vector->list (response->f64vector (vector 1 1/2 2.5)))
  (f64vector->list (response->f64vector (flvector 1.0 0.5 2.5)))
  (eval:error (response->f64vector (flvector 1.0 +nan.0 2.0)))]}

@subsection[#:tag "ref-tables"]{Tables}

A @tech{table} holds named columns, and is what the formula front end
(@secref["ref-formula"]) fits from. A column name is a string or a symbol, and
names are compared as strings; a column is a list, vector, @racket[flvector]
or @racket[f64vector] of reals, or a list or vector of strings, symbols or
booleans, which the formula front end reads as a factor. A table is one of:

@itemlist[
 @item{a non-empty association list of @racket[(name . column)] pairs, whose
       columns are in the list's order;}
 @item{a non-empty hash from name to column, whose columns are in the order of
       their names, sorted with @racket[string<?];}
 @item{a @racket[design-matrix?] with column names, in the order of its
       columns.}
]

Only the columns that are selected are checked: every entry of a column
selected as numbers must be a real, finite number, and the selected columns
must have the same length, at least 1. An error names the column, and the row
of an entry that is wrong. The guide's @secref["formulas"] chapter shows
tables in use.

@defproc[(table? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{table}: an association list or
  hash whose names are strings or symbols and whose columns are lists,
  vectors, @racket[flvector]s or @racket[f64vector]s, or a design matrix with
  column names. The entries of the columns are not checked.

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

@section[#:tag "ref-data-nested"]{Nested lists and vectors}

@defmodule[glmnet/data/nested]

Racket's own data for a @tech{design matrix} is a list or a vector of rows,
each row a list or a vector. Every fit and prediction procedure accepts these
four nestings as they are (see @racket[design-matrix/c]). This module converts
them to a @racket[design-matrix?] value, to check the data once and fit it
many times or to name its columns, and converts a design matrix back to any of
them. A @tech{response} needs no conversion of its own: a list, vector,
@racket[flvector] or @racket[f64vector] is accepted wherever a response is
(see @racket[response/c] and @racket[response->f64vector]).
@racketmodname[glmnet] does not re-export this module.

@racket[rows->design-matrix] and @racket[columns->design-matrix] are the
list-of-lists cases of @racket[nested->design-matrix]. The three read their
input the same way, so the same entries give @racket[equal?] design matrices,
and so do the fits and predictions on them.

@defproc[(nested->design-matrix [xss (or/c (listof (or/c list? vector?))
                                           (vectorof (or/c list? vector?)))]
                                [#:by by (or/c 'rows 'columns) 'rows]
                                [#:column-names column-names
                                                (or/c #f (listof (or/c string? symbol?)))
                                                #f])
         design-matrix?]{
  Builds a design matrix from @racket[xss], whose elements are the rows of the
  matrix when @racket[by] is @racket['rows] and its columns when @racket[by]
  is @racket['columns]. @racket[xss] is one list or one vector, and its rows
  (or columns) can be any mix of lists and vectors. The
  rules are those of @racket[rows->design-matrix]: at least one row and one
  column, every row (or column) of the same length, every entry a real, finite
  number, and @racket[column-names], when given, one distinct name per column.
  A design matrix with column names is a @tech{table}, from which the formula
  front end fits by name.

  @examples[#:eval ev
  (require glmnet/data/nested)
  (define vrows (vector #(1.0 4.0) #(2.0 5.0) #(3.0 6.0)))
  (define V (nested->design-matrix vrows))
  (design-matrix->rows V)
  (equal? V (nested->design-matrix (list #(1 2 3) '(4 5 6)) #:by 'columns))
  (equal? V (rows->design-matrix '((1 4) (2 5) (3 6))))
  (design-matrix-column-names (nested->design-matrix vrows #:column-names '(x z)))
  (eval:error (nested->design-matrix (vector #(1.0 2.0) #(3.0))))]}

@defproc[(design-matrix->nested [dm design-matrix?]
                                [#:by by (or/c 'rows 'columns) 'rows]
                                [#:outer outer (or/c 'list 'vector) 'list]
                                [#:inner inner (or/c 'list 'vector) 'list])
         (or/c list? vector?)]{
  The entries of @racket[dm], as flonums: its rows when @racket[by] is
  @racket['rows], and its columns when it is @racket['columns]. They are held
  in an @racket[outer] (a list or a vector) of @racket[inner]s, so that the
  defaults give the list of lists of @racket[design-matrix->rows]. The vectors
  are fresh and mutable. The column names are not included; see
  @racket[design-matrix-column-names].

  @examples[#:eval ev
  (design-matrix->nested V #:outer 'vector #:inner 'vector)
  (design-matrix->nested V #:inner 'vector)
  (design-matrix->nested V #:by 'columns #:outer 'vector)
  (equal? (design-matrix->nested V) (design-matrix->rows V))]}

@section[#:tag "ref-data-csv"]{CSV files}

@defmodule[glmnet/data/csv]

@racketmodname[glmnet/data/csv] reads and writes @tech{tables} as CSV files,
following @hyperlink["https://www.rfc-editor.org/rfc/rfc4180"]{RFC 4180}.
@racketmodname[glmnet] does not re-export it. See @secref["data-csv"].

When reading, the input must be UTF-8. The first record is the header, whose
cells name the columns: each name must be non-empty and appear once. Records
end with a line feed, a carriage return or both, and the last one need not; a
byte-order mark at the start is skipped. A cell in double quotes can hold
commas, line breaks and double quotes, which are written twice. Every record
must have one cell per column.

Each cell is typed on its own, as R's @tt{read.csv} types a column that holds
only that cell:

@itemlist[
 @item{A cell is missing when it is @tt{NA}, quoted or not, or when it is not
       quoted and is empty or holds only white space. A missing cell is an
       error.}
 @item{A number is a flonum. A number is a decimal number, such as @tt{3},
       @tt{-2.5}, @tt{.5}, @tt{5.} or @tt{1e-3}; a hexadecimal one, such as
       @tt{0x1A}, @tt{0x.8} or @tt{0x1.8p1}; or @tt{Inf}, @tt{Infinity} or
       @tt{NaN}, in any case; each with an optional sign. White space around a
       number is ignored. A number is rounded correctly, however many digits
       it has, which R's reader does not always do for a decimal number.
       @tt{-0} is @racket[-0.0], where R reads it as the integer 0.}
 @item{@tt{TRUE} and @tt{T} are @racket[#t], and @tt{FALSE} and @tt{F} are
       @racket[#f]. Other spellings, such as @tt{true} or @tt{False}, are
       strings, as they are in R.}
 @item{Any other cell is a string, as it is written between the quotes, white
       space included: @tt{" TRUE"} and @tt{"NA "} are strings, as they are in
       R, and @tt{""} is the empty string.}
]

White space is what R takes as white space in a UTF-8 locale: space, tab,
line feed, vertical tab, form feed and carriage return, and the Unicode
spaces U+1680, U+2000 to U+2006, U+2008 to U+200A, U+2028, U+2029, U+205F
and U+3000, but not the no-break spaces U+00A0, U+2007 and U+202F. It makes a
cell blank, and it can follow a number; before a number, only the ASCII white
space is skipped, as in R. In the C locale R takes only the ASCII white space,
and reads a cell of Unicode spaces as a string.

Quotes change the value of a cell only when it is empty or holds only white
space: then it is a string when it is quoted, and missing when it is not,
where R reads both as missing. A number, a logical or @tt{NA} is the same
with or without quotes, as in R. Unlike R, which gives a whole column one
type, a column can mix kinds: a column of the letters @tt{A}, @tt{C}, @tt{G}
and @tt{T}, which R reads as strings, holds three strings and @racket[#t],
and a formula that reads it raises an error naming the column. R's complex
numbers, such as @tt{1i}, are strings.

The table is an association list from each name, a string, to its column, a
vector, in the header's order. An error names the procedure called, the
column, the row, counting from 0, and the line of the input on which the row
starts, counting from 1, and the file when there is one. Input that is not
UTF-8 is an error that names the line and the offset of its first byte that
is not, counting from 0.

When writing, the header is the table's column names, in its order, and then
there is one line per row, each ended by a line feed; a table whose columns
are empty is its header alone. A real is written as the shortest decimal that
reads back as the same flonum, without a trailing @tt{.0}; an infinity as
@tt{Inf} or @tt{-Inf}, as R writes it; and a NaN as @tt{NaN}, which R reads
back as NaN, where R's @tt{write.csv} writes @tt{NA}. A boolean is @tt{TRUE}
or @tt{FALSE}, and a string, or a symbol's name, is written as it is. A cell
or a name is quoted when it holds a comma, a double quote or a line break,
starts or ends with white space or starts with a byte-order mark, or is
empty, and a string is also quoted
when it would read as another value, such as @racket["42"] or @racket["T"],
as R's @tt{write.csv} quotes every string. It still reads back as that value,
as it does in R. A string that would read back as a missing value,
@racket["NA"], cannot be written, nor can a value of another kind or a column
without a name; an error names the column and the row.

@defproc[(csv->table [in input-port? (current-input-port)]) table?]{
  Reads a CSV file from @racket[in] to its end, and returns it as a table.

  @examples[#:eval ev
  (require glmnet/data/csv)
  (csv->table (open-input-string "id,dose,treated\nA,2.5,TRUE\n\"B, 2\",5,F\n"))
  (csv->table (open-input-string "note\n\"said \"\"hi\"\"\"\n\"two\nlines\"\n"))
  (csv->table (open-input-string "x\n0x1A\n-inf\n 7 \n\" TRUE\"\ntrue\n"))
  (eval:error (csv->table (open-input-string "x,y\n1,2\n3,NA\n")))
  (eval:error (csv->table (open-input-string "x\n1\u3000\n\u3000\n")))
  (eval:error (csv->table (open-input-bytes #"name\nM\374ller\n")))]}

@defproc[(csv-file->table [path path-string?]) table?]{
  Reads the CSV file at @racket[path] as @racket[csv->table] reads a port.

  @examples[#:eval ev
  (define mtcars-file (collection-file-path "mtcars.csv" "glmnet" "datasets"))
  (map car (csv-file->table mtcars-file))]}

@defproc[(table->csv [table table?] [out output-port? (current-output-port)]) void?]{
  Writes @racket[table] to @racket[out] as a CSV file.

  @examples[#:eval ev
  (table->csv (list (cons "x" (list 1 0.1 1/3 +inf.0))
                    (cons "label" '("a" "b, c" "42" say))
                    (cons "flag" '(#t #f #t #f))))
  (define out (open-output-string))
  (table->csv (list (cons "x" (list 0.1 (+ 0.1 0.2) 1e-300))) out)
  (get-output-string out)
  (csv->table (open-input-string (get-output-string out)))
  (table->csv (csv->table (open-input-string "id,dose\n")))
  (eval:error (table->csv (list (cons "code" '("NA" "b")))))]}

@defproc[(table->csv-file [table table?]
                          [path path-string?]
                          [#:exists exists
                                    (or/c 'error 'replace 'truncate 'truncate/replace)
                                    'error])
         void?]{
  Writes @racket[table] to the file at @racket[path] as @racket[table->csv]
  writes it to a port. @racket[exists] says what to do when the file exists,
  as for @racket[open-output-file]; none of the choices keeps the file's old
  contents.

  @examples[#:eval ev
  (define path (make-temporary-file "table-~a.csv"))
  (table->csv-file (list (cons "x" '(1 2)) (cons "y" '(3.5 4.5))) path #:exists 'replace)
  (file->string path)
  (csv-file->table path)]}

@section[#:tag "ref-data-math"]{Matrices from @racketmodname[math/matrix]}

@defmodule[glmnet/data/math]

@(define math-ev (make-glmnet-eval))

Conversions between the matrices of @racketmodname[math/matrix] and
@tech[#:key "design matrix"]{design matrices}, and from
@racketmodname[math/array] arrays to @tech[#:key "response"]{responses}. The
rows of a matrix are observations and its columns predictors.
@racketmodname[glmnet] does not re-export these bindings, and
@racket[(require glmnet)] does not load @racketmodname[math/matrix]. See
@secref["data-math"].

Every array that @racketmodname[math/array] returns to untyped code carries a
contract, and so does the procedure that computes its elements, which checks
the index it is called with each time. The conversions' element loops are
written in Typed Racket, so they call that procedure without adding contracts
of their own, and they avoid it where they can. The design matrix is
written in place, with no intermediate copy:

@itemlist[
 @item{a flonum array (an @racket[FlArray], from @racket[array->flarray],
       @racket[flarray] or @racket[design-matrix->matrix]) is copied from its
       @racket[flarray-data] in one pass;}
 @item{a mutable array (a @racket[Mutable-Array], from @racket[vector->matrix]
       or @racket[array->mutable-array]) is read from its
       @racket[mutable-array-data];}
 @item{any other array, such as the result of @racket[build-matrix],
       @racket[matrix] or @racket[matrix*], is read one element at a time
       through its contracted procedure;}
 @item{a lazy array, one made while @racket[array-strictness] is @racket[#f]
       or one that @racket[array-broadcast] or @racket[array-lazy] returns,
       which are lazy at the default setting too, is
       read the same way, with a fresh mutable index vector for each element,
       because its index transforms, such as those of
       @racket[matrix-transpose] and @racket[array-slice-ref], write to the
       vector they are given.}
]

For a large matrix, the first two take about as long as
@racket[rows->design-matrix] takes for the same rows as lists, the third
several times as long, and the fourth far longer.

@defproc[(matrix->design-matrix [M (and/c array? matrix?)]
                                [#:column-names column-names
                                                (or/c #f (listof (or/c string? symbol?)))
                                                #f])
         design-matrix?]{
  A design matrix of the entries of @racket[M], one row per row of
  @racket[M]. Every entry must be a real, finite number, and exact numbers
  become flonums; an error names the column of an entry that is not (by its
  name, when the columns have names), its row, counting from 0, and the
  entry. @racket[column-names] names the columns, as for
  @racket[rows->design-matrix], and the names become strings. A design matrix with column names is a
  @tech{table}, which the formula front end fits from by name. The response
  matrix @racket[Y] of the multi-response Gaussian family converts the same
  way.

  @examples[#:eval math-ev
  (require math/array math/matrix glmnet/data/math)
  (define M (matrix [[1 2 3] [2 1 5] [3 4 11] [4 3 11] [5 6 17]]))
  (define D (matrix->design-matrix M #:column-names '(x1 x2 y)))
  (design-matrix->rows D)
  (equal? D (rows->design-matrix (matrix->list* M) #:column-names '(x1 x2 y)))
  (coef (formula-fit (~ y x1 x2) D #:lambda 0))
  (eval:error (matrix->design-matrix (matrix [[1.0 2.0] [3.0 +nan.0]])
                                     #:column-names '(x1 x2)))]}

@defproc[(design-matrix->matrix [dm design-matrix?]) (and/c array? matrix?)]{
  A matrix of the entries of @racket[dm], one row per row of @racket[dm]: a
  flonum array, an @racket[FlArray], that holds a copy of them. It does not
  carry @racket[dm]'s column names, which @racket[design-matrix-column-names]
  gives.

  @examples[#:eval math-ev
  (design-matrix->matrix D)
  (equal? (matrix->design-matrix (design-matrix->matrix D)) (matrix->design-matrix M))
  (matrix* (design-matrix->matrix (rows->design-matrix '((1 2) (3 4))))
           (col-matrix [1 -1]))]}

@defproc[(array->response
          [A (and/c array?
                    (or/c row-matrix?
                          col-matrix?
                          (property/c array-shape (vector/c exact-positive-integer?))))])
         (and/c (listof real?) pair?)]{
  The elements of @racket[A] as a list, the form in which the fit procedures
  take a @tech{response}. @racket[A] is a row matrix, a column matrix or a
  one-dimensional array with at least one element. Every element must be a
  real, finite number; an error names the position of one that is not,
  counting from 0, and the element. The elements are returned as they are, so
  class labels such as those of @racket[multinomial-fit] stay exact integers.

  @examples[#:eval math-ev
  (array->response (col-matrix [1 0 1]))
  (array->response (row-matrix [2.5 1/2]))
  (array->response (array #[0 2 1]))
  (lasso (matrix->design-matrix M) (array->response (->col-matrix '(1 3 2 5 4)))
         #:lambda 0.1)
  (eval:error (array->response (matrix [[1 2] [3 4]])))]}

@(close-eval math-ev)

@section[#:tag "ref-data-polars"]{Polars dataframes}

@defmodule[glmnet/data/polars]

Conversions between the dataframes of @racketmodname[polars] and glmnet's
@tech[#:key "design matrix"]{design matrices}, @tech{responses} and
@tech{tables}. @racketmodname[glmnet] does not re-export them and does not
load Polars. See @secref["data-polars"].

A column that is converted to numbers must have an integer or floating-point
dtype; Polars casts it to doubles as it copies it out. Column names are
strings or symbols, compared as strings, and must be names of the dataframe's
columns; the contracts check the names and the dtypes. A missing value (a
null), anywhere in a column that is converted, is an error naming its column
and row, and so is a value that is not finite where numbers are needed.
Nothing is dropped or filled in.

@examples[#:eval ev #:hidden
(require glmnet/data/polars
         glmnet/datasets
         (only-in polars dataframe series read-csv ref column-names dtype
                  polars-null))]

@defproc[(polars->design-matrix [df dataframe?]
                                [columns (and/c (listof (or/c string? symbol?)) pair?)])
         design-matrix?]{
  A design matrix of the columns of @racket[df] named by @racket[columns], in
  that order, with their names, as strings, as its column names. The contract
  requires @racket[df] to have at least one row, and @racket[columns] to name
  distinct columns of @racket[df] with numeric dtypes. The matrix is Polars'
  column-major export, @racket[dataframe->f64vector], taken over without a
  further copy; it shares no memory with @racket[df].

  @examples[#:eval ev
  (define trial
    (dataframe (list (series '(1 2 3) #:name "dose")
                     (series '(0.5 1.5 2.5) #:name "age")
                     (series '("a" "b" "a") #:name "site"))))
  (define trial-matrix (polars->design-matrix trial '(age "dose")))
  (design-matrix->rows trial-matrix)
  (design-matrix-column-names trial-matrix)
  (eval:error (polars->design-matrix trial '("dose" "site")))]}

@defproc[(design-matrix->polars [dm design-matrix?]
                                [#:column-names column-names
                                                (listof (or/c string? symbol?))
                                                (or (design-matrix-column-names dm)
                                                    (list "V1" "V2" ...))])
         dataframe?]{
  A dataframe of the columns of @racket[dm], each a @racket['float64] series,
  named by @racket[column-names]: by default, @racket[dm]'s column names, or
  @racket["V1"], @racket["V2"], …, as R names the columns of a matrix, when
  it has none. The contract requires one distinct name per column.
  @secref["data-polars"] says why this direction is the slower one.

  @examples[#:eval ev
  (design-matrix->polars trial-matrix)
  (column-names (design-matrix->polars (rows->design-matrix '((1 2)))))
  (column-names (design-matrix->polars trial-matrix #:column-names '(a b)))]}

@defproc[(polars->response [df dataframe?] [column (or/c string? symbol?)])
         (and/c (listof real?) pair?)]{
  The values of the column of @racket[df] named @racket[column] as a
  @tech{response}: exact integers from an integer column, flonums from a
  floating-point one. The contract requires @racket[df] to have at least one
  row, and @racket[column] to name a column of @racket[df] with a numeric
  dtype.

  @examples[#:eval ev
  (polars->response trial "dose")
  (eval:error (polars->response (dataframe (list (series (list 1.0 polars-null 3.0)
                                                         #:name "y")))
                                "y"))]}

@defproc[(polars->table [df dataframe?]
                        [columns (and/c (listof (or/c string? symbol?)) pair?)
                                 (column-names df)])
         table?]{
  A @tech{table} of the columns of @racket[df] named by @racket[columns], in
  that order: an association list from each name to a vector of the column's
  values. Numeric columns hold numbers, exact integers from an integer
  column, and boolean columns booleans. String columns hold strings, and
  categorical and enum columns symbols, which the formula front end reads as
  factors. A factor's levels are sorted as strings, so the order of an enum's
  categories is not kept (see @secref["data-polars"]). The contract requires
  @racket[df] to have at least one row. When @racket[columns] is given, it
  must name distinct columns of @racket[df] with those dtypes; when it is
  not, every column must have one of them.

  @examples[#:eval ev
  (polars->table trial)
  (polars->table trial '("site"))]}

@defproc[(table->polars [t table?]
                        [columns (and/c (listof (or/c string? symbol?)) pair?)
                                 (table-column-names t)])
         dataframe?]{
  A dataframe of the columns of @racket[t] named by @racket[columns], in that
  order. A column's values choose its dtype:
  @itemlist[
   @item{@racket['int64] for exact integers from @math{−2@superscript{63}} to
         @math{2@superscript{63} − 1};}
   @item{@racket['uint64] for exact integers from 0 to
         @math{2@superscript{64} − 1}, when some are
         @math{2@superscript{63}} or more;}
   @item{@racket['float64] for any other reals, and for integers mixed with
         them;}
   @item{@racket['boolean] for booleans, @racket['categorical] for symbols,
         and @racket['string] for strings, or for strings and symbols mixed.}]
  The contract requires @racket[columns] to name distinct columns of
  @racket[t]. An exact integer outside both ranges, any other exact number
  too large for a flonum, a column of integers that needs both (a negative
  one and one of @math{2@superscript{63}} or more), and a column whose values
  no one dtype holds are errors naming the column, the row and the element.

  @examples[#:eval ev
  (define cars (table->polars mtcars '("mpg" "cyl" "wt")))
  (map (lambda (name) (dtype (ref cars name))) (column-names cars))
  (table->polars (list (cons "id" '(a b a)) (cons "x" '(1 2.5 3))))
  (dtype (ref (table->polars (list (cons "n" (list 1 (expt 2 63))))) "n"))
  (eval:error (table->polars (list (cons "x" '(1 "two")))))]}

@section[#:tag "ref-datasets"]{Example datasets}

@defmodule[glmnet/datasets]

@racketmodname[glmnet/datasets] provides the example datasets of R's glmnet
4.1.10, which its vignettes use, and R's @tt{mtcars} and @tt{iris}, which the
formula examples use. @racketmodname[glmnet] does not re-export it. See
@secref["data-datasets"].

A dataset is a CSV file in the package, under @filepath{glmnet/datasets/},
which @filepath{scripts/export-datasets.R} writes from R with 17 significant
digits, so that every number reads back as R's double. R glmnet's datasets
have a procedure each, which reads its file on its first call and returns the
same values on every call. The values are the arguments of the family's
fitter, in order: the predictors as a design matrix whose columns are named
@racket["V1"], @racket["V2"], and so on, as R's @tt{coef} names the columns
of these unnamed matrices, and then the response, in the shape the family
takes (see @secref["concepts-data"]). @racket[mtcars] and @racket[iris] are
tables, read when the module is instantiated. Every module that requires
them shares them, so their columns are immutable vectors;
@racket[csv-file->table] reads a fresh copy whose columns can be changed.

@defproc[(quick-start-example) (values design-matrix? (listof flonum?))]{
  R's @tt{QuickStartExample}, the data of the vignette's Quick Start: 100
  observations of 20 predictors and a numeric response, for
  @racket[elnet-fit] and its relatives. See @secref["ex-quick-start"].

  @examples[#:eval ev
  (require glmnet/datasets)
  (define-values (x y) (quick-start-example))
  x
  (length y)
  (lasso x y #:lambda 0.1)]}

@defproc[(binomial-example) (values design-matrix? (listof (or/c 0 1)))]{
  R's @tt{BinomialExample}: 100 observations of 30 predictors and a 0/1
  response, for @racket[logistic-fit] and its relatives.

  @examples[#:eval ev
  (define-values (bx by) (binomial-example))
  (list (design-matrix-nrows bx) (design-matrix-ncols bx))
  (for/sum ([label (in-list by)]) label)]}

@defproc[(multinomial-example) (values design-matrix? (listof (or/c 0 1 2)))]{
  R's @tt{MultinomialExample}: 500 observations of 30 predictors and a class
  label, for @racket[multinomial-fit] and its relatives. R's classes are 1, 2
  and 3; they are 0, 1 and 2 here, the labels @racket[multinomial-fit] takes.

  @examples[#:eval ev
  (define-values (mx my) (multinomial-example))
  (for/list ([k (in-range 3)])
    (for/sum ([label (in-list my)]) (if (= label k) 1 0)))]}

@defproc[(poisson-example) (values design-matrix? (listof exact-nonnegative-integer?))]{
  R's @tt{PoissonExample}: 500 observations of 20 predictors and a count,
  for @racket[poisson-fit] and its relatives.

  @examples[#:eval ev
  (define-values (px py) (poisson-example))
  (apply max py)]}

@defproc[(cox-example)
         (values design-matrix? (listof (and/c flonum? positive?)) (listof (or/c 0 1)))]{
  R's @tt{CoxExample}: 1000 observations of 30 predictors, and the survival
  times and statuses, R's columns @tt{time} and @tt{status}, for
  @racket[cox-fit] and its relatives, which take them as two arguments. A
  status is 1 for a death and 0 for a censored time.

  @examples[#:eval ev
  (define-values (cx time status) (cox-example))
  (for/sum ([s (in-list status)]) s)
  (cox-path cx time status #:nlambda 5)]}

@defproc[(multi-gaussian-example) (values design-matrix? design-matrix?)]{
  R's @tt{MultiGaussianExample}: 100 observations of 20 predictors and four
  numeric responses, for @racket[mgaussian-fit] and its relatives. The
  responses are a design matrix whose columns are named @racket["y1"] to
  @racket["y4"], as R's @tt{coef} names them.

  @examples[#:eval ev
  (define-values (gx gy) (multi-gaussian-example))
  (design-matrix-column-names gy)]}

@defproc[(sparse-example) (values design-matrix? (listof flonum?))]{
  R's @tt{SparseExample}: 100 observations of 20 predictors, 84% of whose
  entries are zero, and a numeric response. R holds the predictors as a
  sparse matrix; here they are a dense design matrix, until sparse input is
  supported (@hyperlink["https://github.com/bkc39/glmnet/issues/11"]{#11}).

  @examples[#:eval ev
  (define-values (sx sy) (sparse-example))
  (for*/sum ([row (in-list (design-matrix->rows sx))] [v (in-list row)])
    (if (zero? v) 1 0))]}

@defthing[mtcars table?]{
  R's @tt{datasets::mtcars}, from the 1974 Motor Trend road tests of 32 cars,
  as an association list from each of R's eleven column names to its column,
  an immutable vector:
  @racket["mpg"], @racket["cyl"], @racket["disp"], @racket["hp"],
  @racket["drat"], @racket["wt"], @racket["qsec"], @racket["vs"],
  @racket["am"], @racket["gear"] and @racket["carb"], in R's order, with the
  cars in R's order, from the Mazda RX4 to the Volvo 142E. R's row names, the
  cars' models, are not a column.

  @examples[#:eval ev
  (table-column-names mtcars)
  (cdr (assoc "wt" mtcars))
  (eval:error (vector-set! (cdr (assoc "wt" mtcars)) 0 3.0))]}

@defthing[iris table?]{
  R's @tt{datasets::iris}, Anderson's measurements of 150 irises, 50 of each
  of three species, as an association list from each of R's five column names
  to its column, an immutable vector: @racket["Sepal.Length"], @racket["Sepal.Width"],
  @racket["Petal.Length"] and @racket["Petal.Width"] in centimetres, and
  @racket["Species"], whose values are the strings @racket["setosa"],
  @racket["versicolor"] and @racket["virginica"]. R holds the species as a
  factor with these levels, which is also the order that sorting the strings
  gives. The flowers are in R's order.

  @examples[#:eval ev
  (table-column-names iris)
  (formula-predictor-names (Sepal.Length . ~ . Petal.Width + Species) iris)]}

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

@defproc[(elnet-fit [X data/c]
                    [y (response-for/c X real?)]
                    [#:predictors predictors (predictors-for/c X y) #f]
                    [#:lambda lambda (>=/c 0)]
                    [#:alpha alpha (real-in 0 1) 1.0]
                    [#:standardize? standardize? boolean? #t]
                    [#:intercept? intercept? boolean? #t]
                    [#:thresh thresh (>/c 0) 1e-7]
                    [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  Fits a Gaussian elastic-net model of the response @racket[y], one real per
  row of @racket[X], at a single @racket[lambda]. @racket[X] and @racket[y]
  are unnamed data, or named data with the response and @racket[predictors]
  named, as in @secref["ref-common-data"]. As in R, a constant
  @racket[y] (all zero, when @racket[intercept?] is @racket[#f]) has nothing to
  standardize and raises @racket[exn:fail]. The four models below call
  @racket[elnet-fit], and its errors name the one called.

  @examples[#:eval ev
  (elnet-fit X y #:alpha 0.5 #:lambda 0.5)
  (elnet-fit autos "mpg" #:predictors '("wt" "hp") #:alpha 0.5 #:lambda 0.5)
  (eval:error (lasso X '(2.0 2.0 2.0 2.0 2.0 2.0) #:lambda 0.5))]}

@defproc[(ols [X data/c]
              [y (response-for/c X real?)]
              [#:predictors predictors (predictors-for/c X y) #f]
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
        '(1.0 4.0 3.0 6.0 5.0)))
  (elnet-result-coefficients (ols autos "mpg" #:predictors '("wt" "hp")))]}

@defproc[(ridge [X data/c]
                [y (response-for/c X real?)]
                [#:predictors predictors (predictors-for/c X y) #f]
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
  (elnet-result-coefficients (ridge X y #:lambda 0.1))
  (elnet-result-coefficients (ridge autos "mpg" #:predictors '("wt" "hp") #:lambda 1.0))]}

@defproc[(lasso [X data/c]
                [y (response-for/c X real?)]
                [#:predictors predictors (predictors-for/c X y) #f]
                [#:lambda lambda (>=/c 0)]
                [#:standardize? standardize? boolean? #t]
                [#:intercept? intercept? boolean? #t]
                [#:thresh thresh (>/c 0) 1e-7]
                [#:max-iters max-iters exact-positive-integer? 100000])
         elnet-result?]{
  The lasso: @racket[elnet-fit] with @racket[#:alpha 1.0]. Sets coefficients
  exactly to zero, more of them as @racket[lambda] grows.

  @examples[#:eval ev
  (elnet-result-coefficients (lasso X y #:lambda 0.5))
  (elnet-result-coefficients (lasso autos "mpg" #:predictors '("wt" "hp") #:lambda 1.0))]}

@defproc[(elastic-net [X data/c]
                      [y (response-for/c X real?)]
                      [#:predictors predictors (predictors-for/c X y) #f]
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
  (elnet-result-coefficients (elastic-net X y #:alpha 0.5 #:lambda 0.5))
  (elnet-result-coefficients
   (elastic-net autos "mpg" #:predictors '("wt" "hp") #:alpha 0.5 #:lambda 1.0))]}

@defproc[(elnet-predict [fit elnet-result?]
                        [X data/c])
         (listof real?)]{
  The fitted value @math{β₀ + xβ} for each row of @racket[X]: @racket[predict]
  with its defaults. A fit from named data reads named @racket[X] by its
  predictors' names.

  @examples[#:eval ev
  (elnet-predict fit '((7.0 6.0 49.0) (0.0 0.0 0.0)))
  (elnet-predict car-fit (list (cons "wt" '(3.0 2.5)) (cons "hp" '(150 100))))]}

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

@defproc[(logistic-fit [X data/c]
                       [y (response-for/c X (or/c 0 1) #:classes? #t)]
                       [#:predictors predictors (predictors-for/c X y) #f]
                       [#:lambda lambda (>=/c 0)]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? #t]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         logistic-result?]{
  Fits a two-class logistic elastic-net model of the labels @racket[y], 0 and
  1, or two class labels (see @secref["ref-common-data"]), the second of
  which is class 1. If a class probability collapses, typically under perfect
  separation, the @exnraise[exn:fail]; a larger @racket[lambda] usually fixes
  it.

  @examples[#:eval ev
  (logistic-fit X y #:lambda 0.05)
  (define cars+ (cons (cons "heavy" (for/list ([wt (in-vector (cdr (assoc "wt" mtcars)))])
                                      (if (> wt 3.3) "yes" "no")))
                      mtcars))
  (define heavy-fit (logistic-fit cars+ "heavy" #:predictors '("mpg" "hp") #:lambda 0.05))
  (logistic-predict heavy-fit (hash "mpg" '(15.0 30.0) "hp" '(200 70)))
  (eval:error (logistic-fit iris "Species" #:lambda 0.05
                            #:predictors '("Petal.Length" "Petal.Width")))]}

@defproc[(logistic-predict-proba [fit logistic-result?]
                                 [X data/c])
         (listof (real-in 0 1))]{
  The class-1 probability @math{1 / (1 + exp(−(β₀ + xβ)))} for each row of
  @racket[X]: @racket[predict] with @racket[#:type 'response].

  @examples[#:eval ev
  (logistic-predict-proba fit '((2.0 5.0 2.0) (5.0 2.0 2.0)))]}

@defproc[(logistic-predict [fit logistic-result?]
                           [X data/c]
                           [#:threshold threshold (real-in 0 1) 0.5])
         (listof (or/c 0 1 string?))]{
  Hard labels: @racket[1] where @racket[logistic-predict-proba] is at least
  @racket[threshold], otherwise @racket[0], or the classes' labels for a fit
  of a response of labels. At the default threshold this is
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

@defproc[(multinomial-fit [X data/c]
                          [y (response-for/c X (and/c integer? (>=/c 0)) #:classes? #t)]
                          [#:predictors predictors (predictors-for/c X y) #f]
                          [#:lambda lambda (>=/c 0)]
                          [#:alpha alpha (real-in 0 1) 1.0]
                          [#:standardize? standardize? boolean? #t]
                          [#:intercept? intercept? boolean? #t]
                          [#:thresh thresh (>/c 0) 1e-7]
                          [#:max-iters max-iters exact-positive-integer? 100000])
         multinomial-result?]{
  Fits a @math{K}-class multinomial elastic-net model. The labels @racket[y]
  are integers, exact or inexact as in an @racket[flvector], and must cover
  @racket[0] to @math{K−1} with every class present; otherwise the
  @exnraise[exn:fail]. They may instead be class labels, at least two (see
  @secref["ref-common-data"]). As with @racket[logistic-fit], a collapsed
  class probability raises @racket[exn:fail].

  @examples[#:eval ev
  (multinomial-result-intercepts (multinomial-fit X y #:lambda 0.05))
  (multinomial-result-intercepts (multinomial-fit X '(a a b b c c) #:lambda 0.05))
  (multinomial-result-intercepts
   (multinomial-fit iris "Species" #:predictors '("Petal.Length" "Petal.Width") #:lambda 0.05))
  (eval:error (multinomial-fit X '(0 0 2 2 2 2) #:lambda 0.05))]}

@defproc[(multinomial-predict-proba [fit multinomial-result?]
                                    [X data/c])
         (listof (listof (real-in 0 1)))]{
  The softmax class probabilities for each row of @racket[X]: one list of
  @math{K} entries, summing to 1, per row. This is @racket[predict] with
  @racket[#:type 'response].

  @examples[#:eval ev
  (multinomial-predict-proba fit '((1.5 1.0) (3.5 5.5)))]}

@defproc[(multinomial-predict [fit multinomial-result?]
                              [X data/c])
         (listof (or/c exact-nonnegative-integer? string?))]{
  The most probable class for each row of @racket[X], or its label for a fit
  of a response of labels: @racket[predict] with @racket[#:type 'class]. On a
  tie the lowest class wins.

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

@defproc[(cox-fit [X data/c]
                  [y (survival-for/c X)]
                  [statuses (statuses-for/c X y) @#,elem{none}]
                  [#:predictors predictors (predictors-for/c X y) #f]
                  [#:lambda lambda (>=/c 0)]
                  [#:alpha alpha (real-in 0 1) 1.0]
                  [#:standardize? standardize? boolean? #t]
                  [#:thresh thresh (>/c 0) 1e-7]
                  [#:max-iters max-iters exact-positive-integer? 100000])
         cox-result?]{
  Fits a Cox proportional-hazards elastic-net model of positive follow-up
  times and the matching event indicators: @racket[1] or @racket[#t] for an
  observed event, @racket[0] or @racket[#f] for right-censoring. For unnamed data, @racket[y] is the times
  and @racket[statuses], then required, the indicators; for named data,
  @racket[y] names the time and status columns, and there is no
  @racket[statuses] argument. If no status is @racket[1], the
  @exnraise[exn:fail].

  @examples[#:eval ev
  (cox-fit X times statuses #:lambda 0.5)
  (define patients (list (cons "age" '(50 61 45 70 58 66 39 72))
                         (cons "dose" '(1.0 2.0 1.0 2.0 1.0 2.0 1.0 2.0))
                         (cons "months" times)
                         (cons "died" statuses)))
  (cox-fit patients '("months" "died") #:predictors '("age" "dose") #:lambda 0.05)
  (eval:error (cox-fit patients '("months") #:predictors '("age") #:lambda 0.5))
  (eval:error (cox-fit X times '(0 0 0 0 0 0 0 0) #:lambda 0.1))]}

@defproc[(cox-linear-predictor [fit cox-result?]
                               [X data/c])
         (listof real?)]{
  The log relative hazard @math{xβ} for each row of @racket[X]:
  @racket[predict] with its defaults.

  @examples[#:eval ev
  (cox-linear-predictor fit '((1.0 1.0) (3.0 1.0)))]}

@defproc[(cox-relative-risk [fit cox-result?]
                            [X data/c])
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

@defproc[(poisson-fit [X data/c]
                      [y (response-for/c X (>=/c 0))]
                      [#:predictors predictors (predictors-for/c X y) #f]
                      [#:lambda lambda (>=/c 0)]
                      [#:alpha alpha (real-in 0 1) 1.0]
                      [#:standardize? standardize? boolean? #t]
                      [#:intercept? intercept? boolean? #t]
                      [#:thresh thresh (>/c 0) 1e-7]
                      [#:max-iters max-iters exact-positive-integer? 100000])
         poisson-result?]{
  Fits a Poisson elastic-net model of the non-negative response @racket[y],
  which need not be integral. At least one value must be positive; an all-zero
  response raises @racket[exn:fail] before fitting (R warns and returns an
  empty model).

  @examples[#:eval ev
  (poisson-fit X y #:lambda 0.5)
  (poisson-fit mtcars "carb" #:predictors '("hp" "wt") #:lambda 0.1)
  (eval:error (poisson-fit X '(0 0 0 0 0 0 0 0) #:lambda 0.5))]}

@defproc[(poisson-predict-mean [fit poisson-result?]
                               [X data/c])
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

@defproc[(mgaussian-fit [X data/c]
                        [Y (responses-for/c X)]
                        [#:predictors predictors (predictors-for/c X Y) #f]
                        [#:lambda lambda (>=/c 0)]
                        [#:alpha alpha (real-in 0 1) 1.0]
                        [#:standardize? standardize? boolean? #t]
                        [#:intercept? intercept? boolean? #t]
                        [#:thresh thresh (>/c 0) 1e-7]
                        [#:max-iters max-iters exact-positive-integer? 100000])
         mgaussian-result?]{
  Fits a multi-response Gaussian elastic-net model. @racket[Y] is a matrix with
  one row per observation and one column per response, or, for named data, the
  names of the response columns. With @racket[alpha]
  above zero, the grouped lasso keeps or drops each predictor for every
  response at once. A constant column is fitted by its intercept alone, but
  when every column is constant (all zero, when @racket[intercept?] is
  @racket[#f]) there is nothing to fit and the @exnraise[exn:fail], as for
  @racket[elnet-fit].

  @examples[#:eval ev
  (mgaussian-fit X Y #:lambda 0.5)
  (mgaussian-result-coefficients
   (mgaussian-fit mtcars '("mpg" "qsec") #:predictors '("wt" "hp") #:lambda 0.5))]}

@defproc[(mgaussian-predict [fit mgaussian-result?]
                            [X data/c])
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
       predictors, and to @racket[1e-4] otherwise; glmnet raises a ratio below
       @racket[1e-6], including @racket[0], to @racket[1e-6]. The first value
       of an automatic sequence is the extrapolation R's @tt{glmnet} reports.}
 @item{A path can hold fewer @math{λ} than asked for, as in R. An automatic
       sequence stops early once another @math{λ} would barely change the
       fit; the rule depends on the family (below). A binomial or multinomial
       path, automatic or not, stops where the fitted probabilities saturate
       at 0 and 1. And any path ends where glmnet fails at one of its
       @math{λ}, for example by not converging within @racket[#:max-iters]
       passes: it keeps the @math{λ} before that one and logs a warning with
       the topic @racket['glmnet] (shown with
       @tt{PLTSTDERR="warning@"@"glmnet"}), where R warns. The warning names
       the procedure called: the path fitter, or the cross-validation or formula
       procedure that called it, with the fold whose training data it was
       fitting. When glmnet fails at
       the first @math{λ}, the fitter raises @racket[exn:fail] instead (R
       returns an empty model).}
]

glmnet's early-stopping rules apply only to an automatic sequence, and only
from its fifth value on. It stops at the @math{λ} where, writing @math{D} for
the deviance ratio:

@itemlist[
 @item{Gaussian: @math{D} gains less than @racket[1e-5] of its own value over
       the previous @math{λ}, or exceeds @racket[0.999].}
 @item{Multi-response Gaussian: the residual sum of squares falls by less than
       @racket[1e-5] of its own value, or @math{D} exceeds @racket[0.999].}
 @item{Binomial and multinomial: @math{D} gains less than @racket[1e-5] over
       the previous @math{λ}, or exceeds @racket[0.999].}
 @item{Poisson: @math{D} gains less than @racket[1e-4] of its own value over
       the previous four @math{λ}, or exceeds @racket[0.999].}
 @item{Cox: @math{D} gains less than @racket[1e-3] of its own value over the
       previous four @math{λ}, or exceeds @racket[0.99].}
]

These are R's @tt{glmnet.control} defaults (@tt{fdev = 1e-5},
@tt{devmax = 0.999}, @tt{mnlam = 5}) as each solver applies them.

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
         Multinomial intercepts are centred to sum to zero, as in
         @racket[multinomial-result]. It is @racket[#f] for Cox, which has no
         intercept.}
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

@defproc[(elnet-path [X data/c]
                     [y (response-for/c X real?)]
                     [#:predictors predictors (predictors-for/c X y) #f]
                     [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                     [#:nlambda nlambda exact-positive-integer? 100]
                     [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
                     [#:alpha alpha (real-in 0 1) 1.0]
                     [#:standardize? standardize? boolean? #t]
                     [#:intercept? intercept? boolean? #t]
                     [#:thresh thresh (>/c 0) 1e-7]
                     [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The Gaussian path, as @racket[elnet-fit] fits one point of it.

  @examples[#:eval ev
  (vector-length (glmnet-path-lambda (elnet-path X y)))
  (glmnet-path-df (elnet-path X y #:alpha 0.0 #:nlambda 5))
  (glmnet-path-lambda (elnet-path X y #:nlambda 5 #:lambda-min-ratio 0.1))
  (glmnet-path-df (elnet-path autos "mpg" #:predictors '("wt" "hp") #:nlambda 5))
  (eval:error (elnet-path X y #:lambda '(0.1) #:max-iters 1))]}

@defproc[(logistic-path [X data/c]
                        [y (response-for/c X (or/c 0 1) #:classes? #t)]
                        [#:predictors predictors (predictors-for/c X y) #f]
                        [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                        [#:nlambda nlambda exact-positive-integer? 100]
                        [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
                        [#:alpha alpha (real-in 0 1) 1.0]
                        [#:standardize? standardize? boolean? #t]
                        [#:intercept? intercept? boolean? #t]
                        [#:thresh thresh (>/c 0) 1e-7]
                        [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The binomial path, as @racket[logistic-fit] fits one point of it.

  @examples[#:eval ev
  (glmnet-path-df (logistic-path X '(0 0 0 1 1 1) #:lambda '(0.3 0.1 0.03)))
  (glmnet-path-df (logistic-path cars+ "heavy" #:predictors '("mpg" "hp") #:nlambda 5))]}

@defproc[(multinomial-path [X data/c]
                           [y (response-for/c X (and/c integer? (>=/c 0)) #:classes? #t)]
                           [#:predictors predictors (predictors-for/c X y) #f]
                           [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                           [#:nlambda nlambda exact-positive-integer? 100]
                           [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
                           [#:alpha alpha (real-in 0 1) 1.0]
                           [#:standardize? standardize? boolean? #t]
                           [#:intercept? intercept? boolean? #t]
                           [#:thresh thresh (>/c 0) 1e-7]
                           [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The multinomial path, as @racket[multinomial-fit] fits one point of it.

  @examples[#:eval ev
  (define mpath (multinomial-path X '(0 0 1 1 2 2) #:lambda '(0.3 0.03)))
  (vector-ref (glmnet-path-coefficients mpath) 1)
  (glmnet-path-df (multinomial-path iris "Species" #:nlambda 5
                                    #:predictors '("Petal.Length" "Petal.Width")))]}

@defproc[(cox-path [X data/c]
                   [y (survival-for/c X)]
                   [statuses (statuses-for/c X y) @#,elem{none}]
                   [#:predictors predictors (predictors-for/c X y) #f]
                   [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                   [#:nlambda nlambda exact-positive-integer? 100]
                   [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
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
  (glmnet-path-coefficients cpath)
  (glmnet-path-df (cox-path patients '("months" "died") #:predictors '("age" "dose")
                            #:lambda '(0.5 0.05)))]}

@defproc[(poisson-path [X data/c]
                       [y (response-for/c X (>=/c 0))]
                       [#:predictors predictors (predictors-for/c X y) #f]
                       [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                       [#:nlambda nlambda exact-positive-integer? 100]
                       [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? #t]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         glmnet-path?]{
  The Poisson path, as @racket[poisson-fit] fits one point of it.

  @examples[#:eval ev
  (glmnet-path-df (poisson-path X '(1 2 2 3 5 8) #:lambda '(0.5 0.05)))
  (glmnet-path-df (poisson-path mtcars "carb" #:predictors '("hp" "wt") #:nlambda 5))]}

@defproc[(mgaussian-path [X data/c]
                         [Y (responses-for/c X)]
                         [#:predictors predictors (predictors-for/c X Y) #f]
                         [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                         [#:nlambda nlambda exact-positive-integer? 100]
                         [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
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
  (glmnet-path-intercepts gpath)
  (glmnet-path-df (mgaussian-path mtcars '("mpg" "qsec") #:predictors '("wt" "hp") #:nlambda 5))]}

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
       one fold id per row of @racket[X], counting from 0, in a list, vector,
       @racket[flvector], @racket[f64vector], @racketmodname[math/array]
       array or Polars series, as a response can be. Every id
       from 0 to the largest must appear, and there must be at least 3 (see
       @racket[fold-ids/c]).}
 @item{@racket[#:grouped?], R's @tt{grouped}, computes the error and its
       standard error from the per-fold means when true, and from the
       per-observation losses otherwise. When the folds average fewer than 3
       observations, they are never grouped; @racket['auc] and @racket['C]
       are always computed per fold; and when the folds average fewer than 10
       observations, the Cox deviance is grouped. These adjustments, which R
       also makes, log a warning with the topic @racket['glmnet] that names
       the procedure called.}
 @item{@racket[#:lambda] is as for the path fitter, but needs at least two
       values. Every fold's path is fitted at those values. Without it, each
       fold's path chooses its own sequence, as R's does, and is evaluated at
       the full-data path's values by interpolation.}
]

The data are checked before any fold is fitted: the response must have what
its family needs, and so must every fold's training data (each class, or an
event). When a fit fails all the same, the error names the cross-validation
procedure and says which fit failed: the one to all the data, or the one to
the training data of a given held-out fold.

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
  @racket[deviance-ratio] is the path's at @racket[lambda-1se]. It prints
  like R's @tt{print.cv.glmnet}, with the index counted from 0: the measure,
  then for each of @racket[lambda-min] and @racket[lambda-1se] its value,
  index, @racket[cvm], @racket[cvsd] and @racket[nzero] (see
  @secref["ref-model-printing"]).

  @examples[#:eval ev
  (define cv (elnet-cv X60 y60))
  cv
  (glmnet-cv-lambda-min cv)
  (vector-ref (glmnet-cv-cvm cv) (glmnet-cv-index-min cv))
  (coef cv)
  (coef cv #:lambda 'lambda-min)
  (predict cv '((0.5 -0.5 0.0 0.0 0.0 0.0 0.0 0.0)))
  (deviance-ratio cv)]}

@defproc[(elnet-cv [X data/c]
                   [y (response-for/c X real?)]
                   [#:predictors predictors (predictors-for/c X y) #f]
                   [#:type-measure type-measure (or/c 'mse 'deviance 'mae) 'mse]
                   [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                   [#:fold-ids fold-ids fold-ids/c #f]
                   [#:grouped? grouped? boolean? #t]
                   [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                   [#:nlambda nlambda exact-positive-integer? 100]
                   [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
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
  (define car-cv
    (elnet-cv mtcars "mpg" #:predictors '("wt" "hp" "qsec")
              #:fold-ids (for/list ([i (in-range 32)]) (modulo i 4))))
  (coef car-cv)
  (predict car-cv (list (cons "qsec" '(17.0)) (cons "hp" '(150)) (cons "wt" '(3.0))))
  (define halves (for/list ([i (in-range 60)]) (modulo i 2)))
  (eval:error (elnet-cv X60 y60 #:fold-ids halves))]}

@defproc[(logistic-cv [X data/c]
                      [y (response-for/c X (or/c 0 1) #:classes? #t)]
                      [#:predictors predictors (predictors-for/c X y) #f]
                      [#:type-measure type-measure (or/c 'deviance 'class 'auc 'mse 'mae) 'deviance]
                      [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                      [#:fold-ids fold-ids fold-ids/c #f]
                      [#:grouped? grouped? boolean? #t]
                      [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                      [#:nlambda nlambda exact-positive-integer? 100]
                      [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
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
  their predicted probabilities, summed over the two classes. When the folds
  average fewer than 10 observations, @racket['auc] becomes
  @racket['deviance], as in R. @racket[y] must hold both classes.

  @examples[#:eval ev
  (define labels (for/list ([v (in-list y60)]) (if (> v 1.0) 1 0)))
  (logistic-cv X60 labels #:type-measure 'class)
  (logistic-cv X60 labels #:type-measure 'auc #:nfolds 5)
  (define heavy-cv
    (logistic-cv cars+ "heavy" #:predictors '("mpg" "hp")
                 #:fold-ids (for/list ([i (in-range 32)]) (modulo i 4))))
  (predict heavy-cv (hash "mpg" '(15.0 30.0) "hp" '(200 70)) #:type 'class)]}

@defproc[(multinomial-cv [X data/c]
                         [y (response-for/c X (and/c integer? (>=/c 0)) #:classes? #t)]
                         [#:predictors predictors (predictors-for/c X y) #f]
                         [#:type-measure type-measure (or/c 'deviance 'class 'mse 'mae) 'deviance]
                         [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                         [#:fold-ids fold-ids fold-ids/c #f]
                         [#:grouped? grouped? boolean? #t]
                         [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                         [#:nlambda nlambda exact-positive-integer? 100]
                         [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
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
  (multinomial-cv X60 classes #:type-measure 'class)
  (multinomial-cv iris "Species" #:predictors '("Petal.Length" "Petal.Width")
                  #:fold-ids (for/list ([i (in-range 150)]) (modulo i 3)) #:type-measure 'class)]}

@defproc[(cox-cv [X data/c]
                 [y (survival-for/c X)]
                 [statuses (statuses-for/c X y) @#,elem{none}]
                 [#:predictors predictors (predictors-for/c X y) #f]
                 [#:type-measure type-measure (or/c 'deviance 'C) 'deviance]
                 [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                 [#:fold-ids fold-ids fold-ids/c #f]
                 [#:grouped? grouped? boolean? #t]
                 [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                 [#:nlambda nlambda exact-positive-integer? 100]
                 [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
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

  A held-out fold's own deviance is undefined when the fold has no event, or
  when its first event is among its last two observations in time order. R
  stops then, and so does @racket[cox-cv], naming the fold. With
  @racket[#:grouped? #t], the default, the fold's own deviance is not needed.

  @examples[#:eval ev
  (define times (for/list ([v (in-list y60)]) (exp (* -0.3 v))))
  (define statuses
    (for/list ([i (in-range 60)])
      (if (zero? (modulo i 5)) 0 1)))
  (cox-cv X60 times statuses)
  (cox-cv X60 times statuses #:type-measure 'C)
  (define survival60 (hash "time" times "status" statuses
                           "x1" (map car X60) "x2" (map cadr X60)))
  (cox-cv survival60 '("time" "status") #:predictors '("x1" "x2"))
  (define thirds (for/list ([i (in-range 60)]) (modulo i 3)))
  (define fold-0-censored
    (for/list ([i (in-range 60)])
      (if (zero? (modulo i 3)) 0 1)))
  (eval:error
   (cox-cv X60 times fold-0-censored #:fold-ids thirds #:grouped? #f))]}

@defproc[(poisson-cv [X data/c]
                     [y (response-for/c X (>=/c 0))]
                     [#:predictors predictors (predictors-for/c X y) #f]
                     [#:type-measure type-measure (or/c 'deviance 'mse 'mae) 'deviance]
                     [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                     [#:fold-ids fold-ids fold-ids/c #f]
                     [#:grouped? grouped? boolean? #t]
                     [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                     [#:nlambda nlambda exact-positive-integer? 100]
                     [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
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
  (poisson-cv X60 counts)
  (poisson-cv (hash "count" counts "x1" (map car X60) "x2" (map cadr X60)) "count"
              #:predictors '("x1" "x2"))]}

@defproc[(mgaussian-cv [X data/c]
                       [Y (responses-for/c X)]
                       [#:predictors predictors (predictors-for/c X Y) #f]
                       [#:type-measure type-measure (or/c 'mse 'deviance 'mae) 'mse]
                       [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                       [#:fold-ids fold-ids fold-ids/c #f]
                       [#:grouped? grouped? boolean? #t]
                       [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                       [#:nlambda nlambda exact-positive-integer? 100]
                       [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
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
  (mgaussian-cv X60 Y60)
  (mgaussian-cv mtcars '("mpg" "qsec") #:predictors '("wt" "hp")
                #:fold-ids (for/list ([i (in-range 32)]) (modulo i 4)))]}

@defproc[(random-fold-ids [n exact-positive-integer?]
                          [#:nfolds nfolds exact-positive-integer? 10])
         (listof exact-nonnegative-integer?)]{
  Assigns @racket[n] observations to @racket[nfolds] folds at random, as R's
  @tt{sample(rep(seq(nfolds), length = n))} does: the fold ids
  @racket[0], @racket[1], ..., @racket[(- nfolds 1)], @racket[0], ... are
  shuffled with @racket[current-pseudo-random-generator], so that the folds
  differ in size by at most one. @racket[nfolds], given or by default, must
  not exceed @racket[n]. The cross-validation procedures call it when they
  are not given @racket[#:fold-ids]; calling it directly gives folds to reuse
  across calls.

  @examples[#:eval ev
  (random-fold-ids 10 #:nfolds 3)
  (define (draw)
    (parameterize ([current-pseudo-random-generator
                    (make-pseudo-random-generator)])
      (random-seed 1)
      (random-fold-ids 10 #:nfolds 3)))
  (equal? (draw) (draw))
  (eval:error (random-fold-ids 3 #:nfolds 5))]}

@defthing[fold-ids/c flat-contract?]{
  The contract on the @racket[#:fold-ids] argument of every cross-validation
  procedure. It accepts @racket[#f], for folds drawn by
  @racket[random-fold-ids], or fold ids: a non-empty list, vector,
  @racket[flvector], @racket[f64vector], @racketmodname[math/array] array or
  Polars series of non-negative integers, exact or, as in an @racket[flvector],
  inexact, that use every fold from 0 to the
  largest id, of which there are at least 3. A violation says which fold is
  missing. The procedures check the ids again on their own copy, with one id
  per observation.

  @examples[#:eval ev
  (require racket/flonum)
  (contract-first-order-passes? fold-ids/c '(0 1 2 0 1 2))
  (contract-first-order-passes? fold-ids/c (flvector 0.0 1.0 2.0 2.0))
  (contract-first-order-passes? fold-ids/c #f)
  (contract-first-order-passes? fold-ids/c '(0 1 0 1))
  (contract-first-order-passes? fold-ids/c (vector 0 2 3 0 2 3))
  (elnet-cv X60 y60 #:fold-ids (for/vector ([i (in-range 60)]) (modulo i 4)))
  (eval:error (elnet-cv X60 y60 #:fold-ids (for/list ([i (in-range 60)]) (* 2 (modulo i 3)))))]}

@section[#:tag "ref-formula"]{Formulas}

The formula front end fits any family from named data: a @tech{table} (see
@secref["ref-tables"]) or a Polars dataframe. A @tech{formula} names the
response and, in R's formula algebra, the predictor terms.
@racket[formula-fit], @racket[formula-path] and @racket[formula-cv] expand the
terms against the data's columns, as R's @tt{terms} does, build their design
matrix, as R's @tt{model.matrix} does, and call the fit procedure, path fitter
or cross-validation procedure of the family that @racket[#:family] names on
it. They return a @racket[formula-model], which keeps the formula, the names
of the design matrix's columns and the expanded terms, so that @racket[coef]
keys the coefficients by name and @racket[predict] builds the design matrix of
new named data (see @secref["ref-model"]). @secref["formulas"] works through
examples, @secref["formulas-algebra"] explains the algebra,
@secref["formulas-transforms"] the transforms, and
@secref["formulas-factors"] the factors.

A dataframe is read as @racket[(polars->table df)] would be, and the results
are the same, except that only the columns the formula uses are converted:
those its terms read and, for a fit, the response columns. The other columns
may have any dtype, and missing values. A converted column needs a numeric,
boolean, string, categorical or enum dtype and no missing value; a response
needs a numeric one, except the class labels of a binomial or multinomial
response and a Cox status, which may be boolean. An error names the column.
A dataframe with no rows is an error only for a formula that reads a column
of it, as a table is.

The terms of a formula expand as R's @tt{terms} expands them:

@itemlist[
 @item{A column name is a term of one variable, that column. @racket[all]
       is a term for each column of the table that is not a response column,
       in the table's order, as R's @tt{.} is.}
 @item{A transform, such as @racket[(log hp)] or @racket[(I (expt hp 2))], is
       a term of one variable, computed from columns (see below).}
 @item{@racket[(factor x)], R's @tt{factor(x)}, is a term of one variable,
       the values of the column @racket[x] or of the transform @racket[x] as
       a factor (see below).}
 @item{@racket[(+ a b)], or @racket[a + b], is the terms of @racket[a], then
       those of @racket[b]. Terms written side by side after the response are
       joined in the same way.}
 @item{@racket[(- a b)], or @racket[a - b], is the terms of @racket[a] except
       those equal to a term of @racket[b]. A term of @racket[b] that
       @racket[a] does not have is ignored. @racket[(- a)] removes @racket[a]
       from nothing.}
 @item{@racket[(: a b)], or @racket[a : b], is every term of @racket[a]
       combined with every term of @racket[b]: an interaction, whose variables
       are those of both.}
 @item{@racket[(* a b)], or @racket[a * b], crosses them: the terms of
       @racket[a + b + a : b]. When @racket[a] has no terms, as @racket[1] and
       @racket[0] have none, neither has @racket[a * b], as in R, whose
       @tt{y ~ 0*x + z} is @tt{y ~ z - 1}.}
 @item{@racket[(^ a n)], or @racket[a ^ n], crosses @racket[a] with itself:
       every interaction of at most @racket[n] of the terms of @racket[a], for
       an exact integer @racket[n] of at least 2. A variable crossed with
       itself is itself, so @racket[(^ x 2)] is @racket[x], as @tt{x^2} is in
       R.}
 @item{@racket[1] keeps the intercept and @racket[0] removes it, so
       @racket[- 1] removes it too. Inside the removed operand of @racket[-]
       their meanings swap, and the last one written wins, as in R. A formula
       without them has an intercept.}
]

A term is a set of variables, so @racket[(: wt hp)] and @racket[(: hp wt)] are
the same term. Each operation drops the terms it repeats, keeping the first.
The terms are then ordered by degree, the main effects first and then the
interactions of two variables, of three, and so on, each degree in the order
the terms first appear; the variables of a term are in the order they first
appear in the formula. The prefix forms with more operands are the infix forms
folded from the left: @racket[(* a b c)] is @racket[a * b * c].

The design matrix, which @racket[formula-design-matrix] returns, has one
column per term for numeric variables: a column of the table, or for an
interaction the product of its variables' columns, named by joining their
names with colons, as R names them: @racket["wt:hp"]. A factor has a column
for each of its levels, or for each but the first (see below), and an
interaction a column for each combination of its variables' columns, the
first variable's varying fastest. A column of the table keeps its name, where
R puts backticks around a name that R's syntax does not allow, such as
@racket["blood pressure"]. It has no intercept column, since the family's
procedure fits the intercept. Every column the formula names, even one that it
removes, must be a column of the table, and so must the response columns, which
must be distinct. Names are compared as strings. The design matrix's column
names must be distinct, and none can be @racket["(Intercept)"], since
@racket[coef] keys the coefficients by them and the intercept by
@racket["(Intercept)"]; the formula procedures raise an error that names the
column otherwise.

A group that starts with an identifier other than the operators, such as
@racket[(log hp)], is a @emph{transform}, as R's @tt{log(hp)} is: a Racket
expression, evaluated for each row of the table, whose values are a column of
the design matrix named by the transform's source, @racket["(log hp)"].
@racket[(I expr)], R's @tt{I()}, is the transform whose expression is
@racket[expr]. Inside a transform the operators are Racket's:
@racket[(I (* wt hp))] is the product of two columns, and
@racket[(log (+ hp 1))] adds 1. An identifier in argument position, that is,
not first in a group, is the table's column of that name when the table has
one, and otherwise the Racket binding of that name where the formula is
written, as R looks for a name in the data and then in the formula's
environment. An identifier first in a group is always the Racket binding, even
when the table has a column of that name, as R looks up the function of a
call by name and skips a column: with a column @racket[max],
@racket[(max max x)] is the larger of that column and @racket[x], R's
@tt{pmax(max, x)}. A name that the transform binds itself, with
@racket[let], @racket[lambda] or a @racket[for] form, is its own, a name
in quoted data is data, and a name that Racket's forms match as a literal,
@racket[=>], @racket[else], @racket[unquote], @racket[unquote-splicing],
@racket[...] or @racket[_], keeps its Racket meaning; the columns a
transform reads are the others. A column named like one of these literals
cannot be read inside a transform, where @racket[(log else)] is Racket's syntax
error @racketerror{else: not allowed as an expression}; write it as an
ordinary term, or rename it in the table. The
expression is evaluated once for each row, with each column name standing for
the row's value, so a transform is elementwise. A fit evaluates each transform
the terms use once on the table, as R's @tt{model.frame} does, and takes a
factor's levels and the design matrix from those values, so a transform with
side effects or random values gives both from the same draw. A transform that
@racket[-] removes is not evaluated, where R's @tt{model.frame} still
evaluates it once. A column of numbers gives its
value as a flonum, and a column of strings, symbols or booleans gives the
value itself, as R's calls read a character or logical column, so
@racket[(equal? Species "setosa")] is R's @tt{I(Species == "setosa")} and
@racket[(not b)] is R's @tt{I(!b)}. A column that a transform reads must hold
one kind of value, numbers, strings, symbols or booleans, and its numbers
must be finite; an error names the transform and the column. Strings and
symbols are two kinds here, though a factor's levels match them by label,
since a transform can tell them apart: @racket[(equal? g "a")] is false for
the symbol @racket['a]. @racket[predict] needs each such column to hold the
kind of value it held when the model was fitted, and an error names the
transform, the column and both kinds; a boolean column given as
@racket[1] and @racket[0] would otherwise change @racket[(if b x 0)]
silently, as @racket[0] is true in Racket. A name that is neither a column
nor bound is an error, naming it, when the transform is evaluated, not when
the formula is compiled. Each value must be a real number and finite, or
else each a string, a symbol or a boolean, which makes the transform a factor
(see below), and an error names the transform and the row. A transform must
read at least one column. It is a variable of the algebra, so
@racket[(* (log hp) wt)] has the terms @racket["(log hp)"], @racket["wt"] and
@racket["(log hp):wt"], and a transform written twice, as the same datum, is
one term.

A variable is a @emph{factor}, a categorical variable, when its values, a
column's or a transform's, are strings or symbols, or booleans, as R's
character and logical variables are, and under @racket[(factor x)], whose
values may be numbers too. Otherwise its values must be numbers. A factor's
levels are its distinct values, labelled as R labels them: a string itself, a
symbol's name, @racket["FALSE"] and @racket["TRUE"] for booleans, and a number
as Racket prints it, without a trailing @litchar{.0}; values with the same
label are one level. They are sorted as R's @tt{factor()} sorts them: numbers
by value, labels by @racket[string<?], which is R's order in the C locale,
and @racket["FALSE"] before @racket["TRUE"]. A boolean variable other than
@racket[(factor x)] has both boolean levels, even when one is absent, as R's
logical variable has. A factor needs at least two levels; a column or a
transform whose values mix numbers, strings or symbols, and booleans is an
error that names it. A factor is coded by treatment contrasts, as R's
@tt{model.matrix} codes it by default: a 0/1 column for each level but the
first, the baseline, named by the variable's name and the level's label, as
@racket["(factor cyl)6"] or @racket["Speciesversicolor"]. It has a column for
every level instead, dummies, where R's @tt{model.matrix} has: in a term
that has other variables, when no earlier term contains all of them (R's
@tt{factors} attribute of @tt{terms}), and, without an intercept, for the
first factor of the first term that has one. So in
@racket[(~ mpg wt : (factor cyl))] the factor has dummies, and after
@racket[wt], or after @racket[wt : hp], which contains @racket[wt], it has
contrasts. The fitted model keeps the levels (see
@racket[formula-model-levels]), and @racket[predict] codes a new table with
them; a value whose level the model was not fitted with is an error that
names the factor and the new levels.

A response column that stands alone as a term on the right-hand side is
dropped from it, and a warning naming the procedure called is logged on the
@racket['glmnet] topic, as R's @tt{model.matrix} warns. An interaction with a
response column is kept, as in R. The columns of a @racket[(surv time status)]
or multi-column response are each treated in the same way.

The intercept of the fit is the formula's: without @racket[#:intercept?], the
fit procedures fit an intercept unless the formula has @racket[0] or
@racket[- 1]. An explicit @racket[#:intercept?] must agree with a formula
that writes @racket[1], @racket[0] or @racket[- 1], which its contract checks,
naming both; with a formula that writes none, it decides. The Cox family
fits no intercept, and accepts @racket[1], @racket[0] and @racket[- 1] with
any @racket[#:intercept?]. As in R, they still decide how a factor is coded:
with @racket[0] or @racket[- 1], the first factor has a column for every
level (see above). A formula whose
terms leave no predictors, such as @racket[(~ y 1)], cannot be fitted: glmnet
needs at least one predictor.

The examples in this section use R's @tt{mtcars}, 32 cars of the 1974 Motor
Trend road tests, and R's @tt{iris}, 150 irises of three species, which
@racketmodname[glmnet/datasets] provides as tables (see @secref["ref-datasets"]):

@examples[#:eval ev #:label #f
(require glmnet/datasets)
(table-column-names mtcars)
(table-column-names iris)
]

Some also use @tt{mtcars} as a dataframe, from the @racketmodname[datasets]
package, with a text column of the cars' names:

@examples[#:eval ev #:label #f
(require (only-in datasets load-dataset) (prefix-in pl: (only-in polars head)))
(define cars (load-dataset 'mtcars #:format 'polars))
(column-names cars)
]

@defform*[#:literals (all surv I factor + - * : ^)
          [(~ response term ...)
           (~ response maybe-sign term operation ...)]
          #:grammar
          [(response column
                     (surv time-column status-column)
                     (column ...+))
           (term column
                 all
                 1
                 0
                 (+ term ...+)
                 (- term ...+)
                 (* term term ...+)
                 (: term term ...+)
                 (^ term power)
                 (maybe-sign term operation ...+)
                 (I expr)
                 (proc-id arg-expr ...)
                 (factor column)
                 (factor (I expr))
                 (factor (proc-id arg-expr ...)))
           (operation (code:line + term)
                      (code:line - term)
                      (code:line * term)
                      (code:line : term)
                      (code:line ^ power))
           (maybe-sign (code:line) + -)
           (column identifier
                   string)]]{
  A @tech{formula}: the @racket[response], then the right-hand side, either
  terms side by side, which are joined as by @racket[+], or terms with infix
  operators between them. The infix operators have R's precedence: @racket[^]
  binds tightest, then a leading sign, then @racket[:], then @racket[*], then
  @racket[+] and @racket[-]; each groups from the left, except that a power
  cannot be raised again, which R does not allow either. A parenthesized group
  is a prefix form when it starts with an operator and has no operators
  between its terms, a factor when it starts with @racket[factor], a
  transform when it starts with @racket[I] or another identifier that is not
  a word of the formula language, and an infix one otherwise. A
  @racket[power] is an exact integer of at least 2. The Racket reader's infix
  dots make @racketfont{(y . ~ . x + z)} the same as @racket[(~ y x + z)], so
  a formula can read as R writes it.

  The body is quoted, as by @racket[quote], except for its transforms, and the
  result is @racket[(make-formula 'response rhs ...)], with the elements of the
  right-hand side as written. Each transform becomes a
  @racket[transform-term?] value that computes it, whose name is its source,
  the datum as @racket[write] writes it with
  @racket[print-reader-abbreviations] on, so that quoted data is written
  @racket['x] and not @racketfont{(quote x)}. A transform's @racket[proc-id]
  must be bound where the formula is written: in a module, anywhere in it; at
  the top level, as in the REPL, by a definition evaluated before the
  formula, since a later one cannot be seen there. A name in argument
  position is read when the transform runs, so at the top level it can be
  defined after the formula. An @racket[arg-expr] or the @racket[expr] of
  @racket[I] is any Racket expression, checked as it is expanded, where the
  names bound in it are known: @racket[^], R's power, is a syntax error that
  says to write @racket[expt] unless it is bound, and so is R's infix
  arithmetic, such as @racket[(hp * wt)], whose first name is not bound,
  which says to write @racket[(* hp wt)]. A column is written as an
  identifier or a string, and as a string when its name is a word of the
  formula language (@racket[all], @racket[surv], the operators,
  and R's @tt{/} and @tt{%in%}, which it does not have), starts with
  @litchar{-}, or contains @litchar{:}, @litchar{*}, @litchar{^}, @litchar{/}
  or @litchar{+}. The reader reads @tt{wt:hp} and @tt{-wt} as one name, so an
  operator needs spaces around it; written as an identifier, such a name is a
  syntax error that says so. So is a number other than @racket[0] and
  @racket[1]; a @racket[0] or @racket[1] written with more than its digit,
  such as @tt{-0} or @tt{+0}, since the reader reads both as @racket[0] and
  drops the sign that R reads as an operator, so that R's @tt{-0} is written
  @racket[(- 0)] and its @tt{+0} @racket[(+ 0)]; a group that is neither a
  prefix form, an infix form nor a transform, such as @racket[(1 x z)]; a
  quoted name, such as @racket['hp], since @racket[~] quotes the names
  itself; and R's @tt{/} or @tt{%in%}, between terms or at the head of a
  group, where the error for @tt{/} says to write a ratio as
  @racket[(I (/ hp wt))]. The error points at the form that is wrong. Inside
  a transform, a column whose name is not an identifier is written with bars,
  as @racket[(log |blood pressure|)].

  Whether a @racket[0] or @racket[1] is written with more than its digit is
  read from its source location, the only trace of its spelling. A macro that
  builds one into a formula therefore gives it a location of its own, or none:
  a @racket[0] made by @racket[datum->syntax] with the location of a longer
  form, such as the @racket[#f] option it stands for, counts as written with
  more than its digit, and is a syntax error.

  @racket[(factor column)] is R's @tt{factor(column)}, and is quoted as the
  list @racket[(factor column)]. @racket[factor] of a transform is the list of
  @racket['factor] and the transform's @racket[transform-term?] value. A
  @racket[factor] group takes one column or transform, and is named by its
  source, as @racket["(factor cyl)"]; a column whose name is not an
  identifier is written there as a string, @racket["(factor \"n k\")"].

  The response is one column, @racket[(surv time-column status-column)] for
  the Cox family, or a list of columns for the multi-response Gaussian family.
  It is not a transform: a response that starts with @racket[I], or with a
  bound name and holds more than column names, such as
  @racket[(log (+ mpg 1))], is a syntax error that says a transformed response
  is not supported. So is a list of columns whose first name is a procedure
  where the formula is written, such as @racket[(log mpg)], an error raised
  when the formula is made, as only then is the value known; the columns of
  such a response are written as strings, as in @racket[("log" "mpg")].

  @examples[#:eval ev
  (~ mpg (* wt hp))
  (mpg . ~ . wt * hp)
  (formula-predictor-names (mpg . ~ . wt * hp) mtcars)
  (formula-predictor-names (~ mpg (^ (+ wt hp qsec) 2)) mtcars)
  (formula-predictor-names (mpg . ~ . (wt + hp + qsec) ^ 2 - wt : hp) mtcars)
  (formula-predictor-names (mpg . ~ . 0 + wt + hp) mtcars)
  (formula-predictor-names (~ mpg (- all cyl disp)) mtcars)
  (formula-predictor-names (mpg . ~ . (log hp) * wt + (I (/ disp cyl))) mtcars)
  (formula-predictor-names (mpg . ~ . wt * (factor cyl)) mtcars)
  (formula-predictor-names (mpg . ~ . (factor (> gear 3)) + wt) mtcars)
  (formula-predictor-names (Sepal.Length . ~ . 0 + Species + Petal.Width) iris)
  (formula-predictor-names (Sepal.Length . ~ . (equal? Species "setosa") + Petal.Width) iris)
  (~ (surv time status) age "blood pressure")
  (eval:error (~ mpg wt:hp))]}

@defthing[formula-term/c flat-contract?]{
  Accepts a term as data, as @racket[~] makes it: a column name (a string,
  or a symbol that @racket[~] accepts as a column name), @racket['all],
  @racket[0], @racket[1], a @racket[transform-term?] value, a list of
  @racket['factor] and a column name or a @racket[transform-term?] value, a
  prefix form, or a group of terms with infix operators between them. A
  transform is a @racket[transform-term?] value, not a list, which would be a
  group of terms.

  @examples[#:eval ev
  (formula-term/c '(* wt hp))
  (formula-term/c '(wt + hp))
  (formula-term/c '(^ (+ wt hp) 2))
  (formula-term/c (transform-term "(log wt)" '(wt) log))
  (formula-term/c '(factor cyl))
  (formula-term/c '(log wt))]}

@defthing[formula-rhs/c flat-contract?]{
  Accepts the right-hand side of a formula as data, as @racket[~] takes it: a
  list of terms, joined as by @racket[+], or an infix sequence, a term with a
  leading sign or with infix operators and terms after it. The empty list is
  the right-hand side of a formula without predictors.

  @examples[#:eval ev
  (formula-rhs/c '(wt hp (: wt hp)))
  (formula-rhs/c '(- 1 + wt * hp))
  (formula-rhs/c '(wt hp + qsec))]}

@defthing[formula-response/c flat-contract?]{
  Accepts a response as data: a column name, a list of @racket['surv] and two
  column names, or a non-empty list of column names.

  @examples[#:eval ev
  (formula-response/c '(surv time status))
  (formula-response/c '(surv time))]}

@defproc[(transform-term [name string?]
                         [columns (and/c (listof (or/c string? symbol?)) pair?)]
                         [proc (procedure-arity-includes/c (length columns))])
         transform-term?]{
  A transform as data, for @racket[make-formula]: the design-matrix column
  @racket[name], whose value in each row is @racket[proc] applied to the
  row's value of each of the @racket[columns], in order: a flonum for a column
  of numbers, and the value itself for a column of strings, symbols or
  booleans. Each of the @racket[columns] must be a column of the table that
  holds one kind of value, strings and symbols counting as two, and whose
  numbers are finite; new data for @racket[predict] must give it the same
  kind. @racket[~] makes the
  same value from a transform written in a formula, with its source as the
  name and the names it reads as the columns, except that a name of that
  transform that is not a column of the table is the Racket binding of that
  name.

  A procedure has no source and names no columns, so a transform as data
  names both. Two transforms are @racket[equal?] when their names and columns
  are, so a transform named by its source is @racket[equal?] to the one that
  @racket[~] makes from that source. It prints as
  @racketresultfont{#<transform-term} and its name, and a formula prints it
  as its name.

  @examples[#:eval ev
  (define log-hp (transform-term "(log hp)" '(hp) log))
  (define f (make-formula 'mpg log-hp '* 'wt))
  f
  (equal? f (mpg . ~ . (log hp) * wt))
  (define power-to-weight (transform-term "hp/wt" '("hp" "wt") /))
  (define P (formula-design-matrix (make-formula 'mpg power-to-weight) mtcars))
  (design-matrix->rows (design-matrix-select-rows P '(0 1 2)))]}

@defproc[(transform-term? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a transform, as @racket[transform-term]
  and @racket[~] make them.

  @examples[#:eval ev
  (formula-terms (~ mpg (log hp) wt))
  (transform-term? (car (formula-terms (~ mpg (log hp) wt))))]}

@defproc[(make-formula [response formula-response/c] [rhs any/c] ...)
         formula?]{
  The formula with @racket[response] and right-hand side @racket[rhs]s, which
  is what @racket[~] expands to. The list of the @racket[rhs]s must satisfy
  @racket[formula-rhs/c]. It builds a formula from names or terms computed at
  run time; a transform in it is a @racket[transform-term].

  @examples[#:eval ev
  (define chosen '("wt" "hp" "qsec"))
  (apply make-formula "mpg" chosen)
  (make-formula 'mpg `(^ (+ ,@chosen) 2))
  (make-formula 'mpg 'wt '* 'hp)
  (equal? (make-formula 'mpg 'wt '* 'hp) (mpg . ~ . wt * hp))
  (make-formula 'mpg 'wt (list 'factor "cyl"))]}

@defproc[(formula? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a formula. Two formulas are
  @racket[equal?] when their responses and right-hand sides are, as written,
  so @racket[(~ mpg (* wt hp))] and @racket[(~ mpg wt * hp)] are different
  formulas with the same terms, and two transforms are equal when they are
  written the same. A formula prints as the @racket[~] form that makes it.

  @examples[#:eval ev
  (formula? (~ mpg all))
  (formula? '(~ mpg all))
  (equal? (~ mpg wt * hp) (mpg . ~ . wt * hp))]}

@deftogether[(@defproc[(formula-response [f formula?]) formula-response/c]
              @defproc[(formula-terms [f formula?]) formula-rhs/c])]{
  The response and the right-hand side of @racket[f], as written: its terms,
  and the infix operators between them.

  @examples[#:eval ev
  (formula-response (~ (surv time status) age))
  (formula-terms (mpg . ~ . 1 + wt * hp))
  (formula-terms (~ mpg wt (- all cyl)))]}

@defproc[(formula-predictor-names [f formula?] [data (or/c table? named-data?)])
         (listof string?)]{
  The names of the columns of @racket[f]'s design matrix on @racket[data], a
  table or a dataframe, which are the names of the model's coefficients, in
  their order, as R's @tt{colnames(model.matrix(f, data))} without
  @tt{(Intercept)}. It is empty for a formula without predictors. It reads the
  values of the columns the terms use, which decide a factor's levels, and
  evaluates the transforms, as R's @tt{model.matrix} does.

  @examples[#:eval ev
  (formula-predictor-names (~ mpg wt hp (: wt hp qsec)) mtcars)
  (formula-predictor-names (mpg . ~ . wt * hp * qsec - wt : hp : qsec) mtcars)
  (formula-predictor-names (~ mpg (: all wt)) mtcars)
  (formula-predictor-names (mpg . ~ . (factor cyl) * (factor am)) mtcars)
  (formula-predictor-names (~ mpg 1) mtcars)
  (formula-predictor-names (~ mpg (- all model)) cars)]}

@defproc[(formula-design-matrix [f formula?] [data (or/c table? named-data?)])
         design-matrix?]{
  The design matrix of @racket[f] on @racket[data], a table or a dataframe,
  with one named column per predictor, which is what the formula procedures
  fit: R's @tt{model.matrix(f, data)} without its @tt{(Intercept)} column. The
  formula must have at least one predictor. The name is not
  @racketidfont{formula-model-matrix}, which would read as an accessor of a
  @racket[formula-model].

  @examples[#:eval ev
  (define X (formula-design-matrix (~ mpg (* wt hp)) mtcars))
  X
  (design-matrix-column-names X)
  (design-matrix->rows (design-matrix-select-rows X '(0 1 2)))
  (define L (formula-design-matrix (~ mpg hp (log hp) (I (expt hp 2))) mtcars))
  (design-matrix->rows (design-matrix-select-rows L '(0 1 2)))
  (define F (formula-design-matrix (~ mpg wt (factor cyl)) mtcars))
  (design-matrix-column-names F)
  (design-matrix->rows (design-matrix-select-rows F '(0 1 2)))]}

@defstruct*[formula-model ([formula formula?]
                           [predictor-names (listof string?)]
                           [fit glmnet-model?])
            #:transparent
            #:omit-constructor]{
  A model fitted from a formula. @racket[fit] is the result of the family's
  procedure: a single fit, a @racket[glmnet-path] or a @racket[glmnet-cv].
  @racket[predictor-names] names its predictors, the columns of the design
  matrix, in the order of its coefficients. Only @racket[formula-fit],
  @racket[formula-path] and @racket[formula-cv] make a formula model, since
  only they know that its names are those of the columns its fit was fitted
  to; the constructor is not exported. The model also keeps the terms it was
  fitted with, expanded, with its factors' levels (see
  @racket[formula-model-levels]), and for a binomial or multinomial response
  of strings, symbols or booleans, its classes; these are internal.

  A formula model implements @racket[gen:glmnet-model] through @racket[fit]:
  @racket[predict], @racket[coef] and @racket[deviance-ratio] give what they
  give for @racket[fit], except that @racket[coef] keys the coefficients by
  name and @racket[predict] takes a table or a dataframe, from whose columns
  it builds the design matrix of the fitted terms, reading only those the
  terms read. It does not expand the formula again, so @racket[all] stands for
  the columns it stood for in the fit, and the new data's columns can come in
  any order, without the response. It evaluates the transforms again, on the
  new rows: each name of a transform that was a column in the fit must be a
  column of the new data, and one that was a Racket binding is
  read again. It codes the factors with the fit's levels, whichever of them
  the new rows have, and a level that the fit did not have is an error that
  names the factor and the new levels. For a response with classes,
  @racket[coef] keys a multinomial model's coefficients by class, and
  @racket[predict] with @racket[#:type 'class] returns classes, as R's
  @tt{glmnet} does for a factor response.
  @racket[glmnet-model-predictor-names] returns @racket[predictor-names], and
  @racket[glmnet-model-response-names] the formula's response columns. A
  formula model prints as @racket[fit] does, with the formula after the
  family.

  @examples[#:eval ev
  (define m (formula-fit (mpg . ~ . wt * hp) mtcars #:lambda 0.1))
  m
  (formula-model-predictor-names m)
  (formula-model-fit m)
  (predict m (list (cons "hp" '(100 200)) (cons "wt" '(2.5 3.5))))
  (define logged (formula-fit (mpg . ~ . (log hp) + wt) mtcars #:lambda 0.1))
  (predict logged (list (cons "hp" '(100 200)) (cons "wt" '(2.5 3.5))))
  (define by-cylinders (formula-fit (mpg . ~ . wt + (factor cyl)) mtcars #:lambda 0.1))
  (predict by-cylinders (list (cons "cyl" '(8 4)) (cons "wt" '(3.5 2.5))))]}

@defproc[(formula-model-levels [m formula-model?])
         (listof (cons/c string? (listof string?)))]{
  The levels of @racket[m]'s factors, as R's @tt{xlevels}: for each variable of
  its terms that is a factor, in the order the variables first appear in the
  formula, a list of its name and the labels of its levels, the baseline
  first. A response column in an interaction is left out, as in R. A boolean
  variable has the levels @racket["FALSE"] and @racket["TRUE"], which R's
  @tt{xlevels} leaves out. @racket[predict] codes new data with these levels.

  @examples[#:eval ev
  (formula-model-levels
   (formula-fit (mpg . ~ . wt + (factor cyl)) mtcars #:lambda 0.1))
  (formula-model-levels
   (formula-fit (Sepal.Length . ~ . Species * Petal.Width + (> Sepal.Width 3))
                iris #:lambda 0.01))
  (formula-model-levels
   (formula-fit (mpg . ~ . wt + hp) mtcars #:lambda 0.1))]}

@defproc[(formula-fit [f formula?]
                      [data (or/c table? named-data?)]
                      [#:lambda lambda (>=/c 0)]
                      [#:family family
                                (or/c 'gaussian 'binomial 'multinomial 'poisson 'cox 'mgaussian)
                                'gaussian]
                      [#:alpha alpha (real-in 0 1) 1.0]
                      [#:standardize? standardize? boolean? #t]
                      [#:intercept? intercept? boolean? @#,elem{the formula's}]
                      [#:thresh thresh (>/c 0) 1e-7]
                      [#:max-iters max-iters exact-positive-integer? 100000])
         formula-model?]{
  Fits @racket[f] to @racket[data], a table or a dataframe, at a single
  @math{λ}, with the fit
  procedure of @racket[family]: @racket[elnet-fit], @racket[logistic-fit],
  @racket[multinomial-fit], @racket[poisson-fit], @racket[cox-fit] or
  @racket[mgaussian-fit], on the design matrix of @racket[f]. The keywords are
  passed on to it. @racket[intercept?] defaults to the formula's intercept and
  must agree with a formula's @racket[1], @racket[0] or @racket[- 1]; the Cox
  family has no intercept, so it does not apply to it.

  The response of @racket[f] must suit @racket[family], which the contract on
  @racket[f] checks: @racket[(surv time status)] for the Cox family, one or
  more columns for the multi-response family, and one column otherwise. Its
  values must be ones the family models: 0 or 1 for the binomial family, class
  labels @math{0, 1, …, K−1} with @math{K ≥ 2} and every class present for the
  multinomial family, non-negative for the Poisson family, and for the Cox
  family positive times and 0/1 statuses. An error names the column, and
  either the row of a value the family does not model or the missing class
  label. A binomial or multinomial response may instead hold strings, symbols
  or booleans, whose levels, sorted as a factor's are, are its classes, as R's
  @tt{glmnet} makes a factor response's: two for the binomial family, whose
  second is the class the model gives the probability of, and at least two for
  the multinomial family. An error that the family's procedure raises names
  @racket[formula-fit].

  @examples[#:eval ev
  (define fit (formula-fit (mpg . ~ . wt * hp) mtcars #:lambda 0.1))
  (coef fit)
  (coef (formula-fit (mpg . ~ . 0 + wt * hp) mtcars #:lambda 0.1))
  (coef (formula-fit (mpg . ~ . hp + (I (expt hp 2))) mtcars #:lambda 0.1))
  (coef (formula-fit (mpg . ~ . wt + (factor cyl)) mtcars #:lambda 0.1))
  (formula-fit (am . ~ . wt + hp) mtcars #:family 'binomial #:lambda 0.05)
  (map car (coef (formula-fit (~ Species all) iris #:family 'multinomial #:lambda 0.05)))
  (define car-model (formula-fit (~ mpg (+ wt hp)) cars #:lambda 0))
  (coef car-model)
  (predict car-model (pl:head cars 3))
  (eval:error (formula-fit (~ mpg 1) mtcars #:lambda 0.1))
  (eval:error (formula-fit (~ model wt) cars #:lambda 0.1))]}

@defproc[(formula-path [f formula?]
                       [data (or/c table? named-data?)]
                       [#:family family
                                 (or/c 'gaussian 'binomial 'multinomial 'poisson 'cox 'mgaussian)
                                 'gaussian]
                       [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) pair?)) #f]
                       [#:nlambda nlambda exact-positive-integer? 100]
                       [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
                       [#:alpha alpha (real-in 0 1) 1.0]
                       [#:standardize? standardize? boolean? #t]
                       [#:intercept? intercept? boolean? @#,elem{the formula's}]
                       [#:thresh thresh (>/c 0) 1e-7]
                       [#:max-iters max-iters exact-positive-integer? 100000])
         formula-model?]{
  Fits the @tech{regularization path} of @racket[f] on @racket[data], with the
  path fitter of @racket[family], such as @racket[elnet-path], to which the
  keywords are passed. The response and the intercept are as for
  @racket[formula-fit], and an error that the path fitter raises names
  @racket[formula-path].

  @examples[#:eval ev
  (define path
    (formula-path (~ mpg (^ (+ wt hp qsec) 2)) mtcars #:lambda '(1.0 0.5 0.1)))
  path
  (coef path #:lambda 0.1)
  (formula-path (~ am (- all model)) cars #:family 'binomial #:nlambda 5)]}

@defproc[(formula-cv [f formula?]
                     [data (or/c table? named-data?)]
                     [#:family family
                               (or/c 'gaussian 'binomial 'multinomial 'poisson 'cox 'mgaussian)
                               'gaussian]
                     [#:type-measure type-measure (or/c #f 'mse 'deviance 'mae 'class 'auc 'C) #f]
                     [#:nfolds nfolds (and/c exact-integer? (>=/c 3)) 10]
                     [#:fold-ids fold-ids fold-ids/c #f]
                     [#:grouped? grouped? boolean? #t]
                     [#:lambda lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))) #f]
                     [#:nlambda nlambda exact-positive-integer? 100]
                     [#:lambda-min-ratio lambda-min-ratio (or/c #f (and/c real? (>=/c 0) (</c 1))) #f]
                     [#:alpha alpha (real-in 0 1) 1.0]
                     [#:standardize? standardize? boolean? #t]
                     [#:intercept? intercept? boolean? @#,elem{the formula's}]
                     [#:thresh thresh (>/c 0) 1e-7]
                     [#:max-iters max-iters exact-positive-integer? 100000])
         formula-model?]{
  Cross-validates the path of @racket[f] on @racket[data] with the
  cross-validation procedure of @racket[family], such as @racket[elnet-cv], to
  which the keywords are passed. @racket[type-measure] must be one of the
  family's measures (see @secref["ref-cv"]), which the contract on it checks;
  @racket[#f], the default, is the family's default. @racket[fold-ids] needs
  one entry per row of @racket[data]. The response and the intercept are as
  for @racket[formula-fit], and an error that the cross-validation procedure
  raises names @racket[formula-cv].

  @examples[#:eval ev
  (define folds (for/list ([i (in-range 32)]) (modulo i 4)))
  (define cv-model (formula-cv (mpg . ~ . wt * hp) mtcars #:fold-ids folds))
  cv-model
  (coef cv-model)
  (define iris-df (table->polars iris))
  (define flowers (formula-cv (~ Species all) iris-df #:family 'multinomial
                              #:fold-ids (for/list ([i (in-range 150)]) (modulo i 5))))
  (predict flowers (pl:head iris-df 3) #:type 'class #:lambda 'lambda-1se)]}

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
and @racket[predict] takes a @tech{table} or a dataframe and reads the
predictors from it by name, or, for a formula model, builds them from its
columns, as its formula's terms say.

@racket[predict], @racket[coef] and @racket[glmnet-model-default-lambda] need
a model with at least one fitted @math{λ}. A @racket[glmnet-path] built by
hand can have none, and they reject such a model with a contract error that
blames the caller.

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
  or @racket[#f] if it does not name them. A @racket[formula-model] names them,
  and so does a fit from named data (see @secref["ref-common-data"]), which
  remembers them; a fit from unnamed data does not. When a model, such as a
  formula model, implements this method of @racket[gen:glmnet-model],
  @racket[coef] keys its coefficients by these names and
  @racket[predict] reads the columns with these names from a table or a
  dataframe, except that a formula model builds its predictors, such as the
  interaction @racket["a:b"] or the transform @racket["(log a)"], from its
  columns; the names a fit from named data remembers leave @racket[coef]'s
  layout as it is. The names must be distinct, one
  per predictor of the model's path; @racket[coef] and @racket[predict] raise
  an error for a model whose names are not.

  @examples[#:eval ev
  (define named-model
    (formula-fit (~ y all)
                 (list (cons "y" y) (cons "a" (map car X))
                       (cons "b" (map cadr X)) (cons "c" (map caddr X)))
                 #:lambda 0.05))
  (glmnet-model-predictor-names named-model)
  (glmnet-model-predictor-names car-fit)
  (glmnet-model-predictor-names fit)]}

@defproc[(glmnet-model-response-names [model glmnet-model?])
         (or/c #f (listof string?))]{
  The names of @racket[model]'s response columns, or @racket[#f] if it does not
  name them. For a @racket[formula-model], they are the columns of its
  formula's response: for the Cox family the time and status columns. For a
  multi-response formula model, @racket[coef] keys the coefficients of each
  response by its name, and raises an error for a model that does not name
  each response once. A multi-response fit from named data remembers its
  responses' names, which leave @racket[coef]'s layout as it is.

  @examples[#:eval ev
  (glmnet-model-response-names named-model)
  (glmnet-model-response-names path)]}

@defproc[(glmnet-model-class-labels [model glmnet-model?])
         (or/c #f (listof string?))]{
  The labels of a binomial or multinomial @racket[model]'s classes, in the
  order of the class indices, or @racket[#f] if it does not name them. A fit
  of a response of strings, symbols or booleans, a formula model's included,
  names them (see @secref["ref-common-data"]), and @racket[predict] with
  @racket[#:type 'class] returns them; a fit of class numbers does not.

  @examples[#:eval ev
  (glmnet-model-class-labels flowers)
  (glmnet-model-class-labels (multinomial-fit X '(0 1 2 0 1 2) #:lambda 0.05))]}

@defproc[(deviance-ratio [model glmnet-model?]) (or/c real? (vectorof real?))]{
  The fraction of null deviance explained, which is @math{R²} for the Gaussian
  families: a real for a single fit, and for a path a vector with one entry per
  fitted @math{λ}, R's @tt{dev.ratio}. It is not interpolated.

  @examples[#:eval ev
  (deviance-ratio fit)
  (deviance-ratio path)]}

@defproc[(predict [model glmnet-model?]
                  [X data/c]
                  [#:type type (or/c 'link 'response 'class) 'link]
                  [#:lambda lambda (or/c (>=/c 0) (and/c (listof (>=/c 0)) pair?)
                                         'lambda-min 'lambda-1se)
                                   (glmnet-model-default-lambda model)])
         list?]{
  Predictions for each row of @racket[X], as R's
  @tt{predict(fit, newx, s, type)}. @racket[X] needs one column per
  coefficient, in the order of the coefficients. For a model that names its
  predictors (see @racket[glmnet-model-predictor-names]), @racket[X] is instead
  a @tech{table} or a dataframe with a column of each of those names, in any
  order; its other columns are ignored, and a missing one is an error that
  names it. For a @racket[formula-model], @racket[X] needs the columns that the
  formula's terms read, from which @racket[predict] builds the design matrix as
  @racket[formula-design-matrix] does, transforms included; an error names the
  missing ones. A fit from named data (see @secref["ref-common-data"]) reads
  named @racket[X], a table or a dataframe, by its predictors' names, and
  unnamed @racket[X] by position. Any other model reads @racket[X] by
  position, and so does not take named data. @racket[type] chooses what is
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

  @racket['class] is an error for the families without classes. A class is its
  label, @racket[0], @racket[1], …, except for a @racket[formula-model] or a
  fit whose response holds strings, symbols or booleans, whose classes are
  the labels of its levels, such as @racket["setosa"] (see
  @racket[formula-fit] and @secref["ref-common-data"]). For one
  @math{λ}, the result has one entry per row of @racket[X]: a real, a class,
  or, for the multinomial and multi-response families, a list with one entry
  per class or response. When @racket[lambda] is a list, the result is a
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
  (predict named-model (table->polars new-table))
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
  association lists are in turn keyed by class, as @racket[predict] labels
  the classes, and for the multi-response family by response name (see
  @racket[glmnet-model-response-names]; @racket["y1"], @racket["y2"], … when
  the model names no responses), as R names the elements of its lists.

  @examples[#:eval ev
  (coef fit)
  (coef path #:lambda 0.1)
  (coef path #:lambda 0.4)
  (coef path #:lambda 5.0)
  (coef named-model)
  (eval:error (coef (glmnet-path 'gaussian (vector) (vector) (vector)
                                 (vector) (vector) 0)))]}

@defproc[(in-path [model glmnet-model?]) sequence?]{
  A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence}
  that walks @racket[model]'s @tech{regularization path}, with two values per
  element: each fitted @math{λ}, in the path's order, and the coefficients at
  that @math{λ} as @racket[coef] gives them, keyed by name for a model that
  names its predictors. A single fit is a path with one @math{λ}, and a
  @racket[glmnet-cv] walks its path of all the data. Nothing is interpolated:
  each element holds the path's own fit at its @math{λ}.

  @examples[#:eval ev
  (for ([(λ β) (in-path path)])
    (printf "~a: ~a\n" λ β))
  (for/list ([(λ β) (in-path (formula-path (~ mpg (+ wt hp)) mtcars #:nlambda 4))])
    β)]}

@subsection[#:tag "ref-model-printing"]{Printing}

A single fit prints on one line with its family, its @math{λ} (to four
significant digits), its deviance ratio (to four decimal places) and the
number of nonzero coefficients out of the number of predictors, counting a
predictor once when it is nonzero for any class or response.

A path prints R's @tt{print.glmnet} table line for line, without the
@tt{Call:} line R prints above it. Each fitted @math{λ} has a row, numbered
from 1, with @tt{Df}, the deviance ratio as a percentage rounded to two
places (@tt{%Dev}), and @math{λ} to four significant digits (@tt{Lambda}).
As in R, @tt{%Dev} and @tt{Lambda} are then rounded to about five significant
digits of the column's largest value, so a @math{λ} far below the first can
show as @racket[0], and each column is written with one number of decimals
throughout, or in scientific notation when that is narrower.

A @racket[glmnet-cv] prints like R's @tt{print.cv.glmnet}: the name of its
measure, then a row for each of @racket[glmnet-cv-lambda-min] and
@racket[glmnet-cv-lambda-1se] with that @math{λ}, its index, the
cross-validated error, its standard error and the number of nonzero
coefficients. It differs from R's in two ways: the index counts from 0, where
R's counts from 1, and each real is rounded to four significant digits on
its own, where R formats each column as a whole and can show a digit more. A @racket[formula-model]
prints as the result it holds, with its formula after the family.

Printing does not change @racket[equal?], which compares results field by
field.

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
                                [#:out-file out-file
                                            (or/c #f (and/c path-string? has-image-extension?))
                                            #f])
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
         its predictors, as a @racket[formula-model] and a fit from named data
         (see @secref["ref-common-data"]) do, and otherwise its
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
         @filepath{pdf}, @filepath{svg} or @filepath{eps}, in upper or lower
         case. @racket[has-image-extension?] in the contract holds for exactly
         those paths, so any other extension, or none, raises
         @racket[exn:fail:contract] before anything is drawn.}
  ]

  An error is raised if every coefficient is zero at every @math{λ}, as there
  is then nothing to plot. A @math{λ} of @racket[0] has no position on a
  log @math{λ} axis and is left out: the curves, and their labels, end at the
  smallest positive @math{λ}. If every @math{λ} of the path is @racket[0], an
  error is raised for a log @math{λ} axis, as R's @tt{plot} stops too; the
  path can still be plotted against the L1 norm or the deviance ratio.

  @examples[#:eval ev
  (plot-coefficient-path path #:xvar 'norm #:label '("x1" "x2" "x3")
                         #:width 400 #:height 300 #:title "Lasso path")
  (plot-coefficient-path (elnet-path mtcars "mpg" #:predictors '("wt" "hp" "qsec"))
                         #:label #t #:width 400 #:height 300)
  (eval:error (plot-coefficient-path (elnet-path X y #:lambda '(10.0 5.0))))
  (eval:error (plot-coefficient-path (elnet-path X y #:lambda '(0.0))))]}

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
                  [#:out-file out-file
                              (or/c #f (and/c path-string? has-image-extension?))
                              #f])
         pict?]{
  Plots the cross-validation curve of @racket[cv] against
  @racket[sign-lambda] times @math{log λ}, as R's @tt{plot.cv.glmnet} does (see
  @secref["plot-cv"]), with the number of nonzero coefficients along the top.
  A @racket[formula-model] must hold a @racket[glmnet-cv], from
  @racket[formula-cv].
  @racket[width], @racket[height], @racket[title] and @racket[out-file] are as
  for @racket[plot-coefficient-path]. As there, a @math{λ} of @racket[0] is left
  out, and an error is raised if every @math{λ} is @racket[0].

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
