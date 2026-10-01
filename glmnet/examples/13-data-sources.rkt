#lang scribble/lp2

@(require (for-label racket/base
                     glmnet
                     glmnet/datasets
                     glmnet/data/nested
                     glmnet/data/csv
                     glmnet/data/math
                     glmnet/data/polars
                     (only-in math/matrix build-matrix ->col-matrix)
                     (only-in polars read-csv)))

@section[#:tag "ex-data-sources"]{Data sources}

Every fitter reads a @tech{design matrix} and a @tech{response}, and each
data format that glmnet reads is a module under @filepath{glmnet/data/} that
converts to them. This example takes one dataset through every one of them,
and fits the same model to each: R's
@tt{glmnet(as.matrix(mtcars[, c("wt", "hp", "qsec")]), mtcars$mpg)}, the
lasso path of fuel economy on a car's weight, horsepower and quarter-mile
time.

The data are R's @tt{mtcars}, which @racketmodname[glmnet/datasets] ships as
a CSV file and provides as a table. The predictors and the response come from
five sources:

@itemlist[
 @item{the table itself, through @racket[table->design-matrix];}
 @item{Racket's vectors, the table's columns, through
       @racket[nested->design-matrix] from @racketmodname[glmnet/data/nested];}
 @item{the CSV file, through @racket[csv-file->table] from
       @racketmodname[glmnet/data/csv];}
 @item{a @racketmodname[math/matrix] matrix and a column matrix, through
       @racket[matrix->design-matrix] and @racket[array->response] from
       @racketmodname[glmnet/data/math];}
 @item{a Polars dataframe that Polars' own @racket[read-csv] reads from the
       file, through @racket[polars->design-matrix] and
       @racket[polars->response] from @racketmodname[glmnet/data/polars].}
]

Every source holds the same doubles, and every conversion keeps them, so the
five design matrices are @racket[equal?], column names included, and so are
the five fitted paths. Each is R's: 58 values of @math{λ}, from 5.147 down to
0.02562, where the model explains 83.5% of the deviance, and at
@math{λ = 0.5}, R's @tt{coef(fit, s = 0.5)}, an intercept of 33.17 and
coefficients of −3.683 for the weight, −0.02385 for the horsepower and 0.1273
for the quarter-mile time.

@chunk[<require>
(require glmnet
         glmnet/datasets
         glmnet/data/nested
         glmnet/data/csv
         glmnet/data/math
         glmnet/data/polars
         (only-in math/matrix build-matrix ->col-matrix)
         (only-in polars read-csv))]

@chunk[<provide>
(provide run-example)]

The predictors by name, the file, and a column of the table by name:

@chunk[<data>
(define predictors '("wt" "hp" "qsec"))
(define file (collection-file-path "mtcars.csv" "glmnet" "datasets"))
(define (column name) (cdr (assoc name mtcars)))]

A table's numeric columns are a design matrix, and its response column is a
response as it is, a vector:

@chunk[<table>
(define from-table (table->design-matrix mtcars predictors))
(define mpg (column "mpg"))]

The table's columns are vectors, which @racket[nested->design-matrix] takes
one per column with @racket[#:by 'columns]:

@chunk[<nested>
(define from-nested
  (nested->design-matrix (map column predictors) #:by 'columns #:column-names predictors))]

@racket[csv-file->table] reads the file into a table of vector columns, typing
each cell as R's @tt{read.csv} would:

@chunk[<csv>
(define cars (csv-file->table file))
(define from-csv (table->design-matrix cars predictors))
(define mpg-csv (cdr (assoc "mpg" cars)))]

A @racketmodname[math/matrix] matrix has its observations as rows, and a
response can be a column matrix:

@chunk[<math>
(define M
  (build-matrix 32 3 (lambda (i j) (vector-ref (column (list-ref predictors j)) i))))
(define from-math (matrix->design-matrix M #:column-names predictors))
(define mpg-math (array->response (->col-matrix mpg)))]

Polars reads the file itself, in native code, and the predictors leave it in
one copy:

@chunk[<polars>
(define df (read-csv file))
(define from-polars (polars->design-matrix df predictors))
(define mpg-polars (polars->response df "mpg"))]

The same lasso path from each source, and R's @tt{coef(fit, s = 0.5)} from
the first:

@chunk[<fits>
(define design-matrices (list from-table from-nested from-csv from-math from-polars))
(define fits
  (for/list ([x (in-list design-matrices)]
             [y (in-list (list mpg mpg mpg-csv mpg-math mpg-polars))])
    (elnet-path x y)))
(define coefficients (coef (car fits) #:lambda 0.5))]

@chunk[<run-example>
(define (run-example)
  <data>
  <table>
  <nested>
  <csv>
  <math>
  <polars>
  <fits>
  (values design-matrices fits coefficients))]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
