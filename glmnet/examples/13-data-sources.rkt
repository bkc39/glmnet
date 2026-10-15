#lang scribble/lp2

@(require (for-label racket/base
                     racket/dict
                     racket/port
                     racket/vector
                     glmnet
                     glmnet/datasets
                     glmnet/data/nested
                     glmnet/data/csv
                     glmnet/data/math
                     glmnet/data/polars
                     (only-in math/matrix ->col-matrix vector*->matrix)
                     (only-in polars read-csv)))

@section[#:tag "ex-data-sources"]{Data sources}

Every fit takes its data as it is, unnamed or named. This example fits one
model to one dataset, which arrives from each data source in turn: R's
@tt{glmnet(as.matrix(mtcars[, c("wt", "hp", "qsec")]), mtcars$mpg)}, the
lasso path of fuel economy on a car's weight, horsepower and quarter-mile
time.

The data are R's @tt{mtcars}, which @racketmodname[glmnet/datasets] ships as
a CSV file and provides as a table. The predictors and the response come from
five sources:

@itemlist[
 @item{the table itself;}
 @item{Racket's vectors, one per row;}
 @item{the CSV file, through @racket[csv-file->table] from
       @racketmodname[glmnet/data/csv];}
 @item{a @racketmodname[math/matrix] matrix and a column matrix;}
 @item{a Polars dataframe that Polars' own @racket[read-csv] reads from the
       file.}
]

Every source holds the same doubles, and every fit keeps them, so the five
fitted paths are @racket[equal?]. Each is R's: 58 values of @math{λ}, from
5.147 down to 0.02562, where the model explains 83.5% of the deviance, and at
@math{λ = 0.5}, R's @tt{coef(fit, s = 0.5)}, an intercept of 33.17 and
coefficients of −3.683 for the weight, −0.02385 for the horsepower and 0.1273
for the quarter-mile time.

@chunk[<require>
(require racket/dict
         racket/port
         racket/vector
         glmnet
         glmnet/datasets
         glmnet/data/nested
         glmnet/data/csv
         glmnet/data/math
         glmnet/data/polars
         (only-in math/matrix ->col-matrix vector*->matrix)
         (only-in polars read-csv))]

@chunk[<provide>
(provide predictors file mpg rows cars df fits coefficients formula-coefs
         design from-design first-cars nested matrix dataframe csv)]

The predictors by name, and the file the table comes from:

@chunk[<data>
(define predictors '("wt" "hp" "qsec"))
(define file (collection-file-path "mtcars.csv" "glmnet" "datasets"))]

A table names the response column, and @racket[#:predictors] the predictor
columns:

@chunk[<table>
(define from-table (elnet-path mtcars "mpg" #:predictors predictors))]

The table's columns are vectors; a vector of rows and the response column
are unnamed data:

@chunk[<rows>
(define mpg (dict-ref mtcars "mpg"))
(define columns (for/list ([name (in-list predictors)]) (dict-ref mtcars name)))
(define rows (apply vector-map vector columns))
(define from-rows (elnet-path rows mpg))]

@racket[csv-file->table] reads the file into a table of vector columns, typing
each cell as R's @tt{read.csv} would:

@chunk[<csv>
(define cars (csv-file->table file))
(define from-csv (elnet-path cars "mpg" #:predictors predictors))]

A @racketmodname[math/matrix] matrix has its observations as rows, and a
response can be a column matrix:

@chunk[<math>
(define from-math (elnet-path (vector*->matrix rows) (->col-matrix mpg)))]

Polars reads the file itself, in native code, and the predictors leave it in
one copy:

@chunk[<polars>
(define df (read-csv file))
(define from-polars (elnet-path df "mpg" #:predictors predictors))]

The same lasso path from each source, and R's @tt{coef(fit, s = 0.5)}:

@chunk[<fits>
(define fits (list from-table from-rows from-csv from-math from-polars))
(define coefficients (coef from-table #:lambda 0.5))]

Each source that is named data, the dataset, the CSV file's table and the
dataframe, fits the same path as a formula model, whose @racket[coef] names
the coefficients:

@chunk[<formulas>
(define (formula-coef data)
  (coef (formula-path (mpg . ~ . wt + hp + qsec) data) #:lambda 0.5))
(define formula-coefs (map formula-coef (list mtcars cars df)))]

Converting to a @tech{design matrix} once checks the data once, for many
fits, and fits the same path. The design matrix converts back to each source, and @racket[table->csv]
writes it as CSV:

@chunk[<explicit>
(define design (table->design-matrix mtcars predictors))
(define from-design (elnet-path design mpg))
(define first-cars (design-matrix-select-rows design '(0 1 2)))
(define nested (design-matrix->nested first-cars))
(define matrix (design-matrix->matrix first-cars))
(define dataframe (design-matrix->polars first-cars))
(define csv (with-output-to-string (λ () (table->csv first-cars))))]

@chunk[<*>
  <require>
  <provide>
  <data>
  <table>
  <rows>
  <csv>
  <math>
  <polars>
  <fits>
  <formulas>
  <explicit>]
