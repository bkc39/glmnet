#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-data-sources"]{Data sources}

@margin-note{Source: @filepath{glmnet/examples/13-data-sources.rkt}}

This section fits one model to one dataset, which arrives through each data
source of @secref["data"] in turn: R's
@tt{glmnet(as.matrix(mtcars[, c("wt", "hp", "qsec")]), mtcars$mpg)}, the
lasso path of fuel economy on a car's weight, horsepower and quarter-mile
time. The data are R's @tt{mtcars}, which @racketmodname[glmnet/datasets]
ships as a CSV file and provides as a table. Every source holds the same
doubles and every fit keeps them, so each gives the same path, R's.

@section[#:tag "ex-data-sources-table"]{A table}

A table names the response column, and @racket[#:predictors] the predictor
columns. The path has R's sequence of @math{λ}, and R's
@tt{coef(fit, s = 0.5)}:

@examples[#:eval ev #:label #f
(require glmnet/datasets)
(define predictors '("wt" "hp" "qsec"))
(define from-table (elnet-path mtcars "mpg" #:predictors predictors))
(vector-length (glmnet-path-lambda from-table))
(coef from-table #:lambda 0.5)
]

@section[#:tag "ex-data-sources-nested"]{Lists and vectors}

The table's columns are vectors. A vector of rows and the response column are
unnamed data:

@examples[#:eval ev #:label #f
(define mpg (dict-ref mtcars "mpg"))
(define columns (map (λ (name) (dict-ref mtcars name)) predictors))
(define rows (apply vector-map vector columns))
(vector-take rows 3)
(equal? (elnet-path rows mpg) from-table)
]

@section[#:tag "ex-data-sources-csv"]{A CSV file}

@racket[csv-file->table] reads the file into a table of vector columns:

@examples[#:eval ev #:label #f
(require glmnet/data/csv)
(define file (collection-file-path "mtcars.csv" "glmnet" "datasets"))
(define cars (csv-file->table file))
(equal? (elnet-path cars "mpg" #:predictors predictors) from-table)
]

@section[#:tag "ex-data-sources-math"]{A matrix}

A @racketmodname[math/matrix] matrix has the observations as its rows, and the
response can be a column matrix:

@examples[#:eval ev #:label #f
(require (only-in math/matrix ->col-matrix matrix-shape vector*->matrix))
(define M (vector*->matrix rows))
(matrix-shape M)
(equal? (elnet-path M (->col-matrix mpg)) from-table)
]

@section[#:tag "ex-data-sources-polars"]{A Polars dataframe}

Polars reads the file itself, and the predictors leave it in one copy:

@examples[#:eval ev #:label #f
(require (only-in polars read-csv))
(define df (read-csv file))
(equal? (elnet-path df "mpg" #:predictors predictors) from-table)
]

@section[#:tag "ex-data-sources-formulas"]{Formulas}

The table, the CSV table and the dataframe name their columns, so each also
fits the path as a formula model, whose @racket[coef] names the coefficients:

@examples[#:eval ev #:label #f
(define (formula-fit data)
  (formula-path (mpg . ~ . wt + hp + qsec) data))
(define formula-from-table (formula-fit mtcars))
(define formula-from-csv (formula-fit cars))
(define formula-from-polars (formula-fit df))
(coef formula-from-table #:lambda 0.5)
(equal? (coef formula-from-csv #:lambda 0.5) (coef formula-from-table #:lambda 0.5))
(equal? (coef formula-from-polars #:lambda 0.5) (coef formula-from-table #:lambda 0.5))
]

@section[#:tag "ex-data-sources-explicit"]{A design matrix, and back}

Converting to a @tech{design matrix} once checks the data once, for many fits
(see @secref["data-explicit"]), and fits the same path:

@examples[#:eval ev #:label #f
(require glmnet/data/nested glmnet/data/math glmnet/data/polars)
(define design (table->design-matrix mtcars predictors))
(equal? (elnet-path design mpg) from-table)
]

The design matrix converts back to each source, and @racket[table->csv] writes
it as CSV:

@examples[#:eval ev #:label #f
(define first-cars (design-matrix-select-rows design '(0 1 2)))
(design-matrix->nested first-cars)
(design-matrix->matrix first-cars)
(design-matrix->polars first-cars)
(table->csv first-cars)
]

@(close-eval ev)
