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
doubles and every conversion keeps them, so each gives the same design
matrix, names included, and the same path, R's.

@section[#:tag "ex-data-sources-table"]{A table}

The predictors by name, the file the table comes from, and a column of the
table by name:

@examples[#:eval ev #:label #f
(require glmnet/datasets
         glmnet/data/nested
         glmnet/data/csv
         glmnet/data/math
         glmnet/data/polars
         (only-in math/matrix build-matrix ->col-matrix matrix-shape)
         (only-in polars read-csv))
(define predictors '("wt" "hp" "qsec"))
(define file (collection-file-path "mtcars.csv" "glmnet" "datasets"))
(define (column name) (cdr (assoc name mtcars)))
]

A table's numeric columns are a design matrix, and its response column, a
vector, is a response as it is. The path has R's sequence of @math{λ}, and
R's @tt{coef(fit, s = 0.5)}:

@examples[#:eval ev #:label #f
(define from-table (table->design-matrix mtcars predictors))
(define mpg (column "mpg"))
(define fit (elnet-path from-table mpg))
(vector-length (glmnet-path-lambda fit))
(coef fit #:lambda 0.5)
]

@section[#:tag "ex-data-sources-nested"]{Lists and vectors}

The table's columns are vectors, which @racket[nested->design-matrix] takes
one per column:

@examples[#:eval ev #:label #f
(define from-nested
  (nested->design-matrix (map column predictors) #:by 'columns #:column-names predictors))
(equal? from-nested from-table)
]

@section[#:tag "ex-data-sources-csv"]{A CSV file}

@racket[csv-file->table] reads the file into a table of vector columns:

@examples[#:eval ev #:label #f
(define cars (csv-file->table file))
(define from-csv (table->design-matrix cars predictors))
(equal? from-csv from-table)
(equal? (elnet-path from-csv (cdr (assoc "mpg" cars))) fit)
]

@section[#:tag "ex-data-sources-math"]{A matrix}

A @racketmodname[math/matrix] matrix has the observations as its rows, and the
response can be a column matrix:

@examples[#:eval ev #:label #f
(define M
  (build-matrix 32 3 (lambda (i j) (vector-ref (column (list-ref predictors j)) i))))
(matrix-shape M)
(define from-math (matrix->design-matrix M #:column-names predictors))
(equal? from-math from-table)
(equal? (elnet-path from-math (array->response (->col-matrix mpg))) fit)
]

@section[#:tag "ex-data-sources-polars"]{A Polars dataframe}

Polars reads the file itself, and the predictors leave it in one copy:

@examples[#:eval ev #:label #f
(define df (read-csv file))
(define from-polars (polars->design-matrix df predictors))
(equal? from-polars from-table)
(equal? (elnet-path from-polars (polars->response df "mpg")) fit)
]

@section[#:tag "ex-data-sources-formulas"]{Formulas, and back}

Each source that gives a @tech{table}, the dataset, the CSV file's table and
the dataframe through @racket[polars->table], fits the same formula model:

@examples[#:eval ev #:label #f
(define (fit-formula table)
  (coef (formula-fit (mpg . ~ . wt + hp + qsec) table #:lambda 0.5)))
(fit-formula mtcars)
(equal? (fit-formula cars) (fit-formula mtcars))
(equal? (fit-formula (polars->table df)) (fit-formula mtcars))
]

The design matrix converts back to each of them. A design matrix with column
names is a table, which @racket[table->csv] writes:

@examples[#:eval ev #:label #f
(define first-cars (design-matrix-select-rows from-table '(0 1 2)))
(design-matrix->nested first-cars)
(design-matrix->matrix first-cars)
(design-matrix->polars first-cars)
(table->csv first-cars)
]

@(close-eval ev)
