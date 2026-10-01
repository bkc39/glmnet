#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "data" #:style 'toc]{Data}

Every fit reads a @tech{design matrix} and a @tech{response}, and the formula
front end reads a @tech{table}. This chapter is about where they come from:
the example datasets that ship with the package, tables, Racket's own lists
and vectors, and CSV files. Each data format is a module under
@filepath{glmnet/data/}, which @racket[(require glmnet)] does not load.

@local-table-of-contents[]

@section[#:tag "data-datasets"]{Example datasets}

@racketmodname[glmnet/datasets] provides the example datasets of R's
@hyperlink["https://glmnet.stanford.edu/"]{glmnet} package, which its vignettes
use, and R's @tt{mtcars} and @tt{iris}, which the formula examples use.
@racket[(require glmnet)] does not load it:

@examples[#:eval ev #:label #f
(require glmnet/datasets)
]

@examples[#:eval ev #:hidden
(require racket/list racket/vector)
]

@margin-note{See @secref["ref-datasets"] in the @secref["reference"] for
each loader.}

R loads a dataset with @tt{data}, which makes a list of @tt{x} and @tt{y}:
@tt{data(QuickStartExample); x <- QuickStartExample$x}. Each dataset here has
a procedure instead, which returns the arguments of its family's fitter, in
order: a design matrix of the predictors, then the response.

@examples[#:eval ev #:label #f
(define-values (x y) (quick-start-example))
x
(lasso x y #:lambda 0.1)
]

The design matrix names its columns @racket["V1"], @racket["V2"], and so on,
as R's @tt{coef} names the columns of these unnamed matrices. The response
has the shape its family takes (see @secref["concepts-data"]):

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{R}                       @bold{Loader}                   @bold{Values})
       (list @tt{QuickStartExample}         @racket[quick-start-example]     "100 × 20, a list of reals")
       (list @tt{BinomialExample}           @racket[binomial-example]        "100 × 30, a list of 0/1 labels")
       (list @tt{MultinomialExample}        @racket[multinomial-example]     "500 × 30, a list of class labels 0, 1, 2")
       (list @tt{PoissonExample}            @racket[poisson-example]         "500 × 20, a list of counts")
       (list @tt{CoxExample}                @racket[cox-example]             "1000 × 30, a list of times, a list of 0/1 statuses")
       (list @tt{MultiGaussianExample}      @racket[multi-gaussian-example]  "100 × 20, a 100 × 4 design matrix")
       (list @tt{SparseExample}             @racket[sparse-example]          "100 × 20, dense, a list of reals"))]

So a loader's values go straight to its fitter, and
@racket[call-with-values] passes them all:

@examples[#:eval ev #:label #f
(call-with-values binomial-example
                  (lambda (x y) (logistic-fit x y #:lambda 0.05)))
(define-values (cx time status) (cox-example))
(take time 3)
(take status 3)
]

Two datasets differ from R's. R's @tt{MultinomialExample} labels its classes
1, 2 and 3, and here they are 0, 1 and 2, the labels
@racket[multinomial-fit] takes. R's @tt{SparseExample} holds its predictors
in a sparse matrix, and here they are dense, until sparse input is supported
(@hyperlink["https://github.com/bkc39/glmnet/issues/11"]{#11}).

@racket[mtcars] and @racket[iris] are tables, as R's are data frames, whose
columns are vectors. Every module that uses them shares them, so the vectors
are immutable:

@examples[#:eval ev #:label #f
(table-column-names mtcars)
(vector-take (cdr (assoc "Species" iris)) 3)
]

The datasets are CSV files in the package, under
@filepath{glmnet/datasets/}, which @filepath{scripts/export-datasets.R}
writes from R. Every number is written with 17 significant digits, so each
reads back as the same double as R's, and the parity tests check every one of
them against R's own. They can be read like any other CSV file:

@examples[#:eval ev #:label #f
(require glmnet/data/csv)
(define iris-file (collection-file-path "iris.csv" "glmnet" "datasets"))
(equal? (csv-file->table iris-file) iris)
]

@section[#:tag "data-tables"]{Tables}

A @tech{table} is named columns. It can be an association list from names to
columns, a hash, or a design matrix with column names, and a column can be a
list, a vector, an @racket[flvector] or an @racket[f64vector] (see
@secref["ref-tables"]). A column of numbers is a
predictor or a numeric response, and a column of strings, symbols or booleans
is a factor, as R's character and logical columns are (see
@secref["formulas-factors"]). The formula front end fits from a table, and
@racket[table->design-matrix] takes numeric columns out of one:

@examples[#:eval ev #:label #f
(define patients
  (list (cons "age" '(34 51 67 45))
        (cons "site" '("Boston" "Portland" "Portland" "Boston"))
        (cons "dose" #(2.5 5.0 1.0 3.5))
        (cons "response" '(3.1 5.2 1.9 4.0))))
(table-column-names patients)
(design-matrix->rows (table->design-matrix patients '("age" "dose")))
(coef (formula-fit (response . ~ . age + dose + site) patients #:lambda 0.1))
]

@section[#:tag "data-nested"]{Lists and vectors}

Racket's own data needs no conversion: every fit and prediction procedure
takes rows as a list or a vector of lists or vectors, and a response as a
list, vector, @racket[flvector] or @racket[f64vector] (see
@secref["concepts-data"]). @racketmodname[glmnet/data/nested] converts the
nestings to a @racket[design-matrix?] value, by rows or by columns, to check
the data once and fit it many times or to name its columns, and converts a
design matrix back to any of them:

@margin-note{See @secref["ref-data-nested"] in the @secref["reference"] for
the two procedures.}

@examples[#:eval ev #:label #f
(require glmnet/data/nested)
(define rows (vector #(1.0 2.0) #(2.0 1.0) #(3.0 4.0) #(4.0 3.0) #(5.0 6.0)))
(define D (nested->design-matrix rows #:column-names '(x1 x2)))
(design-matrix-column-names D)
(define by-column (vector #(1.0 2.0 3.0 4.0 5.0) #(2.0 1.0 4.0 3.0 6.0)))
(equal? (nested->design-matrix by-column #:by 'columns #:column-names '("x1" "x2")) D)
(elnet-result-coefficients (lasso D (vector 1.0 4.0 3.0 6.0 5.0) #:lambda 0.1))
(design-matrix->nested D #:outer 'vector #:inner 'vector)
(design-matrix->nested D #:by 'columns #:outer 'vector #:inner 'vector)
]

The column names are kept as strings, whether they were given as strings or
symbols, so the same data named either way gives @racket[equal?] design
matrices. A design matrix with column names is a @tech{table}, from which a
formula fits by name.

@section[#:tag "data-csv"]{CSV files}

@racketmodname[glmnet/data/csv] reads a CSV file into a table and writes a
table to one. It depends on nothing beyond Racket's @tt{base} package:

@margin-note{See @secref["ref-data-csv"] in the @secref["reference"] for the
four procedures.}

@examples[#:eval ev #:label #f
(define text "site,age,response\nBoston,34,3.1\n\"Portland, ME\",51,5.2\n")
(define t (csv->table (open-input-string text)))
t
(table->csv t)
]

The first line is the header, which names the columns. A cell in quotes can
hold commas, newlines, and quotes written twice (@tt{""}), as
@hyperlink["https://www.rfc-editor.org/rfc/rfc4180"]{RFC 4180} has it. Each
cell is read as R's @tt{read.csv} reads a column that holds only that cell:

@itemlist[
 @item{a number becomes a flonum: a decimal number, a hexadecimal one such as
       @tt{0x1A}, or @tt{Inf}, @tt{Infinity} or @tt{NaN} in any case, with
       white space around it or not;}
 @item{@tt{TRUE} and @tt{T} become @racket[#t], and @tt{FALSE} and @tt{F}
       become @racket[#f];}
 @item{anything else stays a string, white space and all, so a column of
       strings works as a factor.}
]

@examples[#:eval ev #:label #f
(csv->table (open-input-string "x\n0x1A\n-inf\n 7 \nT\ntrue\n\" TRUE\"\n"))
]

Unlike R's @tt{read.csv}, which gives a whole column one type, each cell is
read on its own. A column that mixes numbers and strings stays mixed, and the
formula front end then raises an error naming the column, rather than reading
the numbers as categories.

The input must be UTF-8. A file in another encoding, such as the Windows-1252
that Excel writes on Windows, is an error that names the line and the byte
where it stops being UTF-8, rather than strings with characters replaced.

A missing cell, an empty one, one of white space or R's @tt{NA}, is an
error that names the
column, the row and the line of the file; nothing is dropped or filled in:

@examples[#:eval ev #:label #f
(eval:error (csv->table (open-input-string "x,y\n1,2\n3,\n")))
]

@racket[table->csv] writes a flonum as the shortest decimal that reads back
as the same flonum, so numbers round-trip exactly. It quotes a string when the
file needs it, and a string that would read back as a number, such as
@racket["42"], is quoted too, as R quotes every string; it still reads back
as the number, as it does in R. @racket[csv-file->table] and
@racket[table->csv-file] do the same with a file:

@examples[#:eval ev #:label #f
(require racket/file)
(define file (make-temporary-file "patients-~a.csv"))
(table->csv-file patients file #:exists 'replace)
(display (file->string file))
(csv-file->table file)
]

@racket[csv->table] reads the whole input into memory, and each column is a
vector. For large files, a data frame library that reads CSV files in native
code, such as rkt-polars, is faster.

@(close-eval ev)
