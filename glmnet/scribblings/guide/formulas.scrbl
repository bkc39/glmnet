#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "formulas" #:style 'toc]{Formulas and named data}

The procedures of the previous chapters take a @tech{design matrix} and a
separate @tech{response}, and their coefficients are known by position. Data
usually arrives as a table of named columns instead, one of which is the
response. The formula front end fits a model from such a table: a
@tech{formula} names the response and the predictors, and the fitted model
keeps the names, so that its coefficients are keyed by name and it predicts
from a new table by matching columns by name. The same three procedures fit
every family: @racket[formula-fit] at one @math{λ}, @racket[formula-path] along
a @tech{regularization path} and @racket[formula-cv] with cross-validation.

R's @tt{glmnet} itself has no formula interface; the R package
@hyperlink["https://cran.r-project.org/package=glmnetUtils"]{glmnetUtils}
adds one, which this front end resembles (see @secref["formulas-r"]).

@local-table-of-contents[]

@section[#:tag "formulas-tables"]{Tables}

A @tech{table} is any of these:

@itemlist[
 @item{an association list of @racket[(name . column)] pairs;}
 @item{a hash from name to column;}
 @item{a @racket[design-matrix?] with column names, such as a frame
       converted by an adapter.}
]

A name is a string or a symbol, and a column a list or vector of reals. Names
are compared as strings, so the symbol @racket['age] and the string
@racket["age"] name the same column. The examples in this chapter use a table
of 60 simulated patients: an @racket["age"], a @racket["dose"] and a
@racket["marker"], and the responses of three models. Blood pressure,
@racket["bp"], depends on age and dose; a relapse, @racket["relapse"] (0 or
1), on dose and the marker; and a survival time, @racket["time"] with its
event indicator @racket["status"], on age and the marker:

@examples[#:eval ev #:label #f
(random-seed 1)
(define (uniform a b) (+ a (* (- b a) (random))))
(define n 60)
(define age (for/list ([i (in-range n)]) (+ 30 (random 51))))
(define dose (for/list ([i (in-range n)]) (uniform 0 10)))
(define marker (for/list ([i (in-range n)]) (uniform -1 1)))
(define bp
  (for/list ([a (in-list age)] [d (in-list dose)])
    (+ 90 (* 0.6 a) (* -2 d) (uniform -5 5))))
(define relapse
  (for/list ([d (in-list dose)] [m (in-list marker)])
    (if (> (+ 1 (* -0.4 d) (* 3 m) (uniform -1 1)) 0) 1 0)))
(define event-times
  (for/list ([a (in-list age)] [m (in-list marker)])
    (/ (- (log (random))) (exp (+ (* 0.04 (- a 55)) (* 1.5 m))))))
(define censor-times (for/list ([i (in-range n)]) (uniform 0 4)))
(define patients
  (list (cons "age" age)
        (cons "dose" dose)
        (cons "marker" marker)
        (cons "bp" bp)
        (cons "relapse" relapse)
        (cons "time" (map min event-times censor-times))
        (cons "status" (for/list ([e (in-list event-times)]
                                  [c (in-list censor-times)])
                         (if (<= e c) 1 0)))))
(table-column-names patients)
]

@racket[table->design-matrix] converts named columns to a design matrix, which
is what the formula front end does before it fits:

@examples[#:eval ev #:label #f
(define D (table->design-matrix patients '("age" dose)))
D
(design-matrix-column-names D)
]

Only the columns a model reads must hold numbers, so a table can carry other
columns, such as an identifier or a label.

@section[#:tag "formulas-language"]{Formulas}

A @tech{formula} is written with @racket[~], as R writes @tt{y ~ x1 + x2}:
the response, then the predictor terms. @racket[~] quotes its body, so the
column names are written as identifiers, or as strings when they are not
identifiers:

@examples[#:eval ev #:label #f
(~ bp age dose)
(formula-predictor-names (~ bp age dose) patients)
]

A term is one of:

@itemlist[
 @item{a column name, which selects that column;}
 @item{@racket[all], every column that is not a response, in the table's
       order (R's @tt{.});}
 @item{@racket[(+ term ...)], the columns of each term, in order;}
 @item{@racket[(- term excluded ...)], the columns of @racket[term] except
       those of the @racket[excluded] terms (R's @tt{-}).}
]

Several terms after the response are joined as by @racket[+], and a column
selected twice counts once. @racket[formula-predictor-names] shows what a
formula selects from a table:

@examples[#:eval ev #:label #f
(formula-predictor-names (~ bp all) patients)
(formula-predictor-names (~ bp (- all relapse time status)) patients)
(formula-predictor-names (~ bp (+ marker age) age) patients)
]

@racket[all] leaves out the response but not the table's other responses, so
the second formula excludes them. A column name that is not in the table, or
a response column added as a predictor, is an error:

@examples[#:eval ev #:label #f
(eval:error (formula-predictor-names (~ bp age weight) patients))
(eval:error (formula-predictor-names (~ bp age bp) patients))
]

The response is one column for most families. For the Cox family it is
@racket[(surv time status)], as R's @tt{Surv(time, status)}, and for the
multi-response Gaussian family a list of columns, as R's
@tt{cbind(y1, y2)}. Their columns are left out of @racket[all] too:

@examples[#:eval ev #:label #f
(formula-predictor-names (~ (surv time status) (- all bp relapse)) patients)
]

A formula is a value. It prints as the @racket[~] form that makes it, and
@racket[make-formula] builds one from data, for names that are only known when
the program runs:

@examples[#:eval ev #:label #f
(define predictors '("age" "marker"))
(apply make-formula "bp" predictors)
]

@section[#:tag "formulas-gaussian"]{A Gaussian fit}

@racket[formula-fit] resolves the formula against the table and fits the
family that @racket[#:family] names, the Gaussian by default, with the same
keywords as that family's fit procedure. The model prints as the fit does,
with the formula after the family:

@examples[#:eval ev #:label #f
(define bp-fit (formula-fit (~ bp age dose marker) patients #:lambda 0.5))
bp-fit
]

@racket[coef] returns an association list keyed by name, with the intercept
first under R's name for it, @racket["(Intercept)"]. The lasso has found the
two predictors that blood pressure depends on:

@examples[#:eval ev #:label #f
(coef bp-fit)
(cdr (assoc "dose" (coef bp-fit)))
]

@racket[predict] takes a new table and reads the predictors from it by name.
The columns can come in any order, and other columns are ignored; a missing
predictor is an error that names it:

@examples[#:eval ev #:label #f
(define new-patients
  (list (cons "id" '(101 102))
        (cons "marker" '(0.5 -0.5))
        (cons "dose" '(2.0 8.0))
        (cons "age" '(40 70))))
(predict bp-fit new-patients)
(eval:error (predict bp-fit (list (cons "age" '(40)) (cons "dose" '(2.0)))))
]

@racket[formula-path] fits a @tech{regularization path}, and
@racket[formula-cv] cross-validates one, with the keywords of the family's
path fitter and cross-validation procedure. Both keep the names:

@examples[#:eval ev #:label #f
(formula-path (~ bp age dose marker) patients #:lambda '(4.0 1.0 0.25))
(define bp-cv (formula-cv (~ bp age dose marker) patients))
bp-cv
(coef bp-cv)
(coef bp-cv #:lambda 'lambda-min)
]

The model is a @racket[formula-model]. @racket[formula-model-fit] returns the
result it holds, which is exactly what the family's procedure returns for the
same columns as a matrix:

@examples[#:eval ev #:label #f
(formula-model-fit bp-fit)
(equal? (formula-model-fit bp-fit)
        (lasso (map list age dose marker) bp #:lambda 0.5))
]

@section[#:tag "formulas-binomial"]{A binomial fit}

With @racket[#:family 'binomial], the response column holds 0/1 labels. Here
the formula takes every column except the other responses:

@examples[#:eval ev #:label #f
(define relapse-cv
  (formula-cv (~ relapse (- all bp time status)) patients
              #:family 'binomial))
relapse-cv
(coef relapse-cv)
(predict relapse-cv new-patients #:type 'response)
(predict relapse-cv new-patients #:type 'class)
]

At @tech{lambda-1se}, the model keeps the dose and the marker, the two
predictors that the relapse depends on.

@section[#:tag "formulas-cox"]{A Cox fit}

With @racket[#:family 'cox], the response is @racket[(surv time status)]. A Cox
model has no intercept, so its coefficients are the predictors' alone, and
@racket[#:type 'response] predicts the relative risk:

@examples[#:eval ev #:label #f
(define survival-cv
  (formula-cv (~ (surv time status) age dose marker) patients
              #:family 'cox))
survival-cv
(coef survival-cv)
(coef survival-cv #:lambda 'lambda-min)
(predict survival-cv new-patients #:type 'response #:lambda 'lambda-min)
]

At @tech{lambda-1se}, the model keeps only the marker, the stronger of the two
predictors that the survival time depends on; at @tech{lambda-min} it adds
age. Both drop the dose.

@section[#:tag "formulas-groups"]{Multinomial and multi-response fits}

For the multinomial family, @racket[coef] has one association list per class,
keyed by the class label, and for the multi-response family one per response,
keyed by the response's name, as R names the elements of its lists:

@examples[#:eval ev #:label #f
(define level
  (for/list ([b (in-list bp)])
    (cond [(< b 110) 0] [(< b 120) 1] [else 2])))
(define pulse
  (for/list ([a (in-list age)])
    (+ 60 (* 0.2 a) (uniform -3 3))))
(define more-patients
  (list* (cons "level" level) (cons "pulse" pulse) patients))
(coef (formula-fit (~ level age dose) more-patients
                   #:family 'multinomial #:lambda 0.05))
(coef (formula-fit (~ (bp pulse) age dose marker) more-patients
                   #:family 'mgaussian #:lambda 0.5))
]

@section[#:tag "formulas-plots"]{Plots}

The plots of @racketmodname[glmnet/plot] accept a formula model whose fit is
a path or a cross-validated path. With @racket[#:label #t], they label each
curve with its predictor's name; see @secref["plot-path-names"].

@section[#:tag "formulas-r"]{Matching R}

R's @tt{glmnet} takes a matrix @tt{x} and a response @tt{y}, and has no
formula interface. The formula front end therefore has no numbers of its own:
a formula fit is the matrix fit of the columns the formula selects, which the
parity tests compare with R, and its tests require the two to be
@racket[equal?] for every family. What it takes from R is how the results are
named: R's @tt{coef} labels the intercept @tt{(Intercept)} and each predictor
by its column name, and names the elements of a multinomial or multi-response
result by class or response. The parity tests also check those names against
R's.

The formula language is smaller than R's:

@itemlist[
 @item{Every predictor is a numeric column. There are no factors to expand
       into indicator columns, no interactions (@tt{x1:x2}, @tt{x1 * x2}) and
       no transformations such as @tt{log(x)} or @tt{I(x^2)}; add such columns
       to the table instead.}
 @item{R's @tt{.} is written @racket[all], and @tt{- 1} is not a term: use
       @racket[#:intercept? #f].}
 @item{A missing or non-finite value is an error that names its column and
       row, where R's default @tt{na.action} would drop the row.}
]

@(close-eval ev)
