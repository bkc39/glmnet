#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "formulas" #:style 'toc]{Formulas and named data}

The family procedures, such as @racket[elnet-fit] and @racket[logistic-path],
take a @tech{design matrix} and a separate @tech{response}, and their
coefficients are known by position. Data usually arrives as a table of named
columns instead, one of which is the response. The formula front end, which
@secref["gs-formulas"] and @secref["concepts-named"] introduce, fits a model
from such a table: a @tech{formula} names the response and the predictors, and
the fitted model keeps the names, so that its coefficients are keyed by name
and it predicts from a new table by matching columns by name. The same three procedures fit
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
 @item{a @racket[design-matrix?] with column names.}
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
the response, then the predictor terms. @racket[~] quotes its body, except for
its transforms, so the column names are written as identifiers, or as strings
when they are not
identifiers or are words of the formula language (@racket[all],
@racket[surv] and the operators @racket[+], @racket[-], @racket[*],
@racket[:] and @racket[^]):

@examples[#:eval ev #:label #f
(~ bp age dose)
(formula-predictor-names (~ bp age dose) patients)
]

Since @racket[~] quotes the names itself, a quoted name, such as
@racket['age], is a syntax error that says to write @racket[age].

The simplest terms are:

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
the second formula excludes them. A column name that is not in the table is an
error:

@examples[#:eval ev #:label #f
(eval:error (formula-predictor-names (~ bp age weight) patients))
]

The formula language also has R's operators for interactions, crossing,
powers and the intercept, written prefix as here or infix as R writes them,
which @secref["formulas-algebra"] describes, and transforms such as
@racket[(log age)], which @secref["formulas-transforms"] describes.

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

@section[#:tag "formulas-algebra"]{The formula algebra}

R's formulas have an algebra of terms, and so do these. A term is a set of
variables, the table's columns: a column name is a term of one variable, and
an @emph{interaction} of several variables is a term whose column in the
design matrix is the product of theirs. The operators build terms from terms,
as R's @tt{terms} builds them, and the design matrix has a column per term, as
R's @tt{model.matrix} builds it.

@subsection[#:tag "formulas-interactions"]{Interactions and crossing}

@racket[(: age dose)], R's @tt{age:dose}, is the interaction of age and dose,
and @racket[(* age dose)], R's @tt{age*dose}, crosses them: both main effects
and their interaction. The interaction's column is named as R names it, with
a colon:

@examples[#:eval ev #:label #f
(formula-predictor-names (~ bp (* age dose)) patients)
(define X (formula-design-matrix (~ bp (* age dose)) patients))
(design-matrix->rows (design-matrix-select-rows X '(0 1)))
]

Each row's third entry is the product of the first two. The fit is the matrix
fit of that design matrix. In the simulated data blood pressure does not
depend on the product, and the lasso leaves it out:

@examples[#:eval ev #:label #f
(coef (formula-fit (~ bp (* age dose)) patients #:lambda 0.5))
]

The operators take any terms, so sums cross and interact term by term:
@racket[(: (+ age dose) marker)] gives @racket["age:marker"] and
@racket["dose:marker"], and @racket[(* (+ age dose) marker)] adds the three
main effects. The prefix forms take more than two operands, folded from the
left, and @racket[(* age dose marker)] crosses all three:

@examples[#:eval ev #:label #f
(formula-predictor-names (~ bp (: (+ age dose) marker)) patients)
(formula-predictor-names (~ bp (* age dose marker)) patients)
]

The terms come out in R's order: main effects first, then the interactions of
two variables, then of three, each in the order they first appear. A term
written twice, as @racket[(: age dose)] and @racket[(: dose age)] are, counts
once.

@subsection[#:tag "formulas-powers"]{Powers, and R's @tt{x^2}}

@racket[(^ term n)], R's @tt{(a + b + c)^n}, crosses a sum with itself: every
interaction of at most @racket[n] of its terms. With @racket[2], that is the
main effects and every two-way interaction:

@examples[#:eval ev #:label #f
(formula-predictor-names (~ bp (^ (+ age dose marker) 2)) patients)
]

A variable crossed with itself is itself, so in a formula @tt{x^2} is not a
square. R's @tt{y ~ x + x^2} is the same model as @tt{y ~ x}, and so is
@racket[(~ bp age (^ age 2))]:

@examples[#:eval ev #:label #f
(formula-predictor-names (~ bp age (^ age 2)) patients)
]

This surprises people who come to R's formulas from algebra. R writes a
square @tt{I(x^2)}, a transform, and so does a formula here:
@racket[(I (expt age 2))], or @racket[(sqr age)] (see
@secref["formulas-squares"]).

@subsection[#:tag "formulas-removal"]{Removing terms}

@racket[(- term removed ...)] removes the terms that are equal to a term of
@racket[removed], so it can take an interaction out of a crossing or a column
out of @racket[all]. A term that is not there is ignored, as in R:

@examples[#:eval ev #:label #f
(formula-predictor-names (~ bp (- (* age dose marker) (: age dose marker))) patients)
(formula-predictor-names (~ bp (- (* age dose) dose)) patients)
(formula-predictor-names (~ bp (- (* age dose) marker)) patients)
]

@subsection[#:tag "formulas-intercept"]{The intercept}

A model has an intercept unless its formula says otherwise. @racket[0], or
removing @racket[1] with @racket[- 1], fits without one, as R's @tt{y ~ 0 + x}
and @tt{y ~ x - 1} do; @racket[1] keeps it. The coefficients still list the
intercept, at zero, as R's @tt{coef} does:

@examples[#:eval ev #:label #f
(coef (formula-fit (~ bp 0 age dose) patients #:lambda 0.5))
]

The fit procedures' @racket[#:intercept?] then defaults to the formula's
intercept. Given explicitly, it must agree with a formula that writes
@racket[1], @racket[0] or @racket[- 1], and a contradiction is an error that
names both; with a formula that writes none, it decides. The Cox family has
no intercept, and ignores intercept terms.

A formula whose terms leave no predictors, such as @racket[(~ bp 1)] or
@racket[(~ bp)], is R's intercept-only model. glmnet cannot fit it, since it
needs at least one predictor, and the formula procedures say so.

@subsection[#:tag "formulas-infix"]{Infix formulas}

The operators can also go between terms, as R writes them. The Racket
reader's infix dots make @racketfont{(bp . ~ . age * dose + marker)} the same as
@racket[(~ bp age * dose + marker)], so an R formula reads almost as written:

@examples[#:eval ev #:label #f
(define f (bp . ~ . 1 + age * dose + marker))
f
(formula-predictor-names f patients)
]

The formula prints as the @racket[~] form that the reader makes of it. The
operators have R's precedence: @racket[^] binds tightest, then a leading
@racket[-] or @racket[+], then @racket[:], then @racket[*], then @racket[+]
and @racket[-], and each groups from the left, except that a power cannot be
raised again, as in R. A parenthesized group can be infix too, and infix and
prefix forms mix:

@examples[#:eval ev #:label #f
(formula-predictor-names (bp . ~ . (age + dose + marker) ^ 2 - age : dose) patients)
(formula-predictor-names (bp . ~ . - 1 + (* age dose) + marker) patients)
]

An operator needs spaces around it. The reader reads @tt{age:dose} and
@tt{-age} as one name each, @tt{-1} as a number, and @tt{-0} as @racket[0],
dropping the sign that R reads as an operator, and a formula that uses one is
a syntax error that says so:

@examples[#:eval ev #:label #f
(eval:error (~ bp age:dose))
]

R's @tt{-0}, which in @tt{y ~ x + -0} keeps the intercept, is therefore
written @racket[(- 0)], as in @racketfont{(y . ~ . x + (- 0))}.

@subsection[#:tag "formulas-response-rhs"]{The response on the right-hand side}

A response column is not a predictor. R's @tt{model.matrix} drops the response
when the right-hand side has it as a term of its own, with a warning, and keeps
it in an interaction; so do the formula procedures, which log the warning on
the @racket['glmnet] topic. The columns of a Cox or multi-response response
are each dropped in the same way:

@examples[#:eval ev #:label #f
(formula-predictor-names (bp . ~ . age + bp + age : bp) patients)
]

@subsection[#:tag "formulas-new-data"]{Predicting from new data}

A formula model keeps the terms it was fitted with. @racket[predict] builds
their design matrix from a new table's columns, which can come in any order,
and does not expand the formula again, so @racket[all] stands for the columns
it stood for in the fit. The table needs only the columns that the terms read,
here the age and the dose:

@examples[#:eval ev #:label #f
(define crossed (formula-fit (~ bp (* age dose)) patients #:lambda 0.1))
(predict crossed (list (cons "dose" '(2.0 8.0)) (cons "age" '(40 70))))
]

A table that lacks one of them is an error that names it, as
@secref["formulas-gaussian"] shows.

@subsection[#:tag "formulas-r-syntax"]{From R's syntax}

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{R}                        @bold{Prefix}                          @bold{Infix})
       (list @tt{y ~ a + b}                  @racket[(~ y a b)]                     @racket[(~ y a + b)])
       (list @tt{y ~ a:b}                    @racket[(~ y (: a b))]                 @racket[(~ y a : b)])
       (list @tt{y ~ a*b}                    @racket[(~ y (* a b))]                 @racket[(~ y a * b)])
       (list @tt{y ~ (a + b + c)^2}          @racket[(~ y (^ (+ a b c) 2))]         @racket[(~ y (a + b + c) ^ 2)])
       (list @tt{y ~ a*b - a}                @racket[(~ y (- (* a b) a))]           @racket[(~ y a * b - a)])
       (list @tt{y ~ 0 + a}                  @racket[(~ y 0 a)]                     @racket[(~ y 0 + a)])
       (list @tt{y ~ a - 1}                  @racket[(~ y (- a 1))]                 @racket[(~ y a - 1)])
       (list @tt{y ~ .}                      @racket[(~ y all)]                     "")
       (list @tt{y ~ . - a}                  @racket[(~ y (- all a))]               @racket[(~ y all - a)])
       (list @tt{y ~ .^2}                    @racket[(~ y (^ all 2))]               @racket[(~ y all ^ 2)])
       (list @tt{Surv(time, status) ~ .}     @racket[(~ (surv time status) all)]    "")
       (list @tt{cbind(y1, y2) ~ a}          @racket[(~ (y1 y2) a)]                 ""))]

Each infix form can also be written with the reader's infix dots, as
@racketfont{(y . ~ . a * b)}. @secref["formulas-transforms"] has the
transforms, such as R's @tt{log(a)}, and @secref["formulas-r"] lists what the
formula language does not have.

@section[#:tag "formulas-transforms"]{Transforms}

A @emph{transform} is a term computed from columns, as R's @tt{log(x)} is. A
group that starts with a function, such as @racket[(log age)] or
@racket[(sqrt dose)], is one: an ordinary Racket expression, evaluated for
each row with each column name standing for the row's value. The design matrix
holds its values as a column named by its source:

@examples[#:eval ev #:label #f
(define T (formula-design-matrix (~ bp age (log age) (sqrt dose)) patients))
(design-matrix-column-names T)
(design-matrix->rows (design-matrix-select-rows T '(0 1)))
]

A transform is a variable of the formula algebra, as a column is, so it
crosses and interacts with other terms, and the interaction's column is the
product of the transform's and the other variable's:

@examples[#:eval ev #:label #f
(formula-predictor-names (~ bp (* (log age) dose)) patients)
]

@subsection[#:tag "formulas-squares"]{Squares, and R's @tt{I()}}

@racket[(I expr)] is R's @tt{I()}: the transform whose values are those of
@racket[expr], any Racket arithmetic. Inside a transform the operators are
Racket's, as they are inside R's @tt{I()} and any other function call, so
@racket[(I (* age dose))] is the product of two columns and
@racket[(log (+ dose 1))] adds 1.

This is how a formula squares a column. @racket[(^ age 2)] is crossing, and
the same as @racket[age] (see @secref["formulas-powers"]); R's
@tt{y ~ x + x^2} is @tt{y ~ x}. The square is @racket[(I (expt age 2))], as R
writes @tt{I(x^2)}, or @racket[(sqr age)], with @racket[sqr] from
@racketmodname[racket/math]:

@examples[#:eval ev #:label #f
(require racket/math)
(formula-predictor-names (bp . ~ . age + age ^ 2) patients)
(formula-predictor-names (bp . ~ . age + (sqr age)) patients)
(coef (formula-fit (bp . ~ . age + (sqr age) + dose) patients #:lambda 0.1))
]

At @math{λ = 0.1} the lasso leaves the square out, since blood pressure in
the simulated data is linear in age. @racket[^] is not a Racket function, and
a transform that uses it is a syntax error that says so:

@examples[#:eval ev #:label #f
(eval:error (~ bp age (I (^ age 2))))
]

@subsection[#:tag "formulas-transform-names"]{Names in a transform}

A transform reads names as R does, from the data first and then from the
program. An identifier in argument position, that is, anywhere but first in a
group, is the table's column of that name when the table has one, and
otherwise the Racket binding of that name where the formula is written. So a
constant of the program can scale a column:

@examples[#:eval ev #:label #f
(define reference-age 50)
(define scaled (formula-design-matrix (~ bp (I (/ age reference-age))) patients))
(design-matrix->rows (design-matrix-select-rows scaled '(0 1)))
]

In this chapter @racket[age] is also a Racket variable, the list of ages that
@racket[patients] was built from, but inside the formula it names the
column. The identifier first in a group, the function, is always the Racket
binding, even when the table has a column of that name, as R looks up a
function by name and skips a column: with a column @racket[max],
@racket[(max max x)] is the larger of that column and @racket[x], R's
@tt{pmax(max, x)}. Names that the transform binds itself, with @racket[let],
@racket[lambda] or @racket[for/sum], are its own, and so are not read from the
table, and neither are the names in quoted data, nor the names that Racket's
forms match as literals, such as @racket[cond]'s @racket[=>] and @racket[else]
and @racket[quasiquote]'s @racket[unquote]; @racket[predict] needs only the
columns that a transform reads. A name that is neither a column nor bound
is an error when the formula is fitted, which names it:

@examples[#:eval ev #:label #f
(eval:error (formula-fit (~ bp (I (* age scale)) dose) patients #:lambda 0.1))
]

At the top level, as in the REPL and in this chapter, a transform's function
must be defined before the formula, since a later definition cannot be seen
there; in a module it can be defined anywhere in the module. A name in
argument position can be defined later in both, since the transform reads it
when it runs.

A transform is elementwise: it sees one row at a time. R's @tt{scale(x)} and
@tt{x - mean(x)} read the whole column; compute such a column in the table
instead.

@subsection[#:tag "formulas-transform-values"]{Values and new data}

Each value of a transform must be a real number and finite, and an error
names the transform and the row where it is not. @racket[predict] evaluates
the transforms again on the new table's rows, so it needs the columns they
read, and it reads their Racket bindings again, as R's @tt{predict} does:

@examples[#:eval ev #:label #f
(define curved
  (formula-fit (bp . ~ . age + (sqr age) + (log dose)) patients #:lambda 0.1))
(predict curved new-patients)
(eval:error (predict curved (list (cons "age" '(40)) (cons "dose" '(0.0)))))
]

@subsection[#:tag "formulas-transform-data"]{Transforms as data}

A formula prints each transform as its source, and two formulas that write a
transform the same way are @racket[equal?]. @racket[make-formula] takes a
transform as a @racket[transform-term]: a name, the columns it reads and a
procedure of their values, which has no source to name it by:

@examples[#:eval ev #:label #f
(define log-age (transform-term "(log age)" '("age") log))
(make-formula 'bp log-age 'dose)
(equal? (make-formula 'bp log-age 'dose) (~ bp (log age) dose))
]

@subsection[#:tag "formulas-transforms-r"]{From R's transforms}

Transforms are named by their Racket source, so R's @tt{log(hp)} is the
column @racket["(log hp)"] here:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{R}             @bold{Racket}                  @bold{Column})
       (list @tt{log(x)}          @racket[(log x)]               @racket["(log x)"])
       (list @tt{log(x, 2)}       @racket[(log x 2)]             @racket["(log x 2)"])
       (list @tt{sqrt(x)}         @racket[(sqrt x)]              @racket["(sqrt x)"])
       (list @tt{exp(x)}          @racket[(exp x)]               @racket["(exp x)"])
       (list @tt{abs(x)}          @racket[(abs x)]               @racket["(abs x)"])
       (list @tt{I(x^2)}          @racket[(I (expt x 2))]        @racket["(I (expt x 2))"])
       (list ""                   @racket[(sqr x)]               @racket["(sqr x)"])
       (list @tt{I(x * z)}        @racket[(I (* x z))]           @racket["(I (* x z))"])
       (list @tt{I(x / z)}        @racket[(I (/ x z))]           @racket["(I (/ x z))"])
       (list @tt{log(x + 1)}      @racket[(log (+ x 1))]         @racket["(log (+ x 1))"])
       (list @tt{pmin(x, 10)}     @racket[(min x 10)]            @racket["(min x 10)"])
       (list @tt{log(x):z}        @racket[(: (log x) z)]         @racket["(log x):z"]))]

R's vectorized @tt{pmin} is Racket's @racket[min] here, since a transform
sees one row at a time.

A name is the source as the reader writes it, so quoted data keeps its
abbreviation, @racket['x] and not @racket[(quote x)]. A transform reads a
column's values as flonums, so the data it compares them with are flonums
too, @racket['(40.0 50.0)] and not @racket['(40 50)], which @racket[memv]
would never find:

@examples[#:eval ev #:label #f
(formula-predictor-names (~ bp (I (if (memv age '(40.0 50.0)) 1 0))) patients)
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
formula interface. A formula fit is R's @tt{glmnet} on R's
@tt{model.matrix(formula, data)} without its intercept column, with
@tt{intercept} set to the formula's. The parity tests check, on R's
@tt{mtcars} and @tt{longley}, that each formula expands to the terms R's
@tt{terms} gives, in R's order, that @racket[formula-design-matrix] is R's
model matrix, names and values, and that the fit is R's; and this package's
tests require a formula fit to be @racket[equal?] to the matrix fit of its
design matrix for every family. From R also comes how the results are named:
R's @tt{coef} labels the intercept @tt{(Intercept)} and each predictor by its
column name, @tt{wt:hp} for an interaction, and names the elements of a
multinomial or multi-response result by class or response. The one change is
a transform's name, its Racket source: R's @tt{log(hp)}, @tt{I(hp^2)} and
@tt{log(hp):wt} are @racket["(log hp)"], @racket["(I (expt hp 2))"] or
@racket["(sqr hp)"], and @racket["(log hp):wt"] here, and the parity tests map
R's names to these before they compare them.

The formula language is smaller than R's:

@itemlist[
 @item{Every variable is a numeric column or a transform of numeric columns.
       There are no factors to expand into indicator columns.}
 @item{A transform is evaluated one row at a time, where R evaluates it on
       whole columns. R's transforms that read the whole column, such as
       @tt{scale(x)}, @tt{x - mean(x)} and @tt{poly(x, 2)}, the orthogonal
       polynomials, have no counterpart; add such columns to the table
       instead. R's @tt{poly(x, 2, raw = TRUE)} is @racket[x] and
       @racket[(sqr x)].}
 @item{A transform's names are resolved on the table it is fitted to, and
       @racket[predict] reads the same columns of a new table. R resolves
       them again on the new data, where a name that the new data lacks
       falls back on a variable of the same name.}
 @item{The response is a column, not a transform: R's @tt{log(y) ~ x} has no
       counterpart, and @racket[(~ (log y) x)] is a syntax error that says
       so; add the column to the table. A response of several columns whose
       first name is a function where the formula is written reads as such
       a transform, so write those columns as strings.}
 @item{R's @tt{.} is written @racket[all]. R's @tt{%in%} and @tt{/}
       (nesting) are syntax errors that say the language does not have them;
       for @tt{/}, the error says to write a ratio as
       @racket[(I (/ x z))], R's @tt{I(x / z)}. @tt{offset()} has no
       counterpart.}
 @item{An operator needs spaces around it, since the reader reads
       @tt{wt:hp} as one name and @tt{-0} as @racket[0], and a power is an
       exact integer of at least 2, where R truncates @tt{x^2.5} to @tt{x^2}.}
 @item{A column keeps the table's name in the design matrix, where R's
       @tt{model.matrix} puts backticks around a name that R's syntax does
       not allow, such as @tt{@literal{`blood pressure`}}. So a column named
       @racket["wt:hp"] beside the interaction of @racket[wt] and @racket[hp],
       which R names @tt{@literal{`wt:hp`}} and @tt{wt:hp}, gives two columns
       of the same name here, which is an error, since @racket[coef] keys the
       coefficients by name; and so is a column named
       @racket["(Intercept)"], the intercept's name.}
 @item{The columns of a @racket[(surv time status)] or multi-column response
       are each dropped from the right-hand side, as R drops a one-column
       response. R's response there is the one variable @tt{Surv(time, status)}
       or @tt{cbind(y1, y2)}, so R would keep @tt{time} as a predictor.}
 @item{A missing or non-finite value is an error that names its column and
       row, where R's default @tt{na.action} would drop the row. So is a
       transform's value that is not a finite real, which names the
       transform: R drops a row where @tt{log(x)} is @tt{NaN} and keeps one
       where it is @tt{-Inf}.}
]

@(close-eval ev)
