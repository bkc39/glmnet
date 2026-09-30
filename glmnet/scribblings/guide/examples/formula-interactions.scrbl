#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-formula-interactions"]{Formula interactions}

@margin-note{Source: @filepath{glmnet/examples/09-formula-interactions.rkt}}

An @emph{interaction} lets the effect of one predictor depend on another. A
classic one is in R's @tt{mtcars}: a car's fuel economy falls with its weight
and with its horsepower, and the product of the two lets the cost of each extra
thousand pounds depend on the engine. R writes the model @tt{mpg ~ wt * hp};
the formula front end writes @racket[(~ mpg (* wt hp))], or
@racketfont{(mpg . ~ . wt * hp)} (see @secref["formulas-algebra"]).

@section[#:tag "ex-formula-interactions-data"]{The data}

@racketmodname[glmnet/datasets] provides R's @tt{mtcars} as a table: 32 cars
from the 1974 Motor Trend road tests, with R's eleven columns,
among them miles per gallon (@racket["mpg"]), weight in thousands of pounds
(@racket["wt"]), horsepower (@racket["hp"]) and the quarter-mile time in
seconds (@racket["qsec"]):

@examples[#:eval ev #:label #f
(require glmnet/datasets)
(table-column-names mtcars)
]

@section[#:tag "ex-formula-interactions-cross"]{Crossing weight and horsepower}

@racket[(* wt hp)] crosses the two columns: both main effects and their
interaction, whose column is their product, named @racket["wt:hp"] as R names
it:

@examples[#:eval ev #:label #f
(define X (formula-design-matrix (~ mpg (* wt hp)) mtcars))
(design-matrix-column-names X)
(design-matrix->rows (design-matrix-select-rows X '(0 1 2)))
]

@racket[formula-path] fits the lasso path on that design matrix:

@examples[#:eval ev #:label #f
(define path (formula-path (~ mpg (* wt hp)) mtcars))
(formula-model-predictor-names path)
(coef path #:lambda 0.5)
(coef path #:lambda 0.01)
]

Weight enters the path first. The interaction follows it, with a small negative
coefficient, standing in for horsepower, which is still out; once horsepower
enters, at @math{λ ≈ 2.7}, the interaction leaves again. At @math{λ = 0.5} the
lasso keeps weight and horsepower and leaves the interaction at zero. The
interaction comes back below @math{λ ≈ 0.10}, now positive, and at
@math{λ = 0.01} its coefficient is about @math{0.025}: each extra thousand
pounds costs less fuel economy in a more powerful car. Weight and horsepower
then have larger negative coefficients, which the positive interaction
offsets. The plot of the path (@secref["plot-path"]) shows where the
interaction comes back:

@examples[#:eval ev #:label #f
(require glmnet/plot)
(plot-coefficient-path path #:label #t)
]

Near @math{−log λ = 2.3}, where the count along the top goes from 2 to 3, the
weight's curve turns sharply down, as the interaction takes over part of its
effect. The interaction's own curve stays close to zero on this scale, since
its column, the product of weight and horsepower, is over a hundred times
larger than the weight's.

The infix spelling is the same formula, so it fits the same path:

@examples[#:eval ev #:label #f
(mpg . ~ . wt * hp)
(equal? (formula-model-fit (formula-path (mpg . ~ . wt * hp) mtcars))
        (formula-model-fit path))
]

@section[#:tag "ex-formula-interactions-pairs"]{Every pair}

@racket[(^ (+ wt hp qsec) 2)], R's @tt{(wt + hp + qsec)^2}, crosses the sum
with itself: the three main effects, then the three two-way interactions, in
R's order.

@examples[#:eval ev #:label #f
(define pairs-path (formula-path (~ mpg (^ (+ wt hp qsec) 2)) mtcars))
(formula-model-predictor-names pairs-path)
(coef pairs-path #:lambda 0.1)
]

At @math{λ = 0.1} the lasso keeps weight, the quarter-mile time and all three
interactions, and drops horsepower's main effect: horsepower acts only through
its interactions.

@section[#:tag "ex-formula-interactions-cv"]{Choosing λ by cross-validation}

@racket[formula-cv] cross-validates the crossed model. Four fixed folds, cars
0, 4, 8, … in the first, make the result the one R's
@tt{cv.glmnet(x, y, foldid = rep(1:4, length.out = 32))} gives:

@examples[#:eval ev #:label #f
(define folds (for/list ([i (in-range 32)]) (modulo i 4)))
(define cv (formula-cv (mpg . ~ . wt * hp) mtcars #:fold-ids folds))
cv
(coef cv)
]

Both chosen λ values are below @math{0.10}, where the interaction comes back,
so both keep all three terms: every λ at which the interaction is out has a
cross-validated error more than one standard error above the smallest. The
model predicts from a new table, whose columns can come in any order; it
computes the interaction itself:

@examples[#:eval ev #:label #f
(define new-cars (list (cons "hp" '(100 200)) (cons "wt" '(2.5 3.5))))
(predict cv new-cars)
]

@section[#:tag "ex-formula-interactions-r"]{Matching R}

The same fits in R are

@verbatim[#:indent 2]|{
x <- model.matrix(mpg ~ wt * hp, mtcars)[, -1]
fit <- glmnet(x, mtcars$mpg)
coef(fit, s = 0.5)
cv <- cv.glmnet(x, mtcars$mpg, foldid = rep(1:4, length.out = 32))
}|

and R 4.5.3 with glmnet 4.1.10 gives the same path of 83 λ values, from
@math{5.147} down to @math{0.002503}, the coefficients above, and
@tt{lambda.min} @math{0.002503} and @tt{lambda.1se} @math{0.04477}. The
example's companion test checks each against R's numbers to six significant
digits.

@(close-eval ev)
