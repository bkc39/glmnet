#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-formula-factors"]{Formula factors}

@margin-note{Source: @filepath{glmnet/examples/11-formula-factors.rkt}}

Cars with more cylinders burn more fuel, but a cylinder count is a category,
not a quantity to multiply by a slope. R fits it as a @emph{factor} with
@tt{mpg ~ wt + factor(cyl)}; the formula front end writes
@racketfont{(mpg . ~ . wt + (factor cyl))} (see @secref["formulas-factors"]).
A column of strings is a factor without @racket[factor], and a multinomial
response of strings has its strings as classes.

@section[#:tag "ex-formula-factors-data"]{The data}

@racketmodname[glmnet/datasets] provides R's @tt{mtcars}: 32 cars from the
1974 Motor Trend road tests, among them their miles per gallon
(@racket["mpg"]), weight in thousands of pounds (@racket["wt"]) and number of
cylinders (@racket["cyl"]), 4, 6 or 8. It also provides R's @tt{iris}: four
measurements of 150 flowers and their species, as strings:

@examples[#:eval ev #:label #f
(require glmnet/datasets)
(table-column-names mtcars)
(table-column-names iris)
]

@section[#:tag "ex-formula-factors-cylinders"]{A factor}

@racket[(factor cyl)] codes the cylinder counts by treatment contrasts: four
cylinders, the first level, is the baseline, and six and eight get a 0/1
column each:

@examples[#:eval ev #:label #f
(define X (formula-design-matrix (mpg . ~ . wt + (factor cyl)) mtcars))
(design-matrix-column-names X)
(design-matrix->rows (design-matrix-select-rows X '(0 2 4)))
]

The first, third and fifth cars have six, four and eight cylinders.
@racket[formula-path] fits the lasso path:

@examples[#:eval ev #:label #f
(define cylinders
  (formula-path (mpg . ~ . wt + (factor cyl)) mtcars))
(formula-model-levels cylinders)
(coef cylinders #:lambda 1.0)
(coef cylinders #:lambda 0.1)
(coef cylinders #:lambda 0.01)
]

The weight enters the path first, alone. Eight cylinders follow at
@math{λ ≈ 2.45}, since the eight-cylinder cars differ most from the
four-cylinder ones, and six cylinders only at @math{λ ≈ 0.80}. At
@math{λ = 0.01} the coefficients are close to those of R's unpenalized
@tt{lm(mpg ~ wt + factor(cyl), mtcars)}, @math{34.0 − 3.21 wt − 4.26} for six
cylinders and @math{− 6.07} for eight: at the same weight, six cylinders cost
about 4 miles per gallon and eight about 6.

@section[#:tag "ex-formula-factors-slopes"]{A slope for each cylinder count}

@racket[(* wt (factor cyl))] crosses the weight with the cylinders. The
interactions are the weight in the rows of six or eight cylinders and 0 in
the others, so each is a difference between that level's slope and the
four-cylinder cars' slope:

@examples[#:eval ev #:label #f
(define slopes (formula-path (mpg . ~ . wt * (factor cyl)) mtcars))
(formula-model-predictor-names slopes)
(coef slopes #:lambda 0.01)
]

The interactions enter late, eight cylinders at @math{λ ≈ 0.059} and six at
@math{λ ≈ 0.016}. At @math{λ = 0.01}, a thousand pounds cost a four-cylinder
car 5.0 miles per gallon, a six-cylinder car @math{4.98 − 1.06 = 3.9} and an
eight-cylinder car @math{4.98 − 2.60 = 2.4}. R's @tt{lm} puts the slopes
further apart, at 5.6, 2.8 and 2.2, and the lasso has shrunk the
differences.

@section[#:tag "ex-formula-factors-new"]{New cars}

@racket[predict] codes new cars with the levels the model was fitted with.
These three have only four and eight cylinders, which is fine; a car with
five is an error that names the level, as R's "factor has new levels" is:

@examples[#:eval ev #:label #f
(define new-cars (list (cons "wt" '(2.5 3.5 3.5)) (cons "cyl" '(4 4 8))))
(predict cylinders new-cars #:lambda 0.1)
(eval:error (predict cylinders (list (cons "wt" '(3.0)) (cons "cyl" '(5)))))
]

At @math{λ = 0.1}, a thousand pounds more cost the four-cylinder car 3.3
miles per gallon, and eight cylinders instead of four another 5.6.

@section[#:tag "ex-formula-factors-species"]{Species as classes}

The species are strings, so a multinomial fit on them has the species as
classes, in R's order of levels. @racket[coef] keys the coefficients by
species, and @racket[predict] with @racket[#:type 'class] names a species for
each new flower:

@examples[#:eval ev #:label #f
(define species
  (formula-path (~ Species all) iris #:family 'multinomial))
(coef species #:lambda 0.05)
(define flowers
  (list (cons "Sepal.Length" '(5.0 6.0 6.5))
        (cons "Sepal.Width" '(3.4 2.8 3.0))
        (cons "Petal.Length" '(1.5 4.5 5.6))
        (cons "Petal.Width" '(0.2 1.4 2.1))))
(predict species flowers #:type 'class #:lambda 0.05)
(predict species flowers #:type 'response #:lambda 0.05)
]

At @math{λ = 0.05} each species keeps few measurements: short petals and wide
sepals for setosa, narrow sepals for versicolor, and wide petals for
virginica. The first flower is a setosa with probability 0.90, the second a
versicolor with 0.65 and the third a virginica with 0.82.

@section[#:tag "ex-formula-factors-r"]{Matching R}

The same fits in R are

@verbatim[#:indent 2]|{
x <- model.matrix(mpg ~ wt + factor(cyl), mtcars)[, -1]
fit <- glmnet(x, mtcars$mpg)
coef(fit, s = c(1, 0.1, 0.01))
tt <- delete.response(terms(mpg ~ wt + factor(cyl)))
new <- data.frame(wt = c(2.5, 3.5, 3.5), cyl = c(4, 4, 8))
newx <- model.matrix(tt, model.frame(tt, new,
                     xlev = list(`factor(cyl)` = c("4", "6", "8"))))[, -1]
predict(fit, newx = newx, s = 0.1)
species <- glmnet(as.matrix(iris[, 1:4]), iris$Species, family = "multinomial")
}|

and the same for @tt{mpg ~ wt * factor(cyl)}. R 4.5.3 with glmnet 4.1.10
gives the same paths, 61 λ values from @math{5.147} down to @math{0.01938}
for the cylinders, 92 down to @math{0.001083} for the slopes and 100 down to
@math{0.0000435} for the species, and the coefficients, predictions and
classes above. R names the columns @tt{factor(cyl)6} and
@tt{wt:factor(cyl)6}, which are @racket["(factor cyl)6"] and
@racket["wt:(factor cyl)6"] here. The example's companion test checks each
number against R's to six significant digits.

@(close-eval ev)
