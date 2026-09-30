#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-formula-polynomial"]{Formula transforms}

@margin-note{Source: @filepath{glmnet/examples/10-formula-polynomial.rkt}}

Fuel economy falls with horsepower, but not along a straight line: an extra
horsepower costs a small car more miles per gallon than a powerful one. R fits
the bend with a quadratic, @tt{mpg ~ hp + I(hp^2)}; the formula front end
writes @racketfont{(mpg . ~ . hp + (sqr hp))}, where @racket[(sqr hp)] is a
@emph{transform}, a Racket function of the column (see
@secref["formulas-transforms"]).

@section[#:tag "ex-formula-polynomial-data"]{The data}

@racketmodname[glmnet/datasets] provides R's @tt{mtcars}: 32 cars from the
1974 Motor Trend road tests, among them their miles per gallon
(@racket["mpg"]), horsepower (@racket["hp"]) and weight in thousands of pounds
(@racket["wt"]). @racket[sqr] is @racketmodname[racket/math]'s:

@examples[#:eval ev #:label #f
(require racket/math glmnet/datasets)
(table-column-names mtcars)
]

@section[#:tag "ex-formula-polynomial-quadratic"]{A quadratic}

@racket[(sqr hp)] is a column of the design matrix, the square of each car's
horsepower, named by its source:

@examples[#:eval ev #:label #f
(define X (formula-design-matrix (mpg . ~ . hp + (sqr hp)) mtcars))
(design-matrix-column-names X)
(design-matrix->rows (design-matrix-select-rows X '(0 1 2)))
]

@racket[formula-path] fits the lasso path on it:

@examples[#:eval ev #:label #f
(define quadratic (formula-path (mpg . ~ . hp + (sqr hp)) mtcars))
(coef quadratic #:lambda 1.0)
(coef quadratic #:lambda 0.1)
(coef quadratic #:lambda 0.01)
]

Horsepower enters the path first, alone. The square follows only at
@math{λ ≈ 0.26}, with a positive coefficient, which bends the line up: at
@math{λ = 0.01} the coefficients are close to those of R's unpenalized
@tt{lm(mpg ~ hp + I(hp^2), mtcars)}, @math{40.41 − 0.2133 hp + 0.000421 hp²},
whose lowest point is at about 253 horsepower. R's @tt{I(hp^2)} is the same
column, and @racket[(I (expt hp 2))] writes it as R does:

@examples[#:eval ev #:label #f
(define as-in-r (formula-path (mpg . ~ . hp + (I (expt hp 2))) mtcars))
(formula-model-predictor-names as-in-r)
(equal? (formula-model-fit as-in-r) (formula-model-fit quadratic))
]

@section[#:tag "ex-formula-polynomial-trap"]{R's @tt{hp^2}}

In a formula @tt{^} crosses terms, and @tt{hp} crossed with itself is
@tt{hp}, so R's @tt{mpg ~ hp + hp^2} is the linear model @tt{mpg ~ hp}.
@racket[^] is crossing here too:

@examples[#:eval ev #:label #f
(define trap (formula-path (mpg . ~ . hp + hp ^ 2) mtcars))
(formula-model-predictor-names trap)
(equal? (formula-model-fit trap) (formula-model-fit (formula-path (~ mpg hp) mtcars)))
]

R's @tt{glmnet} cannot fit this one, since it needs at least two columns;
@racket[formula-path] fits the one predictor.

@section[#:tag "ex-formula-polynomial-log"]{A logarithm}

Any function is a transform. R's @tt{mpg ~ log(hp) + wt} is
@racketfont{(mpg . ~ . (log hp) + wt)}:

@examples[#:eval ev #:label #f
(define log-path (formula-path (mpg . ~ . (log hp) + wt) mtcars))
(coef log-path #:lambda 0.1)
]

At the end of the path the logarithm of horsepower and the weight explain
@math{85.9%} of the deviance, where horsepower itself and the weight,
@racketfont{(mpg . ~ . hp + wt)}, explain @math{82.7%}:

@examples[#:eval ev #:label #f
(define (final-deviance-ratio model)
  (define ratios (deviance-ratio model))
  (vector-ref ratios (sub1 (vector-length ratios))))
(final-deviance-ratio log-path)
(final-deviance-ratio (formula-path (mpg . ~ . hp + wt) mtcars))
]

@section[#:tag "ex-formula-polynomial-new"]{New data}

@racket[predict] evaluates the transforms again, on the new table's columns:

@examples[#:eval ev #:label #f
(define new-cars (list (cons "hp" '(100 200 300)) (cons "wt" '(2.5 3.5 4.0))))
(predict quadratic new-cars #:lambda 0.01)
(predict log-path new-cars #:lambda 0.1)
]

The quadratic flattens out: from 100 to 200 horsepower it predicts 8.6 miles
per gallon fewer, and from 200 to 300 only half a mile fewer. At
@math{λ = 0.01} its curve bottoms out near 257 horsepower and rises after it,
which is the quadratic's shape more than the cars': only two of the 32 cars
have more horsepower. The quadratic reads only @racket["hp"] from the new
table, and the logarithm model both columns.

@section[#:tag "ex-formula-polynomial-r"]{Matching R}

The same fits in R are

@verbatim[#:indent 2]|{
x <- model.matrix(mpg ~ hp + I(hp^2), mtcars)[, -1]
fit <- glmnet(x, mtcars$mpg)
coef(fit, s = c(1, 0.1, 0.01))
new <- data.frame(hp = c(100, 200, 300), wt = c(2.5, 3.5, 4))
predict(fit, newx = model.matrix(~ hp + I(hp^2), new)[, -1], s = 0.01)
}|

and the same for @tt{mpg ~ log(hp) + wt}. R 4.5.3 with glmnet 4.1.10 gives
the same paths, 78 λ values from @math{4.604} down to @math{0.003565} for the
quadratic and 55 from @math{5.147} down to @math{0.03386} for the logarithm,
and the coefficients and predictions above. The example's companion test
checks each against R's numbers to six significant digits.

@(close-eval ev)
