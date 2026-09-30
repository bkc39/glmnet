#lang scribble/lp2

@(require (for-label racket/base
                     racket/math
                     glmnet
                     glmnet/datasets))

@section[#:tag "ex-formula-polynomial"]{Formula transforms}

Fuel economy falls with horsepower, but not along a straight line: an extra
horsepower costs a small car more miles per gallon than a powerful one. R fits
the bend with a quadratic, @tt{mpg ~ hp + I(hp^2)}. The formula front end
writes @racketfont{(mpg . ~ . hp + (sqr hp))}: @racket[(sqr hp)] is a
@emph{transform}, a Racket function of the column, which the design matrix
holds as a column named @racket["(sqr hp)"].

The trap is R's @tt{mpg ~ hp + hp^2}. In a formula, @tt{^} crosses terms, and
@tt{hp} crossed with itself is @tt{hp}, so the formula is @tt{mpg ~ hp}; the
same holds for @racketfont{(mpg . ~ . hp + hp ^ 2)}.

A transform can be any function: R's @tt{mpg ~ log(hp) + wt}, the logarithm of
horsepower and the weight, is @racketfont{(mpg . ~ . (log hp) + wt)}.

The data are R's @tt{mtcars}, which
@racketmodname[glmnet/datasets] provides as a table.
@racket[sqr] is @racketmodname[racket/math]'s.

@chunk[<require>
(require racket/math
         glmnet
         glmnet/datasets)]

@chunk[<provide>
(provide run-example)]

The quadratic's lasso path. Horsepower enters first, and the square only at
@math{λ ≈ 0.26}; at @math{λ = 0.01} the coefficients are close to R's
@tt{lm}, @math{40.4 − 0.213 hp + 0.00042 hp²}.

@chunk[<quadratic>
(define quadratic (formula-path (mpg . ~ . hp + (sqr hp)) mtcars))]

R's @tt{x^2} is not a square: this formula has the one predictor
@racket["hp"].

@chunk[<trap>
(define trap (formula-path (mpg . ~ . hp + hp ^ 2) mtcars))]

The logarithm of horsepower with the weight fits better than horsepower
itself does: at the end of the path the deviance ratio is @math{0.859}, where
@tt{mpg ~ hp + wt} reaches @math{0.827}.

@chunk[<log>
(define log-path (formula-path (mpg . ~ . (log hp) + wt) mtcars))]

@racket[predict] computes the transforms again from the new table's columns.
The quadratic flattens out: from 100 to 200 horsepower it loses 8.6 miles per
gallon at @math{λ = 0.01}, and from 200 to 300 only half a mile.

@chunk[<new-data>
(define new-cars (list (cons "hp" '(100 200 300)) (cons "wt" '(2.5 3.5 4.0))))
(define curve (predict quadratic new-cars #:lambda 0.01))
(define log-predictions (predict log-path new-cars #:lambda 0.1))]

@chunk[<run-example>
(define (run-example)
  <quadratic>
  <trap>
  <log>
  <new-data>
  (values quadratic trap log-path curve log-predictions))]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
