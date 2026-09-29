#lang scribble/lp2

@(require (for-label racket/base
                     glmnet
                     glmnet/examples/data/mtcars))

@section[#:tag "ex-formula-interactions"]{Formula interactions}

An @emph{interaction} lets the effect of one predictor depend on another. In
R's formula @tt{mpg ~ wt * hp}, fuel economy falls with a car's weight and with
its horsepower, and the product @tt{wt:hp} lets the cost of each extra ton
depend on the engine. The formula front end writes the same formula
@racket[(~ mpg (* wt hp))], or with R's infix operators
@racketfont{(mpg . ~ . wt * hp)}. Both expand, as R's @tt{terms} does, into the
columns @racket["wt"], @racket["hp"] and @racket["wt:hp"], the last one the
product of the other two.

The data are R's @tt{mtcars}: 32 cars from the 1974 Motor Trend road tests,
which @racketmodname[glmnet/examples/data/mtcars] provides as a table.

@chunk[<require>
(require glmnet
         glmnet/examples/data/mtcars)]

@chunk[<provide>
(provide run-example)]

A lasso path over the crossed formula. Weight enters first, then briefly the
interaction, standing in for horsepower until horsepower enters at
@math{λ ≈ 2.7}. The interaction comes back below @math{λ ≈ 0.10}, positive:
at @math{λ = 0.5} its coefficient is @racket[0.0], and at @math{λ = 0.01} it is
about @racket[0.025].

@chunk[<crossing>
(define path (formula-path (~ mpg (* wt hp)) mtcars))]

The infix spelling is the same formula, and fits the same path:

@chunk[<infix>
(define infix-path (formula-path (mpg . ~ . wt * hp) mtcars))]

R's @tt{(wt + hp + qsec)^2} crosses a sum with itself: the three main effects
and the three two-way interactions, in R's order.

@chunk[<pairs>
(define pairs-path (formula-path (~ mpg (^ (+ wt hp qsec) 2)) mtcars))]

Cross-validation over four fixed folds, as R's
@tt{cv.glmnet(x, y, foldid = rep(1:4, length.out = 32))} runs it, keeps the
interaction at both of the λ values it chooses.

@chunk[<cv>
(define folds (for/list ([i (in-range 32)]) (modulo i 4)))
(define cv (formula-cv (~ mpg (* wt hp)) mtcars #:fold-ids folds))]

@chunk[<run-example>
(define (run-example)
  <crossing>
  <infix>
  <pairs>
  <cv>
  (values path infix-path pairs-path cv))]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
