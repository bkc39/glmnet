#lang scribble/lp2

@(require (for-label racket/base
                     glmnet
                     glmnet/examples/data/mtcars
                     glmnet/examples/data/iris))

@section[#:tag "ex-formula-factors"]{Formula factors}

Cars with more cylinders burn more fuel, but a cylinder count is a category,
not a quantity to multiply by a slope. R fits it as a @emph{factor} with
@tt{mpg ~ wt + factor(cyl)}, and the formula front end writes
@racketfont{(mpg . ~ . wt + (factor cyl))}. The design matrix codes the factor
as R's @tt{model.matrix} does, by treatment contrasts: the first level, four
cylinders, is the baseline, and each other level gets a column that is 1 for
the cars at that level and 0 for the others, named @racket["(factor cyl)6"]
and @racket["(factor cyl)8"], where R names them @tt{factor(cyl)6} and
@tt{factor(cyl)8}. A coefficient is then the difference from four-cylinder
cars of the same weight.

A column of strings is a factor without @racket[factor]. R's @tt{iris} names
each flower's species, and @racketfont{(~ Species all)} fits a multinomial
model whose classes are the three species.

The data are R's @tt{mtcars} and @tt{iris}, which
@racketmodname[glmnet/examples/data/mtcars] and
@racketmodname[glmnet/examples/data/iris] provide as tables.

@chunk[<require>
(require glmnet
         glmnet/examples/data/mtcars
         glmnet/examples/data/iris)]

@chunk[<provide>
(provide run-example)]

The lasso path of fuel economy on the weight and the cylinders. The weight
enters first, eight cylinders at @math{λ ≈ 2.45} and six cylinders at
@math{λ ≈ 0.80}. At @math{λ = 0.01} the coefficients are close to R's
@tt{lm}: at the same weight, six cylinders cost 4.2 miles per gallon and
eight cylinders 6.0.

@chunk[<cylinders>
(define cylinders (formula-path (mpg . ~ . wt + (factor cyl)) mtcars))]

@racket[(* wt (factor cyl))] crosses the weight with the cylinders, which
gives each cylinder count its own slope. The interactions enter late, eight
cylinders at @math{λ ≈ 0.059} and six at @math{λ ≈ 0.016}: at
@math{λ = 0.01} a thousand pounds cost a four-cylinder car 5.0 miles per
gallon, a six-cylinder car 3.9 and an eight-cylinder car 2.4.

@chunk[<slopes>
(define slopes (formula-path (mpg . ~ . wt * (factor cyl)) mtcars))]

The model keeps the levels it was fitted with, as R's @tt{xlevels}, and
@racket[predict] codes new cars with them. The new cars need not have every
level, but a level that the model was not fitted with, such as five
cylinders, is an error that names it.

@chunk[<new-cars>
(define new-cars (list (cons "wt" '(2.5 3.5 3.5)) (cons "cyl" '(4 4 8))))
(define predictions (predict cylinders new-cars #:lambda 0.1))
(define unseen
  (with-handlers ([exn:fail? exn-message])
    (predict cylinders (list (cons "wt" '(3.0)) (cons "cyl" '(5))))))]

The species are strings, so the multinomial path's classes are the species,
in R's order of levels: @racket[coef] keys the coefficients by species, and
@racket[predict] with @racket[#:type 'class] names one for each new flower.

@chunk[<species>
(define species (formula-path (~ Species all) iris #:family 'multinomial))
(define flowers
  (list (cons "Sepal.Length" '(5.0 6.0 6.5))
        (cons "Sepal.Width" '(3.4 2.8 3.0))
        (cons "Petal.Length" '(1.5 4.5 5.6))
        (cons "Petal.Width" '(0.2 1.4 2.1))))
(define classes (predict species flowers #:type 'class #:lambda 0.05))]

@chunk[<run-example>
(define (run-example)
  <cylinders>
  <slopes>
  <new-cars>
  <species>
  (values cylinders slopes predictions unseen species classes))]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
