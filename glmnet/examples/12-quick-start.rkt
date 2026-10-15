#lang scribble/lp2

@(require (for-label racket/base
                     racket/dict
                     glmnet
                     glmnet/plot
                     glmnet/datasets))

@section[#:tag "ex-quick-start"]{Quick start}

This is the Quick Start of R glmnet's vignette,
@hyperlink["https://glmnet.stanford.edu/articles/glmnet.html#quick-start"]{An
Introduction to glmnet}, step by step, on the vignette's own data: fit a lasso
path, plot it, print it, read its coefficients and predictions at a
@math{λ}, then choose @math{λ} by cross-validation. Each step names the R
call it follows.

The data are R glmnet's @tt{QuickStartExample}, 100 observations of 20
predictors and a numeric response, which @racketmodname[glmnet/datasets]
loads as R's @tt{data(QuickStartExample)} does. R's
@tt{x <- QuickStartExample$x; y <- QuickStartExample$y} is one call:

@chunk[<require>
(require racket/dict
         racket/list
         glmnet
         glmnet/plot
         glmnet/datasets)]

@chunk[<provide>
(provide x y fit path-plot entries coefficients first-five predictions
         fold-ids cvfit cv-plot lambda-min coefficients-min predictions-min)]

@chunk[<data>
(define-values (x y) (quick-start-example))]

R's @tt{fit <- glmnet(x, y)} fits the lasso path over R's automatic sequence
of @math{λ}. It stops after 67 values, where the deviance explained stops
growing, at 91.3%.

@chunk[<fit>
(define fit (elnet-path x y))]

R's @tt{plot(fit)} draws one curve per coefficient against @math{−log λ}.
Printing the path, R's @tt{print(fit)}, gives R's table of the number of
nonzero coefficients, the percentage of deviance explained and @math{λ}.

@chunk[<plot>
(define path-plot (plot-coefficient-path fit))]

Each curve leaves zero where its predictor enters the model. @racket[entries]
maps each column name to the largest @math{λ} at which its coefficient is
nonzero, in order of entry. @racket[in-path] walks the path from the largest
@math{λ} and gives the intercept first, so the coefficients of the columns
start at index 1.

@chunk[<entries>
(define entries
  (for*/fold ([entered '()] #:result (reverse entered))
             ([(λ β) (in-path fit)]
              [(name b) (in-parallel (design-matrix-column-names x) (in-vector β 1))]
              #:unless (or (zero? b) (dict-has-key? entered name)))
    (cons (cons name λ) entered)))]

R's @tt{coef(fit, s = 0.1)}: the intercept and the 20 coefficients at
@math{λ = 0.1}, which is between two fitted values, so they are interpolated
as R interpolates them. Eleven are zero. R's
@tt{predict(fit, newx = x[1:5,], s = c(0.1, 0.05))} predicts the first five
observations at two values of @math{λ}.

@chunk[<coef>
(define coefficients (coef fit #:lambda 0.1))
(define first-five (design-matrix-select-rows x (range 5)))
(define predictions (predict fit first-five #:lambda '(0.1 0.05)))]

R's @tt{cv.glmnet(x, y)} assigns each observation to one of ten folds at
random, with R's random number generator, so the vignette's numbers change
with the seed, and no other program draws the same folds. Here the folds are
fixed: observation @math{i} is in fold @math{i mod 10}, R's
@tt{foldid <- rep(1:10, length.out = 100)}, counted from 0. With those folds,
R's @tt{cv.glmnet(x, y, foldid = foldid)} and @racket[elnet-cv] agree:
@tt{lambda.min} is @math{0.06897}, where the model has ten nonzero
coefficients, and @tt{lambda.1se} is @math{0.1323}, with eight.

@chunk[<cv>
(define fold-ids (build-list (length y) (λ (i) (modulo i 10))))
(define cvfit (elnet-cv x y #:fold-ids fold-ids))
(define cv-plot (plot-cv cvfit))]

R's @tt{cvfit$lambda.min}, @tt{coef(cvfit, s = "lambda.min")} and
@tt{predict(cvfit, newx = x[1:5,], s = "lambda.min")}:

@chunk[<lambda-min>
(define lambda-min (glmnet-cv-lambda-min cvfit))
(define coefficients-min (coef cvfit #:lambda 'lambda-min))
(define predictions-min (predict cvfit first-five #:lambda 'lambda-min))]

@chunk[<*>
  <require>
  <provide>
  <data>
  <fit>
  <plot>
  <entries>
  <coef>
  <cv>
  <lambda-min>]
