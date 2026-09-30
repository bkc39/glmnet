#lang scribble/manual
@(require "../../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "ex-quick-start"]{Quick start}

@margin-note{Source: @filepath{glmnet/examples/12-quick-start.rkt}}

This section follows the Quick Start of R glmnet's vignette,
@hyperlink["https://glmnet.stanford.edu/articles/glmnet.html#quick-start"]{An
Introduction to glmnet}, step by step and on the same data: fit a lasso path,
plot it, print it, read its coefficients and predictions at a @math{λ}, then
choose @math{λ} by cross-validation. Each step gives the R call it follows,
and the numbers are R's; the last section shows how they were checked.

@examples[#:eval ev #:hidden
(require racket/list racket/string)
(define (first-rows p n)
  (for-each displayln (take (string-split (format "~a" p) "\n") (+ n 2)))
  (displayln "..."))
]

@section[#:tag "ex-quick-start-data"]{The data}

R's @tt{data(QuickStartExample)} loads a list of @tt{x}, 100 observations of
20 predictors, and @tt{y}, a numeric response. @racketmodname[glmnet/datasets]
loads it with one call, which returns both, as R's
@tt{x <- QuickStartExample$x; y <- QuickStartExample$y} names them:

@examples[#:eval ev #:label #f
(require glmnet/datasets glmnet/plot)
(define-values (x y) (quick-start-example))
x
(take (design-matrix-column-names x) 5)
(take y 5)
]

@racket[x] is a @racket[design-matrix?]. R's matrix has no column names, and
R's @tt{coef} calls its columns @tt{V1} to @tt{V20}, as @racket[x] names
them.

@section[#:tag "ex-quick-start-path"]{The path}

R's @tt{fit <- glmnet(x, y)} fits the lasso over R's automatic sequence of
@math{λ}; @racket[elnet-path] is its counterpart. R's @tt{plot(fit)} draws
each coefficient against @math{−log λ}:

@examples[#:eval ev #:label #f
(define fit (elnet-path x y))
(plot-coefficient-path fit)
]

Each curve leaves zero where its predictor enters the model. The largest
@math{λ} at which each coefficient is nonzero gives the order of entry:

@examples[#:eval ev #:label #f
(define (entry-lambda j)
  (for/first ([beta (in-vector (glmnet-path-coefficients fit))]
              [lam (in-vector (glmnet-path-lambda fit))]
              #:unless (zero? (vector-ref beta j)))
    lam))
(define entries
  (sort (for/list ([name (in-list (design-matrix-column-names x))]
                   [j (in-naturals)])
          (cons name (entry-lambda j)))
        > #:key cdr))
(for/list ([entry (in-list entries)])
  (cons (car entry) (/ (round (* 1000 (cdr entry))) 1000)))
]

@tt{V1} and @tt{V14} enter first, at @math{λ ≈ 1.49}, then @tt{V5} and
@tt{V20} at @math{0.93}, @tt{V6} at @math{0.85} and @tt{V3} at
@math{0.71}; the other fourteen follow at smaller @math{λ}, in the same order
as in R's fit. The vignette's text
describes the older x axis, the L1 norm of the coefficients, which was R's
default before glmnet 4.1-9 and is @racket[#:xvar 'norm] here (see
@secref["plot-path-xvar"]).

Printing the path, R's @tt{print(fit)}, gives R's table: the number of
nonzero coefficients, the percentage of deviance explained, and @math{λ}. The
path has 67 rows; as in the vignette, only the first ten are shown:

@examples[#:eval ev #:label #f
(eval:alts fit (first-rows fit 10))
]

The path stops at the 67th @math{λ}, @math{0.00351}, with every predictor in
the model and 91.32% of the deviance explained, because the deviance
explained has stopped growing.

@section[#:tag "ex-quick-start-coef"]{Coefficients and predictions}

R's @tt{coef(fit, s = 0.1)} gives the intercept and the 20 coefficients at
@math{λ = 0.1}. That value is between two fitted ones, and the coefficients
are interpolated between them, as R interpolates them:

@examples[#:eval ev #:label #f
(coef fit #:lambda 0.1)
]

R prints them as a sparse column, with the names and a dot for each zero.
The nonzero ones, by name:

@examples[#:eval ev #:label #f
(for/list ([name (in-list (cons "(Intercept)" (design-matrix-column-names x)))]
           [b (in-vector (coef fit #:lambda 0.1))]
           #:unless (zero? b))
  (cons name b))
]

Nine predictors are in the model at @math{λ = 0.1}. @tt{V7} has only just
entered, at @math{λ ≈ 0.1001}, and its coefficient is @math{0.0048}.

R's @tt{predict(fit, newx = x[1:5,], s = c(0.1, 0.05))} predicts the first
five observations at two values of @math{λ}. The vignette draws random new
rows, which Racket cannot reproduce, so the new rows here are the first five
of @racket[x]; @racket[design-matrix-select-rows] counts rows from 0:

@examples[#:eval ev #:label #f
(define first-five (design-matrix-select-rows x '(0 1 2 3 4)))
(predict fit first-five #:lambda '(0.1 0.05))
]

@section[#:tag "ex-quick-start-cv"]{Cross-validation}

R's @tt{cvfit <- cv.glmnet(x, y)} assigns each observation to one of ten
folds at random, with R's random number generator. The vignette's numbers
therefore depend on R's generator and its seed: with @tt{set.seed(123)}, R's
@tt{lambda.min} is @math{0.06284}, and another seed gives another value.
Racket's generator is not R's, so it cannot draw R's folds. The folds here are
fixed instead: observation @math{i} is in fold @math{i mod 10}. R writes the
same folds as @tt{foldid <- rep(1:10, length.out = 100)}, counted from 1, and
with them @tt{cv.glmnet(x, y, foldid = foldid)} gives the numbers below:

@examples[#:eval ev #:label #f
(define fold-ids (for/list ([i (in-range (length y))]) (modulo i 10)))
(take fold-ids 12)
(define cvfit (elnet-cv x y #:fold-ids fold-ids))
cvfit
]

The printed summary has the rows and columns of R's @tt{print(cvfit)}, with
two differences: the index of each @math{λ} counts from 0, and each number is
rounded to four significant digits on its own, where R formats each column as
a whole and can show a digit more. R's @tt{plot(cvfit)} draws the cross-validated mean
squared error with one standard error above and below it, and a dotted line
at each of @tech{lambda-min} and @tech{lambda-1se}:

@examples[#:eval ev #:label #f
(plot-cv cvfit)
]

The error is smallest at @math{λ = 0.06897}, the 35th @math{λ} of the path,
where ten predictors are in the model. @tech{lambda-1se}, @math{0.1323}, is the
largest @math{λ} whose error is within one standard error of that minimum,
@math{1.022 + 0.082}: a simpler model, with eight predictors, whose error
cannot be told apart from the best.

@section[#:tag "ex-quick-start-lambda-min"]{At lambda-min}

R's @tt{cvfit$lambda.min}, @tt{coef(cvfit, s = "lambda.min")} and
@tt{predict(cvfit, newx = x[1:5,], s = "lambda.min")}. A cross-validated
model's @racket[coef] and @racket[predict] take @racket['lambda-min] and
@racket['lambda-1se] for @racket[#:lambda], and default to
@racket['lambda-1se], as R's do:

@examples[#:eval ev #:label #f
(glmnet-cv-lambda-min cvfit)
(coef cvfit #:lambda 'lambda-min)
(predict cvfit first-five #:lambda 'lambda-min)
]

At @tech{lambda-min}, @tt{V10} has joined the nine predictors of
@math{λ = 0.1}, with a coefficient of @math{0.0012}.

@section[#:tag "ex-quick-start-r"]{Matching R}

The same calls in R are

@verbatim[#:indent 2]|{
data(QuickStartExample)
x <- QuickStartExample$x
y <- QuickStartExample$y
fit <- glmnet(x, y)
plot(fit)
print(fit)
coef(fit, s = 0.1)
predict(fit, newx = x[1:5, ], s = c(0.1, 0.05))
foldid <- rep(1:10, length.out = 100)
cvfit <- cv.glmnet(x, y, foldid = foldid)
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = x[1:5, ], s = "lambda.min")
}|

R 4.5.3 with glmnet 4.1.10 prints the same table, 67 rows from
@math{λ = 1.631} down to @math{0.003513}, and gives the same coefficients
and predictions, and @tt{lambda.min} @math{0.06897} and @tt{lambda.1se}
@math{0.1323}. The example's companion test checks each number against R's
to six significant digits, and the parity tests (@filepath{vignette-*} in
@filepath{scripts/r-parity/gen-reference.R}) check the whole path, the printed
table and the cross-validation to glmnet's tolerances, on this dataset and on
R glmnet's six other example datasets.

@(close-eval ev)
