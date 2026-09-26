#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "plots" #:style 'toc]{Plots}

@racketmodname[glmnet/plot] draws the two plots of R's
@hyperlink["https://glmnet.stanford.edu/"]{glmnet} package: the coefficient
path of a fit, as R's @tt{plot.glmnet} draws it, and the cross-validation
curve, as @tt{plot.cv.glmnet} draws it. It plots a @racket[glmnet-path] from
one of the path fitters (see @secref["concepts-path"]), a @racket[glmnet-cv]
from one of the cross-validation procedures (see @secref["concepts-cv"]), and
a @racket[formula-model] of either (see @secref["formulas"]).

@racket[(require glmnet)] does not load the plots, so a program that only fits
models does not load the plot library either. They have a module of their
own:

@examples[#:eval ev #:label #f
(require glmnet/plot)
]

@margin-note{See @secref["ref-plot"] in the @secref["reference"] for the plot
procedures and their arguments.}

Each plot is a pict, which DrRacket and this manual show as an image. The
plots are drawn with @racketmodname[plot/no-gui], whose parameters, such as
@racket[plot-font-size], apply to them.

@local-table-of-contents[]

@section[#:tag "plot-path"]{Coefficient paths}

The running fixture of @secref["gs-first-fit"] has a response that is exactly
@math{y = 1 + 2x₁ − x₂}, and a third predictor, @math{x₃ = x₁²}, that carries
no signal. @racket[plot-coefficient-path] plots its path as R's
@tt{plot(fit, label = TRUE)} does:

@examples[#:eval ev #:label #f
(define X '((1.0 2.0  1.0)
            (2.0 1.0  4.0)
            (3.0 4.0  9.0)
            (4.0 3.0 16.0)
            (5.0 6.0 25.0)
            (6.0 5.0 36.0)))
(define y '(1.0 4.0 3.0 6.0 5.0 8.0))
(define path (elnet-path X y))
(plot-coefficient-path path #:label #t)
]

To read it as R users do:

@itemlist[
 @item{Each curve is one coefficient, as a function of @math{λ}. The x axis is
       @math{−log λ}: the largest @math{λ} of the path, at which every
       coefficient is zero, is at the left, and the penalty weakens to the
       right.}
 @item{A coefficient that is zero along the whole path has no curve. The
       others are coloured as R colours them, cycling through the first six
       colours of R's palette, and @racket[#:label] labels each at the
       right-hand end of the path with the predictor's position, counting from
       1.}
 @item{The axis along the top counts the nonzero coefficients at the
       positions of the ticks below it.}
]

Read from the left, @math{x₁} enters the model at once. @math{x₂} enters at
about @math{−log λ = 1.2}, where the curve of @math{x₁} bends, and the two
coefficients then grow towards their least-squares values, @math{2} and
@math{−1}. The irrelevant @math{x₃} enters only over the last few @math{λ},
with a coefficient below @racket[0.001]: its curve looks flat, but the count
along the top reaches 3.

@subsection[#:tag "plot-path-elastic-net"]{Lasso and elastic net}

The elastic net fits the same data with @racket[#:alpha 0.5], as the
elastic-net example does:

@examples[#:eval ev #:label #f
(plot-coefficient-path (elnet-path X y #:alpha 0.5) #:label #t)
]

The ridge part of the penalty spreads weight across correlated predictors,
and @math{x₃ = x₁²} is strongly correlated with @math{x₁}. So @math{x₃} enters
together with @math{x₁}, at the start. Its coefficient peaks near
@racket[0.07] and shrinks again as the penalty weakens and @math{x₁} takes
over, while @math{x₂} enters at about @math{−log λ = 0.85}. The lasso, which
has no ridge part, picked @math{x₁} alone. @secref["ex-elastic-net-grouping"]
shows the same effect in single fits.

@subsection[#:tag "plot-path-xvar"]{The x axis}

@racket[#:xvar] chooses the x axis, with R's names:

@itemlist[
 @item{@racket['lambda], the default, is @math{−log λ}. With
       @racket[#:sign-lambda 1] it is @math{log λ}, and the path runs from
       right to left.}
 @item{@racket['norm] is the L1 norm of the coefficients, the sum of their
       absolute values. For the lasso it is the budget of the constrained form
       of the problem.}
 @item{@racket['dev] is the fraction of the null deviance explained, the
       path's @racket[glmnet-path-dev-ratio].}
]

@examples[#:eval ev #:label #f
(plot-coefficient-path path #:xvar 'norm)
(plot-coefficient-path path #:xvar 'dev)
]

Against the deviance ratio, the curves rise steeply at the right, where the
last few percent of deviance cost large changes in the coefficients. That is
where the fit starts to overfit when the data are noisy.

Before version 4.1-9, R's default was @racket['norm]. R glmnet 4.1.10, which
@racketmodname[glmnet/plot] follows, defaults to @math{−log λ}.

@subsection[#:tag "plot-path-names"]{Labels}

@racket[#:label] also takes the names to use for the curves: a list with one
name per predictor, or a @racket[design-matrix?] whose column names are used,
such as the one the path was fitted to:

@examples[#:eval ev #:label #f
(define D (rows->design-matrix X #:column-names '(x1 x2 x3)))
(plot-coefficient-path (elnet-path D y) #:label D #:sign-lambda 1)
]

With @racket[#:sign-lambda 1], the path ends at the left, and so do the
labels. Where R cuts off labels that run past the edge of the plot, these
plots widen the x axis until the labels fit.

A @racket[formula-model] fitted by @racket[formula-path] or
@racket[formula-cv] knows its predictors' names, so @racket[#:label #t]
labels its curves with them:

@examples[#:eval ev #:label #f
(define table
  (list (cons "y" y)
        (cons "x1" (map car X))
        (cons "x2" (map cadr X))
        (cons "square" (map caddr X))))
(define named-path (formula-path (~ y all) table))
(plot-coefficient-path named-path #:label #t)
]

For the multi-response family, each plot's y-axis label names its response,
as R names it by the column of @tt{y}.

@subsection[#:tag "plot-path-multi"]{Several classes or responses}

For the multinomial family, R draws one plot per class, and for the
multi-response Gaussian family, one per response. @racket[plot-coefficient-path]
stacks them into one picture, top to bottom, each the given width and height.
The top axis of each counts that class's nonzero coefficients:

@examples[#:eval ev #:label #f
(define X3 '((1.0 1.0) (2.0 1.0) (1.0 2.0) (2.0 2.0)
             (5.0 1.0) (6.0 1.0) (5.0 2.0) (6.0 2.0)
             (3.0 5.0) (4.0 5.0) (3.0 6.0) (4.0 6.0)))
(define classes '(0 0 0 0 1 1 1 1 2 2 2 2))
(define mpath (multinomial-path X3 classes))
(plot-coefficient-path mpath #:height 250 #:label #t)
]

The classes are separable, so the coefficients grow without bound as the
penalty weakens, until the path stops once the deviance ratio passes 0.999.
Classes 0 and 1 split on @math{x₁}, with opposite signs, and class 2 on
@math{x₂}.

@racket[#:type-coef '2norm] draws one plot instead, of each predictor's
2-norm across the classes, @math{(Σ_k β_{jk}²)^{1/2}}. Its top axis is the mean
of the classes' counts, rounded to one decimal place (for the multi-response
family, whose responses share their nonzero coefficients, the first
response's count):

@examples[#:eval ev #:label #f
(plot-coefficient-path mpath #:type-coef '2norm)
]

@section[#:tag "plot-cv"]{Cross-validation curves}

The lasso example cross-validates 40 observations of the fixture's model,
with noise added to @math{y} (see @secref["ex-lasso-cv"]).
@racket[plot-cv] plots the result of @racket[elnet-cv] as R's
@tt{plot(cvfit)} does:

@examples[#:eval ev #:label #f
(random-seed 3)
(define (uniform a b)
  (+ a (* (- b a) (random))))
(define X40
  (for/list ([i (in-range 40)])
    (define x1 (uniform 0 6))
    (list x1 (uniform 0 6) (* x1 x1))))
(define y40
  (for/list ([row (in-list X40)])
    (+ 1.0 (* 2 (car row)) (- (cadr row)) (uniform -1 1))))
(define cv (elnet-cv X40 y40))
(plot-cv cv)
]

To read it:

@itemlist[
 @item{The red points are the cross-validated error, @racket[glmnet-cv-cvm],
       at each @math{λ}, and the grey bars reach from
       @racket[glmnet-cv-cvlo] to @racket[glmnet-cv-cvup], one standard error
       either side. The y axis is labelled with R's name for the measure,
       @racket[glmnet-cv-name].}
 @item{The x axis is @math{−log λ}, as in the path plot: heavy
       regularization at the left.}
 @item{The dotted lines mark the two choices. The right-hand one is
       @tech{lambda-min}, where the error is smallest; the left-hand one is
       @tech{lambda-1se}, the largest @math{λ} whose error is within one
       standard error of that smallest error.}
 @item{The axis along the top counts the nonzero coefficients, from
       @racket[glmnet-cv-nzero].}
]

The error falls steeply while @math{x₁} and @math{x₂} enter, then flattens.
From @tech{lambda-1se} rightwards the curve stays within one standard error
of its minimum, so the data cannot tell those models apart from the best one.
@tech{lambda-1se} is the simplest of them, which is why R's @tt{predict} and
@tt{coef} use it by default.

@racket[plot-coefficient-path] plots the path of a @racket[glmnet-cv], the
path fitted to all the data:

@examples[#:eval ev #:label #f
(plot-coefficient-path cv #:label #t)
]

@section[#:tag "plot-compose"]{Combining with other plots}

@racket[coefficient-path-renderers] and @racket[cv-renderers] return the
curves, points and lines of the two plots as plot-lib renderers, without the
axes. They combine with other renderers, here to mark the two choices of
cross-validation on the coefficient path:

@examples[#:eval ev #:label #f
(require plot/no-gui)
(define (at lam) (- (log lam)))
(plot-pict (list (coefficient-path-renderers cv)
                 (vrule (at (glmnet-cv-lambda-min cv)) #:style 'dot)
                 (vrule (at (glmnet-cv-lambda-1se cv)) #:style 'dot))
           #:x-label "-Log(λ)"
           #:y-label "Coefficients")
]

@section[#:tag "plot-files"]{Writing to a file}

With @racket[#:out-file], both plot procedures also write the plot to a file,
in the format that the file's extension names: @filepath{.png}, @filepath{.pdf},
@filepath{.svg} or @filepath{.eps}. They still return the pict:

@examples[#:eval ev #:label #f
(require racket/file)
(define file (make-temporary-file "cv-~a.png"))
(void (plot-cv cv #:out-file file))
(call-with-input-file file (lambda (in) (read-bytes 4 in)))
(eval:error (plot-cv cv #:out-file "cv.jpg"))
]

@section[#:tag "plot-r"]{Matching R}

The plots follow R glmnet 4.1.10's @tt{plotCoef}, @tt{plot.multnet},
@tt{plot.mrelnet} and @tt{plot.cv.glmnet}:

@itemlist[
 @item{The same axes, x-axis labels and y-axis labels, the same defaults for
       @racket[#:xvar] and @racket[#:sign-lambda], each axis widened by 4% at
       both ends, and the same colours: the first six of R's palette for the
       curves, and red points, dark grey bars and black dotted lines for the
       cross-validation curve.}
 @item{The counts along the top of a path plot are R's: at each tick, the
       count of the fitted @math{λ} next to it, read as R's @tt{approx} with
       @tt{method = "constant"} reads it. That is the next @math{λ} to the
       right, or to the left for @math{log λ}.}
 @item{A curve is drawn for each predictor that is nonzero at some @math{λ}.
       When no coefficient of a class is nonzero, R draws no plot for it, and
       neither does @racket[plot-coefficient-path]. When exactly one is, both
       warn that the plot is not meaningful; here the warning is logged.}
 @item{The ticks are plot-lib's, not R's @tt{pretty} values, and the error
       bars' caps are a fixed number of pixels wide rather than 2% of the x
       axis.}
 @item{R writes a count at every @math{λ} along the top of the
       cross-validation plot and leaves out those that would overlap. Here
       each run of consecutive @math{λ} with the same count gets one label, in
       its middle, and runs whose labels would overlap are left out.}
 @item{R draws the plots of the classes or responses one after another;
       @racket[plot-coefficient-path] stacks them into one pict.}
]

@(close-eval ev)
