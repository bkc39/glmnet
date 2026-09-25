#lang scribble/manual
@(require "../utils.rkt")

@(define ev (make-glmnet-eval))

@title[#:tag "concepts" #:style 'toc]{Concepts}

Every model family in @racketmodname[glmnet] shares one data layout, one
penalty and one shape of result. This chapter covers those shared pieces; the
@secref["examples"] then take each family in turn.

@local-table-of-contents[]

@section[#:tag "concepts-data"]{Data layout}

A @deftech{design matrix} is a non-empty list of rows, one per observation.
Every row is a list of reals of the same length, one entry per predictor. Exact
numbers are accepted and converted to flonums. The bindings pack the rows into
the column-major, double-precision array the Fortran expects, so there is no
matrix type to construct first.

@examples[#:eval ev #:label #f
(define X '((1.0 2.0)
            (2.0 1.0)
            (3.0 4.0)
            (4.0 3.0)
            (5.0 6.0)))
(define y '(1 4 3 6 5))
(elnet-result-coefficients (ols X y))
]

The @deftech{response} has one entry per row of the design matrix. Its shape
depends on the family:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Family}                    @bold{Response})
       (list "Gaussian"                       "a list of reals")
       (list "Binomial"                       @elem{a list of @racket[0]/@racket[1] class labels})
       (list "Multinomial"                    @elem{a list of class labels @math{0, …, K−1}, every class present})
       (list "Cox"                            @elem{a list of positive times @emph{and} a list of @racket[0]/@racket[1] event indicators})
       (list "Poisson"                        "a list of non-negative counts")
       (list "Multi-response Gaussian"        "a matrix: one row per observation, one column per response"))]

Shapes are checked before the Fortran is called:

@examples[#:eval ev #:label #f
(eval:error (ols '((1.0 2.0) (3.0)) '(1.0 2.0)))
(eval:error (ols X '(1.0 2.0)))
]

@section[#:tag "concepts-penalty"]{The penalty: @math{α} and @math{λ}}

For the Gaussian family, glmnet solves

@centered{@math{min_{β₀,β} 1/(2n) ‖y − β₀ − Xβ‖² + λ [ α‖β‖₁ + (1−α)/2 ‖β‖₂² ]}}

over the intercept @math{β₀} and the coefficients @math{β}. The other families
replace the squared-error term with their negative log-likelihood; the penalty
is the same for all of them. It has two knobs:

@itemlist[
 @item{@bold{@racket[#:alpha], @math{α ∈ [0, 1]}}, mixes the penalty.
       @math{α = 0} is the ridge (L2) penalty, which shrinks every coefficient
       but zeroes none; @math{α = 1} is the lasso (L1) penalty, which sets
       coefficients exactly to zero; values in between are the elastic net.
       Every fit procedure defaults to @racket[1.0].}
 @item{@bold{@racket[#:lambda], @math{λ ≥ 0}}, sets the penalty's strength.
       It is required: there is no default and, for now, no automatic path of
       values. @math{λ = 0} is the unpenalized fit.}
]

The four Gaussian models are one routine, @racket[elnet-fit], with different
arguments; the named procedures only fix @racket[#:alpha]:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Procedure}           @bold{@math{α}}   @bold{@math{λ}})
       (list @racket[ols]               "(ignored)"       "0")
       (list @racket[ridge]             "0"               "> 0")
       (list @racket[lasso]             "1"               "> 0")
       (list @racket[elastic-net]       "0 < α < 1"       "> 0"))]

@examples[#:eval ev #:label #f
(equal? (elnet-result-coefficients (lasso X y #:lambda 0.1))
        (elnet-result-coefficients (elnet-fit X y #:alpha 1.0 #:lambda 0.1)))
(eval:error (lasso X y))
]

@section[#:tag "concepts-families"]{Model families}

Each family pairs a response type with a glmnet solver and a link from the
linear predictor @math{η = β₀ + xβ} to the response:

@itemlist[
 @item{The @deftech{Gaussian family} (@tt{elnet}) models a numeric response
       with the identity link: @math{E[y] = η}.}
 @item{The @deftech{binomial family} (@tt{lognet}) models a 0/1 label through
       the log-odds of class 1: @math{log(P(y=1) / P(y=0)) = η}.}
 @item{The @deftech{multinomial family} (@tt{lognet} with @math{K > 2}
       classes) fits one linear predictor per class, @math{η_k = a0_k + xβ_k},
       and takes class probabilities as their softmax.}
 @item{The @deftech{Cox family} (@tt{coxnet}) models survival times through the
       proportional hazard @math{h(t | x) = h₀(t) exp(xβ)}. The baseline hazard
       @math{h₀} absorbs the intercept, so there is none.}
 @item{The @deftech{Poisson family} (@tt{fishnet}) models counts with the log
       link: @math{E[y] = exp(η)}.}
 @item{The @deftech{multi-response Gaussian family} (@tt{multelnet}) fits one
       Gaussian model per response column under a grouped penalty: each
       predictor enters for every response or for none.}
]

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
 (list (list @bold{Family}          @bold{Fit}                 @bold{Result}                  @bold{Prediction})
       (list "Gaussian"             @racket[elnet-fit]         @racket[elnet-result]          "---")
       (list "Binomial"             @racket[logistic-fit]      @racket[logistic-result]       @elem{@racket[logistic-predict-proba], @racket[logistic-predict]})
       (list "Multinomial"          @racket[multinomial-fit]   @racket[multinomial-result]    @elem{@racket[multinomial-predict-proba], @racket[multinomial-predict]})
       (list "Cox"                  @racket[cox-fit]           @racket[cox-result]            @elem{@racket[cox-linear-predictor], @racket[cox-relative-risk]})
       (list "Poisson"              @racket[poisson-fit]       @racket[poisson-result]        @racket[poisson-predict-mean])
       (list "Multi-response"       @racket[mgaussian-fit]     @racket[mgaussian-result]      @racket[mgaussian-predict]))]

All six fit procedures take the same keywords: @racket[#:lambda],
@racket[#:alpha], @racket[#:standardize?], @racket[#:intercept?] (not
@racket[cox-fit]), @racket[#:thresh] and @racket[#:max-iters].

@section[#:tag "concepts-results"]{Results}

A fit returns a transparent struct. The fields follow one pattern across the
families:

@itemlist[
 @item{@bold{intercept} --- a real, or a vector of one per class or response
       (@racket[multinomial-result-intercepts],
       @racket[mgaussian-result-intercepts]). Cox results have none.}
 @item{@bold{coefficients} --- a dense vector with one entry per predictor, on
       the original (unstandardized) scale; a predictor the penalty dropped is
       exactly @racket[0.0]. Multinomial and multi-response results hold a
       vector of such vectors, one per class or response.}
 @item{@bold{r-squared} or @bold{dev-ratio} --- the fraction of the null
       deviance explained. For the Gaussian families this is @math{R²}.}
 @item{@bold{lambda} --- the penalty the solver used.}
 @item{@bold{num-passes} --- the number of coordinate-descent passes.}
]

@examples[#:eval ev #:label #f
(define fit (ridge X y #:lambda 0.1))
fit
(elnet-result-r-squared fit)
]

Because the structs are transparent, @racket[equal?] compares them field by
field and @racket[match] destructures them:

@examples[#:eval ev #:label #f
(require racket/match)
(match-define (elnet-result a0 beta _ _ _) fit)
(list a0 beta)
]

@section[#:tag "concepts-standardize"]{Standardization and the intercept}

By default each predictor is scaled to unit variance before fitting and the
coefficients are reported on the original scale, which is also the R package's
default. The penalty acts on the standardized coefficients, so predictors
measured in different units are penalized evenly. Pass
@racket[#:standardize? #f] to penalize the raw coefficients instead; a
penalized fit then changes:

@examples[#:eval ev #:label #f
(elnet-result-coefficients (ridge X y #:lambda 0.1))
(elnet-result-coefficients (ridge X y #:lambda 0.1 #:standardize? #f))
]

Every family except Cox fits an unpenalized intercept. Pass
@racket[#:intercept? #f] to force it to zero:

@examples[#:eval ev #:label #f
(elnet-result-intercept (ols X y #:intercept? #f))
(elnet-result-coefficients (ols X y #:intercept? #f))
]

@section[#:tag "concepts-convergence"]{Convergence and errors}

Coordinate descent cycles over the coefficients until no update moves the
objective by more than @racket[#:thresh] (default @racket[1e-7]) times the null
deviance, or until
@racket[#:max-iters] passes (default @racket[100000]). @racket[ols] defaults to
a tighter @racket[1e-10], because the unpenalized solution is approached slowly.
The pass count is recorded in every result:

@examples[#:eval ev #:label #f
(elnet-result-num-passes (lasso X y #:lambda 0.1))
(elnet-result-num-passes (lasso X y #:lambda 0.1 #:thresh 1e-12))
]

Problems are reported in three ways:

@itemlist[
 @item{Before the solver runs, an argument of the wrong type raises
       @racket[exn:fail:contract], and rows or responses of mismatched length
       raise @racket[exn:fail].}
 @item{A fatal condition reported by glmnet raises @racket[exn:fail] with a
       readable message, for example when every predictor is constant, or when
       a class probability collapses under perfect separation (a larger
       @racket[#:lambda] usually fixes that).}
 @item{If the solver hits @racket[#:max-iters] before converging, the result
       is still returned, holding partial coefficients, and a warning is logged
       to the current logger.}
]

@examples[#:eval ev #:label #f
(eval:error (ols '((1.0 2.0) (1.0 2.0) (1.0 2.0)) '(1.0 2.0 3.0)))
]

@section[#:tag "concepts-precision"]{The native library}

The vendored Fortran declares its arrays as single-precision @tt{real}, but
@tt{libglmnetcompat} is compiled with @tt{-fdefault-real-8}, which promotes
them to double precision. R's glmnet and Julia's @tt{glmnet_jll} are built the
same way. When the package loads, it checks that the flag took effect
(@racket[glmnet-default-real-bytes]) and that the library exports the entry
points this version expects (@racket[glmnet-capi-abi-version]).

The pre-install hook stages the library from the first of these that exists:

@itemlist[#:style 'ordered
 @item{the directory named by the @envvar{GLMNET_NATIVE_LIB_PATH} environment
       variable (its @filepath{lib} subdirectory), as the Nix build sets it;}
 @item{a library already staged in @filepath{glmnet/native-libs/};}
 @item{the prebuilt candidate for the platform under
       @filepath{glmnet/native-libs/candidates/}.}
]

To rebuild the library from source you need a Fortran toolchain:

@verbatim|{
  # via Nix (also runs the Fortran ctest suite)
  nix build .#native

  # or directly with CMake and gfortran
  cmake -S fortran -B fortran/build -DBUILD_TESTING=ON
  cmake --build fortran/build
  ctest --test-dir fortran/build --output-on-failure
}|

@(close-eval ev)
