#lang racket/base

;; The generic model interface (#25). Every result type implements
;; `gen:glmnet-model` by presenting itself as a `glmnet-path`: a single-lambda
;; fit is a path with one lambda. `predict` and `coef` read any model through
;; that view, and between two fitted lambdas they interpolate the coefficients
;; as R's predict.glmnet does with exact = FALSE. A cross-validated model (#27)
;; also names lambdas, 'lambda-min and 'lambda-1se, which `predict` and `coef`
;; accept in place of a number. A model that names its predictors, such as a
;; formula model (#26), gets name-keyed coefficients from `coef` and has
;; `predict` read a table by those names.

(require racket/contract
         racket/generic
         racket/list
         racket/match
         racket/math
         racket/vector
         "marshal.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         (only-in "../data.rkt" design-matrix? table?)
         (only-in (submod "../data.rkt" support) select-table-columns))

(define lambda-arg/c (or/c (>=/c 0) (and/c (listof (>=/c 0)) pair?)))
(define lambda-name/c (or/c 'lambda-min 'lambda-1se))
(define type/c (or/c 'link 'response 'class))

(define-generics glmnet-model
  (glmnet-model->path glmnet-model)
  (glmnet-model-default-lambda glmnet-model)
  (glmnet-model-named-lambda glmnet-model name)
  (glmnet-model-predictor-names glmnet-model)
  (glmnet-model-response-names glmnet-model)
  (deviance-ratio glmnet-model)
  #:defaults
  ([glmnet-path?
    (define (glmnet-model->path p) p)
    (define (glmnet-model-default-lambda p) (vector->list (glmnet-path-lambda p)))
    (define (glmnet-model-named-lambda p name) #f)
    (define (glmnet-model-predictor-names p) #f)
    (define (glmnet-model-response-names p) #f)
    (define (deviance-ratio p) (glmnet-path-dev-ratio p))])
  #:fallbacks
  [(define/generic ->path glmnet-model->path)
   (define (glmnet-model-default-lambda m)
     (vector-ref (glmnet-path-lambda (->path m)) 0))
   (define (glmnet-model-named-lambda m name) #f)
   (define (glmnet-model-predictor-names m) #f)
   (define (glmnet-model-response-names m) #f)
   (define (deviance-ratio m)
     (vector-ref (glmnet-path-dev-ratio (->path m)) 0))])

(provide
 gen:glmnet-model
 glmnet-model?
 (contract-out
  [glmnet-model->path (-> glmnet-model? glmnet-path?)]
  [glmnet-model-default-lambda (-> glmnet-model? lambda-arg/c)]
  [glmnet-model-named-lambda (-> glmnet-model? lambda-name/c (or/c #f (>=/c 0)))]
  [glmnet-model-predictor-names (-> glmnet-model? (or/c #f (listof string?)))]
  [glmnet-model-response-names (-> glmnet-model? (or/c #f (listof string?)))]
  [deviance-ratio (-> glmnet-model? (or/c real? (vectorof real? #:flat? #t)))]
  [predict
   (->* (glmnet-model? (or/c design-matrix/c table?))
        (#:type type/c #:lambda (or/c lambda-arg/c lambda-name/c))
        list?)]
  [coef
   (->* (glmnet-model?) (#:lambda (or/c lambda-arg/c lambda-name/c))
        (or/c vector? list?))]))

;; For the family modules only; not part of the public API.
(module* support #f
  (provide single-fit-path
           write-fit
           predict-as))

;; --- single fits -------------------------------------------------------------

;; A single-lambda fit as a path with one lambda.
(define (single-fit-path family lambda intercept coefficients dev-ratio num-passes)
  (define coefs (vector coefficients))
  (glmnet-path family (vector lambda) (and intercept (vector intercept)) coefs
               (vector dev-ratio)
               (if (memq family '(multinomial mgaussian))
                   (count-nonzero-groups coefs)
                   (count-nonzero coefs))
               num-passes))

;; The prop:custom-write procedure of the single-fit results.
(define (write-fit m port mode)
  (write-point (glmnet-model->path m) port))

;; --- lambda interpolation ------------------------------------------------------

;; R's lambda.interp, for the fitted lambdas `lams`: a procedure that takes a
;; lambda s and returns the 0-based indices of the fitted lambdas on either side
;; of it and the weight `frac` of the left one, so that the coefficients at s are
;; left * frac + right * (1 - frac). The lambdas are rescaled to [0, 1]; s
;; outside the path is clamped to its nearest end.
(define (lambda-interpolator lams)
  (define k (vector-length lams))
  (define l1 (vector-ref lams 0))
  (define span (- l1 (vector-ref lams (sub1 k))))
  (cond
    [(or (= k 1) (zero? span))
     (lambda (s) (values 0 0 1.0))]
    [else
     (define xs (for/vector #:length k ([l (in-vector lams)]) (/ (- l1 l) span)))
     (define lo (for/fold ([m +inf.0]) ([x (in-vector xs)]) (min m x)))
     (define hi (for/fold ([m -inf.0]) ([x (in-vector xs)]) (max m x)))
     (define knots (approx-knots xs))
     (lambda (s)
       (define v (max lo (min hi (/ (- l1 (exact->inexact s)) span))))
       (define coord (approx knots v))
       (define left (exact-floor coord))
       (define right (exact-ceiling coord))
       (define xl (vector-ref xs (sub1 left)))
       (define xr (vector-ref xs (sub1 right)))
       (values (sub1 left)
               (sub1 right)
               (if (or (= left right) (< (abs (- xl xr)) double-eps))
                   1.0
                   (/ (- v xr) (- xl xr)))))]))

;; R's .Machine$double.eps.
(define double-eps 2.220446049250313e-16)

;; The knots R's approx(xs, 1..k) interpolates between: (x . position) sorted
;; by x, with tied xs collapsed to the mean of their 1-based positions.
(define (approx-knots xs)
  (define pairs
    (sort (for/list ([x (in-vector xs)] [pos (in-naturals 1)]) (cons x pos)) < #:key car))
  (for/vector ([group (in-list (group-by car pairs =))])
    (cons (caar group) (/ (apply + (map cdr group)) (length group)))))

;; R's approx1: the position at v, found by bisection and linear interpolation.
;; v lies between the first and last knots.
(define (approx knots v)
  (let loop ([i 0] [j (sub1 (vector-length knots))])
    (cond
      [(< i (sub1 j))
       (define ij (quotient (+ i j) 2))
       (if (< v (car (vector-ref knots ij))) (loop i ij) (loop ij j))]
      [else
       (match-define (cons xi yi) (vector-ref knots i))
       (match-define (cons xj yj) (vector-ref knots j))
       (cond
         [(= v xj) yj]
         [(= v xi) yi]
         [else (+ yi (* (- yj yi) (/ (- v xi) (- xj xi))))])])))

;; a * frac + b * (1 - frac), elementwise through nested vectors.
(define (blend a b frac)
  (cond
    [(= frac 1.0) a]
    [(vector? a)
     (for/vector #:length (vector-length a) ([x (in-vector a)] [y (in-vector b)])
       (blend x y frac))]
    [else (+ (* a frac) (* b (- 1.0 frac)))]))

;; The intercepts (#f for Cox) and coefficients of path p at lambda s.
(define (point-at p interpolate s)
  (define-values (left right frac) (interpolate s))
  (define (at field)
    (blend (vector-ref field left) (vector-ref field right) frac))
  (values (and (glmnet-path-intercepts p) (at (glmnet-path-intercepts p)))
          (at (glmnet-path-coefficients p))))

;; Applies `one` to s, or to each lambda of a list s.
(define (at-lambdas s one)
  (if (list? s) (map one s) (one s)))

;; s, with a lambda name such as 'lambda-min replaced by the lambda the model
;; gives it; `who` names the caller in the error for a model that has none.
(define (resolve-lambda who model s)
  (cond
    [(symbol? s)
     (or (glmnet-model-named-lambda model s)
         (raise-arguments-error who "only a cross-validated model has a named lambda"
                                "lambda" s))]
    [else s]))

;; --- coef ----------------------------------------------------------------------

;; As R's coef: the intercept first, then one entry per predictor (no intercept
;; for Cox); a vector of those, one per class or response, for the multinomial
;; and multi-response families.
(define (coefficient-vector a0 beta)
  (cond
    [(not a0) (vector-copy beta)]
    [(vector? a0)
     (for/vector #:length (vector-length a0) ([a (in-vector a0)] [b (in-vector beta)])
       (vector-append (vector a) b))]
    [else (vector-append (vector a0) beta)]))

;; For a model with named predictors, what turns coefficient-vector's result
;; into association lists keyed as R's coef names its rows and list elements:
;; "(Intercept)" and the predictor names, inside one list per class label
;; (multinomial) or response name (multi-response; y1, y2, ... when the model
;; names no responses). For any other model, `values`.
(define (coefficient-namer model p)
  (define names (glmnet-model-predictor-names model))
  (cond
    [(not names) values]
    [else
     (define rows (if (glmnet-path-intercepts p) (cons "(Intercept)" names) names))
     (define (label v)
       (for/list ([row (in-list rows)] [c (in-vector v)])
         (cons row c)))
     (define ((label-groups keys) vs)
       (for/list ([key (in-list keys)] [v (in-vector vs)])
         (cons key (label v))))
     (define k (vector-length (vector-ref (glmnet-path-coefficients p) 0)))
     (case (glmnet-path-family p)
       [(multinomial) (label-groups (range k))]
       [(mgaussian)
        (label-groups (or (glmnet-model-response-names model)
                          (for/list ([r (in-range k)]) (format "y~a" (add1 r)))))]
       [else label])]))

(define (coef model #:lambda [s (glmnet-model-default-lambda model)])
  (define p (glmnet-model->path model))
  (define interpolate (lambda-interpolator (glmnet-path-lambda p)))
  (define name (coefficient-namer model p))
  (at-lambdas (resolve-lambda 'coef model s)
              (lambda (s)
                (define-values (a0 beta) (point-at p interpolate s))
                (name (coefficient-vector a0 beta)))))

;; --- predict -------------------------------------------------------------------

(define (sigmoid z) (/ 1.0 (+ 1.0 (exp (- z)))))

;; Numerically stable softmax of a list of linear predictors.
(define (softmax etas)
  (define m (apply max etas))
  (define exps (for/list ([e (in-list etas)]) (exp (- e m))))
  (define s (apply + exps))
  (for/list ([e (in-list exps)]) (/ e s)))

;; The 0-based index of the largest element; the first one wins a tie, as in
;; R's glmnet_softmax.
(define (argmax-index xs)
  (let loop ([rest (cdr xs)] [i 1] [best (car xs)] [best-i 0])
    (cond
      [(null? rest) best-i]
      [(> (car rest) best) (loop (cdr rest) (add1 i) (car rest) i)]
      [else (loop (cdr rest) (add1 i) best best-i)])))

;; What `type` makes of a row's linear predictor (a list of them for the
;; multinomial and multi-response families), as R's predict methods do.
(define (row-transform family type)
  (case type
    [(link) values]
    [(response)
     (case family
       [(gaussian mgaussian) values]
       [(binomial) sigmoid]
       [(multinomial) softmax]
       [(poisson cox) exp])]
    [(class)
     (case family
       [(binomial) (lambda (eta) (if (> eta 0.0) 1 0))]
       [(multinomial) argmax-index])]))

(define (check-type who family type)
  (when (and (eq? type 'class) (not (memq family '(binomial multinomial))))
    (raise-arguments-error who "type 'class is only for the binomial and multinomial families"
                           "family" family)))

;; The predictions for each row of x from intercepts a0 and coefficients beta.
(define (predict-rows family x a0 beta transform)
  (define n (design-matrix-nrows x))
  (if (memq family '(multinomial mgaussian))
      (for/list ([i (in-range n)])
        (transform (for/list ([a (in-vector a0)] [b (in-vector beta)])
                     (linear-predictor x i a b))))
      (let ([a0 (or a0 0.0)])
        (for/list ([i (in-range n)])
          (transform (linear-predictor x i a0 beta))))))

;; The new data X as a design matrix with one column per coefficient: for a
;; model with named predictors, the columns of the table X with those names,
;; in the model's order; otherwise X itself, column for column.
(define (model-matrix who model X ni)
  (define names (glmnet-model-predictor-names model))
  (cond
    [names
     (unless (table? X)
       (raise-arguments-error who "the model's predictors are named, so X must be a table"
                              "predictors" names "X" X))
     (select-table-columns X names who)]
    [(and (table? X) (not (design-matrix? X)))
     (raise-arguments-error who "the model's predictors are not named, so X must be a design matrix"
                            "X" X)]
    [else (prediction-matrix X ni who)]))

;; `predict`, with `who` named in errors; the family prediction helpers are
;; this at a fixed type.
(define (predict-as who model X type [s (glmnet-model-default-lambda model)])
  (define p (glmnet-model->path model))
  (define family (glmnet-path-family p))
  (check-type who family type)
  (define x (model-matrix who model X (path-num-predictors p)))
  (define interpolate (lambda-interpolator (glmnet-path-lambda p)))
  (define transform (row-transform family type))
  (at-lambdas (resolve-lambda who model s)
              (lambda (s)
                (define-values (a0 beta) (point-at p interpolate s))
                (predict-rows family x a0 beta transform))))

(define (predict model X
                 #:type [type 'link]
                 #:lambda [s (glmnet-model-default-lambda model)])
  (predict-as 'predict model X type s))
