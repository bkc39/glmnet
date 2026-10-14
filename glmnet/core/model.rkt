#lang racket/base

;; The generic model interface (#25). Every result type implements
;; `gen:glmnet-model` by presenting itself as a `glmnet-path`: a single-lambda
;; fit is a path with one lambda. `predict` and `coef` read any model through
;; that view, and between two fitted lambdas they interpolate the coefficients
;; as R's predict.glmnet does with exact = FALSE. A cross-validated model (#27)
;; also names lambdas, 'lambda-min and 'lambda-1se, which `predict` and `coef`
;; accept in place of a number. A model that names its predictors, such as a
;; formula model (#26), gets name-keyed coefficients from `coef` and has
;; `predict` read named data, a table or a dataframe, by those names. `in-path`
;; walks a model's path, a λ and its coefficients at a time.

(require racket/contract
         racket/generic
         racket/list
         racket/match
         racket/math
         racket/vector
         "marshal.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         (only-in "../data.rkt" table?)
         (only-in "input.rkt" data/c named-data?)
         (only-in (submod "input.rkt" support) new-data->design-matrix))

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

(define (fitted? model)
  (positive? (vector-length (glmnet-path-lambda (glmnet-model->path model)))))

(provide
 gen:glmnet-model
 glmnet-model?
 (contract-out
  [glmnet-model->path (-> glmnet-model? glmnet-path?)]
  [glmnet-model-default-lambda
   (->i ([model glmnet-model?])
        #:pre/name (model) "the model has at least one fitted λ" (fitted? model)
        [result lambda-arg/c])]
  [glmnet-model-named-lambda (-> glmnet-model? lambda-name/c (or/c #f (>=/c 0)))]
  [rename remembered-predictor-names glmnet-model-predictor-names
          (-> glmnet-model? (or/c #f (listof string?)))]
  [rename remembered-response-names glmnet-model-response-names
          (-> glmnet-model? (or/c #f (listof string?)))]
  [rename model-class-labels glmnet-model-class-labels
          (-> glmnet-model? (or/c #f (listof string?)))]
  [deviance-ratio (-> glmnet-model? (or/c real? (vectorof real? #:flat? #t)))]
  [predict
   (->i ([model glmnet-model?] [X data/c])
        (#:type [type type/c] #:lambda [s (or/c lambda-arg/c lambda-name/c)])
        #:pre/name (model) "the model has at least one fitted λ" (fitted? model)
        [result list?])]
  [coef
   (->i ([model glmnet-model?])
        (#:lambda [s (or/c lambda-arg/c lambda-name/c)])
        #:pre/name (model) "the model has at least one fitted λ" (fitted? model)
        [result (or/c vector? list?)])]
  [in-path (-> glmnet-model? sequence?)]))

;; For the family modules only; not part of the public API.
(module* support #f
  (provide single-fit-path
           write-fit
           predict-as
           prop:predictor-matrix
           prop:class-labels
           model-class-labels
           attach-data-names
           data-predictor-names))

;; What a fit through core/input.rkt remembers of its data: its predictors'
;; names, its classes' labels and its responses' names, each #f when it has
;; none, on a chaperone of the result, which stays the plain struct (equal?,
;; coef); see AGENTS.md.
(struct data-input (predictors classes responses))

(define-values (prop:data-names data-names? data-names-ref)
  (make-impersonator-property 'data-names))

;; model, remembering `names`, `classes` and `responses`, or model itself
;; when all are #f. Every result type is transparent, so struct-info finds its
;; type.
(define (attach-data-names model names #:classes [classes #f] #:responses [responses #f])
  (cond
    [(or names classes responses)
     (define-values (type skipped?) (struct-info model))
     (chaperone-struct model type prop:data-names (data-input names classes responses))]
    [else model]))

(define ((data-name-ref field) model)
  (and (data-names? model) (field (data-names-ref model))))

(define data-predictor-names (data-name-ref data-input-predictors))
(define data-class-labels (data-name-ref data-input-classes))
(define data-response-names (data-name-ref data-input-responses))

;; The public glmnet-model-predictor-names and glmnet-model-response-names:
;; the names a model gives, as a formula model does, which key `coef`, or
;; else those a fit from named data remembers, which do not.
(define (remembered-predictor-names model)
  (or (glmnet-model-predictor-names model) (data-predictor-names model)))

(define (remembered-response-names model)
  (or (glmnet-model-response-names model) (data-response-names model)))

;; How a model that names its predictors builds their design matrix from named
;; data, a table or a dataframe, for `predict`: a procedure of the model, the
;; data and the name of the procedure called. A formula model (#53) has it,
;; since its predictors, such as the interaction "x:z", are computed from the
;; data's columns. Without it, `predict` reads the columns with the
;; predictors' names.
(define-values (prop:predictor-matrix predictor-matrix? predictor-matrix-ref)
  (make-struct-type-property 'predictor-matrix))

;; How a binomial or multinomial model names its classes: a procedure of the
;; model that returns their labels, in the order of the class indices, or #f
;; for the indices themselves. A formula model of a response of strings (#53)
;; names them, as R's glmnet names a factor response's: `coef` keys a
;; multinomial model's coefficients by them, and `predict` with #:type 'class
;; returns them. A fit from such a response (core/input.rkt) remembers them
;; instead.
(define-values (prop:class-labels class-labels? class-labels-ref)
  (make-struct-type-property 'class-labels))

(define (model-class-labels model)
  (cond
    [(class-labels? model) ((class-labels-ref model) model)]
    [else (data-class-labels model)]))

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

;; The names a model gives its predictors, or #f if it names none. They must
;; name each predictor of its path p once, since `coef` pairs them with the
;; coefficients and `predict` reads the table's columns by them.
(define (model-predictor-names who model p)
  (define names (glmnet-model-predictor-names model))
  (when names
    (define n (path-num-predictors p))
    (unless (= (length names) n)
      (raise-arguments-error who "the model does not have one name per predictor"
                             "predictor names" names "predictors" n))
    (define dup (check-duplicates names))
    (when dup
      (raise-arguments-error who "the model gives two predictors the same name" "name" dup)))
  names)

;; For a model with named predictors, what turns coefficient-vector's result
;; into association lists keyed as R's coef names its rows and list elements:
;; "(Intercept)" and the predictor names, inside one list per class
;; (multinomial; its label, or its index when the model names no classes) or
;; response name (multi-response; y1, y2, ... when the model names no
;; responses). For any other model, `values`.
(define (coefficient-namer who model p)
  (define names (model-predictor-names who model p))
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
       [(multinomial) (label-groups (or (model-class-labels model) (range k)))]
       [(mgaussian)
        (define responses (glmnet-model-response-names model))
        (when (and responses (not (= (length responses) k)))
          (raise-arguments-error who "the model does not have one name per response"
                                 "response names" responses "responses" k))
        (label-groups (or responses (for/list ([r (in-range k)]) (format "y~a" (add1 r)))))]
       [else label])]))

(define (coef model #:lambda [s (glmnet-model-default-lambda model)])
  (define p (glmnet-model->path model))
  (define interpolate (lambda-interpolator (glmnet-path-lambda p)))
  (define name (coefficient-namer 'coef model p))
  (at-lambdas (resolve-lambda 'coef model s)
              (lambda (s)
                (define-values (a0 beta) (point-at p interpolate s))
                (name (coefficient-vector a0 beta)))))

;; --- in-path -------------------------------------------------------------------

;; Each fitted λ of the model's path with its coefficients, built as `coef`
;; builds them, but from the path's own column at that λ.
(define (in-path model)
  (define p (glmnet-model->path model))
  (define lams (glmnet-path-lambda p))
  (define intercepts (glmnet-path-intercepts p))
  (define coefficients (glmnet-path-coefficients p))
  (define name (and (fitted? model) (coefficient-namer 'in-path model p)))
  (define (step i)
    (values (vector-ref lams i)
            (name (coefficient-vector (and intercepts (vector-ref intercepts i))
                                      (vector-ref coefficients i)))))
  (make-do-sequence
   (lambda ()
     (values step add1 0 (lambda (i) (< i (vector-length lams))) #f #f))))

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

;; The new data X as a design matrix with one column per predictor of the
;; model's path p: for a model with named predictors, the design matrix that
;; the model builds from X, a table or a dataframe, or the columns of X with
;; those names, in the model's order; otherwise as new-data->design-matrix
;; reads it, by the names of the predictors of a fit from named data or by
;; position.
(define (model-matrix who model X p)
  (define names (model-predictor-names who model p))
  (cond
    [(and names (not (or (table? X) (named-data? X))))
     (raise-arguments-error who (string-append "the model's predictors are named, so X must be "
                                               "a table or a dataframe")
                            "predictors" names "X" X)]
    [(and names (predictor-matrix? model)) ((predictor-matrix-ref model) model X who)]
    [names (new-data->design-matrix who X names)]
    [else
     (prediction-matrix (new-data->design-matrix who X (data-predictor-names model))
                        (path-num-predictors p) who)]))

;; `predict`, with `who` named in errors; the family prediction helpers are
;; this at a fixed type.
(define (predict-as who model X type [s (glmnet-model-default-lambda model)])
  (define p (glmnet-model->path model))
  (define family (glmnet-path-family p))
  (check-type who family type)
  (define x (model-matrix who model X p))
  (define interpolate (lambda-interpolator (glmnet-path-lambda p)))
  (define classes (and (eq? type 'class) (model-class-labels model)))
  (define transform
    (if classes
        (let ([labels (list->vector classes)] [index (row-transform family type)])
          (lambda (eta) (vector-ref labels (index eta))))
        (row-transform family type)))
  (at-lambdas (resolve-lambda who model s)
              (lambda (s)
                (define-values (a0 beta) (point-at p interpolate s))
                (predict-rows family x a0 beta transform))))

(define (predict model X
                 #:type [type 'link]
                 #:lambda [s (glmnet-model-default-lambda model)])
  (predict-as 'predict model X type s))
