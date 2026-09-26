#lang racket/base

;; The generic model interface (#25). Every result type implements
;; `gen:glmnet-model` by presenting itself as a `glmnet-path`: a single-lambda
;; fit is a path with one lambda. `predict` and `coef` read any model through
;; that view, and between two fitted lambdas they interpolate the coefficients
;; as R's predict.glmnet does with exact = FALSE.

(require racket/contract
         racket/generic
         racket/list
         racket/match
         racket/math
         racket/vector
         "marshal.rkt"
         "path.rkt"
         (submod "path.rkt" support))

(define lambda-arg/c (or/c (>=/c 0) (and/c (listof (>=/c 0)) pair?)))
(define type/c (or/c 'link 'response 'class))

(define-generics glmnet-model
  (glmnet-model->path glmnet-model)
  (glmnet-model-default-lambda glmnet-model)
  (deviance-ratio glmnet-model)
  #:defaults
  ([glmnet-path?
    (define (glmnet-model->path p) p)
    (define (glmnet-model-default-lambda p) (vector->list (glmnet-path-lambda p)))
    (define (deviance-ratio p) (glmnet-path-dev-ratio p))])
  #:fallbacks
  [(define/generic ->path glmnet-model->path)
   (define (glmnet-model-default-lambda m)
     (vector-ref (glmnet-path-lambda (->path m)) 0))
   (define (deviance-ratio m)
     (vector-ref (glmnet-path-dev-ratio (->path m)) 0))])

(provide
 gen:glmnet-model
 glmnet-model?
 (contract-out
  [glmnet-model->path (-> glmnet-model? glmnet-path?)]
  [glmnet-model-default-lambda (-> glmnet-model? lambda-arg/c)]
  [deviance-ratio (-> glmnet-model? (or/c real? (vectorof real? #:flat? #t)))]
  [predict
   (->* (glmnet-model? design-matrix/c) (#:type type/c #:lambda lambda-arg/c) list?)]
  [coef
   (->* (glmnet-model?) (#:lambda lambda-arg/c) (or/c vector? (listof vector?)))]))

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

(define (coef model #:lambda [s (glmnet-model-default-lambda model)])
  (define p (glmnet-model->path model))
  (define interpolate (lambda-interpolator (glmnet-path-lambda p)))
  (at-lambdas s (lambda (s)
                  (define-values (a0 beta) (point-at p interpolate s))
                  (coefficient-vector a0 beta))))

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

;; `predict`, with `who` named in errors; the family prediction helpers are
;; this at a fixed type.
(define (predict-as who model X type [s (glmnet-model-default-lambda model)])
  (define p (glmnet-model->path model))
  (define family (glmnet-path-family p))
  (check-type who family type)
  (define x (prediction-matrix X (path-num-predictors p) who))
  (define interpolate (lambda-interpolator (glmnet-path-lambda p)))
  (define transform (row-transform family type))
  (at-lambdas s (lambda (s)
                  (define-values (a0 beta) (point-at p interpolate s))
                  (predict-rows family x a0 beta transform))))

(define (predict model X
                 #:type [type 'link]
                 #:lambda [s (glmnet-model-default-lambda model)])
  (predict-as 'predict model X type s))
