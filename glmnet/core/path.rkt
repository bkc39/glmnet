#lang racket/base

;; The regularization-path result shared by every family, plus the pieces each
;; family's `*-path` fitter uses: R's lambda handling (the automatic sequence,
;; the decreasing sort of a user sequence, fix.lam) and unpacking the shim's
;; column-major per-lambda outputs. Also how results print: a path as R's
;; print.glmnet table, a single fit (a path with one lambda) as one line.

(require racket/contract
         racket/format
         racket/math
         ffi/vector)

(provide (struct-out glmnet-path))

;; For the family modules and core/model.rkt only; not part of the public API.
(module* support #f
  (provide lambda-sequence/c
           lambda-min-ratio/c
           path-lambdas
           finish-lambdas
           unpack-vector
           unpack-columns
           unpack-column-groups
           unpack-intercept-groups
           count-nonzero
           count-nonzero-groups
           path-num-predictors
           write-point))

;; family       : 'gaussian 'binomial 'multinomial 'cox 'poisson 'mgaussian
;; lambda       : the fitted lambdas, decreasing (vector of length L)
;; intercepts   : per lambda, a real or (multinomial/mgaussian) a vector of K
;;                reals; #f for Cox, which has no intercept
;; coefficients : per lambda, a dense vector with one entry per predictor or
;;                (multinomial/mgaussian) a vector of K such vectors
;; dev-ratio    : per lambda, the fraction of null deviance explained
;; df           : per lambda, the number of predictors with a nonzero coefficient
;; num-passes   : total coordinate-descent passes over the whole path
(struct glmnet-path (family lambda intercepts coefficients dev-ratio df num-passes)
  #:transparent
  #:property prop:custom-write
  (lambda (p port mode) (write-path p port)))

(define lambda-sequence/c (or/c #f (and/c (listof (>=/c 0)) pair?)))
(define lambda-min-ratio/c (or/c #f (and/c real? (>/c 0) (</c 1))))

;; The shim's lambda arguments, as R's glmnet() derives them. A user sequence is
;; fitted largest first (R: rev(sort(lambda))), with flmin = 1. Otherwise glmnet
;; picks `nlambda` lambdas down to lambda-min-ratio * lambda_max, where the ratio
;; defaults to 0.01 when there are fewer observations than predictors and 1e-4
;; otherwise. Returns (values nlam flmin ulam).
(define (path-lambdas lambda nlambda lambda-min-ratio no ni)
  (cond
    [lambda
     (define sorted (sort (map exact->inexact lambda) >))
     (values (length sorted) 1.0 (list->f64vector sorted))]
    [else
     (define ratio (or lambda-min-ratio (if (< no ni) 0.01 1e-4)))
     (values nlambda (exact->inexact ratio) (make-f64vector nlambda 0.0))]))

;; The first `lmu` fitted lambdas. In automatic mode glmnet reports the first as
;; its `big` sentinel; R's fix.lam replaces it with the log-linear extrapolation
;; exp(2 log l2 - log l3), and leaves it alone when there are only two lambdas.
(define (finish-lambdas lambda-out lmu automatic?)
  (define lams (unpack-vector lambda-out lmu))
  (when (and automatic? (> lmu 2))
    (vector-set! lams 0 (exp (- (* 2 (log (vector-ref lams 1)))
                                (log (vector-ref lams 2))))))
  lams)

(define (unpack-vector v lmu)
  (for/vector #:length lmu ([m (in-range lmu)])
    (f64vector-ref v m)))

;; Column m (0-based) of an n-row column-major matrix, for m < lmu.
(define (unpack-columns v n lmu)
  (for/vector #:length lmu ([m (in-range lmu)])
    (for/vector #:length n ([j (in-range n)])
      (f64vector-ref v (+ j (* n m))))))

;; An (n, k, nlam) column-major array as, per lambda, k vectors of length n.
(define (unpack-column-groups v n k lmu)
  (for/vector #:length lmu ([m (in-range lmu)])
    (for/vector #:length k ([c (in-range k)])
      (for/vector #:length n ([j (in-range n)])
        (f64vector-ref v (+ j (* n (+ c (* k m)))))))))

;; A (k, nlam) column-major array as, per lambda, a vector of k values.
(define (unpack-intercept-groups v k lmu)
  (for/vector #:length lmu ([m (in-range lmu)])
    (for/vector #:length k ([c (in-range k)])
      (f64vector-ref v (+ c (* k m))))))

(define (count-nonzero coefficients)
  (for/vector #:length (vector-length coefficients) ([beta (in-vector coefficients)])
    (for/sum ([b (in-vector beta)]) (if (zero? b) 0 1))))

;; A predictor counts once if it is nonzero for any class or response.
(define (count-nonzero-groups coefficients)
  (for/vector #:length (vector-length coefficients) ([groups (in-vector coefficients)])
    (for/sum ([j (in-range (vector-length (vector-ref groups 0)))])
      (if (for/or ([beta (in-vector groups)]) (not (zero? (vector-ref beta j)))) 1 0))))

(define (path-num-predictors p)
  (define beta (vector-ref (glmnet-path-coefficients p) 0))
  (if (memq (glmnet-path-family p) '(multinomial mgaussian))
      (vector-length (vector-ref beta 0))
      (vector-length beta)))

;; --- printing ----------------------------------------------------------------

;; x to `digits` significant digits, as R's signif, in positional notation
;; unless that would need more than four leading zeros or `digits` places
;; before the point.
(define (signif x [digits 4])
  (cond
    [(zero? x) "0"]
    [else
     (define e (order-of-magnitude (inexact->exact (abs x))))
     (if (< -5 e digits)
         (~r x #:precision (max 0 (- digits 1 e)))
         (~r x #:notation 'exponential #:precision (sub1 digits)))]))

;; x rounded to `places` decimal places, with a rounded -0.0 shown as 0.
(define (fixed x places)
  (define scale (expt 10 places))
  (define r (/ (round (* x scale)) scale))
  (~r (if (zero? r) 0.0 r) #:precision `(= ,places)))

;; A single fit, from its one-lambda path: #<glmnet:binomial λ=0.04 dev=0.7134 nz=2/3>.
(define (write-point p port)
  (fprintf port "#<glmnet:~a λ=~a dev=~a nz=~a/~a>"
           (glmnet-path-family p)
           (signif (vector-ref (glmnet-path-lambda p) 0))
           (fixed (vector-ref (glmnet-path-dev-ratio p) 0) 4)
           (vector-ref (glmnet-path-df p) 0)
           (path-num-predictors p)))

;; A path, as R's print.glmnet: Df, %Dev and Lambda for each fitted lambda.
(define (write-path p port)
  (define rows
    (cons '("Df" "%Dev" "Lambda")
          (for/list ([df (in-vector (glmnet-path-df p))]
                     [dev (in-vector (glmnet-path-dev-ratio p))]
                     [lam (in-vector (glmnet-path-lambda p))])
            (list (number->string df) (fixed (* 100 dev) 2) (signif lam)))))
  (define widths
    (for/list ([column (in-list (apply map list rows))])
      (apply max (map string-length column))))
  (fprintf port "#<glmnet-path:~a" (glmnet-path-family p))
  (for ([row (in-list rows)])
    (newline port)
    (for ([cell (in-list row)]
          [width (in-list widths)])
      (write-string "  " port)
      (write-string (~a cell #:min-width width #:align 'right) port)))
  (write-string ">" port))
