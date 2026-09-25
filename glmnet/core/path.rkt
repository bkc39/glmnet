#lang racket/base

;; The regularization-path result shared by every family, plus the pieces each
;; family's `*-path` fitter uses: R's lambda handling (the automatic sequence,
;; the decreasing sort of a user sequence, fix.lam) and unpacking the shim's
;; column-major per-lambda outputs.

(require racket/contract
         ffi/vector)

(provide (struct-out glmnet-path))

;; For the family modules' `*-path` fitters only; not part of the public API.
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
           count-nonzero-groups))

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
  #:transparent)

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
