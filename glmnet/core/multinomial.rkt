#lang racket/base

;; High-level K-class multinomial elastic-net fitting on top of the raw FFI.
;;
;; `multinomial-fit` is the multiclass extension of `logistic-fit`: the same
;; #:alpha / #:lambda knobs, but the response is a list of integer class labels
;; 0..K-1 and the fit returns K intercepts and K coefficient vectors (the
;; symmetric multinomial parameterization). `multinomial-predict-proba` turns a
;; fit plus new predictors into per-class probabilities (softmax over the K
;; linear predictors); `multinomial-predict` takes the argmax.

(require racket/contract
         ffi/vector
         "marshal.rkt"
         "../foreign/raw/multinomial.rkt")

(provide
 (struct-out multinomial-result)
 (contract-out
  [multinomial-fit
   (->* (matrix/c multiclass-response/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        multinomial-result?)]
  [multinomial-predict-proba
   (-> multinomial-result? matrix/c (listof (listof (real-in 0 1))))]
  [multinomial-predict
   (-> multinomial-result? matrix/c (listof exact-nonnegative-integer?))]))

;; A fitted K-class multinomial model. `intercepts` is a vector of K reals;
;; `coefficients` is a vector of K coefficient vectors (each length ni), one per
;; class, on the original predictor scale. Both are on the log-odds scale of the
;; symmetric multinomial parameterization. `dev-ratio` is the fraction of null
;; deviance explained; `lambda` the penalty used; `num-passes` glmnet's pass count.
(struct multinomial-result (intercepts coefficients dev-ratio lambda num-passes)
  #:transparent)

;; --- input contract --------------------------------------------------------

;; A non-empty list of integer class labels 0..K-1.
(define multiclass-response/c (and/c (listof exact-nonnegative-integer?) pair?))

;; Validate the labels cover 0..K-1 with every class present; return K (>= 2).
(define (labels->num-classes y who)
  (define k (add1 (apply max y)))
  (when (< k 2)
    (error who "multinomial needs at least 2 classes, got ~a" k))
  (define present (make-vector k #f))
  (for ([v (in-list y)]) (vector-set! present v #t))
  (for ([c (in-range k)])
    (unless (vector-ref present c)
      (error who
             "class ~a has no observations; labels must cover 0..~a contiguously"
             c (sub1 k))))
  k)

;; Multinomial shares the binomial (lognet) jerr codes: 8000/9000 (a class
;; probability collapsed -- e.g. perfect separation) and 90000 (coefficient-bound
;; non-convergence) on top of the shared cases in `check-jerr`.
(define (check-multinomial-jerr jerr who)
  (cond
    [(and (>= jerr 8000) (< jerr 9000))
     (error who
            (format (string-append
                     "a class probability collapsed (perfect separation or a "
                     "degenerate class); try a larger lambda (jerr=~a)")
                    jerr))]
    [(and (>= jerr 9000) (< jerr 10000))
     (error who (format "a class has a degenerate null probability (jerr=~a)" jerr))]
    [(= jerr 90000)
     (error who "coefficient-bound adjustment failed to converge (jerr=90000)")]
    [else (check-jerr jerr who)]))

;; --- public API ------------------------------------------------------------

(define (multinomial-fit X y
                         #:lambda lambda
                         #:alpha [alpha 1.0]
                         #:standardize? [standardize? #t]
                         #:intercept? [intercept? #t]
                         #:thresh [thresh 1e-7]
                         #:max-iters [max-iters 100000])
  (define-values (no ni) (rows->dims X 'multinomial-fit))
  (define nc (labels->num-classes y 'multinomial-fit))
  (define xcol (matrix->colmajor X no ni))
  (define yv (response->f64vector y no 'multinomial-fit))
  (define intercepts (make-f64vector nc 0.0))
  (define beta (make-f64vector (* ni nc) 0.0))
  (define-values (dev-ratio lam nlp jerr)
    (glmnet-multinomial-solo/raw (exact->inexact alpha) no ni nc xcol yv
                                 (exact->inexact lambda)
                                 (if standardize? 1 0)
                                 (if intercept? 1 0)
                                 (exact->inexact thresh)
                                 max-iters
                                 intercepts beta))
  (check-multinomial-jerr jerr 'multinomial-fit)
  (multinomial-result
   (for/vector ([k (in-range nc)]) (f64vector-ref intercepts k))
   ;; beta is class-major: class k's predictor j at k*ni + j.
   (for/vector ([k (in-range nc)])
     (for/vector ([j (in-range ni)]) (f64vector-ref beta (+ (* k ni) j))))
   dev-ratio lam nlp))

;; --- prediction ------------------------------------------------------------

;; The K linear predictors eta_k = a0_k + x . beta_k for one predictor row.
(define (row-etas result row)
  (define intercepts (multinomial-result-intercepts result))
  (define coefs (multinomial-result-coefficients result))
  (define ni (vector-length (vector-ref coefs 0)))
  (unless (= (length row) ni)
    (error 'multinomial-predict
           "row has ~a features, expected ~a" (length row) ni))
  (for/list ([k (in-range (vector-length intercepts))])
    (for/fold ([acc (vector-ref intercepts k)])
              ([b (in-vector (vector-ref coefs k))]
               [xj (in-list row)])
      (+ acc (* b (exact->inexact xj))))))

;; Numerically-stable softmax of a list of linear predictors.
(define (softmax etas)
  (define m (apply max etas))
  (define exps (for/list ([e (in-list etas)]) (exp (- e m))))
  (define s (apply + exps))
  (for/list ([e (in-list exps)]) (/ e s)))

;; Per-class probabilities (summing to 1) for each row of X.
(define (multinomial-predict-proba result X)
  (for/list ([row (in-list X)])
    (softmax (row-etas result row))))

;; Index (0-based class label) of the largest element.
(define (argmax-index xs)
  (let loop ([rest (cdr xs)] [i 1] [best (car xs)] [best-i 0])
    (cond
      [(null? rest) best-i]
      [(> (car rest) best) (loop (cdr rest) (add1 i) (car rest) i)]
      [else (loop (cdr rest) (add1 i) best best-i)])))

;; Predicted class label (0..K-1) for each row of X: argmax of the probabilities.
(define (multinomial-predict result X)
  (for/list ([ps (in-list (multinomial-predict-proba result X))])
    (argmax-index ps)))
