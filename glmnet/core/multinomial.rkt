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
         "model.rkt"
         (submod "model.rkt" support)
         "../foreign/raw/multinomial.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "../foreign/raw/path.rkt"
         (only-in "../data.rkt" design-matrix-select-rows)
         "cv.rkt"
         (submod "cv.rkt" support))

(provide
 (struct-out multinomial-result)
 (contract-out
  [multinomial-fit
   (->* (design-matrix/c multiclass-response/c #:lambda (>=/c 0))
        (#:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        multinomial-result?)]
  [multinomial-predict-proba
   (-> multinomial-result? design-matrix/c (listof (listof (real-in 0 1))))]
  [multinomial-predict
   (-> multinomial-result? design-matrix/c (listof exact-nonnegative-integer?))]))

(provide
 (contract-out
  [multinomial-path
   (->* (design-matrix/c multiclass-response/c)
           (#:lambda lambda-sequence/c
            #:nlambda exact-positive-integer?
            #:lambda-min-ratio lambda-min-ratio/c
            #:alpha (real-in 0 1)
            #:standardize? boolean?
            #:intercept? boolean?
            #:thresh (>/c 0)
            #:max-iters exact-positive-integer?)
        glmnet-path?)]
  [multinomial-cv
   (->* (design-matrix/c multiclass-response/c)
        (#:type-measure (or/c 'deviance 'class 'mse 'mae)
         #:nfolds nfolds/c
         #:fold-ids fold-ids/c
         #:grouped? boolean?
         #:lambda cv-lambda-sequence/c
         #:nlambda exact-positive-integer?
         #:lambda-min-ratio lambda-min-ratio/c
         #:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        glmnet-cv?)]))

;; A fitted K-class multinomial model. `intercepts` is a vector of K reals,
;; centred to sum to zero; `coefficients` is a vector of K coefficient vectors
;; (each length ni), one per class, on the original predictor scale. Both are on
;; the log-odds scale of the symmetric multinomial parameterization. `dev-ratio`
;; is the fraction of null deviance explained; `lambda` the penalty used;
;; `num-passes` glmnet's pass count.
(struct multinomial-result (intercepts coefficients dev-ratio lambda num-passes)
  #:transparent
  #:property prop:custom-write write-fit
  #:methods gen:glmnet-model
  [(define (glmnet-model->path r)
     (single-fit-path 'multinomial (multinomial-result-lambda r)
                      (multinomial-result-intercepts r) (multinomial-result-coefficients r)
                      (multinomial-result-dev-ratio r) (multinomial-result-num-passes r)))])

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

;; The softmax is unchanged by adding a constant to every class's intercept, and
;; the Fortran leaves that constant free. R's getcoef.multinomial centres the
;; intercepts at every lambda; so do we.
(define (center-intercepts a0)
  (define mean (/ (for/sum ([a (in-vector a0)]) a) (vector-length a0)))
  (for/vector #:length (vector-length a0) ([a (in-vector a0)])
    (- a mean)))

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
  (define x (as-design-matrix X 'multinomial-fit "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define nc (labels->num-classes y 'multinomial-fit))
  (define yv (as-response y no 'multinomial-fit "y"))
  (define intercepts (make-f64vector nc 0.0))
  (define beta (make-f64vector (* ni nc) 0.0))
  (define-values (dev-ratio lam nlp jerr)
    (glmnet-multinomial-solo/raw (exact->inexact alpha) no ni nc (design-matrix-data x) yv
                                 (exact->inexact lambda)
                                 (if standardize? 1 0)
                                 (if intercept? 1 0)
                                 (exact->inexact thresh)
                                 max-iters
                                 intercepts beta))
  (check-multinomial-jerr jerr 'multinomial-fit)
  ;; beta is class-major: class k's predictor j at k*ni + j.
  (multinomial-result (center-intercepts (unpack-vector intercepts nc))
                      (unpack-columns beta ni nc)
                      dev-ratio lam nlp))

;; --- prediction ------------------------------------------------------------

;; Per-class probabilities (summing to 1) for each row of X: the softmax of the
;; K linear predictors.
(define (multinomial-predict-proba result X)
  (predict-as 'multinomial-predict-proba result X 'response))

;; Predicted class label (0..K-1) for each row of X: the class with the largest
;; linear predictor, and so the largest probability.
(define (multinomial-predict result X)
  (predict-as 'multinomial-predict result X 'class))

;; --- regularization path (#10) ---------------------------------------------

(define (multinomial-path X y
                          #:lambda [lambda #f]
                          #:nlambda [nlambda 100]
                          #:lambda-min-ratio [lambda-min-ratio #f]
                          #:alpha [alpha 1.0]
                          #:standardize? [standardize? #t]
                          #:intercept? [intercept? #t]
                          #:thresh [thresh 1e-7]
                          #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'multinomial-path "X"))
  (define no (design-matrix-nrows x))
  (define ni (design-matrix-ncols x))
  (define k (labels->num-classes y 'multinomial-path))
  (define yv (as-response y no 'multinomial-path "y"))
  (define-values (nlam flmin ulam)
    (path-lambdas lambda nlambda lambda-min-ratio no ni))
  (define a0 (make-f64vector (* k nlam) 0.0))
  (define beta (make-f64vector (* ni k nlam) 0.0))
  (define dev (make-f64vector nlam 0.0))
  (define alm (make-f64vector nlam 0.0))
  (define-values (lmu nlp jerr)
    (glmnet-multinomial-path/raw (exact->inexact alpha) no ni k (design-matrix-data x) yv
                                 nlam flmin ulam
                                 (if standardize? 1 0) (if intercept? 1 0)
                                 (exact->inexact thresh) max-iters
                                 a0 beta dev alm))
  (check-multinomial-jerr jerr 'multinomial-path)
  (define coefficients (unpack-column-groups beta ni k lmu))
  (glmnet-path 'multinomial (finish-lambdas alm lmu (not lambda))
               (for/vector #:length lmu ([a (in-vector (unpack-intercept-groups a0 k lmu))])
                 (center-intercepts a))
               coefficients (unpack-vector dev lmu)
               (count-nonzero-groups coefficients) nlp))

;; --- cross-validation (#27) ------------------------------------------------

(define (multinomial-cv X y
                        #:type-measure [measure 'deviance]
                        #:nfolds [nfolds 10]
                        #:fold-ids [fold-ids #f]
                        #:grouped? [grouped? #t]
                        #:lambda [lambda #f]
                        #:nlambda [nlambda 100]
                        #:lambda-min-ratio [lambda-min-ratio #f]
                        #:alpha [alpha 1.0]
                        #:standardize? [standardize? #t]
                        #:intercept? [intercept? #t]
                        #:thresh [thresh 1e-7]
                        #:max-iters [max-iters 100000])
  (define x (as-design-matrix X 'multinomial-cv "X"))
  (as-response y (design-matrix-nrows x) 'multinomial-cv "y")
  (define labels (list->vector y))
  (define (fit x y)
    (multinomial-path x y
                      #:lambda lambda #:nlambda nlambda #:lambda-min-ratio lambda-min-ratio
                      #:alpha alpha #:standardize? standardize? #:intercept? intercept?
                      #:thresh thresh #:max-iters max-iters))
  (define (fit-all) (fit x y))
  (define (fit-rows rows) (fit (design-matrix-select-rows x rows) (select labels rows)))
  (cross-validate 'multinomial-cv x labels fit-all fit-rows
                  #:measure measure #:nfolds nfolds #:fold-ids fold-ids #:grouped? grouped?))
