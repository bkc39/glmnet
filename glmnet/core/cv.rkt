#lang racket/base

;; Cross-validation (#27): the result type of every family's `*-cv`, the fold
;; assignment, and the procedure the families share. It follows R glmnet
;; 4.1.10's cv.glmnet step by step: the full-data path fixes the lambdas; each
;; fold's path is fitted to the other folds (with its own automatic lambdas, or
;; the user's) and predicts the held-out fold at the full-data lambdas through
;; R's lambda interpolation; the losses become cvm and cvsd as R's cv.<family>,
;; cvcompute and cvstats compute them; and lambda.min and lambda.1se are chosen
;; as getOptcv.glmnet chooses them. Where R warns and changes a setting because
;; a fold has too few observations, a warning is logged and the setting changes
;; the same way.

(require racket/contract
         racket/flonum
         racket/list
         (only-in "../data.rkt" design-matrix-select-rows)
         (only-in "marshal.rkt" design-matrix-nrows design-matrix-ncols linear-predictor)
         "model.rkt"
         "path.rkt"
         (submod "path.rkt" support))

(provide
 (struct-out glmnet-cv)
 (contract-out
  [random-fold-ids
   (->* (exact-positive-integer?) (exact-positive-integer?)
        (listof exact-nonnegative-integer?))]))

;; For the family modules and the unit tests only; not part of the public API.
(module* support #f
  (provide nfolds/c
           fold-ids/c
           cv-lambda-sequence/c
           select
           cross-validate
           optimal-lambdas
           concordance
           cox-deviance
           write-cv))

;; lambda, cvm, cvsd, cvup, cvlo, nzero : one entry per lambda, as in R
;; measure    : the loss, such as 'mse or 'auc (R's type.measure)
;; name       : R's name for it, such as "Mean-Squared Error"
;; path       : the path fitted to all the data
;; lambda-min : the largest lambda at which cvm is smallest (largest, for 'auc
;;              and 'C)
;; lambda-1se : the largest lambda whose cvm is within one cvsd of that
;; index-min, index-1se : their 0-based positions in `lambda`
;; fold-ids   : the fold of each observation, from 0
(struct glmnet-cv (lambda cvm cvsd cvup cvlo nzero measure name path
                   lambda-min lambda-1se index-min index-1se fold-ids)
  #:transparent
  #:property prop:custom-write
  (lambda (cv port mode) (write-cv cv port))
  #:methods gen:glmnet-model
  [(define (glmnet-model->path cv) (glmnet-cv-path cv))
   (define (glmnet-model-default-lambda cv) (glmnet-cv-lambda-1se cv))
   (define (glmnet-model-named-lambda cv name)
     (case name
       [(lambda-min) (glmnet-cv-lambda-min cv)]
       [(lambda-1se) (glmnet-cv-lambda-1se cv)]
       [else #f]))
   (define (deviance-ratio cv)
     (define p (glmnet-cv-path cv))
     (for/first ([l (in-vector (glmnet-path-lambda p))]
                 [dev (in-vector (glmnet-path-dev-ratio p))]
                 #:when (= l (glmnet-cv-lambda-1se cv)))
       dev))])

(define nfolds/c (and/c exact-integer? (>=/c 3)))
(define fold-ids/c (or/c #f (and/c (listof exact-nonnegative-integer?) pair?)))
(define cv-lambda-sequence/c
  (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2)))))

;; The elements of vector v at the indices in `rows`, as a list.
(define (select v rows)
  (for/list ([i (in-list rows)])
    (vector-ref v i)))

;; --- folds -------------------------------------------------------------------

;; n fold ids, as R's sample(rep(seq(nfolds), length = n)) makes them: the ids
;; 0, 1, ..., nfolds - 1, 0, 1, ... shuffled with the current pseudo-random
;; generator.
(define (random-fold-ids n [nfolds 10])
  (when (> nfolds n)
    (raise-arguments-error 'random-fold-ids "there are more folds than observations"
                           "folds" nfolds "observations" n))
  (define ids (for/vector #:length n ([i (in-range n)]) (modulo i nfolds)))
  (for ([i (in-range (sub1 n) 0 -1)])
    (define j (random (add1 i)))
    (define t (vector-ref ids i))
    (vector-set! ids i (vector-ref ids j))
    (vector-set! ids j t))
  (vector->list ids))

;; The fold of each of the n observations, as a vector.
(define (resolve-folds who n nfolds fold-ids)
  (cond
    [fold-ids
     (unless (= (length fold-ids) n)
       (raise-arguments-error who "fold-ids does not have one entry per row of X"
                              "length of fold-ids" (length fold-ids) "rows of X" n))
     (define k (add1 (apply max fold-ids)))
     (when (< k 3)
       (raise-arguments-error who "cross-validation needs at least 3 folds" "folds" k))
     (define sizes (make-vector k 0))
     (for ([f (in-list fold-ids)])
       (vector-set! sizes f (add1 (vector-ref sizes f))))
     (for ([size (in-vector sizes)]
           [f (in-naturals)]
           #:when (zero? size))
       (raise-arguments-error who "a fold has no observations; fold ids must cover 0..k-1"
                              "fold" f "k" k))
     (list->vector fold-ids)]
    [else
     (when (> nfolds n)
       (raise-arguments-error who "there are more folds than observations"
                              "folds" nfolds "observations" n))
     (list->vector (random-fold-ids n nfolds))]))

;; Each fold's training data (every other fold) must be data the family can
;; fit: both classes for the binomial, every class for the multinomial, and an
;; event for Cox.
(define (check-training-folds who family response folds k)
  (define (missing f what value)
    (raise-arguments-error who (format "the training data of a fold has no ~a" what)
                           "held-out fold" f what value))
  (case family
    [(binomial multinomial)
     (define classes
       (if (eq? family 'binomial)
           2
           (add1 (for/fold ([m 0]) ([y (in-vector response)]) (max m y)))))
     (for ([f (in-range k)])
       (define present (make-vector classes #f))
       (for ([y (in-vector response)]
             [g (in-vector folds)]
             #:unless (= g f))
         (vector-set! present (exact-round y) #t))
       (for ([c (in-range classes)]
             #:unless (vector-ref present c))
         (missing f "class" c)))]
    [(cox)
     (for ([f (in-range k)])
       (unless (for/or ([t+d (in-vector response)]
                        [g (in-vector folds)])
                 (and (not (= g f)) (fl= (cdr t+d) 1.0)))
         (missing f "event" 1)))]
    [else (void)]))

(define (exact-round y) (inexact->exact (round y)))

;; --- losses ------------------------------------------------------------------

;; R's cvtype names.
(define (measure-name family measure)
  (case measure
    [(deviance)
     (case family
       [(gaussian mgaussian) "Mean-squared Error"]
       [(binomial) "Binomial Deviance"]
       [(multinomial) "Multinomial Deviance"]
       [(poisson) "Poisson Deviance"]
       [(cox) "Partial Likelihood Deviance"])]
    [(mse) "Mean-Squared Error"]
    [(mae) "Mean Absolute Error"]
    [(class) "Misclassification Error"]
    [(auc) "AUC"]
    [(C) "C-index"]))

;; R's .Machine$double.eps.
(define double-eps 2.220446049250313e-16)

(define (sigmoid eta) (fl/ 1.0 (fl+ 1.0 (flexp (fl- 0.0 eta)))))

(define (square x) (fl* x x))

;; The loss of one observation, from its response y and the link prediction
;; eta (a list, for the multinomial and multi-response families), as cv.elnet,
;; cv.lognet, cv.multnet, cv.fishnet and cv.mrelnet compute it.
(define (observation-loss family measure)
  (define prob-min 1e-5)
  (define prob-max (fl- 1.0 prob-min))
  (define (clamp p) (flmin (flmax p prob-min) prob-max))
  (case family
    [(gaussian)
     (case measure
       [(mse deviance) (lambda (y eta) (square (fl- y eta)))]
       [(mae) (lambda (y eta) (flabs (fl- y eta)))])]
    [(binomial)
     ;; R's two indicator columns, y0 = (y == 0) and y1 = (y == 1), against the
     ;; probability p of class 1.
     (define ((with-probability loss) y eta)
       (loss (fl- 1.0 y) y (sigmoid eta)))
     (with-probability
      (case measure
        [(mse) (lambda (y0 y1 p) (fl+ (square (fl- y0 (fl- 1.0 p))) (square (fl- y1 p))))]
        [(mae) (lambda (y0 y1 p) (fl+ (flabs (fl- y0 (fl- 1.0 p))) (flabs (fl- y1 p))))]
        [(deviance)
         (lambda (y0 y1 p)
           (define q (clamp p))
           (fl* 2.0 (fl- 0.0 (fl+ (fl* y0 (fllog (fl- 1.0 q))) (fl* y1 (fllog q))))))]
        [(class)
         (lambda (y0 y1 p)
           (fl+ (fl* y0 (if (fl> p 0.5) 1.0 0.0)) (fl* y1 (if (fl<= p 0.5) 1.0 0.0))))]))]
    [(multinomial)
     ;; y is a class label; class k's probability is exp(eta_k) / sum_j exp(eta_j).
     (define ((per-class term) y etas)
       (define exps (for/list ([e (in-list etas)]) (flexp e)))
       (define total (r-sum exps))
       (define probs (for/list ([e (in-list exps)]) (fl/ e total)))
       (if (eq? measure 'class)
           (fl- 1.0 (if (= y (argmax-index probs)) 1.0 0.0))
           (r-sum (for/list ([p (in-list probs)] [k (in-naturals)])
                    (term (if (= y k) 1.0 0.0) p)))))
     (per-class
      (case measure
        [(mse) (lambda (yk p) (square (fl- yk p)))]
        [(mae) (lambda (yk p) (flabs (fl- yk p)))]
        [(deviance) (lambda (yk p) (fl* 2.0 (fl- 0.0 (fl* yk (fllog (clamp p))))))]
        [(class) #f]))]
    [(poisson)
     (case measure
       [(mse) (lambda (y eta) (square (fl- y (flexp eta))))]
       [(mae) (lambda (y eta) (flabs (fl- y (flexp eta))))]
       [(deviance)
        (lambda (y eta)
          (define devy (if (fl= y 0.0) 0.0 (fl- (fl* y (fllog y)) y)))
          (fl* 2.0 (fl- devy (fl- (fl* y eta) (flexp eta)))))])]
    [(mgaussian)
     (define ((per-response term) ys etas)
       (r-sum (for/list ([y (in-vector ys)] [eta (in-list etas)])
                (term y eta))))
     (per-response
      (case measure
        [(mse deviance) (lambda (y eta) (square (fl- y eta)))]
        [(mae) (lambda (y eta) (flabs (fl- y eta)))]))]))

;; The 0-based index of the largest element; the first one wins a tie, as in
;; R's glmnet_softmax.
(define (argmax-index xs)
  (for/fold ([best -inf.0] [best-i 0] #:result best-i)
            ([x (in-list xs)] [i (in-naturals)])
    (if (or (zero? i) (fl> x best)) (values x i) (values best best-i))))

;; --- concordance -------------------------------------------------------------

;; survival::concordance(Surv(times, statuses) ~ xs)$concordance, which R's
;; auc and Cindex compute. A pair is comparable when the earlier time is an
;; event; an event comes before a censored time equal to it, and two events at
;; the same time are not compared. A comparable pair is concordant when the
;; later time has the larger x, and a tie in x counts a half. The pairs are
;; counted in O(n log n) with a Fenwick tree over the ranks of x.
(define (concordance times statuses xs)
  (define n (vector-length times))
  (define keys (for/vector #:length n ([x (in-vector xs)]) (fl+ 0.0 x)))
  (define distinct (list->vector (sort (remove-duplicates (vector->list keys) =) <)))
  (define m (vector-length distinct))
  (define (rank i)
    (define x (vector-ref keys i))
    (let search ([lo 0] [hi (sub1 m)])
      (define mid (quotient (+ lo hi) 2))
      (cond
        [(fl< (vector-ref distinct mid) x) (search (add1 mid) hi)]
        [(fl> (vector-ref distinct mid) x) (search lo (sub1 mid))]
        [else (add1 mid)])))
  (define tree (make-vector (add1 m) 0))
  (define (insert! i)
    (let loop ([r (rank i)])
      (when (<= r m)
        (vector-set! tree r (add1 (vector-ref tree r)))
        (loop (+ r (bitwise-and r (- r)))))))
  (define (count-to r)
    (let loop ([r r] [acc 0])
      (if (zero? r) acc (loop (- r (bitwise-and r (- r))) (+ acc (vector-ref tree r))))))
  (define latest-first (sort (range n) > #:key (lambda (i) (vector-ref times i))))
  ;; Latest time first: the censored at a time join the tree before its events
  ;; are counted, and its events after.
  (let loop ([order latest-first] [inserted 0] [concordant 0] [discordant 0] [tied 0])
    (cond
      [(null? order)
       (define pairs (exact->inexact (+ concordant discordant tied)))
       (fl/ (fl+ (fl/ (exact->inexact (- concordant discordant)) pairs) 1.0) 2.0)]
      [else
       (define t (vector-ref times (car order)))
       (define-values (group later) (splitf-at order (lambda (i) (= (vector-ref times i) t))))
       (define-values (events censored)
         (partition (lambda (i) (fl= (vector-ref statuses i) 1.0)) group))
       (for-each insert! censored)
       (define at-risk (+ inserted (length censored)))
       (define-values (c d t*)
         (for/fold ([c concordant] [d discordant] [t* tied])
                   ([i (in-list events)])
           (define below (count-to (sub1 (rank i))))
           (define up-to (count-to (rank i)))
           (values (+ c (- at-risk up-to)) (+ d below) (+ t* (- up-to below)))))
       (for-each insert! events)
       (loop later (+ at-risk (length events)) c d t*)])))

;; --- Cox partial-likelihood deviance -------------------------------------------

;; R's coxnet.deviance on the observations `rows`, without weights, offset or
;; strata: a procedure from a coefficient vector to 2 (lsat - loglik). loglik
;; is the Breslow partial log-likelihood that the Fortran's loglike computes,
;; from the linear predictor centred at its mean, and lsat its saturated value.
;; As in R, censored times are first nudged up by 100 * .Machine$double.eps, so
;; that a subject censored at an event time stays in that event's risk set.
(define (cox-deviance x rows response)
  (define xs (design-matrix-select-rows x rows))
  (define n (length rows))
  (define times
    (for/vector #:length n ([i (in-list rows)])
      (define t+d (vector-ref response i))
      (fl+ (car t+d) (fl* (fl- 1.0 (cdr t+d)) (fl* 100.0 double-eps)))))
  (define events (for/vector #:length n ([i (in-list rows)]) (cdr (vector-ref response i))))
  (define event-rows (for/list ([d (in-vector events)] [i (in-naturals)] #:when (fl= d 1.0)) i))
  ;; The distinct event times, latest first, and the number of events at each.
  (define event-times
    (sort (remove-duplicates (for/list ([i (in-list event-rows)]) (vector-ref times i)) =) >))
  (define events-at
    (for/list ([t (in-list event-times)])
      (for/sum ([i (in-list event-rows)] #:when (fl= (vector-ref times i) t)) 1.0)))
  (define lsat (fl- 0.0 (r-sum (for/list ([dk (in-list events-at)]) (fl* dk (fllog dk))))))
  (define latest-first (sort (range n) > #:key (lambda (i) (vector-ref times i))))
  (define fmax (fllog (fl* 0.1 1.7976931348623157e308)))
  (lambda (beta)
    (define eta (for/list ([i (in-range n)]) (linear-predictor xs i 0.0 beta)))
    (define mean-eta (fl/ (r-sum eta) (->fl n)))
    (define f (for/vector #:length n ([e (in-list eta)]) (fl- e mean-eta)))
    ;; Latest first, each observation joins the risk set before the event
    ;; times at or below its own are reached.
    (define risk-terms
      (for/fold ([terms '()] [risk 0.0] [order latest-first] #:result terms)
                ([t (in-list event-times)]
                 [dk (in-list events-at)])
        (define-values (joining later)
          (splitf-at order (lambda (i) (fl>= (vector-ref times i) t))))
        (define risk*
          (for/fold ([h risk]) ([i (in-list joining)])
            (fl+ h (flexp (flmax (fl- 0.0 fmax) (flmin (vector-ref f i) fmax))))))
        (values (cons (fl* dk (fllog risk*)) terms) risk* later)))
    (define loglik
      (fl- (r-sum (for/list ([i (in-list event-rows)]) (vector-ref f i)))
           (r-sum risk-terms)))
    (fl* 2.0 (fl- lsat loglik))))

;; --- R's arithmetic ------------------------------------------------------------

;; The sum of a list of flonums as R's sum gives it. R accumulates in extended
;; precision, which Neumaier's compensated sum approximates closely; this keeps
;; values that are equal in R, such as tied misclassification rates, equal here.
(define (r-sum xs)
  (let loop ([xs xs] [s 0.0] [c 0.0] [plain 0.0])
    (cond
      [(null? xs) (if (flrational? plain) (fl+ s c) plain)]
      [else
       (define x (car xs))
       (define t (fl+ s x))
       (loop (cdr xs)
             t
             (if (fl>= (flabs s) (flabs x))
                 (fl+ c (fl+ (fl- s t) x))
                 (fl+ c (fl+ (fl- x t) s)))
             (fl+ plain x))])))

(define (flrational? x) (fl< (flabs x) +inf.0))

(define (nan? x) (not (fl= x x)))

;; R's weighted.mean(xs, ws, na.rm = TRUE): a NaN in xs is dropped with its
;; weight.
(define (weighted-mean xs ws)
  (define-values (products weights)
    (for/lists (products weights)
               ([x (in-list xs)] [w (in-list ws)] #:unless (nan? x))
      (values (fl* x w) w)))
  (fl/ (r-sum products) (r-sum weights)))

;; --- cv.glmnet -------------------------------------------------------------------

;; Cross-validates a family's path fitter, as R's cv.glmnet.
;;   x        : the design matrix of all the data
;;   response : per observation, what the losses read: a flonum (Gaussian,
;;              binomial, Poisson), a class label (multinomial), a pair of
;;              flonums (time . status) (Cox), or a vector of flonums
;;              (multi-response)
;;   fit-all  : fits the path to all the data
;;   fit-rows : fits the path to the rows at a list of indices, at the user's
;;              lambdas or, if there are none, at its own automatic ones
(define (cross-validate who x response fit-all fit-rows
                        #:measure measure
                        #:nfolds nfolds
                        #:fold-ids fold-ids
                        #:grouped? grouped?)
  (define n (design-matrix-nrows x))
  (define folds (resolve-folds who n nfolds fold-ids))
  (define k (add1 (for/fold ([m 0]) ([f (in-vector folds)]) (max m f))))
  (define path (fit-all))
  (define family (glmnet-path-family path))
  (check-training-folds who family response folds k)
  (define held-out
    (for/vector #:length k ([f (in-range k)])
      (for/list ([g (in-vector folds)] [i (in-naturals)] #:when (= g f)) i)))
  (define fold-paths
    (for/vector #:length k ([f (in-range k)])
      (fit-rows (for/list ([g (in-vector folds)] [i (in-naturals)] #:unless (= g f)) i))))
  (define per-fold (/ n k))
  (define measure*
    (cond
      [(and (eq? measure 'auc) (< per-fold 10))
       (log-warning "~a: fewer than 10 observations per fold for 'auc; using 'deviance instead"
                    who)
       'deviance]
      [else measure]))
  (define-values (raw weights counts grouped-raw?)
    (cond
      [(eq? family 'cox)
       (cox-losses who x response path fold-paths held-out folds measure* grouped? per-fold)]
      [(eq? measure* 'auc)
       (auc-losses response (fold-predictions x path fold-paths held-out) held-out)]
      [else
       (define loss (observation-loss family measure*))
       (values (for/vector ([at-lambda (in-vector (fold-predictions x path fold-paths held-out))])
                 (for/list ([y (in-vector response)] [eta (in-vector at-lambda)])
                   (loss y eta)))
               (for/list ([i (in-range n)]) 1.0)
               n
               grouped?)]))
  (define grouped-stats?
    (cond
      [(and grouped-raw? (< per-fold 3))
       (log-warning "~a: fewer than 3 observations per fold; the folds are not grouped" who)
       #f]
      [else grouped-raw?]))
  (define-values (stats-raw stats-weights stats-counts)
    (if grouped-stats?
        (values (for/vector ([at-lambda (in-vector raw)]) (fold-means at-lambda held-out))
                (for/list ([rows (in-vector held-out)]) (->fl (length rows)))
                k)
        (values raw weights counts)))
  (define nzero (nonzero-counts path))
  ;; cvstats, keeping only the lambdas whose cvsd is defined.
  (define rows
    (for*/list ([(at-lambda l) (in-parallel (in-vector stats-raw) (in-naturals))]
                [cvm (in-value (weighted-mean at-lambda stats-weights))]
                [count (in-value (if (vector? stats-counts)
                                     (vector-ref stats-counts l)
                                     stats-counts))]
                [cvsd (in-value
                       (flsqrt (fl/ (weighted-mean (for/list ([v (in-list at-lambda)])
                                                     (square (fl- v cvm)))
                                                   stats-weights)
                                    (->fl (sub1 count)))))]
                #:unless (nan? cvsd))
      (list (vector-ref (glmnet-path-lambda path) l) cvm cvsd (vector-ref nzero l))))
  (when (null? rows)
    (raise-arguments-error who "no lambda has a defined cross-validation error"
                           "measure" measure*))
  (define (column i)
    (for/vector #:length (length rows) ([row (in-list rows)]) (list-ref row i)))
  (define cv-lambda (column 0))
  (define cvm (column 1))
  (define cvsd (column 2))
  (define-values (lambda-min index-min lambda-1se index-1se)
    (optimal-lambdas cv-lambda cvm cvsd measure*))
  (glmnet-cv cv-lambda cvm cvsd
             (for/vector ([m (in-vector cvm)] [s (in-vector cvsd)]) (fl+ m s))
             (for/vector ([m (in-vector cvm)] [s (in-vector cvsd)]) (fl- m s))
             (column 3)
             measure* (measure-name family measure*) path
             lambda-min lambda-1se index-min index-1se
             (vector->list folds)))

;; Per lambda, per observation, the link prediction of the fold path that did
;; not see the observation, at the full-data lambdas: R's buildPredmat with
;; alignment = "lambda".
(define (fold-predictions x path fold-paths held-out)
  (define lams (vector->list (glmnet-path-lambda path)))
  (define n (design-matrix-nrows x))
  (define preds (for/vector #:length (length lams) ([l (in-list lams)]) (make-vector n #f)))
  (for ([fold-path (in-vector fold-paths)]
        [rows (in-vector held-out)])
    (for ([at-lambda (in-list (predict fold-path (design-matrix-select-rows x rows)
                                       #:lambda lams))]
          [out (in-vector preds)])
      (for ([i (in-list rows)] [eta (in-list at-lambda)])
        (vector-set! out i eta))))
  preds)

;; R's cvcompute: the mean loss of each fold's held-out observations, with an
;; infinite loss dropped as missing.
(define (fold-means at-lambda held-out)
  (define losses (list->vector at-lambda))
  (for/list ([rows (in-vector held-out)])
    (weighted-mean (for/list ([i (in-list rows)])
                     (define v (vector-ref losses i))
                     (if (flrational? v) v +nan.0))
                   (for/list ([i (in-list rows)]) 1.0))))

;; cv.lognet's 'auc: per lambda, the AUC of each fold, weighted by the fold's
;; size.
(define (auc-losses response preds held-out)
  (values (for/vector ([at-lambda (in-vector preds)])
            (for/list ([rows (in-vector held-out)])
              (concordance (for/vector ([i (in-list rows)]) (vector-ref response i))
                           (for/vector ([i (in-list rows)]) 1.0)
                           (for/vector ([i (in-list rows)]) (sigmoid (vector-ref at-lambda i))))))
          (for/list ([rows (in-vector held-out)]) (->fl (length rows)))
          (vector-length held-out)
          #f))

;; cv.coxnet with buildPredmat.coxnetlist: per lambda, per fold, either the
;; deviance divided by the fold's size (grouped, of all the data less that of
;; the training data; otherwise of the held-out fold) or the C-index of the
;; held-out fold, with the folds weighted by their sizes.
(define (cox-losses who x response path fold-paths held-out folds measure grouped? per-fold)
  (define lams (vector->list (glmnet-path-lambda path)))
  (define k (vector-length held-out))
  (define sizes (for/list ([rows (in-vector held-out)]) (->fl (length rows))))
  (define per-fold-columns
    (case measure
      [(deviance)
       (define grouped?*
         (cond
           [(and (not grouped?) (< per-fold 10))
            (log-warning "~a: fewer than 10 observations per fold; the Cox deviance is grouped"
                         who)
            #t]
           [else grouped?]))
       (define all (and grouped?* (cox-deviance x (range (design-matrix-nrows x)) response)))
       (for/list ([fold-path (in-vector fold-paths)]
                  [rows (in-vector held-out)]
                  [size (in-list sizes)]
                  [f (in-naturals)])
         (define betas (coef fold-path #:lambda lams))
         (define deviance
           (cond
             [grouped?*
              (define training
                (cox-deviance x
                              (for/list ([g (in-vector folds)] [i (in-naturals)] #:unless (= g f))
                                i)
                              response))
              (lambda (beta) (fl- (all beta) (training beta)))]
             [else (cox-deviance x rows response)]))
         (for/vector ([beta (in-list betas)])
           (fl/ (deviance beta) size)))]
      [(C)
       (for/list ([fold-path (in-vector fold-paths)]
                  [rows (in-vector held-out)])
         (define times (for/vector ([i (in-list rows)]) (car (vector-ref response i))))
         (define statuses (for/vector ([i (in-list rows)]) (cdr (vector-ref response i))))
         (for/vector ([at-lambda (in-list (predict fold-path (design-matrix-select-rows x rows)
                                                   #:lambda lams))])
           (concordance times statuses
                        (for/vector ([eta (in-list at-lambda)]) (fl- 0.0 eta)))))]))
  (define raw
    (for/vector #:length (length lams) ([l (in-range (length lams))])
      (for/list ([column (in-list per-fold-columns)]) (vector-ref column l))))
  (values raw
          sizes
          (if (eq? measure 'deviance)
              (for/vector ([at-lambda (in-vector raw)])
                (- k (for/sum ([v (in-list at-lambda)]) (if (nan? v) 1 0))))
              k)
          #f))

;; R's nzero: the number of predictors in the model at each lambda. For the
;; multinomial it is the median over the classes, rounded up. For the
;; multi-response Gaussian it is the number of nonzero entries in the first
;; response's coefficients, counting its intercept, as R's
;; predict(type = "nonzero") counts them.
(define (nonzero-counts path)
  (define (nonzero v) (for/sum ([b (in-vector v)]) (if (zero? b) 0 1)))
  (case (glmnet-path-family path)
    [(multinomial)
     (for/vector ([groups (in-vector (glmnet-path-coefficients path))])
       (define counts (sort (for/list ([beta (in-vector groups)]) (nonzero beta)) <))
       (define m (length counts))
       (ceiling (if (odd? m)
                    (list-ref counts (quotient m 2))
                    (/ (+ (list-ref counts (sub1 (quotient m 2))) (list-ref counts (quotient m 2)))
                       2))))]
    [(mgaussian)
     (for/vector ([groups (in-vector (glmnet-path-coefficients path))]
                  [a0 (in-vector (glmnet-path-intercepts path))])
       (+ (if (zero? (vector-ref a0 0)) 0 1) (nonzero (vector-ref groups 0))))]
    [else (glmnet-path-df path)]))

;; R's getOptcv.glmnet: lambda.min is the largest lambda at which cvm (negated
;; for 'auc and 'C, which are better when larger) is smallest, and lambda.1se
;; the largest lambda at which it is at most that minimum plus its cvsd; each
;; comes with the index at which it first appears.
(define (optimal-lambdas lambdas cvm cvsd measure)
  (define score
    (if (memq measure '(auc C))
        (for/vector ([m (in-vector cvm)]) (fl- 0.0 m))
        cvm))
  (define (largest-lambda-within bound)
    (define best
      (for/fold ([best -inf.0])
                ([l (in-vector lambdas)] [s (in-vector score)] #:when (fl<= s bound))
        (flmax best l)))
    (values best (for/first ([l (in-vector lambdas)] [i (in-naturals)] #:when (fl= l best)) i)))
  (define-values (lambda-min index-min)
    (largest-lambda-within (for/fold ([m +inf.0]) ([s (in-vector score)]) (flmin m s))))
  (define-values (lambda-1se index-1se)
    (largest-lambda-within (fl+ (vector-ref score index-min) (vector-ref cvsd index-min))))
  (values lambda-min index-min lambda-1se index-1se))

;; --- printing ------------------------------------------------------------------

;; As R's print.cv.glmnet: the measure, then lambda.min and lambda.1se with
;; their index, cvm, cvsd and nzero. `call`, when given, follows the family.
(define (write-cv cv port [call #f])
  (define (number x) (if (flrational? x) (signif x) (number->string x)))
  (fprintf port "#<glmnet-cv:~a~a ~a"
           (glmnet-path-family (glmnet-cv-path cv)) (call-suffix call) (glmnet-cv-name cv))
  (write-table
   (cons '("" "Lambda" "Index" "Measure" "SE" "Nonzero")
         (for/list ([label (in-list '("min" "1se"))]
                    [i (in-list (list (glmnet-cv-index-min cv) (glmnet-cv-index-1se cv)))])
           (list label
                 (number (vector-ref (glmnet-cv-lambda cv) i))
                 (number->string i)
                 (number (vector-ref (glmnet-cv-cvm cv) i))
                 (number (vector-ref (glmnet-cv-cvsd cv) i))
                 (number->string (vector-ref (glmnet-cv-nzero cv) i)))))
   port)
  (write-string ">" port))
