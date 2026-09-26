#lang racket/base

;; Unit tests for cross-validation (#27): fold assignment, the CV error against
;; a direct computation, R's rules for lambda.min and lambda.1se, the settings
;; R changes when folds are small, the losses, the generic interface on a CV
;; result, printing, and the errors. R parity for every family and type
;; measure lives in parity-test.rkt.

(module+ test
  (require rackunit
           racket/list
           racket/logging
           racket/math
           racket/vector
           glmnet
           (submod glmnet/core/cv support))

  (define n 30)
  (define X
    (for/list ([i (in-range n)])
      (list (exact->inexact (modulo (* 7 i) 11))
            (exact->inexact (modulo (* 3 i) 5))
            (sin (* 1.0 i)))))
  (define y
    (for/list ([row (in-list X)] [i (in-naturals)])
      (+ 1.0 (* 2.0 (first row)) (- (second row)) (* 3.0 (sin (* 3.0 i))))))
  (define folds (for/list ([i (in-range n)]) (modulo i 5)))
  (define new-rows '((3.0 1.0 0.5) (8.0 4.0 -0.5)))

  (define labels
    (for/list ([row (in-list X)] [i (in-naturals)])
      (if (> (+ (first row) (* 4.0 (sin (* 5.0 i)))) 5.0) 1 0)))
  (define classes
    (for/list ([row (in-list X)] [i (in-naturals)])
      (modulo (+ (exact-round (first row)) (if (> (sin (* 2.0 i)) 0.5) 1 0)) 3)))
  (define times
    (for/list ([row (in-list X)] [i (in-naturals)])
      (+ 1.0 (modulo (* 13 i) 17) (first row))))
  (define statuses (for/list ([i (in-range n)]) (if (= (modulo i 4) 3) 0 1)))
  (define counts
    (for/list ([row (in-list X)] [i (in-naturals)])
      (exact-round (exp (+ 0.2 (* 0.2 (first row)) (* 0.3 (sin (* 3.0 i))))))))
  (define Y
    (for/list ([row (in-list X)] [yi (in-list y)] [i (in-naturals)])
      (list yi (+ (* 0.5 (second row)) (cos (* 1.0 i))))))

  (define (check-contract-error thunk . patterns)
    (check-exn
     (lambda (e)
       (and (exn:fail:contract? e)
            (for/and ([p (in-list patterns)])
              (regexp-match? p (exn-message e)))))
     thunk))

  (define (check-vector= got want [tol 1e-12])
    (check-equal? (vector-length got) (vector-length want))
    (for ([g (in-vector got)] [w (in-vector want)])
      (check-= g w tol)))

  (define (rows-of xs rows) (for/list ([i (in-list rows)]) (list-ref xs i)))
  (define (mean xs) (/ (apply + xs) (length xs)))

  ;; --- folds -------------------------------------------------------------------

  (test-case "random-fold-ids: every fold, sizes as even as possible"
    (define ids (random-fold-ids 23 5))
    (check-equal? (length ids) 23)
    (check-equal? (sort (remove-duplicates ids) <) '(0 1 2 3 4))
    (check-equal? (for/list ([f (in-range 5)]) (count (lambda (i) (= i f)) ids))
                  '(5 5 5 4 4))
    (check-equal? (length (remove-duplicates (random-fold-ids 30))) 10))

  (test-case "random-fold-ids draws from current-pseudo-random-generator"
    (define (draw seed)
      (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
        (random-seed seed)
        (random-fold-ids 40 4)))
    (check-equal? (draw 7) (draw 7))
    (check-not-equal? (draw 7) (draw 8)))

  (test-case "random-fold-ids needs no more folds than observations"
    (check-contract-error (lambda () (random-fold-ids 3 5))
                          #rx"^random-fold-ids: there are more folds than observations"))

  (test-case "a CV result records its folds; random folds are reproducible"
    (check-equal? (glmnet-cv-fold-ids (elnet-cv X y #:fold-ids folds)) folds)
    (define (cv-with-seed seed)
      (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
        (random-seed seed)
        (elnet-cv X y #:nfolds 6)))
    (define a (cv-with-seed 11))
    (check-equal? (cv-with-seed 11) a)
    (check-equal? (length (remove-duplicates (glmnet-cv-fold-ids a))) 6)
    (check-equal? (elnet-cv X y #:fold-ids (glmnet-cv-fold-ids a)) a))

  ;; --- the CV error, computed directly ---------------------------------------

  ;; Each fold's path, fitted to the other folds with its own lambdas, predicts
  ;; its held-out rows at the full-data lambdas.
  (define (held-out-squared-errors lams)
    (for/fold ([errors (hash)]) ([f (in-range 5)])
      (define out (for/list ([g (in-list folds)] [i (in-naturals)] #:when (= g f)) i))
      (define in (for/list ([g (in-list folds)] [i (in-naturals)] #:unless (= g f)) i))
      (define p (elnet-path (rows-of X in) (rows-of y in)))
      (for/fold ([errors errors])
                ([preds (in-list (predict p (rows-of X out) #:lambda lams))]
                 [l (in-naturals)])
        (for/fold ([errors errors]) ([i (in-list out)] [pred (in-list preds)])
          (hash-set errors (cons l i) (sqr (- (list-ref y i) pred)))))))

  (define cv (elnet-cv X y #:fold-ids folds))
  (define full (elnet-path X y))
  (define lams (vector->list (glmnet-path-lambda full)))
  (define errors (held-out-squared-errors lams))

  (test-case "the path of a CV result is the full-data path"
    (check-equal? (glmnet-cv-path cv) full)
    (check-equal? (glmnet-cv-lambda cv) (glmnet-path-lambda full))
    (check-equal? (glmnet-cv-nzero cv) (glmnet-path-df full)))

  (test-case "grouped: cvm and cvsd from the fold means, weighted by fold size"
    (define-values (cvm cvsd)
      (for/lists (cvm cvsd) ([l (in-range (length lams))])
        (define fold-means
          (for/list ([f (in-range 5)])
            (mean (for/list ([g (in-list folds)] [i (in-naturals)] #:when (= g f))
                    (hash-ref errors (cons l i))))))
        (define m (mean fold-means))
        (values m (sqrt (/ (mean (for/list ([v (in-list fold-means)]) (sqr (- v m)))) 4)))))
    (check-vector= (glmnet-cv-cvm cv) (list->vector cvm))
    (check-vector= (glmnet-cv-cvsd cv) (list->vector cvsd))
    (check-vector= (glmnet-cv-cvup cv) (vector-map + (glmnet-cv-cvm cv) (glmnet-cv-cvsd cv)))
    (check-vector= (glmnet-cv-cvlo cv) (vector-map - (glmnet-cv-cvm cv) (glmnet-cv-cvsd cv))))

  (test-case "ungrouped: cvm and cvsd over the observations"
    (define ungrouped (elnet-cv X y #:fold-ids folds #:grouped? #f))
    (for ([l (in-range (length lams))])
      (define losses (for/list ([i (in-range n)]) (hash-ref errors (cons l i))))
      (define m (mean losses))
      (check-= (vector-ref (glmnet-cv-cvm ungrouped) l) m 1e-12)
      (check-= (vector-ref (glmnet-cv-cvsd ungrouped) l)
               (sqrt (/ (mean (for/list ([v (in-list losses)]) (sqr (- v m)))) (sub1 n)))
               1e-12)))

  (test-case "lambda-min and lambda-1se, and their indices"
    (define cvm (glmnet-cv-cvm cv))
    (define best (for/fold ([m +inf.0]) ([v (in-vector cvm)]) (min m v)))
    (define i-min (index-of (vector->list cvm) best))
    (check-equal? (glmnet-cv-index-min cv) i-min)
    (check-equal? (glmnet-cv-lambda-min cv) (list-ref lams i-min))
    (define bound (+ best (vector-ref (glmnet-cv-cvsd cv) i-min)))
    (define i-1se (for/first ([v (in-vector cvm)] [i (in-naturals)] #:when (<= v bound)) i))
    (check-equal? (glmnet-cv-index-1se cv) i-1se)
    (check-equal? (glmnet-cv-lambda-1se cv) (list-ref lams i-1se))
    (check-true (>= (glmnet-cv-lambda-1se cv) (glmnet-cv-lambda-min cv))))

  (test-case "a user lambda sequence is fitted, largest first, on every fold"
    (define user (elnet-cv X y #:fold-ids folds #:lambda '(0.5 2.0 0.1 1.0)))
    (check-equal? (glmnet-cv-lambda user) #(2.0 1.0 0.5 0.1))
    (define p (elnet-path (rows-of X '(1 2 3 4 6 7 8 9)) (rows-of y '(1 2 3 4 6 7 8 9))
                          #:lambda '(0.5 2.0 0.1 1.0)))
    (check-equal? (glmnet-path-lambda p) #(2.0 1.0 0.5 0.1)))

  ;; --- R's rules for lambda.min and lambda.1se -------------------------------

  (test-case "lambda.min: the largest lambda among tied minima"
    (define-values (lmin imin l1se i1se)
      (optimal-lambdas #(5.0 4.0 3.0 2.0 1.0) #(3.0 2.0 1.0 1.0 2.0) #(0.5 0.5 0.5 0.5 0.5) 'mse))
    (check-equal? (list lmin imin l1se i1se) '(3.0 2 3.0 2)))

  (test-case "lambda.1se: within cvm + cvsd at lambda.min, the bound included"
    (define-values (lmin imin l1se i1se)
      (optimal-lambdas #(3.0 2.0 1.0) #(1.5 2.0 1.0) #(0.1 0.1 0.5) 'deviance))
    (check-equal? (list lmin imin l1se i1se) '(1.0 2 3.0 0)))

  (test-case "'auc and 'C are maximized"
    (for ([measure '(auc C)])
      (define-values (lmin imin l1se i1se)
        (optimal-lambdas #(5.0 4.0 3.0 2.0 1.0) #(0.5 0.9 0.95 0.95 0.9) (make-vector 5 0.06)
                         measure))
      (check-equal? (list lmin imin l1se i1se) '(3.0 2 4.0 1))))

  (test-case "a repeated lambda is reported at its first index, as R's match does"
    (define-values (lmin imin l1se i1se)
      (optimal-lambdas #(2.0 2.0 1.0) #(5.0 1.0 3.0) #(0.1 0.1 0.1) 'mse))
    (check-equal? (list lmin imin l1se i1se) '(2.0 0 2.0 0)))

  ;; --- settings R changes for small folds -----------------------------------

  (define (warnings-of thunk)
    (define messages '())
    (define result
      (with-intercepted-logging
        (lambda (v) (set! messages (cons (vector-ref v 1) messages)))
        thunk
        'warning))
    (values result messages))

  (test-case "fewer than 3 observations per fold: the folds are not grouped"
    (define small-folds (for/list ([i (in-range n)]) (modulo i 12)))
    (define-values (grouped messages)
      (warnings-of (lambda () (elnet-cv X y #:fold-ids small-folds))))
    (check-equal? grouped (elnet-cv X y #:fold-ids small-folds #:grouped? #f))
    (check-true (for/or ([m (in-list messages)])
                  (regexp-match? #rx"elnet-cv: fewer than 3 observations per fold" m))))

  (test-case "fewer than 10 observations per fold: 'auc becomes 'deviance"
    (define-values (auc messages)
      (warnings-of (lambda () (logistic-cv X labels #:fold-ids folds #:type-measure 'auc))))
    (check-eq? (glmnet-cv-measure auc) 'deviance)
    (check-equal? auc (logistic-cv X labels #:fold-ids folds))
    (check-true (for/or ([m (in-list messages)])
                  (regexp-match? #rx"logistic-cv: fewer than 10 observations per fold" m))))

  (test-case "fewer than 10 observations per fold: the Cox deviance is grouped"
    (check-equal? (cox-cv X times statuses #:fold-ids folds #:grouped? #f)
                  (cox-cv X times statuses #:fold-ids folds)))

  ;; --- losses ----------------------------------------------------------------

  (test-case "every family defaults to R's type measure"
    (check-equal? (map glmnet-cv-measure
                       (list cv
                             (logistic-cv X labels #:fold-ids folds)
                             (multinomial-cv X classes #:fold-ids folds)
                             (cox-cv X times statuses #:fold-ids folds)
                             (poisson-cv X counts #:fold-ids folds)
                             (mgaussian-cv X Y #:fold-ids folds)))
                  '(mse deviance deviance deviance deviance mse))
    (check-equal? (map glmnet-cv-name
                       (list cv
                             (elnet-cv X y #:fold-ids folds #:type-measure 'deviance)
                             (logistic-cv X labels #:fold-ids folds #:type-measure 'class)
                             (cox-cv X times statuses #:fold-ids folds #:type-measure 'C)))
                  '("Mean-Squared Error" "Mean-squared Error" "Misclassification Error"
                                         "C-index")))

  (test-case "'class is the fraction of held-out observations misclassified"
    (define cv-class (logistic-cv X labels #:fold-ids folds #:type-measure 'class))
    (define lams (vector->list (glmnet-path-lambda (glmnet-cv-path cv-class))))
    (define wrong
      (for/fold ([wrong (hash)]) ([f (in-range 5)])
        (define out (for/list ([g (in-list folds)] [i (in-naturals)] #:when (= g f)) i))
        (define in (for/list ([g (in-list folds)] [i (in-naturals)] #:unless (= g f)) i))
        (define p (logistic-path (rows-of X in) (rows-of labels in)))
        (for/fold ([wrong wrong])
                  ([probs (in-list (predict p (rows-of X out) #:lambda lams #:type 'response))]
                   [l (in-naturals)])
          (hash-set wrong l (+ (hash-ref wrong l 0)
                               (for/sum ([i (in-list out)] [prob (in-list probs)])
                                 (if (= (if (> prob 0.5) 1 0) (list-ref labels i)) 0 1)))))))
    (for ([l (in-range (length lams))])
      (check-= (vector-ref (glmnet-cv-cvm cv-class) l) (/ (hash-ref wrong l) n) 1e-12)))

  (define (brute-force-concordance times statuses xs)
    (define-values (c d t)
      (for*/fold ([c 0] [d 0] [t 0])
                 ([i (in-range (vector-length times))]
                  [j (in-range (vector-length times))]
                  #:when (and (= (vector-ref statuses i) 1.0)
                              (or (< (vector-ref times i) (vector-ref times j))
                                  (and (= (vector-ref times i) (vector-ref times j))
                                       (= (vector-ref statuses j) 0.0)))))
        (define xi (vector-ref xs i))
        (define xj (vector-ref xs j))
        (cond
          [(< xi xj) (values (add1 c) d t)]
          [(> xi xj) (values c (add1 d) t)]
          [else (values c d (add1 t))])))
    (/ (+ c (/ t 2)) (+ c d t)))

  (test-case "concordance: an event precedes a censored time equal to it; x ties count a half"
    (check-= (concordance #(5.0 5.0 7.0) #(1.0 0.0 1.0) #(-2.0 -1.0 0.0)) 1.0 1e-15)
    (check-= (concordance #(5.0 5.0 7.0) #(1.0 0.0 1.0) #(-1.0 -2.0 0.0)) 0.5 1e-15)
    (check-= (concordance #(5.0 5.0 7.0) #(1.0 1.0 1.0) #(-1.0 -2.0 0.0)) 1.0 1e-15)
    (check-= (concordance #(0.0 1.0 0.0 1.0) #(1.0 1.0 1.0 1.0) #(0.5 0.5 0.2 0.9)) 0.875 1e-15)
    (define ts (for/vector ([t (in-list times)]) (exact->inexact (exact-round (/ t 3)))))
    (define ds (for/vector ([s (in-list statuses)]) (exact->inexact s)))
    (define xs (for/vector ([row (in-list X)]) (exact->inexact (exact-round (third row)))))
    (check-= (concordance ts ds xs) (brute-force-concordance ts ds xs) 1e-15))

  (test-case "the Cox deviance is 2 (lsat - loglik) with Breslow's partial likelihood"
    (define beta #(0.1 -0.2 0.3))
    (define ts (map (lambda (t) (exact->inexact (exact-round (/ t 3)))) times))
    (define eta (for/list ([row (in-list X)]) (for/sum ([x (in-list row)] [b (in-vector beta)]) (* x b))))
    (define event-times (remove-duplicates (for/list ([t (in-list ts)] [s (in-list statuses)] #:when (= s 1)) t)))
    (define-values (loglik lsat)
      (for/fold ([loglik (for/sum ([e (in-list eta)] [s (in-list statuses)] #:when (= s 1)) e)]
                 [lsat 0.0])
                ([t (in-list event-times)])
        (define d (for/sum ([t2 (in-list ts)] [s (in-list statuses)] #:when (and (= s 1) (= t2 t))) 1))
        (define risk (for/sum ([t2 (in-list ts)] [e (in-list eta)] #:when (>= t2 t)) (exp e)))
        (values (- loglik (* d (log risk))) (- lsat (* d (log d))))))
    (define response (for/vector ([t (in-list ts)] [s (in-list statuses)]) (cons t (exact->inexact s))))
    (define deviance (cox-deviance (rows->design-matrix X) (range n) response))
    (check-= (deviance beta) (* 2 (- lsat loglik)) 1e-10))

  (test-case "multinomial nzero: the median over the classes, rounded up"
    (define mcv (multinomial-cv X classes #:fold-ids folds))
    (define p (glmnet-cv-path mcv))
    (check-equal? (glmnet-cv-nzero mcv)
                  (for/vector ([groups (in-vector (glmnet-path-coefficients p))])
                    (define per-class
                      (sort (for/list ([beta (in-vector groups)])
                              (for/sum ([b (in-vector beta)]) (if (zero? b) 0 1)))
                            <))
                    (second per-class))))

  (test-case "multi-response nzero counts the first response's intercept, as R does"
    (define gcv (mgaussian-cv X Y #:fold-ids folds))
    (define p (glmnet-cv-path gcv))
    (check-equal? (vector-ref (glmnet-cv-nzero gcv) 0) 1)
    (check-equal? (glmnet-cv-nzero gcv) (vector-map add1 (glmnet-path-df p))))

  (test-case "every family and type measure gives a finite curve"
    (for ([result (list (elnet-cv X y #:fold-ids folds #:type-measure 'mae)
                        (logistic-cv X labels #:fold-ids folds #:type-measure 'mse)
                        (logistic-cv X labels #:fold-ids folds #:type-measure 'mae)
                        (multinomial-cv X classes #:fold-ids folds #:type-measure 'class)
                        (multinomial-cv X classes #:fold-ids folds #:type-measure 'mse)
                        (multinomial-cv X classes #:fold-ids folds #:type-measure 'mae)
                        (cox-cv X times statuses #:fold-ids folds #:type-measure 'C)
                        (poisson-cv X counts #:fold-ids folds #:type-measure 'mse)
                        (poisson-cv X counts #:fold-ids folds #:type-measure 'mae)
                        (mgaussian-cv X Y #:fold-ids folds #:type-measure 'mae))])
      (check-true (for/and ([v (in-vector (glmnet-cv-cvm result))]) (rational? v)))
      (check-true (for/and ([v (in-vector (glmnet-cv-cvsd result))]) (rational? v)))))

  ;; --- the generic interface ---------------------------------------------------

  (test-case "predict and coef default to lambda-1se, as R's predict.cv.glmnet"
    (check-true (glmnet-model? cv))
    (check-eq? (glmnet-model->path cv) (glmnet-cv-path cv))
    (check-equal? (glmnet-model-default-lambda cv) (glmnet-cv-lambda-1se cv))
    (check-equal? (coef cv) (coef full #:lambda (glmnet-cv-lambda-1se cv)))
    (check-equal? (predict cv new-rows) (predict full new-rows #:lambda (glmnet-cv-lambda-1se cv)))
    (check-equal? (coef cv #:lambda 'lambda-1se) (coef cv)))

  (test-case "'lambda-min, a number or a list"
    (check-equal? (coef cv #:lambda 'lambda-min) (coef full #:lambda (glmnet-cv-lambda-min cv)))
    (check-equal? (predict cv new-rows #:lambda 'lambda-min)
                  (predict full new-rows #:lambda (glmnet-cv-lambda-min cv)))
    (check-equal? (coef cv #:lambda 0.3) (coef full #:lambda 0.3))
    (check-equal? (predict cv new-rows #:lambda '(1.0 0.1))
                  (predict full new-rows #:lambda '(1.0 0.1))))

  (test-case "glmnet-model-named-lambda: a CV result names two lambdas, other models none"
    (check-equal? (glmnet-model-named-lambda cv 'lambda-min) (glmnet-cv-lambda-min cv))
    (check-equal? (glmnet-model-named-lambda cv 'lambda-1se) (glmnet-cv-lambda-1se cv))
    (check-false (glmnet-model-named-lambda full 'lambda-min))
    (check-false (glmnet-model-named-lambda (lasso X y #:lambda 0.1) 'lambda-1se)))

  (test-case "a named lambda needs a cross-validated model"
    (check-contract-error (lambda () (coef full #:lambda 'lambda-min))
                          #rx"^coef: only a cross-validated model has a named lambda")
    (check-contract-error (lambda () (predict (lasso X y #:lambda 0.1) X #:lambda 'lambda-1se))
                          #rx"^predict: only a cross-validated model has a named lambda")
    (check-contract-error (lambda () (coef cv #:lambda 'lambda-max)) #rx"coef"))

  (test-case "deviance-ratio is the path's at lambda-1se"
    (check-equal? (deviance-ratio cv)
                  (vector-ref (glmnet-path-dev-ratio full)
                              (index-of lams (glmnet-cv-lambda-1se cv)))))

  (test-case "a CV result prints the measure and R's lambda.min and lambda.1se table"
    (define lines (regexp-split #rx"\n" (format "~a" cv)))
    (check-equal? (length lines) 4)
    (check-equal? (first lines) "#<glmnet-cv:gaussian Mean-Squared Error")
    (check-regexp-match #rx"^ +Lambda +Index +Measure +SE +Nonzero$" (second lines))
    (check-regexp-match (pregexp (format "^  min +[0-9.e-]+ +~a +[0-9.e-]+ +[0-9.e-]+ +~a$"
                                         (glmnet-cv-index-min cv)
                                         (vector-ref (glmnet-cv-nzero cv) (glmnet-cv-index-min cv))))
                        (third lines))
    (check-regexp-match #rx"^  1se .*>$" (fourth lines))
    (check-equal? (format "~s" cv) (format "~a" cv)))

  ;; --- errors ------------------------------------------------------------------

  (test-case "fold ids: one per row, at least 3 folds, every fold used"
    (check-contract-error (lambda () (elnet-cv X y #:fold-ids (cdr folds)))
                          #rx"^elnet-cv: fold-ids does not have one entry per row of X")
    (check-contract-error (lambda () (elnet-cv X y #:fold-ids (map (lambda (f) (modulo f 2)) folds)))
                          #rx"^elnet-cv: cross-validation needs at least 3 folds")
    (check-contract-error (lambda () (elnet-cv X y #:fold-ids (map (lambda (f) (* 2 f)) folds)))
                          #rx"^elnet-cv: a fold has no observations")
    (check-contract-error (lambda () (elnet-cv X y #:fold-ids (cons -1 (cdr folds)))) #rx"elnet-cv"))

  (test-case "#:nfolds: at least 3, and no more than the observations"
    (check-contract-error (lambda () (elnet-cv X y #:nfolds 2)) #rx"elnet-cv")
    (check-contract-error (lambda () (poisson-cv X counts #:nfolds 31))
                          #rx"^poisson-cv: there are more folds than observations"))

  (test-case "each family accepts R's type measures for it and no others"
    (check-contract-error (lambda () (elnet-cv X y #:type-measure 'auc)) #rx"elnet-cv")
    (check-contract-error (lambda () (logistic-cv X labels #:type-measure 'C)) #rx"logistic-cv")
    (check-contract-error (lambda () (multinomial-cv X classes #:type-measure 'auc))
                          #rx"multinomial-cv")
    (check-contract-error (lambda () (cox-cv X times statuses #:type-measure 'mse)) #rx"cox-cv")
    (check-contract-error (lambda () (poisson-cv X counts #:type-measure 'class)) #rx"poisson-cv")
    (check-contract-error (lambda () (mgaussian-cv X Y #:type-measure 'class)) #rx"mgaussian-cv"))

  (test-case "a user lambda sequence needs at least two values, as in R"
    (check-contract-error (lambda () (elnet-cv X y #:lambda '(0.1))) #rx"elnet-cv"))

  (test-case "every training fold must hold every class, or an event"
    (define one-fold-ones (for/list ([f (in-list folds)]) (if (= f 0) 1 0)))
    (check-contract-error (lambda () (logistic-cv X one-fold-ones #:fold-ids folds))
                          #rx"^logistic-cv: the training data of a fold has no class")
    (define one-fold-twos (for/list ([f (in-list folds)] [c (in-list classes)])
                            (if (= f 2) 2 (modulo c 2))))
    (check-contract-error (lambda () (multinomial-cv X one-fold-twos #:fold-ids folds))
                          #rx"^multinomial-cv: the training data of a fold has no class")
    (define one-fold-events (for/list ([f (in-list folds)]) (if (= f 4) 1 0)))
    (check-contract-error (lambda () (cox-cv X times one-fold-events #:fold-ids folds))
                          #rx"^cox-cv: the training data of a fold has no event"))

  (test-case "data errors name the CV procedure"
    (check-contract-error (lambda () (elnet-cv X (cdr y)))
                          #rx"^elnet-cv: y does not have one entry per row of X")
    (check-contract-error (lambda () (mgaussian-cv X (cdr Y)))
                          #rx"^mgaussian-cv: Y does not have one row per row of X")
    (check-exn #rx"^cox-cv: at least one observation must be an event"
               (lambda () (cox-cv X times (map (lambda (s) 0) statuses))))))
