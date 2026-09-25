#lang racket/base

;; Parity tests: our bindings must reproduce R glmnet's numbers on real datasets.
;;
;; Data-driven. Each golden JSON declares a (dataset, family, alpha, lambda)
;; fixture and R's reference intercept / coefficients / dev.ratio / predictions
;; (scripts/r-parity/gen-reference.R generates them with R glmnet). We load the
;; same committed dataset, fit the matching family, and check we agree within the
;; tolerances recorded in the golden's `meta`.
;;
;; Goldens are generated on demand, never committed: the Nix `checks.parity` gate
;; regenerates them with the pinned R glmnet and points GLMNET_PARITY_GOLDENS at
;; them. When that env var is unset and no goldens are present (the offline /
;; package-catalog `raco test` path, which has no R), this test SKIPS -- parity is
;; an R-backed CI gate, not an offline unit test. The committed datasets under
;; glmnet/private/data/ are the fixed inputs both R and our bindings read.

(module+ test
  (require rackunit
           json
           racket/list
           racket/runtime-path
           glmnet
           (file "../private/demo-utils.rkt"))

  (define-runtime-path committed-goldens "../../scripts/r-parity/goldens")
  (define goldens-dir
    (let ([e (getenv "GLMNET_PARITY_GOLDENS")])
      (if (and e (positive? (string-length e))) e committed-goldens)))

  (define (load-dataset name)
    (case name
      [("longley")    (call-with-values load-longley list)]
      [("wdbc")       (call-with-values load-wdbc list)]
      [("iris")       (call-with-values load-iris list)]
      [("veteran")    (call-with-values load-veteran list)]
      [("warpbreaks") (call-with-values load-warpbreaks list)]
      [("linnerud")   (call-with-values load-linnerud list)]
      [else (error 'parity-test "unknown dataset: ~a" name)]))

  ;; absolute tolerance with a relative fallback for large magnitudes
  (define (close? a b tol)
    (<= (abs (- a b)) (+ tol (* tol (abs b)))))
  (define (check-close a b tol msg)
    (check-true (close? (exact->inexact a) (exact->inexact b) tol)
                (format "~a: got ~a, want ~a (tol ~a)" msg a b tol)))
  (define (check-vec-close got expected tol msg)
    (check-equal? (length got) (length expected) (format "~a: length mismatch" msg))
    (for ([g (in-list got)] [e (in-list expected)] [i (in-naturals)])
      (check-close g e tol (format "~a[~a]" msg i))))

  ;; compare a matrix (list of rows) element-wise
  (define (check-mat-close got expected tol msg)
    (check-equal? (length got) (length expected) (format "~a: row count" msg))
    (for ([g (in-list got)] [e (in-list expected)] [i (in-naturals)])
      (check-vec-close g e tol (format "~a[row ~a]" msg i))))

  ;; linear predictor intercept + X.beta for each row (gaussian fitted values)
  (define (linear-predictions intercept coefs X)
    (for/list ([row (in-list X)])
      (for/fold ([acc intercept]) ([c (in-vector coefs)] [x (in-list row)])
        (+ acc (* c (exact->inexact x))))))

  (define (run-golden g)
    (define id     (hash-ref g 'id))
    (define family (hash-ref g 'family))
    (define alpha  (hash-ref g 'alpha))
    (define lambda (hash-ref g 'lambda))
    (define thresh (hash-ref g 'thresh))
    (define tols   (hash-ref (hash-ref g 'meta) 'tolerances))
    (define ctol   (hash-ref tols 'coef))
    (define itol   (hash-ref tols 'intercept))
    (define dtol   (hash-ref tols 'dev_ratio))
    (define ptol   (hash-ref tols 'pred))
    (define ds     (load-dataset (hash-ref g 'dataset)))
    (define X (first ds))
    (test-case id
      (cond
        [(string=? family "gaussian")
         (define y (second ds))
         (define r (elnet-fit X y #:lambda lambda #:alpha alpha #:thresh thresh
                              #:intercept? (hash-ref g 'fit_intercept #t)))
         (check-close (elnet-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-close (elnet-result-intercept r) (hash-ref g 'intercept) itol "intercept")
         (check-vec-close (vector->list (elnet-result-coefficients r))
                          (hash-ref g 'coefficients) ctol "coef")
         (check-close (elnet-result-r-squared r) (hash-ref g 'dev_ratio) dtol "dev-ratio")
         (check-vec-close (linear-predictions (elnet-result-intercept r)
                                              (elnet-result-coefficients r) X)
                          (hash-ref g 'predictions) ptol "predictions")]
        [(string=? family "binomial")
         (define y (second ds))
         (define r (logistic-fit X y #:lambda lambda #:alpha alpha #:thresh thresh))
         (check-close (logistic-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-close (logistic-result-intercept r) (hash-ref g 'intercept) itol "intercept")
         (check-vec-close (vector->list (logistic-result-coefficients r))
                          (hash-ref g 'coefficients) ctol "coef")
         (check-close (logistic-result-dev-ratio r) (hash-ref g 'dev_ratio) dtol "dev-ratio")
         (check-vec-close (logistic-predict-proba r X)
                          (hash-ref g 'predictions) ptol "proba")]
        [(string=? family "multinomial")
         (define y (second ds))
         (define r (multinomial-fit X y #:lambda lambda #:alpha alpha #:thresh thresh))
         (check-close (multinomial-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-close (multinomial-result-dev-ratio r) (hash-ref g 'dev_ratio) dtol "dev-ratio")
         ;; Probabilities are the parameterization-invariant target. The raw
         ;; per-class intercepts differ from R by a uniform shift (the symmetric
         ;; multinomial param), which softmax absorbs -- so we skip them.
         (check-mat-close (multinomial-predict-proba r X) (hash-ref g 'probabilities) ptol "proba")
         (for ([k (in-naturals)] [ck (in-list (hash-ref g 'coefficients))])
           (check-vec-close (vector->list (vector-ref (multinomial-result-coefficients r) k))
                            ck ctol (format "coef[class ~a]" k)))]
        [(string=? family "poisson")
         (define y (second ds))
         (define r (poisson-fit X y #:lambda lambda #:alpha alpha #:thresh thresh))
         (check-close (poisson-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-close (poisson-result-intercept r) (hash-ref g 'intercept) itol "intercept")
         (check-vec-close (vector->list (poisson-result-coefficients r))
                          (hash-ref g 'coefficients) ctol "coef")
         (check-close (poisson-result-dev-ratio r) (hash-ref g 'dev_ratio) dtol "dev-ratio")
         (check-vec-close (poisson-predict-mean r X) (hash-ref g 'predictions) ptol "predict-mean")]
        [(string=? family "cox")
         (define times (second ds))
         (define statuses (third ds))
         (define r (cox-fit X times statuses #:lambda lambda #:alpha alpha #:thresh thresh))
         ;; For dense, non-stratified, right-censored data R 4.1.10 fits cox via
         ;; the classic .Fortran("coxnet") -- the same routine we bind -- but our
         ;; vendored glmnet5.f90 snapshot's coxnet predates upstream refinements
         ;; (veteran has tied event times, where they bite), so the fits agree to
         ;; ~1e-3 (identical support and signs, dev-ratio ~1e-4) vs the other
         ;; families' ~1e-13. Re-vendoring a newer coxnet would tighten this; see #21.
         (define cox-ctol 5e-3)
         (define cox-dtol 1e-3)
         (check-close (cox-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-vec-close (vector->list (cox-result-coefficients r))
                          (hash-ref g 'coefficients) cox-ctol "coef")
         (check-close (cox-result-dev-ratio r) (hash-ref g 'dev_ratio) cox-dtol "dev-ratio")]
        [(string=? family "mgaussian")
         (define Y (second ds))
         (define r (mgaussian-fit X Y #:lambda lambda #:alpha alpha #:thresh thresh))
         (check-close (mgaussian-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-vec-close (vector->list (mgaussian-result-intercepts r))
                          (hash-ref g 'intercepts) itol "intercepts")
         (for ([k (in-naturals)] [ck (in-list (hash-ref g 'coefficients))])
           (check-vec-close (vector->list (vector-ref (mgaussian-result-coefficients r) k))
                            ck ctol (format "coef[response ~a]" k)))
         (check-close (mgaussian-result-r-squared r) (hash-ref g 'r_squared) dtol "r-squared")
         (check-mat-close (mgaussian-predict r X) (hash-ref g 'predictions) ptol "predictions")]
        [else (fail (format "~a: unhandled family ~a" id family))])))

  (define explicit-goldens?
    (let ([e (getenv "GLMNET_PARITY_GOLDENS")]) (and e (positive? (string-length e)) #t)))

  (define golden-files
    (if (directory-exists? goldens-dir)
        (sort (for/list ([p (in-list (directory-list goldens-dir #:build? #t))]
                         #:when (regexp-match? #rx"[.]json$" (path->string p)))
                p)
              string<? #:key path->string)
        '()))

  (cond
    [(pair? golden-files)
     (for ([path (in-list golden-files)])
       (run-golden (call-with-input-file path read-json)))]
    [explicit-goldens?
     ;; The CI gate sets GLMNET_PARITY_GOLDENS; an empty dir there means R
     ;; generation failed -- a real error.
     (test-case "parity goldens present"
       (check-true #f (format "GLMNET_PARITY_GOLDENS=~a contains no golden JSON" goldens-dir)))]
    [else
     ;; No goldens and none requested: skip so the offline / catalog `raco test`
     ;; (no R) still passes. Run via `nix flake check` or `nix run .#gen-goldens`.
     (printf "parity-test: no goldens at ~a; skipping (R-backed gate -- see checks.parity)\n"
             goldens-dir)]))
