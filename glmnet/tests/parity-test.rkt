#lang racket/base

;; Parity tests: our bindings must reproduce R glmnet's numbers on real datasets.
;;
;; Data-driven. Each golden JSON declares a (dataset, family, alpha, lambda)
;; fixture and R's reference intercept / coefficients / dev.ratio / predictions
;; (scripts/r-parity/gen-reference.R generates them with R glmnet). We load the
;; same committed dataset, fit the matching family, and check we agree within the
;; tolerances recorded in the golden's `meta`. Path goldens (kind "path", #10)
;; hold R's whole regularization path; predict goldens (kind "predict", #25) and
;; the `generic` entry of each single-fit golden hold R's coef(fit, s) and
;; predict(fit, newx, s, type) for the generic interface; and CV goldens (kind
;; "cv", #27) hold R's cv.glmnet on fold ids recorded in the golden. Every
;; fixture is also fitted through the formula front end (#26), from the
;; dataset as a table with the CSV's column names, and its name-keyed `coef`
;; checked against R's coef, names and values. Formula goldens (kind
;; "formula", #53) hold R's terms() and model.matrix() for a formula on mtcars
;; or longley and glmnet fitted on that matrix; each Racket spelling of the
;; formula must expand to R's terms, build R's matrix and fit R's path, with
;; R's name of each transform, such as log(hp), read as its Racket source.
;; Dataset goldens (kind "dataset", #61) hold R's own copy of each CSV that
;; glmnet/datasets ships, which must read back bit for bit, and vignette
;; goldens (kind "vignette") the vignette's calls on R glmnet's example
;; datasets, which are loaded through glmnet/datasets. The csv-cells golden
;; holds how R's read.csv types a column of one cell, for many spellings of
;; numbers, logicals and missing values, which csv->table must match.
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
           racket/match
           (only-in racket/math nan? pi sqr)
           racket/runtime-path
           racket/string
           glmnet
           glmnet/data/csv
           glmnet/datasets
           ;; The collection's instance, whose transform structs `~` makes;
           ;; the checks run this file from the source tree against an
           ;; installed copy of the package.
           (only-in glmnet/core/terms
                    expand-terms model-terms-labels model-terms-intercept? model-terms-terms
                    model-terms-codings term-variables variable-label drop-response-terms
                    resolve-levels terms->design-matrix)
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

  ;; The formula of each dataset: its response against every other column, in
  ;; the order of the columns of R's X.
  (define dataset-formulas
    (hash "longley"    (~ Employed all)
          "wdbc"       (~ diagnosis all)
          "iris"       (~ class all)
          "veteran"    (~ (surv time status) all)
          "warpbreaks" (~ breaks all)
          "linnerud"   (~ (weight waist pulse) all)))

  (define (golden-formula g) (hash-ref dataset-formulas (hash-ref g 'dataset)))
  ;; The table of a golden's dataset; `nobs` restricts it to the first nobs rows.
  (define (golden-table g)
    (define full (load-table (hash-ref g 'dataset)))
    (match (hash-ref g 'nobs #f)
      [#f full]
      [n (for/list ([column (in-list full)]) (cons (car column) (take (cdr column) n)))]))
  (define (golden-family g) (string->symbol (hash-ref g 'family)))

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

  ;; compare nested lists (or vectors) of reals element-wise
  (define (check-nested-close got expected tol msg)
    (cond
      [(list? expected)
       (define got* (if (vector? got) (vector->list got) got))
       (check-equal? (length got*) (length expected) (format "~a: length mismatch" msg))
       (for ([g (in-list got*)] [e (in-list expected)] [i (in-naturals)])
         (check-nested-close g e tol (format "~a[~a]" msg i)))]
      [else (check-close got expected tol msg)]))

  ;; Whether R's class for a row, given R's linear predictor(s) there, is
  ;; further from a tie than the prediction tolerance can move it. Only then
  ;; must our class agree.
  (define (decisive? eta tol)
    (cond
      [(list? eta)
       (define sorted (sort eta >))
       (> (- (first sorted) (second sorted))
          (* 2 tol (+ 1 (apply max (map abs eta)))))]
      [else (> (abs eta) (* 2 tol (+ 1 (abs eta))))]))

  ;; The generic interface (#25): `coef` and `predict` of every type at each s
  ;; of `gen`, against R's coef(fit, s) and predict(fit, newx, s, type).
  (define (check-generic model X gen tols)
    (define s    (hash-ref gen 's))
    (define ctol (hash-ref tols 'coef))
    (define coefs (coef model #:lambda s))
    (check-equal? (length coefs) (length (hash-ref gen 'coef_s)) "coef: number of s")
    (for ([got (in-list coefs)]
          [expected (in-list (hash-ref gen 'coef_s))]
          [i (in-naturals)])
      (check-nested-close got expected ctol (format "coef[s ~a]" i)))
    (check-predict model X s (hash-ref gen 'predict_s) (hash-ref tols 'pred) "predict"))

  ;; `predict` of every type at each of `s` against R's predictions `preds`,
  ;; by type; a class only where R's is decisive.
  (define (check-predict model X s preds ptol what)
    (for ([(type all-expected) (in-hash preds)])
      (define all-got (predict model X #:type type #:lambda s))
      (check-equal? (length all-got) (length all-expected) (format "~a ~a: number of s" what type))
      (for ([got (in-list all-got)]
            [expected (in-list all-expected)]
            [etas (in-list (hash-ref preds 'link))]
            [i (in-naturals)])
        (define msg (format "~a ~a[s ~a]" what type i))
        (cond
          [(eq? type 'class)
           (check-equal? (length got) (length expected) (format "~a: row count" msg))
           (for ([c (in-list got)] [e (in-list expected)] [eta (in-list etas)] [row (in-naturals)]
                 #:when (decisive? eta ptol))
             (check-equal? c e (format "~a[row ~a]" msg row)))]
          [else (check-nested-close got expected ptol msg)]))))

  ;; A formula model's coef, one entry per s, against R's: the names R gives
  ;; its rows and list elements (`names`, the golden's coef_names), and its
  ;; values.
  (define (check-named-coef got-per-s expected-per-s names tol)
    (define rows (hash-ref names 'rows))
    (define groups (hash-ref names 'groups #f))
    (for ([got (in-list got-per-s)]
          [expected (in-list expected-per-s)]
          [i (in-naturals)])
      (define msg (format "formula coef[s ~a]" i))
      (cond
        [groups
         (check-equal? (for/list ([group (in-list got)]) (format "~a" (car group))) groups
                       (format "~a: names of the list" msg))
         (for ([group (in-list got)] [e (in-list expected)] [k (in-naturals)])
           (check-equal? (map car (cdr group)) rows (format "~a[~a]: row names" msg k))
           (check-vec-close (map cdr (cdr group)) e tol (format "~a[~a]" msg k)))]
        [else
         (check-equal? (map car got) rows (format "~a: row names" msg))
         (check-vec-close (map cdr got) expected tol msg)])))

  ;; The lines of a path's printed table, without the #<glmnet-path:family
  ;; line before it and the > after it.
  (define (printed-table p)
    (define s (format "~a" p))
    (cdr (regexp-split #rx"\n" (substring s 0 (sub1 (string-length s))))))

  ;; The dataset of a path or predict golden; `nobs` restricts it to the first
  ;; nobs observations.
  (define (golden-dataset g)
    (define full (load-dataset (hash-ref g 'dataset)))
    (match (hash-ref g 'nobs #f)
      [#f full]
      [n (for/list ([column (in-list full)]) (take column n))]))

  ;; The path a path or predict golden describes, fitted as R fits it. A golden
  ;; may fix #:nlambda and #:lambda-min-ratio.
  (define (fit-golden-path g ds)
    (define family (hash-ref g 'family))
    (define alpha  (hash-ref g 'alpha))
    (define thresh (hash-ref g 'thresh))
    (define lambda (hash-ref g 'lambda_user #f))
    (define nlambda (hash-ref g 'nlambda 100))
    (define ratio  (hash-ref g 'lambda_min_ratio #f))
    (define X      (first ds))
    (define (fit path-proc . data)
      (keyword-apply path-proc '(#:alpha #:lambda #:lambda-min-ratio #:nlambda #:thresh)
                     (list alpha lambda ratio nlambda thresh)
                     X data))
    (case family
      [("gaussian")    (fit elnet-path (second ds))]
      [("binomial")    (fit logistic-path (second ds))]
      [("multinomial") (fit multinomial-path (second ds))]
      [("poisson")     (fit poisson-path (second ds))]
      [("cox")         (fit cox-path (second ds) (third ds))]
      [("mgaussian")   (fit mgaussian-path (second ds))]))

  ;; predict and coef along a path (#25), at s on, between, above and below
  ;; the fitted lambdas, and the path's printed table.
  (define (run-predict-golden g)
    (define ds (golden-dataset g))
    (define p  (fit-golden-path g ds))
    (define tols (hash-ref (hash-ref g 'meta) 'tolerances))
    (test-case (hash-ref g 'id)
      (check-generic p (first ds) g tols)
      (check-equal? (printed-table p) (hash-ref g 'print) "printed table"))
    (test-case (format "~a (formula)" (hash-ref g 'id))
      (define fp (formula-path (golden-formula g) (golden-table g)
                               #:family (golden-family g) #:alpha (hash-ref g 'alpha)
                               #:lambda (hash-ref g 'lambda_user #f)
                               #:nlambda (hash-ref g 'nlambda 100)
                               #:lambda-min-ratio (hash-ref g 'lambda_min_ratio #f)
                               #:thresh (hash-ref g 'thresh)))
      (check-named-coef (coef fp #:lambda (hash-ref g 's)) (hash-ref g 'coef_s)
                        (hash-ref g 'coef_names) (hash-ref tols 'coef))))

  ;; A regularization path (#10): R's lambda sequence (or the user's), where it
  ;; stops, and the fit at every lambda. A golden may fix #:nlambda and
  ;; #:lambda-min-ratio, and `nobs` restricts it to the first nobs observations.
  (define (run-path-golden g)
    (define p (fit-golden-path g (golden-dataset g)))
    (test-case (hash-ref g 'id)
      (check-path p g)))

  ;; Path p against the golden's lambda_path, dev_ratio_path, df_path,
  ;; coefficients_path and intercepts_path.
  (define (check-path p g)
    (define id     (hash-ref g 'id))
    (define tols   (hash-ref (hash-ref g 'meta) 'tolerances))
    (define ctol   (hash-ref tols 'coef))
    (define itol   (hash-ref tols 'intercept))
    (define dtol   (hash-ref tols 'dev_ratio))
    (define expected-lambda (hash-ref g 'lambda_path))
    (check-equal? (vector-length (glmnet-path-lambda p)) (length expected-lambda)
                  (format "~a: number of lambdas fitted" id))
    (check-vec-close (vector->list (glmnet-path-lambda p)) expected-lambda 1e-8 "lambda")
    (check-vec-close (vector->list (glmnet-path-dev-ratio p))
                     (hash-ref g 'dev_ratio_path) dtol "dev-ratio")
    (check-equal? (vector->list (glmnet-path-df p)) (hash-ref g 'df_path) "df")
    (for ([coefs (in-vector (glmnet-path-coefficients p))]
          [expected (in-list (hash-ref g 'coefficients_path))]
          [m (in-naturals)])
      (if (vector? (vector-ref coefs 0))
          (for ([c (in-vector coefs)] [e (in-list expected)] [k (in-naturals)])
            (check-vec-close (vector->list c) e ctol (format "coef[lambda ~a, group ~a]" m k)))
          (check-vec-close (vector->list coefs) expected ctol (format "coef[lambda ~a]" m))))
    (when (glmnet-path-intercepts p)
      (for ([a0 (in-vector (glmnet-path-intercepts p))]
            [expected (in-list (hash-ref g 'intercepts_path))]
            [m (in-naturals)])
        (if (vector? a0)
            (check-vec-close (vector->list a0) expected itol (format "intercepts[lambda ~a]" m))
            (check-close a0 expected itol (format "intercept[lambda ~a]" m))))))

  ;; Cross-validation (#27): R's cv.glmnet on the golden's folds (numbered from
  ;; 1 there, from 0 here), for one type measure.
  (define (run-cv-golden g)
    (define id       (hash-ref g 'id))
    (define tols     (hash-ref (hash-ref g 'meta) 'tolerances))
    (define ctol     (hash-ref tols 'coef))
    (define ds       (load-dataset (hash-ref g 'dataset)))
    (define X        (first ds))
    (define fold-ids (for/list ([f (in-list (hash-ref g 'foldid))]) (sub1 f)))
    (define measure  (string->symbol (hash-ref g 'type_measure)))
    (define grouped? (hash-ref g 'grouped))
    (define alpha    (hash-ref g 'alpha))
    (define thresh   (hash-ref g 'thresh))
    (define lambda   (hash-ref g 'lambda_user #f))
    (define cv
      (case (hash-ref g 'family)
        [("gaussian")
         (elnet-cv X (second ds) #:type-measure measure #:fold-ids fold-ids #:grouped? grouped?
                   #:alpha alpha #:lambda lambda #:thresh thresh)]
        [("binomial")
         (logistic-cv X (second ds) #:type-measure measure #:fold-ids fold-ids
                      #:grouped? grouped? #:alpha alpha #:lambda lambda #:thresh thresh)]
        [("multinomial")
         (multinomial-cv X (second ds) #:type-measure measure #:fold-ids fold-ids
                         #:grouped? grouped? #:alpha alpha #:lambda lambda #:thresh thresh)]
        [("poisson")
         (poisson-cv X (second ds) #:type-measure measure #:fold-ids fold-ids
                     #:grouped? grouped? #:alpha alpha #:lambda lambda #:thresh thresh)]
        [("cox")
         (cox-cv X (second ds) (third ds) #:type-measure measure #:fold-ids fold-ids
                 #:grouped? grouped? #:alpha alpha #:lambda lambda #:thresh thresh)]
        [("mgaussian")
         (mgaussian-cv X (second ds) #:type-measure measure #:fold-ids fold-ids
                       #:grouped? grouped? #:alpha alpha #:lambda lambda #:thresh thresh)]))
    (test-case id
      (check-cv cv g fold-ids tols))
    (test-case (format "~a (formula)" id)
      (define fcv (formula-cv (golden-formula g) (golden-table g)
                              #:family (golden-family g) #:type-measure measure
                              #:fold-ids fold-ids #:grouped? grouped? #:alpha alpha
                              #:lambda lambda #:thresh thresh))
      (check-named-coef (list (coef fcv #:lambda 'lambda-min)) (list (hash-ref g 'coef_min))
                        (hash-ref g 'coef_names) ctol)))

  ;; A cross-validated model against R's cv.glmnet as golden g records it,
  ;; on the golden's folds, which `fold-ids` numbers from 0.
  (define (check-cv cv g fold-ids tols)
    (define ptol (hash-ref tols 'pred))
    (define ctol (hash-ref tols 'coef))
    (check-equal? (symbol->string (glmnet-cv-measure cv)) (hash-ref g 'measure) "measure")
    (check-equal? (glmnet-cv-name cv) (hash-ref g 'name) "name")
    (check-equal? (glmnet-cv-fold-ids cv) fold-ids "fold ids")
    (check-vec-close (vector->list (glmnet-cv-lambda cv)) (hash-ref g 'lambda) 1e-8 "lambda")
    (for ([field (list glmnet-cv-cvm glmnet-cv-cvsd glmnet-cv-cvup glmnet-cv-cvlo)]
          [key '(cvm cvsd cvup cvlo)])
      (check-vec-close (vector->list (field cv)) (hash-ref g key) ptol (symbol->string key)))
    (check-equal? (vector->list (glmnet-cv-nzero cv)) (hash-ref g 'nzero) "nzero")
    (check-close (glmnet-cv-lambda-min cv) (hash-ref g 'lambda_min) 1e-8 "lambda-min")
    (check-close (glmnet-cv-lambda-1se cv) (hash-ref g 'lambda_1se) 1e-8 "lambda-1se")
    (check-equal? (add1 (glmnet-cv-index-min cv)) (hash-ref g 'index_min) "index-min")
    (check-equal? (add1 (glmnet-cv-index-1se cv)) (hash-ref g 'index_1se) "index-1se")
    (check-nested-close (coef cv #:lambda 'lambda-min) (hash-ref g 'coef_min) ctol
                        "coef at lambda-min")
    (check-nested-close (coef cv) (hash-ref g 'coef_1se) ctol "coef, default lambda"))

  ;; C's %a of a double, such as "-0x1.8p+1", as that flonum.
  (define (hex->flonum s)
    (match-define (list _ sign lead fraction exponent)
      (regexp-match #px"^(-?)0x([01])(?:[.]([0-9a-f]+))?p([+-][0-9]+)$" s))
    (define digits (or fraction ""))
    (define mantissa
      (+ (string->number lead)
         (if (string=? digits "")
             0
             (/ (string->number digits 16) (expt 16 (string-length digits))))))
    (define v (exact->inexact (* mantissa (expt 2 (string->number exponent)))))
    (if (string=? sign "-") (- v) v))

  ;; R's %a of a double, or its Inf, -Inf or NaN.
  (define (r-hex->flonum s)
    (match s
      ["Inf" +inf.0]
      ["-Inf" -inf.0]
      ["NaN" +nan.0]
      [_ (hex->flonum s)]))

  ;; Each cell of the csv-cells golden, alone in its column, as csv->table
  ;; reads it against R's read.csv. Where the manual says they differ, the
  ;; difference is checked instead: a quoted blank cell is a string where R
  ;; reads NA, and a complex number is a string.
  (define (run-csv-cells-golden g)
    (for ([c (in-list (hash-ref g 'cells))])
      (define cell (hash-ref c 'cell))
      (define token (hash-ref c 'token))
      (define (ours)
        (vector-ref (cdr (assoc "a" (csv->table (open-input-string (string-append "a,b\n" cell ",z\n")))))
                    0))
      (define blank?
        (regexp-match? #px"^[\\s\v\u1680\u2000-\u2006\u2008-\u200A\u2028\u2029\u205F\u3000]*$" token))
      (test-case (format "csv-cells ~s is R's ~a ~a" cell (hash-ref c 'class) (hash-ref c 'value))
        (cond
          [(and (hash-ref c 'na) (hash-ref c 'quoted) blank?) (check-equal? (ours) token)]
          [(hash-ref c 'na)
           (check-exn (lambda (e) (regexp-match? #rx"has a missing value" (exn-message e))) ours)]
          [else
           (match (hash-ref c 'class)
             ["logical" (check-eq? (ours) (hash-ref c 'value))]
             ["integer" (check-true (and (flonum? (ours)) (= (ours) (r-hex->flonum (hash-ref c 'value)))))]
             ["numeric"
              (define r (r-hex->flonum (hash-ref c 'value)))
              (if (eqv? r +nan.0)
                  (check-true (and (flonum? (ours)) (nan? (ours))))
                  (check-eqv? (ours) r))]
             ["complex" (check-equal? (ours) token)]
             ["character" (check-equal? (ours) (hash-ref c 'value))])]))))

  ;; A CSV that glmnet/datasets ships (#61), read as a table, against R's own
  ;; copy of its data: every number the same double, every string the same.
  (define (run-dataset-golden g)
    (define file (hash-ref g 'file))
    (test-case (hash-ref g 'id)
      (define table (csv-file->table (collection-file-path file "glmnet" "datasets")))
      (check-equal? (map car table)
                    (for/list ([column (in-list (hash-ref g 'columns))]) (hash-ref column 'name))
                    "column names")
      (for ([column (in-list table)]
            [expected (in-list (hash-ref g 'columns))])
        (define cells (vector->list (cdr column)))
        (define exact
          (or (hash-ref expected 'strings #f)
              (map hex->flonum (hash-ref expected 'hex))))
        (check-equal? (length cells) (hash-ref g 'nrow) (format "~a: rows" (car column)))
        (check-true (andmap equal? cells exact)
                    (format "~a ~a: every value is R's" file (car column))))))

  ;; A glmnet dataset as its loader returns it: the fitter's arguments.
  (define (vignette-dataset name)
    (call-with-values
     (case name
       [("QuickStartExample")    quick-start-example]
       [("BinomialExample")      binomial-example]
       [("MultinomialExample")   multinomial-example]
       [("PoissonExample")       poisson-example]
       [("CoxExample")           cox-example]
       [("MultiGaussianExample") multi-gaussian-example]
       [("SparseExample")        sparse-example])
     list))

  ;; The vignette's calls on a glmnet dataset (#61), loaded through
  ;; glmnet/datasets: the default path, printed, coef and predict at the
  ;; vignette's s on its rows of x, and cross-validation on R's folds, with
  ;; predict at lambda-min.
  (define (run-vignette-golden g)
    (define id (hash-ref g 'id))
    (define tols (hash-ref (hash-ref g 'meta) 'tolerances))
    (define args (vignette-dataset (hash-ref g 'dataset)))
    (define newx (design-matrix-select-rows (first args) (map sub1 (hash-ref g 'rows))))
    (define-values (path-proc cv-proc)
      (case (hash-ref g 'family)
        [("gaussian")    (values elnet-path elnet-cv)]
        [("binomial")    (values logistic-path logistic-cv)]
        [("multinomial") (values multinomial-path multinomial-cv)]
        [("poisson")     (values poisson-path poisson-cv)]
        [("cox")         (values cox-path cox-cv)]
        [("mgaussian")   (values mgaussian-path mgaussian-cv)]))
    (test-case id
      (define p (apply path-proc args))
      (check-path p g)
      (check-equal? (printed-table p) (hash-ref g 'print) "printed table")
      (check-generic p newx (hash-ref g 'generic) tols))
    (test-case (format "~a (cv)" id)
      (define gcv (hash-ref g 'cv))
      (define fold-ids (map sub1 (hash-ref gcv 'foldid)))
      (define cv (keyword-apply cv-proc '(#:fold-ids #:type-measure)
                                (list fold-ids (string->symbol (hash-ref gcv 'type_measure)))
                                args))
      (check-cv cv gcv fold-ids tols)
      (check-predict cv newx (list (glmnet-cv-lambda-min cv)) (hash-ref gcv 'predict_min)
                     (hash-ref tols 'pred) "predict at lambda-min")))

  ;; This module's namespace, in which `~` expands the formula goldens' Racket
  ;; sources to formulas of this module's instance of glmnet.
  (define-namespace-anchor anchor)
  (define formula-namespace (namespace-anchor->namespace anchor))

  ;; The messages that `thunk` logs at level warning on the glmnet topic, and
  ;; its value.
  (define (glmnet-warnings thunk)
    (define receiver (make-log-receiver (current-logger) 'warning 'glmnet))
    (define result (thunk))
    (values result
            (let loop ()
              (match (sync/timeout 0 receiver)
                [#f '()]
                [(vector _ message _ _) (cons message (loop))]))))

  ;; A table as a golden carries it, a list of named columns, with the
  ;; strings of the columns named in `symbols` read as symbols.
  (define (golden-columns->table columns symbols)
    (for/list ([column (in-list columns)])
      (define name (hash-ref column 'name))
      (define entries (hash-ref column 'values))
      (cons name (if (member name symbols) (map string->symbol entries) entries))))

  ;; The table of a formula golden: R's mtcars or iris as glmnet/datasets
  ;; holds them, a committed dataset, or the table the golden carries.
  (define (formula-golden-table g symbols)
    (define table
      (cond
        [(hash-ref g 'table #f) => (lambda (columns) (golden-columns->table columns '()))]
        [else (match (hash-ref g 'dataset)
                ["mtcars" mtcars]
                ["iris" iris]
                [name (load-table name)])]))
    (for/list ([column (in-list table)])
      (if (member (car column) symbols)
          (cons (car column) (for/list ([v (cdr column)]) (string->symbol v)))
          column)))

  ;; The formula algebra (#53): for each Racket spelling of the golden's
  ;; formula, `~` and make-formula agree, when the formula has no transforms;
  ;; the expansion has R's term labels, intercept and factors;
  ;; formula-design-matrix is R's model.matrix without its intercept column,
  ;; names and values, and warns where R does; the factors have R's levels;
  ;; and the path fitted from the formula, for the golden's family, is R's
  ;; glmnet on that matrix. On new data, the design matrix coded with the
  ;; fit's levels and the predictions are R's, and a level the fit did not see
  ;; is an error naming the levels R names. Names are compared after the
  ;; golden's `names` map R's name of each transform or factor() to its
  ;; Racket source.
  (define (run-formula-golden g)
    (define id      (hash-ref g 'id))
    (define tols    (hash-ref (hash-ref g 'meta) 'tolerances))
    (define gen     (hash-ref g 'generic))
    (define s       (hash-ref gen 's))
    (define family  (string->symbol (hash-ref g 'family)))
    (define symbols (hash-ref g 'symbols '()))
    (define table   (formula-golden-table g symbols))
    (define response-dropped?
      (member "the response appeared on the right-hand side and was dropped"
              (hash-ref g 'warnings)))
    ;; R's names of transforms and factor()s, longest first, so that a name
    ;; is mapped by the longest one it starts with.
    (define variable-names
      (sort (for/list ([(r rkt) (in-hash (hash-ref g 'names (hash)))])
              (cons (symbol->string r) rkt))
            > #:key (lambda (pair) (string-length (car pair)))))
    ;; An R name, a variable's, a column's or an interaction's, as the Racket
    ;; one: a factor's column is its name followed by a level's.
    (define (racket-name r)
      (string-join (for/list ([part (in-list (string-split r ":"))])
                     (or (for/first ([pair (in-list variable-names)]
                                     #:when (string-prefix? part (car pair)))
                           (string-append (cdr pair) (substring part (string-length (car pair)))))
                         part))
                   ":"))
    (define r-coef-names (hash-ref gen 'coef_names))
    (define coef-names
      (hash-set r-coef-names 'rows (map racket-name (hash-ref r-coef-names 'rows))))
    (define levels
      (for/list ([l (in-list (hash-ref g 'levels))])
        (cons (racket-name (hash-ref l 'name)) (hash-ref l 'levels))))
    (for ([source (in-list (hash-ref g 'rkt))])
      (test-case (format "~a ~a" id source)
        (define datum (read (open-input-string source)))
        (define f (eval datum formula-namespace))
        (when (formula-rhs/c (cddr datum))
          (check-equal? (apply make-formula (cdr datum)) f "~ and make-formula"))
        (check-equal? (format "~a" f)
                      (parameterize ([print-reader-abbreviations #t]) (format "~s" datum))
                      "printed")
        (define mt (expand-terms (formula-terms f) (list (format "~a" (formula-response f)))
                                 (table-column-names table)))
        (check-equal? (model-terms-labels mt) (map racket-name (hash-ref g 'term_labels))
                      "term labels")
        (check-equal? (model-terms-intercept? mt) (hash-ref g 'intercept) "intercept")
        (check-equal? (for/list ([term (in-list (model-terms-terms mt))]
                                 [coding (in-list (model-terms-codings mt))])
                        (for/hash ([v (in-list (term-variables mt term))] [c (in-list coding)])
                          (values (variable-label v) c)))
                      (for/list ([factors (in-list (hash-ref g 'factors))])
                        (for/hash ([(v c) (in-hash factors)])
                          (values (racket-name (symbol->string v)) c)))
                      "factors")
        (define column-names (map racket-name (hash-ref g 'column_names)))
        (define-values (dm warnings)
          (glmnet-warnings (lambda () (formula-design-matrix f table))))
        (check-equal? (design-matrix-column-names dm) column-names "column names")
        (check-equal? (formula-predictor-names f table) column-names)
        (check-mat-close (design-matrix->columns dm) (hash-ref g 'columns) 1e-12 "design matrix")
        (check-equal? (and (pair? warnings) #t) (and response-dropped? #t)
                      (format "a warning where R warns: ~s" warnings))
        (when response-dropped?
          (check-regexp-match #rx"^glmnet: formula-design-matrix: the response column \"[^\"]+\" appeared on the right-hand side and was dropped$"
                              (car warnings)))
        (define fp (formula-path f table #:family family #:lambda (hash-ref g 'lambda_user)
                                 #:alpha (hash-ref g 'alpha) #:thresh (hash-ref g 'thresh)))
        (check-equal? (formula-model-levels fp) levels "levels")
        (check-named-coef (coef fp #:lambda s) (hash-ref gen 'coef_s) coef-names (hash-ref tols 'coef))
        (check-predict fp table s (hash-ref gen 'predict_s) (hash-ref tols 'pred) "formula predict")
        (define new-columns (hash-ref g 'new_columns #f))
        (when new-columns
          (define new-table (golden-columns->table (hash-ref g 'new_table) symbols))
          (define-values (kept dropped) (drop-response-terms mt))
          (define coded
            (terms->design-matrix 'predict (resolve-levels 'parity kept table) new-table))
          (check-equal? (design-matrix-column-names coded) column-names "new data: column names")
          (check-mat-close (design-matrix->columns coded) new-columns 1e-12 "new data: design matrix")
          (check-predict fp new-table s (hash-ref g 'new_predict) (hash-ref tols 'pred)
                         "new data: predict"))
        (define bad-levels (hash-ref g 'bad_levels #f))
        (when bad-levels
          (define bad-table (golden-columns->table (hash-ref g 'bad_table) symbols))
          (check-exn (lambda (e)
                       (and (exn:fail? e)
                            (regexp-match?
                             (regexp-quote
                              (format ": ~s\n  new levels: '~s"
                                      (racket-name (hash-ref g 'bad_factor)) bad-levels))
                             (exn-message e))))
                     (lambda () (predict fp bad-table #:lambda s))
                     "new levels")))))

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
    (define (check-generic-single r)
      (define gen (hash-ref g 'generic))
      (check-generic r X gen tols)
      (check-nested-close (coef r) (first (hash-ref gen 'coef_s)) ctol "coef, default lambda")
      (check-nested-close (predict r X) (first (hash-ref (hash-ref gen 'predict_s) 'link))
                          ptol "predict, default type and lambda"))
    ;; The same fit through the formula front end, whose predict reads the
    ;; table by name.
    (define (check-formula-single)
      (define gen (hash-ref g 'generic))
      (define table (golden-table g))
      (define m (formula-fit (golden-formula g) table
                             #:family (golden-family g) #:lambda lambda #:alpha alpha
                             #:thresh thresh #:intercept? (hash-ref g 'fit_intercept #t)))
      (check-named-coef (coef m #:lambda (hash-ref gen 's)) (hash-ref gen 'coef_s)
                        (hash-ref gen 'coef_names) ctol)
      (check-nested-close (predict m table #:lambda (hash-ref gen 's))
                          (hash-ref (hash-ref gen 'predict_s) 'link) ptol "formula predict"))
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
                          (hash-ref g 'predictions) ptol "predictions")
         (check-vec-close (elnet-predict r X) (hash-ref g 'predictions) ptol "elnet-predict")
         (check-generic-single r)]
        [(string=? family "binomial")
         (define y (second ds))
         (define r (logistic-fit X y #:lambda lambda #:alpha alpha #:thresh thresh))
         (check-close (logistic-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-close (logistic-result-intercept r) (hash-ref g 'intercept) itol "intercept")
         (check-vec-close (vector->list (logistic-result-coefficients r))
                          (hash-ref g 'coefficients) ctol "coef")
         (check-close (logistic-result-dev-ratio r) (hash-ref g 'dev_ratio) dtol "dev-ratio")
         (check-vec-close (logistic-predict-proba r X)
                          (hash-ref g 'predictions) ptol "proba")
         (check-generic-single r)]
        [(string=? family "multinomial")
         (define y (second ds))
         (define r (multinomial-fit X y #:lambda lambda #:alpha alpha #:thresh thresh))
         (check-close (multinomial-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-close (multinomial-result-dev-ratio r) (hash-ref g 'dev_ratio) dtol "dev-ratio")
         (check-vec-close (vector->list (multinomial-result-intercepts r))
                          (hash-ref g 'intercepts) itol "intercepts")
         (check-mat-close (multinomial-predict-proba r X) (hash-ref g 'probabilities) ptol "proba")
         (for ([k (in-naturals)] [ck (in-list (hash-ref g 'coefficients))])
           (check-vec-close (vector->list (vector-ref (multinomial-result-coefficients r) k))
                            ck ctol (format "coef[class ~a]" k)))
         (check-generic-single r)]
        [(string=? family "poisson")
         (define y (second ds))
         (define r (poisson-fit X y #:lambda lambda #:alpha alpha #:thresh thresh))
         (check-close (poisson-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-close (poisson-result-intercept r) (hash-ref g 'intercept) itol "intercept")
         (check-vec-close (vector->list (poisson-result-coefficients r))
                          (hash-ref g 'coefficients) ctol "coef")
         (check-close (poisson-result-dev-ratio r) (hash-ref g 'dev_ratio) dtol "dev-ratio")
         (check-vec-close (poisson-predict-mean r X) (hash-ref g 'predictions) ptol "predict-mean")
         (check-generic-single r)]
        [(string=? family "cox")
         (define times (second ds))
         (define statuses (third ds))
         (define r (cox-fit X times statuses #:lambda lambda #:alpha alpha #:thresh thresh))
         (check-close (cox-result-lambda r) (hash-ref g 'lambda_used) 1e-12 "lambda")
         (check-vec-close (vector->list (cox-result-coefficients r))
                          (hash-ref g 'coefficients) ctol "coef")
         (check-close (cox-result-dev-ratio r) (hash-ref g 'dev_ratio) dtol "dev-ratio")
         (check-vec-close (cox-linear-predictor r X)
                          (hash-ref g 'linear_predictor) ptol "linear-predictor")
         (check-generic-single r)]
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
         (check-mat-close (mgaussian-predict r X) (hash-ref g 'predictions) ptol "predictions")
         (check-generic-single r)]
        [else (fail (format "~a: unhandled family ~a" id family))]))
    (test-case (format "~a (formula)" id)
      (check-formula-single)))

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
       (define g (call-with-input-file path read-json))
       (case (hash-ref g 'kind #f)
         [("path")    (run-path-golden g)]
         [("predict") (run-predict-golden g)]
         [("cv")      (run-cv-golden g)]
         [("formula") (run-formula-golden g)]
         [("dataset") (run-dataset-golden g)]
         [("csv-cells") (run-csv-cells-golden g)]
         [("vignette") (run-vignette-golden g)]
         [else        (run-golden g)]))]
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
