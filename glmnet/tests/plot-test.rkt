#lang racket/base

;; Unit tests for glmnet/plot (#28): the coefficient-path and cross-validation
;; plots render for every family, at the requested size and with the expected
;; renderers; the options, the errors and the file output; and the pieces that
;; follow R, checked against R glmnet 4.1.10's values: the x positions, the
;; counts along the top and the panel labels of a coefficient plot, for a
;; single response and for each class or response of the multinomial and
;; multi-response paths; where the curve labels go; approx with method
;; "constant"; and the labels along the top of a CV plot. The images
;; themselves are not compared, except that a formula model (#26) must draw
;; the same image as its fit labelled with its predictor names.

(module+ test
  (require rackunit
           racket/file
           racket/list
           racket/logging
           racket/math
           file/convertible
           (only-in racket/contract exn:fail:contract:blame?)
           pict
           (only-in plot/no-gui renderer2d? plot-width plot-height ticks-format pre-tick)
           glmnet
           glmnet/plot
           (submod glmnet/plot support))

  (define n 30)
  (define X
    (for/list ([i (in-range n)])
      (list (exact->inexact (modulo (* 7 i) 11))
            (exact->inexact (modulo (* 3 i) 5))
            (sin (* 1.0 i))
            (cos (* 2.0 i)))))
  (define y
    (for/list ([row (in-list X)] [i (in-naturals)])
      (+ 1.0 (* 2.0 (first row)) (- (second row)) (* 3.0 (sin (* 3.0 i))))))
  (define folds (for/list ([i (in-range n)]) (modulo i 5)))
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

  ;; Every family's path and CV result.
  (define families
    (list (list 'gaussian (elnet-path X y) (elnet-cv X y #:fold-ids folds))
          (list 'binomial (logistic-path X labels) (logistic-cv X labels #:fold-ids folds))
          (list 'multinomial (multinomial-path X classes)
                (multinomial-cv X classes #:fold-ids folds))
          (list 'cox (cox-path X times statuses) (cox-cv X times statuses #:fold-ids folds))
          (list 'poisson (poisson-path X counts) (poisson-cv X counts #:fold-ids folds))
          (list 'mgaussian (mgaussian-path X Y) (mgaussian-cv X Y #:fold-ids folds))))

  ;; The coefficient vectors of one panel, per lambda: the class or response r
  ;; of a multinomial or multi-response path, or the path's own.
  (define (response-coefficients p r)
    (for/list ([beta (in-vector (glmnet-path-coefficients p))])
      (if (vector? (vector-ref beta 0)) (vector-ref beta r) beta)))

  (define (panel-count p)
    (if (memq (glmnet-path-family p) '(multinomial mgaussian))
        (vector-length (vector-ref (glmnet-path-coefficients p) 0))
        1))

  ;; The predictors that are nonzero at some lambda, the ones plotted.
  (define (ever-nonzero betas)
    (for/list ([j (in-range (vector-length (car betas)))]
               #:when (for/or ([beta (in-list betas)]) (not (zero? (vector-ref beta j)))))
      j))

  (define (check-size picture width height)
    (check-equal? (pict-width picture) width)
    (check-equal? (pict-height picture) height))

  (test-case "every family's path plots, one panel per class or response"
    (for ([family+path+cv (in-list families)])
      (define-values (family p cv) (apply values family+path+cv))
      (define panels (panel-count p))
      (check-equal? panels (case family [(multinomial) 3] [(mgaussian) 2] [else 1])
                    (format "~a" family))
      (for ([xvar (in-list '(lambda norm dev))])
        (check-size (plot-coefficient-path p #:xvar xvar #:width 300 #:height 200)
                    300 (* 200 panels)))
      (check-size (plot-coefficient-path p #:sign-lambda 1 #:label #t #:width 300 #:height 200)
                  300 (* 200 panels))
      (check-size (plot-coefficient-path cv #:width 300 #:height 200) 300 (* 200 panels))))

  (test-case "a panel has a line per predictor that is ever nonzero, and a label per line"
    (for ([family+path+cv (in-list families)])
      (define p (second family+path+cv))
      (for ([r (in-range (panel-count p))])
        (define lines (length (ever-nonzero (response-coefficients p r))))
        (check-true (positive? lines))
        (check-equal? (length (coefficient-path-renderers p #:response r)) lines)
        (check-equal? (length (coefficient-path-renderers p #:response r #:label #t))
                      (* 2 lines))
        (check-true (andmap renderer2d? (coefficient-path-renderers p #:response r))))))

  (test-case "a CV result has its path's renderers, and the CV plot has four"
    (for ([family+path+cv (in-list families)])
      (define-values (family p cv) (apply values family+path+cv))
      (check-equal? (length (coefficient-path-renderers cv))
                    (length (coefficient-path-renderers (glmnet-cv-path cv))))
      (check-equal? (length (cv-renderers cv)) 4 (format "~a" family))
      (check-equal? (length (cv-renderers cv #:sign-lambda 1)) 4)
      (check-size (plot-cv cv #:width 350 #:height 250) 350 250)
      (check-size (plot-cv cv #:sign-lambda 1 #:title "CV") 400 400)))

  (test-case "the defaults are plot-lib's size and R's axes"
    (define p (second (first families)))
    (check-size (plot-coefficient-path p) 400 400)
    (parameterize ([plot-width 320] [plot-height 240])
      (check-size (plot-coefficient-path p) 320 240)))

  (test-case "'2norm draws one panel of 2-norms across the classes or responses"
    (for ([family+path+cv (in-list families)]
          #:when (memq (first family+path+cv) '(multinomial mgaussian)))
      (define p (second family+path+cv))
      (check-size (plot-coefficient-path p #:type-coef '2norm #:width 300 #:height 200) 300 200)
      (define norms
        (for/list ([groups (in-vector (glmnet-path-coefficients p))])
          (for/vector ([j (in-range (vector-length (vector-ref groups 0)))])
            (sqrt (for/sum ([beta (in-vector groups)]) (sqr (vector-ref beta j)))))))
      (check-equal? (length (coefficient-path-renderers p #:type-coef '2norm))
                    (length (ever-nonzero norms)))
      (check-exn exn:fail:contract?
                 (lambda () (coefficient-path-renderers p #:type-coef '2norm #:response 1)))))

  (test-case "labels: positions, names, and a design matrix's column names"
    (define p (second (first families)))
    (define names '(a b c d))
    (check-size (plot-coefficient-path p #:label names) 400 400)
    (check-size (plot-coefficient-path p #:label (rows->design-matrix X #:column-names names))
                400 400)
    (check-size (plot-coefficient-path p #:label (rows->design-matrix X)) 400 400)
    (check-size (plot-coefficient-path p #:label '("a long predictor name" b c d)) 400 400)
    (check-exn #rx"one entry per predictor"
               (lambda () (plot-coefficient-path p #:label '(a b))))
    (check-exn #rx"one column per predictor"
               (lambda () (plot-coefficient-path p #:label (rows->design-matrix '((1.0 2.0))))))
    (check-exn exn:fail:contract? (lambda () (plot-coefficient-path p #:label '(1 2 3 4)))))

  (define (png pict) (convert pict 'png-bytes))

  (test-case "a formula model is plotted through its fit, its curves labelled by name"
    (define names '("a" "b" "c" "d"))
    (define table
      (cons (cons "y" y)
            (for/list ([name (in-list names)] [j (in-naturals)])
              (cons name (map (lambda (row) (list-ref row j)) X)))))
    (define fp (formula-path (~ y all) table))
    (define fcv (formula-cv (~ y all) table #:fold-ids folds))
    (define p (formula-model-fit fp))
    (define cv (formula-model-fit fcv))
    (check-equal? (png (plot-coefficient-path fp #:label #t))
                  (png (plot-coefficient-path p #:label names)))
    (check-not-equal? (png (plot-coefficient-path fp #:label #t))
                      (png (plot-coefficient-path p #:label #t)))
    (check-equal? (png (plot-coefficient-path fp #:label '(w x y z)))
                  (png (plot-coefficient-path p #:label '(w x y z))))
    (check-equal? (png (plot-coefficient-path fp)) (png (plot-coefficient-path p)))
    (check-equal? (png (plot-coefficient-path fcv #:label #t #:xvar 'norm))
                  (png (plot-coefficient-path cv #:label names #:xvar 'norm)))
    (check-equal? (png (plot-cv fcv)) (png (plot-cv cv)))
    (check-equal? (length (coefficient-path-renderers fcv #:label #t))
                  (length (coefficient-path-renderers cv #:label #t)))
    (check-equal? (length (cv-renderers fcv)) (length (cv-renderers cv)))
    (check-exn #rx"plot-cv: the formula model does not hold a cross-validated path"
               (lambda () (plot-cv fp)))
    (check-exn #rx"does not hold a path or a cross-validated path"
               (lambda () (plot-coefficient-path (formula-fit (~ y all) table #:lambda 0.1))))
    (check-exn exn:fail:contract? (lambda () (plot-cv p))))

  (test-case "a formula model's responses name the multi-response panels, in the formula's order"
    (define-values (u v a b) (values (map first Y) (map second Y) (map first X) (map second X)))
    (define table (list (cons "u" u) (cons "v" v) (cons "a" a) (cons "b" b)))
    (define mg (formula-path (~ (v u) a b) table #:family 'mgaussian))
    (check-equal? (map panel-y-label
                       (path-panels (formula-model-fit mg) 'coef (glmnet-model-response-names mg)))
                  '("Coefficients: Response v" "Coefficients: Response u"))
    (check-equal? (map panel-y-label (path-panels (formula-model-fit mg) 'coef))
                  '("Coefficients: Response y1" "Coefficients: Response y2"))
    ;; The same fit with its responses named the other way round draws other
    ;; panel titles, and a table in another order draws the same ones.
    (define swapped (list (cons "u" v) (cons "v" u) (cons "a" a) (cons "b" b)))
    (define mg-swapped (formula-path (~ (u v) a b) swapped #:family 'mgaussian))
    (check-equal? (formula-model-fit mg-swapped) (formula-model-fit mg))
    (check-not-equal? (png (plot-coefficient-path mg)) (png (plot-coefficient-path mg-swapped)))
    (define reordered (list (cons "b" b) (cons "u" u) (cons "a" a) (cons "v" v)))
    (check-equal? (png (plot-coefficient-path (formula-path (~ (v u) a b) reordered
                                                            #:family 'mgaussian)))
                  (png (plot-coefficient-path mg)))
    (check-not-equal? (png (plot-coefficient-path mg #:label #t))
                      (png (plot-coefficient-path (formula-model-fit mg) #:label '(a b))))
    (check-size (plot-coefficient-path mg #:label #t #:height 200) 400 400))

  (test-case "a formula model's classes name the multinomial panels, as R's plot.multnet does"
    ;; R titles each panel by names(beta), the response's levels.
    (define species
      (for/list ([c (in-list classes)]) (list-ref '("setosa" "versicolor" "virginica") c)))
    (define table (list (cons "species" species) (cons "a" (map first X)) (cons "b" (map second X))))
    (define m (formula-path (~ species a b) table #:family 'multinomial))
    (check-equal? (map panel-y-label (model-panels m (formula-model-fit m) 'coef))
                  '("Coefficients: Response setosa" "Coefficients: Response versicolor"
                    "Coefficients: Response virginica"))
    (define numbered (formula-path (~ k a b) (cons (cons "k" classes) (cdr table))
                                   #:family 'multinomial))
    (check-equal? (formula-model-fit numbered) (formula-model-fit m))
    (check-equal? (map panel-y-label (model-panels numbered (formula-model-fit numbered) 'coef))
                  '("Coefficients: Response 0" "Coefficients: Response 1" "Coefficients: Response 2"))
    (check-not-equal? (png (plot-coefficient-path m)) (png (plot-coefficient-path numbered))))

  (test-case "a lambda of 0 has no place on a log axis and is left out"
    (define p (elnet-path X y #:lambda '(1.0 0.5 0.1 0.0)))
    (define lines (length (ever-nonzero (response-coefficients p 0))))
    (check-equal? (length (coefficient-path-renderers p)) lines)
    (check-size (plot-coefficient-path p #:label #t) 400 400)
    (check-size (plot-coefficient-path p #:xvar 'norm) 400 400)
    (define cv (elnet-cv X y #:lambda '(1.0 0.5 0.1 0.0) #:fold-ids folds))
    (check-size (plot-cv cv) 400 400))

  (test-case "a path whose only λ is 0 has nothing to show on a log λ axis"
    (define p (elnet-path X y #:lambda '(0.0)))
    (for ([sign-lambda (in-list '(-1 1))])
      (check-exn #rx"^plot-coefficient-path: every λ of the path is 0"
                 (lambda () (plot-coefficient-path p #:sign-lambda sign-lambda))))
    (check-size (plot-coefficient-path p #:xvar 'norm #:label #t) 400 400)
    (check-size (plot-coefficient-path p #:xvar 'dev) 400 400))

  (test-case "a CV result whose every λ is 0 has nothing to show on a log λ axis"
    (define cv (elnet-cv X y #:lambda '(0.0 0.0) #:fold-ids folds))
    (for ([sign-lambda (in-list '(-1 1))])
      (check-exn #rx"^plot-cv: every λ of the path is 0, which has no place on a log λ axis"
                 (lambda () (plot-cv cv #:sign-lambda sign-lambda)))))

  (test-case "a path with no nonzero coefficient cannot be plotted, as in R"
    (define p (elnet-path X y #:lambda '(1000.0 500.0)))
    (check-equal? (coefficient-path-renderers p) '())
    (check-exn #rx"nothing to plot" (lambda () (plot-coefficient-path p))))

  (test-case "one nonzero coefficient plots, with R's warning logged"
    (define p (elnet-path X y #:lambda '(7.0 5.0 3.0)))
    (check-equal? (length (ever-nonzero (response-coefficients p 0))) 1)
    (define logged '())
    (define picture
      (with-intercepted-logging
        (lambda (v) (set! logged (cons (vector-ref v 1) logged)))
        (lambda () (plot-coefficient-path p))
        'warning))
    (check-size picture 400 400)
    (check-true (for/or ([m (in-list logged)]) (regexp-match? #rx"not meaningful" m))))

  (test-case "a single fit's one-lambda path plots"
    (check-size (plot-coefficient-path (elnet-path X y #:lambda '(0.1))) 400 400))

  (test-case "the response of a panel is checked"
    (define p (second (third families)))
    (check-exn exn:fail:contract? (lambda () (coefficient-path-renderers p #:response 3)))
    (check-exn exn:fail:contract?
               (lambda () (coefficient-path-renderers (second (first families)) #:response 1))))

  (test-case "the contracts reject what they should"
    (define p (second (first families)))
    (check-exn exn:fail:contract? (lambda () (plot-coefficient-path (lasso X y #:lambda 0.1))))
    (check-exn exn:fail:contract? (lambda () (plot-coefficient-path p #:xvar 'log)))
    (check-exn exn:fail:contract? (lambda () (plot-coefficient-path p #:sign-lambda 2)))
    (check-exn exn:fail:contract? (lambda () (plot-coefficient-path p #:type-coef 'norm)))
    (check-exn exn:fail:contract? (lambda () (plot-cv p))))

  (test-case "the plots are written to files in the format their extension names"
    (define dir (make-temporary-directory))
    (define cv (third (first families)))
    (define p (second (third families)))
    (define (starts-with? file prefix)
      (define bytes (file->bytes file))
      (and (>= (bytes-length bytes) (bytes-length prefix))
           (equal? (subbytes bytes 0 (bytes-length prefix)) prefix)))
    (for ([ext (in-list '("png" "pdf" "svg" "eps" "PNG"))]
          [magic (in-list (list #"\211PNG" #"%PDF" #"<?xml" #"%!PS" #"\211PNG"))])
      (define cv-file (build-path dir (string-append "cv." ext)))
      (define path-file (build-path dir (string-append "path." ext)))
      (check-true (pict? (plot-cv cv #:out-file cv-file)))
      (check-true (pict? (plot-coefficient-path p #:out-file (path->string path-file))))
      (check-true (starts-with? cv-file magic) ext)
      (check-true (starts-with? path-file magic) ext))
    (define png (build-path dir "cv.png"))
    (define before (file-size png))
    (plot-cv cv #:out-file png #:width 200 #:height 150)
    (check-true (< (file-size png) before))
    ;; An extension other than those four breaks the contract, and the caller
    ;; is blamed.
    (define (out-file-blamed? e)
      (and (exn:fail:contract:blame? e)
           (regexp-match? #rx"has-image-extension[?].*the #:out-file argument" (exn-message e))))
    (check-exn out-file-blamed? (lambda () (plot-cv cv #:out-file (build-path dir "cv.jpg"))))
    (check-exn out-file-blamed? (lambda () (plot-coefficient-path p #:out-file "path")))
    (check-exn out-file-blamed? (lambda () (plot-cv cv #:out-file (build-path dir ".png"))))
    (check-exn out-file-blamed? (lambda () (plot-coefficient-path p #:out-file "path.png.bak")))
    (check-false (file-exists? (build-path dir "cv.jpg")))
    (delete-directory/files dir))

  (test-case "constant-approx is R's approx with method \"constant\" and rule 2"
    ;; R 4.5.3: x <- c(2, 0, 1, 1, 4); y <- c(3, 0, 1, 2, 5)
    ;; approx(x, y, xout = c(-1, 0, 0.5, 1, 1.5, 2, 3, 4, 5),
    ;;        method = "constant", rule = 2, f = 1)$y  and with f = 0
    (define xs '(2 0 1 1 4))
    (define ys '(3 0 1 2 5))
    (define at '(-1 0 0.5 1 1.5 2 3 4 5))
    (check-equal? (map (constant-approx xs ys 1) at) '(0 0 3/2 3/2 3 3 5 5 5))
    (check-equal? (map (constant-approx xs ys 0) at) '(0 0 0 3/2 3/2 3 3 5 5))
    (check-equal? (map (constant-approx '(1.0) '(7) 1) '(0.0 1.0 2.0)) '(7 7 7)))

  ;; The manual's fixtures, and R glmnet 4.1.10's values for them (R 4.5.3):
  ;;   X <- matrix(c(1,2,1, 2,1,4, 3,4,9, 4,3,16, 5,6,25, 6,5,36), ncol = 3, byrow = TRUE)
  ;;   y <- c(1, 4, 3, 6, 5, 8); fit <- glmnet(X, y)
  ;;   X3 <- matrix(c(1,1, 2,1, 1,2, 2,2, 5,1, 6,1, 5,2, 6,2, 3,5, 4,5, 3,6, 4,6),
  ;;                ncol = 2, byrow = TRUE)
  ;;   mfit <- glmnet(X3, factor(c(0,0,0,0,1,1,1,1,2,2,2,2)), family = "multinomial")
  ;;   gfit <- glmnet(X, unname(cbind(y, c(0.5, 1.5, 0.2, 2.2, 1.1, 3.0))),
  ;;                  family = "mgaussian")
  ;; plotCoef's `index` is -log(fit$lambda), log(fit$lambda),
  ;; colSums(abs(fit$beta)) or fit$dev.ratio; its top axis is
  ;; approx(index, df, xout = pretty(index), method = "constant", rule = 2, f)$y.
  (define Xr '((1.0 2.0 1.0) (2.0 1.0 4.0) (3.0 4.0 9.0)
               (4.0 3.0 16.0) (5.0 6.0 25.0) (6.0 5.0 36.0)))
  (define yr '(1.0 4.0 3.0 6.0 5.0 8.0))
  (define X3 '((1.0 1.0) (2.0 1.0) (1.0 2.0) (2.0 2.0) (5.0 1.0) (6.0 1.0)
               (5.0 2.0) (6.0 2.0) (3.0 5.0) (4.0 5.0) (3.0 6.0) (4.0 6.0)))
  (define classes3 '(0 0 0 0 1 1 1 1 2 2 2 2))
  (define Yr (map list yr '(0.5 1.5 0.2 2.2 1.1 3.0)))

  ;; The runs of equal values of a vector, as (value . length) pairs: R's rle.
  (define (runs v)
    (for/list ([group (in-list (group-by values (vector->list v) =))])
      (cons (car group) (length group))))

  ;; The labels along the top of a panel of path p, at the positions `at`.
  (define (top-axis p pnl xvar sign-lambda at)
    (define counts
      (count-ticks (x-positions p xvar sign-lambda) (panel-df pnl) (approx-f xvar sign-lambda)))
    ((ticks-format counts) (apply min at) (apply max at)
                           (for/list ([x (in-list at)]) (pre-tick x #t))))

  (define (check-x-positions p xvar sign-lambda expected)
    (define xs (x-positions p xvar sign-lambda))
    (for ([i (in-list (map car expected))]
          [x (in-list (map cdr expected))])
      (check-= (list-ref xs i) x 1e-9 (format "~a ~a at ~a" xvar sign-lambda i))))

  (test-case "a lasso path's x positions and counts along the top are R's"
    (define p (elnet-path Xr yr))
    (define panels (path-panels p 'coef))
    (check-equal? (length panels) 1)
    (define pnl (car panels))
    (check-equal? (panel-y-label pnl) "Coefficients")
    (check-equal? (vector-length (glmnet-path-lambda p)) 51)
    (check-equal? (runs (panel-df pnl)) '((0 . 1) (1 . 20) (2 . 25) (3 . 5)))
    (check-x-positions p 'lambda -1
                       '((0 . -0.69344471106560057) (25 . 1.63239881721121227)
                         (50 . 3.95824234548802600)))
    (check-x-positions p 'lambda 1
                       '((0 . 0.69344471106560057) (25 . -1.63239881721121227)
                         (50 . -3.95824234548802600)))
    (check-x-positions p 'norm -1
                       '((0 . 0.0) (25 . 1.6646752961164939) (50 . 2.8663294332141187)))
    (check-x-positions p 'dev -1
                       '((0 . 0.0) (25 . 0.90933430176092389) (50 . 0.99912684440990540)))
    (check-equal? (top-axis p pnl 'lambda -1 '(-1 0 1 2 3 4)) '("0" "1" "1" "2" "2" "3"))
    (check-equal? (top-axis p pnl 'lambda 1 '(-4 -3 -2 -1 0 1)) '("3" "2" "2" "1" "1" "0"))
    (check-equal? (top-axis p pnl 'norm -1 '(0 1/2 1 3/2 2 5/2 3)) '("0" "1" "2" "2" "2" "2" "3"))
    (check-equal? (top-axis p pnl 'dev -1 '(0 1/5 2/5 3/5 4/5 1)) '("0" "1" "1" "1" "2" "3"))
    ;; Midway between two lambdas whose counts differ (the 1st and 2nd, 21st
    ;; and 22nd, 46th and 47th), every axis reads the smaller lambda's count.
    (for ([xvar (in-list '(lambda lambda norm dev))]
          [sign-lambda (in-list '(-1 1 -1 -1))])
      (define xs (x-positions p xvar sign-lambda))
      (define midpoints
        (for/list ([i (in-list '(0 20 45))])
          (inexact->exact (/ (+ (list-ref xs i) (list-ref xs (add1 i))) 2))))
      (check-equal? (top-axis p pnl xvar sign-lambda (sort midpoints <))
                    (if (= sign-lambda 1) '("3" "2" "1") '("1" "2" "3"))
                    (format "~a ~a" xvar sign-lambda))))

  (test-case "a multinomial path's panels, counts and x positions are R's"
    (define p (multinomial-path X3 classes3))
    (check-equal? (vector-length (glmnet-path-lambda p)) 75)
    (define panels (path-panels p 'coef))
    ;; R: rownames(mfit$dfmat), and rle(mfit$dfmat[i, ]).
    (check-equal? (map panel-y-label panels)
                  '("Coefficients: Response 0" "Coefficients: Response 1"
                    "Coefficients: Response 2"))
    (check-equal? (map (lambda (pnl) (runs (panel-df pnl))) panels)
                  '(((0 . 2) (1 . 73)) ((0 . 2) (1 . 73)) ((0 . 1) (1 . 74))))
    ;; plot.multnet's `norm`, the sum over the classes of their L1 norms.
    (check-x-positions p 'norm -1
                       '((0 . 0.0) (1 . 0.092063394309243179) (74 . 8.033146860472337281)))
    (for ([pnl (in-list panels)])
      (check-equal? (top-axis p pnl 'norm -1 '(0 2 4 6 8 10)) '("0" "1" "1" "1" "1" "1")))
    ;; R: dfseq <- round(colMeans(mfit$dfmat), 1).
    (define norms (path-panels p '2norm))
    (check-equal? (map panel-y-label norms) '("Coefficient 2Norms"))
    (define dfseq (car norms))
    (check-equal? (runs (panel-df dfseq)) '((0 . 1) (3/10 . 1) (1 . 73)))
    (check-equal? (top-axis p dfseq 'lambda -1 '(0 1 2 3 4 5 6 7 8))
                  '("0" "1" "1" "1" "1" "1" "1" "1" "1"))
    (define second-x (inexact->exact (cadr (x-positions p 'lambda -1))))
    (check-equal? (top-axis p dfseq 'lambda -1 (list second-x)) '("0.3")))

  (test-case "a multi-response path's panels and counts are R's"
    (define p (mgaussian-path Xr Yr))
    (check-equal? (vector-length (glmnet-path-lambda p)) 100)
    (define panels (path-panels p 'coef))
    ;; R names the unnamed responses y1 and y2.
    (check-equal? (map panel-y-label panels)
                  '("Coefficients: Response y1" "Coefficients: Response y2"))
    (for ([pnl (in-list panels)])
      (check-equal? (runs (panel-df pnl)) '((0 . 1) (1 . 10) (2 . 8) (3 . 81))))
    ;; plot.mrelnet's 2-norm panel counts the first response's coefficients.
    (check-equal? (panel-df (car (path-panels p '2norm))) (panel-df (car panels))))

  (test-case "the curve labels go at the end of the x axis, level with the last drawn λ"
    ;; R's plotCoef: xpos = max(index), or min(index) for log lambda, and
    ;; ypos = beta[, ncol(beta)], the coefficients at the last lambda.
    (define p (elnet-path Xr yr))
    (define pnl (car (path-panels p 'coef)))
    (define at-right (label-positions pnl (x-positions p 'lambda -1) #t))
    (check-equal? (map car at-right) '(0 1 2))
    (for ([position (in-list (map cdr at-right))]
          [y (in-list '(1.93121767336867 -0.934669738099164 0.000442021746287323))])
      (check-= (vector-ref position 0) 3.95824234548802600 1e-9)
      (check-= (vector-ref position 1) y 1e-9))
    (for ([position (in-list (map cdr (label-positions pnl (x-positions p 'lambda 1) #f)))])
      (check-= (vector-ref position 0) -3.95824234548802600 1e-9))
    ;; A lambda of 0 is not drawn on a log axis, so the labels are level with
    ;; the curves' ends at the last lambda that is drawn. On the L1-norm axis,
    ;; where lambda 0 is drawn, they are level with it.
    (define p0 (elnet-path Xr yr #:lambda '(1.0 0.5 0.1 0.0)))
    (define pnl0 (car (path-panels p0 'coef)))
    (define betas0 (glmnet-path-coefficients p0))
    (define on-log-axis (label-positions pnl0 (x-positions p0 'lambda -1) #t))
    (check-equal? (map car on-log-axis) '(0 1 2))
    (for ([j+position (in-list on-log-axis)])
      (check-equal? (cdr j+position)
                    (vector (- (log 0.1)) (vector-ref (vector-ref betas0 2) (car j+position)))))
    (define norms0 (x-positions p0 'norm -1))
    (for ([j+position (in-list (label-positions pnl0 norms0 #t))])
      (check-equal? (cdr j+position)
                    (vector (apply max norms0) (vector-ref (vector-ref betas0 3) (car j+position))))))

  (test-case "the CV counts get one label per run, and overlapping labels are left out"
    (check-equal? (label-runs '((0.0 . "0") (1.0 . "1") (2.0 . "1") (3.0 . "1") (4.0 . "2")))
                  '((0.0 . "0") (2.0 . "1") (4.0 . "2")))
    (check-equal? (label-runs '((0.0 . "3") (1.0 . "4") (2.0 . "3")))
                  '((0.0 . "3") (1.0 . "4") (2.0 . "3")))
    ;; Labels 10 pixels wide at 10 pixels per unit, with a gap of 2.
    (define (kept x+labels)
      (keep-apart x+labels (lambda (s) 10) (lambda (x) (* 10 x)) 2))
    (check-equal? (kept '((0 . "a") (1 . "b") (1.2 . "c") (2.3 . "d")))
                  '((0 . "a") (1.2 . "c")))
    (check-equal? (kept '((2.5 . "d") (0 . "a"))) '((0 . "a") (2.5 . "d")))))
