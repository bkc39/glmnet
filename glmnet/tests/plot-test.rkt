#lang racket/base

;; Unit tests for glmnet/plot (#28): the coefficient-path and cross-validation
;; plots render for every family, at the requested size and with the expected
;; renderers; the options, the errors and the file output; and the pieces that
;; follow R: approx with method "constant", the per-class counts and 2-norms of
;; the multi-response panels, and the labels along the top of a CV plot. The
;; images themselves are not compared, except that a formula model (#26) must
;; draw the same image as its fit labelled with its predictor names.

(module+ test
  (require rackunit
           racket/file
           racket/list
           racket/logging
           racket/math
           file/convertible
           pict
           (only-in plot/no-gui renderer2d? plot-width plot-height)
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
  (define (panel-coefficients p r)
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
        (define lines (length (ever-nonzero (panel-coefficients p r))))
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

  (test-case "a formula model's responses name the multi-response panels"
    (define table (list (cons "u" (map first Y)) (cons "v" (map second Y))
                        (cons "a" (map first X)) (cons "b" (map second X))))
    (define mg (formula-path (~ (v u) a b) table #:family 'mgaussian))
    (check-equal? (map panel-y-label (path-panels (formula-model-fit mg) 'coef '("v" "u")))
                  '("Coefficients: Response v" "Coefficients: Response u"))
    (check-equal? (map panel-y-label (path-panels (formula-model-fit mg) 'coef))
                  '("Coefficients: Response y1" "Coefficients: Response y2"))
    (check-not-equal? (png (plot-coefficient-path mg #:label #t))
                      (png (plot-coefficient-path (formula-model-fit mg) #:label '(a b))))
    (check-size (plot-coefficient-path mg #:label #t #:height 200) 400 400))

  (test-case "a lambda of 0 has no place on a log axis and is left out"
    (define p (elnet-path X y #:lambda '(1.0 0.5 0.1 0.0)))
    (define lines (length (ever-nonzero (panel-coefficients p 0))))
    (check-equal? (length (coefficient-path-renderers p)) lines)
    (check-size (plot-coefficient-path p #:label #t) 400 400)
    (check-size (plot-coefficient-path p #:xvar 'norm) 400 400)
    (define cv (elnet-cv X y #:lambda '(1.0 0.5 0.1 0.0) #:fold-ids folds))
    (check-size (plot-cv cv) 400 400))

  (test-case "a path with no nonzero coefficient cannot be plotted, as in R"
    (define p (elnet-path X y #:lambda '(1000.0 500.0)))
    (check-equal? (coefficient-path-renderers p) '())
    (check-exn #rx"nothing to plot" (lambda () (plot-coefficient-path p))))

  (test-case "one nonzero coefficient plots, with R's warning logged"
    (define p (elnet-path X y #:lambda '(7.0 5.0 3.0)))
    (check-equal? (length (ever-nonzero (panel-coefficients p 0))) 1)
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
    (check-exn #rx"extension is not" (lambda () (plot-cv cv #:out-file (build-path dir "cv.jpg"))))
    (check-exn #rx"extension is not" (lambda () (plot-coefficient-path p #:out-file "path")))
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
