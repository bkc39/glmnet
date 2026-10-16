#lang racket/base

;; Runner + tests for the literate example ../12-quick-start.rkt. The
;; expected numbers are R 4.5.3 with glmnet 4.1.10:
;;   data(QuickStartExample); x <- QuickStartExample$x; y <- QuickStartExample$y
;;   fit <- glmnet(x, y); print(fit)
;;   b <- as.matrix(fit$beta); order(apply(b, 1, function(r) min(which(r != 0))))
;;   coef(fit, s = 0.1); predict(fit, newx = x[1:5, ], s = c(0.1, 0.05))
;;   foldid <- rep(1:10, length.out = 100); cvfit <- cv.glmnet(x, y, foldid = foldid)
;;   cvfit$lambda.min; coef(cvfit, s = "lambda.min")
;;   predict(cvfit, newx = x[1:5, ], s = "lambda.min")

(require glmnet
         (only-in pict pict-height pict-width)
         "../12-quick-start.rkt")

(module+ main
  (println fit)
  (printf "path plot: ~ax~a pict\n" (pict-width path-plot) (pict-height path-plot))
  (println entries)
  (println coefficients)
  (println predictions)
  (println cvfit)
  (printf "cv plot: ~ax~a pict\n" (pict-width cv-plot) (pict-height cv-plot))
  (println lambda-min)
  (println coefficients-min)
  (println predictions-min))

(module+ test
  (require racket/dict
           racket/list
           racket/match
           racket/string
           rackunit
           (only-in pict pict?)
           (submod "common.rkt" test))

  (check-lambda-range fit 67 1.63076208204452 0.00351337040074534)
  (check-close (final-deviance-ratio fit) 0.913176465748194 "R's fit$dev.ratio[67]")
  (check-equal? (vector->list (glmnet-path-df fit))
                '(0 2 2 2 2 2 4 5 5 6 6 6 6 6 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 9 9 9 9 10 11 11
                  12 15 16 16 16 17 17 18 18 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 20 20
                  20 20 20 20))

  (check-equal? (take (rest (string-split (format "~a" fit) "\n")) 4)
                '("   Df  %Dev  Lambda"
                  "1   0  0.00 1.63100"
                  "2   2  5.53 1.48600"
                  "3   2 14.59 1.35400"))

  (check-equal? (dict-keys entries)
                '("V1" "V14" "V5" "V20" "V6" "V3" "V8" "V11" "V7" "V10" "V15" "V13" "V2"
                  "V12" "V18" "V4" "V16" "V17" "V9" "V19")
                "R's order of entry, ties in column order")

  (check-pred pict? path-plot)
  (check-pred pict? cv-plot)

  (check-close coefficients
               #(0.15092807247412948 1.32059719454791158 0.0 0.67511023448474050 0.0
                 -0.81741151754896735 0.52143667145873451 0.00482933514861668
                 0.31941591671556846 0.0 0.0 0.14249851856673512 0.0 0.0
                 -1.05997870191922949 0.0 0.0 0.0 0.0 0.0 -1.02187370368307984))
  (match-define (list predictions-0.1 predictions-0.05) predictions)
  (check-close predictions-0.1
               '(-1.366760695877866 2.562379220607292 0.565026939440315 1.922395960000677
                 1.446774332126730))
  (check-close predictions-0.05
               '(-1.336265196924598 2.589424524770468 0.587286847997336 2.097722192283847
                 1.643627992490082))

  (define (cv-row λ*)
    (for/first ([λ (in-vector (glmnet-cv-lambda cvfit))]
                [nzero (in-vector (glmnet-cv-nzero cvfit))]
                [cvm (in-vector (glmnet-cv-cvm cvfit))]
                [cvsd (in-vector (glmnet-cv-cvsd cvfit))]
                #:when (= λ λ*))
      `((nzero . ,nzero) (cvm . ,cvm) (cvsd . ,cvsd))))
  (check-within lambda-min 0.0689688891531137 1e-12)
  (check-within (glmnet-cv-lambda-1se cvfit) 0.132276140242909 1e-12)
  (check-equal? (glmnet-cv-index-min cvfit) 34 "R's 35th λ")
  (check-equal? (glmnet-cv-index-1se cvfit) 27 "R's 28th λ")
  (check-close (cv-row lambda-min)
               '((nzero . 10) (cvm . 1.02152422514955) (cvsd . 0.0820845136953187)))
  (check-equal? (dict-ref (cv-row (glmnet-cv-lambda-1se cvfit)) 'nzero) 8)
  (check-close coefficients-min
               #(0.14792705583241417 1.33739391120070783 0.0 0.70408648061563961 0.0
                 -0.84285314968357261 0.54940348029658681 0.03270391434751053
                 0.34243052114343225 0.0 0.00120660768673628 0.17899598882838239 0.0 0.0
                 -1.07999347268151591 0.0 0.0 0.0 0.0 0.0 -1.06138244352511890))
  (check-close predictions-min
               '(-1.362697695821565 2.573677362048242 0.576733459651696 2.006260443038276
                 1.541006086750130)))
