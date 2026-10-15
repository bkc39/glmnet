#lang racket/base

;; Runner + tests for the literate example ../11-formula-factors.rkt. The
;; expected numbers are R 4.5.3 with glmnet 4.1.10, in the C collation:
;;   x <- model.matrix(mpg ~ wt + factor(cyl), mtcars)[, -1]; fit <- glmnet(x, mtcars$mpg)
;;   coef(fit, s = c(1, 0.1, 0.01))
;;   tt <- delete.response(terms(mpg ~ wt + factor(cyl)))
;;   new <- data.frame(wt = c(2.5, 3.5, 3.5), cyl = c(4, 4, 8))
;;   predict(fit, newx = model.matrix(tt, model.frame(tt, new,
;;           xlev = list(`factor(cyl)` = c("4", "6", "8"))))[, -1], s = 0.1)
;; the same for mpg ~ wt * factor(cyl), and for the species
;;   fit <- glmnet(as.matrix(iris[, 1:4]), as.character(iris$Species), "multinomial")
;;   coef(fit, s = 0.05); predict(fit, flowers, s = 0.05, type = "class")

(require glmnet
         "../11-formula-factors.rkt")

(module+ main
  (println cylinders)
  (println (formula-model-predictor-names cylinders))
  (println (formula-model-levels cylinders))
  (println (coef cylinders #:lambda 0.1))
  (println (coef cylinders #:lambda 0.01))
  (println predictions)
  (println (coef slopes #:lambda 0.01))
  (println (coef species #:lambda 0.05))
  (println classes))

(module+ test
  (require racket/match
           rackunit
           (submod "common.rkt" test))

  (check-equal? (formula-model-predictor-names cylinders) '("wt" "(factor cyl)6" "(factor cyl)8"))
  (check-equal? (formula-model-levels cylinders) '(("(factor cyl)" "4" "6" "8")))
  (check-lambda-range cylinders 61 5.14698106283144 0.0193780533003369)
  (match-define (vector _ second-λ _ ...) (glmnet-path-lambda (formula-model-fit cylinders)))
  (check-equal? ((entry cylinders) "wt") second-λ "wt enters at R's fit$lambda[2]")
  (check-close (map (entry cylinders) '("(factor cyl)8" "(factor cyl)6"))
               '(2.44523299374503 0.800703565270879)
               "(factor cyl)8 enters at R's fit$lambda[9], (factor cyl)6 at fit$lambda[21]")
  (check-close (coef cylinders #:lambda 1.0)
               '(("(Intercept)" . 32.60819854761174) ("wt" . -3.64802252248363)
                 ("(factor cyl)6" . 0.0) ("(factor cyl)8" . -1.78508134206005)))
  (check-close (coef cylinders #:lambda 0.1)
               '(("(Intercept)" . 33.87817654234954) ("wt" . -3.27236069460026)
                 ("(factor cyl)6" . -3.74496474977405) ("(factor cyl)8" . -5.57791556259148)))
  (check-close (coef cylinders #:lambda 0.01)
               '(("(Intercept)" . 33.97371114365072) ("wt" . -3.22086080520115)
                 ("(factor cyl)6" . -4.15330452151469) ("(factor cyl)8" . -5.97082595208224)))
  (check-close (final-deviance-ratio cylinders) 0.837388980136312)
  (check-close predictions '(25.6972748058489 22.4249141112486 16.8469985486571))
  (check-exn #rx"^predict: a factor has levels that the model was not fitted with\n  factor: \"\\(factor cyl\\)\"\n  new levels: '\\(\"5\"\\)"
             (λ () (predict cylinders '((wt 3.0) (cyl 5)))))

  (check-lambda-range slopes 92 5.14698106283144 0.00108339017708788)
  (check-close (map (entry slopes) '("wt:(factor cyl)8" "wt:(factor cyl)6"))
               '(0.0591777748217013 0.0160880002861375)
               "wt:(factor cyl)8 enters at R's fit$lambda[49], wt:(factor cyl)6 at fit$lambda[63]")
  (check-close (coef slopes #:lambda 0.1)
               '(("(Intercept)" . 33.87817654234954) ("wt" . -3.27236069460026)
                 ("(factor cyl)6" . -3.74496474977405) ("(factor cyl)8" . -5.57791556259148)
                 ("wt:(factor cyl)6" . 0.0) ("wt:(factor cyl)8" . 0.0)))
  (check-close (coef slopes #:lambda 0.01)
               '(("(Intercept)" . 38.01918583595351) ("wt" . -4.98069577942272)
                 ("(factor cyl)6" . -6.02448840093726) ("(factor cyl)8" . -13.39864037665694)
                 ("wt:(factor cyl)6" . 1.05642912906303) ("wt:(factor cyl)8" . 2.60336397535620)))
  (check-close (final-deviance-ratio slopes) 0.861496613587194)

  (check-lambda-range species 100 0.434995773978776 4.34995773978777e-05)
  (check-close (coef species #:lambda 0.05)
               '(("setosa"
                  ("(Intercept)" . 2.869706551251400) ("Sepal.Length" . 0.0)
                  ("Sepal.Width" . 0.735054889751889) ("Petal.Length" . -1.360405695080846)
                  ("Petal.Width" . 0.0))
                 ("versicolor"
                  ("(Intercept)" . 1.3944568206526984) ("Sepal.Length" . 0.0)
                  ("Sepal.Width" . -0.0663947119206749) ("Petal.Length" . 0.0)
                  ("Petal.Width" . 0.0))
                 ("virginica"
                  ("(Intercept)" . -4.26416337190410) ("Sepal.Length" . 0.0)
                  ("Sepal.Width" . 0.0) ("Petal.Length" . 0.0)
                  ("Petal.Width" . 3.32953365348215))))
  (check-close classes '("setosa" "versicolor" "virginica"))
  (check-close (final-deviance-ratio species) 0.96376556042186))
