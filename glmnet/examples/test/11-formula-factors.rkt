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
  (define-values (cylinders slopes predictions unseen species classes) (run-example))
  (printf "formula        = ~a\n" (formula-model-formula cylinders))
  (printf "predictors     = ~a\n" (formula-model-predictor-names cylinders))
  (printf "levels         = ~a\n" (formula-model-levels cylinders))
  (printf "coef, λ = 0.1  = ~a\n" (coef cylinders #:lambda 0.1))
  (printf "coef, λ = 0.01 = ~a\n" (coef cylinders #:lambda 0.01))
  (printf "new cars       = ~a\n" predictions)
  (printf "unseen level   = ~a\n" unseen)
  (printf "slopes         = ~a\n" (coef slopes #:lambda 0.01))
  (printf "species        = ~a\n" (coef species #:lambda 0.05))
  (printf "flowers        = ~a\n" classes))

(module+ test
  (require rackunit)
  (define-values (cylinders slopes predictions unseen species classes) (run-example))

  ;; A name-keyed coef against R's coefficients, in R's order and names.
  (define (check-coef got names expected)
    (check-equal? (map car got) names)
    (for ([g (in-list (map cdr got))] [e (in-list expected)] [name (in-list names)])
      (check-within g e (* 1e-6 (+ 1 (abs e))) name)))

  (define (check-values got expected)
    (for ([g (in-list got)] [e (in-list expected)])
      (check-within g e 1e-6)))

  ;; The index of the first λ at which predictor j of a path is not zero.
  (define (entry path j)
    (for/first ([beta (in-vector (glmnet-path-coefficients path))]
                [k (in-naturals)]
                #:unless (zero? (vector-ref beta j)))
      k))

  ;; R: mpg ~ wt + factor(cyl), 61 λ from 5.146981 down to 0.019378; wt
  ;; enters at the 2nd, factor(cyl)8 at the 9th and factor(cyl)6 at the 21st.
  (define names '("(Intercept)" "wt" "(factor cyl)6" "(factor cyl)8"))
  (check-equal? (formula-model-predictor-names cylinders) (cdr names))
  (check-equal? (formula-model-levels cylinders) '(("(factor cyl)" "4" "6" "8")))
  (define fit (formula-model-fit cylinders))
  (define lambdas (glmnet-path-lambda fit))
  (check-equal? (vector-length lambdas) 61)
  (check-within (vector-ref lambdas 0) 5.14698106283144 1e-9)
  (check-within (vector-ref lambdas 60) 0.0193780533003369 1e-9)
  (check-equal? (map (lambda (j) (entry fit j)) '(0 1 2)) '(1 20 8))
  (check-within (vector-ref lambdas 8) 2.44523299374503 1e-9)
  (check-within (vector-ref lambdas 20) 0.800703565270879 1e-9)
  (check-coef (coef cylinders #:lambda 1.0) names
              '(32.60819854761174 -3.64802252248363 0.0 -1.78508134206005))
  (check-coef (coef cylinders #:lambda 0.1) names
              '(33.87817654234954 -3.27236069460026 -3.74496474977405 -5.57791556259148))
  (check-coef (coef cylinders #:lambda 0.01) names
              '(33.97371114365072 -3.22086080520115 -4.15330452151469 -5.97082595208224))
  (check-within (vector-ref (glmnet-path-dev-ratio fit) 60) 0.837388980136312 1e-6)
  (check-values predictions '(25.6972748058489 22.4249141112486 16.8469985486571))
  (check-regexp-match #rx"^predict: a factor has levels that the model was not fitted with\n  factor: \"\\(factor cyl\\)\"\n  new levels: '\\(\"5\"\\)"
                      unseen)

  ;; R: mpg ~ wt * factor(cyl), 92 λ from 5.146981 down to 0.001083;
  ;; wt:factor(cyl)8 enters at the 49th and wt:factor(cyl)6 at the 63rd.
  (define slope-names '("(Intercept)" "wt" "(factor cyl)6" "(factor cyl)8"
                        "wt:(factor cyl)6" "wt:(factor cyl)8"))
  (define slope-fit (formula-model-fit slopes))
  (define slope-lambdas (glmnet-path-lambda slope-fit))
  (check-equal? (vector-length slope-lambdas) 92)
  (check-within (vector-ref slope-lambdas 91) 0.00108339017708788 1e-9)
  (check-equal? (map (lambda (j) (entry slope-fit j)) '(3 4)) '(62 48))
  (check-within (vector-ref slope-lambdas 48) 0.0591777748217013 1e-9)
  (check-within (vector-ref slope-lambdas 62) 0.0160880002861375 1e-9)
  (check-coef (coef slopes #:lambda 0.1) slope-names
              '(33.87817654234954 -3.27236069460026 -3.74496474977405 -5.57791556259148 0.0 0.0))
  (check-coef (coef slopes #:lambda 0.01) slope-names
              '(38.01918583595351 -4.98069577942272 -6.02448840093726 -13.39864037665694
                1.05642912906303 2.60336397535620))
  (check-within (vector-ref (glmnet-path-dev-ratio slope-fit) 91) 0.861496613587194 1e-6)

  ;; R: the multinomial path of the species, 100 λ from 0.434996 down to
  ;; 4.349958e-05.
  (define species-fit (formula-model-fit species))
  (define species-lambdas (glmnet-path-lambda species-fit))
  (check-equal? (vector-length species-lambdas) 100)
  (check-within (vector-ref species-lambdas 0) 0.434995773978776 1e-9)
  (check-within (vector-ref species-lambdas 99) 4.34995773978777e-05 1e-12)
  (define species-coef (coef species #:lambda 0.05))
  (check-equal? (map car species-coef) '("setosa" "versicolor" "virginica"))
  (define rows '("(Intercept)" "Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width"))
  (check-coef (cdr (assoc "setosa" species-coef)) rows
              '(2.869706551251400 0.0 0.735054889751889 -1.360405695080846 0.0))
  (check-coef (cdr (assoc "versicolor" species-coef)) rows
              '(1.3944568206526984 0.0 -0.0663947119206749 0.0 0.0))
  (check-coef (cdr (assoc "virginica" species-coef)) rows
              '(-4.26416337190410 0.0 0.0 0.0 3.32953365348215))
  (check-equal? classes '("setosa" "versicolor" "virginica"))
  (check-within (vector-ref (glmnet-path-dev-ratio species-fit) 99) 0.96376556042186 1e-6))
