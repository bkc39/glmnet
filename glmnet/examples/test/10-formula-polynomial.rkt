#lang racket/base

;; Runner + tests for the literate example ../10-formula-polynomial.rkt. The
;; expected numbers are R 4.5.3 with glmnet 4.1.10:
;;   x <- model.matrix(mpg ~ hp + I(hp^2), mtcars)[, -1]; fit <- glmnet(x, mtcars$mpg)
;;   coef(fit, s = ...)
;;   new <- data.frame(hp = c(100, 200, 300), wt = c(2.5, 3.5, 4))
;;   predict(fit, newx = model.matrix(~ hp + I(hp^2), new)[, -1], s = 0.01)
;; and the same for mpg ~ log(hp) + wt at s = 0.1.

(require glmnet
         glmnet/datasets
         "../10-formula-polynomial.rkt")

(module+ main
  (println quadratic)
  (println (formula-model-predictor-names quadratic))
  (println (coef quadratic #:lambda 0.1))
  (println (coef quadratic #:lambda 0.01))
  (println curve)
  (println (formula-model-predictor-names trap))
  (println (coef log-path #:lambda 0.1))
  (println (final-deviance-ratio log-path))
  (println (final-deviance-ratio hp-path))
  (println log-predictions))

(module+ test
  (require racket/dict
           racket/match
           rackunit
           (submod "common.rkt" test))

  (define (lambda-range model)
    (match-define (vector first-λ λs ... last-λ) (glmnet-path-lambda (formula-model-fit model)))
    (list (+ 2 (length λs)) first-λ last-λ))

  ;; R: mpg ~ hp + I(hp^2), 78 λ from 4.604254 down to 0.003565; I(hp^2) is
  ;; its column "(sqr hp)", which enters at R's fit$lambda[32], 0.2574.
  (check-equal? (formula-model-predictor-names quadratic) '("hp" "(sqr hp)"))
  (match-define (list count first-λ last-λ) (lambda-range quadratic))
  (check-equal? count 78)
  (check-within first-λ 4.60425371923683 1e-9)
  (check-within last-λ 0.00356490644065459 1e-9)
  (check-within (for/first ([(λ β) (in-path quadratic)]
                            #:unless (zero? (dict-ref β "(sqr hp)")))
                  λ)
                0.257415085763791 1e-9)
  (check-true (for/and ([(λ β) (in-path quadratic)])
                (not (negative? (dict-ref β "(sqr hp)")))))
  (check-coef (coef quadratic #:lambda 1.0)
              '(("(Intercept)" . 27.925167222547568) ("hp" . -0.053409746723801)
                ("(sqr hp)" . 0.0)))
  (check-coef (coef quadratic #:lambda 0.1)
              '(("(Intercept)" . 36.372373459285334718) ("hp" . -0.158108169758438666)
                ("(sqr hp)" . 0.000265072072077869)))
  (check-coef (coef quadratic #:lambda 0.01)
              '(("(Intercept)" . 39.974599695276616274) ("hp" . -0.207377580883709334)
                ("(sqr hp)" . 0.000404113706006007)))
  (check-values (final-deviance-ratio quadratic) 0.75606931709776)
  (check-values curve '(23.2779786669658 14.6636317587750 14.1315589707045))

  (check-equal? (formula-model-predictor-names as-in-r) '("hp" "(I (expt hp 2))"))
  (check-equal? (formula-model-fit as-in-r) (formula-model-fit quadratic))

  ;; R: mpg ~ hp + hp^2 is mpg ~ hp.
  (check-equal? (formula-model-predictor-names trap) '("hp"))
  (check-equal? (formula-model-fit trap) (formula-model-fit (formula-path (~ mpg hp) mtcars)))

  ;; R: mpg ~ log(hp) + wt, 55 λ from 5.146981 down to 0.033864.
  (match-define (list log-count log-first-λ log-last-λ) (lambda-range log-path))
  (check-equal? log-count 55)
  (check-within log-first-λ 5.14698106283144 1e-9)
  (check-within log-last-λ 0.0338636984792013 1e-9)
  (check-coef (coef log-path #:lambda 1.0)
              '(("(Intercept)" . 51.54713067286103) ("(log hp)" . -4.67738640216780)
                ("wt" . -2.67989058468284)))
  (check-coef (coef log-path #:lambda 0.1)
              '(("(Intercept)" . 58.77272600056763) ("(log hp)" . -5.79844993972794)
                ("wt" . -3.22465784595786)))
  (check-coef (coef log-path #:lambda 0.01)
              '(("(Intercept)" . 59.30243753404763) ("(log hp)" . -5.88049630716284)
                ("wt" . -3.26480632856807)))
  (check-values (final-deviance-ratio log-path) 0.85910638527768)
  (check-values log-predictions '(24.0082325982934 16.7643955249952 12.8009974703447))
  (check-= (final-deviance-ratio hp-path) 0.827 5e-4 "the 82.7% of the prose"))
