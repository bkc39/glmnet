#lang racket/base

;; Runner + tests for the literate example ../10-formula-polynomial.rkt. The
;; expected numbers are R 4.5.3 with glmnet 4.1.10:
;;   x <- model.matrix(mpg ~ hp + I(hp^2), mtcars)[, -1]; fit <- glmnet(x, mtcars$mpg)
;;   coef(fit, s = ...)
;;   new <- data.frame(hp = c(100, 200, 300), wt = c(2.5, 3.5, 4))
;;   predict(fit, newx = model.matrix(~ hp + I(hp^2), new)[, -1], s = 0.01)
;; and the same for mpg ~ log(hp) + wt at s = 0.1.

(require glmnet
         glmnet/examples/data/mtcars
         "../10-formula-polynomial.rkt")

(module+ main
  (define-values (quadratic trap log-path curve log-predictions) (run-example))
  (printf "formula        = ~a\n" (formula-model-formula quadratic))
  (printf "predictors     = ~a\n" (formula-model-predictor-names quadratic))
  (printf "coef, λ = 0.1  = ~a\n" (coef quadratic #:lambda 0.1))
  (printf "coef, λ = 0.01 = ~a\n" (coef quadratic #:lambda 0.01))
  (printf "curve          = ~a\n" curve)
  (printf "trap           = ~a ~a\n" (formula-model-formula trap) (formula-model-predictor-names trap))
  (printf "log            = ~a\n" (coef log-path #:lambda 0.1))
  (printf "log, new cars  = ~a\n" log-predictions))

(module+ test
  (require rackunit)
  (define-values (quadratic trap log-path curve log-predictions) (run-example))

  ;; A name-keyed coef against R's coefficients, in R's order and names.
  (define (check-coef got names expected)
    (check-equal? (map car got) names)
    (for ([g (in-list (map cdr got))] [e (in-list expected)] [name (in-list names)])
      (check-within g e (* 1e-6 (+ 1 (abs e))) name)))

  (define (check-values got expected)
    (for ([g (in-list got)] [e (in-list expected)])
      (check-within g e 1e-6)))

  ;; R: mpg ~ hp + I(hp^2), 78 λ from 4.604254 down to 0.003565; I(hp^2) is
  ;; its column "(sqr hp)", which enters at the 32nd λ, 0.2574.
  (define square '("(Intercept)" "hp" "(sqr hp)"))
  (check-equal? (formula-model-predictor-names quadratic) '("hp" "(sqr hp)"))
  (define fit (formula-model-fit quadratic))
  (define lambdas (glmnet-path-lambda fit))
  (check-equal? (vector-length lambdas) 78)
  (check-within (vector-ref lambdas 0) 4.60425371923683 1e-9)
  (check-within (vector-ref lambdas 77) 0.00356490644065459 1e-9)
  (define (square-at k) (vector-ref (vector-ref (glmnet-path-coefficients fit) k) 1))
  (check-equal? (square-at 30) 0.0)
  (check-true (> (square-at 31) 0.0))
  (check-within (vector-ref lambdas 31) 0.257415085763791 1e-9)
  (check-coef (coef quadratic #:lambda 1.0) square '(27.925167222547568 -0.053409746723801 0.0))
  (check-coef (coef quadratic #:lambda 0.1) square
              '(36.372373459285334718 -0.158108169758438666 0.000265072072077869))
  (check-coef (coef quadratic #:lambda 0.01) square
              '(39.974599695276616274 -0.207377580883709334 0.000404113706006007))
  (check-within (vector-ref (glmnet-path-dev-ratio fit) 77) 0.75606931709776 1e-6)
  (check-values curve '(23.2779786669658 14.6636317587750 14.1315589707045))

  ;; R: mpg ~ hp + hp^2 is mpg ~ hp.
  (check-equal? (formula-model-predictor-names trap) '("hp"))
  (check-equal? (formula-model-fit trap) (formula-model-fit (formula-path (~ mpg hp) mtcars)))

  ;; R: mpg ~ log(hp) + wt, 55 λ from 5.146981 down to 0.033864.
  (define log-names '("(Intercept)" "(log hp)" "wt"))
  (define log-fit (formula-model-fit log-path))
  (check-equal? (vector-length (glmnet-path-lambda log-fit)) 55)
  (check-within (vector-ref (glmnet-path-lambda log-fit) 0) 5.14698106283144 1e-9)
  (check-within (vector-ref (glmnet-path-lambda log-fit) 54) 0.0338636984792013 1e-9)
  (check-coef (coef log-path #:lambda 1.0) log-names
              '(51.54713067286103 -4.67738640216780 -2.67989058468284))
  (check-coef (coef log-path #:lambda 0.1) log-names
              '(58.77272600056763 -5.79844993972794 -3.22465784595786))
  (check-coef (coef log-path #:lambda 0.01) log-names
              '(59.30243753404763 -5.88049630716284 -3.26480632856807))
  (check-within (vector-ref (glmnet-path-dev-ratio log-fit) 54) 0.85910638527768 1e-6)
  (check-values log-predictions '(24.0082325982934 16.7643955249952 12.8009974703447)))
