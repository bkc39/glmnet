#lang racket/base

;; Runner + tests for the literate example ../09-formula-interactions.rkt. The
;; expected numbers are R 4.5.3 with glmnet 4.1.10:
;;   x <- model.matrix(mpg ~ wt * hp, mtcars)[, -1]; fit <- glmnet(x, mtcars$mpg)
;;   coef(fit, s = ...)
;;   cv.glmnet(x, mtcars$mpg, foldid = rep(1:4, length.out = 32))
;; and the same for mpg ~ (wt + hp + qsec)^2.

(require glmnet
         "../09-formula-interactions.rkt")

(module+ main
  (println path)
  (println (formula-model-predictor-names path))
  (println (coef path #:lambda 0.5))
  (println (coef path #:lambda 0.01))
  (println (formula-model-predictor-names pairs-path))
  (println (coef pairs-path #:lambda 0.1))
  (println cv)
  (println (coef cv))
  (println predictions))

(module+ test
  (require racket/dict
           rackunit
           (submod "common.rkt" test))

  (check-equal? (formula-model-predictor-names path) '("wt" "hp" "wt:hp"))
  (check-lambda-range path 83 5.146981062830 0.002502771825)

  (define hp-λ ((entry path) "hp"))
  (define wt:hp-λ ((entry path) "wt:hp"))
  (define leave-λ
    (for/first ([(λ β) (in-path path)]
                #:when (and (< λ hp-λ) (zero? (dict-ref β "wt:hp"))))
      λ))
  (define return-λ
    (for/first ([(λ β) (in-path path)]
                #:when (positive? (dict-ref β "wt:hp")))
      λ))
  (check-close ((entry path) "wt") 4.68973745088373 "wt enters at R's fit$lambda[2]")
  (check-close wt:hp-λ 3.89350242165014 "wt:hp enters at R's fit$lambda[4]")
  (check-true (> wt:hp-λ hp-λ) "wt:hp enters before hp")
  (check-close hp-λ 2.68364019197806 "hp enters at R's fit$lambda[8]")
  (check-close leave-λ 2.44523299374503 "wt:hp leaves at R's fit$lambda[9]")
  (check-close return-λ 0.103414842150 "wt:hp comes back positive at R's fit$lambda[43]")
  (check-true (for/and ([(λ β) (in-path path)]
                        #:when (and (< leave-λ λ) (<= λ wt:hp-λ)))
                (negative? (dict-ref β "wt:hp")))
              "wt:hp is negative from its entry to R's fit$lambda[8]")
  (check-true (for/and ([(λ β) (in-path path)]
                        #:when (< return-λ λ leave-λ))
                (zero? (dict-ref β "wt:hp")))
              "wt:hp is out strictly between R's fit$lambda[9] and fit$lambda[43]")
  (check-true (for/and ([(λ β) (in-path path)]
                        #:when (<= λ return-λ))
                (positive? (dict-ref β "wt:hp")))
              "wt:hp is positive from R's fit$lambda[43] on")
  (check-close (coef path #:lambda 1.0)
               '(("(Intercept)" . 33.9037235411278) ("wt" . -3.2523144521271)
                 ("hp" . -0.0228348691607798) ("wt:hp" . 0.0)))
  (check-close (coef path #:lambda 0.5)
               '(("(Intercept)" . 35.5656284349261) ("wt" . -3.56514419056097)
                 ("hp" . -0.0273032350257783) ("wt:hp" . 0.0)))
  (check-close (coef path #:lambda 0.1)
               '(("(Intercept)" . 37.3564993170298) ("wt" . -3.97487798000443)
                 ("hp" . -0.0341498399582343) ("wt:hp" . 0.00103286017657962)))
  (check-close (coef path #:lambda 0.01)
               '(("(Intercept)" . 48.4848201592447) ("wt" . -7.76579700746338)
                 ("hp" . -0.11098237778796) ("wt:hp" . 0.0250029001029414)))
  (check-close (predict path new-cars #:lambda 0.5) '(23.9224444559459 17.6269767628071)
               "R's predict(fit, newx, s = 0.5)")

  (check-equal? (formula-model-fit infix-path) (formula-model-fit path))

  (check-close (coef pairs-path #:lambda 0.5)
               '(("(Intercept)" . 31.3961639325795) ("wt" . -3.36652169969068) ("hp" . 0.0)
                 ("qsec" . 0.226070506646891) ("wt:hp" . 0.0) ("wt:qsec" . 0.0)
                 ("hp:qsec" . -0.00177957442978985)))
  (check-close (coef pairs-path #:lambda 0.1)
               '(("(Intercept)" . 24.7803556255478) ("wt" . -3.16330770785517) ("hp" . 0.0)
                 ("qsec" . 0.705206335387755) ("wt:hp" . 0.00201708780129361)
                 ("wt:qsec" . -0.0566365769326373) ("hp:qsec" . -0.0019345410205699)))

  (define fit (formula-model-fit cv))
  (define (cv-error lam)
    (for/first ([(λ err) (in-parallel (glmnet-cv-lambda fit) (glmnet-cv-cvm fit))]
                #:when (= λ lam))
      err))
  (check-within (glmnet-cv-lambda-min fit) 0.00250277182486907 1e-9)
  (check-within (glmnet-cv-lambda-1se fit) 0.0447658164589027 1e-9)
  (check-within (cv-error (glmnet-cv-lambda-min fit)) 6.08030633778 1e-6)
  (check-within (cv-error (glmnet-cv-lambda-1se fit)) 6.6098350739 1e-6)
  (check-close (coef cv #:lambda 'lambda-min)
               '(("(Intercept)" . 49.4121805994915) ("wt" . -8.08170825664582)
                 ("hp" . -0.117384962057037) ("wt:hp" . 0.0270003747721069)))
  (check-close (coef cv)
               '(("(Intercept)" . 44.1859978249934) ("wt" . -6.30138140767303)
                 ("hp" . -0.0813023212966243) ("wt:hp" . 0.0157433836058477)))
  (check-close predictions '(24.2381580776103 16.8910671629063) "R's predict(cv, newx)"))
