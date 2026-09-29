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
  (define-values (path infix-path pairs-path cv) (run-example))
  (printf "formula       = ~a\n" (formula-model-formula path))
  (printf "predictors    = ~a\n" (formula-model-predictor-names path))
  (printf "coef, λ = 0.5  = ~a\n" (coef path #:lambda 0.5))
  (printf "coef, λ = 0.01 = ~a\n" (coef path #:lambda 0.01))
  (printf "pairs         = ~a\n" (formula-model-predictor-names pairs-path))
  (printf "coef, λ = 0.1  = ~a\n" (coef pairs-path #:lambda 0.1))
  (printf "~a\n" cv)
  (printf "coef, λ-1se    = ~a\n" (coef cv)))

(module+ test
  (require rackunit)
  (define-values (path infix-path pairs-path cv) (run-example))

  ;; A name-keyed coef against R's coefficients, in R's order and names.
  (define (check-coef got names expected)
    (check-equal? (map car got) names)
    (for ([g (in-list (map cdr got))] [e (in-list expected)] [name (in-list names)])
      (check-within g e (* 1e-6 (+ 1 (abs e))) name)))

  (define crossed '("(Intercept)" "wt" "hp" "wt:hp"))

  ;; The expansion and R's path: 83 λ from 5.146981 down to 0.002503.
  (check-equal? (formula-model-predictor-names path) '("wt" "hp" "wt:hp"))
  (define lambdas (glmnet-path-lambda (formula-model-fit path)))
  (check-equal? (vector-length lambdas) 83)
  (check-within (vector-ref lambdas 0) 5.146981062830 1e-9)
  (check-within (vector-ref lambdas 82) 0.002502771825 1e-9)
  ;; The interaction enters negative before horsepower, leaves when it enters,
  ;; and comes back positive at the 43rd λ, 0.1034.
  (define (at k) (vector-ref (glmnet-path-coefficients (formula-model-fit path)) k))
  (check-equal? (vector-ref (at 3) 1) 0.0)
  (check-true (< (vector-ref (at 3) 2) 0.0))
  (check-true (< (vector-ref (at 7) 1) 0.0))
  (check-equal? (vector-ref (at 8) 2) 0.0)
  (check-equal? (vector-ref (at 41) 2) 0.0)
  (check-true (> (vector-ref (at 42) 2) 0.0))
  (check-within (vector-ref lambdas 42) 0.103414842150 1e-9)
  (check-coef (coef path #:lambda 1.0) crossed
              '(33.9037235411278 -3.2523144521271 -0.0228348691607798 0.0))
  (check-coef (coef path #:lambda 0.5) crossed
              '(35.5656284349261 -3.56514419056097 -0.0273032350257783 0.0))
  (check-coef (coef path #:lambda 0.1) crossed
              '(37.3564993170298 -3.97487798000443 -0.0341498399582343 0.00103286017657962))
  (check-coef (coef path #:lambda 0.01) crossed
              '(48.4848201592447 -7.76579700746338 -0.11098237778796 0.0250029001029414))
  ;; R: predict(fit, newx = model.matrix(~ wt * hp, new)[, -1], s = 0.5)
  (define new-cars (list (cons "hp" '(100 200)) (cons "wt" '(2.5 3.5))))
  (for ([g (in-list (predict path new-cars #:lambda 0.5))]
        [e (in-list '(23.9224444559459 17.6269767628071))])
    (check-within g e 1e-6))

  ;; The infix spelling fits the same path.
  (check-equal? (formula-model-fit infix-path) (formula-model-fit path))

  ;; (^ (+ wt hp qsec) 2): R's mpg ~ (wt + hp + qsec)^2.
  (define pairs '("(Intercept)" "wt" "hp" "qsec" "wt:hp" "wt:qsec" "hp:qsec"))
  (check-coef (coef pairs-path #:lambda 0.5) pairs
              '(31.3961639325795 -3.36652169969068 0.0 0.226070506646891 0.0 0.0
                -0.00177957442978985))
  (check-coef (coef pairs-path #:lambda 0.1) pairs
              '(24.7803556255478 -3.16330770785517 0.0 0.705206335387755 0.00201708780129361
                -0.0566365769326373 -0.0019345410205699))

  ;; Cross-validation on R's folds.
  (define fit (formula-model-fit cv))
  (check-within (glmnet-cv-lambda-min fit) 0.00250277182486907 1e-9)
  (check-within (glmnet-cv-lambda-1se fit) 0.0447658164589027 1e-9)
  (check-within (vector-ref (glmnet-cv-cvm fit) (glmnet-cv-index-min fit)) 6.08030633778 1e-6)
  (check-within (vector-ref (glmnet-cv-cvm fit) (glmnet-cv-index-1se fit)) 6.6098350739 1e-6)
  (check-coef (coef cv #:lambda 'lambda-min) crossed
              '(49.4121805994915 -8.08170825664582 -0.117384962057037 0.0270003747721069))
  (check-coef (coef cv) crossed
              '(44.1859978249934 -6.30138140767303 -0.0813023212966243 0.0157433836058477)))
