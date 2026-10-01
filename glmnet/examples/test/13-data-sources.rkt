#lang racket/base

;; Runner + tests for the literate example ../13-data-sources.rkt. The
;; expected numbers are R 4.5.3 with glmnet 4.1.10:
;;   x <- as.matrix(mtcars[, c("wt", "hp", "qsec")]); y <- mtcars$mpg
;;   fit <- glmnet(x, y); length(fit$lambda); range(fit$lambda)
;;   tail(fit$dev.ratio, 1); coef(fit, s = 0.5)

(require racket/list
         glmnet
         "../13-data-sources.rkt")

(define sources '(table nested csv math polars))

(module+ main
  (define-values (design-matrices fits coefficients) (run-example))
  (for ([source (in-list sources)] [x (in-list design-matrices)])
    (printf "~a: ~a\n" source x))
  (printf "same design matrices = ~a\n" (andmap (lambda (x) (equal? x (first design-matrices)))
                                                design-matrices))
  (printf "same fits            = ~a\n" (andmap (lambda (f) (equal? f (first fits))) fits))
  (displayln (first fits))
  (printf "coef, λ = 0.5        = ~a\n" coefficients))

(module+ test
  (require rackunit)
  (define-values (design-matrices fits coefficients) (run-example))

  (check-equal? (length design-matrices) (length sources))
  (for ([source (in-list sources)] [x (in-list design-matrices)] [f (in-list fits)])
    (check-equal? x (first design-matrices) (format "~a: design matrix" source))
    (check-equal? f (first fits) (format "~a: path" source)))

  (define x (first design-matrices))
  (check-equal? (design-matrix-column-names x) '("wt" "hp" "qsec"))
  (check-equal? (design-matrix-nrows x) 32)

  ;; R: 58 λ from 5.1469810628314443 down to 0.025616646034883256, 83.47% of
  ;; the deviance at the end.
  (define fit (first fits))
  (define lambdas (glmnet-path-lambda fit))
  (check-equal? (vector-length lambdas) 58)
  (check-within (vector-ref lambdas 0) 5.1469810628314443 1e-9)
  (check-within (vector-ref lambdas 57) 0.025616646034883256 1e-12)
  (check-within (vector-ref (glmnet-path-dev-ratio fit) 57) 0.83473179432107047 1e-6)

  ;; R: coef(fit, s = 0.5), to six significant digits.
  (for ([g (in-vector coefficients)]
        [e (in-list '(33.165620144257374591 -3.682927973039096692 -0.023852761602279591
                      0.127337031698973008))])
    (check-within g e (* 1e-6 (abs e)))))
