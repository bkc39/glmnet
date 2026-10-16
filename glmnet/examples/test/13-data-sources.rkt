#lang racket/base

;; Runner + tests for the literate example ../13-data-sources.rkt. The
;; expected numbers are R 4.5.3 with glmnet 4.1.10:
;;   x <- as.matrix(mtcars[, c("wt", "hp", "qsec")]); y <- mtcars$mpg
;;   fit <- glmnet(x, y); length(fit$lambda); range(fit$lambda)
;;   tail(fit$dev.ratio, 1); coef(fit, s = 0.5)

(require glmnet
         "../13-data-sources.rkt")

(module+ main
  (for ([fit (in-list (list from-table from-rows from-csv from-math from-polars from-design))])
    (println fit))
  (println coefficients)
  (println formula-coefs)
  (println nested)
  (println matrix)
  (println dataframe)
  (display csv))

(module+ test
  (require racket/dict
           (only-in math/matrix matrix->list*)
           (only-in polars column-names)
           rackunit
           (submod "common.rkt" test))

  (for ([(source fit) (in-dict (list (cons 'rows from-rows)
                                     (cons 'csv from-csv)
                                     (cons 'math from-math)
                                     (cons 'polars from-polars)
                                     (cons 'design from-design)))])
    (check-equal? fit from-table (format "~a: path" source)))

  (check-lambda-range from-table 58 5.1469810628314443 0.025616646034883256)
  (check-close (final-deviance-ratio from-table) 0.83473179432107047)

  (define r-coef
    '(33.165620144257374591 -3.682927973039096692 -0.023852761602279591 0.127337031698973008))
  (check-close coefficients (list->vector r-coef))
  (for ([formula-coef (in-list formula-coefs)])
    (check-close formula-coef (map cons '("(Intercept)" "wt" "hp" "qsec") r-coef)))
  (for ([(source fit) (in-dict (list (cons 'table formula-from-table)
                                     (cons 'csv formula-from-csv)
                                     (cons 'polars formula-from-polars)))])
    (check-equal? (glmnet-path-lambda (formula-model-fit fit)) (glmnet-path-lambda from-table)
                  (format "~a formula: λ" source)))

  (check-equal? (design-matrix-column-names design) predictors)
  (check-equal? (design-matrix-nrows design) 32)
  (define first-rows (for/list ([row (in-vector rows 0 3)]) (vector->list row)))
  (check-equal? nested first-rows)
  (check-equal? (matrix->list* matrix) first-rows)
  (check-equal? (column-names dataframe) predictors)
  (check-regexp-match #rx"^wt,hp,qsec\n2.62,110,16.46\n" csv))
