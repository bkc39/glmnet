#lang racket/base

;; Runner + tests for the literate example ../13-data-sources.rkt. The
;; expected numbers are R 4.5.3 with glmnet 4.1.10:
;;   x <- as.matrix(mtcars[, c("wt", "hp", "qsec")]); y <- mtcars$mpg
;;   fit <- glmnet(x, y); length(fit$lambda); range(fit$lambda)
;;   tail(fit$dev.ratio, 1); coef(fit, s = 0.5)

(require glmnet
         "../13-data-sources.rkt")

(module+ main
  (for ([fit (in-list fits)])
    (println fit))
  (println coefficients)
  (println formula-coefs)
  (println nested)
  (println matrix)
  (println dataframe)
  (display csv))

(module+ test
  (require racket/match
           (only-in math/matrix matrix->vector*)
           (only-in polars column-names)
           rackunit
           (submod "common.rkt" test))

  (match-define (list from-table from-rows from-csv from-math from-polars) fits)
  (for ([source (in-list '(rows csv math polars design))]
        [fit (in-list (list from-rows from-csv from-math from-polars from-design))])
    (check-equal? fit from-table (format "~a: path" source)))

  ;; R: 58 λ from 5.1469810628314443 down to 0.025616646034883256, 83.47% of
  ;; the deviance at the end.
  (match-define (vector first-λ λs ... last-λ) (glmnet-path-lambda from-table))
  (check-equal? (+ 2 (length λs)) 58)
  (check-within first-λ 5.1469810628314443 1e-9)
  (check-within last-λ 0.025616646034883256 1e-12)
  (check-values (for/last ([ratio (in-vector (deviance-ratio from-table))]) ratio)
                0.83473179432107047)

  (define r-coef
    '(33.165620144257374591 -3.682927973039096692 -0.023852761602279591 0.127337031698973008))
  (check-coef coefficients (list->vector r-coef))
  (for ([formula-coef (in-list formula-coefs)])
    (check-coef formula-coef (map cons '("(Intercept)" "wt" "hp" "qsec") r-coef)))

  (check-equal? (design-matrix-column-names design) predictors)
  (check-equal? (design-matrix-nrows design) 32)
  (define first-rows (for/list ([row (in-vector rows 0 3)]) (vector->list row)))
  (check-equal? nested first-rows)
  (check-equal? (map vector->list (vector->list (matrix->vector* matrix))) first-rows)
  (check-equal? (column-names dataframe) predictors)
  (check-regexp-match #rx"^wt,hp,qsec\n2.62,110,16.46\n" csv))
