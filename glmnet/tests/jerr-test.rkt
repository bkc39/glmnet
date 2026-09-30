#lang racket/base

;; Each family's jerr check: its own fatal codes raise its message, any other
;; positive code is check-jerr's, and 0 passes.

(module+ test
  (require rackunit)
  (require/expose glmnet/core/cox (check-cox-jerr))
  (require/expose glmnet/core/lognet (check-logistic-jerr))
  (require/expose glmnet/core/multinomial (check-multinomial-jerr))
  (require/expose glmnet/core/poisson (check-poisson-jerr))

  (define (check-raises check jerr pattern)
    (check-exn (lambda (e) (and (exn:fail? e) (regexp-match? pattern (exn-message e))))
               (lambda () (check jerr 'fit))
               (format "jerr ~a" jerr)))

  (test-case "Cox: all censored, and an initialization error"
    (check-raises check-cox-jerr 8888 #rx"^fit: all observations are censored")
    (check-raises check-cox-jerr 20000 #rx"initialization numerical error \\(jerr=20000\\)")
    (check-raises check-cox-jerr 30000 #rx"initialization numerical error \\(jerr=30000\\)")
    (check-raises check-cox-jerr 7777 #rx"zero variance")
    (check-equal? (check-cox-jerr 0 'fit) (void)))

  (test-case "Poisson: a negative count"
    (check-raises check-poisson-jerr 8888 #rx"^fit: response counts must be non-negative")
    (check-raises check-poisson-jerr 7777 #rx"zero variance")
    (check-equal? (check-poisson-jerr 0 'fit) (void)))

  (test-case "binomial and multinomial: a collapsed class, a degenerate one, the bounds"
    (for ([check (in-list (list check-logistic-jerr check-multinomial-jerr))])
      (check-raises check 8000 #rx"class probability collapsed.*jerr=8000")
      (check-raises check 8999 #rx"class probability collapsed.*jerr=8999")
      (check-raises check 9000 #rx"degenerate null probability \\(jerr=9000\\)")
      (check-raises check 9999 #rx"degenerate null probability \\(jerr=9999\\)")
      (check-raises check 90000 #rx"coefficient-bound adjustment failed")
      (check-raises check 10000 #rx"no penalized predictors")
      (check-raises check 7777 #rx"zero variance")
      (check-equal? (check 0 'fit) (void)))))
