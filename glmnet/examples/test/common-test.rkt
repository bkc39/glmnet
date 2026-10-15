#lang racket/base

(module+ test
  (require (only-in glmnet elnet-path glmnet-path-lambda logistic-fit)
           (only-in racket/match match-define)
           rackunit
           (submod "common.rkt" test))

  (define X '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0) (5.0 6.0) (6.0 5.0)))
  (define y '(0 0 0 1 1 1))

  (define (failure run-check)
    (with-handlers ([exn:test:check? values])
      (parameterize ([current-check-around (λ (check) (check))])
        (run-check))
      #f))

  (define (fails? run-check)
    (and (failure run-check) #t))

  (define (report run-check)
    (for/list ([info (in-list (exn:test:check-stack (failure run-check)))]
               #:when (memq (check-info-name info) '(position got expected)))
      (cons (check-info-name info) (check-info-value info))))

  (test-case "check-deviance-ratio is in (0, 1]"
    (check-deviance-ratio (logistic-fit X y #:lambda 0.01))
    (check-true (fails? (λ () (check-deviance-ratio (logistic-fit X y #:lambda 10.0))))
                "the null model explains nothing"))

  (test-case "check-close has a relative tolerance of 1e-6"
    (check-close #(1.0 -2.0) #(1.0000005 -2.000001))
    (check-true (fails? (λ () (check-close #(1.0) #(1.000002))))))

  (test-case "check-close has a floor of 1e-12 at zero"
    (check-close #(0.0) #(1e-13))
    (check-true (fails? (λ () (check-close #(1e-9) #(0.0))))))

  (test-case "check-close fails on a different shape"
    (check-true (fails? (λ () (check-close #(1.0 2.0) '(1.0 2.0)))) "a vector is not a list")
    (check-true (fails? (λ () (check-close #(1.0 2.0) #(1.0 2.0 3.0)))) "lengths differ")
    (check-true (fails? (λ () (check-close #() #()))) "two empty vectors")
    (check-true (fails? (λ () (check-close '() '()))) "two empty lists"))

  (test-case "check-close passes equal non-numbers"
    (check-close '("setosa") '("setosa"))
    (check-close '((a . 1.0)) '((a . 1.0)))
    (check-true (fails? (λ () (check-close '("setosa") '("virginica"))))))

  (test-case "check-close descends into nested vectors and alists, keys in order"
    (check-close #(#(1.0 0.0) #(-1.0 2.0)) #(#(1.0 0.0) #(-1.0 2.0000001)))
    (check-true (fails? (λ () (check-close #(#(1.0 0.0) #(-1.0 2.0)) #(#(1.0 0.0) #(-1.0 2.1))))))
    (define coefs '(("a" ("(Intercept)" . 1.0) ("wt" . -3.25)) ("b" ("(Intercept)" . -1.0))))
    (check-close coefs '(("a" ("(Intercept)" . 1.0) ("wt" . -3.2500001)) ("b" ("(Intercept)" . -1.0))))
    (check-true (fails? (λ () (check-close coefs '(("a" ("(Intercept)" . 1.0) ("hp" . -3.25))
                                                   ("b" ("(Intercept)" . -1.0)))))))
    (check-true (fails? (λ () (check-close coefs '(("b" ("(Intercept)" . -1.0))
                                                   ("a" ("(Intercept)" . 1.0) ("wt" . -3.25))))))))

  (test-case "a failure reports the first differing position, got and expected"
    (check-equal? (report (λ () (check-close '(("a" . #(1.0 2.0)) ("b" . #(3.0)))
                                             '(("a" . #(1.0 2.5)) ("b" . #(4.0))))))
                  '((position . 1) (got . 2.0) (expected . 2.5))))

  (test-case "check-lambda-range checks the count, first and last λ"
    (define path (elnet-path X '(1.0 2.0 2.5 4.0 5.5 6.0)))
    (match-define (vector first-λ λs ... last-λ) (glmnet-path-lambda path))
    (define count (+ 2 (length λs)))
    (check-lambda-range path count first-λ last-λ)
    (check-true (fails? (λ () (check-lambda-range path (add1 count) first-λ last-λ))))
    (check-true (fails? (λ () (check-lambda-range path count (* 1.00001 first-λ) last-λ))))
    (check-true (fails? (λ () (check-lambda-range path count first-λ (* 1.00001 last-λ)))))))
