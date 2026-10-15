#lang racket/base

(module+ test
  (require (only-in glmnet logistic-fit)
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

  (define (differences-info run-check)
    (for/first ([info (in-list (exn:test:check-stack (failure run-check)))]
                #:when (eq? (check-info-name info) 'differences))
      (check-info-value info)))

  (check-deviance-ratio (logistic-fit X y #:lambda 0.01))
  (check-true (fails? (λ () (check-deviance-ratio (logistic-fit X y #:lambda 10.0))))
              "the null model explains nothing")

  (test-case "check-coef compares to six significant digits"
    (check-coef #(1.0 -2.0 0.0) #(1.0000005 -2.000001 0.0))
    (check-true (fails? (λ () (check-coef #(1.0 -2.0) #(1.00001 -2.0)))))
    (check-true (fails? (λ () (check-coef #(0.0) #(1e-9)))) "a zero has a floor of 1e-12")
    (check-true (fails? (λ () (check-coef #(1.0 2.0) #(1.0 2.0 3.0)))) "lengths differ"))

  (test-case "check-coef reads a name-keyed coef by name, in order"
    (define coefs '(("(Intercept)" . 33.9) ("wt" . -3.25)))
    (check-coef coefs '(("(Intercept)" . 33.9) ("wt" . -3.25)))
    (check-true (fails? (λ () (check-coef coefs '(("(Intercept)" . 33.9) ("hp" . -3.25))))))
    (check-true (fails? (λ () (check-coef coefs '(("wt" . -3.25) ("(Intercept)" . 33.9)))))))

  (test-case "check-coef descends into classes and responses"
    (check-coef #(#(1.0 0.0) #(-1.0 2.0)) #(#(1.0 0.0) #(-1.0 2.0)))
    (check-true (fails? (λ () (check-coef #(#(1.0 0.0) #(-1.0 2.0)) #(#(1.0 0.0) #(-1.0 2.1))))))
    (check-coef '(("a" ("(Intercept)" . 1.0)) ("b" ("(Intercept)" . -1.0)))
                '(("a" ("(Intercept)" . 1.0)) ("b" ("(Intercept)" . -1.0)))))

  (test-case "a failure names each position that differs"
    (check-equal? (differences-info
                   (λ () (check-coef '(("a" . #(1.0 2.0)) ("b" . #(3.0)))
                                     '(("a" . #(1.0 2.5)) ("b" . #(4.0))))))
                  "(\"a\" 1): got 2.0, expected 2.5\n(\"b\" 0): got 3.0, expected 4.0"))

  (test-case "check-values compares predictions the same way"
    (check-values '(23.9224444 17.6269768) '(23.92244446 17.62697676))
    (check-values 0.75606931 0.756069317)
    (check-true (fails? (λ () (check-values '(1.0 2.0) '(1.0 2.1)))))))
