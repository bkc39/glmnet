#lang racket/base

(module+ test
  (require (only-in glmnet logistic-fit)
           rackunit
           (submod "common.rkt" test))

  (define X '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0) (5.0 6.0) (6.0 5.0)))
  (define y '(0 0 0 1 1 1))
  (define (fails? fit)
    (with-handlers ([exn:test:check? (λ (_) #t)])
      (parameterize ([current-check-around (λ (check) (check))])
        (check-deviance-ratio fit))
      #f))

  (check-deviance-ratio (logistic-fit X y #:lambda 0.01))
  (check-true (fails? (logistic-fit X y #:lambda 10.0)) "the null model explains nothing"))
