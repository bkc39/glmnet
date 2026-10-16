#lang racket/base

(module+ test
  (require (only-in glmnet deviance-ratio)
           (only-in rackunit define-check fail-check with-check-info))

  (provide check-deviance-ratio)

  (define-check (check-deviance-ratio fit)
    (define ratio (deviance-ratio fit))
    (with-check-info (['deviance-ratio ratio])
      (unless (and (< 0.0 ratio) (<= ratio 1.0))
        (fail-check "the deviance ratio is not in (0, 1]")))))
