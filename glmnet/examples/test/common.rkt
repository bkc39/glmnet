#lang racket/base

(module+ test
  (require (only-in glmnet deviance-ratio)
           (only-in racket/dict dict-keys dict-values)
           (only-in racket/match match)
           (only-in racket/string string-join)
           (only-in rackunit define-check fail-check with-check-info))

  (provide check-coef
           check-deviance-ratio
           check-values)

  (define-check (check-deviance-ratio fit)
    (define ratio (deviance-ratio fit))
    (with-check-info (['deviance-ratio ratio])
      (unless (and (< 0.0 ratio) (<= ratio 1.0))
        (fail-check "the deviance ratio is not in (0, 1]"))))

  (define (six-digits? got expected)
    (<= (abs (- got expected)) (max (* 1e-6 (abs expected)) 1e-12)))

  (define (entries v)
    (match v
      [(? vector?) (for/list ([x (in-vector v)] [i (in-naturals)]) (cons i x))]
      [(list (cons (? string?) _) ...) v]
      [(? list?) (for/list ([x (in-list v)] [i (in-naturals)]) (cons i x))]
      [_ #f]))

  (define (differences got expected)
    (define got-entries (entries got))
    (define expected-entries (entries expected))
    (cond
      [(and (real? got) (real? expected) (six-digits? got expected)) '()]
      [(and got-entries
            expected-entries
            (equal? (dict-keys got-entries) (dict-keys expected-entries)))
       (for*/list ([(position g e) (in-parallel (dict-keys got-entries)
                                                (dict-values got-entries)
                                                (dict-values expected-entries))]
                   [difference (in-list (differences g e))])
         (cons position difference))]
      [else (list (list got expected))]))

  (define (difference->string difference)
    (match difference
      [(list position ... got expected)
       (format "~s: got ~s, expected ~s" position got expected)]))

  (define (check-six-digits got expected)
    (define found (differences got expected))
    (with-check-info (['differences (string-join (map difference->string found) "\n")])
      (unless (null? found)
        (fail-check "not equal to six significant digits"))))

  (define-check (check-coef got expected)
    (check-six-digits got expected))

  (define-check (check-values got expected)
    (check-six-digits got expected)))
