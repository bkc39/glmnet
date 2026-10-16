#lang racket/base

(module+ test
  (require (only-in glmnet deviance-ratio in-path)
           (only-in racket/dict dict-ref)
           (only-in racket/match match match* match-define)
           (only-in rackunit define-check fail-check with-check-info))

  (provide check-close
           check-deviance-ratio
           check-lambda-range
           entry
           final-deviance-ratio)

  (define-check (check-deviance-ratio fit)
    (define ratio (deviance-ratio fit))
    (with-check-info (['deviance-ratio ratio])
      (unless (and (< 0.0 ratio) (<= ratio 1.0))
        (fail-check "the deviance ratio is not in (0, 1]"))))

  (define (close? got expected)
    (and (rational? expected)
         (<= (abs (- got expected)) (max (* 1e-6 (abs expected)) 1e-12))))

  (define (difference got expected position)
    (define (pairwise gs es)
      (for/or ([g (in-list gs)] [e (in-list es)] [i (in-naturals)])
        (difference g e i)))
    (match* (got expected)
      [(_ _) #:when (equal? got expected) #f]
      [((? real?) (? real?)) #:when (close? got expected) #f]
      [((? vector?) (? vector?)) #:when (= (vector-length got) (vector-length expected))
       (pairwise (vector->list got) (vector->list expected))]
      [((? list?) (? list?)) #:when (= (length got) (length expected))
       (pairwise got expected)]
      [((cons key g) (cons key e)) #:when (not (list? got))
       (difference g e key)]
      [(_ _) (list position got expected)]))

  (define (fail-unless-close got expected)
    (match* (got (difference got expected #f))
      [((or (vector) '()) _) (fail-check "an empty value checks nothing")]
      [(_ #f) (void)]
      [(_ (list position g e))
       (with-check-info (['position position] ['got g] ['expected e])
         (fail-check "not equal to six significant digits"))]))

  (define-check (check-close got expected)
    (fail-unless-close got expected))

  (define-check (check-lambda-range model count largest smallest)
    (match-define (and λs (list largest-λ _ ... smallest-λ)) (for/list ([(λ β) (in-path model)]) λ))
    (fail-unless-close `((count . ,(length λs)) (largest . ,largest-λ) (smallest . ,smallest-λ))
                       `((count . ,count) (largest . ,largest) (smallest . ,smallest))))

  (define ((entry model) name)
    (for/first ([(λ β) (in-path model)]
                #:unless (zero? (dict-ref β name)))
      λ))

  (define (final-deviance-ratio model)
    (for/last ([ratio (in-vector (deviance-ratio model))]) ratio)))
