#lang racket/base

;; Unit tests for `in-path` (#59, decision A): every fitted λ of a model's path
;; with its coefficients, equal to what `coef` gives at that λ, for each
;; family's path, single fit and cross-validation, and for formula models.

(module+ test
  (require rackunit
           racket/match
           racket/sequence
           glmnet
           (only-in glmnet/datasets mtcars [iris iris-species])
           (file "../private/demo-utils.rkt"))

  (define longley (load-table "longley"))
  (define wdbc (load-table "wdbc"))
  (define iris (load-table "iris"))
  (define veteran (load-table "veteran"))
  (define warpbreaks (load-table "warpbreaks"))
  (define linnerud (load-table "linnerud"))

  (define-values (Xg yg) (load-longley))
  (define-values (Xb yb) (load-wdbc))
  (define-values (Xm ym) (load-iris))
  (define-values (Xc tc sc) (load-veteran))
  (define-values (Xp yp) (load-warpbreaks))
  (define-values (Xr Yr) (load-linnerud))

  (define (folds n) (for/list ([i (in-range n)]) (modulo i 3)))

  ;; in-path gives the path's λ in order, each with coef's value there.
  (define (check-in-path model)
    (define lams (glmnet-path-lambda (glmnet-model->path model)))
    (define steps (for/list ([(λ β) (in-path model)]) (cons λ β)))
    (check-equal? (map car steps) (vector->list lams))
    (for ([step (in-list steps)])
      (match-define (cons λ β) step)
      (check-equal? β (coef model #:lambda λ) (format "at λ = ~a" λ))))

  (test-case "every family's path"
    (for ([path (list (elnet-path Xg yg #:nlambda 20)
                      (elnet-path Xg yg #:lambda '(5.0 1.0 0.1) #:alpha 0.5)
                      (logistic-path Xb yb #:nlambda 10)
                      (multinomial-path Xm ym #:nlambda 20)
                      (cox-path Xc tc sc #:nlambda 20)
                      (poisson-path Xp yp #:nlambda 20)
                      (mgaussian-path Xr Yr #:nlambda 20))])
      (check-in-path path)))

  (test-case "the layout is coef's: intercept first, none for Cox, one vector per class or response"
    (define-values (λ₁ β₁) (sequence-ref (in-path (elnet-path Xg yg #:nlambda 5)) 1))
    (check-equal? (vector-length β₁) (add1 (length (car Xg))))
    (define-values (λc βc) (sequence-ref (in-path (cox-path Xc tc sc #:nlambda 5)) 1))
    (check-equal? (vector-length βc) (length (car Xc)))
    (define-values (λm βm) (sequence-ref (in-path (multinomial-path Xm ym #:nlambda 5)) 1))
    (check-equal? (vector-length βm) 3)
    (check-equal? (vector-length (vector-ref βm 0)) (add1 (length (car Xm))))
    (define-values (λr βr) (sequence-ref (in-path (mgaussian-path Xr Yr #:nlambda 5)) 1))
    (check-equal? (vector-length βr) 3))

  (test-case "a single fit is a path with one λ"
    (define fit (lasso Xg yg #:lambda 0.5))
    (check-equal? (for/list ([(λ β) (in-path fit)]) (list λ β))
                  (list (list 0.5 (coef fit))))
    (for ([fit (list (logistic-fit Xb yb #:lambda 0.05)
                     (multinomial-fit Xm ym #:lambda 0.02)
                     (cox-fit Xc tc sc #:lambda 0.05)
                     (poisson-fit Xp yp #:lambda 0.05)
                     (mgaussian-fit Xr Yr #:lambda 1.0))])
      (check-in-path fit)))

  (test-case "a cross-validation result walks its path of all the data"
    (for ([cv (list (elnet-cv Xg yg #:fold-ids (folds 16) #:nlambda 20)
                    (multinomial-cv Xm ym #:fold-ids (folds 150) #:nlambda 10)
                    (cox-cv Xc tc sc #:fold-ids (folds 137) #:nlambda 10)
                    (mgaussian-cv Xr Yr #:fold-ids (folds 20) #:nlambda 10))])
      (check-equal? (for/vector ([(λ β) (in-path cv)]) λ)
                    (glmnet-path-lambda (glmnet-cv-path cv)))
      (check-in-path cv)))

  (test-case "a formula path gives coefficients keyed by name"
    (define path (formula-path (~ mpg (+ wt hp)) mtcars))
    (check-in-path path)
    (define-values (λ β) (sequence-ref (in-path path) 10))
    (check-equal? (map car β) '("(Intercept)" "wt" "hp"))
    (check-equal? (for/list ([(λ β) (in-path path)]) λ)
                  (vector->list (glmnet-path-lambda (formula-model-fit path)))))

  (test-case "formula models of every family, as paths, fits and cross-validations"
    (for ([model (list (formula-path (~ Employed all) longley #:nlambda 20)
                       (formula-path (~ class all) iris #:family 'multinomial #:nlambda 10)
                       (formula-path (~ Species all) iris-species #:family 'multinomial #:nlambda 10)
                       (formula-path (~ (surv time status) all) veteran #:family 'cox #:nlambda 10)
                       (formula-path (~ breaks all) warpbreaks #:family 'poisson #:nlambda 10)
                       (formula-path (~ (weight waist pulse) all) linnerud #:family 'mgaussian
                                     #:nlambda 10)
                       (formula-fit (~ mpg wt hp) mtcars #:lambda 0.5)
                       (formula-cv (~ mpg wt hp) mtcars #:fold-ids (folds 32) #:nlambda 10)
                       (formula-cv (~ Species all) iris-species #:family 'multinomial
                                   #:fold-ids (folds 150) #:nlambda 10))])
      (check-in-path model))
    (define-values (λ β)
      (sequence-ref (in-path (formula-path (~ Species all) iris-species #:family 'multinomial
                                           #:nlambda 10))
                    5))
    (check-equal? (map car β) '("setosa" "versicolor" "virginica")))

  (test-case "where a λ repeats, each step gives the path's own fit there"
    (define path (elnet-path Xg yg #:lambda '(1.0 1.0 0.1)))
    (define steps (for/list ([(λ β) (in-path path)]) β))
    (check-equal? (list-ref steps 0) (coef path #:lambda 1.0))
    (check-equal? (list-ref steps 2) (coef path #:lambda 0.1))
    (check-equal? (vector-ref (list-ref steps 1) 0)
                  (vector-ref (glmnet-path-intercepts path) 1)))

  (test-case "a first-class sequence of two values"
    (define path (elnet-path Xg yg #:nlambda 5))
    (define seq (in-path path))
    (check-true (sequence? seq))
    (check-equal? (sequence-length seq) 5)
    (check-equal? (for/list ([(λ β) seq]) λ) (for/list ([(λ β) seq]) λ))
    (check-equal? (for/list ([(λ β) (in-path path)] [i (in-naturals)]) i) '(0 1 2 3 4))
    (define-values (lams betas)
      (for/lists (lams betas) ([(λ β) (in-path path)]) (values λ β)))
    (check-equal? lams (vector->list (glmnet-path-lambda path)))
    (check-equal? betas (coef path #:lambda lams)))

  (test-case "a path with no fitted λ is an empty sequence"
    (define empty-path (glmnet-path 'gaussian (vector) (vector) (vector) (vector) (vector) 0))
    (check-equal? (for/list ([(λ β) (in-path empty-path)]) λ) '()))

  (test-case "in-path takes a model, by contract"
    (check-exn #rx"in-path: contract violation\n  expected: glmnet-model\\?"
               (lambda () (in-path Xg)))))
