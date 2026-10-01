#lang racket/base

;; R glmnet's example datasets and R's mtcars and iris (#61), `(require
;; glmnet/datasets)`: the CSV files under datasets/, which
;; scripts/export-datasets.R writes from the pinned R. A glmnet dataset is
;; read on its loader's first call, and the loader returns the positional
;; arguments of its family's fitter.

(require racket/contract
         racket/list
         racket/promise
         racket/runtime-path
         "data.rkt"
         "data/csv.rkt")

(provide
 mtcars
 iris
 (contract-out
  [quick-start-example (-> (values design-matrix? (listof flonum?)))]
  [binomial-example (-> (values design-matrix? (listof (or/c 0 1))))]
  [multinomial-example (-> (values design-matrix? (listof (or/c 0 1 2))))]
  [poisson-example (-> (values design-matrix? (listof exact-nonnegative-integer?)))]
  [cox-example (-> (values design-matrix? (listof (and/c flonum? positive?)) (listof (or/c 0 1))))]
  [multi-gaussian-example (-> (values design-matrix? design-matrix?))]
  [sparse-example (-> (values design-matrix? (listof flonum?)))]))

(define-runtime-path datasets-dir "datasets")

(define (read-dataset name)
  (csv-file->table (build-path datasets-dir (string-append name ".csv"))))

;; Every module that requires glmnet/datasets shares these two tables.
(define (read-shared-dataset name)
  (for/list ([column (in-list (read-dataset name))])
    (cons (car column) (vector->immutable-vector (cdr column)))))

(define mtcars (read-shared-dataset "mtcars"))
(define iris (read-shared-dataset "iris"))

;; The dataset's columns V1, V2, ... as a design matrix, and its other
;; columns, the response, as a table.
(define (split-dataset name)
  (define-values (x y)
    (partition (lambda (column) (regexp-match? #rx"^V[0-9]+$" (car column)))
               (read-dataset name)))
  (values (table->design-matrix x) y))

(define (column table name) (vector->list (cdr (assoc name table))))
(define (exact-integers xs) (map inexact->exact xs))

(define quick-start
  (delay (let-values ([(x y) (split-dataset "QuickStartExample")])
           (values x (column y "y")))))
(define (quick-start-example) (force quick-start))

(define binomial
  (delay (let-values ([(x y) (split-dataset "BinomialExample")])
           (values x (exact-integers (column y "y"))))))
(define (binomial-example) (force binomial))

;; R's classes 1, 2 and 3 are multinomial-fit's 0, 1 and 2.
(define multinomial
  (delay (let-values ([(x y) (split-dataset "MultinomialExample")])
           (values x (map sub1 (exact-integers (column y "y")))))))
(define (multinomial-example) (force multinomial))

(define poisson
  (delay (let-values ([(x y) (split-dataset "PoissonExample")])
           (values x (exact-integers (column y "y"))))))
(define (poisson-example) (force poisson))

(define cox
  (delay (let-values ([(x y) (split-dataset "CoxExample")])
           (values x (column y "time") (exact-integers (column y "status"))))))
(define (cox-example) (force cox))

(define multi-gaussian
  (delay (let-values ([(x y) (split-dataset "MultiGaussianExample")])
           (values x (table->design-matrix y)))))
(define (multi-gaussian-example) (force multi-gaussian))

(define sparse
  (delay (let-values ([(x y) (split-dataset "SparseExample")])
           (values x (column y "y")))))
(define (sparse-example) (force sparse))
