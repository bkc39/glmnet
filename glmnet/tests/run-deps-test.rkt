#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/runtime-path
         syntax/modcode)

(define-runtime-path collection-dir "..")

(define build-only-submodules '(test doc srcdoc))

(define (collection-modules)
  (sort (find-files (λ (path)
                      (and (regexp-match? #rx"[.]rkt$" path)
                           (not (member "compiled" (map path->string (explode-path path))))))
                    (simplify-path collection-dir))
        path<?))

(define (build-only? code)
  (match (module-compiled-name code)
    [(list _ ... name) (memq name build-only-submodules)]
    [_ #f]))

(define (run-modules code)
  (cond
    [(build-only? code) '()]
    [else
     (cons code (append-map run-modules
                            (append (module-compiled-submodules code #t)
                                    (module-compiled-submodules code #f))))]))

(define (rackunit-path? module-path)
  (match module-path
    [(? symbol?) (regexp-match? #rx"^rackunit(/|$)" (symbol->string module-path))]
    [(list 'lib (? string? name) _ ...) (regexp-match? #rx"^rackunit(/|$)" name)]
    [_ #f]))

(define (import-path mpi)
  (define-values (module-path _) (module-path-index-split mpi))
  module-path)

(define (rackunit-imports code)
  (for*/list ([phase+imports (in-list (module-compiled-imports code))]
              [module-path (in-list (map import-path (cdr phase+imports)))]
              #:when (rackunit-path? module-path))
    (list (module-compiled-name code) module-path)))

(define (run-time-rackunit-imports path)
  (parameterize ([current-namespace (make-base-namespace)])
    (append-map rackunit-imports (run-modules (get-module-code path)))))

(module+ test
  (require rackunit)

  (for ([path (in-list (collection-modules))])
    (check-equal? (run-time-rackunit-imports path) '()
                  (format "~a requires rackunit outside a test submodule" path))))
