#lang racket/base

;; Runner + tests for the literate example ../00-hello-fortran.rkt.
;; The example itself is a #lang scribble/lp2 program (woven into the manual);
;; lp2 submodules can't see chunk-level bindings, so main/test live here and
;; require the example's provides.

(require "../00-hello-fortran.rkt")

(module+ main
  (define r (run-example))
  (printf "glmnet-hello(2.5, 4.0) = ~a\n" (car r))
  (printf "default real bytes     = ~a\n" (cadr r))
  (printf "capi abi version       = ~a\n" (caddr r)))

(module+ test
  (require rackunit)
  (define r (run-example))
  (check-= (car r) 6.5 1e-12 "by-value double round-trip through Fortran")
  (check-equal? (cadr r) 8 "library built with -fdefault-real-8")
  (check-equal? (caddr r) 1 "C-ABI shim version"))
