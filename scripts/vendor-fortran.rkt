#lang racket/base

;; Regenerate fortran/vendor/ from R glmnet's Fortran on the CRAN GitHub mirror
;; (https://github.com/cran/glmnet). Upgrading upstream means changing a commit
;; below and rerunning:
;;
;;   racket scripts/vendor-fortran.rkt
;;
;; glmnet5dpclean.f comes from the last release with every family in Fortran;
;; coxnet5dpclean.f from the release R's own Cox fits run. The Cox file carries
;; its own copies of the routines it shares with glmnet5dpclean.f, so those
;; program units are dropped from glmnet5dpclean.f and the Cox file is kept
;; byte-for-byte. See fortran/vendor/NOTICE.md.

(require racket/list
         racket/port
         racket/runtime-path
         racket/string
         net/url)

(define glmnet-commit "537e1a9ea45f0c98ff64e65d818cf250b4b2851b") ; tag 4.1
(define coxnet-commit "2b85f4d60b7629929ebd8f9c87b2179b721e50b5") ; tag 4.1-10

(define-runtime-path vendor-dir "../fortran/vendor")

(define (fetch commit path)
  (define url
    (string->url
     (format "https://raw.githubusercontent.com/cran/glmnet/~a/~a" commit path)))
  (call/input-url url get-pure-port port->string))

(define (comment-line? line)
  (or (string=? line "")
      (memv (string-ref line 0) '(#\c #\C #\* #\!))))

(define (statement line)
  (substring line 0 (min 72 (string-length line))))

(define unit-start
  #px"^\\s+(?i:subroutine|(?:[a-z*0-9 ]+\\s)?function)\\s+(\\w+)")

(define (unit-end? line)
  (and (not (comment-line? line))
       (regexp-match? #px"^\\s*(?i:end)\\s*$" (statement line))))

;; (listof (list name start end)), line indices, END line inclusive.
(define (program-units lines)
  (define v (list->vector lines))
  (let loop ([i 0] [acc '()])
    (cond
      [(>= i (vector-length v)) (reverse acc)]
      [(and (not (comment-line? (vector-ref v i)))
            (regexp-match unit-start (statement (vector-ref v i))))
       => (lambda (m)
            (define end
              (for/first ([j (in-range i (vector-length v))]
                          #:when (unit-end? (vector-ref v j)))
                j))
            (loop (add1 end) (cons (list (string-downcase (cadr m)) i end) acc)))]
      [else (loop (add1 i) acc)])))

(define (drop-units lines names)
  (define v (list->vector lines))
  (define dropped
    (for*/list ([u (in-list (program-units lines))]
                #:when (member (car u) names)
                [i (in-range (cadr u) (add1 (caddr u)))])
      i))
  (define drop (for/hasheqv ([i (in-list dropped)]) (values i #t)))
  (for/list ([line (in-vector v)]
             [i (in-naturals)]
             #:unless (hash-ref drop i #f))
    line))

(define (header dropped)
  (append
   (list "c"
         "c     Vendored for the glmnet Racket bindings by"
         "c     scripts/vendor-fortran.rkt from R glmnet's src/glmnet5dpclean.f"
         (format "c     at cran/glmnet ~a (tag 4.1)." (substring glmnet-commit 0 10))
         "c     Changed from upstream: these program units were removed because"
         "c     coxnet5dpclean.f (R glmnet 4.1-10) defines them too:")
   (for/list ([names (in-slice 6 dropped)])
     (string-append "c       " (string-join names " ")))
   (list "c")))

(define (in-slice n xs)
  (let loop ([xs xs] [acc '()])
    (if (null? xs)
        (reverse acc)
        (let-values ([(a b) (split-at xs (min n (length xs)))])
          (loop b (cons a acc))))))

(module+ main
  (define glmnet-src (fetch glmnet-commit "src/glmnet5dpclean.f"))
  (define coxnet-src (fetch coxnet-commit "src/coxnet5dpclean.f"))
  (define cox-names (map car (program-units (string-split coxnet-src "\n" #:trim? #f))))
  (define glmnet-lines (string-split glmnet-src "\n" #:trim? #f))
  (define dropped
    (sort (for/list ([u (in-list (program-units glmnet-lines))]
                     #:when (member (car u) cox-names))
            (car u))
          string<?))
  (define trimmed (append (header dropped) (drop-units glmnet-lines cox-names)))
  (with-output-to-file (build-path vendor-dir "glmnet5dpclean.f") #:exists 'truncate
    (lambda () (void (write-string (string-join trimmed "\n")))))
  (with-output-to-file (build-path vendor-dir "coxnet5dpclean.f") #:exists 'truncate
    (lambda () (void (write-string coxnet-src))))
  (printf "glmnet5dpclean.f: dropped ~a units: ~a\n" (length dropped) (string-join dropped " "))
  (printf "coxnet5dpclean.f: ~a units, copied unchanged\n" (length cox-names)))
