#lang racket/base

;; The docs idiom gate. Resyntax reads neither the guide's .scrbl pages nor
;; the lp2 programs, so this test checks the code they show their readers:
;; block code (examples, racketblock, chunk, ...), not @racket in prose. The
;; reference is not checked, since it documents the struct accessors. A file
;; on `allowlist` is not checked yet; every other file must have no hit, and
;; a listed file with no hit left fails until it is removed, so the list only
;; shrinks. `racket docs-idiom-test.rkt` prints every hit, listed files' too.

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/runtime-path
         racket/string
         scribble/reader)

(define-runtime-path tests-dir ".")
(define collection-dir (simplify-path (build-path tests-dir "..")))

(struct idiom (name pattern fix))

(define idioms
  (list
   (idiom 'family-accessor
          #px"(?<![\\w-])(?:elnet|logistic|multinomial|cox|poisson|mgaussian)-result-(?!num-passes(?![\\w-]))[\\w-]+"
          "use coef, predict or deviance-ratio (only num-passes has no generic)")
   (idiom 'hand-rounding
          #px"(?<![\\w-])(?:round3|rounded)(?![\\w-])|\\(/\\s+\\(round\\s+\\(\\*\\s+10+(?![\\w.])"
          "show the value as returned, or format it with ~r")
   (idiom 'positional-access
          #px"\\((?:car|cadr|caddr|first|second|third|list-ref) |\\(cdr \\(assoc |\\(map (?:car|cdr|cadr|caddr)[ )]"
          "destructure with match-define, read a key with dict-ref, or iterate a sequence")
   (idiom 'hand-written-output
          #px"\\(code:comment \"\\s*=>"
          "evaluate it: an @examples block, or a chunk whose value the companion test checks")))

;; Files, relative to the glmnet collection, not yet rewritten to the idioms.
;; A leg that rewrites one removes it here.
(define allowlist
  '("examples/04-logistic.rkt"
    "examples/05-multinomial.rkt"
    "examples/06-cox.rkt"
    "examples/07-poisson.rkt"
    "examples/08-mgaussian.rkt"
    "examples/13-data-sources.rkt"
    "scribblings/guide/concepts.scrbl"
    "scribblings/guide/data.scrbl"
    "scribblings/guide/examples/cox.scrbl"
    "scribblings/guide/examples/data-sources.scrbl"
    "scribblings/guide/examples/elastic-net.scrbl"
    "scribblings/guide/examples/lasso.scrbl"
    "scribblings/guide/examples/logistic.scrbl"
    "scribblings/guide/examples/mgaussian.scrbl"
    "scribblings/guide/examples/multinomial.scrbl"
    "scribblings/guide/examples/ols.scrbl"
    "scribblings/guide/examples/poisson.scrbl"
    "scribblings/guide/examples/quick-start.scrbl"
    "scribblings/guide/examples/ridge.scrbl"
    "scribblings/guide/formulas.scrbl"
    "scribblings/guide/plots.scrbl"))

;; Forms whose body is block code shown to the reader.
(define code-heads
  '(examples interaction interaction0 interaction-eval interaction-eval-show defexamples
    racketblock racketblock0 RACKETBLOCK RACKETBLOCK0 racketmod racketmod0
    racketinput racketinput0 racketresultblock racketresultblock0
    codeblock codeblock0 schemeblock schemeblock0 chunk CHUNK))

;; The [start, end) character offsets of the block code in an @-syntax source.
(define (code-spans in)
  (port-count-lines! in)
  (let walk ([stx (read-syntax-inside (object-name in) in)])
    (define forms (syntax->list stx))
    (match forms
      [#f '()]
      [(cons head _)
       #:when (and (identifier? head) (memq (syntax-e head) code-heads))
       (define start (sub1 (syntax-position stx)))
       (list (cons start (+ start (syntax-span stx))))]
      [_ (append-map walk forms)])))

(struct hit (file line idiom text))

;; The hits in `text`, the source of `file`, in order.
(define (source-hits file text)
  (define spans (code-spans (open-input-string text file)))
  (define (in-code? pos)
    (for/or ([span (in-list spans)])
      (and (<= (car span) pos) (< pos (cdr span)))))
  (define (line-of pos)
    (add1 (length (regexp-match-positions* #rx"\n" text 0 pos))))
  (define matches
    (for*/list ([id (in-list idioms)]
                [pos (in-list (regexp-match-positions* (idiom-pattern id) text))]
                #:when (in-code? (car pos)))
      (cons (car pos) (hit file (line-of (car pos)) id (substring text (car pos) (cdr pos))))))
  (map cdr (sort matches < #:key car)))

(define (hit->string h)
  (format "~a:~a: ~a `~a`: ~a" (hit-file h) (hit-line h) (idiom-name (hit-idiom h))
          (hit-text h) (idiom-fix (hit-idiom h))))

(define (lp2? path)
  (equal? (call-with-input-file path read-line) "#lang scribble/lp2"))

;; Every checked file, relative to the collection: the .scrbl files under
;; scribblings/ but the reference, and the lp2 programs in examples/.
(define (checked-files)
  (define (relative path) (path->string (find-relative-path collection-dir path)))
  (define pages
    (for/list ([path (in-directory (build-path collection-dir "scribblings"))]
               #:when (and (path-has-extension? path #".scrbl")
                           (not (equal? (file-name-from-path path) (string->path "reference.scrbl")))))
      (relative path)))
  (define programs
    (for/list ([path (in-list (directory-list (build-path collection-dir "examples") #:build? #t))]
               #:when (and (path-has-extension? path #".rkt") (lp2? path)))
      (relative path)))
  (sort (append pages programs) string<?))

(define (file-hits file)
  (source-hits file (file->string (build-path collection-dir file))))

(module+ test
  (require rackunit)

  (define (hit-texts text)
    (for/list ([h (in-list (source-hits "t.scrbl" text))])
      (list (hit-line h) (idiom-name (hit-idiom h)) (hit-text h))))

  (test-case "each idiom hits in block code"
    (check-equal? (hit-texts "@examples[\n(elnet-result-coefficients fit)\n(map logistic-result-intercept fits)]")
                  '((2 family-accessor "elnet-result-coefficients")
                    (3 family-accessor "logistic-result-intercept")))
    (check-equal? (hit-texts "@chunk[<a>\n(define (round3 x)\n  (/ (round (* 1000 x)) 1000))\n(map rounded v)]")
                  '((2 hand-rounding "round3")
                    (3 hand-rounding "(/ (round (* 1000")
                    (4 hand-rounding "rounded")))
    (check-equal? (hit-texts "@racketblock[(car row) (list-ref xs 2) (cdr (assoc \"a\" t)) (map cadr X) (second r)]")
                  '((1 positional-access "(car ")
                    (1 positional-access "(list-ref ")
                    (1 positional-access "(cdr (assoc ")
                    (1 positional-access "(map cadr ")
                    (1 positional-access "(second ")))
    (check-equal? (hit-texts "@racketblock[\n(f x)\n(code:comment \"=> 3\")]")
                  '((3 hand-written-output "(code:comment \"=>"))))

  (test-case "prose and legitimate code do not hit"
    (check-equal? (hit-texts "rounded to one decimal place, (car x) and @racket[(round3 x)]")
                  '())
    (check-equal? (hit-texts "@examples[(elnet-result-num-passes fit) (elnet-result? fit)
(match-define (elnet-result a0 beta _ _ _) fit) (cdr pair) (vector-ref v 0)
(scar x) (map cars xs) (rounded-up 1) (first-rows p 3) (/ (round (* 0.5 x)) 2)
(code:comment \"the result => 3\")]")
                  '()))

  (define files (checked-files))

  (test-case "the pages and lp2 programs are found"
    (check-not-false (member "scribblings/guide/concepts.scrbl" files))
    (check-not-false (member "examples/04-logistic.rkt" files))
    (check-false (member "scribblings/reference.scrbl" files))
    (check-false (member "examples/test/04-logistic.rkt" files)))

  (for ([file (in-list allowlist)])
    (test-case (format "~a is a checked file" file)
      (check-not-false (member file files)
                       (format "~a is on the allowlist in tests/docs-idiom-test.rkt but is not a checked file"
                               file))))

  (for ([file (in-list files)])
    (define hits (file-hits file))
    (test-case (format "docs idioms: ~a" file)
      (if (member file allowlist)
          (check-false (null? hits)
                       (format "~a has no idiom hits left: remove it from the allowlist in tests/docs-idiom-test.rkt"
                               file))
          (check-true (null? hits) (string-join (map hit->string hits) "\n"))))))

(module+ main
  (for* ([file (in-list (checked-files))]
         [h (in-list (file-hits file))])
    (displayln (hit->string h))))
