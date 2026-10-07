#lang racket/base

;; The docs idiom gate (AGENTS.md, Documentation). `racket docs-idiom-test.rkt`
;; prints every hit, then the counts in the form of `allowed-hits`.

(require racket/dict
         racket/file
         racket/list
         racket/match
         racket/path
         racket/pretty
         racket/runtime-path
         racket/string
         scribble/reader)

(define-runtime-path tests-dir ".")
(define collection-dir (simplify-path (build-path tests-dir "..")))

(struct idiom (name pattern fix))

;; Where two idioms match at one position, the first listed one is the hit.
(define idioms
  (list
   (idiom 'family-accessor
          #px"(?<![\\w-])(?:elnet|logistic|multinomial|cox|poisson|mgaussian)-result-(?!num-passes(?![\\w-]))[\\w-]+"
          "use coef, predict or deviance-ratio (only num-passes has no generic)")
   (idiom 'path-accessor
          #px"(?<![\\w-])glmnet-path-(?:coefficients|intercepts|dev-ratio)(?![\\w-])"
          "use in-path to walk the path, or coef and deviance-ratio at one λ")
   (idiom 'last-by-index
          #px"\\((?:vector-ref|list-ref)\\s+(?:[^()\\s]+|\\([^()]*\\))\\s+\\((?:sub1|-)\\s+\\((?:vector-)?length\\s"
          "take the last element with last, or for/last over in-vector")
   (idiom 'hand-rounding
          #px"(?<![\\w-])(?:round3|rounded)(?![\\w-])|\\(/\\s+\\(round\\s+\\(\\*\\s+10+(?![\\w.])"
          "show the value as returned, or format it with ~r")
   (idiom 'positional-access
          #px"\\((?:car|cadr|caddr|first|second|third|list-ref) |\\(cdr \\(assoc |\\(map (?:car|cdr|cadr|caddr)[ )]"
          "destructure with match-define, read a key with dict-ref, or iterate a sequence")
   (idiom 'hand-written-output
          #px"\\(code:comment \"\\s*=>"
          "evaluate it: an @examples block, or a chunk whose value the companion test checks")))

;; The hits each file, relative to the collection, may still have, by idiom.
;; A count must equal the file's hits exactly, so it only goes down; a file
;; not listed must have none.
(define allowed-hits
  '(("examples/04-logistic.rkt" (hand-written-output . 2))
    ("examples/05-multinomial.rkt" (hand-written-output . 2))
    ("examples/06-cox.rkt" (hand-written-output . 1))
    ("examples/07-poisson.rkt" (hand-written-output . 1))
    ("examples/08-mgaussian.rkt" (hand-written-output . 1))
    ("examples/13-data-sources.rkt" (positional-access . 4))
    ("scribblings/guide/concepts.scrbl"
     (family-accessor . 9) (path-accessor . 2) (hand-rounding . 9))
    ("scribblings/guide/data.scrbl" (family-accessor . 2) (positional-access . 1))
    ("scribblings/guide/examples/cox.scrbl"
     (family-accessor . 1) (path-accessor . 2) (hand-rounding . 6))
    ("scribblings/guide/examples/data-sources.scrbl" (positional-access . 3))
    ("scribblings/guide/examples/elastic-net.scrbl"
     (family-accessor . 5) (hand-rounding . 10) (positional-access . 5))
    ("scribblings/guide/examples/formula-polynomial.scrbl" (last-by-index . 1))
    ("scribblings/guide/examples/lasso.scrbl"
     (family-accessor . 3) (path-accessor . 2) (hand-rounding . 8) (positional-access . 2))
    ("scribblings/guide/examples/logistic.scrbl"
     (family-accessor . 2) (path-accessor . 2) (hand-rounding . 7))
    ("scribblings/guide/examples/mgaussian.scrbl"
     (family-accessor . 7) (hand-rounding . 8) (positional-access . 4))
    ("scribblings/guide/examples/multinomial.scrbl" (family-accessor . 2))
    ("scribblings/guide/examples/ols.scrbl" (family-accessor . 1))
    ("scribblings/guide/examples/poisson.scrbl" (family-accessor . 2))
    ("scribblings/guide/examples/quick-start.scrbl"
     (path-accessor . 1) (hand-rounding . 1) (positional-access . 1))
    ("scribblings/guide/examples/ridge.scrbl"
     (family-accessor . 2) (path-accessor . 2) (hand-rounding . 6))
    ("scribblings/guide/formulas.scrbl" (positional-access . 3))
    ("scribblings/guide/plots.scrbl" (positional-access . 5))))

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

;; The hits in `source`, the text of `file`, in order. A counting port reads
;; CRLF as one position, so the text is read with LF line ends.
(define (source-hits file source)
  (define text (regexp-replace* #rx"\r\n" source "\n"))
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
  (map cdr (remove-duplicates (sort matches < #:key car) = #:key car)))

(define (hit->string h)
  (format "~a:~a: ~a `~a`: ~a" (hit-file h) (hit-line h) (idiom-name (hit-idiom h))
          (hit-text h) (idiom-fix (hit-idiom h))))

(define (hits-of id hits)
  (filter (λ (h) (eq? (hit-idiom h) id)) hits))

;; `file`'s hits as an `allowed-hits` entry.
(define (hit-counts file hits)
  (cons file
        (for*/list ([id (in-list idioms)]
                    [n (in-value (length (hits-of id hits)))]
                    #:unless (zero? n))
          (cons (idiom-name id) n))))

;; How `hits`, all in `file`, differ from `allowed`, the file's counts.
(define (count-problems file hits allowed)
  (for*/list ([id (in-list idioms)]
              [found (in-value (hits-of id hits))]
              [n (in-value (length found))]
              [limit (in-value (dict-ref allowed (idiom-name id) 0))]
              #:unless (= n limit))
    (cond
      [(> n limit)
       (format "~a has ~a ~a hit(s) where ~a are allowed, and no file may gain one; they are:\n~a"
               file n (idiom-name id) limit
               (string-join (map hit->string found) "\n  " #:before-first "  "))]
      [(zero? n)
       (format "~a has no ~a hit left: remove (~a . ~a) from its allowed-hits entry"
               file (idiom-name id) (idiom-name id) limit)]
      [else
       (format "~a has ~a ~a hit(s) where ~a are allowed: lower the count to ~a in allowed-hits"
               file n (idiom-name id) limit n)])))

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
    (check-equal? (hit-texts "@examples[(glmnet-path-coefficients p)\n(glmnet-path-intercepts p) (glmnet-path-dev-ratio p)]")
                  '((1 path-accessor "glmnet-path-coefficients")
                    (2 path-accessor "glmnet-path-intercepts")
                    (2 path-accessor "glmnet-path-dev-ratio")))
    (check-equal? (hit-texts "@examples[(vector-ref ratios (sub1 (vector-length ratios)))
(list-ref xs (sub1 (length xs))) (vector-ref (coef m) (- (vector-length (coef m)) 1))]")
                  '((1 last-by-index "(vector-ref ratios (sub1 (vector-length ")
                    (2 last-by-index "(list-ref xs (sub1 (length ")
                    (2 last-by-index "(vector-ref (coef m) (- (vector-length ")))
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

  (test-case "CRLF line ends give the same hits"
    (check-equal? (hit-texts "@examples[\r\n(f x)\r\n(car row)]\r\n@examples[(second r)]")
                  (hit-texts "@examples[\n(f x)\n(car row)]\n@examples[(second r)]")))

  (test-case "prose and legitimate code do not hit"
    (check-equal? (hit-texts "rounded to one decimal place, (car x) and @racket[(round3 x)]
@racket[(glmnet-path-coefficients p)]")
                  '())
    (check-equal? (hit-texts "@examples[(elnet-result-num-passes fit) (elnet-result? fit)
(match-define (elnet-result a0 beta _ _ _) fit) (cdr pair) (vector-ref v 0)
(scar x) (map cars xs) (rounded-up 1) (first-rows p 3) (/ (round (* 0.5 x)) 2)
(code:comment \"the result => 3\")
(glmnet-path-lambda p) (glmnet-path-df p) (glmnet-path? p) (my-glmnet-path-dev-ratio p)
(/ ss (sub1 (length y))) (vector-ref v (sub1 k)) (for/last ([r (in-vector v)]) r) (last xs)]")
                  '()))

  (define (h name)
    (hit "f.scrbl" 1 (findf (λ (id) (eq? (idiom-name id) name)) idioms) "x"))

  (test-case "counts must match exactly and only go down"
    (define hits (list (h 'hand-rounding) (h 'hand-rounding) (h 'positional-access)))
    (define counts '((hand-rounding . 2) (positional-access . 1)))
    (check-equal? (hit-counts "f.scrbl" hits) (cons "f.scrbl" counts))
    (check-equal? (count-problems "f.scrbl" hits counts) '())
    (check-match (count-problems "f.scrbl" hits '((hand-rounding . 2)))
                 (list (regexp #rx"^f.scrbl has 1 positional-access hit\\(s\\) where 0 are allowed.*\n  f.scrbl:1: positional-access `x`: destructure")))
    (check-match (count-problems "f.scrbl" hits '((hand-rounding . 3) (positional-access . 1)))
                 (list (regexp #rx"lower the count to 2 in allowed-hits$")))
    (check-match (count-problems "f.scrbl" hits (cons '(family-accessor . 1) counts))
                 (list (regexp #rx"no family-accessor hit left: remove \\(family-accessor . 1\\)"))))

  (define files (checked-files))

  (test-case "the pages and lp2 programs are found"
    (check-not-false (member "scribblings/guide/concepts.scrbl" files))
    (check-not-false (member "examples/04-logistic.rkt" files))
    (check-false (member "scribblings/reference.scrbl" files))
    (check-false (member "examples/test/04-logistic.rkt" files)))

  (for ([entry (in-list allowed-hits)])
    (match-define (cons file counts) entry)
    (test-case (format "~a's allowed-hits entry" file)
      (check-not-false (member file files)
                       (format "~a has an allowed-hits entry but is not a checked file" file))
      (check-false (null? counts)
                   (format "~a has no hits allowed: remove its allowed-hits entry" file))
      (for ([count (in-list counts)])
        (match-define (cons name n) count)
        (check-not-false (findf (λ (id) (eq? (idiom-name id) name)) idioms)
                         (format "~a's allowed-hits entry names ~a, which is not an idiom" file name))
        (check-true (exact-positive-integer? n)
                    (format "~a's allowed-hits count for ~a is ~s, not a positive count" file name n)))))

  (for ([file (in-list files)])
    (define problems (count-problems file (file-hits file) (dict-ref allowed-hits file '())))
    (test-case (format "docs idioms: ~a" file)
      (with-check-info (['problems (string-info (string-join problems "\n"))])
        (check-true (null? problems))))))

(module+ main
  (define all-hits
    (for/list ([file (in-list (checked-files))])
      (cons file (file-hits file))))
  (for* ([file+hits (in-list all-hits)]
         [h (in-list (cdr file+hits))])
    (displayln (hit->string h)))
  (newline)
  (pretty-write (for/list ([file+hits (in-list all-hits)]
                           #:unless (null? (cdr file+hits)))
                  (hit-counts (car file+hits) (cdr file+hits)))))
