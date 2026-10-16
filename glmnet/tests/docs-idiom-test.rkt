#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/runtime-path
         racket/string
         scribble/reader)

(define-runtime-path tests-dir ".")
(define lp2-line "#lang scribble/lp2")
(define collection-dir (simplify-path (build-path tests-dir "..")))

(struct idiom (name pattern fix))

(define positional-names
  (string-append "car|c[ad]{2,4}r|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth"
                 "|list-ref|list-tail|vector\\*?-ref|matrix-ref|array-ref|sequence-ref"))

(define positional-access
  (idiom 'positional-access
         (pregexp (format "\\((?:~a)[\\s(]|\\(cdr\\s*\\(ass(?:oc|q|v|f)\\s|\\((?:map|curryr?)\\s+(?:cdr|~a)[\\s)]"
                          positional-names positional-names))
         "destructure with match-define, read a key with dict-ref, or iterate a sequence"))

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
          #px"(?<![\\w-])(?:round3|rounded)(?![\\w-])|\\(/\\s+\\(round\\s+\\(\\*\\s+(?:10+(?![\\w.])|(?:[^()\\s]+|\\([^()]*\\))\\s+10+(?![\\w.]))"
          "show the value as returned, or format it with ~r")
   positional-access
   (idiom 'hand-written-output
          #px"\\(code:comment\\s+\"\\s*=>"
          "evaluate it: an @examples block, or a chunk whose value the companion test checks")
   (idiom 'page-helper
          #px"\\(define\\s+\\(?(?:fmt|coef-line|~r\\d+)(?=[\\s()])"
          "use the evaluator's fmt and coef-line, defined once in scribblings/utils.rkt")))

(define code-heads
  '(examples interaction interaction0 interaction-eval interaction-eval-show defexamples
    racketblock racketblock0 RACKETBLOCK RACKETBLOCK0 racketmod racketmod0
    racketinput racketinput0 racketresultblock racketresultblock0
    codeblock codeblock0 schemeblock schemeblock0 chunk CHUNK))

(define (code-spans in)
  (port-count-lines! in)
  (define file (object-name in))
  (define source
    (cond
      [(lp2-source? in) (read-syntax-inside file in)]
      [(path-has-extension? file #".rkt")
       (parameterize ([read-accept-reader #t] [read-accept-lang #t])
         (read-syntax file in))]
      [else (read-syntax-inside file in)]))
  (let walk ([stx source])
    (define forms (syntax->list stx))
    (match forms
      [#f '()]
      [(cons head _)
       #:when (and (identifier? head) (memq (syntax-e head) code-heads))
       (define start (sub1 (syntax-position stx)))
       (list (cons start (+ start (syntax-span stx))))]
      [_ (append-map walk forms)])))

(define (lp2-source? in)
  (equal? (peek-string (string-length lp2-line) 0 in) lp2-line))

(struct hit (file line idiom text))

;; A counting port reads CRLF as one position, so the text is read with LF
;; line ends.
(define (source-hits file source [checked idioms] [spans-of code-spans])
  (define text (regexp-replace* #rx"\r\n" source "\n"))
  (define spans (spans-of (open-input-string text file)))
  (define (in-code? pos)
    (for/or ([span (in-list spans)])
      (and (<= (car span) pos) (< pos (cdr span)))))
  (define (line-of pos)
    (add1 (length (regexp-match-positions* #rx"\n" text 0 pos))))
  (define matches
    (for*/list ([id (in-list checked)]
                [pos (in-list (regexp-match-positions* (idiom-pattern id) text))]
                #:when (in-code? (car pos)))
      (cons (car pos) (hit file (line-of (car pos)) id (substring text (car pos) (cdr pos))))))
  (map cdr (remove-duplicates (sort matches < #:key car) = #:key car)))

(define (whole-source in)
  (list (cons 0 +inf.0)))

(define (test-source-hits file source)
  (source-hits file source (list positional-access) whole-source))

(define (hit->string h)
  (format "~a:~a: ~a `~a`: ~a" (hit-file h) (hit-line h) (idiom-name (hit-idiom h))
          (hit-text h) (idiom-fix (hit-idiom h))))

(define (lp2? path)
  (call-with-input-file path lp2-source?))

(define (checked-files)
  (define (relative path) (path->string (find-relative-path collection-dir path)))
  (define pages
    (for/list ([path (in-directory (build-path collection-dir "scribblings"))]
               #:when (or (path-has-extension? path #".rkt")
                          (and (path-has-extension? path #".scrbl")
                               (not (equal? (file-name-from-path path)
                                            (string->path "reference.scrbl"))))))
      (relative path)))
  (define programs
    (for/list ([path (in-list (directory-list (build-path collection-dir "examples") #:build? #t))]
               #:when (and (path-has-extension? path #".rkt") (lp2? path)))
      (relative path)))
  (define tests
    (for/list ([path (in-list (directory-list (build-path collection-dir "examples" "test")
                                              #:build? #t))]
               #:when (path-has-extension? path #".rkt"))
      (relative path)))
  (sort (append pages programs tests) string<?))

(define (file-hits file)
  (define source (file->string (build-path collection-dir file)))
  (cond
    [(string-prefix? file "examples/test/") (test-source-hits file source)]
    [else (source-hits file source)]))

(struct binding (name rhs line))

(define (block-bindings file source heads)
  (define in (open-input-string source file))
  (port-count-lines! in)
  (let walk ([stx (read-syntax-inside file in)])
    (match (syntax->list stx)
      [#f '()]
      [(cons head body)
       #:when (and (identifier? head) (memq (syntax-e head) heads))
       (filter-map define->binding body)]
      [forms (append-map walk forms)])))

(define (define->binding stx)
  (match (syntax->datum stx)
    [(list 'define (? symbol? name) rhs) (binding name rhs (syntax-line stx))]
    [(list 'define (cons (? symbol? name) formals) body ...)
     (binding name `(λ ,formals ,@body) (syntax-line stx))]
    [(list (or 'match-define 'define-values) pattern rhs) (binding pattern rhs (syntax-line stx))]
    [_ #f]))

(define (renamings page-file page program-file program)
  (for*/list ([shown (in-list (block-bindings page-file page '(examples)))]
              [defined (in-list (block-bindings program-file program '(chunk CHUNK)))]
              #:when (and (equal? (binding-rhs shown) (binding-rhs defined))
                          (not (equal? (binding-name shown) (binding-name defined)))))
    (format "~a:~a: `~a` is ~a's `~a`; use the program's name" page-file (binding-line shown)
            (binding-name shown) program-file (binding-name defined))))

(define (example-page? file)
  (regexp-match? #rx"^scribblings/guide/examples/[^/]+[.]scrbl$" file))

(define (source-program page)
  (match (regexp-match #rx"@margin-note{Source: @filepath{glmnet/([^}]+)}}" page)
    [(list _ program-file) program-file]
    [#f #f]))

(define (page-renamings page-file)
  (define page (file->string (build-path collection-dir page-file)))
  (match (source-program page)
    [#f '()]
    [program-file
     (renamings page-file page program-file
                (file->string (build-path collection-dir program-file)))]))

(module+ test
  (require rackunit)

  (define (hit-texts text [file "t.scrbl"])
    (for/list ([h (in-list (source-hits file text))])
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
    (check-equal? (hit-texts "@racketblock[(car row) (list-ref xs 2) (cdr (assoc \"a\" t)) (map cadr X) (second r)
(vector-ref v 0) (vector-ref β (sub1 k))]")
                  '((1 positional-access "(car ")
                    (1 positional-access "(list-ref ")
                    (1 positional-access "(cdr (assoc ")
                    (1 positional-access "(map cadr ")
                    (1 positional-access "(second ")
                    (2 positional-access "(vector-ref ")
                    (2 positional-access "(vector-ref ")))
    (check-equal? (hit-texts "@racketblock[\n(f x)\n(code:comment \"=> 3\")]")
                  '((3 hand-written-output "(code:comment \"=>")))
    (check-equal? (hit-texts "@racketblock[(first\n xs) (cdr\t(assoc \"a\" t)) (map\n cadr X)
(code:comment\n \"=> 3\") (/ (round (* x 1000)) 1000) (/ (round (* (f x) 100)) 100)]")
                  '((1 positional-access "(first\n")
                    (2 positional-access "(cdr\t(assoc ")
                    (2 positional-access "(map\n cadr ")
                    (4 hand-written-output "(code:comment\n \"=>")
                    (5 hand-rounding "(/ (round (* x 1000")
                    (5 hand-rounding "(/ (round (* (f x) 100")))
    (check-equal? (hit-texts "@examples[(define (~r3 x) (~r x #:precision '(= 3)))
(define (coef-line coefs)\n  coefs)\n(define (fmt x [digits 3]) x) (define ~r5 values)]")
                  '((1 page-helper "(define (~r3")
                    (2 page-helper "(define (coef-line")
                    (4 page-helper "(define (fmt")
                    (4 page-helper "(define ~r5"))))

  (test-case "positional access hits in its every form"
    (check-equal? (map (compose1 idiom-name hit-idiom) (source-hits "t.scrbl" "@examples[(vector*-ref v 0) (fourth r) (map first rows) (list-tail xs 2) (cdr (assq 'a t))]"))
                  (make-list 5 'positional-access))
    (check-equal? (map hit-text (source-hits "t.scrbl" "@examples[(cddr xs) (caar xs) (cdar xs) (seventh xs)
(matrix-ref M 0 1) (array-ref A #(0)) (sequence-ref s 2) (map vector-ref vs is)
(curryr vector-ref 1) (map second rows) (cdr (assv 1 t))]"))
                  '("(cddr " "(caar " "(cdar " "(seventh " "(matrix-ref " "(array-ref "
                    "(sequence-ref " "(map vector-ref " "(curryr vector-ref " "(map second "
                    "(cdr (assv "))
    (check-equal? (map hit-text (source-hits "t.scrbl" "@examples[(car(f x)) (cdr(assoc k t)) (second(g y))]"))
                  '("(car(" "(cdr(assoc " "(second(")))

  (test-case "a test program is scanned whole, for positional access only"
    (check-equal? (map hit-text (test-source-hits "t.rkt" "#lang racket/base
(module+ test
  (check-equal? (vector-ref (glmnet-path-df fit) 0) 0)
  (glmnet-path-coefficients p))"))
                  '("(vector-ref ")))

  (test-case "CRLF line ends give the same hits"
    (check-equal? (hit-texts "@examples[\r\n(f x)\r\n(car row)]\r\n@examples[(second r)]")
                  (hit-texts "@examples[\n(f x)\n(car row)]\n@examples[(second r)]")))

  (test-case "prose and legitimate code do not hit"
    (check-equal? (hit-texts "rounded to one decimal place, (car x) and @racket[(round3 x)]
@racket[(glmnet-path-coefficients p)]")
                  '())
    (check-equal? (hit-texts "@examples[(elnet-result-num-passes fit) (elnet-result? fit)
(match-define (elnet-result a0 beta _ _ _) fit) (cdr pair) (vector-take v 3)
(scar x) (map cars xs) (rounded-up 1) (first-rows p 3) (/ (round (* 0.5 x)) 2)
(code:comment \"the result => 3\")
(glmnet-path-lambda p) (glmnet-path-df p) (glmnet-path? p) (my-glmnet-path-dev-ratio p)
(/ ss (sub1 (length y))) (vector-length v) (for/last ([r (in-vector v)]) r) (last xs)
(fmt x 4) (coef-line β #:intercept? #f) (define (fmt-row r) r) (define (show-coefs cs) cs)]")
                  '()))

  (test-case "a helper module's example templates are block code"
    (check-equal? (hit-texts "#lang racket/base
(define (f row) (car row))
(define-syntax-parse-rule (fixture ev)
  (examples #:eval ev
   (define X (car row))))"
                             "u.rkt")
                  '((5 positional-access "(car ")))
    (define planted
      (string-replace (file->string (build-path collection-dir "scribblings/utils.rkt"))
                      "(random-seed 3)" "(random-seed 3) (car row)"))
    (check-equal? (map hit-text (source-hits "scribblings/utils.rkt" planted))
                  '("(car ")))

  (test-case "a hit names its file, line, idiom and fix"
    (check-match (map hit->string (source-hits "f.scrbl" "@examples[\n(car row)]"))
                 (list (regexp #rx"^f.scrbl:2: positional-access `\\(car `: destructure"))))

  (define files (checked-files))

  (test-case "the pages and lp2 programs are found"
    (check-not-false (member "scribblings/guide/concepts.scrbl" files))
    (check-not-false (member "examples/04-logistic.rkt" files))
    (check-not-false (member "scribblings/utils.rkt" files))
    (check-not-false (member "scribblings/guide/examples/ols.scrbl" files))
    (check-false (member "scribblings/reference.scrbl" files))
    (check-not-false (member "examples/test/04-logistic.rkt" files)))

  (test-case "a page binding a program's value under another name hits"
    (check-equal? (renamings "p.scrbl" "#lang scribble/manual
@examples[#:eval ev
(define coefs-min (coef cvfit #:lambda 'lambda-min))
(define fit (f x))
(define (g x) (h x))
(define nonzero (n fit))]"
                             "examples/p.rkt" "#lang scribble/lp2
@chunk[<a>
(define coefficients-min (coef cvfit #:lambda 'lambda-min))
(define fit (f x))
(define (k x) (h x))]")
                  '("p.scrbl:3: `coefs-min` is examples/p.rkt's `coefficients-min`; use the program's name"
                    "p.scrbl:5: `g` is examples/p.rkt's `k`; use the program's name")))

  (test-case "a page destructuring a program's value into other names hits"
    (check-equal? (renamings "p.scrbl" "#lang scribble/manual
@examples[#:eval ev
(match-define (list a b) (split x))
(define-values (q r) (quotient/remainder n 3))
(match-define (vector _ β ...) (coef fit))]"
                             "examples/p.rkt" "#lang scribble/lp2
@chunk[<a>
(match-define (list lo hi) (split x))
(define-values (quo rem) (quotient/remainder n 3))
(match-define (vector _ β ...) (coef fit))]")
                  '("p.scrbl:3: `(list a b)` is examples/p.rkt's `(list lo hi)`; use the program's name"
                    "p.scrbl:4: `(q r)` is examples/p.rkt's `(quo rem)`; use the program's name")))

  (test-case "an example page's source note names its program"
    (check-equal? (source-program "@margin-note{Source: @filepath{glmnet/examples/00-ols.rkt}}")
                  "examples/00-ols.rkt")
    (check-false (source-program "@margin-note{Source: glmnet/examples/00-ols.rkt}"))
    (check-true (example-page? "scribblings/guide/examples/ols.scrbl"))
    (check-false (example-page? "scribblings/guide/concepts.scrbl")))

  (for ([file (in-list files)]
        #:when (example-page? file))
    (test-case (format "source note: ~a" file)
      (check-not-false (source-program (file->string (build-path collection-dir file))))))

  (for ([file (in-list files)])
    (test-case (format "shared names: ~a" file)
      (check-equal? (page-renamings file) '())))

  (for ([file (in-list files)])
    (define hits (file-hits file))
    (test-case (format "docs idioms: ~a" file)
      (with-check-info (['hits (string-info (string-join (map hit->string hits) "\n"))])
        (check-true (null? hits))))))

(module+ main
  (for ([file (in-list (checked-files))])
    (for-each (compose1 displayln hit->string) (file-hits file))
    (for-each displayln (page-renamings file))))
