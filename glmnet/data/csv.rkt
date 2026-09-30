#lang racket/base

(require racket/contract
         racket/fixnum
         racket/flonum
         racket/list
         racket/port
         racket/string
         racket/vector
         "../data.rkt"
         (only-in (submod "../data.rkt" support) table-names select-table-values missing-error))

(define exists/c (or/c 'error 'replace 'truncate 'truncate/replace))

(provide
 (contract-out
  [csv->table (->* () (input-port?) table?)]
  [csv-file->table (-> path-string? table?)]
  [table->csv (->* (table?) (output-port?) void?)]
  [table->csv-file (->* (table? path-string?) (#:exists exists/c) void?)]))

;; --- reading -----------------------------------------------------------------

;; `where` is the extra fields of every error, such as the file's name.
(define (raise-input-error who where message . fields)
  (apply raise-arguments-error who message (append where fields)))

(define (input->bytes in who where)
  (define b (port->bytes in))
  (unless (bytes-utf-8-length b #f)
    (define offset (invalid-utf-8-offset b))
    (raise-input-error who where "the input is not UTF-8"
                       "line" (add1 (line-ends b 0 offset)) "byte offset" offset))
  b)

(define (invalid-utf-8-offset b)
  (define converter (bytes-open-converter "UTF-8" "UTF-8"))
  (define-values (_converted offset _status) (bytes-convert converter b))
  (bytes-close-converter converter)
  offset)

(define (line-ends b start end)
  (length (regexp-match-positions* #rx#"\r\n|\r|\n" b start end)))

(define comma (char->integer #\,))
(define dquote (char->integer #\"))
(define lf (char->integer #\newline))
(define cr (char->integer #\return))

;; C's isspace in the C locale: space, tab, LF, VT, FF and CR.
(define (space-byte? c)
  (or (fx= c 32) (and (fx<= 9 c) (fx<= c 13))))

;; The length of the line end at pos (LF, CR or CRLF), or 0.
(define (newline-length b n pos)
  (cond
    [(fx>= pos n) 0]
    [(fx= (bytes-ref b pos) lf) 1]
    [(fx= (bytes-ref b pos) cr)
     (if (and (fx< (fx+ pos 1) n) (fx= (bytes-ref b (fx+ pos 1)) lf)) 2 1)]
    [else 0]))

;; A field of a record: its bytes are content[start, end).
(struct field (content start end quoted?) #:mutable)

;; Scans the field that starts at pos, on line `line`, into f. Returns the
;; position after it and the line there.
(define (scan-field! f b n pos line who where)
  (cond
    [(and (fx< pos n) (fx= (bytes-ref b pos) dquote))
     (scan-quoted-field! f b n pos line who where)]
    [else
     (let loop ([end pos])
       (define c (if (fx< end n) (bytes-ref b end) comma))
       (cond
         [(or (fx= c comma) (fx= c lf) (fx= c cr))
          (set-field-content! f b)
          (set-field-start! f pos)
          (set-field-end! f end)
          (set-field-quoted?! f #f)
          (values end line)]
         [(fx= c dquote)
          (raise-input-error who where "a cell that is not quoted has a quote in it" "line" line)]
         [else (loop (fx+ end 1))]))]))

;; A quoted field's content is copied only when it has a doubled quote.
(define (scan-quoted-field! f b n pos start-line who where)
  (let loop ([p (fx+ pos 1)] [line start-line] [pieces '()] [piece-start (fx+ pos 1)])
    (cond
      [(fx>= p n)
       (raise-input-error who where "a quoted cell is not closed" "line" start-line)]
      [(fx= (bytes-ref b p) dquote)
       (cond
         [(and (fx< (fx+ p 1) n) (fx= (bytes-ref b (fx+ p 1)) dquote))
          (loop (fx+ p 2) line (cons (subbytes b piece-start (fx+ p 1)) pieces) (fx+ p 2))]
         [(or (fx= (fx+ p 1) n)
              (fx= (bytes-ref b (fx+ p 1)) comma)
              (fx> (newline-length b n (fx+ p 1)) 0))
          (cond
            [(null? pieces)
             (set-field-content! f b)
             (set-field-start! f piece-start)
             (set-field-end! f p)]
            [else
             (define content
               (apply bytes-append (reverse (cons (subbytes b piece-start p) pieces))))
             (set-field-content! f content)
             (set-field-start! f 0)
             (set-field-end! f (bytes-length content))])
          (set-field-quoted?! f #t)
          (values (fx+ p 1) line)]
         [else
          (raise-input-error who where "a quoted cell has text after its closing quote"
                             "line" line)])]
      [else
       (define nl (newline-length b n p))
       (if (fx> nl 0)
           (loop (fx+ p nl) (fx+ line 1) pieces piece-start)
           (loop (fx+ p 1) line pieces piece-start))])))

;; Scans the record that starts at pos into the fields of `fields`, growing
;; it as needed. Returns the fields, their number, the position of the next
;; record and its line.
(define (scan-record b n pos line fields who where)
  (let loop ([pos pos] [line line] [fields fields] [k 0])
    (define fields*
      (if (fx< k (vector-length fields))
          fields
          (let ([grown (make-vector (fx* 2 (vector-length fields)) #f)])
            (vector-copy! grown 0 fields)
            (for ([j (in-range (vector-length fields) (vector-length grown))])
              (vector-set! grown j (field #f 0 0 #f)))
            grown)))
    (define-values (after line*) (scan-field! (vector-ref fields* k) b n pos line who where))
    (cond
      [(and (fx< after n) (fx= (bytes-ref b after) comma))
       (loop (fx+ after 1) line* fields* (fx+ k 1))]
      [else
       (define nl (newline-length b n after))
       (values fields* (fx+ k 1) (fx+ after nl) (if (fx> nl 0) (fx+ line* 1) line*))])))

(define (make-fields n)
  (for/vector #:length n ([_ (in-range n)]) (field #f 0 0 #f)))

(define (field->string f)
  (string->immutable-string
   (bytes->string/utf-8 (field-content f) #f (field-start f) (field-end f))))

(define missing (string->uninterned-symbol "missing"))

;; The value of a cell, typed as R's type.convert types a column that holds
;; only this cell.
(define (field-value f)
  (define b (field-content f))
  (define s (field-start f))
  (define e (field-end f))
  (cond
    [(and (not (field-quoted? f)) (fx= (skip-spaces b s e) e)) missing]
    [(bytes-range=? b s e #"NA") missing]
    [(bytes->flonum b s e)]
    [(or (bytes-range=? b s e #"TRUE") (bytes-range=? b s e #"T")) #t]
    [(or (bytes-range=? b s e #"FALSE") (bytes-range=? b s e #"F")) #f]
    [else (field->string f)]))

(define (bytes-range=? b s e word)
  (and (fx= (fx- e s) (bytes-length word))
       (for/and ([k (in-range (bytes-length word))])
         (fx= (bytes-ref b (fx+ s k)) (bytes-ref word k)))))

(define (skip-spaces b p e)
  (if (and (fx< p e) (space-byte? (bytes-ref b p))) (skip-spaces b (fx+ p 1) e) p))

;; R's R_strtod accepts white space after a number as iswspace does in a
;; UTF-8 locale, which takes these characters besides the ASCII ones.
(define unicode-spaces
  (list->string (map integer->char '(#x1680 #x2000 #x2001 #x2002 #x2003 #x2004 #x2005 #x2006
                                     #x2008 #x2009 #x200A #x2028 #x2029 #x205F #x3000))))

(define (trailing-spaces? b p e)
  (define q (skip-spaces b p e))
  (or (fx= q e)
      (and (fx>= (bytes-ref b q) 128)
           (for/and ([c (in-string (bytes->string/utf-8 b #f q e))])
             (or (and (char<? c #\u80) (space-byte? (char->integer c)))
                 (for/or ([u (in-string unicode-spaces)]) (char=? c u)))))))

(define (digit-value c)
  (and (fx<= 48 c) (fx<= c 57) (fx- c 48)))

(define (hex-digit-value c)
  (cond
    [(and (fx<= 48 c) (fx<= c 57)) (fx- c 48)]
    [(and (fx<= 97 c) (fx<= c 102)) (fx- c 87)]
    [(and (fx<= 65 c) (fx<= c 70)) (fx- c 55)]
    [else #f]))

;; Whether the bytes at p spell `word`, which is lower case, in any case.
(define (word-ci? b p e word)
  (define m (bytes-length word))
  (and (fx<= (fx+ p m) e)
       (for/and ([k (in-range m)])
         (fx= (fxior (bytes-ref b (fx+ p k)) 32) (bytes-ref word k)))))

;; The number that b[s, e) spells as R_strtod5 reads it, correctly rounded,
;; or #f. After leading white space, text that starts with NA is not a
;; number, since R reads it both with and without NA as a number.
(define (bytes->flonum b s e)
  (define p0 (skip-spaces b s e))
  (cond
    [(and (fx<= (fx+ p0 2) e) (fx= (bytes-ref b p0) 78) (fx= (bytes-ref b (fx+ p0 1)) 65)) #f]
    [else
     (define c0 (if (fx< p0 e) (bytes-ref b p0) 0))
     (define negative? (fx= c0 45))
     (define p (if (or negative? (fx= c0 43)) (fx+ p0 1) p0))
     (define-values (magnitude end)
       (cond
         [(word-ci? b p e #"nan") (values +nan.0 (fx+ p 3))]
         [(word-ci? b p e #"infinity") (values +inf.0 (fx+ p 8))]
         [(word-ci? b p e #"inf") (values +inf.0 (fx+ p 3))]
         [(and (fx> (fx- e p) 2)
               (fx= (bytes-ref b p) 48)
               (fx= (fxior (bytes-ref b (fx+ p 1)) 32) 120))
          (hex-magnitude b (fx+ p 2) e)]
         [else (decimal-magnitude b p e)]))
     (and magnitude
          (trailing-spaces? b end e)
          (if negative? (fl* -1.0 magnitude) magnitude))]))

;; An exponent's optional sign and digits, which must be at least one; R
;; stops adding digits once the exponent passes 9999.
(define (scan-exponent b p e)
  (define c (if (fx< p e) (bytes-ref b p) 0))
  (define sign (if (fx= c 45) -1 1))
  (let loop ([q (if (or (fx= c 45) (fx= c 43)) (fx+ p 1) p)] [n 0] [ndigits 0])
    (define d (and (fx< q e) (digit-value (bytes-ref b q))))
    (cond
      [d (loop (fx+ q 1) (if (fx< n 9999) (fx+ (fx* n 10) d) n) (fx+ ndigits 1))]
      [(fx= ndigits 0) (values #f q)]
      [else (values (fx* sign n) q)])))

;; A hexadecimal number after its 0x: digits with an optional point, which
;; R lets appear more than once, the last one counting, and an optional
;; binary exponent.
(define (hex-magnitude b p e)
  (let loop ([p p] [mantissa 0] [point-bits -1])
    (define c (if (fx< p e) (bytes-ref b p) 0))
    (define d (hex-digit-value c))
    (cond
      [d (loop (fx+ p 1) (+ (* 16 mantissa) d) (if (fx>= point-bits 0) (fx+ point-bits 4) -1))]
      [(fx= c 46) (loop (fx+ p 1) mantissa 0)]
      [(fx= (fxior c 32) 112)
       (define-values (expn end) (scan-exponent b (fx+ p 1) e))
       (if expn
           (values (binary->flonum mantissa (fx- expn (fxmax point-bits 0))) end)
           (values #f p))]
      [else (values (binary->flonum mantissa (fx- 0 (fxmax point-bits 0))) p)])))

(define (decimal-magnitude b p e)
  (let loop ([p p] [mantissa 0] [ndigits 0] [expn 0] [point? #f])
    (define c (if (fx< p e) (bytes-ref b p) 0))
    (define d (digit-value c))
    (cond
      [d (loop (fx+ p 1) (+ (* 10 mantissa) d) (fx+ ndigits 1) (if point? (fx- expn 1) expn) point?)]
      [(and (fx= c 46) (not point?)) (loop (fx+ p 1) mantissa ndigits expn #t)]
      [(fx= ndigits 0) (values #f p)]
      [(fx= (fxior c 32) 101)
       (define-values (n end) (scan-exponent b (fx+ p 1) e))
       (if n (values (decimal->flonum mantissa (fx+ expn n)) end) (values #f p))]
      [else (values (decimal->flonum mantissa expn) p)])))

(define powers-of-ten (for/flvector #:length 23 ([k (in-range 23)]) (exact->inexact (expt 10 k))))

;; mantissa * 10^expn, correctly rounded: exactly in flonums when both
;; factors are exact flonums, and otherwise in exact arithmetic, unless the
;; result is certainly out of range.
(define (decimal->flonum mantissa expn)
  (cond
    [(eqv? mantissa 0) 0.0]
    [(and (fixnum? mantissa) (fx< mantissa 9007199254740992) (fx<= -22 expn) (fx<= expn 22))
     (if (fx< expn 0)
         (fl/ (fx->fl mantissa) (flvector-ref powers-of-ten (fx- 0 expn)))
         (fl* (fx->fl mantissa) (flvector-ref powers-of-ten expn)))]
    [(and (fx> expn 0) (> (+ (integer-length mantissa) -1 (* 3.32 expn)) 1030)) +inf.0]
    [(and (fx< expn 0) (< (+ (integer-length mantissa) (* 3.32 expn)) -1080)) 0.0]
    [(fx>= expn 0) (exact->inexact (* mantissa (expt 10 expn)))]
    [else (exact->inexact (/ mantissa (expt 10 (fx- 0 expn))))]))

(define (binary->flonum mantissa expn)
  (define bits (integer-length mantissa))
  (cond
    [(eqv? mantissa 0) 0.0]
    [(> (+ bits expn) 1030) +inf.0]
    [(< (+ bits expn) -1080) 0.0]
    [else (exact->inexact (* mantissa (expt 2 expn)))]))

(define (read-table in who where)
  (define b (input->bytes in who where))
  (define n (bytes-length b))
  (define start
    (if (and (fx>= n 3) (fx= (bytes-ref b 0) #xEF) (fx= (bytes-ref b 1) #xBB) (fx= (bytes-ref b 2) #xBF))
        3
        0))
  (when (fx= start n)
    (raise-input-error who where "the input has no header"))
  (define-values (header-fields ncols pos line) (scan-record b n start 1 (make-fields 8) who where))
  (define names (header-names header-fields ncols who where))
  (define-values (columns nrows) (scan-rows b n pos line names who where))
  (for/list ([name (in-list names)]
             [column (in-vector columns)])
    (cons name (if (fx= (vector-length column) nrows) column (vector-copy column 0 nrows)))))

(define (header-names fields ncols who where)
  (define names
    (for/list ([j (in-range ncols)])
      (define name (field->string (vector-ref fields j)))
      (when (string=? name "")
        (raise-input-error who where "the header has a column without a name" "column" j "line" 1))
      name))
  (define dup (check-duplicates names))
  (when dup
    (raise-input-error who where "the header names a column twice" "name" dup "line" 1))
  names)

;; The columns of the rows after the header, each a vector at least as long
;; as the number of rows, and the number of rows.
(define (scan-rows b n pos line names who where)
  (define ncols (length names))
  (define name-vector (list->vector names))
  (let loop ([pos pos] [line line] [row 0] [capacity 64]
             [columns (for/vector #:length ncols ([_ (in-range ncols)]) (make-vector 64 #f))]
             [fields (make-fields (fxmax ncols 1))])
    (cond
      [(fx>= pos n) (values columns row)]
      [else
       (define-values (fields* count next next-line) (scan-record b n pos line fields who where))
       (unless (fx= count ncols)
         (raise-input-error who where "a row does not have one cell per column of the header"
                            "row" row "cells" count "columns" ncols "line" line))
       (define capacity* (if (fx< row capacity) capacity (fx* 2 capacity)))
       (define columns*
         (if (fx= capacity* capacity)
             columns
             (for/vector #:length ncols ([column (in-vector columns)])
               (define grown (make-vector capacity* #f))
               (vector-copy! grown 0 column)
               grown)))
       (for ([j (in-range ncols)])
         (define v (field-value (vector-ref fields* j)))
         (when (eq? v missing)
           (missing-error who "the input" #:row row #:column (vector-ref name-vector j)
                          #:details (list* "line" line where)))
         (vector-set! (vector-ref columns* j) row v))
       (loop next next-line (fx+ row 1) capacity* columns* fields*)])))

(define (csv->table [in (current-input-port)])
  (read-table in 'csv->table '()))

(define (csv-file->table path)
  (call-with-input-file path
    (lambda (in) (read-table in 'csv-file->table (list "file" path)))))

;; --- writing -----------------------------------------------------------------

(define (quote-text s)
  (string-append "\"" (string-replace s "\"" "\"\"") "\""))

(define (string->cell s name i who)
  (define b (string->bytes/utf-8 s))
  (define read-back (field-value (field b 0 (bytes-length b) #t)))
  (cond
    [(eq? read-back missing)
     (raise-arguments-error who "the table has a string that would be read back as a missing value"
                            "column" name "row" i "element" s)]
    [(or (regexp-match? #rx"[\",\r\n]|^$|^[ \t\v\f]|[ \t\v\f]$" s)
         (not (string? read-back)))
     (quote-text s)]
    [else s]))

(define (name->cell name who)
  (when (string=? name "")
    (raise-arguments-error who "the table has a column without a name"))
  (if (regexp-match? #rx"[\",\r\n]|^[ \t]|[ \t]$|^﻿" name) (quote-text name) name))

(define (real->csv x)
  (define v (real->double-flonum x))
  (cond
    [(eqv? v +inf.0) "Inf"]
    [(eqv? v -inf.0) "-Inf"]
    [(eqv? v +nan.0) "NaN"]
    [else
     (define s (number->string v))
     (if (string-suffix? s ".0") (substring s 0 (- (string-length s) 2)) s)]))

(define (cell->csv v name i who)
  (cond
    [(real? v) (real->csv v)]
    [(string? v) (string->cell v name i who)]
    [(symbol? v) (string->cell (symbol->string v) name i who)]
    [(boolean? v) (if v "TRUE" "FALSE")]
    [else
     (raise-arguments-error
      who "the table has an element that is not a real, string, symbol or boolean"
      "column" name "row" i "element" v)]))

;; Every line is made before any is written, so that a table that cannot be
;; written leaves nothing behind, not even an empty file.
(define (table->lines t who)
  (define names (table-names t who))
  (define columns (select-table-values t names who #:rows-required? #f))
  (define header (for/list ([name (in-list names)]) (name->cell name who)))
  (define rows
    (for/list ([i (in-range (vector-length (car columns)))])
      (for/list ([column (in-list columns)]
                 [name (in-list names)])
        (cell->csv (vector-ref column i) name i who))))
  (for/list ([cells (in-list (cons header rows))])
    (string-join cells ",")))

(define (write-lines lines out)
  (for ([line (in-list lines)])
    (write-string line out)
    (newline out)))

(define (table->csv t [out (current-output-port)])
  (write-lines (table->lines t 'table->csv) out))

(define (table->csv-file t path #:exists [exists 'error])
  (define lines (table->lines t 'table->csv-file))
  (call-with-output-file path #:exists exists
    (lambda (out) (write-lines lines out))))
