#lang racket/base

;; CSV files to and from tables (#61), `(require glmnet/data/csv)`: RFC 4180
;; records, each cell typed on its own by the rules R's read.csv applies to a
;; column. It needs neither the native library nor anything beyond `base`.

(require racket/contract
         racket/list
         racket/port
         racket/string
         "../data.rkt"
         (only-in (submod "../data.rkt" support) table-names select-table-values))

(define exists/c (or/c 'error 'append 'update 'replace 'truncate 'truncate/replace))

(provide
 (contract-out
  [csv->table (->* () (input-port?) table?)]
  [csv-file->table (-> path-string? table?)]
  [table->csv (->* (table?) (output-port?) void?)]
  [table->csv-file (->* (table? path-string?) (#:exists exists/c) void?)]))

;; --- reading -----------------------------------------------------------------

;; A cell's text and whether it was quoted; a record's cells and the line it
;; starts on.
(struct field (text quoted?))
(struct record (fields line))

;; `where` is the extra fields of every error, such as the file's name.
(define (parse-error who where message line)
  (apply raise-arguments-error who message (append where (list "line" line))))

;; The records of `text`, RFC 4180 with LF, CR or CRLF ending a record. A
;; newline after the last record does not start another.
(define (parse-records text who where)
  (define n (string-length text))
  (define start (if (and (positive? n) (char=? (string-ref text 0) #\uFEFF)) 1 0))
  (define (newline-length pos)
    (cond
      [(>= pos n) 0]
      [(char=? (string-ref text pos) #\newline) 1]
      [(char=? (string-ref text pos) #\return)
       (if (and (< (add1 pos) n) (char=? (string-ref text (add1 pos)) #\newline)) 2 1)]
      [else 0]))
  ;; Returns the field, the position after it and the line there.
  (define (quoted-field pos start-line)
    (define out (open-output-string))
    (let loop ([pos (add1 pos)] [line start-line])
      (cond
        [(>= pos n)
         (parse-error who where "a quoted cell is not closed" start-line)]
        [(char=? (string-ref text pos) #\")
         (cond
           [(and (< (add1 pos) n) (char=? (string-ref text (add1 pos)) #\"))
            (write-char #\" out)
            (loop (+ pos 2) line)]
           [(or (= (add1 pos) n)
                (char=? (string-ref text (add1 pos)) #\,)
                (positive? (newline-length (add1 pos))))
            (values (field (get-output-string out) #t) (add1 pos) line)]
           [else
            (parse-error who where "a quoted cell has text after its closing quote" line)])]
        [else
         (define nl (newline-length pos))
         (cond
           [(positive? nl)
            (write-string text out pos (+ pos nl))
            (loop (+ pos nl) (add1 line))]
           [else
            (write-char (string-ref text pos) out)
            (loop (add1 pos) line)])])))
  (define (unquoted-field pos line)
    (let loop ([end pos])
      (cond
        [(or (>= end n)
             (char=? (string-ref text end) #\,)
             (positive? (newline-length end)))
         (values (field (substring text pos end) #f) end line)]
        [(char=? (string-ref text end) #\")
         (parse-error who where "a cell that is not quoted has a quote in it" line)]
        [else (loop (add1 end))])))
  (let records ([pos start] [line 1] [acc '()])
    (cond
      [(>= pos n) (reverse acc)]
      [else
       (let fields ([pos pos] [line line] [cells '()])
         (define-values (cell after line*)
           (if (and (< pos n) (char=? (string-ref text pos) #\"))
               (quoted-field pos line)
               (unquoted-field pos line)))
         (define cells* (cons cell cells))
         (cond
           [(and (< after n) (char=? (string-ref text after) #\,))
            (fields (add1 after) line* cells*)]
           [else
            (define done (record (reverse cells*) line))
            (records (+ after (newline-length after)) (add1 line*) (cons done acc))]))])))

(define number-rx
  #px"^[[:blank:]]*[+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:[eE][+-]?[0-9]+)?[[:blank:]]*$")

(define missing (string->uninterned-symbol "missing"))

;; The value of a cell's text: `missing` when it is blank and not quoted, or
;; R's NA; a flonum when it is a decimal number or one of R's Inf, -Inf and
;; NaN; a boolean for R's TRUE and FALSE; and otherwise the text itself.
;; Blanks around a number, NA, TRUE or FALSE are ignored, as R ignores them.
(define (text->value text quoted?)
  (define trimmed (string-trim text #px"[[:blank:]]+"))
  (cond
    [(and (string=? trimmed "") (not quoted?)) missing]
    [(string=? trimmed "NA") missing]
    [(regexp-match? number-rx text)
     (define x
       (real->double-flonum (string->number trimmed 10 'number-or-false 'decimal-as-inexact)))
     (if (and (zero? x) (char=? (string-ref trimmed 0) #\-)) -0.0 x)]
    [(member trimmed '("Inf" "+Inf")) +inf.0]
    [(string=? trimmed "-Inf") -inf.0]
    [(string=? trimmed "NaN") +nan.0]
    [(string=? trimmed "TRUE") #t]
    [(string=? trimmed "FALSE") #f]
    [else (string->immutable-string text)]))

(define (header-names rec who where)
  (define names
    (for/list ([cell (in-list (record-fields rec))]
               [j (in-naturals)])
      (when (string=? (field-text cell) "")
        (apply raise-arguments-error who "the header has a column without a name"
               (append where (list "column" j "line" (record-line rec)))))
      (string->immutable-string (field-text cell))))
  (define dup (check-duplicates names))
  (when dup
    (apply raise-arguments-error who "the header names a column twice"
           (append where (list "name" dup "line" (record-line rec)))))
  names)

(define (read-table in who where)
  (define records (parse-records (port->string in) who where))
  (when (null? records)
    (apply raise-arguments-error who "the input has no header" where))
  (define names (header-names (car records) who where))
  (define ncols (length names))
  (define rows
    (for/list ([rec (in-list (cdr records))]
               [i (in-naturals)])
      (define cells (record-fields rec))
      (unless (= (length cells) ncols)
        (apply raise-arguments-error who "a row does not have one cell per column of the header"
               (append where (list "row" i "cells" (length cells) "columns" ncols
                                   "line" (record-line rec)))))
      (for/vector #:length ncols ([cell (in-list cells)]
                                  [name (in-list names)])
        (define v (text->value (field-text cell) (field-quoted? cell)))
        (when (eq? v missing)
          (apply raise-arguments-error who "a cell is missing"
                 (append where (list "column" name "row" i "line" (record-line rec)))))
        v)))
  (for/list ([name (in-list names)]
             [j (in-naturals)])
    (cons name (for/list ([row (in-list rows)]) (vector-ref row j)))))

(define (csv->table [in (current-input-port)])
  (read-table in 'csv->table '()))

(define (csv-file->table path)
  (call-with-input-file path
    (lambda (in) (read-table in 'csv-file->table (list "file" path)))))

;; --- writing -----------------------------------------------------------------

(define (quote-text s)
  (string-append "\"" (string-replace s "\"" "\"\"") "\""))

;; A string as a cell, quoted when RFC 4180 needs it or when it reads as
;; another value, as R's write.csv quotes every string; csv->table still
;; reads that value, as R's read.csv does. A string that reads as a missing
;; cell cannot be written.
(define (string->cell s name i who)
  (define read-back (text->value s #t))
  (cond
    [(eq? read-back missing)
     (raise-arguments-error who "the table has a string that would be read back as a missing cell"
                            "column" name "row" i "element" s)]
    [(or (regexp-match? #rx"[\",\r\n]|^$|^[ \t]|[ \t]$" s)
         (not (string? read-back)))
     (quote-text s)]
    [else s]))

(define (name->cell name who)
  (when (string=? name "")
    (raise-arguments-error who "the table has a column without a name"))
  (if (regexp-match? #rx"[\",\r\n]|^[ \t]|[ \t]$" name) (quote-text name) name))

;; A real as the shortest decimal that reads back as the same flonum, without
;; a trailing .0, and a non-finite one as R writes it.
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

;; The lines of table t, each without its line feed. All of them are made
;; before any is written, so that a table that cannot be written leaves
;; nothing behind, not even an empty file.
(define (table->lines t who)
  (define names (table-names t who))
  (define columns (select-table-values t names who))
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
