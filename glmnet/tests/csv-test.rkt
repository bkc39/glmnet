#lang racket/base

;; CSV files to and from tables (#61): RFC 4180 records, the value of each
;; cell, errors that name the column and row, exact round trips, and a file of
;; numbers and strings fitted through a formula.

(module+ test
  (require rackunit
           racket/contract
           racket/file
           (only-in racket/math nan?)
           racket/port
           racket/runtime-path
           glmnet
           glmnet/data/csv)

  (define-runtime-path fixtures "fixtures")

  (define (read-csv text) (csv->table (open-input-string text)))
  (define (write-csv table) (with-output-to-string (lambda () (table->csv table))))
  (define (round-trip table) (read-csv (write-csv table)))

  (define (check-error thunk . patterns)
    (check-exn (lambda (e)
                 (and (exn:fail? e)
                      (for/and ([p (in-list patterns)])
                        (regexp-match? p (exn-message e)))))
               thunk))

  ;; --- reading ---------------------------------------------------------------

  (test-case "a header and rows: numbers are flonums, other cells strings"
    (check-equal? (read-csv "x,label,y\n1,a,2.5\n-3,b c,1e3\n")
                  '(("x" . #(1.0 -3.0)) ("label" . #("a" "b c")) ("y" . #(2.5 1000.0))))
    (check-equal? (read-csv "n\n.5\n5.\n+7\n-0.25E-1\n")
                  '(("n" . #(0.5 5.0 7.0 -0.025))))
    (check-equal? (read-csv "n\n1/2\n+inf.0\n1_000\n#e1.5\n1e\n1d5\n.\n")
                  '(("n" . #("1/2" "+inf.0" "1_000" "#e1.5" "1e" "1d5" ".")))))

  (test-case "R's spellings of infinities, NaN and hexadecimal numbers are numbers"
    (check-equal? (read-csv "n\ninf\n-Infinity\nINF\n0x10\n-0X1a\n0x1.8p1\n0x.8\n1e999\n")
                  (list (cons "n" (vector +inf.0 -inf.0 +inf.0 16.0 -26.0 3.0 0.5 +inf.0))))
    (check-true (nan? (vector-ref (cdr (car (read-csv "n\nnan\n"))) 0)))
    (check-equal? (read-csv "n\nNAN\nInfin\n0x1p\n0xg\n")
                  '(("n" . #("NAN" "Infin" "0x1p" "0xg")))))

  (test-case "a decimal is read correctly rounded, however long it is"
    (for ([text (in-list '("123456789012345678901234567890" "0.30000000000000004"
                           "2.4703282292062328e-324" "1.7976931348623158e308"
                           "0.1000000000000000055511151231257827021181583404541015625"))])
      (check-eqv? (vector-ref (cdr (car (read-csv (string-append "x\n" text "\n")))) 0)
                  (real->double-flonum (string->number text 10 'number-or-false 'decimal-as-exact))
                  text)))

  (test-case "T, F, TRUE and FALSE are logicals, and no other spelling"
    (check-equal? (read-csv "b\nT\nF\nTRUE\nFALSE\ntrue\nFalse\n")
                  '(("b" . #(#t #f #t #f "true" "False")))))

  (test-case "blanks are ignored around a number, and kept around anything else"
    (check-equal? (read-csv "b\n TRUE\nFALSE \n NA \n\" 1 \"\n\t2\n")
                  '(("b" . #(" TRUE" "FALSE " " NA " 1.0 2.0)))))

  (test-case "a cell is typed on its own, so a column can mix kinds"
    (check-equal? (read-csv "zip\n02139\nSW1A\n") '(("zip" . #(2139.0 "SW1A")))))

  (test-case "quoted cells hold commas, doubled quotes and newlines"
    (check-equal? (read-csv "a,b\n\"x, y\",\"say \"\"hi\"\"\"\n\"one\ntwo\",\"\"\"\"\n")
                  '(("a" . #("x, y" "one\ntwo")) ("b" . #("say \"hi\"" "\""))))
    (check-equal? (read-csv "\"a b\",\"c,d\"\n1,2\n") '(("a b" . #(1.0)) ("c,d" . #(2.0))))
    (check-equal? (read-csv "a\n\"42\"\n") '(("a" . #(42.0)))))

  (test-case "records end with LF, CRLF or CR, and the last one need not"
    (define expected '(("a" . #(1.0 3.0)) ("b" . #(2.0 4.0))))
    (check-equal? (read-csv "a,b\n1,2\n3,4\n") expected)
    (check-equal? (read-csv "a,b\r\n1,2\r\n3,4\r\n") expected)
    (check-equal? (read-csv "a,b\r1,2\r3,4") expected)
    (check-equal? (read-csv "a,b\n1,2\n3,4") expected)
    (check-equal? (read-csv "a\n\"x\r\ny\"\n") '(("a" . #("x\r\ny")))))

  (test-case "an Excel file: a byte-order mark, CRLF and R's TRUE and FALSE"
    (check-equal? (csv-file->table (build-path fixtures "excel.csv"))
                  '(("name" . #("Ada" "Lovelace, A." "multi\r\nline"))
                    ("score" . #(91.5 78.0 -300.0))
                    ("passed" . #(#t #f #t)))))

  (test-case "R's Inf and NaN are numbers"
    (define t (read-csv "x\nInf\n-Inf\n+Inf\nNaN\n"))
    (check-equal? (cdr (assoc "x" t)) (vector +inf.0 -inf.0 +inf.0 +nan.0)))

  (test-case "blanks around a number are ignored, and a string keeps them"
    (check-equal? (read-csv "a,b\n 1 , x \n") '(("a" . #(1.0)) ("b" . #(" x ")))))

  (test-case "-0 is -0.0"
    (check-eqv? (vector-ref (cdr (assoc "a" (read-csv "a\n-0\n"))) 0) -0.0)
    (check-eqv? (vector-ref (cdr (assoc "a" (read-csv "a\n0\n"))) 0) 0.0))

  (test-case "a quoted empty cell is the empty string"
    (check-equal? (read-csv "a,b\n\"\",1\n") '(("a" . #("")) ("b" . #(1.0)))))

  (test-case "a header without rows is a table of empty columns, and round-trips"
    (check-equal? (read-csv "a,b\n") '(("a" . #()) ("b" . #())))
    (check-equal? (write-csv (read-csv "a,b\n")) "a,b\n")
    (check-equal? (round-trip '(("a") ("b"))) '(("a" . #()) ("b" . #())))
    (check-error (lambda () (write-csv (list (cons "a" '()) (cons "b" '(1)))))
                 #rx"different lengths"))

  (test-case "the column names are the header's, in its order"
    (check-equal? (map car (csv-file->table (build-path fixtures "patients.csv")))
                  '("id" "site" "age" "dose" "response" "note")))

  ;; --- errors ------------------------------------------------------------------

  (test-case "a missing cell is an error naming the column, the row and the line"
    (check-error (lambda () (read-csv "a,b\n1,2\n3,\n"))
                 #rx"^csv->table: the input has a missing value"
                 #rx"column: \"b\"" #rx"row: 1" #rx"line: 3")
    (check-error (lambda () (read-csv "a,b\n1,NA\n")) #rx"missing value" #rx"column: \"b\"")
    (check-error (lambda () (read-csv "a,b\n1,\"NA\"\n")) #rx"missing value")
    (check-error (lambda () (read-csv "a,b\n  ,1\n")) #rx"missing value" #rx"column: \"a\"")
    (check-error (lambda () (read-csv "a\n1\n\n2\n")) #rx"missing value" #rx"row: 1"))

  (test-case "a row with too few or too many cells is an error"
    (check-error (lambda () (read-csv "a,b\n1,2\n3\n"))
                 #rx"a row does not have one cell per column of the header"
                 #rx"row: 1" #rx"cells: 1" #rx"columns: 2" #rx"line: 3")
    (check-error (lambda () (read-csv "a,b\n1,2,3\n")) #rx"cells: 3")
    (check-error (lambda () (read-csv "a,b\n1,2\n\n")) #rx"one cell per column" #rx"line: 3"))

  (test-case "the line of a row counts the newlines inside quoted cells"
    (check-error (lambda () (read-csv "a,b\n\"x\ny\",1\n2,\n")) #rx"row: 1" #rx"line: 4"))

  (test-case "malformed quoting is an error naming the line"
    (check-error (lambda () (read-csv "a\n\"open\n")) #rx"a quoted cell is not closed" #rx"line: 2")
    (check-error (lambda () (read-csv "a,b\n\"x\"y,1\n"))
                 #rx"a quoted cell has text after its closing quote" #rx"line: 2")
    (check-error (lambda () (read-csv "a\nsay \"hi\"\n"))
                 #rx"a cell that is not quoted has a quote in it" #rx"line: 2"))

  (test-case "the header needs a name for each column, once"
    (check-error (lambda () (read-csv "")) #rx"the input has no header")
    (check-error (lambda () (read-csv "a,,c\n1,2,3\n"))
                 #rx"the header has a column without a name" #rx"column: 1")
    (check-error (lambda () (read-csv "a,b,a\n1,2,3\n"))
                 #rx"the header names a column twice" #rx"name: \"a\""))

  (test-case "input that is not UTF-8 is an error naming the line and the byte"
    (define latin-1 (bytes-append #"name,y\nM\374ller,1\nM\344ller,2\n"))
    (check-error (lambda () (csv->table (open-input-bytes latin-1)))
                 #rx"^csv->table: the input is not UTF-8" #rx"line: 2" #rx"byte offset: 8")
    (check-error (lambda () (csv->table (open-input-bytes #"a\r\nb\r\n\342\202")))
                 #rx"not UTF-8" #rx"line: 3" #rx"byte offset: 6")
    (define path (make-temporary-file "glmnet-csv-~a.csv"))
    (call-with-output-file path #:exists 'truncate (lambda (out) (write-bytes latin-1 out)))
    (check-error (lambda () (csv-file->table path))
                 #rx"^csv-file->table: the input is not UTF-8" #rx"file: " #rx"byte offset: 8")
    (delete-file path))

  (test-case "an error reading a file names the procedure and the file"
    (define path (make-temporary-file "glmnet-csv-~a.csv"))
    (display-to-file "a,b\n1,\n" path #:exists 'truncate)
    (check-error (lambda () (csv-file->table path))
                 #rx"^csv-file->table: the input has a missing value" #rx"file: " #rx"column: \"b\"")
    (delete-file path))

  (test-case "the arguments are checked by contracts"
    (check-exn exn:fail:contract:blame? (lambda () (csv->table "a,b\n1,2\n")))
    (check-exn exn:fail:contract:blame? (lambda () (table->csv '((1 2)))))
    (for ([exists (in-list '(never append update))])
      (check-exn exn:fail:contract:blame?
                 (lambda () (table->csv-file (list (cons "a" '(1))) "x.csv" #:exists exists)))))

  ;; --- writing -------------------------------------------------------------------

  (test-case "writing: a header, then one line per row"
    (check-equal? (write-csv (list (cons "x" '(1 2.5)) (cons 'label #("a" "b c"))))
                  "x,label\n1,a\n2.5,b c\n"))

  (test-case "flonums round-trip exactly"
    (define xs (list 0.0 -0.0 1.0 -1.0 0.1 (+ 0.1 0.2) 1/3 2.62 1e-7 1e22 123456789012.0
                     5e-324 2.2250738585072014e-308 1.7976931348623157e308
                     +inf.0 -inf.0 +nan.0 (exp 1) (- (sqrt 2)) 21))
    (define back (cdr (assoc "x" (round-trip (list (cons "x" xs))))))
    (for ([x (in-list xs)] [b (in-vector back)])
      (check-eqv? b (real->double-flonum x) (format "~a" x))))

  (test-case "random flonums round-trip exactly"
    (random-seed 61)
    (define xs
      (for/list ([_ (in-range 2000)])
        (define x (floating-point-bytes->real
                   (list->bytes (for/list ([_ (in-range 8)]) (random 256)))))
        (if (nan? x) 0.5 x)))
    (check-true (andmap eqv? xs (vector->list (cdr (assoc "x" (round-trip (list (cons "x" xs)))))))))

  (test-case "strings and booleans round-trip, and symbols come back as strings"
    (define t (list (cons "s" #("plain" "a,b" "say \"hi\"" "" "two\nlines" " padded " "  "))
                    (cons "b" #(#t #f #t #f #t #f #t))
                    (cons "sym" #(a |b c| |x,y| d e f g))))
    (check-equal? (round-trip t)
                  (list (car t) (cadr t)
                        (cons "sym" #("a" "b c" "x,y" "d" "e" "f" "g")))))

  (test-case "a string that reads as another value is quoted, and still read as that value"
    (check-equal? (write-csv (list (cons "s" '("42" "TRUE" "Inf" "T" "0x10" "nan" "x" "true"))))
                  "s\n\"42\"\n\"TRUE\"\n\"Inf\"\n\"T\"\n\"0x10\"\n\"nan\"\nx\ntrue\n")
    (check-equal? (round-trip (list (cons "s" '("42" "TRUE" "T" "0x10"))))
                  '(("s" . #(42.0 #t #t 16.0)))))

  (test-case "a string of white space is quoted, and kept"
    (define t (list (cons "s" (vector "\f" "\v" " " "\t\t" "a\v"))))
    (check-equal? (round-trip t) t))

  (test-case "names are quoted when they need it"
    (check-equal? (write-csv (list (cons "a,b" '(1)) (cons "say \"x\"" '(2)) (cons " c" '(3))))
                  "\"a,b\",\"say \"\"x\"\"\",\" c\"\n1,2,3\n")
    (check-equal? (map car (round-trip (list (cons "a,b" '(1)) (cons " c" '(2)))))
                  '("a,b" " c")))

  (test-case "a first name that starts with a byte-order mark is quoted, and kept"
    (check-equal? (write-csv (list (cons "\uFEFFid" '(1)))) "\"\uFEFFid\"\n1\n")
    (check-equal? (map car (round-trip (list (cons "\uFEFFid" '(1)) (cons "x" '(2)))))
                  '("\uFEFFid" "x"))
    (check-equal? (map car (read-csv "\uFEFFid,x\n1,2\n")) '("id" "x")))

  (test-case "every kind of table is written in its order of columns"
    (check-equal? (write-csv (hash "b" '(1) 'a '(2))) "a,b\n2,1\n")
    (check-equal? (write-csv (rows->design-matrix '((1 2) (3 4)) #:column-names '(u v)))
                  "u,v\n1,2\n3,4\n"))

  (test-case "what cannot be written is an error naming the column and row"
    (check-error (lambda () (write-csv (list (cons "s" '("x" "NA")))))
                 #rx"^table->csv: the table has a string that would be read back as a missing value"
                 #rx"column: \"s\"" #rx"row: 1")
    (check-error (lambda () (write-csv (list (list "v" 1 '(2)))))
                 #rx"not a real, string, symbol or boolean" #rx"column: \"v\"" #rx"row: 1")
    (check-error (lambda () (write-csv (list (cons "" '(1)))))
                 #rx"the table has a column without a name")
    (check-error (lambda () (write-csv (list (cons "a" '(1 2)) (cons "b" '(3)))))
                 #rx"different lengths"))

  (test-case "a table that cannot be written writes nothing"
    (define out (open-output-string))
    (check-error (lambda () (table->csv (list (cons "s" '("x" "NA"))) out)) #rx"missing value")
    (check-equal? (get-output-string out) "")
    (define path (make-temporary-file "glmnet-csv-~a.csv"))
    (delete-file path)
    (check-error (lambda () (table->csv-file (list (cons "s" '("x" "NA"))) path))
                 #rx"^table->csv-file: " #rx"missing value")
    (check-false (file-exists? path)))

  (test-case "files: table->csv-file and csv-file->table"
    (define path (make-temporary-file "glmnet-csv-~a.csv"))
    (define t (list (cons "x" #(1.5 -2.0)) (cons "g" #("a" "b, c"))))
    (check-error (lambda () (table->csv-file t path)) #rx"exists")
    (table->csv-file t path #:exists 'replace)
    (check-equal? (file->string path) "x,g\n1.5,a\n-2,\"b, c\"\n")
    (check-equal? (csv-file->table path) t)
    (table->csv-file (list (cons "x" '(3))) path #:exists 'truncate)
    (check-equal? (csv-file->table path) '(("x" . #(3.0))))
    (table->csv-file (list (cons "y" '(4))) path #:exists 'truncate/replace)
    (check-equal? (csv-file->table path) '(("y" . #(4.0))))
    (delete-file path))

  (test-case "every shipped dataset round-trips"
    (define dir (collection-file-path "datasets" "glmnet"))
    (for ([file (in-list (directory-list dir))]
          #:when (regexp-match? #rx"[.]csv$" (path->string file)))
      (define t (csv-file->table (build-path dir file)))
      (check-equal? (round-trip t) t (path->string file))))

  ;; --- a file fitted through a formula ------------------------------------------

  (test-case "a file of numbers and strings fits as the same table written by hand"
    (define from-file (csv-file->table (build-path fixtures "patients.csv")))
    (check-equal? (cdr (assoc "note" from-file))
                  #("" "said \"fine\"" "line one\nline two" "none" "none" "follow up" "none"
                    "none" "follow up" "none" "none" "none"))
    (define by-hand
      (list (cons "site" '("Boston" "Portland, ME" "Portland, OR" "Boston" "Boston" "Portland, ME"
                           "Portland, OR" "Boston" "Portland, ME" "Portland, OR" "Boston"
                           "Portland, ME"))
            (cons "age" '(34 51 67 45 29 58 62 41 38 55 49 33))
            (cons "dose" '(2.5 5 1 3.5 4.5 2 6 1.5 3 4 5.5 2.5))
            (cons "response" '(3.1 5.2 1.9 4 4.9 2.4 5.6 2.2 3.7 4.4 6.1 2.6))))
    (define f (response . ~ . age + dose + site))
    (define m (formula-path f from-file))
    (check-equal? (formula-model-predictor-names m)
                  '("age" "dose" "sitePortland, ME" "sitePortland, OR"))
    (check-equal? (formula-model-levels m) '(("site" "Boston" "Portland, ME" "Portland, OR")))
    (check-equal? (coef m #:lambda 0.01) (coef (formula-path f by-hand) #:lambda 0.01))
    (check-equal? (predict m from-file #:lambda 0.01)
                  (predict (formula-path f by-hand) by-hand #:lambda 0.01))))
