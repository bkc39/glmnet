#lang racket/base

;; Tables (#26), the named data that formulas read: what counts as a table,
;; the order of its columns, and the conversion of named columns to a design
;; matrix, with errors that name the column. glmnet/data alone, without the
;; native library.

(module+ test
  (require rackunit
           glmnet/data)

  (define alist
    (list (cons "y" '(1 2 3))
          (cons 'x1 #(4 5 6))
          (cons "x2" '(7.5 8.5 9.5))
          (cons "label" '("a" "b" "c"))))

  (test-case "what is a table"
    (check-true (table? alist))
    (check-true (table? (hash "b" '(1 2) 'a #(3 4))))
    (check-true (table? (make-hash (list (cons "a" '(1 2))))))
    (check-true (table? (rows->design-matrix '((1 2)) #:column-names '(a b))))
    (check-false (table? (rows->design-matrix '((1 2)))))
    (check-false (table? '((1 2) (3 4))))
    (check-false (table? '()))
    (check-false (table? (hash)))
    (check-false (table? (list (cons "a" 1))))
    (check-false (table? (hash 1 '(1 2))))
    (check-false (table? "y")))

  (test-case "column names are strings, in the table's order; a hash's are sorted"
    (check-equal? (table-column-names alist) '("y" "x1" "x2" "label"))
    (check-equal? (table-column-names (hash "b" '(1) 'a '(2) "c" '(3))) '("a" "b" "c"))
    (check-equal? (table-column-names (columns->design-matrix '((1) (2)) #:column-names '(u "v")))
                  '("u" "v")))

  (test-case "two columns whose names are the same string are an error"
    (check-exn #rx"two columns with the same name.*\"x\""
               (lambda () (table-column-names (hash "x" '(1) 'x '(2)))))
    (check-exn #rx"two columns with the same name"
               (lambda () (table->design-matrix (list (cons "x" '(1)) (cons "x" '(2)))))))

  (test-case "table->design-matrix selects columns by name, in the order given"
    (define all-numeric (table->design-matrix (list (cons "a" '(1 2)) (cons 'b #(3 4)))))
    (check-equal? (design-matrix-column-names all-numeric) '("a" "b"))
    (check-equal? (design-matrix->columns all-numeric) '((1.0 2.0) (3.0 4.0)))
    (define d (table->design-matrix alist '(x2 "y")))
    (check-equal? (design-matrix-column-names d) '("x2" "y"))
    (check-equal? (design-matrix->columns d) '((7.5 8.5 9.5) (1.0 2.0 3.0)))
    (check-equal? (design-matrix->columns (table->design-matrix (hash 'b '(1 2) "a" #(3 4))))
                  '((3.0 4.0) (1.0 2.0))))

  (test-case "a design matrix with names is a table"
    (define d (rows->design-matrix '((1 2 3) (4 5 6)) #:column-names '(a b c)))
    (define picked (table->design-matrix d '("c" a)))
    (check-equal? (design-matrix-column-names picked) '("c" "a"))
    (check-equal? (design-matrix->rows picked) '((3.0 1.0) (6.0 4.0)))
    (check-equal? (table->design-matrix d '("a" "b" "c"))
                  (rows->design-matrix '((1 2 3) (4 5 6)) #:column-names '("a" "b" "c"))))

  (test-case "only the selected columns must be numbers"
    (check-exn #rx"not a real number.*column: \"label\".*row: 0"
               (lambda () (table->design-matrix alist))))

  (test-case "errors name the column"
    (check-exn #rx"no column with this name.*column: \"z\""
               (lambda () (table->design-matrix alist '("y" z))))
    (check-exn #rx"not finite.*column: \"a\".*row: 1"
               (lambda () (table->design-matrix (list (cons "a" '(1.0 +inf.0))))))
    (check-exn #rx"different lengths.*column: \"b\".*length: 1"
               (lambda () (table->design-matrix (list (cons "a" '(1 2)) (cons "b" '(3))))))
    (check-exn #rx"no rows.*column: \"a\""
               (lambda () (table->design-matrix (list (cons "a" '()) (cons "b" '())))))
    (check-exn #rx"a column is named twice"
               (lambda () (table->design-matrix alist '("y" y))))
    (check-exn exn:fail:contract? (lambda () (table->design-matrix '((1 2) (3 4)))))
    (check-exn exn:fail:contract? (lambda () (table->design-matrix alist '())))))
