#lang racket/base

;; Real-dataset loaders for the R-parity harness (and demos), mirroring the
;; sibling xgboost bindings' `private/demo-utils.rkt`. This module is NOT
;; re-exported from the `glmnet` collection -- it is a test/demo facility, kept
;; self-contained so it can later be lifted into a shared `datasets` collection.
;;
;; `try-download` is the xgboost-style best-effort fetch. The parity loaders,
;; however, read the COMMITTED CSVs under `private/data/` so that the Racket fit
;; and the R reference (scripts/r-parity/) see byte-identical inputs -- parity
;; must be deterministic, so it never depends on the network. `try-download` is
;; here for refreshing those committed files and for future demo loaders.

(require racket/list
         racket/string
         racket/port
         racket/runtime-path
         net/url)

(provide try-download
         load-longley
         load-wdbc
         load-iris
         load-veteran
         load-warpbreaks
         load-linnerud
         load-table)

(define-runtime-path data-dir "data")

;; Best-effort download: returns the body text, or #f on any failure/timeout.
;; (Thread + sync/timeout mirrors the xgboost helper so a slow/absent network
;; never hangs a build.)
(define (try-download url-string #:timeout [timeout 5])
  (define ch (make-channel))
  (define worker
    (thread
     (lambda ()
       (channel-put
        ch
        (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
          (port->string (get-pure-port (string->url url-string))))))))
  (define result (sync/timeout timeout ch))
  (unless result (kill-thread worker))
  result)

;; Parse numeric CSV text into a list of rows of reals, dropping a header row.
(define (parse-numeric-csv text)
  (define lines
    (for/list ([line (in-list (string-split text "\n"))]
               #:when (positive? (string-length (string-trim line))))
      (string-trim line)))
  (for/list ([line (in-list (cdr lines))])      ; drop the header
    (for/list ([cell (in-list (string-split line ","))])
      (string->number (string-trim cell)))))

(define (read-data-csv name)
  (call-with-input-file (build-path data-dir name) port->string))

;; A dataset as a table (#26): an association list from each column's name, as
;; the CSV's header gives it, to the column's values. `name` is the CSV's name
;; without its extension, such as "longley".
(define (load-table name)
  (define text (read-data-csv (string-append name ".csv")))
  (define header
    (for/list ([cell (in-list (string-split (car (string-split text "\n")) ","))])
      (string-trim (string-trim cell) "\"")))
  (for/list ([column-name (in-list header)]
             [column (in-list (apply map list (parse-numeric-csv text)))])
    (cons column-name column)))

;; Longley US macroeconomic data (16x7). Returns (values X y) with y = Employed
;; (the last column) and X = the first 6 columns. Real data, exported once from
;; R's `datasets::longley` (see scripts/r-parity/gen-reference.R).
(define (load-longley)
  (define rows (parse-numeric-csv (read-data-csv "longley.csv")))
  (values (map (lambda (r) (take r 6)) rows)
          (map (lambda (r) (list-ref r 6)) rows)))

;; WDBC Wisconsin breast-cancer diagnostic data (569x31). Returns (values X y)
;; with y = diagnosis (1 = malignant, 0 = benign; column 0) and X = the 30
;; numeric features. Real data from the UCI repository, sanitized to a plain
;; numeric CSV (id dropped, M/B -> 1/0).
(define (load-wdbc)
  (define rows (parse-numeric-csv (read-data-csv "wdbc.csv")))
  (values (map cdr rows)
          (map car rows)))

;; Iris (UCI, 150x5). Returns (values X y) with 4 features and integer class
;; labels y in {0,1,2} (setosa=0, versicolor=1, virginica=2). Multinomial.
(define (load-iris)
  (define rows (parse-numeric-csv (read-data-csv "iris.csv")))
  (values (map (lambda (r) (take r 4)) rows)
          (map (lambda (r) (list-ref r 4)) rows)))

;; Veteran lung-cancer survival data (137x7). Returns (values X times statuses):
;; 5 numeric features (trt, karno, diagtime, age, prior), the follow-up time, and
;; the 1 = death / 0 = censored indicator. Cox.
(define (load-veteran)
  (define rows (parse-numeric-csv (read-data-csv "veteran.csv")))
  (values (map (lambda (r) (take r 5)) rows)
          (map (lambda (r) (list-ref r 5)) rows)
          (map (lambda (r) (list-ref r 6)) rows)))

;; Warpbreaks (54x4). Returns (values X y) with 3 dummy features (woolB,
;; tensionM, tensionH) and y = breaks (a non-negative count). Poisson.
(define (load-warpbreaks)
  (define rows (parse-numeric-csv (read-data-csv "warpbreaks.csv")))
  (values (map (lambda (r) (take r 3)) rows)
          (map (lambda (r) (list-ref r 3)) rows)))

;; Linnerud (20x6). Returns (values X Y): 3 exercise predictors (chins, situps,
;; jumps) and a 3-column physiological response matrix (weight, waist, pulse).
;; Multi-response Gaussian.
(define (load-linnerud)
  (define rows (parse-numeric-csv (read-data-csv "linnerud.csv")))
  (values (map (lambda (r) (take r 3)) rows)
          (map (lambda (r) (drop r 3)) rows)))
