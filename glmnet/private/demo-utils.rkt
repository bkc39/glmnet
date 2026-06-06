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
         load-wdbc)

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
