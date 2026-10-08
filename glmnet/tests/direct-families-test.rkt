#lang racket/base

;; The binomial, multinomial, Poisson, Cox and multi-response Gaussian
;; families on the user's data directly (#74): every entry point and every
;; format gives what the explicit adapter route gives, string, symbol and
;; boolean class labels are classes that predict returns, fits from named data
;; predict from named data by column name, and bad data is an error naming its
;; column and row.

(module+ test
  (require rackunit
           racket/list
           (only-in racket/contract exn:fail:contract:blame?)
           (only-in math/array list->array)
           (only-in math/matrix list*->matrix)
           (only-in polars dataframe series polars-null)
           glmnet
           glmnet/data/csv
           glmnet/data/polars
           (only-in glmnet/datasets iris))

  ;; --- data -----------------------------------------------------------------------

  (define (dataset name)
    (csv-file->table (collection-file-path (string-append name ".csv") "glmnet" "datasets")))

  (define (column table name) (vector->list (cdr (assoc name table))))
  (define (v-names n) (for/list ([i (in-range 1 (add1 n))]) (format "V~a" i)))
  (define (rows-of table names) (apply map list (map (lambda (name) (column table name)) names)))
  (define (as-hash table) (for/hash ([entry (in-list table)]) (values (car entry) (cdr entry))))
  (define (as-vectors rows) (list->vector (map list->vector rows)))

  ;; The first n rows of a table's columns `names`, in reverse order, after a
  ;; text column: new data that a fit from named data reads by name.
  (define (new-table table names n)
    (cons (cons "id" (for/vector #:length n ([i (in-range n)]) (format "row ~a" i)))
          (reverse (for/list ([name (in-list names)])
                     (cons name (list->vector (take (column table name) n)))))))

  (define binomial-table (dataset "BinomialExample"))
  (define poisson-table (dataset "PoissonExample"))
  (define cox-table (dataset "CoxExample"))
  (define mgaussian-table (dataset "MultiGaussianExample"))

  (define species '("setosa" "versicolor" "virginica"))
  (define iris-predictors '("Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width"))
  ;; iris with a two-level column of strings: whether the flower is a virginica.
  (define iris+
    (cons (cons "virginica" (for/vector ([s (in-list (column iris "Species"))])
                              (if (equal? s "virginica") "yes" "no")))
          iris))

  (define (folds n k) (for/list ([i (in-range n)]) (modulo i k)))

  ;; --- the families --------------------------------------------------------------------

  ;; A family's case: its entry points at fixed options, each a procedure of X,
  ;; the response arguments and #:predictors; a named table, its response (a
  ;; name, or a list of names) and predictors; the response arguments of the
  ;; explicit route, whose predictors are table->design-matrix's; and the
  ;; classes, for a response of labels.
  (struct family-case (name entry-points table response predictors explicit-responses classes))

  ;; The entry points of a family whose procedures are fit, path and cv; the
  ;; CV folds are `fold-ids`.
  (define (entry-points fit path cv fold-ids)
    (list (cons (object-name fit)
                (lambda (X #:predictors [p #f] . ys) (apply fit X ys #:predictors p #:lambda 0.05)))
          (cons (object-name path)
                (lambda (X #:predictors [p #f] . ys) (apply path X ys #:predictors p #:nlambda 10)))
          (cons (object-name cv)
                (lambda (X #:predictors [p #f] . ys)
                  (apply cv X ys #:predictors p #:nlambda 10 #:fold-ids fold-ids)))))

  (define (indices-of labels yes) (for/list ([s (in-list labels)]) (if (equal? s yes) 1 0)))

  (define cases
    (list
     (family-case 'binomial (entry-points logistic-fit logistic-path logistic-cv (folds 100 4))
                  binomial-table "y" (v-names 6)
                  (list (map inexact->exact (column binomial-table "y"))) #f)
     (family-case 'binomial-labels (entry-points logistic-fit logistic-path logistic-cv (folds 150 3))
                  iris+ "virginica" iris-predictors
                  (list (indices-of (column iris+ "virginica") "yes")) '("no" "yes"))
     (family-case 'multinomial
                  (entry-points multinomial-fit multinomial-path multinomial-cv (folds 150 3))
                  iris "Species" iris-predictors
                  (list (for/list ([s (in-list (column iris "Species"))]) (index-of species s)))
                  species)
     (family-case 'poisson (entry-points poisson-fit poisson-path poisson-cv (folds 500 3))
                  poisson-table "y" (v-names 6) (list (column poisson-table "y")) #f)
     (family-case 'cox (entry-points cox-fit cox-path cox-cv (folds 1000 3))
                  cox-table '("time" "status") (v-names 6)
                  (list (column cox-table "time") (column cox-table "status")) #f)
     (family-case 'mgaussian (entry-points mgaussian-fit mgaussian-path mgaussian-cv (folds 100 4))
                  mgaussian-table '("y1" "y2" "y3" "y4") (v-names 6)
                  (list (table->design-matrix mgaussian-table '("y1" "y2" "y3" "y4"))) #f)))

  (define (fit-with fit X ys [predictors #f])
    (keyword-apply fit '(#:predictors) (list predictors) X ys))

  ;; The model's predictions of each type its family has, at its default λ.
  (define (predictions model X)
    (define family (glmnet-path-family (glmnet-model->path model)))
    (cons (predict model X #:type 'response)
          (case family
            [(binomial multinomial) (list (predict model X) (predict model X #:type 'class))]
            [else (list (predict model X))])))

  ;; The explicit model's predictions, with its class indices, one list of them
  ;; per λ for a path, as `classes`.
  (define (labelled-predictions model X classes)
    (define ps (predictions model X))
    (define (label k) (if (list? k) (map label k) (list-ref classes k)))
    (cond
      [(and classes (= (length ps) 3)) (list (car ps) (cadr ps) (label (caddr ps)))]
      [else ps]))

  (define (check-same-model direct explicit classes new-data direct-new-data)
    (check-equal? direct explicit)
    (check-equal? (coef direct) (coef explicit))
    (check-equal? (deviance-ratio direct) (deviance-ratio explicit))
    (check-equal? (predictions direct direct-new-data)
                  (labelled-predictions explicit new-data classes)))

  ;; --- every entry point and format matches the explicit route --------------------------

  ;; The formats of a case's data: a label and the arguments of an entry point,
  ;; X, the response arguments and #:predictors, with the new data that the
  ;; direct model predicts from.
  (define (formats c)
    (define table (family-case-table c))
    (define predictors (family-case-predictors c))
    (define response (family-case-response c))
    (define responses (if (list? response) response (list response)))
    (define mgaussian? (eq? (family-case-name c) 'mgaussian))
    (define rows (rows-of table predictors))
    (define new-rows (take rows 3))
    (define new-named (new-table table predictors 3))
    ;; The unnamed responses: one sequence each, or the rows of mgaussian's.
    (define ys (if mgaussian?
                   (list (rows-of table responses))
                   (map (lambda (name) (column table name)) responses)))
    (define M (list*->matrix rows))
    (define symbol-response
      (if (list? response) (map string->symbol response) (string->symbol response)))
    (append
     (list (list 'dataframe (table->polars table) (list response) predictors
                 (table->polars new-named))
           (list 'alist table (list response) predictors new-named)
           (list 'hash (as-hash table) (list response) predictors (as-hash new-named))
           (list 'symbols table (list symbol-response) (map string->symbol predictors)
                 (table->polars new-named))
           (list 'nested-lists rows ys #f new-rows)
           (list 'nested-vectors (as-vectors rows)
                 (if mgaussian? (list (as-vectors (car ys))) (map list->vector ys))
                 #f (as-vectors new-rows))
           (list 'math-matrix M ys #f (list*->matrix new-rows))
           (list 'series M
                 (if mgaussian?
                     (list (list*->matrix (car ys)))
                     (for/list ([y (in-list ys)] [name (in-list responses)]) (series y #:name name)))
                 #f new-rows))
     (cond
       [(family-case-classes c) '()]
       [mgaussian? (list (list 'design-matrix-response M (family-case-explicit-responses c) #f new-rows))]
       [else (list (list 'math-array M (map list->array ys) #f new-rows))])))

  (for ([c (in-list cases)])
    (define dm (table->design-matrix (family-case-table c) (family-case-predictors c)))
    (define new-rows (take (rows-of (family-case-table c) (family-case-predictors c)) 3))
    (for ([entry (in-list (family-case-entry-points c))])
      (define fit (cdr entry))
      (define explicit (fit-with fit dm (family-case-explicit-responses c)))
      (for ([data (in-list (formats c))])
        (define-values (label X ys predictors new-data) (apply values data))
        (test-case (format "~a from ~a" (car entry) label)
          (check-same-model (fit-with fit X ys predictors) explicit (family-case-classes c)
                            new-rows new-data)))))

  ;; --- prediction helpers ---------------------------------------------------------------

  (test-case "the prediction helpers read named data by name and return class labels"
    (define new-iris (new-table iris+ iris-predictors 5))
    (define rows (take (rows-of iris+ iris-predictors) 5))
    (define b (logistic-fit iris+ "virginica" #:predictors iris-predictors #:lambda 0.05))
    (define b-explicit (logistic-fit (table->design-matrix iris+ iris-predictors)
                                     (indices-of (column iris+ "virginica") "yes") #:lambda 0.05))
    (check-equal? (logistic-predict-proba b (table->polars new-iris))
                  (logistic-predict-proba b-explicit rows))
    (check-equal? (logistic-predict b new-iris)
                  (map (lambda (k) (list-ref '("no" "yes") k)) (logistic-predict b-explicit rows)))
    (check-equal? (logistic-predict b (as-hash new-iris) #:threshold 0.0) (make-list 5 "yes"))
    (define m (multinomial-fit iris "Species" #:predictors iris-predictors #:lambda 0.05))
    (check-equal? (multinomial-predict m (table->polars new-iris)) (make-list 5 "setosa"))
    (check-equal? (multinomial-predict-proba m new-iris) (predict m rows #:type 'response))
    (define p (poisson-fit poisson-table "y" #:predictors (v-names 6) #:lambda 0.05))
    (define p-rows (take (rows-of poisson-table (v-names 6)) 4))
    (check-equal? (poisson-predict-mean p (new-table poisson-table (v-names 6) 4))
                  (poisson-predict-mean p p-rows))
    (define x (cox-fit cox-table '("time" "status") #:predictors (v-names 6) #:lambda 0.05))
    (define x-new (table->polars (new-table cox-table (v-names 6) 4)))
    (define x-rows (take (rows-of cox-table (v-names 6)) 4))
    (check-equal? (cox-linear-predictor x x-new) (cox-linear-predictor x x-rows))
    (check-equal? (cox-relative-risk x x-new) (cox-relative-risk x x-rows))
    (define g (mgaussian-fit mgaussian-table '("y1" "y2") #:predictors (v-names 6) #:lambda 0.05))
    (check-equal? (mgaussian-predict g (as-hash (new-table mgaussian-table (v-names 6) 2)))
                  (mgaussian-predict g (take (rows-of mgaussian-table (v-names 6)) 2))))

  (test-case "classes: levels in R's order, symbols as strings, booleans as FALSE and TRUE"
    (define X '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0) (5.0 6.0) (6.0 4.0)))
    (define (classes-of fit) (predict fit X #:type 'class #:lambda 0.0))
    (define b (logistic-fit X '(b a b a a b) #:lambda 0.0001))
    (check-equal? (logistic-fit X '(b a b a a b) #:lambda 0.0001) (logistic-fit X '(1 0 1 0 0 1) #:lambda 0.0001))
    (check-not-false (andmap (lambda (c) (member c '("a" "b"))) (classes-of b)))
    (define t (logistic-fit X '(#t #f #t #f #f #t) #:lambda 0.0001))
    (check-equal? t (logistic-fit X '(1 0 1 0 0 1) #:lambda 0.0001))
    (check-not-false (andmap (lambda (c) (member c '("FALSE" "TRUE"))) (logistic-predict t X)))
    (define frame (dataframe (list (series '(1.0 2.0 3.0 4.0 5.0 6.0) #:name "x")
                                   (series '(#t #f #t #f #f #t) #:name "event"))))
    (check-equal? (logistic-fit frame "event" #:predictors '("x") #:lambda 0.01)
                  (logistic-fit '((1.0) (2.0) (3.0) (4.0) (5.0) (6.0)) '(1 0 1 0 0 1) #:lambda 0.01))
    ;; Symbols, as a categorical column holds, name the same classes as strings.
    (define iris-symbols
      (for/list ([entry (in-list iris)])
        (if (equal? (car entry) "Species")
            (cons "Species" (for/vector ([s (in-vector (cdr entry))]) (string->symbol s)))
            entry)))
    (define m (multinomial-path iris-symbols "Species" #:predictors iris-predictors #:nlambda 5))
    (check-equal? m (multinomial-path iris "Species" #:predictors iris-predictors #:nlambda 5))
    (check-equal? (predict m (table->polars iris-symbols) #:type 'class #:lambda 0.01)
                  (predict m iris #:type 'class #:lambda 0.01))
    (check-equal? (remove-duplicates (predict m iris #:type 'class #:lambda 0.01)) species)
    (define cv (multinomial-cv (table->polars iris-symbols) "Species" #:predictors iris-predictors
                               #:fold-ids (folds 150 3) #:nlambda 5))
    (check-equal? (predict cv '((5.1 3.5 1.4 0.2)) #:type 'class) '("setosa")))

  ;; --- errors -------------------------------------------------------------------------------

  (define ((blame-matching . patterns) e)
    (and (exn:fail:contract:blame? e)
         (for/and ([p (in-list (cons #rx"blaming: [^\n]*direct-families-test[.]rkt" patterns))])
           (regexp-match? p (exn-message e)))))

  (define ((error-matching . patterns) e)
    (and (exn:fail:contract? e)
         (not (exn:fail:contract:blame? e))
         (for/and ([p (in-list patterns)])
           (regexp-match? p (exn-message e)))))

  (define cox-frame (table->polars (take cox-table 32)))
  (define small-cox
    (list (cons "x" '(1.0 2.0 3.0 4.0)) (cons "z" '(2.0 1.0 4.0 3.0))
          (cons "time" '(5.0 3.0 6.0 2.0)) (cons "status" '(1 0 1 1))))

  (test-case "Cox: the response is a list of the time and status columns"
    (check-exn (blame-matching #rx"^cox-fit: contract violation"
                               #rx"expected: a list of the names of two columns of the table, the time and the status"
                               #rx"given: '\\(\"time\"\\), a list of one name"
                               #rx"in: the y argument of")
               (lambda () (cox-fit small-cox '("time") #:predictors '("x") #:lambda 0.1)))
    (check-exn (blame-matching #rx"given: '\\(\"time\" \"status\" \"z\"\\), a list of 3 names")
               (lambda () (cox-path small-cox '("time" "status" "z") #:predictors '("x"))))
    (check-exn (blame-matching #rx"given: \"time\"\n")
               (lambda () (cox-cv small-cox "time" #:predictors '("x"))))
    (check-exn (blame-matching #rx"given: \"stat\", which is not a column of the dataframe")
               (lambda () (cox-fit cox-frame '("time" "stat") #:predictors '("V1") #:lambda 0.1)))
    (check-exn (blame-matching #rx"given: column \"status\", which is the response"
                               #rx"in: the #:predictors argument of")
               (lambda () (cox-fit small-cox '("time" "status") #:predictors '("x" "status")
                                   #:lambda 0.1)))
    (check-exn (blame-matching #rx"expected: no statuses argument, since y names the time and status columns of the table"
                               #rx"in: the statuses argument of")
               (lambda () (cox-fit small-cox '("time" "status") '(1 0 1 1) #:predictors '("x")
                                   #:lambda 0.1)))
    (check-exn (blame-matching #rx"statuses is required when X is unnamed data")
               (lambda () (cox-fit '((1.0) (2.0)) '(1.0 2.0) #:lambda 0.1)))
    (check-exn (blame-matching #rx"#:predictors is required when X is a dataframe")
               (lambda () (cox-fit cox-frame '("time" "status") #:lambda 0.1))))

  (test-case "Cox: a status that is not 0 or 1, or a time that is not positive, names its row"
    (define (with column values) (cons (cons column values) (remove (assoc column small-cox) small-cox)))
    (check-exn (error-matching #rx"^cox-fit: the table has an element that is not \\(or/c 0 1\\)"
                               #rx"column: \"status\"\n  row: 2\n  element: 2.0")
               (lambda () (cox-fit (with "status" '(1 0 2 1)) '("time" "status") #:predictors '("x")
                                   #:lambda 0.1)))
    (check-exn (error-matching #rx"^cox-path: the dataframe has an element that is not \\(>/c 0\\)"
                               #rx"column: \"time\"\n  row: 1\n  element: 0.0")
               (lambda () (cox-path (table->polars (with "time" '(5.0 0.0 6.0 2.0))) '("time" "status")
                                    #:predictors '("x" "z"))))
    (check-exn (blame-matching #rx"given: column \"id\", of dtype string")
               (lambda () (cox-fit (dataframe (list (series '("a" "b") #:name "id")
                                                    (series '(1.0 2.0) #:name "x")
                                                    (series '(1 1) #:name "status")))
                                   '("id" "status") #:predictors '("x") #:lambda 0.1))))

  (test-case "mgaussian: the response is a non-empty list of numeric columns"
    (define ys '("y1" "y2"))
    (check-exn (blame-matching #rx"^mgaussian-fit: contract violation"
                               #rx"expected: a non-empty list of distinct names of numeric columns of the table"
                               #rx"given: '\\(\\)" #rx"in: the Y argument of")
               (lambda () (mgaussian-fit mgaussian-table '() #:predictors '("V1") #:lambda 0.1)))
    (check-exn (blame-matching #rx"given: \"y1\"\n")
               (lambda () (mgaussian-path mgaussian-table "y1" #:predictors '("V1"))))
    (check-exn (blame-matching #rx"given: \"y1\" twice")
               (lambda () (mgaussian-path mgaussian-table '("y1" y1) #:predictors '("V1"))))
    (check-exn (blame-matching #rx"given: \"y5\", which is not a column of the table")
               (lambda () (mgaussian-cv mgaussian-table '("y1" "y5") #:predictors '("V1"))))
    (check-exn (blame-matching #rx"given: column \"y2\", which is the response")
               (lambda () (mgaussian-fit mgaussian-table ys #:predictors '("V1" "y2") #:lambda 0.1)))
    (check-exn (error-matching #rx"^mgaussian-fit: the table has an element that is not finite"
                               #rx"column: \"y2\"\n  row: 1")
               (lambda () (mgaussian-fit (list (cons "x" '(1 2 3)) (cons "y1" '(1 2 3))
                                               (cons "y2" '(1 +nan.0 3)))
                                         ys #:predictors '("x") #:lambda 0.1)))
    (check-exn (blame-matching #rx"since X is unnamed" #rx"in: the Y argument of")
               (lambda () (mgaussian-fit '((1.0) (2.0)) (hash "y1" '(1 2)) #:lambda 0.1))))

  (test-case "Poisson: a negative count names its column and row"
    (check-exn (error-matching #rx"^poisson-fit: the table has an element that is not \\(>=/c 0\\)"
                               #rx"column: \"n\"\n  row: 2\n  element: -1.0")
               (lambda () (poisson-fit (list (cons "x" '(1 2 3 4)) (cons "n" '(0 1 -1 3))) "n"
                                       #:predictors '("x") #:lambda 0.1)))
    (check-exn (blame-matching #rx"expected: \\(>=/c 0\\)" #rx"the element at position 2 of")
               (lambda () (poisson-path '((1.0) (2.0) (3.0)) '(1 2 -3))))
    (check-exn (error-matching #rx"^poisson-cv: y has an element that is not \\(>=/c 0\\) as a flonum"
                               #rx"position: 1")
               (lambda () (poisson-cv '((1.0) (2.0) (3.0)) (series '(1 -2 3) #:name "n")))))

  (test-case "binomial and multinomial: the classes of a response of labels"
    (check-exn (error-matching #rx"^logistic-fit: a binomial response has two classes; the multinomial family takes more"
                               #rx"column: \"Species\"\n  classes: '\\(\"setosa\" \"versicolor\" \"virginica\"\\)")
               (lambda () (logistic-fit iris "Species" #:predictors iris-predictors #:lambda 0.1)))
    (check-exn (error-matching #rx"^logistic-path: a binomial response needs two classes"
                               #rx"classes: '\\(\"a\"\\)")
               (lambda () (logistic-path '((1.0) (2.0) (3.0)) '(a a a))))
    (check-exn (error-matching #rx"^multinomial-cv: a multinomial response needs at least two classes"
                               #rx"column: \"k\"")
               (lambda () (multinomial-cv (list (cons "x" '(1 2 3)) (cons "k" '("a" "a" "a"))) "k"
                                          #:predictors '("x"))))
    (check-exn (error-matching #rx"^multinomial-fit: a column mixes strings or symbols with numbers"
                               #rx"column: \"k\"\n  row: 1")
               (lambda () (multinomial-fit (list (cons "x" '(1 2 3)) (cons "k" '("a" 1 "b"))) "k"
                                           #:predictors '("x") #:lambda 0.1)))
    (check-exn (blame-matching #rx"expected: a string or a symbol, as the first element is"
                               #rx"given: 1" #rx"the element at position 1 of")
               (lambda () (multinomial-fit '((1.0) (2.0) (3.0)) '("a" 1 "b") #:lambda 0.1)))
    (check-exn (error-matching #rx"^multinomial-fit: the dataframe has a missing value"
                               #rx"column: \"k\"\n  row: 1")
               (lambda () (multinomial-fit (dataframe (list (series '(1.0 2.0 3.0) #:name "x")
                                                            (series (list "a" polars-null "b") #:name "k")))
                                           "k" #:predictors '("x") #:lambda 0.1)))
    (check-exn (blame-matching #rx"given: \"Species\", which is not a column of the table")
               (lambda () (multinomial-path (remove (assoc "Species" iris) iris) "Species"
                                            #:predictors iris-predictors)))
    (check-exn (error-matching #rx"^logistic-cv: a binomial response needs two classes"
                               #rx"classes: '\\(\"FALSE\"\\)")
               (lambda () (logistic-cv '((1.0) (2.0) (3.0)) '(#f #f #f)))))

  ;; --- what a fit remembers ----------------------------------------------------------------

  (test-case "the path inside a cross-validated fit returns class labels and reads by name"
    (define new-iris (new-table iris+ iris-predictors 5))
    (define b (logistic-cv iris+ "virginica" #:predictors iris-predictors #:fold-ids (folds 150 3)
                           #:nlambda 10))
    (check-equal? (predict (glmnet-cv-path b) new-iris #:type 'class #:lambda 0.01)
                  (predict b new-iris #:type 'class #:lambda 0.01))
    (check-not-false (andmap (lambda (c) (member c '("no" "yes")))
                             (predict (glmnet-cv-path b) new-iris #:type 'class #:lambda 0.01)))
    (define m (multinomial-cv iris "Species" #:predictors iris-predictors #:fold-ids (folds 150 3)
                              #:nlambda 10))
    (check-equal? (predict (glmnet-cv-path m) (table->polars new-iris) #:type 'class #:lambda 0.01)
                  (make-list 5 "setosa"))
    (define g (mgaussian-cv mgaussian-table '("y1" "y2") #:predictors (v-names 6)
                            #:fold-ids (folds 100 4) #:nlambda 10))
    (check-equal? (glmnet-model-response-names (glmnet-cv-path g)) '("y1" "y2")))

  (test-case "the accessors read what a fit remembers, and coef keeps its layout"
    (define m (multinomial-fit iris "Species" #:predictors iris-predictors #:lambda 0.05))
    (define explicit (multinomial-fit (table->design-matrix iris iris-predictors)
                                      (for/list ([s (in-list (column iris "Species"))])
                                        (index-of species s))
                                      #:lambda 0.05))
    (check-equal? (glmnet-model-predictor-names m) iris-predictors)
    (check-equal? (glmnet-model-class-labels m) species)
    (check-false (glmnet-model-response-names m))
    (check-equal? (coef m) (coef explicit))
    (check-false (glmnet-model-predictor-names explicit))
    (check-false (glmnet-model-class-labels explicit))
    (define g (mgaussian-path mgaussian-table '("y2" "y1") #:predictors (v-names 6) #:nlambda 5))
    (check-equal? (glmnet-model-response-names g) '("y2" "y1"))
    (check-equal? (glmnet-model-class-labels (logistic-fit '((1.0) (2.0) (3.0) (4.0))
                                                           '(#t #f #f #t) #:lambda 0.01))
                  '("FALSE" "TRUE"))
    (define formula (formula-fit (~ Species all) iris #:family 'multinomial #:lambda 0.05))
    (check-equal? (glmnet-model-class-labels formula) species)
    (check-not-equal? (coef formula) (coef m)))

  ;; --- response forms ------------------------------------------------------------------------

  (define X6 '((1.0 2.0) (2.0 1.0) (3.0 4.0) (4.0 3.0) (5.0 6.0) (6.0 4.0)))

  (test-case "a math array of labels is a response of labels, as a list is"
    (define labels '("p" "q" "p" "q" "q" "p"))
    (check-equal? (logistic-fit X6 (list->array labels) #:lambda 0.01)
                  (logistic-fit X6 labels #:lambda 0.01))
    (check-equal? (logistic-predict (logistic-fit X6 (list->array labels) #:lambda 0.01) X6)
                  (logistic-predict (logistic-fit X6 labels #:lambda 0.01) X6))
    (define kinds '(a b c a b c))
    (check-equal? (multinomial-path X6 (list->array kinds) #:nlambda 5)
                  (multinomial-path X6 kinds #:nlambda 5))
    (check-equal? (glmnet-model-class-labels (multinomial-path X6 (list->array kinds) #:nlambda 5))
                  '("a" "b" "c")))

  (test-case "a Cox status may be a boolean, #t for an event, as R's Surv reads it"
    (define times '(5.0 3.0 6.0 2.0 4.0 1.0))
    (define statuses '(1 0 1 1 0 1))
    (define events (map (lambda (s) (= s 1)) statuses))
    (define expected (cox-fit X6 times statuses #:lambda 0.05))
    (check-equal? (cox-fit X6 times events #:lambda 0.05) expected)
    (check-equal? (cox-fit X6 times (list->vector events) #:lambda 0.05) expected)
    (check-equal? (cox-fit X6 times (series events #:name "d") #:lambda 0.05) expected)
    (define table (list (cons "a" (list->vector (map car X6))) (cons "b" (list->vector (map cadr X6)))
                        (cons "t" (list->vector times)) (cons "d" (list->vector events))))
    (check-equal? (cox-fit table '("t" "d") #:predictors '("a" "b") #:lambda 0.05) expected)
    (check-equal? (cox-fit (table->polars table) '("t" "d") #:predictors '("a" "b") #:lambda 0.05)
                  expected)
    (check-equal? (glmnet-model->path (formula-fit (~ (surv t d) (+ a b)) table #:family 'cox
                                                   #:lambda 0.05))
                  (glmnet-model->path expected))
    (check-exn (blame-matching #rx"expected: a boolean, as the first element is"
                               #rx"the element at position 2 of")
               (lambda () (cox-fit X6 times '(#t #f 1 #t #f #t) #:lambda 0.05))))

  (test-case "fold ids may be a Polars series or a math array, as a response may"
    (define ids (folds 150 3))
    (define expected
      (logistic-cv iris+ "virginica" #:predictors iris-predictors #:fold-ids ids #:nlambda 5))
    (check-equal? (logistic-cv iris+ "virginica" #:predictors iris-predictors #:nlambda 5
                               #:fold-ids (series ids #:name "fold"))
                  expected)
    (check-equal? (logistic-cv iris+ "virginica" #:predictors iris-predictors #:nlambda 5
                               #:fold-ids (list->array ids))
                  expected)
    (check-equal? (formula-cv (~ virginica (+ Sepal.Length Sepal.Width Petal.Length Petal.Width))
                              iris+ #:family 'binomial #:nlambda 5
                              #:fold-ids (series ids #:name "fold"))
                  (formula-cv (~ virginica (+ Sepal.Length Sepal.Width Petal.Length Petal.Width))
                              iris+ #:family 'binomial #:nlambda 5 #:fold-ids ids))
    (check-exn (blame-matching #rx"cross-validation needs at least 3 folds"
                               #rx"the #:fold-ids argument")
               (lambda () (logistic-cv iris+ "virginica" #:predictors iris-predictors
                                       #:fold-ids (series (folds 150 2) #:name "fold")))))

  (test-case "unnamed Cox errors call the response times, and short statuses have no hint"
    (check-exn (error-matching #rx"^cox-fit: times has an element that is not finite")
               (lambda () (cox-fit X6 '(1.0 2.0 +inf.0 4.0 5.0 6.0) '(1 1 0 1 1 1) #:lambda 0.1)))
    (check-exn (lambda (e)
                 (and ((error-matching #rx"^cox-fit: statuses does not have one entry per row of X"
                                       #rx"length of statuses: 2") e)
                      (not (regexp-match? #rx"hint" (exn-message e)))))
               (lambda () (cox-fit X6 '(1.0 2.0 3.0 4.0 5.0 6.0) '(1 0) #:lambda 0.1)))
    (check-exn (error-matching #rx"^cox-fit: times does not have one entry per row of X"
                               #rx"hint: rows are observations")
               (lambda () (cox-fit X6 '(1.0 2.0) '(1 0 1 1 0 1) #:lambda 0.1))))

  ;; --- loading ---------------------------------------------------------------------------

  (test-case "(require glmnet), and fits and formulas from tables of labels, load neither Polars nor math/matrix"
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (namespace-require 'racket/base)
      (namespace-require 'glmnet)
      (eval '(define cells (list (cons "x" '(1 2 4 3 5 6)) (cons "z" '(2 1 3 5 4 6))
                                 (cons "k" '("a" "b" "a" "c" "c" "b"))
                                 (cons "t" '(5.0 3.0 6.0 2.0 4.0 1.0)) (cons "d" '(1 0 1 1 0 1)))))
      (eval '(multinomial-predict (multinomial-fit cells "k" #:predictors '("x" "z") #:lambda 0.1)
                                  (hash "x" '(3) "z" '(3))))
      (eval '(cox-linear-predictor (cox-fit cells '("t" "d") #:predictors '("x" "z") #:lambda 0.1)
                                   cells))
      (eval '(mgaussian-path cells '("x" "z") #:predictors '("t")))
      (eval '(predict (formula-fit (~ k x z) cells #:family 'multinomial #:lambda 0.1)
                      (hash "x" '(3) "z" '(3)) #:type 'class))
      (eval '(coef (formula-cv (~ (surv t d) x z) cells #:family 'cox #:fold-ids '(0 1 2 0 1 2)
                                 #:nlambda 5)))
      (for ([mod (in-list '(polars glmnet/data/polars math/array math/matrix glmnet/data/math
                                   typed/racket/base plot glmnet/plot))])
        (check-false (module-declared? mod #f) (format "~a is declared" mod))))))
