#lang racket/base

;; Formulas on dataframes directly (#75): for every family, formula-fit,
;; formula-path and formula-cv on a Polars dataframe give what they give on
;; (polars->table df), and so do coef, deviance-ratio and predict of every type
;; on new data as a dataframe; only the columns a formula uses are read, and
;; bad data is an error naming its column.

(module+ test
  (require rackunit
           racket/list
           racket/match
           (only-in racket/contract exn:fail:contract:blame?)
           (only-in polars dataframe series cast head polars-null)
           glmnet
           glmnet/data/csv
           glmnet/data/polars
           (only-in glmnet/datasets mtcars iris))

  ;; --- data -----------------------------------------------------------------------

  (define (dataset name)
    (csv-file->table (collection-file-path (string-append name ".csv") "glmnet" "datasets")))

  (define (column table name) (vector->list (cdr (assoc name table))))
  (define (folds n k) (for/list ([i (in-range n)]) (modulo i k)))

  ;; mtcars with a text column of ids, first.
  (define cars
    (cons (cons "id" (for/vector ([i (in-range 32)]) (format "car ~a" i))) mtcars))
  ;; iris with a two-level column of strings: whether the flower is a virginica.
  (define iris+
    (cons (cons "virginica" (for/vector ([s (in-list (column iris "Species"))])
                              (if (equal? s "virginica") "yes" "no")))
          iris))

  ;; The first n rows of a table, without the columns `drop`, its columns in
  ;; reverse order: new data that a formula model reads by name.
  (define (new-data table drop n)
    (reverse (for/list ([entry (in-list table)] #:unless (member (car entry) drop))
               (cons (car entry) (for/vector ([x (in-vector (cdr entry))] [i (in-range n)]) x)))))

  ;; The first n rows of a table's columns `names`, in that order.
  (define (take-rows table n names)
    (for/list ([name (in-list names)])
      (cons name (for/vector ([x (in-vector (cdr (assoc name table)))] [i (in-range n)]) x))))

  ;; --- the families --------------------------------------------------------------------

  ;; A family's case: a formula, its family, the table it is fitted to, its
  ;; response columns, which new data does without, and its CV folds.
  (struct formula-case (label f family table responses fold-ids))

  (define cases
    (list
     (formula-case "gaussian, transforms, interactions and factors"
                   (mpg . ~ . wt * hp + (log disp) + (factor cyl)) 'gaussian cars '("mpg")
                   (folds 32 4))
     (formula-case "gaussian, all but a text column" (~ mpg (- all id)) 'gaussian cars '("mpg")
                   (folds 32 4))
     (formula-case "binomial, 0 and 1" (am . ~ . wt + hp) 'binomial cars '("am") (folds 32 4))
     (formula-case "binomial, string labels" (virginica . ~ . Sepal.Length + Petal.Width)
                   'binomial iris+ '("virginica") (folds 150 3))
     (formula-case "multinomial, all" (~ Species all) 'multinomial iris '("Species")
                   (folds 150 3))
     (formula-case "poisson" (~ y V1 V2 V3 V4 V5) 'poisson (dataset "PoissonExample") '("y")
                   (folds 500 3))
     (formula-case "cox" (~ (surv time status) V1 V2 V3 V4 V5) 'cox (dataset "CoxExample")
                   '("time" "status") (folds 1000 3))
     (formula-case "mgaussian" (~ (y1 y2) V1 V2 V3 V4 V5) 'mgaussian
                   (dataset "MultiGaussianExample") '("y1" "y2") (folds 100 4))))

  ;; The model's predictions of each type its family has, at its default λ.
  (define (predictions model X)
    (case (glmnet-path-family (glmnet-model->path model))
      [(binomial multinomial)
       (list (predict model X) (predict model X #:type 'response) (predict model X #:type 'class))]
      [else (list (predict model X) (predict model X #:type 'response))]))

  (define (check-same-model direct explicit new-frame new-table)
    (check-equal? direct explicit)
    (check-equal? (coef direct) (coef explicit))
    (check-equal? (deviance-ratio direct) (deviance-ratio explicit))
    (check-equal? (predictions direct new-frame) (predictions explicit new-table)))

  (for ([c (in-list cases)])
    (match-define (formula-case label f family table responses fold-ids) c)
    (define frame (table->polars table))
    (define via-table (polars->table frame))
    (define new-table (new-data table responses 3))
    (define new-frame (table->polars new-table))
    (define procedures
      (list (cons 'formula-fit (lambda (data) (formula-fit f data #:family family #:lambda 0.05)))
            (cons 'formula-path (lambda (data) (formula-path f data #:family family #:nlambda 10)))
            (cons 'formula-cv (lambda (data) (formula-cv f data #:family family #:nlambda 10
                                                         #:fold-ids fold-ids)))))
    (for ([entry (in-list procedures)])
      (test-case (format "~a: ~a from a dataframe" (car entry) label)
        (check-same-model ((cdr entry) frame) ((cdr entry) via-table) new-frame new-table)
        (check-equal? ((cdr entry) frame) ((cdr entry) table)))))

  (test-case "a formula of columns fits as the family's procedure on the same named data"
    (define frame (table->polars cars))
    (check-equal? (formula-model-fit (formula-fit (~ mpg wt hp) frame #:lambda 0.1))
                  (elnet-fit frame "mpg" #:predictors '("wt" "hp") #:lambda 0.1))
    (define flowers (table->polars iris+))
    (check-equal? (formula-model-fit (formula-path (virginica . ~ . Sepal.Length + Petal.Width)
                                                   flowers #:family 'binomial #:nlambda 10))
                  (logistic-path flowers "virginica" #:predictors '("Sepal.Length" "Petal.Width")
                                 #:nlambda 10))
    (define cox (table->polars (dataset "CoxExample")))
    (check-equal? (formula-model-fit (formula-fit (~ (surv time status) V1 V2) cox
                                                  #:family 'cox #:lambda 0.05))
                  (cox-fit cox '("time" "status") #:predictors '("V1" "V2") #:lambda 0.05))
    (define mg (table->polars (dataset "MultiGaussianExample")))
    (check-equal? (formula-model-fit (formula-fit (~ (y1 y2) V1 V2) mg #:family 'mgaussian
                                                  #:lambda 0.05))
                  (mgaussian-fit mg '("y1" "y2") #:predictors '("V1" "V2") #:lambda 0.05)))

  (test-case "an unpenalized Gaussian fit has ols's default threshold, whichever procedure fits it"
    (define frame (table->polars cars))
    (define ols-fit (ols frame "mpg" #:predictors '("wt" "hp")))
    (define model (formula-fit (~ mpg (+ wt hp)) frame #:lambda 0))
    (check-equal? (predict model frame) (predict ols-fit frame))
    (check-equal? (formula-model-fit model) ols-fit)
    (for ([fitter (in-list (list elnet-fit ridge lasso))])
      (check-equal? (fitter frame "mpg" #:predictors '("wt" "hp") #:lambda 0) ols-fit
                    (symbol->string (object-name fitter))))
    (check-equal? (elastic-net frame "mpg" #:predictors '("wt" "hp") #:alpha 0.5 #:lambda 0)
                  ols-fit)
    (check-equal? (ols frame "mpg" #:predictors '("wt" "hp") #:thresh 1e-7)
                  (elnet-fit frame "mpg" #:predictors '("wt" "hp") #:lambda 0 #:thresh 1e-7))
    (check-not-equal? (formula-fit (~ mpg (+ wt hp)) frame #:lambda 0 #:thresh 1e-7) model)
    (check-equal? (formula-model-fit (formula-fit (~ mpg (+ wt hp)) frame #:lambda 0.5))
                  (lasso frame "mpg" #:predictors '("wt" "hp") #:lambda 0.5 #:thresh 1e-7)))

  ;; --- what is read --------------------------------------------------------------------

  ;; mtcars as a dataframe with a date column and a column with a missing
  ;; value, which no formula below reads.
  (define cars-frame
    (dataframe (append (for/list ([entry (in-list cars)]) (series (cdr entry) #:name (car entry)))
                       (list (cast (series (range 32) #:name "bought" #:dtype 'int32) 'date)
                             (series (cons polars-null (range 31)) #:name "owners")))))

  (test-case "only the columns a formula uses are read"
    (define f (mpg . ~ . wt + (factor cyl)))
    (define m (formula-fit f cars-frame #:lambda 0.1))
    (check-equal? m (formula-fit f cars #:lambda 0.1))
    (check-equal? (formula-predictor-names (~ mpg (- all id bought owners)) cars-frame)
                  (formula-predictor-names (~ mpg (- all id)) cars))
    (check-equal? (formula-design-matrix (~ mpg (: wt hp)) cars-frame)
                  (formula-design-matrix (~ mpg (: wt hp)) cars))
    (check-equal? (predict m cars-frame) (predict m cars)))

  (test-case "all stands for every column but the response, text columns included, as in a table"
    (define small (take-rows cars 8 '("id" "mpg" "wt" "cyl")))
    (check-equal? (formula-predictor-names (~ mpg all) (table->polars small))
                  (formula-predictor-names (~ mpg all) small))
    (check-equal? (take (formula-predictor-names (~ mpg all) (table->polars small)) 2)
                  '("idcar 1" "idcar 2")))

  (test-case "categorical and boolean columns are factors, and categorical responses classes"
    (define frame
      (dataframe (list (series (column iris "Petal.Width") #:name "width")
                       (cast (series (column iris "Species") #:name "species") 'categorical)
                       (series (for/list ([w (in-list (column iris "Sepal.Width"))]) (> w 3.0))
                               #:name "wide"))))
    (define table
      (list (cons "width" (column iris "Petal.Width"))
            (cons "species" (map string->symbol (column iris "Species")))
            (cons "wide" (for/list ([w (in-list (column iris "Sepal.Width"))]) (> w 3.0)))))
    (define m (formula-fit (~ species width wide) frame #:family 'multinomial #:lambda 0.05))
    (check-equal? m (formula-fit (~ species width wide) table #:family 'multinomial #:lambda 0.05))
    (check-equal? (map car (coef m)) '("setosa" "versicolor" "virginica"))
    (check-equal? (predict m frame #:type 'class) (predict m table #:type 'class))
    (define g (formula-fit (~ width species wide) frame #:lambda 0.01))
    (check-equal? (formula-model-predictor-names g) '("speciesversicolor" "speciesvirginica" "wideTRUE"))
    (check-equal? g (formula-fit (~ width species wide) table #:lambda 0.01)))


  ;; --- errors -------------------------------------------------------------------------------

  (define ((error-matching . patterns) e)
    (and (exn:fail:contract? e)
         (not (exn:fail:contract:blame? e))
         (for/and ([p (in-list patterns)])
           (regexp-match? p (exn-message e)))))

  (define ((blame-matching . patterns) e)
    (and (exn:fail:contract:blame? e)
         (for/and ([p (in-list (cons #rx"blaming: [^\n]*formula-direct-test[.]rkt" patterns))])
           (regexp-match? p (exn-message e)))))

  (test-case "errors name the column"
    (check-exn (error-matching #rx"^formula-fit: the dataframe has no column with this name"
                               #rx"column: \"weight\"" #rx"columns of the dataframe: '\\(\"id\"")
               (lambda () (formula-fit (~ mpg weight) cars-frame #:lambda 0.1)))
    (check-exn (error-matching #rx"^formula-path: the dataframe has a missing value"
                               #rx"column: \"owners\"\n  row: 0")
               (lambda () (formula-path (~ mpg wt owners) cars-frame)))
    (check-exn (error-matching #rx"^formula-fit: the dataframe has a column whose dtype is not numeric, boolean, string, categorical or enum"
                               #rx"column: \"bought\"\n  dtype: 'date")
               (lambda () (formula-fit (~ mpg (- all id)) cars-frame #:lambda 0.1)))
    (check-exn (error-matching #rx"^formula-cv: the response column \"id\" is not numeric\n  dtype: 'string$")
               (lambda () (formula-cv (~ id wt) cars-frame #:fold-ids (folds 32 4))))
    (check-exn (error-matching #rx"^formula-fit: the response column \"id\" is not numeric\n  dtype: 'string$")
               (lambda () (formula-fit (~ (id gear) wt) cars-frame #:family 'mgaussian #:lambda 0.1)))
    (check-exn (error-matching #rx"^formula-fit: the response column \"id\" is not numeric or boolean\n  dtype: 'string$")
               (lambda () (formula-fit (~ (surv mpg id) wt) cars-frame #:family 'cox #:lambda 0.1)))
    (check-exn (error-matching #rx"^formula-fit: the response column \"id\" is not numeric\n  dtype: 'string$")
               (lambda () (formula-fit (~ (surv id am) wt) cars-frame #:family 'cox #:lambda 0.1)))
    (check-exn (error-matching #rx"^formula-cv: fold-ids does not have one entry per row of the dataframe"
                               #rx"rows of the dataframe: 32")
               (lambda () (formula-cv (~ mpg wt hp) cars-frame #:fold-ids '(0 1 2))))
    (check-exn (error-matching #rx"^formula-fit: a binomial response has two classes"
                               #rx"column: \"id\"")
               (lambda () (formula-fit (~ id wt) cars-frame #:family 'binomial #:lambda 0.1)))
    (check-exn (error-matching #rx"^formula-fit: the dataframe has no rows")
               (lambda () (formula-fit (~ mpg wt) (head cars-frame 0) #:lambda 0.1)))
    (check-exn (blame-matching #rx"expected: \\(or/c table[?] named-data[?]\\)" #rx"given: 5")
               (lambda () (formula-fit (~ mpg wt) 5 #:lambda 0.1))))

  (test-case "an empty dataframe is read as an empty table: no rows only for a formula that reads"
    (define empty-frame (head cars-frame 0))
    (define empty-table (list (cons "mpg" #()) (cons "wt" #())))
    (check-equal? (formula-predictor-names (~ mpg 1) empty-frame) '())
    (check-equal? (formula-predictor-names (~ mpg 1) empty-table) '())
    (for ([data (list empty-frame empty-table)])
      (check-exn (error-matching #rx"^formula-design-matrix: the formula has no predictors")
                 (lambda () (formula-design-matrix (~ mpg 1) data))))
    (check-exn (error-matching #rx"^formula-predictor-names: the dataframe has no rows$")
               (lambda () (formula-predictor-names (~ mpg wt) empty-frame)))
    (check-exn (error-matching #rx"^formula-predictor-names: the table has a column with no rows")
               (lambda () (formula-predictor-names (~ mpg wt) empty-table)))
    (check-exn (error-matching #rx"^formula-design-matrix: the dataframe has no rows$")
               (lambda () (formula-design-matrix (~ mpg wt) empty-frame)))
    (check-exn (error-matching #rx"^formula-fit: the dataframe has no rows$")
               (lambda () (formula-fit (~ mpg 1) empty-frame #:lambda 0.1))))

  (test-case "a table's response that is not numbers is an error of the response column"
    (define message
      #rx"^formula-fit: the response column \"id\" has an element that is not a real number\n  row: 0\n  element: \"car 0\"$")
    (check-exn (error-matching message) (lambda () (formula-fit (~ id wt) cars #:lambda 0.1)))
    (check-exn (error-matching message)
               (lambda () (formula-fit (~ (id gear) wt) cars #:family 'mgaussian #:lambda 0.1)))
    (check-exn (error-matching message)
               (lambda () (formula-fit (~ (surv mpg id) wt) cars #:family 'cox #:lambda 0.1))))

  (test-case "predict reads the columns the terms read, by name"
    (define m (formula-fit (mpg . ~ . wt * hp + (factor cyl)) cars-frame #:lambda 0.1))
    (define expected (predict m (list (cons "cyl" '(8 4)) (cons "hp" '(200 90)) (cons "wt" '(3.5 2.2)))))
    (check-equal? (predict m (dataframe (list (series '(8 4) #:name "cyl")
                                              (series '("a" "b") #:name "id")
                                              (series '(3.5 2.2) #:name "wt")
                                              (series '(200 90) #:name "hp"))))
                  expected)
    (check-exn (error-matching #rx"^predict: the dataframe has no column with this name"
                               #rx"column: \"hp\"")
               (lambda () (predict m (dataframe (list (series '(8) #:name "cyl")
                                                      (series '(3.5) #:name "wt"))))))
    (check-exn (error-matching #rx"^predict: the dataframe has no columns with these names"
                               #rx"columns: '\\(\"wt\" \"hp\"\\)")
               (lambda () (predict m (dataframe (list (series '(8) #:name "cyl"))))))
    (check-exn (error-matching #rx"^predict: a factor has levels that the model was not fitted with"
                               #rx"factor: \"\\(factor cyl\\)\"" #rx"new levels: '\\(\"5\"\\)")
               (lambda () (predict m (dataframe (list (series '(5) #:name "cyl")
                                                      (series '(3.5) #:name "wt")
                                                      (series '(100) #:name "hp"))))))
    (check-exn (error-matching #rx"^predict: the dataframe has a missing value"
                               #rx"column: \"wt\"\n  row: 1")
               (lambda () (predict m (dataframe (list (series '(8 4) #:name "cyl")
                                                      (series (list 3.5 polars-null) #:name "wt")
                                                      (series '(100 90) #:name "hp"))))))
    (check-exn (error-matching #rx"^predict: the model's predictors are named, so X must be a table or a dataframe")
               (lambda () (predict m '((3.5 200 8)))))))
