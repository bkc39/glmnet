#lang racket/base

;; The formula front end (#26): R-style formulas over tables, the named data
;; of data.rkt. `~` quotes a formula; `formula-fit`, `formula-path` and
;; `formula-cv` resolve it against a table into a named design matrix and the
;; response columns, call the matrix procedure of the family that #:family
;; names, and wrap the result in a `formula-model`. The model keeps its formula
;; and its predictor names, which gen:glmnet-model passes on, so that `coef` is
;; keyed by name and `predict` reads a table by name (core/model.rkt).

(module words racket/base
  (provide reserved)
  ;; The words of the formula language. A column with one of these names is
  ;; written as a string.
  (define reserved '(all surv + -)))

(require (for-syntax racket/base syntax/parse 'words)
         'words
         racket/contract
         racket/generic
         racket/list
         racket/match
         "model.rkt"
         "path.rkt"
         (submod "path.rkt" support)
         "cv.rkt"
         (only-in (submod "cv.rkt" support) write-cv)
         "elnet.rkt"
         "lognet.rkt"
         "multinomial.rkt"
         "cox.rkt"
         "poisson.rkt"
         "mgaussian.rkt"
         (only-in "../data.rkt" table? design-matrix-column-names design-matrix->columns)
         (only-in (submod "../data.rkt" support)
                  design-matrix-nrows column-name->string table-names select-table-columns))

(define family/c (or/c 'gaussian 'binomial 'multinomial 'poisson 'cox 'mgaussian))
(define measure/c (or/c #f 'mse 'deviance 'mae 'class 'auc 'C))

(provide
 ~
 (contract-out
  [formula-term/c flat-contract?]
  [formula-response/c flat-contract?]
  [make-formula (-> formula-response/c formula-term/c formula-term/c ... formula?)]
  [formula? (-> any/c boolean?)]
  [formula-response (-> formula? formula-response/c)]
  [formula-terms (-> formula? (listof formula-term/c))]
  [formula-predictor-names (-> formula? table? (listof string?))]
  [struct formula-model ([formula formula?]
                         [predictor-names (listof string?)]
                         [fit glmnet-model?])]
  [formula-fit
   (->* (formula? table? #:lambda (>=/c 0))
        (#:family family/c
         #:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        formula-model?)]
  [formula-path
   (->* (formula? table?)
        (#:family family/c
         #:lambda (or/c #f (and/c (listof (>=/c 0)) pair?))
         #:nlambda exact-positive-integer?
         #:lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1)))
         #:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        formula-model?)]
  [formula-cv
   (->* (formula? table?)
        (#:family family/c
         #:type-measure measure/c
         #:nfolds (and/c exact-integer? (>=/c 3))
         #:fold-ids (or/c #f (and/c (listof exact-nonnegative-integer?) pair?))
         #:grouped? boolean?
         #:lambda (or/c #f (and/c (listof (>=/c 0)) (property/c length (>=/c 2))))
         #:nlambda exact-positive-integer?
         #:lambda-min-ratio (or/c #f (and/c real? (>/c 0) (</c 1)))
         #:alpha (real-in 0 1)
         #:standardize? boolean?
         #:intercept? boolean?
         #:thresh (>/c 0)
         #:max-iters exact-positive-integer?)
        formula-model?)]))

;; --- formulas ------------------------------------------------------------------

(define (column-name? v)
  (or (string? v) (and (symbol? v) (not (memq v reserved)))))

(define (term? v)
  (match v
    ['all #t]
    [(? column-name?) #t]
    [(list '+ terms ..1) (andmap term? terms)]
    [(list '- term excluded ..1) (and (term? term) (andmap term? excluded))]
    [_ #f]))

(define (response? v)
  (match v
    [(? column-name?) #t]
    [(list 'surv (? column-name?) (? column-name?)) #t]
    [(list (? column-name?) ..1) #t]
    [_ #f]))

(define formula-term/c (flat-named-contract 'formula-term/c term?))
(define formula-response/c (flat-named-contract 'formula-response/c response?))

;; A formula prints as the `~` form that makes it.
(struct formula (response terms)
  #:transparent
  #:property prop:custom-write
  (lambda (f port mode)
    (write-string "(~ " port)
    (write (formula-response f) port)
    (for ([t (in-list (formula-terms f))])
      (write-string " " port)
      (write t port))
    (write-string ")" port)))

(define (make-formula response . terms)
  (formula response terms))

(begin-for-syntax
  (define-syntax-class column
    #:description "a column name"
    (pattern name:id #:when (not (memq (syntax-e #'name) reserved)))
    (pattern name:str))

  (define-syntax-class term
    #:description "a predictor term: a column name, all, (+ term ...) or (- term term ...)"
    #:datum-literals (all + -)
    (pattern all)
    (pattern _:column)
    (pattern (+ _:term ...+))
    (pattern (- _:term _:term ...+)))

  (define-syntax-class response
    #:description "a response: a column name, (surv time status) or (column ...)"
    #:datum-literals (surv)
    (pattern _:column)
    (pattern (surv _:column _:column))
    (pattern (_:column ...+))))

(define-syntax (~ stx)
  (syntax-parse stx
    [(_ response:response term:term ...+)
     #'(make-formula 'response 'term ...)]))

;; The response columns of formula f, as strings.
(define (response-columns f)
  (match (formula-response f)
    [(list 'surv time status) (list (column-name->string time) (column-name->string status))]
    [(? list? names) (map column-name->string names)]
    [name (list (column-name->string name))]))

;; The predictor columns that f's terms select from the table's `columns`, in
;; order and each once. A term that names a column adds it, `all` adds every
;; column that is not a response, `+` joins its terms, and `-` removes from
;; its first term the columns of the others. A response column may be removed
;; but not added.
(define (resolve-predictors who f columns)
  (define responses (response-columns f))
  (define known (for/hash ([c (in-list columns)]) (values c #t)))
  (define (check-column name)
    (unless (hash-ref known name #f)
      (raise-arguments-error who "the table has no column with this name"
                             "column" name "formula" f "columns of the table" columns)))
  (for-each check-column responses)
  (define dup (check-duplicates responses))
  (when dup
    (raise-arguments-error who "the response names a column twice" "column" dup "formula" f))
  (define (resolve t excluding?)
    (match t
      ['all (filter (lambda (c) (not (member c responses))) columns)]
      [(list '+ terms ...) (append-map (lambda (t) (resolve t excluding?)) terms)]
      [(list '- term excluded ...)
       (define drop
         (for*/hash ([t (in-list excluded)] [c (in-list (resolve t #t))])
           (values c #t)))
       (filter (lambda (c) (not (hash-ref drop c #f))) (resolve term excluding?))]
      [name
       (define column (column-name->string name))
       (check-column column)
       (when (and (not excluding?) (member column responses))
         (raise-arguments-error who "a response column is listed as a predictor"
                                "column" column "formula" f))
       (list column)]))
  (define predictors
    (remove-duplicates (append-map (lambda (t) (resolve t #f)) (formula-terms f))))
  (when (null? predictors)
    (raise-arguments-error who "the formula selects no predictors" "formula" f))
  predictors)

(define (formula-predictor-names f table)
  (define who 'formula-predictor-names)
  (resolve-predictors who f (table-names table who)))

;; --- formula models ------------------------------------------------------------

;; formula        : the formula the model was fitted from
;; predictor-names : the predictor columns, in the order of the coefficients
;; fit            : the family's result: a single fit, a glmnet-path or a
;;                  glmnet-cv
(struct formula-model (formula predictor-names fit)
  #:transparent
  #:property prop:custom-write
  (lambda (m port mode)
    (define fit (formula-model-fit m))
    (define call (format "~s" (formula-model-formula m)))
    (cond
      [(glmnet-path? fit) (write-path fit port call)]
      [(glmnet-cv? fit) (write-cv fit port call)]
      [else (write-point (glmnet-model->path fit) port call)]))
  #:methods gen:glmnet-model
  [(define/generic ->path glmnet-model->path)
   (define/generic default-lambda glmnet-model-default-lambda)
   (define/generic named-lambda glmnet-model-named-lambda)
   (define/generic dev-ratio deviance-ratio)
   (define (glmnet-model->path m) (->path (formula-model-fit m)))
   (define (glmnet-model-default-lambda m) (default-lambda (formula-model-fit m)))
   (define (glmnet-model-named-lambda m name) (named-lambda (formula-model-fit m) name))
   (define (glmnet-model-predictor-names m) (formula-model-predictor-names m))
   (define (glmnet-model-response-names m) (response-columns (formula-model-formula m)))
   (define (deviance-ratio m) (dev-ratio (formula-model-fit m)))])

;; --- families --------------------------------------------------------------------

;; fit, path, cv : the family's matrix procedures
;; measures      : its type measures, the default first
;; intercept?    : whether its procedures take #:intercept?
(struct family (fit path cv measures intercept?))

(define families
  (hash 'gaussian (family elnet-fit elnet-path elnet-cv '(mse deviance mae) #t)
        'binomial (family logistic-fit logistic-path logistic-cv
                          '(deviance class auc mse mae) #t)
        'multinomial (family multinomial-fit multinomial-path multinomial-cv
                             '(deviance class mse mae) #t)
        'poisson (family poisson-fit poisson-path poisson-cv '(deviance mse mae) #t)
        'cox (family cox-fit cox-path cox-cv '(deviance C) #f)
        'mgaussian (family mgaussian-fit mgaussian-path mgaussian-cv '(mse deviance mae) #t)))

;; The response form the family needs: a (surv time status) response for Cox,
;; one or more columns for the multi-response family, one column otherwise.
(define (check-response-form who f family-name)
  (define form
    (match (formula-response f)
      [(list 'surv _ _) 'surv]
      [(? list?) 'columns]
      [_ 'column]))
  (define problem
    (case family-name
      [(cox) (and (not (eq? form 'surv)) "the Cox family needs a (surv time status) response")]
      [(mgaussian) (and (eq? form 'surv) "a (surv time status) response is for the Cox family")]
      [else
       (case form
         [(surv) "a (surv time status) response is for the Cox family"]
         [(columns) "a response of several columns is for the mgaussian family"]
         [else #f])]))
  (when problem
    (raise-arguments-error who problem "family" family-name "formula" f)))

;; Checks that every value of a response column satisfies `ok?`.
(define (check-values who column values ok? problem)
  (for ([v (in-list values)]
        [i (in-naturals)]
        #:unless (ok? v))
    (raise-arguments-error who problem "column" column "row" i "value" v)))

(define (zero-or-one? v) (or (= v 0.0) (= v 1.0)))

;; The design matrix of predictors and the response arguments of the family's
;; procedures, from the table: R's model.frame and model.response.
(define (model-frame who f table family-name)
  (check-response-form who f family-name)
  (define columns (table-names table who))
  (define x (select-table-columns table (resolve-predictors who f columns) who))
  (define responses (response-columns f))
  (define y (select-table-columns table responses who))
  (unless (= (design-matrix-nrows y) (design-matrix-nrows x))
    (raise-arguments-error who "the response and the predictors have different lengths"
                           "response rows" (design-matrix-nrows y)
                           "predictor rows" (design-matrix-nrows x)))
  (define ys (design-matrix->columns y))
  (define y1 (car ys))
  (define column (car responses))
  (values
   x
   (case family-name
     [(gaussian) (list y1)]
     [(binomial)
      (check-values who column y1 zero-or-one? "a binomial response must be 0 or 1")
      (list y1)]
     [(multinomial)
      (check-values who column y1 (lambda (v) (and (integer? v) (>= v 0.0)))
                    "a multinomial response must be a class label 0, 1, ...")
      (list (map inexact->exact y1))]
     [(poisson)
      (check-values who column y1 (lambda (v) (>= v 0.0)) "a Poisson response must be non-negative")
      (list y1)]
     [(cox)
      (check-values who column y1 positive? "a survival time must be positive")
      (check-values who (cadr responses) (cadr ys) zero-or-one? "an event status must be 0 or 1")
      (list y1 (cadr ys))]
     [(mgaussian) (list y)])))

;; Fits the formula with the family procedure that `select` picks, passing
;; `options`, an association list from keyword to value, as keyword arguments.
(define (fit-formula who select f table family-name options)
  (define spec (hash-ref families family-name))
  (define-values (x responses) (model-frame who f table family-name))
  (define kws
    (sort (if (family-intercept? spec)
              options
              (filter (lambda (kw) (not (eq? (car kw) '#:intercept?))) options))
          keyword<? #:key car))
  (formula-model f (design-matrix-column-names x)
                 (keyword-apply (select spec) (map car kws) (map cdr kws) (cons x responses))))

(define (formula-fit f table
                     #:family [family-name 'gaussian]
                     #:lambda lambda
                     #:alpha [alpha 1.0]
                     #:standardize? [standardize? #t]
                     #:intercept? [intercept? #t]
                     #:thresh [thresh 1e-7]
                     #:max-iters [max-iters 100000])
  (fit-formula 'formula-fit family-fit f table family-name
               (list (cons '#:lambda lambda)
                     (cons '#:alpha alpha)
                     (cons '#:standardize? standardize?)
                     (cons '#:intercept? intercept?)
                     (cons '#:thresh thresh)
                     (cons '#:max-iters max-iters))))

(define (formula-path f table
                      #:family [family-name 'gaussian]
                      #:lambda [lambda #f]
                      #:nlambda [nlambda 100]
                      #:lambda-min-ratio [lambda-min-ratio #f]
                      #:alpha [alpha 1.0]
                      #:standardize? [standardize? #t]
                      #:intercept? [intercept? #t]
                      #:thresh [thresh 1e-7]
                      #:max-iters [max-iters 100000])
  (fit-formula 'formula-path family-path f table family-name
               (list (cons '#:lambda lambda)
                     (cons '#:nlambda nlambda)
                     (cons '#:lambda-min-ratio lambda-min-ratio)
                     (cons '#:alpha alpha)
                     (cons '#:standardize? standardize?)
                     (cons '#:intercept? intercept?)
                     (cons '#:thresh thresh)
                     (cons '#:max-iters max-iters))))

(define (formula-cv f table
                    #:family [family-name 'gaussian]
                    #:type-measure [measure #f]
                    #:nfolds [nfolds 10]
                    #:fold-ids [fold-ids #f]
                    #:grouped? [grouped? #t]
                    #:lambda [lambda #f]
                    #:nlambda [nlambda 100]
                    #:lambda-min-ratio [lambda-min-ratio #f]
                    #:alpha [alpha 1.0]
                    #:standardize? [standardize? #t]
                    #:intercept? [intercept? #t]
                    #:thresh [thresh 1e-7]
                    #:max-iters [max-iters 100000])
  (define who 'formula-cv)
  (define measures (family-measures (hash-ref families family-name)))
  (when (and measure (not (memq measure measures)))
    (raise-arguments-error who "the family has no such type measure"
                           "type measure" measure "family" family-name "type measures" measures))
  (fit-formula who family-cv f table family-name
               (list (cons '#:type-measure (or measure (car measures)))
                     (cons '#:nfolds nfolds)
                     (cons '#:fold-ids fold-ids)
                     (cons '#:grouped? grouped?)
                     (cons '#:lambda lambda)
                     (cons '#:nlambda nlambda)
                     (cons '#:lambda-min-ratio lambda-min-ratio)
                     (cons '#:alpha alpha)
                     (cons '#:standardize? standardize?)
                     (cons '#:intercept? intercept?)
                     (cons '#:thresh thresh)
                     (cons '#:max-iters max-iters))))
