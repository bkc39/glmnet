#lang racket/base

;; The formula front end (#26, #53): R-style formulas over tables, the named
;; data of data.rkt. `~` quotes a formula and checks its grammar where it is
;; written; `formula-fit`, `formula-path` and `formula-cv` expand its terms
;; against a table (terms.rkt, R's terms() and model.matrix()), call the matrix
;; procedure of the family that #:family names, and wrap the result in a
;; `formula-model`. The model keeps its formula, its predictor names and its
;; expanded terms, so that `coef` is keyed by name and `predict` rebuilds the
;; design matrix from a new table (core/model.rkt).

(require (for-syntax racket/base syntax/parse (submod "terms.rkt" words))
         racket/contract
         racket/generic
         racket/list
         racket/match
         "terms.rkt"
         "model.rkt"
         (only-in (submod "model.rkt" support) prop:predictor-matrix)
         "path.rkt"
         (submod "path.rkt" support)
         "cv.rkt"
         (only-in (submod "cv.rkt" support)
                  nfolds/c fold-ids/c cv-lambda-sequence/c write-cv)
         "elnet.rkt"
         "lognet.rkt"
         "multinomial.rkt"
         "cox.rkt"
         "poisson.rkt"
         "mgaussian.rkt"
         (only-in "../data.rkt" table? design-matrix? design-matrix-column-names
                  design-matrix->columns)
         (only-in (submod "../data.rkt" support)
                  design-matrix-nrows column-name->string table-names select-table-columns))

(define family/c (or/c 'gaussian 'binomial 'multinomial 'poisson 'cox 'mgaussian))

;; A formula procedure's #:family, 'gaussian when it is not given, for the
;; contracts that depend on it.
(define (family-argument family)
  (if (unsupplied-arg? family) 'gaussian family))

(provide
 ~
 (contract-out
  [formula-term/c flat-contract?]
  [formula-rhs/c flat-contract?]
  [formula-response/c flat-contract?]
  [make-formula (->* (formula-response/c) () #:rest formula-rhs/c formula?)]
  [formula? (-> any/c boolean?)]
  [formula-response (-> formula? formula-response/c)]
  [formula-terms (-> formula? formula-rhs/c)]
  [formula-predictor-names (-> formula? table? (listof string?))]
  [formula-design-matrix (-> formula? table? design-matrix?)]
  [formula-model? (-> any/c boolean?)]
  [formula-model-formula (-> formula-model? formula?)]
  [formula-model-predictor-names (-> formula-model? (listof string?))]
  [formula-model-fit (-> formula-model? glmnet-model?)]
  [formula-fit
   (->i ([f (family) (formula-for/c (family-argument family))]
         [table table?]
         #:lambda [lambda (>=/c 0)])
        (#:family [family family/c]
         #:alpha [alpha (real-in 0 1)]
         #:standardize? [standardize? boolean?]
         #:intercept? [intercept? (f family) (intercept-for/c f (family-argument family))]
         #:thresh [thresh (>/c 0)]
         #:max-iters [max-iters exact-positive-integer?])
        [result formula-model?])]
  [formula-path
   (->i ([f (family) (formula-for/c (family-argument family))]
         [table table?])
        (#:family [family family/c]
         #:lambda [lambda lambda-sequence/c]
         #:nlambda [nlambda exact-positive-integer?]
         #:lambda-min-ratio [lambda-min-ratio lambda-min-ratio/c]
         #:alpha [alpha (real-in 0 1)]
         #:standardize? [standardize? boolean?]
         #:intercept? [intercept? (f family) (intercept-for/c f (family-argument family))]
         #:thresh [thresh (>/c 0)]
         #:max-iters [max-iters exact-positive-integer?])
        [result formula-model?])]
  [formula-cv
   (->i ([f (family) (formula-for/c (family-argument family))]
         [table table?])
        (#:family [family family/c]
         #:type-measure [type-measure (family) (type-measure/c (family-argument family))]
         #:nfolds [nfolds nfolds/c]
         #:fold-ids [fold-ids fold-ids/c]
         #:grouped? [grouped? boolean?]
         #:lambda [lambda cv-lambda-sequence/c]
         #:nlambda [nlambda exact-positive-integer?]
         #:lambda-min-ratio [lambda-min-ratio lambda-min-ratio/c]
         #:alpha [alpha (real-in 0 1)]
         #:standardize? [standardize? boolean?]
         #:intercept? [intercept? (f family) (intercept-for/c f (family-argument family))]
         #:thresh [thresh (>/c 0)]
         #:max-iters [max-iters exact-positive-integer?])
        [result formula-model?])]))

(define-logger glmnet)

;; --- formulas ------------------------------------------------------------------

(define (response? v)
  (match v
    [(? column-name?) #t]
    [(list 'surv (? column-name?) (? column-name?)) #t]
    [(list (? column-name?) ..1) #t]
    [_ #f]))

(define formula-term/c (flat-named-contract 'formula-term/c term?))
(define formula-rhs/c (flat-named-contract 'formula-rhs/c rhs?))
(define formula-response/c (flat-named-contract 'formula-response/c response?))

;; A formula prints as the `~` form that makes it: `terms` is its right-hand
;; side as written, terms and the infix operators between them.
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

(define (make-formula response . rhs)
  (formula response rhs))

;; The grammar of terms.rkt's `term?` and `rhs?`, checked where the formula is
;; written. A group's kind is decided by its shape before it is parsed, with
;; ~fail, whose failures rank below those inside the group, so that an error
;; points at the innermost form that is wrong.
(begin-for-syntax
  (define (operator-id? stx)
    (and (identifier? stx) (memq (syntax-e stx) operators) #t))
  (define (reserved-id? stx)
    (and (identifier? stx) (memq (syntax-e stx) reserved) #t))
  (define (any-operator? stx)
    (ormap operator-id? (syntax->list stx)))
  (define (infix-group? stx)
    (define elements (syntax->list stx))
    (and (pair? elements) (ormap operator-id? (cdr elements))))
  (define (group-shape? stx)
    (define elements (syntax->list stx))
    (and (pair? elements)
         (or (identifier? (car elements)) (infix-group? stx))))

  (define-syntax-class column
    #:description "a column name"
    (pattern name:id
             #:fail-when (and (reserved-id? #'name) #'name)
             (format "~a is a word of the formula language; write a column with this name as a string, ~s"
                     (syntax-e #'name) (symbol->string (syntax-e #'name)))
             #:fail-when (and (glued-operator? (syntax-e #'name)) #'name)
             (format "~a reads as one name; put spaces around an operator, as in wt : hp, or write a column with this name as a string, ~s"
                     (syntax-e #'name) (symbol->string (syntax-e #'name))))
    (pattern name:str))

  (define-syntax-class operator
    #:description "a formula operator"
    (pattern o:id #:when (operator-id? #'o)))

  (define-syntax-class sign
    #:description "a sign, + or -"
    (pattern o:id #:when (memq (syntax-e #'o) '(+ -))))

  (define-syntax-class infix-operator
    #:description "an infix operator (+, -, *, : or ^) between two terms"
    (pattern o:id #:when (memq (syntax-e #'o) '(+ - * :))))

  (define-syntax-class power
    #:description "a power, an exact integer of at least 2"
    (pattern n #:when (let ([v (syntax-e #'n)]) (and (exact-integer? v) (>= v 2)))))

  (define-splicing-syntax-class infix-step
    #:description "an infix operator and the term after it"
    (pattern (~seq (~datum ^) _:power))
    (pattern (~seq _:infix-operator _:term)))

  (define-syntax-class term
    #:description "a term: a column, all, 1, 0, or a group such as (* x z) or (x + z)"
    (pattern (~datum all))
    (pattern (~and n (~fail #:unless (memv (syntax-e #'n) '(0 1)))))
    (pattern (~and _:id (~not _:operator) _:column))
    (pattern _:str)
    (pattern (~and g (~fail #:unless (infix-group? #'g))
                   ((~optional _:sign) _:term _:infix-step ...+)))
    (pattern ((~datum +) _:term ...+))
    (pattern ((~datum -) _:term ...+))
    (pattern ((~datum *) _:term _:term ...+))
    (pattern ((~datum :) _:term _:term ...+))
    (pattern ((~datum ^) _:term _:power))
    (pattern (~and g (f:id _ ...) (~fail #:when (or (reserved-id? #'f) (infix-group? #'g))))
             #:fail-when #'g
             (format "~s is a function call, a transform, which the formula language does not support yet"
                     (syntax->datum #'g)))
    (pattern (~and g (_ ...) (~fail #:when (group-shape? #'g)))
             #:fail-when #'g
             (format "~s is not a term: a group of terms starts with an operator, as (+ x z) does, or has operators between its terms, as (x + z) does"
                     (syntax->datum #'g))))

  (define-syntax-class right-hand-side
    #:description "the right-hand side of a formula"
    (pattern (~and rhs (~fail #:when (any-operator? #'rhs)) (_:term ...)))
    (pattern (~and rhs (~fail #:unless (any-operator? #'rhs))
                   ((~optional _:sign) _:term _:infix-step ...))))

  (define-syntax-class response
    #:description "a response: a column name, (surv time status) or (column ...)"
    (pattern _:column)
    (pattern ((~datum surv) _:column _:column))
    (pattern (_:column ...+))))

(define-syntax (~ stx)
  (syntax-parse stx
    [(_ response:response . rhs:right-hand-side)
     #:with (element ...) #'rhs
     #'(make-formula 'response 'element ...)]))

;; The response columns of formula f, as strings.
(define (response-columns f)
  (match (formula-response f)
    [(list 'surv time status) (list (column-name->string time) (column-name->string status))]
    [(? list? names) (map column-name->string names)]
    [name (list (column-name->string name))]))

;; The terms of formula f on the table's `columns`: R's terms(), without a
;; response column that stands alone as a term, which R's model.matrix drops
;; with a warning, as this does. The response columns must be columns of the
;; table and distinct, and so must every column the formula names, even one
;; it removes, as R's model.frame evaluates them all.
(define (formula-expansion who f columns)
  (define responses (response-columns f))
  (define (check-column name)
    (unless (member name columns)
      (raise-arguments-error who "the table has no column with this name"
                             "column" name "formula" f "columns of the table" columns)))
  (for-each check-column responses)
  (define dup (check-duplicates responses))
  (when dup
    (raise-arguments-error who "the response names a column twice" "column" dup "formula" f))
  (define mt (expand-terms (formula-terms f) responses columns))
  (for* ([v (in-vector (model-terms-variables mt))]
         [name (in-list (variable-inputs v))])
    (check-column name))
  (define-values (kept dropped) (drop-response-terms mt))
  (for ([v (in-list dropped)])
    (log-glmnet-warning "~a: the response column ~s appeared on the right-hand side and was dropped"
                        who (variable-label v)))
  kept)

(define (check-predictors who f mt)
  (when (null? (model-terms-terms mt))
    (raise-arguments-error who "the formula has no predictors, and glmnet needs at least one"
                           "formula" f)))

(define (formula-predictor-names f table)
  (define who 'formula-predictor-names)
  (model-terms-column-names (formula-expansion who f (table-names table who))))

(define (formula-design-matrix f table)
  (define who 'formula-design-matrix)
  (define mt (formula-expansion who f (table-names table who)))
  (check-predictors who f mt)
  (terms->design-matrix who mt table))

;; The intercept an explicit #:intercept? must agree with: the formula's 1, 0
;; or - 1, except for the Cox family, which has no intercept.
(define (intercept-for/c f family-name)
  (define says (if (eq? family-name 'cox) 'unspecified (rhs-intercept (formula-terms f))))
  (flat-contract-with-explanation
   (lambda (v)
     (cond
       [(not (boolean? v))
        (lambda (blame) (raise-blame-error blame v '(expected: "boolean?" given: "~e") v))]
       [(or (eq? says 'unspecified) (eq? v says)) #t]
       [else
        ;; The formula goes after expected:, since a colon in the first line
        ;; of the message would lay it out as a field.
        (lambda (blame)
          (raise-blame-error blame v
                             (list "the intercept? argument contradicts the formula's intercept"
                                   'expected: "~a, since the formula ~s ~a" 'given: "~e")
                             says f (if says "has an intercept term" "has no intercept") v))]))
   #:name 'boolean?))

;; --- formula models ------------------------------------------------------------

;; formula        : the formula the model was fitted from
;; predictor-names : the names of the design matrix's columns, in the order of
;;                  the coefficients
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

;; The model that the formula procedures make: a formula model with the
;; expanded terms of its fit, from which `predict` builds the design matrix of
;; a new table. The terms are internal, so they live in this subtype and
;; formula-model keeps its documented fields.
(struct formula-model/terms formula-model (terms)
  #:transparent
  #:property prop:predictor-matrix
  (lambda (m X who)
    (terms->design-matrix who (formula-model/terms-terms m) X)))

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
;; Returns what is wrong with f's response, or #f.
(define (response-form-problem f family-name)
  (define form
    (match (formula-response f)
      [(list 'surv _ _) 'surv]
      [(? list?) 'columns]
      [_ 'column]))
  (case family-name
    [(cox) (and (not (eq? form 'surv)) "the Cox family needs a (surv time status) response")]
    [(mgaussian) (and (eq? form 'surv) "a (surv time status) response is for the Cox family")]
    [else
     (case form
       [(surv) "a (surv time status) response is for the Cox family"]
       [(columns) "a response of several columns is for the mgaussian family"]
       [else #f])]))

;; A formula whose response suits the family.
(define (formula-for/c family-name)
  (define expected
    (format "a formula with ~a, for the ~a family"
            (case family-name
              [(cox) "a (surv time status) response"]
              [(mgaussian) "a response of one or more columns"]
              [else "a response of one column"])
            family-name))
  (flat-contract-with-explanation
   (lambda (f)
     (cond
       [(not (formula? f))
        (lambda (blame) (raise-blame-error blame f '(expected: "formula?" given: "~e") f))]
       [(response-form-problem f family-name)
        => (lambda (problem)
             (lambda (blame)
               (raise-blame-error blame f (list problem 'expected: "~a" 'given: "~e")
                                  expected f)))]
       [else #t]))
   #:name 'formula?))

;; #f, for the family's default, or one of the family's type measures.
(define (type-measure/c family-name)
  (define measures (family-measures (hash-ref families family-name)))
  (flat-contract-with-explanation
   (lambda (measure)
     (or (not measure)
         (and (memq measure measures) #t)
         (lambda (blame)
           (raise-blame-error blame measure
                              (list (format "the ~a family has no such type measure" family-name)
                                    'expected: "#f or one of ~e" 'given: "~e")
                              measures measure))))
   #:name (list* 'or/c #f measures)))

;; Checks that every value of a response column satisfies `ok?`.
(define (check-values who column values ok? problem)
  (for ([v (in-list values)]
        [i (in-naturals)]
        #:unless (ok? v))
    (raise-arguments-error who problem "column" column "row" i "value" v)))

(define (zero-or-one? v) (or (= v 0.0) (= v 1.0)))

;; The labels of a multinomial response, 0, 1, ..., K - 1 with every class
;; present and K >= 2, as multinomial-fit needs them.
(define (check-class-labels who column labels)
  (define present (for/hasheqv ([label (in-list labels)]) (values label #t)))
  (define largest (apply max labels))
  (define missing
    (for/first ([label (in-naturals)]
                #:unless (hash-ref present label #f))
      label))
  (when (zero? largest)
    (raise-arguments-error who "a multinomial response needs at least two classes"
                           "column" column))
  (when (< missing largest)
    (raise-arguments-error who "a multinomial response must use every class label from 0 to its largest"
                           "column" column "missing label" missing "largest label" largest)))

;; The terms, the design matrix of predictors and the response arguments of the
;; family's procedures, from the table: R's model.frame, model.matrix and
;; model.response.
(define (model-frame who f table family-name)
  (define mt (formula-expansion who f (table-names table who)))
  (check-predictors who f mt)
  (define x (terms->design-matrix who mt table))
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
   mt
   x
   (case family-name
     [(gaussian) (list y1)]
     [(binomial)
      (check-values who column y1 zero-or-one? "a binomial response must be 0 or 1")
      (list y1)]
     [(multinomial)
      (check-values who column y1 (lambda (v) (and (integer? v) (>= v 0.0)))
                    "a multinomial response must be a class label 0, 1, ...")
      (define labels (map inexact->exact y1))
      (check-class-labels who column labels)
      (list labels)]
     [(poisson)
      (check-values who column y1 (lambda (v) (>= v 0.0)) "a Poisson response must be non-negative")
      (list y1)]
     [(cox)
      (check-values who column y1 positive? "a survival time must be positive")
      (check-values who (cadr responses) (cadr ys) zero-or-one? "an event status must be 0 or 1")
      (list y1 (cadr ys))]
     [(mgaussian) (list y)])))

;; An #:intercept? option that was not given.
(define unsupplied-intercept (string->uninterned-symbol "unsupplied"))

;; Fits the formula with the family procedure that `select` picks, passing
;; `options`, an association list from keyword to value, as keyword arguments.
;; A #:fold-ids option must have one entry per row of the table. An
;; #:intercept? that was not given is the formula's intercept, and the Cox
;; family, which has no intercept, takes none.
(define (fit-formula who select f table family-name options)
  (define spec (hash-ref families family-name))
  (define-values (mt x responses) (model-frame who f table family-name))
  (define fold-ids (cond [(assq '#:fold-ids options) => cdr] [else #f]))
  (when (and fold-ids (not (= (length fold-ids) (design-matrix-nrows x))))
    (raise-arguments-error who "fold-ids does not have one entry per row of the table"
                           "length of fold-ids" (length fold-ids)
                           "rows of the table" (design-matrix-nrows x)))
  (define kws
    (sort (for/list ([kw (in-list options)]
                     #:unless (and (eq? (car kw) '#:intercept?) (not (family-intercept? spec))))
            (if (eq? (cdr kw) unsupplied-intercept)
                (cons (car kw) (model-terms-intercept? mt))
                kw))
          keyword<? #:key car))
  (formula-model/terms
   f (design-matrix-column-names x)
   (as-formula-procedure who spec
     (lambda ()
       (keyword-apply (select spec) (map car kws) (map cdr kws)
                      (cons x responses))))
   mt))

;; The value of thunk, which calls one of the family's procedures. An error
;; that the family's procedures raise in their own name is raised again in the
;; name of `who`, the formula procedure that was called.
(define (as-formula-procedure who spec thunk)
  (define names
    (map (lambda (proc) (symbol->string (object-name proc)))
         (list (family-fit spec) (family-path spec) (family-cv spec))))
  (define (family-error? e)
    (and (exn:fail? e)
         (not (exn:fail:contract:blame? e))
         (let ([m (regexp-match #rx"^([^ :]+): " (exn-message e))])
           (and m (member (cadr m) names) #t))))
  (define (rename e)
    (define message
      (regexp-replace #rx"^[^ :]+: " (exn-message e) (lambda (_) (format "~a: " who))))
    ((if (exn:fail:contract? e) exn:fail:contract exn:fail) message (exn-continuation-marks e)))
  (with-handlers ([family-error? (lambda (e) (raise (rename e)))])
    (thunk)))

(define (formula-fit f table
                     #:family [family-name 'gaussian]
                     #:lambda lambda
                     #:alpha [alpha 1.0]
                     #:standardize? [standardize? #t]
                     #:intercept? [intercept? unsupplied-intercept]
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
                      #:intercept? [intercept? unsupplied-intercept]
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
                    #:intercept? [intercept? unsupplied-intercept]
                    #:thresh [thresh 1e-7]
                    #:max-iters [max-iters 100000])
  (define measures (family-measures (hash-ref families family-name)))
  (fit-formula 'formula-cv family-cv f table family-name
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
