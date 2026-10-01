#lang racket/base

;; Formula terms (#53): what R's terms() and model.matrix() do for the formula
;; front end (formula.rkt). The right-hand side of a formula, as written, is
;; term data: columns, all, 0 and 1, prefix groups such as (* x z), and infix
;; sequences such as x + z : w. `expand-terms` parses it into R's binary
;; operators and expands it as R's termsform() does, into `model-terms`: the
;; variables, the ordered terms and the intercept. `resolve-levels` reads the
;; table for which variables are factors and what their levels are, and
;; `terms->design-matrix` builds the design matrix of the terms from a table,
;; as model.matrix() does without its intercept column.
;;
;; A term is a set of variables, held as an ascending list of indices into the
;; variables vector, where R holds a bit set, so that a formula of many main
;; effects stays linear in their number; the response columns are the first
;; variables. A variable is a struct: a `column-variable`, a
;; `transform-variable`, a function of columns computed row by row, or a
;; `factor-variable`, R's factor(x) of a column or a transform. Every
;; procedure that reads a variable dispatches on its kind with `match`, so
;; another kind adds a struct and a clause to each of `leaf-variable`,
;; `variable-label`, `variable-kind`, `variable-inputs`, `variable-values` and
;; `variable-flonums`.
;;
;; Whether a variable is a factor depends on its values, as in R: strings,
;; symbols and booleans make one, and numbers only under (factor x). Its
;; levels are found when the model is fitted and kept with the terms (R's
;; xlevels), so that new data is coded with them. A factor's columns depend on
;; its coding in its term: 1 (R's factors attribute, `codings`) codes it by
;; treatment contrasts, a column for each level but the first, and 2 by
;; dummies, a column for each level (`design-codings`).
;;
;; A transform is resolved against the table's columns when the terms are
;; expanded: each name it reads is the column of that name if the table has
;; one, and otherwise the Racket binding that `~` captured (R's
;; data-then-environment rule). The resolution is kept with the terms, so new
;; data must have the columns the fit read.

(module words racket/base
  (provide operators unsupported-operators reserved glued-operator?)
  ;; The words of the formula language, and R's operators that it does not
  ;; have. A column with one of these names is written as a string.
  (define operators '(+ - * : ^))
  (define unsupported-operators '(/ %in%))
  (define reserved (list* 'all 'surv (append operators unsupported-operators)))
  ;; A symbol such as wt:hp or -wt, which the reader reads as one name where R
  ;; reads an operator. A column with such a name is written as a string, too.
  ;; A hyphen inside a name is allowed, since Racket names use it.
  (define (glued-operator? sym)
    (and (not (memq sym reserved))
         (regexp-match? #rx"[+*:^/]|%in%|^-." (symbol->string sym)))))

(require racket/flonum
         racket/list
         racket/match
         racket/string
         'words
         (only-in (submod "../data.rkt" support)
                  column-name->string table-names select-table-values table-column->flvector
                  flvectors->design-matrix))

(provide (all-from-out 'words)
         column-name?
         term?
         rhs?
         rhs-intercept
         transform-term
         transform-term?
         transform-term-name
         source-transform
         transform-argument
         (struct-out column-variable)
         variable-label
         variable-inputs
         (struct-out model-terms)
         expand-terms
         drop-response-terms
         term-variables
         model-terms-labels
         resolve-levels
         model-terms-column-names
         model-terms-factor-levels
         terms->design-matrix
         response-classes)

;; --- term data -------------------------------------------------------------------

(define (column-name? v)
  (or (string? v) (and (symbol? v) (not (memq v reserved)) (not (glued-operator? v)))))

;; A transform: the design-matrix column `name`, whose value in each row is
;; `proc` applied to the row's value of each of `columns`, which the table
;; must have. Two transforms are equal when they are written the same: the
;; same name and names.
(struct transform-term (name columns proc)
  #:guard (lambda (name columns proc _)
            (values (string->immutable-string name) (map column-name->string columns) proc))
  #:property prop:custom-write
  (lambda (t port mode)
    (write-string "#<transform-term " port)
    (write-string (transform-term-name t) port)
    (write-string ">" port))
  #:property prop:equal+hash
  (let ([key (lambda (t) (cons (transform-term-name t) (transform-term-columns t)))])
    (list (lambda (a b recur) (recur (key a) (key b)))
          (lambda (t recur) (recur (key t)))
          (lambda (t recur) (recur (key t))))))

;; A transform that `~` makes. Its `columns` are the names in its argument
;; positions, each the column of that name when the table has one and
;; otherwise the Racket binding where the formula was written: `fallbacks`
;; has for each name a thunk returning that binding, or #f when it has none.
;; `proc`'s body reads a name through `transform-argument`, so that a name
;; that is neither is an error only if the body reads it.
(struct source-transform transform-term (fallbacks))

;; The value that a transform's name that is neither a column nor bound gets,
;; and the exception that reading it raises.
(define unbound (string->uninterned-symbol "unbound"))
(struct exn:fail:unbound-name exn:fail (name))

(define (transform-argument v name)
  (if (eq? v unbound)
      (raise (exn:fail:unbound-name (format "~a: not a column and not bound" name)
                                    (current-continuation-marks) name))
      v))

(define (operator? v) (and (memq v operators) #t))
(define (sign? v) (and (memq v '(+ -)) #t))
(define (power? v) (and (exact-integer? v) (>= v 2)))

;; Whether elements after the first of a group include a bare operator, which
;; makes the group an infix sequence rather than a prefix form or a call.
(define (infix-group? v)
  (and (pair? v) (list? v) (ormap operator? (cdr v))))

(define (term? v)
  (match v
    ['all #t]
    [(or 0 1) #t]
    [(? column-name?) #t]
    [(? transform-term?) #t]
    [(list 'factor (or (? column-name?) (? transform-term?))) #t]
    [(? infix-group?) (infix? v)]
    [(list '+ (? term?) ..1) #t]
    [(list '- (? term?) ..1) #t]
    [(list (or '* ':) (? term?) (? term?) ..1) #t]
    [(list '^ (? term?) (? power?)) #t]
    [_ #f]))

;; [sign] term {operator term | ^ power} ..., without a power raised again,
;; which R rejects: it reads x^2^3 as x^(2^3).
(define (infix? elements)
  (define after-sign
    (if (and (pair? elements) (sign? (car elements))) (cdr elements) elements))
  (and (pair? after-sign)
       (term? (car after-sign))
       (let loop ([rest (cdr after-sign)])
         (match rest
           ['() #t]
           [(list* '^ (? power?) '^ _) #f]
           [(list* '^ (? power?) more) (loop more)]
           [(list* (or '+ '- '* ':) (? term?) more) (loop more)]
           [_ #f]))))

;; A right-hand side: terms side by side, joined as by +, or an infix sequence.
(define (rhs? v)
  (and (list? v)
       (if (ormap operator? v) (infix? v) (andmap term? v))))

;; --- parsing into R's operators ------------------------------------------------

;; A right-hand side as R's calls, each with one or two operands: (+ a b),
;; (- a b), (- a), (* a b), (: a b) and (^ a n), with columns, all, 0 and 1 at
;; the leaves; #f for an empty right-hand side.
(define (parse-rhs rhs)
  (cond
    [(null? rhs) #f]
    [(ormap operator? rhs) (parse-infix rhs)]
    [else (fold-operands '+ (map parse-term rhs))]))

(define (fold-operands op operands)
  (for/fold ([tree (car operands)]) ([operand (in-list (cdr operands))])
    (list op tree operand)))

(define (parse-term t)
  (match t
    [(? infix-group?) (parse-infix t)]
    [(list '- operand) (list '- (parse-term operand))]
    [(list '^ operand n) (list '^ (parse-term operand) n)]
    [(list 'factor _) t]
    [(cons op operands) (fold-operands op (map parse-term operands))]
    [leaf leaf]))

;; R's precedence: ^ binds tightest, then a leading sign, then :, *, and + and
;; -, each but ^ left-associative.
(define precedence (hasheq '+ 1 '- 1 '* 2 ': 3 '^ 5))
(define sign-precedence 4)

(define (parse-infix elements)
  (define-values (tree rest) (parse-operation elements 0))
  tree)

;; The longest operation at the front of `elements` whose operators bind at
;; least as tightly as `least`, and the elements after it.
(define (parse-operation elements least)
  (define-values (first rest)
    (match elements
      [(cons (? sign? sign) more)
       (define-values (operand after) (parse-operation more (add1 sign-precedence)))
       (values (if (eq? sign '-) (list '- operand) operand) after)]
      [(cons t more) (values (parse-term t) more)]))
  (let loop ([left first] [rest rest])
    (match rest
      [(cons (? operator? op) more)
       #:when (>= (hash-ref precedence op) least)
       (if (eq? op '^)
           (loop (list '^ left (car more)) (cdr more))
           (let-values ([(right after) (parse-operation more (add1 (hash-ref precedence op)))])
             (loop (list op left right) after)))]
      [_ (values left rest)])))

;; --- variables -----------------------------------------------------------------

;; A column of the table. `name` is a string.
(struct column-variable (name) #:transparent)

;; A transform of the table's columns: its `transform-term`, and for each of
;; the term's names, the column it reads, or #f for the Racket binding.
(struct transform-variable (term inputs) #:transparent)

;; R's factor(x): the values of `source`, a column-variable or a
;; transform-variable, as a factor, numbers included.
(struct factor-variable (source) #:transparent)

;; The variable a leaf of the right-hand side names, on a table whose column
;; names are the keys of `present`.
(define (leaf-variable leaf present)
  (match leaf
    [(source-transform _ names _ _)
     (transform-variable leaf (map (lambda (name) (and (hash-ref present name #f) name)) names))]
    [(transform-term _ names _) (transform-variable leaf names)]
    [(list 'factor source) (factor-variable (leaf-variable source present))]
    [_ (column-variable (column-name->string leaf))]))

;; The variable's name in the names of the design matrix's columns and terms.
;; A factor of a column whose name is not an identifier writes it as a string.
(define (variable-label v)
  (match v
    [(column-variable name) name]
    [(transform-variable t _) (transform-term-name t)]
    [(factor-variable (column-variable name))
     (format "(factor ~a)" (if (identifier-name? name) name (format "~s" name)))]
    [(factor-variable source) (format "(factor ~a)" (variable-label source))]))

;; Whether a column's name, as a symbol, is written as itself.
(define (identifier-name? name)
  (define sym (string->symbol name))
  (and (column-name? sym) (string=? (format "~s" sym) name)))

;; What an error calls the variable, before its label.
(define (variable-kind v)
  (match v
    [(column-variable _) "column"]
    [(transform-variable _ _) "transform"]
    [(factor-variable _) "factor"]))

;; The names of the table's columns the variable reads.
(define (variable-inputs v)
  (match v
    [(column-variable name) (list name)]
    [(transform-variable _ inputs) (remove-duplicates (filter values inputs))]
    [(factor-variable source) (variable-inputs source)]))

;; The value of variable v in each row, a vector of `no` values: `column`
;; gives the values of the table's column of a name, and `flonums` those
;; values as flonums, which a transform reads from a column of numbers.
;; `seen` is called with each transform, a column it reads and the kind of
;; the column's values (transform-input).
(define (variable-values who v column flonums no seen)
  (match v
    [(column-variable name) (column name)]
    [(transform-variable t names) (transform-values who t names column flonums no seen)]
    [(factor-variable source) (variable-values who source column flonums no seen)]))

;; The values `vs` of a numeric variable v as an flvector. A column's must be
;; finite reals, as the table's numbers must be, and so must a transform's,
;; and an error names the transform and the row.
(define (variable-flonums who v vs)
  (match v
    [(column-variable name) (table-column->flvector vs name who)]
    [(transform-variable (transform-term label _ _) _)
     (for/flvector #:length (vector-length vs) ([x (in-vector vs)] [i (in-naturals)])
       (unless (real? x)
         (raise-arguments-error who "a transform's value is not a real number"
                                "transform" label "row" i "value" x))
       (define fx (real->double-flonum x))
       (unless (fl< (flabs fx) +inf.0)
         (raise-arguments-error who "a transform's value is not finite"
                                "transform" label "row" i "value" x))
       fx)]
    [(factor-variable source) (variable-flonums who source vs)]))

;; The value of each row of transform t, whose names read the columns
;; `names` (transform-input), or their Racket bindings where a name is #f.
;; The bindings are read each time, as R reads them each time it evaluates
;; the formula; a binding that is not defined, at the top level, counts as
;; none. An error in the transform names it and the row.
(define (transform-values who t names column flonums no seen)
  (match-define (transform-term label _ proc) t)
  (define fallbacks
    (match t
      [(source-transform _ _ _ fallbacks) fallbacks]
      [_ (map (lambda (_) #f) names)]))
  (define arguments
    (for/list ([name (in-list names)] [fallback (in-list fallbacks)])
      (cond
        [name (transform-input who label name column flonums seen)]
        [fallback (with-handlers ([exn:fail:contract:variable? (lambda (e) unbound)])
                    (fallback))]
        [else unbound])))
  (define (row-arguments i)
    (for/list ([name (in-list names)] [a (in-list arguments)])
      (cond
        [(not name) a]
        [(flvector? a) (flvector-ref a i)]
        [else (vector-ref a i)])))
  (for/vector #:length no ([i (in-range no)])
    (with-handlers ([exn:fail:unbound-name?
                     (lambda (e)
                       (raise-arguments-error
                        who "a name in a transform is neither a column of the table nor a defined variable"
                        "name" (symbol->string (exn:fail:unbound-name-name e))
                        "transform" label))]
                    [exn:fail?
                     (lambda (e)
                       (raise-arguments-error who "a transform raised an exception"
                                              "transform" label "row" i
                                              "exception" (unquoted-printing-string (exn-message e))))])
      (apply proc (row-arguments i)))))

;; The values of the table's column `name` as transform `label` reads them,
;; as R's calls read a numeric, character or logical column: an flvector
;; when they are numbers, and a vector of the values themselves when they are
;; strings, symbols or booleans. They must be of one kind, and numbers
;; finite; an error names the transform, the column and the row. `seen` is
;; called with the label, the name and the kind.
(define (transform-input who label name column flonums seen)
  (define vs (column name))
  (define kind (input-kind (vector-ref vs 0)))
  (define (fail message i x)
    (raise-arguments-error who message "transform" label "column" name "row" i "element" x))
  (for ([x (in-vector vs)] [i (in-naturals)])
    (define k (input-kind x))
    (cond
      [(eq? k 'other)
       (fail "a transform reads a column with an element that is not a real number, a string, a symbol or a boolean"
             i x)]
      [(not (eq? k kind))
       (fail (format "a transform reads a column that mixes ~a with ~a"
                     (hash-ref kind-names kind) (hash-ref kind-names k))
             i x)]
      [(and (eq? k 'number) (not (fl< (flabs (real->double-flonum x)) +inf.0)))
       (fail "a transform reads a column with an element that is not finite" i x)]))
  (seen label name kind)
  (if (eq? kind 'number) (flonums name) vs))

;; What a transform reads a value as: a number, a string, a symbol, a boolean
;; or another value. Strings and symbols are two kinds here, where a factor's
;; levels match them by label, since a transform can tell them apart:
;; (equal? g "a") is #f for the symbol a.
(define (input-kind v)
  (cond
    [(real? v) 'number]
    [(string? v) 'string]
    [(symbol? v) 'symbol]
    [(boolean? v) 'boolean]
    [else 'other]))

;; --- factors ---------------------------------------------------------------------

;; The label of the level that a factor's value is, as R names levels: a
;; string itself, a symbol's name, TRUE or FALSE for a boolean, and a number
;; as Racket prints its flonum, without a trailing .0; #f for another value.
;; Values with the same label are the same level: the symbol 'a and the
;; string "a" are, and so are 6 and 6.0.
(define (level-label v)
  (cond
    [(string? v) (string->immutable-string v)]
    [(symbol? v) (string->immutable-string (symbol->string v))]
    [(boolean? v) (if v "TRUE" "FALSE")]
    [(real? v) (number-label v)]
    [else #f]))

(define (number-label v)
  (define x (let ([x (real->double-flonum v)]) (if (zero? x) 0.0 x)))
  (define printed (number->string x))
  (string->immutable-string (if (integer? x) (regexp-replace #rx"[.]0$" printed "") printed)))

;; What a value is, for a factor: a number, text (a string or a symbol), a
;; boolean, or another value.
(define (value-kind v)
  (cond
    [(real? v) 'number]
    [(or (string? v) (symbol? v)) 'text]
    [(boolean? v) 'boolean]
    [else 'other]))

(define kind-names
  (hasheq 'number "numbers" 'text "strings or symbols" 'string "strings" 'symbol "symbols"
          'boolean "booleans" 'other "other values"))

;; The kind of the values `vs` of variable v, a column or a transform: its
;; first value's, which every value must have. Among numbers, a value of
;; another kind is left to the conversion to flonums, which names it, and so
;; is a first value of another kind.
(define (values-kind who v vs)
  (define first-kind (value-kind (vector-ref vs 0)))
  (define kind (if (eq? first-kind 'other) 'number first-kind))
  (for ([x (in-vector vs)] [i (in-naturals)])
    (define k (value-kind x))
    (unless (or (eq? k kind) (and (eq? kind 'number) (eq? k 'other)))
      (define mix (format "~a with ~a" (hash-ref kind-names kind) (hash-ref kind-names k)))
      (match v
        [(column-variable name)
         (raise-arguments-error who (string-append "a column mixes " mix)
                                "column" name "row" i "element" x)]
        [_
         (raise-arguments-error who (string-append "a transform's values mix " mix)
                                "transform" (variable-label v) "row" i "value" x)])))
  kind)

;; The labels of the levels of variable v, whose values are `vs`, in R's
;; order, or #f when v is numeric. Strings and symbols make a factor, sorted
;; by string<?, which is R's sort() in the C locale. Booleans make one with
;; the levels FALSE and TRUE, both even when one is absent, as R codes a
;; logical variable. Numbers make one only under (factor x), sorted by value;
;; (factor x) of booleans has the levels present, as R's factor() does.
(define (factor-levels who v vs)
  (define forced? (factor-variable? v))
  (define source (if forced? (factor-variable-source v) v))
  (case (values-kind who source vs)
    [(number)
     (and forced?
          (let ([xs (for/list ([x (in-flvector (variable-flonums who source vs))])
                      (if (zero? x) 0.0 x))])
            (map number-label (sort (remove-duplicates xs) <))))]
    [(text) (sort (remove-duplicates (map level-label (vector->list vs))) string<?)]
    [(boolean)
     (if forced?
         (for/list ([b (in-list '(#f #t))]
                    #:when (for/or ([x (in-vector vs)]) (eq? x b)))
           (level-label b))
         (list "FALSE" "TRUE"))]))

;; The levels of variable v in a model: its factor-levels, of which there must
;; be two or more, as R's contrasts need.
(define (variable-levels who v vs)
  (define levels (factor-levels who v vs))
  (when (and levels (null? (cdr levels)))
    (raise-arguments-error who "a factor must have at least two levels"
                           (variable-kind v) (variable-label v) "level" (car levels)))
  levels)

;; The index of each value `vs` of factor v among its `levels`, a vector. A
;; value whose level is not one of them is an error naming every such level,
;; as R's "factor x has new levels" does.
(define (level-indices who v levels vs)
  (define index (for/hash ([l (in-list levels)] [k (in-naturals)]) (values l k)))
  (define indices
    (for/vector #:length (vector-length vs) ([x (in-vector vs)] [i (in-naturals)])
      (define label (level-label x))
      (unless label
        (raise-arguments-error who "a factor's value is not a number, a string, a symbol or a boolean"
                               (variable-kind v) (variable-label v) "row" i "value" x))
      (hash-ref index label #f)))
  (define new
    (remove-duplicates (for/list ([x (in-vector vs)] [k (in-vector indices)] #:unless k)
                         (level-label x))))
  (unless (null? new)
    (raise-arguments-error who "a factor has levels that the model was not fitted with"
                           (variable-kind v) (variable-label v) "new levels" new "levels" levels))
  indices)

;; The classes of a binomial or multinomial response, the table's column
;; `name`, whose values are `vs`, and the index of each value's class: #f and
;; #f when the values are numbers, and otherwise the labels of the levels
;; present, as R's glmnet makes them with as.factor().
(define (response-classes who name vs)
  (define v (column-variable name))
  (cond
    [(eq? (values-kind who v vs) 'number) (values #f #f)]
    [else
     (define classes (factor-levels who (factor-variable v) vs))
     (values classes (level-indices who v classes vs))]))

;; --- expansion -------------------------------------------------------------------

;; R's terms object for a formula on a table:
;;  variables      : a vector of variables, the response columns first and then
;;                   the others in the order they first appear
;;  response-count : how many of the first variables are response columns
;;  terms          : the terms, each an ascending list of indices into
;;                   `variables`, ordered as R orders them: by degree, then by
;;                   first appearance
;;  codings        : for each term, R's factors attribute: for each of its
;;                   variables, in order, 1 if the term without it is empty or
;;                   in an earlier term, else 2
;;  intercept?     : whether the model has an intercept
;;  levels         : #f until `resolve-levels`, and then for each variable
;;                   the labels of its levels when it is a factor, or #f
;;  input-kinds    : #f until `resolve-levels`, and then a hash from the name
;;                   of each column that a transform reads to the kind of its
;;                   values (input-kind), which new data must give it too
(struct model-terms (variables response-count terms codings intercept? levels input-kinds)
  #:transparent)

;; The terms of right-hand side `rhs`, with response columns `responses`
;; (strings), `all` standing for the table's `columns` (strings) that are not
;; responses, and transforms resolved against `columns`. As R's termsform
;; does: + joins and - removes terms, * crosses them, : interacts them and ^
;; crosses a sum with itself; each operation drops the duplicates it makes,
;; keeping the first. 1 and 0 set the intercept, the other way round inside
;; the right operand of -, and the last one wins.
(define (expand-terms rhs responses columns)
  (define-values (variables terms intercept) (encode rhs responses columns))
  (define ordered (sort terms < #:key length #:cache-keys? #t))
  (model-terms variables (length responses) ordered (term-codings ordered)
               (not (eq? intercept #f)) #f #f))

;; The intercept that `rhs` asks for: #t for a 1, #f for a 0 or - 1, the last
;; one written winning, or 'unspecified.
(define (rhs-intercept rhs)
  (define-values (variables terms intercept) (encode rhs '() '()))
  intercept)

(define (encode rhs responses columns)
  (define index (make-hash))
  (define installed '())
  (define (install! v)
    (hash-ref index v
              (lambda ()
                (hash-set! index v (hash-count index))
                (set! installed (cons v installed))
                (hash-ref index v))))
  (define (variable-term v) (list (install! v)))
  (for ([r (in-list responses)]) (install! (column-variable r)))
  (define response-set (for/hash ([r (in-list responses)]) (values r #t)))
  (define present (for/hash ([c (in-list columns)]) (values c #t)))
  (define intercept 'unspecified)
  (define parity #t)
  (define (set-intercept! on?) (set! intercept (eq? on? parity)))
  (define (remove-from left t)
    (set! parity (not parity))
    (define removed (for/hash ([term (in-list (encode-tree t))]) (values term #t)))
    (set! parity (not parity))
    (filter (lambda (term) (not (hash-ref removed term #f))) left))
  (define (encode-tree t)
    (match t
      [#f '()]
      ['all (for/list ([c (in-list columns)] #:unless (hash-ref response-set c #f))
              (variable-term (column-variable c)))]
      [1 (set-intercept! #t) '()]
      [0 (set-intercept! #f) '()]
      [(list '+ a b) (let* ([l (encode-tree a)] [r (encode-tree b)]) (trim (append l r)))]
      [(list '- a) (remove-from '() a)]
      [(list '- a b) (remove-from (encode-tree a) b)]
      [(list ': a b) (let* ([l (encode-tree a)] [r (encode-tree b)]) (interact l r))]
      ;; R's CrossTerms appends to the left operand's list in place, so an
      ;; empty left operand, as in 0 * b, loses everything: R's y ~ 0*b + c
      ;; is y ~ c - 1.
      [(list '* a b)
       (let* ([l (encode-tree a)] [r (encode-tree b)])
         (if (null? l) '() (trim (append l r (interact l r)))))]
      ;; R's PowerTerms crosses n - 1 times. A crossing that changes nothing
      ;; leaves every later one unchanged too, so the loop stops there, and a
      ;; huge exponent costs what one past the number of terms does.
      [(list '^ a n)
       (define l (encode-tree a))
       (let loop ([crossed l] [left (sub1 n)])
         (define next (if (zero? left) crossed (interact l crossed)))
         (if (equal? next crossed) crossed (loop next (sub1 left))))]
      [leaf (list (variable-term (leaf-variable leaf present)))]))
  (define terms (encode-tree (parse-rhs rhs)))
  (values (list->vector (reverse installed)) terms intercept))

;; Every term of a with every term of b, as R's InteractTerms.
(define (interact a b)
  (trim (for*/list ([l (in-list a)] [r (in-list b)]) (term-union l r))))

;; Without empty and repeated terms, as R's TrimRepeats.
(define (trim terms)
  (remove-duplicates (filter pair? terms)))

(define (term-union a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [(< (car a) (car b)) (cons (car a) (term-union (cdr a) b))]
    [(> (car a) (car b)) (cons (car b) (term-union a (cdr b)))]
    [else (cons (car a) (term-union (cdr a) (cdr b)))]))

;; Whether every variable of term a is in term b.
(define (subterm? a b)
  (cond
    [(null? a) #t]
    [(null? b) #f]
    [(= (car a) (car b)) (subterm? (cdr a) (cdr b))]
    [(> (car a) (car b)) (subterm? a (cdr b))]
    [else #f]))

;; R's TermCode for each variable of each term.
(define (term-codings terms)
  (for/fold ([earlier '()] [codings '()] #:result (reverse codings))
            ([term (in-list terms)])
    (values (cons term earlier)
            (cons (for/list ([i (in-list term)])
                    (define margin (remv i term))
                    (if (or (null? margin)
                            (for/or ([e (in-list earlier)]) (subterm? margin e)))
                        1
                        2))
                  codings))))

;; --- the response on the right-hand side ------------------------------------------

;; The terms without the response columns that stand alone as terms, and those
;; response variables. R's model.matrix drops the response when it is a term
;; of its own and keeps it in interactions.
(define (drop-response-terms mt)
  (match-define (model-terms variables k terms codings intercept? levels kinds) mt)
  (define (response-term? term)
    (and (null? (cdr term)) (< (car term) k)))
  (define kept
    (for/list ([term (in-list terms)] [coding (in-list codings)] #:unless (response-term? term))
      (cons term coding)))
  (values (model-terms variables k (map car kept) (map cdr kept) intercept? levels kinds)
          (for/list ([term (in-list terms)] #:when (response-term? term))
            (vector-ref variables (car term)))))

;; --- names and design matrices -----------------------------------------------------

;; The variables of a term, in the order of the variables.
(define (term-variables mt term)
  (for/list ([i (in-list term)])
    (vector-ref (model-terms-variables mt) i)))

;; R's term labels: each term's variables joined by colons.
(define (model-terms-labels mt)
  (for/list ([term (in-list (model-terms-terms mt))])
    (string-join (map variable-label (term-variables mt term)) ":")))

;; The indices of the variables that the terms use.
(define (used-variables mt)
  (for*/hasheqv ([term (in-list (model-terms-terms mt))] [i (in-list term)])
    (values i #t)))

;; The names of the table's columns that the terms read, each once, in the
;; order of the variables.
(define (model-terms-inputs mt)
  (define used (used-variables mt))
  (remove-duplicates
   (append* (for/list ([v (in-vector (model-terms-variables mt))]
                       [i (in-naturals)]
                       #:when (hash-ref used i #f))
              (variable-inputs v)))))

;; What the terms read from `table`: a procedure from a column's name to its
;; values, one from a name to its values as flonums, converted once, and the
;; number of rows. The columns the terms read must be in the table, and an
;; error names those that are not. `mt` has at least one term.
(define (table-reader who mt table)
  (define needed (model-terms-inputs mt))
  (define available (table-names table who))
  (define present (for/hash ([name (in-list available)]) (values name #t)))
  (define missing (filter (lambda (name) (not (hash-ref present name #f))) needed))
  (match missing
    ['() (void)]
    [(list name)
     (raise-arguments-error who "the table has no column with this name"
                            "column" name "columns of the table" available)]
    [names
     (raise-arguments-error who "the table has no columns with these names"
                            "columns" names "columns of the table" available)])
  (define by-name
    (for/hash ([name (in-list needed)] [vs (in-list (select-table-values table needed who))])
      (values name vs)))
  (define converted (make-hash))
  (define (column name) (hash-ref by-name name))
  (define (flonums name)
    (hash-ref! converted name (lambda () (table-column->flvector (column name) name who))))
  (values column flonums (vector-length (column (car needed)))))

;; The terms `mt` with the levels of each variable that its terms use, which
;; `table` decides: #f for a numeric variable, or the labels of a factor's
;; levels in R's order, of which it must have two or more; and with the kind
;; of each column that a transform reads.
(define (resolve-levels who mt table)
  (define variables (model-terms-variables mt))
  (define used (used-variables mt))
  (define kinds (make-hash))
  (define (record! label name kind) (hash-set! kinds name kind))
  (define levels
    (if (zero? (hash-count used))
        (make-vector (vector-length variables) #f)
        (let-values ([(column flonums no) (table-reader who mt table)])
          (for/vector #:length (vector-length variables) ([v (in-vector variables)]
                                                          [i (in-naturals)])
            (and (hash-ref used i #f)
                 (variable-levels who v (variable-values who v column flonums no record!)))))))
  (struct-copy model-terms mt [levels levels] [input-kinds (for/hash ([(name kind) (in-hash kinds)]) (values name kind))]))

;; A `seen` for variable-values that checks each column a transform reads
;; against the kind of values it had when `mt`'s levels were resolved: new
;; data of another kind would give the transform other values silently, as
;; 0, which is true in Racket, for a boolean column written as 1 and 0.
(define ((check-input-kind who mt) label name kind)
  (define fitted (hash-ref (or (model-terms-input-kinds mt) #hash()) name #f))
  (when (and fitted (not (eq? fitted kind)))
    (raise-arguments-error who "a transform reads a column whose values are of another kind than when the model was fitted"
                           "transform" label "column" name
                           "fitted with" (unquoted-printing-string (hash-ref kind-names fitted))
                           "given" (unquoted-printing-string (hash-ref kind-names kind)))))

;; The levels of variable i, or #f.
(define (levels-of mt i)
  (define levels (model-terms-levels mt))
  (and levels (vector-ref levels i)))

;; Each factor of the terms and its levels, as (label level ...), in the order
;; of the variables: R's xlevels, which leave out the response, even in an
;; interaction.
(define (model-terms-factor-levels mt)
  (for/list ([v (in-vector (model-terms-variables mt))]
             [i (in-naturals)]
             #:when (and (>= i (model-terms-response-count mt)) (levels-of mt i)))
    (cons (variable-label v) (levels-of mt i))))

;; The coding of each variable of each term in the design matrix: R's
;; factors attribute, `codings`, except that without an intercept the first
;; factor of the first term that has one is coded by dummies, as
;; model.matrix codes it, so that its columns span the intercept. A response
;; variable is not that factor.
(define (design-codings mt)
  (match-define (model-terms _ k terms codings intercept? _ _) mt)
  (define first-factor
    (and (not intercept?)
         (for/or ([term (in-list terms)] [j (in-naturals)])
           (for/first ([i (in-list term)] #:when (and (>= i k) (levels-of mt i)))
             (cons j i)))))
  (match first-factor
    [#f codings]
    [(cons j0 i0)
     (for/list ([term (in-list terms)] [coding (in-list codings)] [j (in-naturals)])
       (if (= j j0)
           (for/list ([i (in-list term)] [c (in-list coding)]) (if (= i i0) 2 c))
           coding))]))

;; The levels that a factor coded by `coding` has a column for, and the index
;; of the first: every level for dummies (2), and every level but the first,
;; the baseline, for treatment contrasts (1).
(define (coded-levels levels coding)
  (if (= coding 1) (values (cdr levels) 1) (values levels 0)))

;; Every combination of an element of each list of `per-variable`, joined by
;; `combine`, the first list's varying fastest, as model.matrix orders the
;; columns of an interaction.
(define (term-product per-variable combine)
  (for/fold ([acc (car per-variable)]) ([next (in-list (cdr per-variable))])
    (for*/list ([n (in-list next)] [a (in-list acc)])
      (combine a n))))

;; The names of the columns of variable i in a term that codes it by
;; `coding`: its label, or for a factor, its label and a level's.
(define (variable-column-names mt i coding)
  (define label (variable-label (vector-ref (model-terms-variables mt) i)))
  (match (levels-of mt i)
    [#f (list label)]
    [levels
     (define-values (coded first) (coded-levels levels coding))
     (for/list ([l (in-list coded)]) (string-append label l))]))

;; The names of the design matrix's columns, as model.matrix names them: a
;; factor's columns by its label and a level's, as "cyl6", and an
;; interaction's by its variables' joined by colons. `mt` has its levels.
(define (model-terms-column-names mt)
  (append*
   (for/list ([term (in-list (model-terms-terms mt))]
              [coding (in-list (design-codings mt))])
     (term-product (for/list ([i (in-list term)] [c (in-list coding)])
                     (variable-column-names mt i c))
                   (lambda (a n) (string-append a ":" n))))))

;; The columns of variable i in a term that codes it by `coding`, as
;; (name . flvector) pairs: its values for a numeric variable, and for a
;; factor a 0/1 column for each level that the coding has one for.
;; `values-of` gives a variable's values by index, and `flonums` a column's
;; values as flonums.
(define (variable-columns who mt i coding values-of flonums)
  (define v (vector-ref (model-terms-variables mt) i))
  (define label (variable-label v))
  (match (levels-of mt i)
    [#f
     (list (cons label (match v
                         [(column-variable name) (flonums name)]
                         [_ (variable-flonums who v (values-of i))])))]
    [levels
     (define indices (level-indices who v levels (values-of i)))
     (define-values (coded first) (coded-levels levels coding))
     (for/list ([l (in-list coded)] [k (in-naturals first)])
       (cons (string-append label l)
             (for/flvector #:length (vector-length indices) ([x (in-vector indices)])
               (if (eqv? x k) 1.0 0.0))))]))

;; The design matrix of the terms on `table`: one or more columns per term, in
;; the order of the terms. A term's columns are the products of its variables'
;; columns, named by joining the variables' column names with colons, as
;; model.matrix names them. The columns the terms read must be in the table,
;; and a factor's values must be among its levels. `mt` has its levels, and at
;; least one term.
(define (terms->design-matrix who mt table)
  (define-values (column flonums no) (table-reader who mt table))
  (define variables (model-terms-variables mt))
  (define variable-cache (make-hasheqv))
  (define seen (check-input-kind who mt))
  (define (values-of i)
    (hash-ref! variable-cache i
               (lambda () (variable-values who (vector-ref variables i) column flonums no seen))))
  ;; A variable in several terms, such as a factor and its interactions, is
  ;; computed once for each of its codings.
  (define computed (make-hash))
  (define (columns-of i coding)
    (hash-ref! computed (cons i coding)
               (lambda () (variable-columns who mt i coding values-of flonums))))
  (define columns
    (append*
     (for/list ([term (in-list (model-terms-terms mt))]
                [coding (in-list (design-codings mt))])
       (term-product (for/list ([i (in-list term)] [c (in-list coding)])
                       (columns-of i c))
                     (lambda (a n)
                       (cons (string-append (car a) ":" (car n))
                             (for/flvector #:length (flvector-length (cdr a))
                                           ([x (in-flvector (cdr a))] [y (in-flvector (cdr n))])
                               (fl* x y))))))))
  (flvectors->design-matrix (map cdr columns) (map car columns) who))
