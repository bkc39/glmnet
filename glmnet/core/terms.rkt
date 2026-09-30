#lang racket/base

;; Formula terms (#53): what R's terms() and model.matrix() do for the formula
;; front end (formula.rkt). The right-hand side of a formula, as written, is
;; term data: columns, all, 0 and 1, prefix groups such as (* x z), and infix
;; sequences such as x + z : w. `expand-terms` parses it into R's binary
;; operators and expands it as R's termsform() does, into `model-terms`: the
;; variables, the ordered terms and the intercept. `terms->design-matrix` then
;; builds the design matrix of those terms from a table, as model.matrix()
;; does without its intercept column.
;;
;; A term is a set of variables, held as an ascending list of indices into the
;; variables vector, where R holds a bit set, so that a formula of many main
;; effects stays linear in their number; the response columns are the first
;; variables. A variable is a struct: a `column-variable`, or a
;; `transform-variable`, a function of columns computed row by row. Every
;; procedure that reads a variable dispatches on its kind with `match`, so
;; another kind adds a struct and a clause to each of `leaf-variable`,
;; `variable-label`, `variable-inputs` and `variable-columns`.
;; `variable-columns` receives R's coding of the variable in its term
;; (`codings`), which only a variable that expands into several columns needs.
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
                  column-name->string design-matrix-data design-matrix-nrows
                  table-names select-table-columns flvectors->design-matrix)
         ffi/vector)

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
         model-terms-column-names
         terms->design-matrix)

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

;; A column of the table, read as numbers. `name` is a string.
(struct column-variable (name) #:transparent)

;; A transform of the table's columns: its `transform-term`, and for each of
;; the term's names, the column it reads, or #f for the Racket binding.
(struct transform-variable (term inputs) #:transparent)

;; The variable a leaf of the right-hand side names, on a table whose column
;; names are the keys of `present`.
(define (leaf-variable leaf present)
  (match leaf
    [(source-transform _ names _ _)
     (transform-variable leaf (map (lambda (name) (and (hash-ref present name #f) name)) names))]
    [(transform-term _ names _) (transform-variable leaf names)]
    [_ (column-variable (column-name->string leaf))]))

;; The variable's name in the names of the design matrix's columns and terms.
(define (variable-label v)
  (match v
    [(column-variable name) name]
    [(transform-variable t _) (transform-term-name t)]))

;; The names of the table's columns the variable reads.
(define (variable-inputs v)
  (match v
    [(column-variable name) (list name)]
    [(transform-variable _ inputs) (remove-duplicates (filter values inputs))]))

;; The variable's columns in a term, as (name . flvector) pairs: `inputs` maps
;; the name of each of its inputs to that column of the table, `coding` is
;; R's coding of the variable in the term, 1 (contrasts) or 2 (dummies), and
;; `no` is the number of rows.
(define (variable-columns who v inputs coding no)
  (match v
    [(column-variable name) (list (cons name (hash-ref inputs name)))]
    [(transform-variable t names) (list (cons (transform-term-name t)
                                              (transform-column who t names inputs no)))]))

;; The value of each row of transform t, whose names read the columns
;; `names`, or their Racket bindings where a name is #f. The bindings are
;; read each time, as R reads them each time it evaluates the formula; a
;; binding that is not defined, at the top level, counts as none. A value
;; must be a finite real, and an error names the transform and the row.
(define (transform-column who t names inputs no)
  (match-define (transform-term label _ proc) t)
  (define fallbacks
    (match t
      [(source-transform _ _ _ fallbacks) fallbacks]
      [_ (map (lambda (_) #f) names)]))
  (define arguments
    (for/list ([name (in-list names)] [fallback (in-list fallbacks)])
      (cond
        [name (hash-ref inputs name)]
        [fallback (with-handlers ([exn:fail:contract:variable? (lambda (e) unbound)])
                    (fallback))]
        [else unbound])))
  (define (row-arguments i)
    (for/list ([name (in-list names)] [a (in-list arguments)])
      (if name (flvector-ref a i) a)))
  (for/flvector #:length no ([i (in-range no)])
    (define v
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
        (apply proc (row-arguments i))))
    (unless (real? v)
      (raise-arguments-error who "a transform's value is not a real number"
                             "transform" label "row" i "value" v))
    (define x (real->double-flonum v))
    (unless (fl< (flabs x) +inf.0)
      (raise-arguments-error who "a transform's value is not finite"
                             "transform" label "row" i "value" v))
    x))

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
(struct model-terms (variables response-count terms codings intercept?) #:transparent)

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
               (not (eq? intercept #f))))

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
      [(list '^ a n)
       (define l (encode-tree a))
       (for/fold ([crossed l]) ([i (in-range 1 n)]) (interact l crossed))]
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
  (match-define (model-terms variables k terms codings intercept?) mt)
  (define (response-term? term)
    (and (null? (cdr term)) (< (car term) k)))
  (define kept
    (for/list ([term (in-list terms)] [coding (in-list codings)] #:unless (response-term? term))
      (cons term coding)))
  (values (model-terms variables k (map car kept) (map cdr kept) intercept?)
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

(define (model-terms-column-names mt)
  (model-terms-labels mt))

;; The names of the table's columns that the terms read, each once, in the
;; order of the variables.
(define (model-terms-inputs mt)
  (define used (for*/hasheqv ([term (in-list (model-terms-terms mt))] [i (in-list term)])
                 (values i #t)))
  (remove-duplicates
   (append* (for/list ([v (in-vector (model-terms-variables mt))]
                       [i (in-naturals)]
                       #:when (hash-ref used i #f))
              (variable-inputs v)))))

;; The design matrix of the terms on `table`: one or more columns per term, in
;; the order of the terms. A term's columns are the products of its variables'
;; columns, the first variable's varying fastest, named by joining the
;; variables' column names with colons, as model.matrix names them. The
;; columns the terms read must be in the table, and an error names those that
;; are not. `mt` has at least one term.
(define (terms->design-matrix who mt table)
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
  (define data (select-table-columns table needed who))
  (define no (design-matrix-nrows data))
  (define inputs
    (let ([v (design-matrix-data data)])
      (for/hash ([name (in-list needed)] [j (in-naturals)])
        (values name (for/flvector #:length no ([i (in-range no)])
                       (f64vector-ref v (+ i (* j no))))))))
  ;; A variable in several terms, such as a transform and its interactions,
  ;; is computed once for each of its codings.
  (define computed (make-hash))
  (define (columns-of i coding)
    (hash-ref! computed (cons i coding)
               (lambda ()
                 (variable-columns who (vector-ref (model-terms-variables mt) i) inputs coding no))))
  (define columns
    (append*
     (for/list ([term (in-list (model-terms-terms mt))]
                [coding (in-list (model-terms-codings mt))])
       (define per-variable
         (for/list ([i (in-list term)] [c (in-list coding)])
           (columns-of i c)))
       (for/fold ([acc (car per-variable)]) ([next (in-list (cdr per-variable))])
         (for*/list ([n (in-list next)] [a (in-list acc)])
           (cons (string-append (car a) ":" (car n))
                 (for/flvector #:length (flvector-length (cdr a))
                               ([x (in-flvector (cdr a))] [y (in-flvector (cdr n))])
                   (fl* x y))))))))
  (flvectors->design-matrix (map cdr columns) (map car columns) who))
