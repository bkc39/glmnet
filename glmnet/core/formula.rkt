#lang racket/base

;; The formula front end (#26, #53): R-style formulas over tables, the named
;; data of data.rkt. `~` quotes a formula, except for its transforms, which it
;; compiles into procedures, and checks its grammar where it is written;
;; `formula-fit`, `formula-path` and `formula-cv` expand its terms against a
;; table (terms.rkt, R's terms() and model.matrix()), call the matrix
;; procedure of the family that #:family names, and wrap the result in a
;; `formula-model`. The model keeps its formula, its predictor names and its
;; expanded terms, so that `coef` is keyed by name and `predict` rebuilds the
;; design matrix from a new table (core/model.rkt).

(require (for-syntax racket/base racket/list syntax/parse
                     (submod "terms.rkt" words))
         racket/contract
         racket/generic
         racket/list
         racket/match
         "terms.rkt"
         "model.rkt"
         (only-in (submod "model.rkt" support) prop:predictor-matrix prop:class-labels)
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
                  design-matrix-nrows column-name->string table-names select-table-columns
                  select-table-values))

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
  [transform-term
   (->i ([name string?]
         [columns (and/c (listof (or/c string? symbol?)) pair?)]
         [proc (columns) (procedure-arity-includes/c (length columns))])
        [result transform-term?])]
  [transform-term? (-> any/c boolean?)]
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
  [formula-model-levels (-> formula-model? (listof (cons/c string? (listof string?))))]
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
;; side as written, terms and the infix operators between them, with each
;; transform written as its name, its source.
(struct formula (response terms)
  #:transparent
  #:property prop:custom-write
  (lambda (f port mode)
    (write-string "(~ " port)
    (write (formula-response f) port)
    (for ([t (in-list (formula-terms f))])
      (write-string " " port)
      (write-term t port))
    (write-string ")" port)))

(define (write-term t port)
  (match t
    [(? transform-term?) (write-string (transform-term-name t) port)]
    [(cons first rest)
     (write-string "(" port)
     (write-term first port)
     (for ([u (in-list rest)])
       (write-string " " port)
       (write-term u port))
     (write-string ")" port)]
    [_ (write t port)]))

(define (make-formula response . rhs)
  (formula response rhs))

;; The grammar of terms.rkt's `term?` and `rhs?`, checked where the formula is
;; written. A group's kind is decided by its shape before it is parsed, with
;; ~fail, whose failures rank below those inside the group, so that an error
;; points at the innermost form that is wrong. Each class's `expr` is an
;; expression for the term as data: quoted, except for its transforms.
(begin-for-syntax
  (define (operator-id? stx)
    (and (identifier? stx) (memq (syntax-e stx) operators) #t))
  (define (unsupported-id? stx)
    (and (identifier? stx) (memq (syntax-e stx) unsupported-operators) #t))
  (define (reserved-id? stx)
    (and (identifier? stx) (memq (syntax-e stx) reserved) #t))
  ;; R's operators that the language lacks count as operators here, so that
  ;; the error is about the operator.
  (define (infix-id? stx)
    (or (operator-id? stx) (unsupported-id? stx)))
  (define (any-operator? stx)
    (define elements (syntax->list stx))
    (and elements (ormap infix-id? elements)))
  ;; What follows the dot of an improper list, as syntax.
  (define (dotted-tail stx)
    (define e (syntax-e stx))
    (if (pair? e) (dotted-tail (datum->syntax stx (cdr e) stx)) stx))
  (define (infix-group? stx)
    (define elements (syntax->list stx))
    (and (pair? elements) (ormap infix-id? (cdr elements))))
  (define (group-shape? stx)
    (define elements (syntax->list stx))
    (and (pair? elements)
         (or (identifier? (car elements)) (infix-group? stx))))
  (define (unsupported-message stx)
    (format "~a is an operator of R's formulas that this formula language does not have"
            (syntax-e stx)))
  ;; A 0 or 1 whose source is longer than its digit, such as -0, which the
  ;; reader reads as 0, dropping the sign that R reads as an operator. The
  ;; spelling is gone, so the message names no sign: +0 and -0 differ in R.
  (define (glued-number? stx)
    (define span (syntax-span stx))
    (and span (> span 1)))
  (define (glued-number-message stx)
    (define n (syntax-e stx))
    (format "~a is written with more than its digit, and the reader reads the rest away, as it does a sign glued to it, which R reads as an operator; write ~a, (+ ~a) or (- ~a)"
            n n n n))

  (define (ratio-hint o operands)
    (cond
      [(eq? (syntax-e o) '/)
       (format "; for a ratio, write ~s"
               (syntax-parse operands
                 [(a b) `(I (/ ,(syntax->datum #'a) ,(syntax->datum #'b)))]
                 [_ '(I (/ x z))]))]
      [else ""]))

  ;; The source of stx as it is written, 'x rather than (quote x): the name
  ;; of a transform's column, and so a key of coef.
  (define (syntax->source-string stx)
    (parameterize ([print-reader-abbreviations #t])
      (format "~s" (syntax->datum stx))))

  (define (quote-id? stx)
    (and (identifier? stx)
         (or (free-identifier=? stx #'quote) (free-identifier=? stx #'quasiquote))))
  (define (quoted-message g d)
    (define v (syntax-e d))
    (format "~a is quoted, and ~~ quotes the names of a formula itself: ~a"
            (syntax->source-string g)
            (cond
              [(symbol? v) (format "write the column as ~a or ~s" v (symbol->string v))]
              [(string? v) (format "write the column as ~s" v)]
              [else (format "write ~s without the quote" (syntax->datum d))])))

  (define transformed-response-message
    "a transformed response is not supported; add the transformed column to the table, or write the columns of a response of several columns as strings")

  ;; Whether id has a binding where it is written. At the top level that is a
  ;; definition already evaluated, as a later one cannot be seen there.
  (define (bound-here? id)
    (and (identifier-binding id (syntax-local-phase-level) #t) #t))

  ;; Whether id can be referred to as an expression where it is written: a
  ;; variable, not a macro such as time, or any name at the top level.
  (define (referable? id)
    (with-handlers ([exn:fail:syntax? (lambda (e) #f)])
      (local-expand id 'expression '())
      #t))

  ;; A thunk of id's Racket binding where it is written, or #f.
  (define (fallback-thunk id)
    (if (referable? id) #`(lambda () #,id) #'#f))

  ;; The identifiers in argument position in the expressions `stxs`: every
  ;; element of a group but the first, recursively, except inside quote.
  (define (argument-identifiers stxs)
    (append-map (lambda (stx)
                  (syntax-parse stx
                    [x:id (list #'x)]
                    [((~literal quote) . _) '()]
                    [(head arg ...)
                     (append (if (identifier? #'head) '() (argument-identifiers (list #'head)))
                             (argument-identifiers (syntax->list #'(arg ...))))]
                    [_ '()]))
                stxs))

  ;; Names that Racket's forms match as literals, by binding: cond's => and
  ;; else, quasiquote's unquote and unquote-splicing, and the patterns' ...
  ;; and _. A transform never rebinds them, which would break the match.
  (define racket-literals
    (list (quote-syntax =>) (quote-syntax else) (quote-syntax unquote)
          (quote-syntax unquote-splicing) (quote-syntax ...) (quote-syntax _)))
  (define (racket-literal? id)
    (for/or ([literal (in-list racket-literals)])
      (free-identifier=? id literal)))

  ;; A transform's body is checked as it is expanded, where every binding is
  ;; known, so that a name bound in the transform, by let or lambda, and
  ;; quoted data are what Racket makes of them.
  (define power-message
    "^ is not a Racket function; inside a transform, write a power as (expt x 2), or a square as (sqr x)")
  (define (infix-arithmetic-message group)
    (syntax-parse group
      [(a op b . _)
       (format "~a is not bound, and inside a transform the operators are Racket's, which come first: write ~s"
               (syntax-e #'a) (syntax->datum #'(op a b)))]))

  (define (factor-id? stx)
    (and (identifier? stx) (eq? (syntax-e stx) 'factor)))

  ;; What an error says of a group g, whose head f is not bound, as a term and
  ;; as the argument of factor.
  (define (not-a-term g f)
    (format "~s is not a term: ~a is not bound, so it is not a transform, and a group of terms starts with an operator, as (+ x z) does, or has operators between its terms, as (x + z) does"
            (syntax->datum g) (syntax-e f)))
  (define (not-an-expression g f)
    (format "~s is not a Racket expression: ~a is not bound" (syntax->datum g) (syntax-e f)))

  ;; Raises a syntax error when `group`, an application in the transform `t`
  ;; whose function is `head`, is R's infix arithmetic, such as (hp * wt):
  ;; `head` is not bound and an operator comes second. `power?` is whether ^
  ;; is R's power, not bound where the formula is written.
  (define (check-infix head group t power?)
    (syntax-parse group
      [(_ op:id _ . _)
       #:when (and (identifier? head) (not (bound-here? head)))
       (define o (syntax-e #'op))
       (cond
         [(and (eq? o '^) power?) (raise-syntax-error '~ power-message t #'op)]
         [(memq o '(+ - * / ^)) (raise-syntax-error '~ (infix-arithmetic-message group) t group)]
         [else (void)])]
      [_ (void)]))

  ;; While a transform's body is expanded, a box of the indices of the names
  ;; in argument position that it reads.
  (define current-reads (make-parameter #f))

  ;; The transformer of the `i`th name in argument position of the transform
  ;; `t`. The name alone reads the name's value with `ref`. A group that it
  ;; starts calls `orig`, the name's Racket binding where the formula is
  ;; written, as R looks up the function of a call by name, skipping columns.
  (define ((argument-transformer i ref orig t power?) stx)
    (syntax-parse stx
      [_:id
       (define reads (current-reads))
       (when reads (set-box! reads (cons i (unbox reads))))
       ref]
      [(_ . args)
       (check-infix orig stx t power?)
       (datum->syntax stx (cons orig #'args) stx stx)]))

  ;; `app` is the #%app where the formula is written.
  (define ((application-transformer app t power?) stx)
    (syntax-parse stx
      [(_ . group)
       (syntax-parse #'group
         [(head . _) (check-infix #'head #'group t power?)]
         [_ (void)])
       (datum->syntax stx (cons app #'group) stx stx)]))

  (define ((power-transformer t) stx)
    (raise-syntax-error '~ power-message t (syntax-parse stx [(o . _) #'o] [_ stx])))

  ;; The transform `t`, whose expression is `body`, as a procedure with an
  ;; argument for each of `names`, the names in its argument positions. In the
  ;; body, each name reads its argument through transform-argument, unless
  ;; the transform binds it itself; the function of a group is the Racket
  ;; binding; and #%app and an unbound ^ are the checking transformers above.
  ;; The names quoted in the transformers' expressions are outside the scope
  ;; of let-syntax, so they keep the bindings where the formula is written.
  (define (transform-lambda t body names power?)
    (define tmps (generate-temporaries names))
    (define app (datum->syntax t '#%app))
    (define power (datum->syntax t '^))
    #`(lambda #,tmps
        (let-syntax (#,@(for/list ([name (in-list names)] [tmp (in-list tmps)] [i (in-naturals)])
                          #`[#,name (argument-transformer
                                     #,i (quote-syntax (transform-argument #,tmp '#,name))
                                     (quote-syntax #,name) (quote-syntax #,t) #,power?)])
                     [#,app (application-transformer (quote-syntax #,app) (quote-syntax #,t) #,power?)]
                     #,@(if power? (list #`[#,power (power-transformer (quote-syntax #,t))]) '()))
          #,body)))

  ;; A transform, compiled into a procedure with an argument for each name
  ;; that it reads: the table's column when the table has one, and otherwise
  ;; the Racket binding of the name where the formula is written, which a
  ;; thunk captures when the name has one. The names it reads are those in
  ;; argument position that its body, once expanded, refers to, so not a name
  ;; that it binds itself; the body is expanded here to find them. The class
  ;; commits: a later term's failure must not backtrack into the second
  ;; alternative, which would expand (I e) as a call of I and raise there.
  (define-syntax-class transform
    #:attributes (expr)
    #:commit
    (pattern (~and t (~or* ((~datum I) body) (~and body (_ arg ...))))
             #:do [(define power? (not (bound-here? (datum->syntax #'t '^))))
                   (define names
                     (for/list ([id (in-list (remove-duplicates
                                              (argument-identifiers (or (attribute arg) (list #'body)))
                                              bound-identifier=?))]
                                #:unless (or (and power? (eq? (syntax-e id) '^))
                                             (racket-literal? id)))
                       id))
                   (define reads (box '()))
                   (define proc
                     (parameterize ([current-reads reads])
                       (local-expand (transform-lambda #'t #'body names power?) 'expression '())))
                   (define read? (for/list ([i (in-range (length names))]) (memv i (unbox reads))))
                   (define tmps (generate-temporaries names))]
             #:with (id ...) (for/list ([name (in-list names)] [r (in-list read?)] #:when r) name)
             #:with (tmp ...) (for/list ([tmp (in-list tmps)] [r (in-list read?)] #:when r) tmp)
             #:with (slot ...) (for/list ([tmp (in-list tmps)] [r (in-list read?)]) (if r tmp #'#f))
             #:with proc proc
             #:with (fallback ...) (map fallback-thunk (syntax->list #'(id ...)))
             #:with name (syntax->source-string #'t)
             #:with expr
             #'(source-transform
                'name '(id ...)
                (let ([expanded proc]) (lambda (tmp ...) (expanded slot ...)))
                (list fallback ...))))

  ;; R's quote, which ~ does not need. It always fails, with a message that
  ;; says what to write.
  (define-syntax-class quoted
    (pattern (~and g ((~and q (~fail #:unless (quote-id? #'q))) d))
             #:fail-when #'g (quoted-message #'g #'d)))

  ;; A transform written as a call, (proc-id arg ...) or (I expr), checked so
  ;; that its errors say what to write; `unbound` makes the message for a
  ;; proc-id that is not bound from the group and the id.
  (define-syntax-class (call unbound)
    #:attributes (expr)
    (pattern (~and g (f:id _ ...) (~fail #:when (quote-id? #'f)))
             #:do [(define I? (eq? (syntax-e #'f) 'I))]
             #:fail-when (and I? (not (= (length (syntax->list #'g)) 2)) #'g)
             "I takes one Racket expression, as in (I (expt x 2))"
             #:do [(check-infix #'f #'g #'g (not (bound-here? (datum->syntax #'g '^))))]
             #:fail-when (and (not I?) (not (bound-here? #'f)) #'g)
             (unbound #'g #'f)
             #:with t:transform #'g
             #:with expr #'t.expr))

  ;; What R's factor(x) takes: a column, or a Racket expression, a transform,
  ;; whose values are the factor's.
  (define-syntax-class factor-argument
    #:description "a column, or a Racket expression such as (> hp 150)"
    #:attributes (expr)
    (pattern (~and c (~fail #:unless (and (identifier? #'c) (not (operator-id? #'c)))) _:column)
             #:with expr #''c)
    (pattern (~and s (~fail #:unless (string? (syntax-e #'s))))
             #:with expr #'s)
    (pattern q:quoted
             #:with expr #'q)
    (pattern (~and g (_ _ ...) (~var c (call not-an-expression)))
             #:with expr #'c.expr))

  (define-syntax-class column
    #:description "a column name"
    (pattern name:id
             #:fail-when (and (unsupported-id? #'name) #'name)
             (unsupported-message #'name)
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
    #:attributes ((expr 1))
    (pattern (~seq (~and o (~datum ^)) n:power (~peek-not (~datum ^)))
             #:with (expr ...) #'('o 'n))
    (pattern (~seq (~datum ^) _:power (~and again (~datum ^)))
             #:fail-when #'again
             "a power cannot be raised again; R reads x ^ 2 ^ 3 as x ^ (2 ^ 3), which is not a power"
             #:with (expr ...) #'())
    (pattern (~seq o:infix-operator t:term)
             #:with (expr ...) #'('o t.expr))
    (pattern (~seq (~and o (~fail #:unless (unsupported-id? #'o))) _ ...)
             #:fail-when #'o (string-append (unsupported-message #'o) (ratio-hint #'o #'()))
             #:with (expr ...) #'()))

  (define-syntax-class term
    #:description "a term: a column, all, 1, 0, a transform such as (log x), (factor x), or a group such as (* x z) or (x + z)"
    #:attributes (expr)
    (pattern (~and a (~datum all))
             #:with expr #''a)
    (pattern (~and n (~fail #:unless (memv (syntax-e #'n) '(0 1))))
             #:fail-when (and (glued-number? #'n) #'n) (glued-number-message #'n)
             #:with expr #''n)
    (pattern (~and n (~fail #:unless (let ([v (syntax-e #'n)])
                                       (and (real? v) (not (memv v '(0 1)))))))
             #:fail-when #'n
             (if (negative? (syntax-e #'n))
                 (format "~a is a number; to remove a term, put a space after the sign, as in - ~a"
                         (syntax-e #'n) (- (syntax-e #'n)))
                 (format "~a is not a term; a formula's numbers are 1, 0 and a power after ^"
                         (syntax-e #'n)))
             #:with expr #'n)
    (pattern (~and c:id (~not _:operator) _:column)
             #:with expr #''c)
    (pattern s:str
             #:with expr #'s)
    (pattern (~and g (~fail #:unless (infix-group? #'g))
                   ((~optional s:sign) t:term step:infix-step ...+))
             #:with expr #'(list (~? 's) t.expr step.expr ... ...))
    (pattern ((~and o (~datum +)) t:term ...+)
             #:with expr #'(list 'o t.expr ...))
    (pattern ((~and o (~datum -)) t:term ...+)
             #:with expr #'(list 'o t.expr ...))
    (pattern ((~and o (~datum *)) t:term u:term ...+)
             #:with expr #'(list 'o t.expr u.expr ...))
    (pattern ((~and o (~datum :)) t:term u:term ...+)
             #:with expr #'(list 'o t.expr u.expr ...))
    (pattern ((~and o (~datum ^)) t:term n:power)
             #:with expr #'(list 'o t.expr 'n))
    (pattern ((~datum factor) a:factor-argument)
             #:with expr #'(list 'factor a.expr))
    (pattern (~and g ((~datum factor) . args)
                   (~fail #:when (let ([args (syntax->list #'args)]) (and args (= (length args) 1)))))
             #:fail-when #'g "factor takes one column or Racket expression, as in (factor cyl)"
             #:with expr #'g)
    (pattern q:quoted
             #:with expr #'q)
    (pattern (~and g ((~and o (~fail #:unless (unsupported-id? #'o))) operand ...)
                   (~fail #:when (infix-group? #'g)))
             #:fail-when #'o
             (string-append (unsupported-message #'o) (ratio-hint #'o #'(operand ...)))
             #:with expr #'g)
    (pattern (~and g (f:id _ ...)
                   (~fail #:when (or (reserved-id? #'f) (factor-id? #'f) (infix-group? #'g)))
                   (~var c (call not-a-term)))
             #:with expr #'c.expr)
    (pattern (~and g (_ ...) (~fail #:when (group-shape? #'g)))
             #:fail-when #'g
             (format "~s is not a term: a group of terms starts with an operator, as (+ x z) does, or has operators between its terms, as (x + z) does"
                     (syntax->datum #'g))
             #:with expr #'g))

  (define-syntax-class right-hand-side
    #:description "the right-hand side of a formula"
    #:attributes ((expr 1))
    (pattern (~and rhs (~fail #:unless (syntax->list #'rhs)) (~fail #:when (any-operator? #'rhs))
                   (t:term ...))
             #:with (expr ...) #'(t.expr ...))
    (pattern (~and rhs (~fail #:unless (any-operator? #'rhs))
                   ((~optional s:sign) t:term step:infix-step ...))
             #:with (expr ...) #'((~? 's) t.expr step.expr ... ...))
    (pattern (~and rhs (~fail #:when (syntax->list #'rhs)))
             #:with tail (dotted-tail #'rhs)
             #:fail-when #'tail
             (format "the right-hand side of a formula is a list of terms, and this one has a dot before ~s"
                     (syntax->datum #'tail))
             #:with (expr ...) #'()))

  ;; A response is columns, not a transform. A group that starts with I, or
  ;; with a bound name and has more than column names, is a transform, and
  ;; so is a group of columns whose first name is a procedure where the
  ;; formula is written, which only the running program knows, so its `expr`
  ;; checks it.
  (define-syntax-class response
    #:description "a response: a column name, (surv time status) or (column ...)"
    #:attributes (expr)
    (pattern c:column
             #:with expr #''c)
    (pattern (~and r ((~datum surv) _:column _:column))
             #:with expr #''r)
    (pattern q:quoted
             #:with expr #'q)
    (pattern (~and g (h:id e ...)
                   (~fail #:unless
                          (or (eq? (syntax-e #'h) 'I)
                              (and (bound-here? #'h)
                                   (not (quote-id? #'h))
                                   (not (andmap (lambda (e) (or (identifier? e) (string? (syntax-e e))))
                                                (syntax->list #'(e ...))))))))
             #:fail-when #'g transformed-response-message
             #:with expr #'g)
    (pattern (~and g ((~and h:column (~fail #:when (or (eq? (syntax-e #'h) 'I) (quote-id? #'h))))
                      _:column ...))
             #:with expr (if (referable? #'h)
                             #`(checked-response 'g (lambda () h) (quote-syntax g)
                                                 #,transformed-response-message)
                             #''g))))

(define-syntax (~ stx)
  (syntax-parse stx
    [(_ response:response . rhs:right-hand-side)
     #'(make-formula response.expr rhs.expr ...)]))

;; The response of several columns `response`, unless its first name has a
;; procedure as its Racket binding where the formula is written, as `head`
;; returns it: the response is then a transform, such as (log mpg), and
;; `form`, the response as written, is a syntax error with `message`.
(define (checked-response response head form message)
  (define v (with-handlers ([exn:fail:contract:variable? (lambda (e) #f)]) (head)))
  (when (procedure? v)
    (raise-syntax-error '~ message form))
  response)

;; The response columns of formula f, as strings.
(define (response-columns f)
  (match (formula-response f)
    [(list 'surv time status) (list (column-name->string time) (column-name->string status))]
    [(? list? names) (map column-name->string names)]
    [name (list (column-name->string name))]))

;; The terms of formula f on the table: R's terms(), without a response
;; column that stands alone as a term, which R's model.matrix drops with a
;; warning, as this does, and with the levels of its factors, which the
;; table's values decide. The response columns must be columns of the table
;; and distinct, and so must every column the formula names, even one it
;; removes, as R's model.frame evaluates them all. A transform must read a
;; column, from which its rows come. The design matrix's column names must be
;; distinct, and none "(Intercept)", as coef keys the coefficients by them and
;; the intercept by that name.
(define (formula-expansion who f table)
  (define columns (table-names table who))
  (define responses (response-columns f))
  (define present (for/hash ([c (in-list columns)]) (values c #t)))
  (define (check-column name)
    (unless (hash-ref present name #f)
      (raise-arguments-error who "the table has no column with this name"
                             "column" name "formula" f "columns of the table" columns)))
  (for-each check-column responses)
  (define dup (check-duplicates responses))
  (when dup
    (raise-arguments-error who "the response names a column twice" "column" dup "formula" f))
  (define mt (expand-terms (formula-terms f) responses columns))
  (for ([v (in-vector (model-terms-variables mt))])
    (define inputs (variable-inputs v))
    (when (null? inputs)
      (raise-arguments-error who "a transform must read a column of the table"
                             "transform" (variable-label v) "formula" f
                             "columns of the table" columns))
    (for-each check-column inputs))
  (define-values (kept dropped) (drop-response-terms mt))
  (for ([v (in-list dropped)])
    (log-glmnet-warning "~a: the response column ~s appeared on the right-hand side and was dropped"
                        who (variable-label v)))
  (define resolved (resolve-levels who kept table))
  (define names (model-terms-column-names resolved))
  (when (member "(Intercept)" names)
    (raise-arguments-error who "a column of the formula's design matrix has the intercept's name"
                           "name" "(Intercept)" "formula" f))
  (define same-name (check-duplicates names))
  (when same-name
    (raise-arguments-error who "two columns of the formula's design matrix have the same name"
                           "name" same-name "formula" f))
  resolved)

(define (check-predictors who f mt)
  (when (null? (model-terms-terms mt))
    (raise-arguments-error who "the formula has no predictors, and glmnet needs at least one"
                           "formula" f)))

(define (formula-predictor-names f table)
  (model-terms-column-names (formula-expansion 'formula-predictor-names f table)))

(define (formula-design-matrix f table)
  (define who 'formula-design-matrix)
  (define mt (formula-expansion who f table))
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
;; expanded terms of its fit, with its factors' levels, from which `predict`
;; builds the design matrix of a new table, and the classes of its response,
;; or #f (model-frame). They are internal, so they live in this subtype and
;; formula-model keeps its documented fields.
(struct formula-model/terms formula-model (terms classes)
  #:transparent
  #:property prop:predictor-matrix
  (lambda (m X who)
    (terms->design-matrix who (formula-model/terms-terms m) X))
  #:property prop:class-labels
  (lambda (m) (formula-model/terms-classes m)))

(define (formula-model-levels m)
  (model-terms-factor-levels (formula-model/terms-terms m)))

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

;; The terms, the design matrix of predictors, the response arguments of the
;; family's procedures and the response's classes, from the table: R's
;; model.frame, model.matrix and model.response. A binomial or multinomial
;; response of strings, symbols or booleans has classes, its levels in R's
;; order, as R's glmnet makes a factor response's; the second of a binomial
;; response's two is the one whose probability the model gives. Otherwise
;; the classes are #f.
(define (model-frame who f table family-name)
  (define mt (formula-expansion who f table))
  (check-predictors who f mt)
  (define x (terms->design-matrix who mt table))
  (define responses (response-columns f))
  (define column (car responses))
  (define-values (classes indices)
    (if (memq family-name '(binomial multinomial))
        (response-classes who column (car (select-table-values table (list column) who)))
        (values #f #f)))
  (define y (and (not classes) (select-table-columns table responses who)))
  (define ys
    (if classes
        (list (for/list ([k (in-vector indices)]) (exact->inexact k)))
        (design-matrix->columns y)))
  (define y1 (car ys))
  (unless (= (length y1) (design-matrix-nrows x))
    (raise-arguments-error who "the response and the predictors have different lengths"
                           "response rows" (length y1)
                           "predictor rows" (design-matrix-nrows x)))
  (values
   mt
   x
   (case family-name
     [(gaussian) (list y1)]
     [(binomial)
      (cond
        [classes
         (unless (= (length classes) 2)
           (raise-arguments-error who (if (null? (cdr classes))
                                          "a binomial response needs two classes"
                                          "a binomial response has two classes; the multinomial family takes more")
                                  "column" column "classes" classes))]
        [else (check-values who column y1 zero-or-one? "a binomial response must be 0 or 1")])
      (list y1)]
     [(multinomial)
      (cond
        [classes
         (when (null? (cdr classes))
           (raise-arguments-error who "a multinomial response needs at least two classes"
                                  "column" column "classes" classes))
         (list (vector->list indices))]
        [else
         (check-values who column y1 (lambda (v) (and (integer? v) (>= v 0.0)))
                       "a multinomial response must be a class label 0, 1, ...")
         (define labels (map inexact->exact y1))
         (check-class-labels who column labels)
         (list labels)])]
     [(poisson)
      (check-values who column y1 (lambda (v) (>= v 0.0)) "a Poisson response must be non-negative")
      (list y1)]
     [(cox)
      (check-values who column y1 positive? "a survival time must be positive")
      (check-values who (cadr responses) (cadr ys) zero-or-one? "an event status must be 0 or 1")
      (list y1 (cadr ys))]
     [(mgaussian) (list y)])
   classes))

;; An #:intercept? option that was not given.
(define unsupplied-intercept (string->uninterned-symbol "unsupplied"))

;; Fits the formula with the family procedure that `select` picks, passing
;; `options`, an association list from keyword to value, as keyword arguments.
;; A #:fold-ids option must have one entry per row of the table. An
;; #:intercept? that was not given is the formula's intercept, and the Cox
;; family, which has no intercept, takes none.
(define (fit-formula who select f table family-name options)
  (define spec (hash-ref families family-name))
  (define-values (mt x responses classes) (model-frame who f table family-name))
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
   mt
   classes))

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
