#lang racket/base

;; The input-data layer (#35): `design-matrix`, the one representation of a
;; predictor matrix that the Fortran solvers read, and the standalone
;; conversions into and out of it. `glmnet` re-exports this module; it does not
;; load the native library, so the data formats under data/ build on it alone.
;;
;; Invariant: `data` is a column-major f64vector of nrows * ncols finite
;; flonums, element (i, j) at index i + j*nrows, and `column-names` is #f or
;; one distinct immutable string per column. Every constructor validates its
;; input and copies it into a fresh vector, except that a data format may hand
;; over a fresh vector of its own (flat->design-matrix's #:adopt?), and no
;; public binding hands out that vector, so the invariant holds for the life of
;; the value.

(require racket/contract
         racket/flonum
         racket/list
         ffi/vector)

(define column-names/c (or/c #f (listof (or/c string? symbol?))))
(define column-list/c (and/c (listof (or/c string? symbol?)) pair?))

(provide
 design-matrix?
 design-matrix/c
 (contract-out
  [response/c (-> flat-contract? flat-contract?)]
  [rows->design-matrix
   (->* ((listof list?)) (#:column-names column-names/c) design-matrix?)]
  [columns->design-matrix
   (->* ((listof list?)) (#:column-names column-names/c) design-matrix?)]
  [f64vector->design-matrix
   (->i ([v (nrows ncols) (f64vector-length/c nrows ncols)]
         [nrows exact-positive-integer?]
         [ncols exact-positive-integer?])
        (#:column-names [column-names column-names/c])
        [result design-matrix?])]
  [design-matrix-nrows (-> design-matrix? exact-positive-integer?)]
  [design-matrix-ncols (-> design-matrix? exact-positive-integer?)]
  [design-matrix-column-names (-> design-matrix? (or/c #f (listof string?)))]
  [design-matrix-ref
   (->i ([dm design-matrix?]
         [i (dm) (integer-in 0 (sub1 (design-matrix-nrows dm)))]
         [j (dm) (integer-in 0 (sub1 (design-matrix-ncols dm)))])
        [result flonum?])]
  [design-matrix-select-rows
   (-> design-matrix? (and/c (listof exact-nonnegative-integer?) pair?) design-matrix?)]
  [design-matrix->rows (-> design-matrix? (listof (listof flonum?)))]
  [design-matrix->columns (-> design-matrix? (listof (listof flonum?)))]
  [design-matrix->f64vector (-> design-matrix? f64vector?)]
  [response->f64vector (-> one-dimensional/c f64vector?)]
  [table? (-> any/c boolean?)]
  [table-column-names (-> table? (listof string?))]
  [table->design-matrix (->* (table?) (column-list/c) design-matrix?)]))

;; For the family modules and the data formats under data/ only; not part of
;; the public API. The data formats share one set of constructors and errors:
;;
;;   (flat->design-matrix v nrows ncols names who what
;;                        #:order ['column-major] #:adopt? [#f])
;;     A design matrix from the flvector or f64vector v of nrows * ncols
;;     flonums, column-major (element (i, j) at i + j*nrows) or row-major (at
;;     i*ncols + j). Its contract checks the length. Every entry must be
;;     finite; the error names the entry's row and its column, by name when
;;     `names` (#f or one name per column) has one. The entries are copied,
;;     unless #:adopt? is #t: then v, a column-major f64vector that the caller
;;     has just made and keeps no reference to, becomes the matrix's own.
;;   (element-error who what problem x #:row i #:column c)
;;   (element-error who what problem x #:position k)
;;     Raises "<what> has an element that is <problem>", such as "not a real
;;     number" or "not finite", with the fields column (a name, or an index
;;     when the data has no names), row and element, or position and element
;;     for one-dimensional data.
;;   (missing-error who what #:row i #:column c [#:element x] [#:details fields])
;;   (missing-error who what #:position k [#:element x] [#:details fields])
;;     Raises "<what> has a missing value" with the same fields, then any
;;     further fields, such as a file's line.
;;   (->finite-flonum x who what i c)
;;     x as a flonum; otherwise element-error at row i and column c.
;;   (default-column-names n)
;;     R's names for the columns of an unnamed matrix: "V1" ... "Vn".
(module* support #f
  (provide design-matrix-data
           design-matrix-nrows
           design-matrix-ncols
           nested-matrix?
           nested->dm
           design-matrix->nested-lines
           one-dimensional?
           one-dimensional-length
           one-dimensional->vector
           as-design-matrix
           as-response
           column-name->string
           table-names
           select-table-columns
           select-table-values
           table-column->flvector
           flvectors->design-matrix
           element-error
           missing-error
           ->finite-flonum
           default-column-names
           (contract-out
            [flat->design-matrix
             (->i ([v (or/c flvector? f64vector?)]
                   [nrows exact-positive-integer?]
                   [ncols exact-positive-integer?]
                   [names column-names/c]
                   [who symbol?]
                   [what string?])
                  (#:order [order (or/c 'column-major 'row-major)]
                   #:adopt? [adopt? boolean?])
                  #:pre/desc (v nrows ncols) (flat-length-problem v nrows ncols)
                  #:pre/desc (v order adopt?) (adopt-problem v order adopt?)
                  [result design-matrix?])])))

(struct design-matrix (data nrows ncols column-names)
  #:property prop:custom-write
  (lambda (dm port mode)
    (fprintf port "#<design-matrix ~ax~a>"
             (design-matrix-nrows dm) (design-matrix-ncols dm)))
  #:property prop:equal+hash
  (list (lambda (a b recur)
          (and (= (design-matrix-nrows a) (design-matrix-nrows b))
               (= (design-matrix-ncols a) (design-matrix-ncols b))
               (recur (design-matrix-column-names a) (design-matrix-column-names b))
               (let ([u (design-matrix-data a)]
                     [v (design-matrix-data b)])
                 (for/and ([k (in-range (f64vector-length u))])
                   (eqv? (f64vector-ref u k) (f64vector-ref v k))))))
        (lambda (dm recur) (hash-shape dm recur))
        (lambda (dm recur) (hash-shape dm recur))))

(define (hash-shape dm recur)
  (recur (list (design-matrix-nrows dm)
               (design-matrix-ncols dm)
               (design-matrix-column-names dm))))

(define (nested-matrix? v)
  (define (line? xs) (or (list? xs) (vector? xs)))
  (cond
    [(list? v) (for/and ([xs (in-list v)]) (line? xs))]
    [(vector? v) (for/and ([xs (in-vector v)]) (line? xs))]
    [else #f]))

;; What the fitters and prediction helpers accept for a matrix argument: a
;; design-matrix, or rows in any of the four nestings. A flat contract, unlike
;; vectorof, so that a vector argument reaches the conversion unwrapped.
(define design-matrix/c
  (flat-named-contract
   '(or/c design-matrix? (listof (or/c list? vector?)) (vectorof (or/c list? vector?)))
   (lambda (v) (or (design-matrix? v) (nested-matrix? v)))))

;; --- one-dimensional data ------------------------------------------------------

(define (one-dimensional? v)
  (or (list? v) (vector? v) (flvector? v) (f64vector? v)))

(define one-dimensional/c
  (flat-named-contract '(or/c list? vector? flvector? f64vector?) one-dimensional?))

(define (one-dimensional-length v)
  (cond
    [(list? v) (length v)]
    [(vector? v) (vector-length v)]
    [(flvector? v) (flvector-length v)]
    [else (f64vector-length v)]))

;; A fresh vector of the entries of a list, vector, flvector or f64vector.
(define (one-dimensional->vector v)
  (cond
    [(list? v) (list->vector v)]
    [(vector? v) (for/vector #:length (vector-length v) ([x (in-vector v)]) x)]
    [(flvector? v) (for/vector #:length (flvector-length v) ([x (in-flvector v)]) x)]
    [else (for/vector #:length (f64vector-length v) ([k (in-range (f64vector-length v))])
            (f64vector-ref v k))]))

;; A response/c contract is a struct, so that two built from equivalent
;; element contracts are contract-equivalent?. An element that fails is blamed
;; with its position. Number contracts such as (or/c 0 1) compare with =, which
;; a complex 1.0+0.0i passes, so the elements are checked with real? first.
(struct response-contract (elem)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (lambda (c) (build-compound-type-name 'response/c (response-contract-elem c)))
   #:first-order
   (lambda (c)
     (define elem? (flat-contract-predicate (response-contract-elem c)))
     (lambda (v) (and (response-shape? v) (not (first-failure v elem?)))))
   #:late-neg-projection
   (lambda (c)
     (define elem/c (response-contract-elem c))
     (define elem? (flat-contract-predicate elem/c))
     (define real/c (coerce-flat-contract 'response/c real?))
     (lambda (blame)
       (lambda (v neg-party)
         (cond
           [(not (response-shape? v))
            (raise-blame-error blame #:missing-party neg-party v
                               '(expected: "a non-empty list, vector, flvector or f64vector of ~a"
                                 given: "~e")
                               (contract-name elem/c) v)]
           [(first-failure v elem?)
            => (lambda (k+x)
                 (define x (cdr k+x))
                 (define elem-blame
                   (blame-add-context blame (format "the element at position ~a of" (car k+x))))
                 (((get/build-late-neg-projection (if (real? x) elem/c real/c)) elem-blame)
                  x neg-party)
                 v)]
           [else v]))))
   #:stronger
   (lambda (this that)
     (and (response-contract? that)
          (contract-stronger? (response-contract-elem this) (response-contract-elem that))))
   #:equivalent
   (lambda (this that)
     (and (response-contract? that)
          (contract-equivalent? (response-contract-elem this) (response-contract-elem that))))))

(define (response/c elem)
  (response-contract (coerce-flat-contract 'response/c elem)))

(define (response-shape? v)
  (and (one-dimensional? v) (positive? (one-dimensional-length v))))

;; The position and value of v's first element that is not a real satisfying
;; elem?, or #f.
(define (first-failure v elem?)
  (define (failure k x) (and (not (and (real? x) (elem? x))) (cons k x)))
  (cond
    [(list? v) (for/or ([x (in-list v)] [k (in-naturals)]) (failure k x))]
    [(vector? v) (for/or ([x (in-vector v)] [k (in-naturals)]) (failure k x))]
    [(flvector? v) (for/or ([x (in-flvector v)] [k (in-naturals)]) (failure k x))]
    [else (for/or ([k (in-range (f64vector-length v))]) (failure k (f64vector-ref v k)))]))

;; --- errors --------------------------------------------------------------------

(define (position-fields who i c k)
  (cond
    [k (list "position" k)]
    [(and i c) (list "column" c "row" i)]
    [else (raise-arguments-error who "an element error needs a row and a column, or a position")]))

(define (element-error who what problem x #:row [i #f] #:column [c #f] #:position [k #f])
  (apply raise-arguments-error who (format "~a has an element that is ~a" what problem)
         (append (position-fields who i c k) (list "element" x))))

(define no-element (string->uninterned-symbol "no-element"))

(define (missing-error who what #:row [i #f] #:column [c #f] #:position [k #f]
                       #:element [x no-element] #:details [details '()])
  (apply raise-arguments-error who (format "~a has a missing value" what)
         (append (position-fields who i c k)
                 (if (eq? x no-element) '() (list "element" x))
                 details)))

(define (->finite-flonum x who what i c)
  (define v
    (if (real? x)
        (real->double-flonum x)
        (element-error who what "not a real number" x #:row i #:column c)))
  (unless (fl< (flabs v) +inf.0)
    (element-error who what "not finite" x #:row i #:column c))
  v)

;; An f64vector with nrows * ncols entries: the v argument of
;; f64vector->design-matrix.
(define (f64vector-length/c nrows ncols)
  (define n (* nrows ncols))
  (flat-contract-with-explanation
   (lambda (v)
     (or (and (f64vector? v) (= (f64vector-length v) n))
         (lambda (blame)
           (raise-blame-error blame v
                              '(expected: "an f64vector of length nrows * ncols = ~a" given: "~a")
                              n
                              (if (f64vector? v)
                                  (format "an f64vector of length ~a" (f64vector-length v))
                                  (format "~e" v))))))
   #:name `(f64vector-length/c ,nrows ,ncols)))

;; The column names as immutable strings, or #f, after checking that there is
;; one per column and that they are distinct as strings.
(define (check-column-names names ncols who)
  (cond
    [names
     (define strings (map column-name->string names))
     (unless (= (length strings) ncols)
       (raise-arguments-error who "the number of column names does not match the columns"
                              "column names" (length strings) "columns" ncols))
     (define dup (check-duplicates names #:key column-name->string))
     (when dup
       (raise-arguments-error who "the column names are not distinct"
                              "duplicate" dup "column names" names))
     strings]
    [else #f]))

(define (default-column-names n)
  (for/list ([j (in-range n)])
    (string->immutable-string (format "V~a" (add1 j)))))

;; --- conversions in --------------------------------------------------------

;; Rows (by = 'rows) or columns (by = 'columns) in any of the four nestings as
;; a design matrix. The outer sequence runs over the rows or columns and each
;; inner one along it; entry p of outer line o goes to o*outer-stride +
;; p*inner-stride of the column-major array.
(define (nested->dm xss by names who what)
  (define rows? (eq? by 'rows))
  (define-values (line other) (if rows? (values "row" "column") (values "column" "row")))
  (define n-outer (if (vector? xss) (vector-length xss) (length xss)))
  (when (zero? n-outer)
    (raise-arguments-error who (format "~a has no ~as" what line)))
  (define n-inner (line-length (if (vector? xss) (vector-ref xss 0) (car xss))))
  (when (zero? n-inner)
    (raise-arguments-error who (format "~a has no ~as" what other)))
  (define-values (no ni) (if rows? (values n-outer n-inner) (values n-inner n-outer)))
  (define-values (outer-stride inner-stride) (if rows? (values 1 no) (values no 1)))
  (define checked-names (check-column-names names ni who))
  (define labels (and checked-names (list->vector checked-names)))
  (define (column-label j) (if labels (vector-ref labels j) j))
  (define v (make-f64vector (* n-outer n-inner)))
  (define (ragged o xs)
    (raise-arguments-error who (format "~a has ~as of different lengths" what line)
                           line o "length" (line-length xs)
                           (format "length of ~a 0" line) n-inner))
  (define (entry x o p)
    (cond
      [(and (flonum? x) (fl< (flabs x) +inf.0)) x]
      [rows? (->finite-flonum x who what o (column-label p))]
      [else (->finite-flonum x who what p (column-label o))]))
  (define (store! xs o)
    (define start (* o outer-stride))
    (cond
      [(vector? xs)
       (for ([p (in-range (min (vector-length xs) n-inner))])
         (f64vector-set! v (+ start (* p inner-stride)) (entry (vector-ref xs p) o p)))
       (unless (= (vector-length xs) n-inner) (ragged o xs))]
      [else
       (let loop ([ys xs] [p 0] [k start])
         (cond
           [(null? ys) (unless (= p n-inner) (ragged o xs))]
           [(= p n-inner) (ragged o xs)]
           [else
            (f64vector-set! v k (entry (car ys) o p))
            (loop (cdr ys) (add1 p) (+ k inner-stride))]))]))
  (if (vector? xss)
      (for ([xs (in-vector xss)] [o (in-naturals)]) (store! xs o))
      (for ([xs (in-list xss)] [o (in-naturals)]) (store! xs o)))
  (design-matrix v no ni checked-names))

(define (line-length xs)
  (if (vector? xs) (vector-length xs) (length xs)))

(define (rows->design-matrix rows #:column-names [names #f])
  (nested->dm rows 'rows names 'rows->design-matrix "the matrix"))

(define (columns->design-matrix columns #:column-names [names #f])
  (nested->dm columns 'columns names 'columns->design-matrix "the matrix"))

(define (f64vector->design-matrix v nrows ncols #:column-names [names #f])
  (flat->design-matrix v nrows ncols names 'f64vector->design-matrix "the vector"))

;; flat->design-matrix's preconditions.
(define (flat-length-problem v nrows ncols)
  (define n (if (flvector? v) (flvector-length v) (f64vector-length v)))
  (or (= n (* nrows ncols))
      (list "the vector does not have nrows * ncols entries"
            (format "entries: ~a" n)
            (format "nrows * ncols: ~a" (* nrows ncols)))))

(define (adopt-problem v order adopt?)
  (or (not (eq? adopt? #t))
      (and (f64vector? v) (not (eq? order 'row-major)))
      (list "only a column-major f64vector can be adopted")))

(define (flat->design-matrix v nrows ncols names who what
                             #:order [order 'column-major]
                             #:adopt? [adopt? #f])
  (define checked-names (check-column-names names ncols who))
  (define row-major? (eq? order 'row-major))
  (define (not-finite k x)
    (define-values (i j)
      (cond
        [row-major? (quotient/remainder k ncols)]
        [else
         (define-values (j i) (quotient/remainder k nrows))
         (values i j)]))
    (element-error who what "not finite" x
                   #:row i #:column (if checked-names (list-ref checked-names j) j)))
  (define n (* nrows ncols))
  ;; The column-major position of entry k of v.
  (define (target k)
    (cond
      [row-major?
       (define-values (i j) (quotient/remainder k ncols))
       (+ i (* j nrows))]
      [else k]))
  (define data
    (cond
      [adopt?
       (for ([k (in-range n)])
         (define x (f64vector-ref v k))
         (unless (fl< (flabs x) +inf.0) (not-finite k x)))
       v]
      [else
       (define out (make-f64vector n))
       (define (store! k x)
         (unless (fl< (flabs x) +inf.0) (not-finite k x))
         (f64vector-set! out (target k) x))
       (if (flvector? v)
           (for ([x (in-flvector v)] [k (in-naturals)]) (store! k x))
           (for ([k (in-range n)]) (store! k (f64vector-ref v k))))
       out]))
  (design-matrix data nrows ncols checked-names))

;; --- conversions out -------------------------------------------------------

(define (design-matrix-ref dm i j)
  (f64vector-ref (design-matrix-data dm) (+ i (* j (design-matrix-nrows dm)))))

;; The rows of dm at the given indices, in that order, as a new design matrix
;; with the same column names. The entries are already valid, so they are
;; copied without being checked again.
(define (design-matrix-select-rows dm rows)
  (define v (design-matrix-data dm))
  (define no (design-matrix-nrows dm))
  (define ni (design-matrix-ncols dm))
  (for ([i (in-list rows)])
    (unless (< i no)
      (raise-range-error 'design-matrix-select-rows "design matrix" "row " i dm 0 (sub1 no))))
  (define m (length rows))
  (define out (make-f64vector (* m ni)))
  (for ([j (in-range ni)])
    (for ([i (in-list rows)]
          [k (in-naturals)])
      (f64vector-set! out (+ k (* j m)) (f64vector-ref v (+ i (* j no))))))
  (design-matrix out m ni (design-matrix-column-names dm)))

;; dm's rows (by = 'rows) or columns (by = 'columns), each an `inner` ('list
;; or 'vector) in an `outer` one: design-matrix->rows, design-matrix->columns
;; and glmnet/data/nested's design-matrix->nested.
(define (design-matrix->nested-lines dm by outer inner)
  (define v (design-matrix-data dm))
  (define no (design-matrix-nrows dm))
  (define ni (design-matrix-ncols dm))
  (define-values (n-outer n-inner outer-stride inner-stride)
    (if (eq? by 'rows)
        (values no ni 1 no)
        (values ni no no 1)))
  (define (line o)
    (define start (* o outer-stride))
    (define (entry p) (f64vector-ref v (+ start (* p inner-stride))))
    (if (eq? inner 'vector)
        (for/vector #:length n-inner ([p (in-range n-inner)]) (entry p))
        (build-list-from-end n-inner entry)))
  (if (eq? outer 'vector)
      (for/vector #:length n-outer ([o (in-range n-outer)]) (line o))
      (build-list-from-end n-outer line)))

;; (build-list n f) consed from its last element, with no reversal.
(define (build-list-from-end n f)
  (for/fold ([acc '()]) ([k (in-range (sub1 n) -1 -1)])
    (cons (f k) acc)))

(define (design-matrix->rows dm)
  (design-matrix->nested-lines dm 'rows 'list 'list))

(define (design-matrix->columns dm)
  (design-matrix->nested-lines dm 'columns 'list 'list))

(define (design-matrix->f64vector dm)
  (define v (design-matrix-data dm))
  (define n (f64vector-length v))
  (define out (make-f64vector n))
  (for ([k (in-range n)])
    (f64vector-set! out k (f64vector-ref v k)))
  out)

;; --- responses -------------------------------------------------------------

;; A list, vector, flvector or f64vector of reals as a fresh f64vector. With
;; `elem`, a flat contract, each entry must satisfy it once converted to a
;; flonum: the fitters check their responses on this copy, which cannot change
;; after the check, and an exact number that the conversion rounds, such as a
;; positive time that rounds to 0.0, fails it here.
(define (->response y who what [elem #f])
  (define n (one-dimensional-length y))
  (when (zero? n)
    (raise-arguments-error who (format "~a is empty" what)))
  (define v (make-f64vector n))
  (define elem? (and elem (flat-contract-predicate elem)))
  (define (store-flonum! fx x k)
    (unless (fl< (flabs fx) +inf.0)
      (element-error who what "not finite" x #:position k))
    (when (and elem? (not (elem? fx)))
      (element-error who what (format "not ~s as a flonum" (contract-name elem)) x #:position k))
    (f64vector-set! v k fx))
  (define (store! x k)
    (cond
      [(flonum? x) (store-flonum! x x k)]
      [(real? x) (store-flonum! (real->double-flonum x) x k)]
      [else (element-error who what "not a real number" x #:position k)]))
  (cond
    [(list? y) (for ([x (in-list y)] [k (in-naturals)]) (store! x k))]
    [(vector? y) (for ([x (in-vector y)] [k (in-naturals)]) (store! x k))]
    [(flvector? y) (for ([x (in-flvector y)] [k (in-naturals)]) (store-flonum! x x k))]
    [else (for ([k (in-range n)])
            (define x (f64vector-ref y k))
            (store-flonum! x x k))])
  v)

(define (response->f64vector y)
  (->response y 'response->f64vector "the response"))

;; --- the fitters' entry points (support submodule) ---------------------------

;; A fitter's matrix argument (named `what` in errors) as a design-matrix.
(define (as-design-matrix X who what)
  (if (design-matrix? X)
      X
      (nested->dm X 'rows #f who what)))

;; A fitter's response argument as an f64vector with one entry per
;; observation, each satisfying `elem` as a flonum when it is given.
(define (as-response y no who what [elem #f])
  (define n (one-dimensional-length y))
  (unless (= n no)
    (raise-arguments-error who (format "~a does not have one entry per row of X" what)
                           (format "length of ~a" what) n "rows of X" no))
  (->response y who what elem))

;; --- tables (#26) ------------------------------------------------------------

;; A table supplies named columns: a design matrix with column names, a hash
;; from name to column, or an association list of (name . column) pairs, where
;; a name is a string or a symbol and a column a list, vector, flvector or
;; f64vector. Names are compared as strings. Only the columns a caller selects
;; are checked for numbers, so a table can carry columns that no model reads.
(define (table? v)
  (cond
    [(design-matrix? v) (and (design-matrix-column-names v) #t)]
    [(hash? v)
     (and (positive? (hash-count v))
          (for/and ([(name column) (in-hash v)])
            (and (column-name? name) (column? column))))]
    [(pair? v)
     (and (list? v)
          (for/and ([entry (in-list v)])
            (and (pair? entry) (column-name? (car entry)) (column? (cdr entry)))))]
    [else #f]))

(define (column-name? v) (or (string? v) (symbol? v)))
(define column? one-dimensional?)

(define (column-name->string name)
  (string->immutable-string (if (symbol? name) (symbol->string name) name)))

;; The table's column names as strings, in its order; a hash has none, so its
;; names are sorted.
(define (table-names t who)
  (define names
    (cond
      [(design-matrix? t) (design-matrix-column-names t)]
      [(hash? t) (sort (map column-name->string (hash-keys t)) string<?)]
      [else (for/list ([entry (in-list t)]) (column-name->string (car entry)))]))
  (define dup (check-duplicates names))
  (when dup
    (raise-arguments-error who "the table has two columns with the same name" "name" dup))
  names)

(define (table-column-names t)
  (table-names t 'table-column-names))

;; The position of each of the table's columns by name, after checking that
;; each of `names` (strings) is one of them.
(define (column-positions t names who)
  (define available (table-names t who))
  (define position
    (for/hash ([name (in-list available)] [j (in-naturals)])
      (values name j)))
  (for ([name (in-list names)])
    (unless (hash-ref position name #f)
      (raise-arguments-error who "the table has no column with this name"
                             "column" name "columns of the table" available)))
  position)

;; The columns of table t with the given names, in that order, as a design
;; matrix with those column names.
(define (select-table-columns t names* who)
  (define names (map column-name->string names*))
  (define position (column-positions t names who))
  (cond
    [(design-matrix? t)
     (define v (design-matrix-data t))
     (define no (design-matrix-nrows t))
     (define ni (length names))
     (define out (make-f64vector (* no ni)))
     (for ([name (in-list names)]
           [j (in-naturals)])
       (define from (* no (hash-ref position name)))
       (for ([i (in-range no)])
         (f64vector-set! out (+ i (* j no)) (f64vector-ref v (+ from i)))))
     (design-matrix out no ni names)]
    [else
     (named-columns->dm (named-columns t names) names who)]))

;; The values of the columns of table t with the given names, in that order,
;; each a vector, as they are in the table: the formula front end (#53) reads
;; factors, whose values are strings, symbols or booleans, this way. Columns
;; with no rows are an error unless rows-required? is #f.
(define (select-table-values t names* who #:rows-required? [rows-required? #t])
  (define names (map column-name->string names*))
  (define position (column-positions t names who))
  (cond
    [(design-matrix? t)
     (define v (design-matrix-data t))
     (define no (design-matrix-nrows t))
     (for/list ([name (in-list names)])
       (define from (* no (hash-ref position name)))
       (for/vector #:length no ([i (in-range no)])
         (f64vector-ref v (+ from i))))]
    [else
     (define columns (named-columns t names))
     (check-column-lengths columns names who rows-required?)
     (for/list ([column (in-list columns)])
       (if (vector? column) column (one-dimensional->vector column)))]))

;; The columns of a hash or association list with the given names, which it
;; has.
(define (named-columns t names)
  (define by-name
    (if (hash? t)
        (for/hash ([(name column) (in-hash t)])
          (values (column-name->string name) column))
        (for/hash ([entry (in-list t)])
          (values (column-name->string (car entry)) (cdr entry)))))
  (for/list ([name (in-list names)]) (hash-ref by-name name)))

(define (check-column-lengths columns names who [rows-required? #t])
  (define no (one-dimensional-length (car columns)))
  (when (and rows-required? (zero? no))
    (raise-arguments-error who "the table has a column with no rows" "column" (car names)))
  (for ([column (in-list columns)]
        [name (in-list names)])
    (unless (= (one-dimensional-length column) no)
      (raise-arguments-error who "the table's columns have different lengths"
                             "column" name "length" (one-dimensional-length column)
                             (format "length of column ~s" (car names)) no))))

;; The values of the table's column `name`, a vector from select-table-values,
;; as an flvector.
(define (table-column->flvector entries name who)
  (for/flvector #:length (vector-length entries) ([x (in-vector entries)] [i (in-naturals)])
    (->finite-flonum x who "the table" i name)))

;; Columns with the given names as a design matrix; errors name the column by
;; its name.
(define (named-columns->dm columns names who)
  (check-column-lengths columns names who)
  (define no (one-dimensional-length (car columns)))
  (define ni (length columns))
  (define v (make-f64vector (* no ni)))
  (for ([column (in-list columns)]
        [name (in-list names)]
        [j (in-naturals)])
    (define (store! x i)
      (f64vector-set! v (+ i (* j no)) (->finite-flonum x who "the table" i name)))
    (cond
      [(list? column) (for ([x (in-list column)] [i (in-naturals)]) (store! x i))]
      [(vector? column) (for ([x (in-vector column)] [i (in-naturals)]) (store! x i))]
      [(flvector? column) (for ([x (in-flvector column)] [i (in-naturals)]) (store! x i))]
      [else (for ([i (in-range (f64vector-length column))]) (store! (f64vector-ref column i) i))]))
  (design-matrix v no ni names))

;; Columns computed from a table, such as the interactions of a formula (#53),
;; as a design matrix with the given names. Each column is an flvector of the
;; same length; an entry that is not finite, from an overflowing product, is an
;; error that names its column and row.
(define (flvectors->design-matrix columns names who #:what [what "the design matrix"])
  (define no (flvector-length (car columns)))
  (define ni (length columns))
  (define checked-names (check-column-names names ni who))
  (define v (make-f64vector (* no ni)))
  (for ([column (in-list columns)]
        [j (in-naturals)])
    (for ([x (in-flvector column)]
          [i (in-naturals)])
      (unless (fl< (flabs x) +inf.0)
        (element-error who what "not finite" x
                       #:row i #:column (if checked-names (list-ref checked-names j) j)))
      (f64vector-set! v (+ i (* j no)) x)))
  (design-matrix v no ni checked-names))

(define (table->design-matrix t [names (table-names t 'table->design-matrix)])
  (define dup (check-duplicates (map column-name->string names)))
  (when dup
    (raise-arguments-error 'table->design-matrix "a column is named twice" "name" dup))
  (select-table-columns t names 'table->design-matrix))
