#lang racket/base

;; Plots of glmnet results (#28), `(require glmnet/plot)`: the coefficient
;; path, as R glmnet 4.1.10's plot.glmnet (plotCoef) draws it, and the
;; cross-validation curve, as plot.cv.glmnet draws it. Each plot has a
;; procedure that returns its plot-lib renderers, to combine with others, and
;; one that returns the finished plot as a pict, with R's axis labels and top
;; axis, and can also write it to a file. A formula model (#26) is plotted
;; through the path or cross-validated path it holds, with its predictor names
;; as the curve labels. This package is separate from `glmnet` so that
;; plot-lib stays out of glmnet's dependencies.

(require racket/class
         racket/contract
         racket/format
         racket/list
         racket/math
         racket/path
         file/convertible
         (only-in racket/draw the-font-list)
         (only-in pict pict? pict-width text vl-append)
         plot/no-gui
         (only-in glmnet
                  glmnet-path?
                  glmnet-path-family
                  glmnet-path-lambda
                  glmnet-path-coefficients
                  glmnet-path-dev-ratio
                  glmnet-path-df
                  glmnet-cv?
                  glmnet-cv-path
                  glmnet-cv-lambda
                  glmnet-cv-cvm
                  glmnet-cv-cvup
                  glmnet-cv-cvlo
                  glmnet-cv-nzero
                  glmnet-cv-name
                  glmnet-cv-lambda-min
                  glmnet-cv-lambda-1se
                  design-matrix?
                  design-matrix-ncols
                  design-matrix-column-names
                  formula-model?
                  formula-model-fit
                  glmnet-model-predictor-names
                  glmnet-model-response-names))

(define model/c (or/c glmnet-path? glmnet-cv? formula-model?))
(define cv/c (or/c glmnet-cv? formula-model?))
(define xvar/c (or/c 'lambda 'norm 'dev))
(define sign-lambda/c (or/c -1 1))
(define label/c (or/c boolean? design-matrix? (listof (or/c string? symbol?))))
(define type-coef/c (or/c 'coef '2norm))
(define out-file/c (or/c #f path-string?))

(provide
 (contract-out
  [coefficient-path-renderers
   (->* (model/c)
        (#:xvar xvar/c
         #:sign-lambda sign-lambda/c
         #:label label/c
         #:type-coef type-coef/c
         #:response exact-nonnegative-integer?)
        (listof renderer2d?))]
  [plot-coefficient-path
   (->* (model/c)
        (#:xvar xvar/c
         #:sign-lambda sign-lambda/c
         #:label label/c
         #:type-coef type-coef/c
         #:width exact-positive-integer?
         #:height exact-positive-integer?
         #:title (or/c #f string?)
         #:out-file out-file/c)
        pict?)]
  [cv-renderers
   (->* (cv/c) (#:sign-lambda sign-lambda/c) (listof renderer2d?))]
  [plot-cv
   (->* (cv/c)
        (#:sign-lambda sign-lambda/c
         #:width exact-positive-integer?
         #:height exact-positive-integer?
         #:title (or/c #f string?)
         #:out-file out-file/c)
        pict?)]))

;; For the unit tests only; not part of the public API.
(module* support #f
  (provide constant-approx
           label-runs
           keep-apart
           path-panels
           panel-y-label))

;; --- R's conventions ---------------------------------------------------------

;; The first six colours of R's default palette, which matplot cycles through.
(define curve-colors
  '((0 0 0) (223 83 107) (97 208 79) (34 151 230) (40 226 229) (205 11 188)))

(define cv-point-color '(255 0 0))
(define cv-bar-color '(169 169 169))

;; The fraction of the data range that R adds at each end of an axis.
(define axis-padding 0.04)

;; R's approx(xs, ys, method = "constant", rule = 2, f = f) as a procedure of
;; the point v: the knots sorted by x, tied xs collapsed to the mean of their
;; ys; below the first knot or above the last, that knot's y; between two
;; knots, (1 - f) times the left y plus f times the right.
(define (constant-approx xs ys f)
  (define knots
    (for/vector ([group (in-list (group-by car (sort (map cons xs ys) < #:key car) =))])
      (cons (caar group) (/ (apply + (map cdr group)) (length group)))))
  (define n (vector-length knots))
  (define (x i) (car (vector-ref knots i)))
  (define (y i) (cdr (vector-ref knots i)))
  (lambda (v)
    (cond
      [(< v (x 0)) (y 0)]
      [(> v (x (sub1 n))) (y (sub1 n))]
      [else
       (let loop ([i 0] [j (sub1 n)])
         (cond
           [(< i (sub1 j))
            (define ij (quotient (+ i j) 2))
            (if (< v (x ij)) (loop i ij) (loop ij j))]
           [(= v (x j)) (y j)]
           [(= v (x i)) (y i)]
           [else (+ (if (= f 1) 0 (* (y i) (- 1 f)))
                    (if (= f 0) 0 (* (y j) f)))]))])))

;; A count for an axis label: an integer as one, anything else to at most two
;; decimal places.
(define (count->string v)
  (if (integer? v)
      (number->string (inexact->exact v))
      (~r v #:precision 2)))

(define (finite? x) (and (real? x) (rational? x)))

;; The range of the finite xs, widened at each end as R widens an axis; #f and
;; #f when it is empty or a single value, for plot-lib to choose.
(define (padded-range xs)
  (define finite (filter finite? xs))
  (cond
    [(null? finite) (values #f #f)]
    [else
     (define lo (apply min finite))
     (define hi (apply max finite))
     (define pad (* axis-padding (- hi lo)))
     (if (zero? pad) (values #f #f) (values (- lo pad) (+ hi pad)))]))

;; --- coefficient paths -------------------------------------------------------

;; One panel of a coefficient plot: per lambda, the coefficients drawn (a
;; vector with one entry per predictor) and the count on the top axis, and the
;; y-axis label.
(struct panel (coefficients df y-label))

(define (nonzero-counts betas)
  (for/vector #:length (vector-length betas) ([beta (in-vector betas)])
    (for/sum ([b (in-vector beta)]) (if (zero? b) 0 1))))

;; The panels of path p, as plot.glmnet, plot.multnet and plot.mrelnet draw
;; them: one for a single response; for the multinomial and multi-response
;; families, one per class or response ('coef), with that class's count of
;; nonzero coefficients on top, or one of each predictor's 2-norm across them
;; ('2norm), with the mean count over the classes, rounded to one decimal
;; place (multinomial), or the first response's count (multi-response). A
;; response is labelled by its name in `responses`, or else as y1, y2, ...
(define (path-panels p type-coef [responses #f])
  (define family (glmnet-path-family p))
  (define coefs (glmnet-path-coefficients p))
  (case family
    [(multinomial mgaussian)
     (define k (vector-length (vector-ref coefs 0)))
     (define per-response
       (for/list ([r (in-range k)])
         (for/vector #:length (vector-length coefs) ([groups (in-vector coefs)])
           (vector-ref groups r))))
     (define dfs (map nonzero-counts per-response))
     (case type-coef
       [(coef)
        (for/list ([betas (in-list per-response)]
                   [df (in-list dfs)]
                   [r (in-naturals)])
          (panel betas df (format "Coefficients: Response ~a"
                                  (cond
                                    [(eq? family 'multinomial) r]
                                    [responses (list-ref responses r)]
                                    [else (format "y~a" (add1 r))]))))]
       [(2norm)
        (define norms
          (for/vector #:length (vector-length coefs) ([groups (in-vector coefs)])
            (for/vector ([j (in-range (vector-length (vector-ref groups 0)))])
              (sqrt (for/sum ([beta (in-vector groups)]) (sqr (vector-ref beta j)))))))
        (define df
          (if (eq? family 'multinomial)
              (for/vector ([l (in-range (vector-length coefs))])
                (/ (round (* 10 (/ (for/sum ([d (in-list dfs)]) (vector-ref d l)) k))) 10))
              (car dfs)))
        (list (panel norms df "Coefficient 2Norms"))])]
    [else (list (panel coefs (glmnet-path-df p) "Coefficients"))]))

;; The sum of the absolute values of a coefficient vector, or of a vector of
;; them.
(define (l1-norm v)
  (for/sum ([b (in-vector v)])
    (if (vector? b) (l1-norm b) (abs b))))

;; Per lambda, its position on the x axis, R's `index`.
(define (x-positions p xvar sign-lambda)
  (case xvar
    [(lambda)
     (for/list ([l (in-vector (glmnet-path-lambda p))])
       (* sign-lambda (log (exact->inexact l))))]
    [(norm)
     (for/list ([beta (in-vector (glmnet-path-coefficients p))])
       (l1-norm beta))]
    [(dev) (vector->list (glmnet-path-dev-ratio p))]))

(define (x-label xvar sign-lambda)
  (case xvar
    [(lambda) (if (= sign-lambda -1) "-Log(λ)" "Log(λ)")]
    [(norm) "L1 Norm"]
    [(dev) "Fraction Deviance Explained"]))

;; Whether curve labels go at the right-hand end of the curves; R puts them at
;; the left for log lambda, where the last lambda is leftmost.
(define (labels-right? xvar sign-lambda)
  (not (and (eq? xvar 'lambda) (= sign-lambda 1))))

;; The label of predictor j (from 0): its name, or its position counting from
;; 1, as R labels it.
(define (label-names who label n)
  (define names
    (cond
      [(design-matrix? label)
       (unless (= (design-matrix-ncols label) n)
         (raise-arguments-error who "the design matrix does not have one column per predictor"
                                "columns" (design-matrix-ncols label) "predictors" n))
       (design-matrix-column-names label)]
      [(list? label)
       (unless (= (length label) n)
         (raise-arguments-error who "the labels do not have one entry per predictor"
                                "labels" (length label) "predictors" n))
       label]
      [else #f]))
  (lambda (j)
    (if names (~a (list-ref names j)) (number->string (add1 j)))))

;; The path or cross-validated path a model holds: the model itself, or a
;; formula model's fit, which must satisfy `kind?`.
(define (model-fit who model kind? what)
  (define fit (if (formula-model? model) (formula-model-fit model) model))
  (unless (kind? fit)
    (raise-arguments-error who (format "the formula model does not hold ~a" what)
                           "model" model))
  fit)

(define (path-or-cv? v) (or (glmnet-path? v) (glmnet-cv? v)))

;; The path to plot for a model of a coefficient plot.
(define (model-path who model)
  (define fit (model-fit who model path-or-cv? "a path or a cross-validated path"))
  (if (glmnet-cv? fit) (glmnet-cv-path fit) fit))

;; #t labels the curves of a model with named predictors, such as a formula
;; model, by name, and those of any other model by position.
(define (model-label model label)
  (if (eq? label #t)
      (or (glmnet-model-predictor-names model) #t)
      label))

;; The renderers of one panel: a line per predictor that is nonzero at some
;; lambda, coloured as matplot colours them, and, when labelled, its label at
;; the end of the path. R's plotCoef draws nothing when every coefficient is
;; zero and warns when only one is not.
(define (panel-renderers who pnl xs label labels-right?)
  (define betas (panel-coefficients pnl))
  (define n (vector-length (vector-ref betas 0)))
  (define which
    (for/list ([j (in-range n)]
               #:when (for/or ([beta (in-vector betas)]) (not (zero? (vector-ref beta j)))))
      j))
  (when (and (pair? which) (null? (cdr which)))
    (log-warning "~a: 1 or less nonzero coefficients; the plot is not meaningful" who))
  (define points
    (for/list ([x (in-list xs)]
               [beta (in-vector betas)]
               #:when (finite? x))
      (cons x beta)))
  (define curves
    (for/list ([j (in-list which)]
               [i (in-naturals)])
      (lines (for/list ([x+beta (in-list points)])
               (vector (car x+beta) (vector-ref (cdr x+beta) j)))
             #:color (list-ref curve-colors (modulo i (length curve-colors))))))
  (define labels
    (cond
      [(and label (pair? points))
       (define name (label-names who label n))
       (define end
         (apply (if labels-right? max min) (map car points)))
       (define last-beta (vector-ref betas (sub1 (vector-length betas))))
       (for/list ([j (in-list which)])
         (point-label (vector end (vector-ref last-beta j))
                      (name j)
                      #:anchor (if labels-right? 'left 'right)
                      #:size (curve-label-size)
                      #:point-size 0))]
      [else '()]))
  (append curves labels))

(define (curve-label-size) (* 3/4 (plot-font-size)))

(define (coefficient-path-renderers model
                                    #:xvar [xvar 'lambda]
                                    #:sign-lambda [sign-lambda -1]
                                    #:label [label #f]
                                    #:type-coef [type-coef 'coef]
                                    #:response [response 0])
  (define who 'coefficient-path-renderers)
  (define p (model-path who model))
  (define panels (path-panels p type-coef (glmnet-model-response-names model)))
  (unless (< response (length panels))
    (raise-arguments-error who "the path has no class or response with this index"
                           "response" response
                           "classes or responses" (length panels)))
  (panel-renderers who (list-ref panels response) (x-positions p xvar sign-lambda)
                   (model-label model label) (labels-right? xvar sign-lambda)))

;; The top axis of a coefficient plot, as plotCoef draws it: at the positions
;; of the bottom axis's ticks, the count read off the path by R's approx with
;; method "constant", taking the count of the lambda to the right of a
;; position except for log lambda, whose count is read to the left.
(define (count-ticks xs counts f)
  (define finite-points
    (for/list ([x (in-list xs)] [c (in-vector counts)] #:when (finite? x))
      (cons x c)))
  (cond
    [(null? finite-points) no-ticks]
    [else
     (define count-at (constant-approx (map car finite-points) (map cdr finite-points) f))
     ;; plot-lib merges neighbouring ticks that have the same label, and only
     ;; draws the labels of major ticks. Each minor tick gets a different blank
     ;; label, so that two major ticks with the same count stay apart.
     (ticks (ticks-layout (plot-x-ticks))
            (lambda (lo hi pre-ticks)
              (for/list ([t (in-list pre-ticks)]
                         [i (in-naturals 1)])
                (if (pre-tick-major? t)
                    (count->string (count-at (pre-tick-value t)))
                    (make-string i #\space)))))]))

(define (plot-coefficient-path model
                               #:xvar [xvar 'lambda]
                               #:sign-lambda [sign-lambda -1]
                               #:label [label #f]
                               #:type-coef [type-coef 'coef]
                               #:width [width (plot-width)]
                               #:height [height (plot-height)]
                               #:title [title (plot-title)]
                               #:out-file [out-file #f])
  (define who 'plot-coefficient-path)
  (define kind (and out-file (image-kind who out-file)))
  (define p (model-path who model))
  (define label* (model-label model label))
  (define xs (x-positions p xvar sign-lambda))
  (define right? (labels-right? xvar sign-lambda))
  (define f (if (and (eq? xvar 'lambda) (= sign-lambda 1)) 0 1))
  (define-values (x-min x-max) (padded-range xs))
  (define pictures
    (for*/list ([pnl (in-list (path-panels p type-coef (glmnet-model-response-names model)))]
                [renderers (in-value (panel-renderers who pnl xs label* right?))]
                #:unless (null? renderers))
      (define-values (y-min y-max)
        (padded-range (for*/list ([beta (in-vector (panel-coefficients pnl))]
                                  [b (in-vector beta)])
                        b)))
      (define (draw x-min x-max)
        (parameterize ([plot-x-far-ticks (count-ticks xs (panel-df pnl) f)])
          (plot-pict renderers
                     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                     #:width width #:height height #:title title
                     #:x-label (x-label xvar sign-lambda) #:y-label (panel-y-label pnl))))
      (define first-draw (draw x-min x-max))
      (if (and label* x-min)
          (let*-values ([(finite-xs) (filter finite? xs)]
                        [(end) (apply (if right? max min) finite-xs)]
                        [(x-min x-max) (room-for-labels first-draw x-min x-max end right?
                                                        (curve-label-width p label*))])
            (draw x-min x-max))
          first-draw)))
  (when (null? pictures)
    (raise-arguments-error who "every coefficient is zero at every λ, so there is nothing to plot"
                           "λ values" (vector-length (glmnet-path-lambda p))))
  (define picture (apply vl-append pictures))
  (when out-file (write-image picture out-file kind))
  picture)

;; The width in pixels of the widest curve label.
(define (curve-label-width p label)
  (define n (vector-length
             (let ([beta (vector-ref (glmnet-path-coefficients p) 0)])
               (if (vector? (vector-ref beta 0)) (vector-ref beta 0) beta))))
  (define name (label-names 'plot-coefficient-path label n))
  (for/fold ([w 0]) ([j (in-range n)])
    (max w (text-width (name j) (curve-label-size)))))

;; The width in pixels of s in the font plot-lib draws text with at `size`.
(define (text-width s size)
  (define face (plot-font-face))
  (define family (plot-font-family))
  (define points (max 1 (min 255 (exact-round size))))
  (define font
    (if face
        (send the-font-list find-or-create-font points face family 'normal 'normal)
        (send the-font-list find-or-create-font points family 'normal 'normal)))
  (pict-width (text s font)))

;; The x bounds widened, if need be, so that labels `width` pixels wide fit
;; between `end`, where the labels start, and the plot's edge. R clips them
;; instead. The plot area keeps its width of A pixels, so widening a range R
;; by e turns room r at that end into (r + e) A / (R + e) pixels.
(define (room-for-labels picture x-min x-max end right? width)
  (define ->dc (plot-pict-plot->dc picture))
  (define (px x) (vector-ref (->dc (vector x 0)) 0))
  (define area (- (px x-max) (px x-min)))
  (define range (- x-max x-min))
  (define room (if right? (- x-max end) (- end x-min)))
  (define need (+ width 4))
  (cond
    [(or (>= (* room (/ area range)) need) (>= need area)) (values x-min x-max)]
    [else
     (define e (/ (- (* need range) (* room area)) (- area need)))
     (if right?
         (values x-min (+ x-max e))
         (values (- x-min e) x-max))]))

;; --- cross-validation --------------------------------------------------------

(define (cv-renderers model #:sign-lambda [sign-lambda -1])
  (define cv (model-fit 'cv-renderers model glmnet-cv? "a cross-validated path"))
  (define (x-of l) (* sign-lambda (log (exact->inexact l))))
  (define rows
    (for/list ([l (in-vector (glmnet-cv-lambda cv))]
               [m (in-vector (glmnet-cv-cvm cv))]
               [up (in-vector (glmnet-cv-cvup cv))]
               [lo (in-vector (glmnet-cv-cvlo cv))]
               #:when (finite? (x-of l)))
      (list (x-of l) m up lo)))
  (append
   (list (error-bars (for/list ([row (in-list rows)])
                       (define-values (x m up lo) (apply values row))
                       (vector x (/ (+ up lo) 2) (/ (- up lo) 2)))
                     #:color cv-bar-color)
         (points (for/list ([row (in-list rows)])
                   (vector (car row) (cadr row)))
                 #:sym 'fullcircle
                 #:color cv-point-color
                 #:fill-color cv-point-color
                 #:size 4))
   (for/list ([l (in-list (list (glmnet-cv-lambda-min cv) (glmnet-cv-lambda-1se cv)))]
              #:when (finite? (x-of l)))
     (vrule (x-of l) #:style 'dot #:color '(0 0 0)))))

;; plot-lib merges neighbouring ticks that have the same label into one tick
;; at their mean position. The counts along the top of a CV plot are therefore
;; given one label per run of consecutive lambdas with the same count, at the
;; run's mean position. `x+labels` are (x . label) pairs in path order.
(define (label-runs x+labels)
  (define runs
    (for/fold ([runs '()]) ([x+label (in-list x+labels)])
      (if (and (pair? runs) (equal? (cdar (car runs)) (cdr x+label)))
          (cons (cons x+label (car runs)) (cdr runs))
          (cons (list x+label) runs))))
  (for/list ([run (in-list (reverse runs))])
    (cons (/ (apply + (map car run)) (length run)) (cdar run))))

;; R's axis() leaves out a label that would overlap the one before it, with a
;; gap of an "m" between them. Of the (x . label) pairs, those it would draw,
;; given the width of a label in pixels and the map from x to pixels.
(define (keep-apart x+labels width ->px gap)
  (define sorted (sort x+labels < #:key car))
  (for/fold ([kept '()] [right -inf.0] #:result (reverse kept))
            ([x+label (in-list sorted)])
    (define half (/ (width (cdr x+label)) 2))
    (define px (->px (car x+label)))
    (if (>= (- px half) (+ right gap))
        (values (cons x+label kept) (+ px half))
        (values kept right))))

;; Ticks at fixed positions with fixed labels. plot-lib hands the format
;; procedure the positions as exact numbers.
(define (fixed-ticks x+labels)
  (define label-of
    (for/hash ([x+label (in-list x+labels)])
      (values (inexact->exact (car x+label)) (cdr x+label))))
  (ticks (lambda (lo hi)
           (for/list ([x+label (in-list x+labels)])
             (pre-tick (car x+label) #t)))
         (lambda (lo hi pre-ticks)
           (for/list ([t (in-list pre-ticks)])
             (hash-ref label-of (pre-tick-value t))))))

(define (plot-cv model
                 #:sign-lambda [sign-lambda -1]
                 #:width [width (plot-width)]
                 #:height [height (plot-height)]
                 #:title [title (plot-title)]
                 #:out-file [out-file #f])
  (define who 'plot-cv)
  (define cv (model-fit who model glmnet-cv? "a cross-validated path"))
  (define kind (and out-file (image-kind who out-file)))
  (define renderers (cv-renderers cv #:sign-lambda sign-lambda))
  (define xs
    (for/list ([l (in-vector (glmnet-cv-lambda cv))])
      (* sign-lambda (log (exact->inexact l)))))
  (define-values (x-min x-max) (padded-range xs))
  (define-values (y-min y-max)
    (padded-range (append (vector->list (glmnet-cv-cvup cv)) (vector->list (glmnet-cv-cvlo cv)))))
  (define nzero-labels
    (label-runs (for/list ([x (in-list xs)]
                           [nz (in-vector (glmnet-cv-nzero cv))]
                           #:when (finite? x))
                  (cons x (count->string nz)))))
  (define (draw top)
    (parameterize ([plot-x-far-ticks (fixed-ticks top)])
      (plot-pict renderers
                 #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                 #:width width #:height height #:title title
                 #:x-label (if (= sign-lambda -1) "-Log(λ)" "Log(λ)")
                 #:y-label (glmnet-cv-name cv))))
  (define ->dc (plot-pict-plot->dc (draw nzero-labels)))
  (define picture
    (draw (keep-apart nzero-labels
                      (lambda (s) (text-width s (plot-font-size)))
                      (lambda (x) (vector-ref (->dc (vector x 0)) 0))
                      (text-width "m" (plot-font-size)))))
  (when out-file (write-image picture out-file kind))
  picture)

;; --- files -------------------------------------------------------------------

(define image-kinds
  '(("png" . png-bytes) ("pdf" . pdf-bytes) ("svg" . svg-bytes) ("eps" . eps-bytes)))

;; The convert format for the file's extension.
(define (image-kind who out-file)
  (define ext (path-get-extension out-file))
  (define kind
    (and ext (assoc (string-downcase (bytes->string/utf-8 (subbytes ext 1) #\?)) image-kinds)))
  (unless kind
    (raise-arguments-error who "the output file's extension is not .png, .pdf, .svg or .eps"
                           "out-file" out-file))
  (cdr kind))

(define (write-image picture out-file kind)
  (call-with-output-file out-file
    (lambda (out) (write-bytes (convert picture kind) out))
    #:exists 'truncate/replace)
  (void))
