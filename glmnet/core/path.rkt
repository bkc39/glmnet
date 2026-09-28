#lang racket/base

;; The regularization-path result shared by every family, plus the pieces each
;; family's `*-path` fitter uses: R's lambda handling (the automatic sequence,
;; the decreasing sort of a user sequence, fix.lam) and unpacking the shim's
;; column-major per-lambda outputs. Also how results print: a path as R's
;; print.glmnet table, a single fit (a path with one lambda) as one line.

(require racket/contract
         racket/format
         racket/math
         ffi/vector)

(provide (struct-out glmnet-path))

;; For the family modules and core/model.rkt only; not part of the public API.
(module* support #f
  (provide lambda-sequence/c
           lambda-min-ratio/c
           path-lambdas
           finish-lambdas
           unpack-vector
           unpack-columns
           unpack-column-groups
           unpack-intercept-groups
           count-nonzero
           count-nonzero-groups
           path-num-predictors
           write-point))

(struct glmnet-path (family lambda intercepts coefficients dev-ratio df num-passes)
  #:transparent
  #:property prop:custom-write
  (lambda (p port mode) (write-path p port)))

(define lambda-sequence/c (or/c #f (and/c (listof (>=/c 0)) pair?)))
(define lambda-min-ratio/c (or/c #f (and/c real? (>=/c 0) (</c 1))))

;; The shim's lambda arguments, as R's glmnet() derives them. A user sequence is
;; fitted largest first (R: rev(sort(lambda))), with flmin = 1. Otherwise glmnet
;; picks `nlambda` lambdas down to lambda-min-ratio * lambda_max, where the ratio
;; defaults to 0.01 when there are fewer observations than predictors and 1e-4
;; otherwise; the Fortran raises a ratio below 1e-6 to 1e-6, as in R. Returns
;; (values nlam flmin ulam).
(define (path-lambdas lambda nlambda lambda-min-ratio no ni)
  (cond
    [lambda
     (define sorted (sort (map exact->inexact lambda) >))
     (values (length sorted) 1.0 (list->f64vector sorted))]
    [else
     (define ratio (or lambda-min-ratio (if (< no ni) 0.01 1e-4)))
     (values nlambda (exact->inexact ratio) (make-f64vector nlambda 0.0))]))

;; The first `lmu` fitted lambdas. In automatic mode glmnet reports the first as
;; its `big` sentinel; R's fix.lam replaces it with the log-linear extrapolation
;; exp(2 log l2 - log l3), and leaves it alone when there are only two lambdas.
(define (finish-lambdas lambda-out lmu automatic?)
  (define lams (unpack-vector lambda-out lmu))
  (when (and automatic? (> lmu 2))
    (vector-set! lams 0 (exp (- (* 2 (log (vector-ref lams 1)))
                                (log (vector-ref lams 2))))))
  lams)

(define (unpack-vector v lmu)
  (for/vector #:length lmu ([m (in-range lmu)])
    (f64vector-ref v m)))

;; Column m (0-based) of an n-row column-major matrix, for m < lmu.
(define (unpack-columns v n lmu)
  (for/vector #:length lmu ([m (in-range lmu)])
    (for/vector #:length n ([j (in-range n)])
      (f64vector-ref v (+ j (* n m))))))

;; An (n, k, nlam) column-major array as, per lambda, k vectors of length n.
(define (unpack-column-groups v n k lmu)
  (for/vector #:length lmu ([m (in-range lmu)])
    (for/vector #:length k ([c (in-range k)])
      (for/vector #:length n ([j (in-range n)])
        (f64vector-ref v (+ j (* n (+ c (* k m)))))))))

;; A (k, nlam) column-major array as, per lambda, a vector of k values.
(define (unpack-intercept-groups v k lmu)
  (for/vector #:length lmu ([m (in-range lmu)])
    (for/vector #:length k ([c (in-range k)])
      (f64vector-ref v (+ c (* k m))))))

(define (count-nonzero coefficients)
  (for/vector #:length (vector-length coefficients) ([beta (in-vector coefficients)])
    (for/sum ([b (in-vector beta)]) (if (zero? b) 0 1))))

;; A predictor counts once if it is nonzero for any class or response.
(define (count-nonzero-groups coefficients)
  (for/vector #:length (vector-length coefficients) ([groups (in-vector coefficients)])
    (for/sum ([j (in-range (vector-length (vector-ref groups 0)))])
      (if (for/or ([beta (in-vector groups)]) (not (zero? (vector-ref beta j)))) 1 0))))

(define (path-num-predictors p)
  (define beta (vector-ref (glmnet-path-coefficients p) 0))
  (if (memq (glmnet-path-family p) '(multinomial mgaussian))
      (vector-length (vector-ref beta 0))
      (vector-length beta)))

;; --- printing ----------------------------------------------------------------

;; x to `digits` significant digits, as R's signif, in positional notation
;; unless that would need more than four leading zeros or `digits` places
;; before the point.
(define (signif x [digits 4])
  (cond
    [(zero? x) "0"]
    [else
     (define e (order-of-magnitude (inexact->exact (abs x))))
     (if (< -5 e digits)
         (~r x #:precision (max 0 (- digits 1 e)))
         (~r x #:notation 'exponential #:precision (sub1 digits)))]))

;; x rounded to `places` decimal places, with a rounded -0.0 shown as 0.
(define (fixed x places)
  (define scale (expt 10 places))
  (define r (/ (round (* x scale)) scale))
  (~r (if (zero? r) 0.0 r) #:precision `(= ,places)))

;; A single fit, from its one-lambda path: #<glmnet:binomial λ=0.04 dev=0.7134 nz=2/3>.
(define (write-point p port)
  (fprintf port "#<glmnet:~a λ=~a dev=~a nz=~a/~a>"
           (glmnet-path-family p)
           (signif (vector-ref (glmnet-path-lambda p) 0))
           (fixed (vector-ref (glmnet-path-dev-ratio p) 0) 4)
           (vector-ref (glmnet-path-df p) 0)
           (path-num-predictors p)))

;; A path, as R's print.glmnet prints its table: Df, round(100 * dev.ratio, 2)
;; and signif(lambda, 4), one row per fitted lambda under its 1-based index,
;; each column zapped and formatted to 5 significant digits as print.anova
;; (printCoefmat) does.
(define (write-path p port)
  (define columns
    (list (cons "Df" (r-format (for/list ([df (in-vector (glmnet-path-df p))])
                                 (exact->inexact df))))
          (cons "%Dev" (r-format (zapsmall (for/list ([dev (in-vector (glmnet-path-dev-ratio p))])
                                             (r-round (* dev 100.0) 2)))))
          (cons "Lambda" (r-format (zapsmall (for/list ([lam (in-vector (glmnet-path-lambda p))])
                                               (r-signif lam 4)))))))
  (define row-names
    (for/list ([i (in-range (vector-length (glmnet-path-lambda p)))])
      (number->string (add1 i))))
  (define name-width (apply max 0 (map string-length row-names)))
  (define widths
    (for/list ([column (in-list columns)])
      (apply max (map string-length column))))
  (fprintf port "#<glmnet-path:~a" (glmnet-path-family p))
  (for ([name (in-list (cons "" row-names))]
        [row (in-list (apply map list columns))])
    (newline port)
    (write-string (~a name #:min-width name-width) port)
    (for ([cell (in-list row)]
          [width (in-list widths)])
      (write-string " " port)
      (write-string (~a cell #:min-width width #:align 'right) port)))
  (write-string ">" port))

;; R's signif(x, digits) (nmath/fprec.c), in the same double arithmetic.
(define (r-signif x digits)
  (cond
    [(zero? x) x]
    [else
     (define e10 (- digits 1 (order-of-magnitude (inexact->exact (abs x)))))
     (define p10 (exact->inexact (expt 10 (abs e10))))
     (if (positive? e10)
         (/ (round (* x p10)) p10)
         (* (round (/ x p10)) p10))]))

;; R's round(x, places) for places >= 0 (nmath/fround.c): the nearer of x
;; rounded down and up, in double arithmetic, and x itself when it has no
;; digit to drop within 15 significant ones.
(define (r-round x places)
  (define a (abs x))
  (define pow10 (exact->inexact (expt 10 places)))
  (define x10 (* a pow10))
  (define down (/ (floor x10) pow10))
  (define up (/ (ceiling x10) pow10))
  (cond
    [(zero? places) (round x)]
    [(or (zero? x) (> (+ (* 0.301029995663981195 (+ 0.5 (binary-exponent a))) places) 15)) x]
    [else
     (define nearer
       (if (or (< (- up a) (- a down)) (and (= (- up a) (- a down)) (odd? (floor x10)))) up down))
     (if (negative? x) (- nearer) nearer)]))

;; floor(log2 a) for a positive double, as C's logb.
(define (binary-exponent a)
  (define q (inexact->exact a))
  (- (integer-length (numerator q)) (integer-length (denominator q))))

;; R's zapsmall(xs, 5) as of R 4.4: xs rounded to max(0, 5 - log10(max |x|))
;; decimal places, which round() takes to the nearest integer.
(define (zapsmall xs)
  (define mx (apply max 0.0 (map abs xs)))
  (define places (if (positive? mx) (max 0.0 (- 5 (log mx 10))) 5))
  (for/list ([x (in-list xs)])
    (r-round x (exact-floor (+ places 0.5)))))

;; R's format(xs, digits = 5) of finite doubles (formatReal in main/format.c
;; and EncodeReal0 in main/printutils.c): every element in fixed notation with
;; the same number of decimals, or every element in scientific notation when
;; that is narrower.
(define (r-format xs [digits 5])
  (define-values (neg max-left min-left right max-signed-left max-sig)
    (for/fold ([neg 0] [max-left -inf.0] [min-left +inf.0] [right 0] [max-signed-left 1] [max-sig 1])
              ([x (in-list xs)])
      (define-values (sign left nsig) (significant-digits x digits))
      (values (max neg sign) (max max-left left) (min min-left left) (max right (- nsig left))
              (max max-signed-left (+ sign (max 1 left))) (max max-sig nsig))))
  (define width-fixed
    (+ (if (< max-left 0) (+ 1 neg) max-signed-left) right (if (zero? right) 0 1)))
  (define mantissa-places (sub1 max-sig))
  (define width-scientific
    (+ neg (if (zero? mantissa-places) 0 1) mantissa-places 4
       (if (or (> max-left 100) (<= min-left -99)) 2 1)))
  (for/list ([x (in-list xs)])
    (if (<= width-fixed width-scientific)
        (fixed-notation x right)
        (scientific-notation x mantissa-places))))

;; R's scientific() at `digits` significant digits: x's sign (1 if negative),
;; its digits left of the point in fixed notation, and how many significant
;; digits it needs.
(define (significant-digits x digits)
  (cond
    [(zero? x) (values 0 1 1)]
    [else
     (define r (abs (inexact->exact x)))
     (define kp (- (order-of-magnitude r) digits -1))
     (define zeros
       (let loop ([alpha (round (/ r (expt 10 kp)))] [z 0])
         (if (and (< z digits) (zero? (remainder alpha 10)))
             (loop (quotient alpha 10) (add1 z))
             z)))
     (define rounds-up? (= zeros digits))
     (define kpower (+ kp digits -1 (if rounds-up? 1 0)))
     (define fuzz (/ 1/2 (expt 10 (max 0 (min 27 (- digits kpower))))))
     (define widens? (and (< 0 kpower 28) (< r (- (expt 10 kpower) fuzz))))
     (values (if (negative? x) 1 0)
             (if widens? kpower (add1 kpower))
             (if rounds-up? 1 (- digits zeros)))]))

;; C's printf("%.*f", places, x).
(define (fixed-notation x places)
  (define digits
    (~a (round (* (abs (inexact->exact x)) (expt 10 places)))
        #:min-width (add1 places) #:align 'right #:left-pad-string "0"))
  (define point (- (string-length digits) places))
  (string-append (if (negative? x) "-" "")
                 (substring digits 0 point)
                 (if (zero? places) "" ".")
                 (substring digits point)))

;; C's printf("%#.*e", places, x), and printf("%.0e", x) when places is 0.
(define (scientific-notation x places)
  (define r (abs (inexact->exact x)))
  (define-values (mantissa exponent)
    (cond
      [(zero? r) (values 0 0)]
      [else
       (define e (order-of-magnitude r))
       (define m (round (* r (expt 10 (- places e)))))
       (if (< m (expt 10 (add1 places)))
           (values m e)
           (values (round (* r (expt 10 (- places e 1)))) (add1 e)))]))
  (define digits (~a mantissa #:min-width (add1 places) #:align 'right #:left-pad-string "0"))
  (string-append (if (negative? x) "-" "")
                 (substring digits 0 1)
                 (if (zero? places) "" ".")
                 (substring digits 1)
                 (if (negative? exponent) "e-" "e+")
                 (~a (abs exponent) #:min-width 2 #:align 'right #:left-pad-string "0")))

