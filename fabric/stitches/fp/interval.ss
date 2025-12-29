;;; fabric/stitches/fp/interval.ss -- Interval Arithmetic Library
;;;
;;; Provides interval type and arithmetic operations with guaranteed bounds.
;;; Intervals represent sets of real numbers [lo, hi] and propagate uncertainty.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Type:
;;;   Interval = [lo, hi] where lo <= hi
;;;
;;; Operations:
;;;   Arithmetic: interval-add, interval-sub, interval-mul, interval-div
;;;   Predicates: interval?, interval-contains?, interval-overlaps?
;;;   Derived: interval-width, interval-midpoint, interval-radius
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/numeric.ss (for Num type class integration)

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/numeric.ss")

;;; ============================================================
;;; Interval Type
;;; ============================================================

;;; make-interval : Real -> Real -> Interval
;;; Create an interval [lo, hi]. Ensures lo <= hi.
(define (make-interval lo hi)
  (if (<= lo hi)
      (vector 'interval lo hi)
      (vector 'interval hi lo)))  ; Swap if needed

;;; interval : Real -> Real -> Interval
;;; Convenience constructor (same as make-interval).
(define interval make-interval)

;;; interval-singleton : Real -> Interval
;;; Create a point interval [x, x].
(define (interval-singleton x)
  (make-interval x x))

;;; interval? : Any -> Boolean
(define (interval? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) 'interval)))

;;; interval-lo : Interval -> Real
;;; Get the lower bound.
(define (interval-lo iv)
  (vector-ref iv 1))

;;; interval-hi : Interval -> Real
;;; Get the upper bound.
(define (interval-hi iv)
  (vector-ref iv 2))

;;; ============================================================
;;; Interval Properties
;;; ============================================================

;;; interval-width : Interval -> Real
;;; Width of the interval (hi - lo).
(define (interval-width iv)
  (- (interval-hi iv) (interval-lo iv)))

;;; interval-midpoint : Interval -> Real
;;; Center of the interval ((lo + hi) / 2).
(define (interval-midpoint iv)
  (/ (+ (interval-lo iv) (interval-hi iv)) 2))

;;; interval-radius : Interval -> Real
;;; Half-width of the interval (width / 2).
(define (interval-radius iv)
  (/ (interval-width iv) 2))

;;; interval-empty? : Interval -> Boolean
;;; An interval is empty if lo > hi (never happens with our constructor).
(define (interval-empty? iv)
  (> (interval-lo iv) (interval-hi iv)))

;;; interval-singleton? : Interval -> Boolean
;;; True if interval is a point [x, x].
(define (interval-singleton? iv)
  (= (interval-lo iv) (interval-hi iv)))

;;; interval-positive? : Interval -> Boolean
;;; True if entire interval is positive.
(define (interval-positive? iv)
  (> (interval-lo iv) 0))

;;; interval-negative? : Interval -> Boolean
;;; True if entire interval is negative.
(define (interval-negative? iv)
  (< (interval-hi iv) 0))

;;; interval-contains-zero? : Interval -> Boolean
;;; True if interval contains zero.
(define (interval-contains-zero? iv)
  (and (<= (interval-lo iv) 0) (>= (interval-hi iv) 0)))

;;; ============================================================
;;; Interval Predicates
;;; ============================================================

;;; interval-contains? : Interval -> Real -> Boolean
;;; True if x is within the interval.
(define (interval-contains? iv x)
  (and (<= (interval-lo iv) x)
       (<= x (interval-hi iv))))

;;; interval-contains-interval? : Interval -> Interval -> Boolean
;;; True if iv1 completely contains iv2.
(define (interval-contains-interval? iv1 iv2)
  (and (<= (interval-lo iv1) (interval-lo iv2))
       (>= (interval-hi iv1) (interval-hi iv2))))

;;; interval-overlaps? : Interval -> Interval -> Boolean
;;; True if intervals overlap (share at least one point).
(define (interval-overlaps? iv1 iv2)
  (and (<= (interval-lo iv1) (interval-hi iv2))
       (<= (interval-lo iv2) (interval-hi iv1))))

;;; interval-disjoint? : Interval -> Interval -> Boolean
;;; True if intervals do not overlap.
(define (interval-disjoint? iv1 iv2)
  (not (interval-overlaps? iv1 iv2)))

;;; ============================================================
;;; Interval Arithmetic - Core Operations
;;; ============================================================

;;; interval-add : Interval -> Interval -> Interval
;;; [a, b] + [c, d] = [a+c, b+d]
(define (interval-add iv1 iv2)
  (make-interval (+ (interval-lo iv1) (interval-lo iv2))
                 (+ (interval-hi iv1) (interval-hi iv2))))

;;; interval-negate : Interval -> Interval
;;; -[a, b] = [-b, -a]
(define (interval-negate iv)
  (make-interval (- (interval-hi iv))
                 (- (interval-lo iv))))

;;; interval-sub : Interval -> Interval -> Interval
;;; [a, b] - [c, d] = [a-d, b-c]
(define (interval-sub iv1 iv2)
  (make-interval (- (interval-lo iv1) (interval-hi iv2))
                 (- (interval-hi iv1) (interval-lo iv2))))

;;; interval-mul : Interval -> Interval -> Interval
;;; Product of intervals - must consider all endpoint combinations.
;;; [a, b] * [c, d] = [min(ac, ad, bc, bd), max(ac, ad, bc, bd)]
(define (interval-mul iv1 iv2)
  (let* ([a (interval-lo iv1)]
         [b (interval-hi iv1)]
         [c (interval-lo iv2)]
         [d (interval-hi iv2)]
         [ac (* a c)]
         [ad (* a d)]
         [bc (* b c)]
         [bd (* b d)]
         [lo (min ac (min ad (min bc bd)))]
         [hi (max ac (max ad (max bc bd)))])
        (make-interval lo hi)))

;;; interval-reciprocal : Interval -> Maybe Interval
;;; 1/[a, b] = [1/b, 1/a] if 0 not in [a, b]
;;; Returns nothing if interval contains zero.
(define (interval-reciprocal iv)
  (if (interval-contains-zero? iv)
      nothing
      (just (make-interval (/ 1 (interval-hi iv))
                           (/ 1 (interval-lo iv))))))

;;; interval-div : Interval -> Interval -> Maybe Interval
;;; [a, b] / [c, d] = [a, b] * (1/[c, d])
;;; Returns nothing if divisor contains zero.
(define (interval-div iv1 iv2)
  (let ([recip (interval-reciprocal iv2)])
       (if (just? recip)
           (just (interval-mul iv1 (from-just recip)))
           nothing)))

;;; interval-div-unsafe : Interval -> Interval -> Interval
;;; Division without zero-checking (use when you know it's safe).
(define (interval-div-unsafe iv1 iv2)
  (interval-mul iv1 (make-interval (/ 1 (interval-hi iv2))
                                   (/ 1 (interval-lo iv2)))))

;;; ============================================================
;;; Scalar-Interval Operations
;;; ============================================================

;;; interval-scale : Real -> Interval -> Interval
;;; k * [a, b] = [k*a, k*b] if k >= 0, [k*b, k*a] if k < 0
(define (interval-scale k iv)
  (if (>= k 0)
      (make-interval (* k (interval-lo iv))
                     (* k (interval-hi iv)))
      (make-interval (* k (interval-hi iv))
                     (* k (interval-lo iv)))))

;;; interval-add-scalar : Interval -> Real -> Interval
;;; [a, b] + k = [a+k, b+k]
(define (interval-add-scalar iv k)
  (make-interval (+ (interval-lo iv) k)
                 (+ (interval-hi iv) k)))

;;; interval-sub-scalar : Interval -> Real -> Interval
;;; [a, b] - k = [a-k, b-k]
(define (interval-sub-scalar iv k)
  (make-interval (- (interval-lo iv) k)
                 (- (interval-hi iv) k)))

;;; ============================================================
;;; Interval Set Operations
;;; ============================================================

;;; interval-union : Interval -> Interval -> Interval
;;; Smallest interval containing both.
(define (interval-union iv1 iv2)
  (make-interval (min (interval-lo iv1) (interval-lo iv2))
                 (max (interval-hi iv1) (interval-hi iv2))))

;;; interval-hull : (List Interval) -> Maybe Interval
;;; Smallest interval containing all intervals in the list.
(define (interval-hull intervals)
  (if (null? intervals)
      nothing
      (just (fold-left interval-union (car intervals) (cdr intervals)))))

;;; interval-intersection : Interval -> Interval -> Maybe Interval
;;; Returns the overlap, or nothing if disjoint.
(define (interval-intersection iv1 iv2)
  (let ([lo (max (interval-lo iv1) (interval-lo iv2))]
        [hi (min (interval-hi iv1) (interval-hi iv2))])
       (if (<= lo hi)
           (just (make-interval lo hi))
           nothing)))

;;; ============================================================
;;; Interval Power Operations
;;; ============================================================

;;; interval-square : Interval -> Interval
;;; [a, b]^2 - special case for better bounds
;;; Must handle zero-crossing carefully.
(define (interval-square iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
       (cond
        ;; Both positive: [a^2, b^2]
        [(>= a 0) (make-interval (* a a) (* b b))]
        ;; Both negative: [b^2, a^2]
        [(<= b 0) (make-interval (* b b) (* a a))]
        ;; Zero crossing: [0, max(a^2, b^2)]
        [else (make-interval 0 (max (* a a) (* b b)))])))

;;; interval-pow-nat : Interval -> Nat -> Interval
;;; Interval raised to a natural number power.
(define (interval-pow-nat iv n)
  (cond
   [(= n 0) (interval-singleton 1)]
   [(= n 1) iv]
   [(= n 2) (interval-square iv)]
   [(even? n)
    ;; Even power: x^n >= 0, special handling for zero-crossing
    (let* ([half-pow (interval-pow-nat iv (/ n 2))])
          (interval-square half-pow))]
   [else
    ;; Odd power: preserve sign
    (let* ([half-pow (interval-pow-nat iv (quotient n 2))]
           [squared (interval-square half-pow)])
          (interval-mul squared iv))]))

;;; interval-abs : Interval -> Interval
;;; Absolute value of interval.
(define (interval-abs iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
       (cond
        [(>= a 0) iv]  ; Already positive
        [(<= b 0) (make-interval (- b) (- a))]  ; Negate
        ;; Zero-crossing: [0, max(|a|, |b|)]
        [else (make-interval 0 (max (- a) b))])))

;;; ============================================================
;;; Interval Comparisons
;;; ============================================================

;;; interval-lt? : Interval -> Interval -> Boolean
;;; Definitely less than: iv1 < iv2 iff hi1 < lo2
(define (interval-lt? iv1 iv2)
  (< (interval-hi iv1) (interval-lo iv2)))

;;; interval-gt? : Interval -> Interval -> Boolean
;;; Definitely greater than: iv1 > iv2 iff lo1 > hi2
(define (interval-gt? iv1 iv2)
  (> (interval-lo iv1) (interval-hi iv2)))

;;; interval-le? : Interval -> Interval -> Boolean
;;; Possibly less than or equal: iv1 <= iv2 iff lo1 <= hi2
(define (interval-le? iv1 iv2)
  (<= (interval-lo iv1) (interval-hi iv2)))

;;; interval-ge? : Interval -> Interval -> Boolean
;;; Possibly greater than or equal: iv1 >= iv2 iff hi1 >= lo2
(define (interval-ge? iv1 iv2)
  (>= (interval-hi iv1) (interval-lo iv2)))

;;; interval-equal? : Interval -> Interval -> Boolean
;;; Exact equality (both bounds equal).
(define (interval-equal? iv1 iv2)
  (and (= (interval-lo iv1) (interval-lo iv2))
       (= (interval-hi iv1) (interval-hi iv2))))

;;; interval-approx-equal? : Interval -> Interval -> Real -> Boolean
;;; Approximate equality within tolerance.
(define (interval-approx-equal? iv1 iv2 tol)
  (and (< (abs (- (interval-lo iv1) (interval-lo iv2))) tol)
       (< (abs (- (interval-hi iv1) (interval-hi iv2))) tol)))

;;; ============================================================
;;; Interval Min/Max
;;; ============================================================

;;; interval-min : Interval -> Interval -> Interval
;;; Interval containing all possible minimums.
(define (interval-min iv1 iv2)
  (make-interval (min (interval-lo iv1) (interval-lo iv2))
                 (min (interval-hi iv1) (interval-hi iv2))))

;;; interval-max : Interval -> Interval -> Interval
;;; Interval containing all possible maximums.
(define (interval-max iv1 iv2)
  (make-interval (max (interval-lo iv1) (interval-lo iv2))
                 (max (interval-hi iv1) (interval-hi iv2))))

;;; ============================================================
;;; Display/Conversion
;;; ============================================================

;;; interval->list : Interval -> (List Real Real)
(define (interval->list iv)
  (list (interval-lo iv) (interval-hi iv)))

;;; interval->string : Interval -> String
(define (interval->string iv)
  (string-append "[" (number->string (interval-lo iv))
                 ", " (number->string (interval-hi iv)) "]"))

;;; ============================================================
;;; Num Type Class Instance
;;; ============================================================

;;; interval-num : Num Interval
;;; Interval satisfies Num type class.
(define interval-num
  (make-num
   interval-add
   interval-sub
   interval-mul
   interval-negate
   interval-abs
   (lambda (iv) ; signum - return interval of possible signs
           (cond
            [(interval-positive? iv) (interval-singleton 1)]
            [(interval-negative? iv) (interval-singleton -1)]
            [else (make-interval -1 1)]))  ; Could be any sign
   (lambda (n) (interval-singleton n))))   ; from-integer

;;; ============================================================
;;; Common Interval Constants
;;; ============================================================

;;; interval-zero : Interval
(define interval-zero (interval-singleton 0))

;;; interval-one : Interval
(define interval-one (interval-singleton 1))

;;; interval-unit : Interval
;;; The unit interval [0, 1].
(define interval-unit (make-interval 0 1))

;;; interval-symmetric-unit : Interval
;;; The symmetric unit interval [-1, 1].
(define interval-symmetric-unit (make-interval -1 1))

;;; interval-positive-reals : Interval
;;; Approximation of positive reals (0, +inf).
(define interval-positive-reals (make-interval 1e-308 1e308))

;;; interval-negative-reals : Interval
;;; Approximation of negative reals (-inf, 0).
(define interval-negative-reals (make-interval -1e308 -1e-308))

;;; ============================================================
;;; Interval Splitting
;;; ============================================================

;;; interval-split : Interval -> (Interval . Interval)
;;; Split interval at midpoint.
(define (interval-split iv)
  (let ([mid (interval-midpoint iv)])
       (cons (make-interval (interval-lo iv) mid)
             (make-interval mid (interval-hi iv)))))

;;; interval-bisect : Interval -> Real -> Maybe (Interval . Interval)
;;; Split interval at point x (must be within interval).
(define (interval-bisect iv x)
  (if (interval-contains? iv x)
      (just (cons (make-interval (interval-lo iv) x)
                  (make-interval x (interval-hi iv))))
      nothing))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Type:
;;;   make-interval, interval, interval-singleton, interval?
;;;   interval-lo, interval-hi
;;;
;;; Properties:
;;;   interval-width, interval-midpoint, interval-radius
;;;   interval-empty?, interval-singleton?
;;;   interval-positive?, interval-negative?, interval-contains-zero?
;;;
;;; Predicates:
;;;   interval-contains?, interval-contains-interval?
;;;   interval-overlaps?, interval-disjoint?
;;;
;;; Arithmetic:
;;;   interval-add, interval-sub, interval-mul, interval-div
;;;   interval-negate, interval-reciprocal, interval-div-unsafe
;;;   interval-scale, interval-add-scalar, interval-sub-scalar
;;;
;;; Set Operations:
;;;   interval-union, interval-hull, interval-intersection
;;;
;;; Power:
;;;   interval-square, interval-pow-nat, interval-abs
;;;
;;; Comparisons:
;;;   interval-lt?, interval-gt?, interval-le?, interval-ge?
;;;   interval-equal?, interval-approx-equal?
;;;   interval-min, interval-max
;;;
;;; Conversion:
;;;   interval->list, interval->string
;;;
;;; Constants:
;;;   interval-zero, interval-one, interval-unit
;;;   interval-symmetric-unit, interval-positive-reals
;;;
;;; Splitting:
;;;   interval-split, interval-bisect
;;;
;;; Type Class:
;;;   interval-num
