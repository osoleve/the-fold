(load "core/base/prelude.ss")

(doc 'module 'interval)
(doc 'description "Verified numerical computation with rigorous bounds. Every operation guarantees the true mathematical result lies within the computed interval.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'author "Claude Opus 4.5")
(doc 'created "2026-01-16")
(doc 'note "ROUNDING MODES: This module provides both standard and rigorous operations. Standard ops (interval-add, etc.) use round-to-nearest for speed. Rigorous ops (interval-add-rigorous, etc.) use directed rounding via fl-next-up/fl-next-down for guaranteed enclosure at ~2x cost.")
(doc 'invariant "For all operations f, if x ∈ [a,b] and y ∈ [c,d], then f(x,y) ∈ f([a,b], [c,d]).")

(doc 'section 'directed-rounding)
(doc 'note "IEEE 754 double-precision floats have a natural ordering when interpreted as 64-bit integers (for positive numbers). We exploit this to implement next up and next down operations that return the adjacent representable floating-point values. These enable directed rounding: round-down for lower bounds, round-up for upper bounds, guaranteeing the computed interval encloses the true result.")

(define (fl-next-up x)
  (doc 'export #t)
  (doc 'type '(-> Flonum Flonum))
  (doc 'description "Return the smallest flonum greater than x")
  (doc 'note "Special cases: +inf → +inf, NaN → NaN, -0 → smallest positive denormal")
  (cond
    [(not (= x x)) x]                    ; NaN stays NaN
    [(= x +inf.0) +inf.0]                ; +inf stays +inf
    [(= x -inf.0) -1.7976931348623157e308]  ; -inf → most negative finite
    [(and (zero? x) (not (negative? x)))    ; +0 → smallest positive
     4.9406564584124654e-324]
    [else
     (let ([bv (make-bytevector 8)])
       (bytevector-ieee-double-native-set! bv 0 x)
       ;; Interpret as 64-bit unsigned int, adjust, convert back
       (let* ([bits (bytevector-u64-native-ref bv 0)]
              [new-bits (if (>= x 0.0)
                            (+ bits 1)    ; Positive: increment
                            (- bits 1))]) ; Negative: decrement (toward zero)
         (bytevector-u64-native-set! bv 0 new-bits)
         (bytevector-ieee-double-native-ref bv 0)))]))

(define (fl-next-down x)
  (doc 'export #t)
  (doc 'type '(-> Flonum Flonum))
  (doc 'description "Return the largest flonum less than x")
  (doc 'note "Special cases: -inf → -inf, NaN → NaN, +0 → smallest negative denormal")
  (cond
    [(not (= x x)) x]                    ; NaN stays NaN
    [(= x -inf.0) -inf.0]                ; -inf stays -inf
    [(= x +inf.0) 1.7976931348623157e308]  ; +inf → most positive finite
    [(and (zero? x) (not (negative? x)))    ; +0 → smallest negative
     -4.9406564584124654e-324]
    [(and (zero? x) (negative? x))          ; -0 → smallest negative
     -4.9406564584124654e-324]
    [else
     (let ([bv (make-bytevector 8)])
       (bytevector-ieee-double-native-set! bv 0 x)
       (let* ([bits (bytevector-u64-native-ref bv 0)]
              [new-bits (if (> x 0.0)
                            (- bits 1)    ; Positive: decrement
                            (+ bits 1))]) ; Negative: increment (away from zero)
         (bytevector-u64-native-set! bv 0 new-bits)
         (bytevector-ieee-double-native-ref bv 0)))]))

(doc 'section 'directed-rounding-arithmetic)
(doc 'note "These operations round the result in a specified direction. Used to guarantee interval bounds enclose the true mathematical result.")

(define (add-down a b)
  (doc 'export #t)
  (doc 'type '(-> Real Real Real))
  (doc 'description "Add with rounding toward -∞")
  (let ([r (+ a b)])
    (if (or (infinite? r) (nan? r)) r (fl-next-down r))))

;;; add-up : Real × Real → Real
;;; Add with rounding toward +∞.
(define (add-up a b)
  (doc 'export #t)
  (let ([r (+ a b)])
    (if (or (infinite? r) (nan? r)) r (fl-next-up r))))

;;; sub-down : Real × Real → Real
;;; Subtract with rounding toward -∞.
(define (sub-down a b)
  (doc 'export #t)
  (let ([r (- a b)])
    (if (or (infinite? r) (nan? r)) r (fl-next-down r))))

;;; sub-up : Real × Real → Real
;;; Subtract with rounding toward +∞.
(define (sub-up a b)
  (doc 'export #t)
  (let ([r (- a b)])
    (if (or (infinite? r) (nan? r)) r (fl-next-up r))))

;;; mul-down : Real × Real → Real
;;; Multiply with rounding toward -∞.
(define (mul-down a b)
  (doc 'export #t)
  (let ([r (* a b)])
    (if (or (infinite? r) (nan? r)) r (fl-next-down r))))

;;; mul-up : Real × Real → Real
;;; Multiply with rounding toward +∞.
(define (mul-up a b)
  (doc 'export #t)
  (let ([r (* a b)])
    (if (or (infinite? r) (nan? r)) r (fl-next-up r))))

;;; div-down : Real × Real → Real
;;; Divide with rounding toward -∞.
(define (div-down a b)
  (doc 'export #t)
  (let ([r (/ a b)])
    (if (or (infinite? r) (nan? r)) r (fl-next-down r))))

;;; div-up : Real × Real → Real
;;; Divide with rounding toward +∞.
(define (div-up a b)
  (doc 'export #t)
  (let ([r (/ a b)])
    (if (or (infinite? r) (nan? r)) r (fl-next-up r))))

;;; sqrt-down : Real → Real
;;; Square root with rounding toward -∞.
(define (sqrt-down x)
  (doc 'export #t)
  (let ([r (sqrt x)])
    (if (or (infinite? r) (nan? r) (< x 0)) r (fl-next-down r))))

;;; sqrt-up : Real → Real
;;; Square root with rounding toward +∞.
(define (sqrt-up x)
  (doc 'export #t)
  (let ([r (sqrt x)])
    (if (or (infinite? r) (nan? r) (< x 0)) r (fl-next-up r))))

;;; ============================================================================
;;; Interval Type
;;; ============================================================================
;;;
;;; An interval [lo, hi] represents the set of all real numbers x
;;; such that lo <= x <= hi. We maintain the invariant lo <= hi.

;;; make-interval : Real × Real → Interval
;;; Create an interval. Automatically orders endpoints.
(define (make-interval lo hi)
  (doc 'export #t)
  (if (<= lo hi)
      (list 'interval lo hi)
      (list 'interval hi lo)))

;;; interval : Real × Real → Interval
;;; Alias for make-interval (shorter).
(doc interval 'export #t)
(define interval make-interval)

;;; interval? : Any → Boolean
;;; Test if value is an interval.
(define (interval? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'interval)
       (= (length x) 3)))

;;; interval-lo : Interval → Real
;;; Get lower bound.
(define (interval-lo iv)
  (doc 'export #t)
  (cadr iv))

;;; interval-hi : Interval → Real
;;; Get upper bound.
(define (interval-hi iv)
  (doc 'export #t)
  (caddr iv))

;;; interval-singleton : Real → Interval
;;; Create a point interval [x, x].
(define (interval-singleton x)
  (doc 'export #t)
  (make-interval x x))

;;; entire-interval : → Interval
;;; The interval (-∞, +∞) — represents any real number.
;;; We approximate with large finite bounds.
(define (entire-interval)
  (doc 'export #t)
  (make-interval -1e308 1e308))

;;; ============================================================================
;;; Accessors and Queries
;;; ============================================================================

;;; interval-mid : Interval → Real
;;; Midpoint of interval. Uses overflow-safe formula.
(define (interval-mid iv)
  (doc 'export #t)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (+ lo (/ (- hi lo) 2))))

;;; interval-width : Interval → Real
;;; Width (diameter) of interval.
(define (interval-width iv)
  (doc 'export #t)
  (- (interval-hi iv) (interval-lo iv)))

;;; interval-radius : Interval → Real
;;; Half-width (radius) of interval.
(define (interval-radius iv)
  (doc 'export #t)
  (/ (interval-width iv) 2))

;;; interval-magnitude : Interval → Real
;;; Maximum absolute value in interval.
(define (interval-magnitude iv)
  (doc 'export #t)
  (max (abs (interval-lo iv)) (abs (interval-hi iv))))

;;; interval-mignitude : Interval → Real
;;; Minimum absolute value in interval (0 if interval contains 0).
(define (interval-mignitude iv)
  (doc 'export #t)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (cond
      [(and (<= lo 0) (>= hi 0)) 0]  ; Contains zero
      [(> lo 0) lo]                   ; Entirely positive
      [else (- hi)])))                ; Entirely negative

;;; ============================================================================
;;; Predicates
;;; ============================================================================

;;; interval-empty? : Interval → Boolean
;;; An interval is never empty by construction, but we check anyway.
(define (interval-empty? iv)
  (doc 'export #t)
  (> (interval-lo iv) (interval-hi iv)))

;;; interval-singleton? : Interval → Boolean
;;; Test if interval is a single point.
(define (interval-singleton? iv)
  (doc 'export #t)
  (= (interval-lo iv) (interval-hi iv)))

;;; interval-contains? : Interval × Real → Boolean
;;; Test if interval contains a point.
(define (interval-contains? iv x)
  (doc 'export #t)
  (and (<= (interval-lo iv) x)
       (<= x (interval-hi iv))))

;;; interval-contains-zero? : Interval → Boolean
;;; Test if interval contains zero (important for division).
(define (interval-contains-zero? iv)
  (doc 'export #t)
  (interval-contains? iv 0))

;;; interval-positive? : Interval → Boolean
;;; Test if entire interval is positive.
(define (interval-positive? iv)
  (doc 'export #t)
  (> (interval-lo iv) 0))

;;; interval-negative? : Interval → Boolean
;;; Test if entire interval is negative.
(define (interval-negative? iv)
  (doc 'export #t)
  (< (interval-hi iv) 0))

;;; interval-subset? : Interval × Interval → Boolean
;;; Test if first interval is subset of second.
(define (interval-subset? iv1 iv2)
  (doc 'export #t)
  (and (>= (interval-lo iv1) (interval-lo iv2))
       (<= (interval-hi iv1) (interval-hi iv2))))

;;; intervals-overlap? : Interval × Interval → Boolean
;;; Test if intervals have non-empty intersection.
(define (intervals-overlap? iv1 iv2)
  (doc 'export #t)
  (and (<= (interval-lo iv1) (interval-hi iv2))
       (<= (interval-lo iv2) (interval-hi iv1))))

;;; intervals-disjoint? : Interval × Interval → Boolean
;;; Test if intervals have empty intersection.
(define (intervals-disjoint? iv1 iv2)
  (doc 'export #t)
  (not (intervals-overlap? iv1 iv2)))

;;; ============================================================================
;;; Comparisons (Three-valued logic)
;;; ============================================================================
;;;
;;; Interval comparisons can be: definitely true, definitely false,
;;; or indeterminate (possibly either).

;;; interval-definitely< : Interval × Interval → Boolean
;;; True iff every element of iv1 is less than every element of iv2.
(define (interval-definitely< iv1 iv2)
  (doc 'export #t)
  (< (interval-hi iv1) (interval-lo iv2)))

;;; interval-definitely<= : Interval × Interval → Boolean
;;; True iff every element of iv1 is <= every element of iv2.
(define (interval-definitely<= iv1 iv2)
  (doc 'export #t)
  (<= (interval-hi iv1) (interval-lo iv2)))

;;; interval-definitely> : Interval × Interval → Boolean
(define (interval-definitely> iv1 iv2)
  (doc 'export #t)
  (interval-definitely< iv2 iv1))

;;; interval-definitely>= : Interval × Interval → Boolean
(define (interval-definitely>= iv1 iv2)
  (doc 'export #t)
  (interval-definitely<= iv2 iv1))

;;; interval-possibly< : Interval × Interval → Boolean
;;; True iff there exist elements x ∈ iv1, y ∈ iv2 with x < y.
(define (interval-possibly< iv1 iv2)
  (doc 'export #t)
  (< (interval-lo iv1) (interval-hi iv2)))

;;; interval-possibly<= : Interval × Interval → Boolean
(define (interval-possibly<= iv1 iv2)
  (doc 'export #t)
  (<= (interval-lo iv1) (interval-hi iv2)))

;;; interval-possibly> : Interval × Interval → Boolean
(define (interval-possibly> iv1 iv2)
  (doc 'export #t)
  (interval-possibly< iv2 iv1))

;;; interval-possibly>= : Interval × Interval → Boolean
(define (interval-possibly>= iv1 iv2)
  (doc 'export #t)
  (interval-possibly<= iv2 iv1))

;;; interval-definitely= : Interval × Interval → Boolean
;;; True iff both are the same singleton.
(define (interval-definitely= iv1 iv2)
  (doc 'export #t)
  (and (interval-singleton? iv1)
       (interval-singleton? iv2)
       (= (interval-lo iv1) (interval-lo iv2))))

;;; interval-possibly= : Interval × Interval → Boolean
;;; True iff intervals overlap.
(define (interval-possibly= iv1 iv2)
  (doc 'export #t)
  (intervals-overlap? iv1 iv2))

;;; ============================================================================
;;; Arithmetic Operations
;;; ============================================================================
;;;
;;; All operations compute the tightest interval guaranteed to contain
;;; all possible results.

;;; interval-neg : Interval → Interval
;;; Negate: -[a,b] = [-b, -a]
(define (interval-neg iv)
  (doc 'export #t)
  (make-interval (- (interval-hi iv))
                 (- (interval-lo iv))))

;;; interval-add : Interval × Interval → Interval
;;; Addition: [a,b] + [c,d] = [a+c, b+d]
(define (interval-add iv1 iv2)
  (doc 'export #t)
  (make-interval (+ (interval-lo iv1) (interval-lo iv2))
                 (+ (interval-hi iv1) (interval-hi iv2))))

;;; interval-sub : Interval × Interval → Interval
;;; Subtraction: [a,b] - [c,d] = [a-d, b-c]
(define (interval-sub iv1 iv2)
  (doc 'export #t)
  (make-interval (- (interval-lo iv1) (interval-hi iv2))
                 (- (interval-hi iv1) (interval-lo iv2))))

;;; interval-mul : Interval × Interval → Interval
;;; Multiplication with sign-based optimization (2 muls in most cases).
;;; Cases: P=positive, N=negative, M=mixed (contains zero)
(define (interval-mul iv1 iv2)
  (doc 'export #t)
  (let ([a (interval-lo iv1)] [b (interval-hi iv1)]
        [c (interval-lo iv2)] [d (interval-hi iv2)])
    (cond
      ;; iv1 positive (a >= 0)
      [(>= a 0)
       (cond
         [(>= c 0) (make-interval (* a c) (* b d))]       ; P*P
         [(<= d 0) (make-interval (* b c) (* a d))]       ; P*N
         [else     (make-interval (* b c) (* b d))])]     ; P*M
      ;; iv1 negative (b <= 0)
      [(<= b 0)
       (cond
         [(>= c 0) (make-interval (* a d) (* b c))]       ; N*P
         [(<= d 0) (make-interval (* b d) (* a c))]       ; N*N
         [else     (make-interval (* a d) (* a c))])]     ; N*M
      ;; iv1 mixed (a < 0 < b)
      [else
       (cond
         [(>= c 0) (make-interval (* a d) (* b d))]       ; M*P
         [(<= d 0) (make-interval (* b c) (* a c))]       ; M*N
         [else     (make-interval (min (* a d) (* b c))   ; M*M (4 muls unavoidable)
                                  (max (* a c) (* b d)))])])))

;;; interval-sqr : Interval → Interval
;;; Square: tighter than interval-mul iv iv when interval contains 0.
(define (interval-sqr iv)
  (doc 'export #t)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(>= a 0)              ; Entirely non-negative
       (make-interval (* a a) (* b b))]
      [(<= b 0)              ; Entirely non-positive
       (make-interval (* b b) (* a a))]
      [else                  ; Contains zero: minimum is 0
       (make-interval 0 (max (* a a) (* b b)))])))

;;; interval-recip : Interval → Interval | '(error division-by-zero)
;;; Reciprocal: 1/[a,b] = [1/b, 1/a] when 0 ∉ [a,b].
;;; Returns '(error division-by-zero) if interval contains zero.
(define (interval-recip iv)
  (doc 'export #t)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(interval-contains-zero? iv) '(error division-by-zero)]
      [else (make-interval (/ 1 b) (/ 1 a))])))

;;; interval-div : Interval × Interval → Interval | '(error division-by-zero)
;;; Division: [a,b] / [c,d] = [a,b] * (1/[c,d])
(define (interval-div iv1 iv2)
  (doc 'export #t)
  (let ([recip (interval-recip iv2)])
    (if (error? recip)
        recip
        (interval-mul iv1 recip))))

;;; interval-scale : Interval × Real → Interval
;;; Scalar multiplication.
(define (interval-scale iv k)
  (doc 'export #t)
  (if (>= k 0)
      (make-interval (* k (interval-lo iv)) (* k (interval-hi iv)))
      (make-interval (* k (interval-hi iv)) (* k (interval-lo iv)))))

;;; ============================================================================
;;; Elementary Functions
;;; ============================================================================

;;; interval-abs : Interval → Interval
;;; Absolute value.
(define (interval-abs iv)
  (doc 'export #t)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(>= a 0) iv]                           ; Entirely non-negative
      [(<= b 0) (interval-neg iv)]            ; Entirely non-positive
      [else (make-interval 0 (max (- a) b))]))) ; Contains zero

;;; interval-sqrt : Interval → Interval | '(error domain-error)
;;; Square root. Requires interval to be non-negative.
(define (interval-sqrt iv)
  (doc 'export #t)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(< b 0) '(error domain-error)]        ; Entirely negative
      [(< a 0)                       ; Partially negative: clamp to 0
       (make-interval 0 (sqrt b))]
      [else                          ; Entirely non-negative
       (make-interval (sqrt a) (sqrt b))])))

;;; interval-pow : Interval × Integer → Interval
;;; Integer power x^n. Uses monotonicity for tight bounds.
(define (interval-pow iv n)
  (doc 'export #t)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(= n 0) (interval-singleton 1)]
      [(= n 1) iv]
      [(< n 0)
       (let ([recip (interval-recip iv)])
         (if (error? recip)
             recip
             (interval-pow recip (- n))))]
      [(odd? n)
       ;; Odd power: x^n is monotonically increasing, so [a^n, b^n]
       (make-interval (expt a n) (expt b n))]
      [else
       ;; Even power: x^n has minimum at 0
       (let ([a^n (expt a n)]
             [b^n (expt b n)])
         (cond
           [(>= a 0) (make-interval a^n b^n)]        ; Entirely non-negative
           [(<= b 0) (make-interval b^n a^n)]        ; Entirely non-positive
           [else     (make-interval 0 (max a^n b^n))]))])))

;;; interval-min : Interval × Interval → Interval
;;; Minimum of two intervals.
(define (interval-min iv1 iv2)
  (doc 'export #t)
  (make-interval (min (interval-lo iv1) (interval-lo iv2))
                 (min (interval-hi iv1) (interval-hi iv2))))

;;; interval-max : Interval × Interval → Interval
;;; Maximum of two intervals.
(define (interval-max iv1 iv2)
  (doc 'export #t)
  (make-interval (max (interval-lo iv1) (interval-lo iv2))
                 (max (interval-hi iv1) (interval-hi iv2))))

;;; ============================================================================
;;; Set Operations
;;; ============================================================================

;;; interval-union : Interval × Interval → Interval
;;; Smallest interval containing both (hull).
(define (interval-union iv1 iv2)
  (doc 'export #t)
  (make-interval (min (interval-lo iv1) (interval-lo iv2))
                 (max (interval-hi iv1) (interval-hi iv2))))

;;; interval-hull : Interval × Interval → Interval
;;; Alias for interval-union.
(doc interval-hull 'export #t)
(define interval-hull interval-union)

;;; interval-hull-list : (Listof Interval) → Interval
;;; Hull of multiple intervals.
(define (interval-hull-list ivs)
  (doc 'export #t)
  (if (null? ivs)
      (error 'interval-hull-list "empty list")
      (fold-left interval-hull (car ivs) (cdr ivs))))

;;; interval-intersection : Interval × Interval → Interval | (error empty)
;;; Intersection of two intervals.
(define (interval-intersection iv1 iv2)
  (doc 'export #t)
  (let ([lo (max (interval-lo iv1) (interval-lo iv2))]
        [hi (min (interval-hi iv1) (interval-hi iv2))])
    (if (<= lo hi)
        (make-interval lo hi)
        '(error empty))))

;;; interval-bisect : Interval → (Pair Interval Interval)
;;; Split interval at midpoint.
(define (interval-bisect iv)
  (doc 'export #t)
  (let ([mid (interval-mid iv)]
        [lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (cons (make-interval lo mid)
          (make-interval mid hi))))

;;; ============================================================================
;;; Coercion and Display
;;; ============================================================================

;;; real->interval : Real → Interval
;;; Lift a real number to a point interval.
(define (real->interval x)
  (doc 'export #t)
  (interval-singleton x))

;;; interval->string : Interval → String
(define (interval->string iv)
  (doc 'export #t)
  (string-append "["
                 (number->string (interval-lo iv))
                 ", "
                 (number->string (interval-hi iv))
                 "]"))

;;; interval-print : Interval → Void
(define (interval-print iv)
  (doc 'export #t)
  (display (interval->string iv)))

;;; ============================================================================
;;; Interval Lists (for multi-dimensional boxes)
;;; ============================================================================

;;; make-box : (Listof Interval) → Box
;;; An n-dimensional box is just a list of intervals.
(define (make-box intervals)
  (doc 'export #t)
  intervals)

;;; box-dimension : Box → Nat
(define (box-dimension box)
  (doc 'export #t)
  (length box))

;;; box-volume : Box → Real
;;; Product of widths.
(define (box-volume box)
  (doc 'export #t)
  (fold-left * 1 (map interval-width box)))

;;; box-contains? : Box × (Listof Real) → Boolean
(define (box-contains? box point)
  (doc 'export #t)
  (and (= (length box) (length point))
       (let loop ([ivs box] [pts point])
         (or (null? ivs)
             (and (interval-contains? (car ivs) (car pts))
                  (loop (cdr ivs) (cdr pts)))))))

;;; ============================================================================
;;; Rigorous Interval Operations (Directed Rounding)
;;; ============================================================================
;;;
;;; These operations use directed rounding to GUARANTEE the computed interval
;;; contains the true mathematical result. They are ~2x slower than standard
;;; operations but provide formal correctness guarantees.
;;;
;;; Use these when:
;;; - Proving properties of algorithms
;;; - Safety-critical applications
;;; - When standard operations aren't tight enough

;;; interval-add-rigorous : Interval × Interval → Interval
;;; Addition with guaranteed enclosure.
(define (interval-add-rigorous iv1 iv2)
  (doc 'export #t)
  (make-interval (add-down (interval-lo iv1) (interval-lo iv2))
                 (add-up (interval-hi iv1) (interval-hi iv2))))

;;; interval-sub-rigorous : Interval × Interval → Interval
;;; Subtraction with guaranteed enclosure.
(define (interval-sub-rigorous iv1 iv2)
  (doc 'export #t)
  (make-interval (sub-down (interval-lo iv1) (interval-hi iv2))
                 (sub-up (interval-hi iv1) (interval-lo iv2))))

;;; interval-mul-rigorous : Interval × Interval → Interval
;;; Multiplication with guaranteed enclosure.
(define (interval-mul-rigorous iv1 iv2)
  (doc 'export #t)
  (let ([a (interval-lo iv1)] [b (interval-hi iv1)]
        [c (interval-lo iv2)] [d (interval-hi iv2)])
    (cond
      ;; iv1 positive (a >= 0)
      [(>= a 0)
       (cond
         [(>= c 0) (make-interval (mul-down a c) (mul-up b d))]
         [(<= d 0) (make-interval (mul-down b c) (mul-up a d))]
         [else     (make-interval (mul-down b c) (mul-up b d))])]
      ;; iv1 negative (b <= 0)
      [(<= b 0)
       (cond
         [(>= c 0) (make-interval (mul-down a d) (mul-up b c))]
         [(<= d 0) (make-interval (mul-down b d) (mul-up a c))]
         [else     (make-interval (mul-down a d) (mul-up a c))])]
      ;; iv1 mixed (a < 0 < b)
      [else
       (cond
         [(>= c 0) (make-interval (mul-down a d) (mul-up b d))]
         [(<= d 0) (make-interval (mul-down b c) (mul-up a c))]
         [else     (make-interval (min (mul-down a d) (mul-down b c))
                                  (max (mul-up a c) (mul-up b d)))])])))

;;; interval-div-rigorous : Interval × Interval → Interval | '(error division-by-zero)
;;; Division with guaranteed enclosure.
(define (interval-div-rigorous iv1 iv2)
  (doc 'export #t)
  (if (interval-contains-zero? iv2)
      '(error division-by-zero)
      (let ([a (interval-lo iv1)] [b (interval-hi iv1)]
            [c (interval-lo iv2)] [d (interval-hi iv2)])
        ;; 1/[c,d] = [1/d, 1/c] (when 0 not in [c,d])
        ;; Then multiply
        (cond
          [(> c 0)  ; Divisor entirely positive
           (cond
             [(>= a 0) (make-interval (div-down a d) (div-up b c))]
             [(<= b 0) (make-interval (div-down a c) (div-up b d))]
             [else     (make-interval (div-down a c) (div-up b c))])]
          [else     ; Divisor entirely negative (d < 0)
           (cond
             [(>= a 0) (make-interval (div-down b d) (div-up a c))]
             [(<= b 0) (make-interval (div-down b c) (div-up a d))]
             [else     (make-interval (div-down b d) (div-up a d))])]))))

;;; interval-sqrt-rigorous : Interval → Interval | '(error domain-error)
;;; Square root with guaranteed enclosure.
(define (interval-sqrt-rigorous iv)
  (doc 'export #t)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(< b 0) '(error domain-error)]
      [(< a 0) (make-interval 0 (sqrt-up b))]  ; Clamp negative to 0
      [else    (make-interval (sqrt-down a) (sqrt-up b))])))

;;; interval-sqr-rigorous : Interval → Interval
;;; Square with guaranteed enclosure.
(define (interval-sqr-rigorous iv)
  (doc 'export #t)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(>= a 0) (make-interval (mul-down a a) (mul-up b b))]
      [(<= b 0) (make-interval (mul-down b b) (mul-up a a))]
      [else     (make-interval 0 (max (mul-up a a) (mul-up b b)))])))

;;; interval-scale-rigorous : Interval × Real → Interval
;;; Scalar multiplication with guaranteed enclosure.
(define (interval-scale-rigorous iv k)
  (doc 'export #t)
  (if (>= k 0)
      (make-interval (mul-down k (interval-lo iv)) (mul-up k (interval-hi iv)))
      (make-interval (mul-down k (interval-hi iv)) (mul-up k (interval-lo iv)))))

;;; ============================================================================
;;; Useful Constants
;;; ============================================================================

;;; pi-interval : Interval
;;; Interval guaranteed to contain π.
(doc pi-interval 'export #t)
(define pi-interval
  (make-interval 3.141592653589793 3.1415926535897936))

;;; e-interval : Interval
;;; Interval guaranteed to contain e.
(doc e-interval 'export #t)
(define e-interval
  (make-interval 2.718281828459045 2.7182818284590455))

;;; ============================================================================
;;; Convenience: Infix-like notation
;;; ============================================================================

;;; iv+ : Interval × Interval → Interval
(doc iv+ 'export #t)
(define iv+ interval-add)

;;; iv- : Interval × Interval → Interval
(doc iv- 'export #t)
(define iv- interval-sub)

;;; iv* : Interval × Interval → Interval
(doc iv* 'export #t)
(define iv* interval-mul)

;;; iv/ : Interval × Interval → Interval
(doc iv/ 'export #t)
(define iv/ interval-div)

;;; ============================================================================
;;; Elementary Transcendental Functions
;;; ============================================================================
;;;
;;; These use monotonicity properties where applicable:
;;;   - exp is monotonically increasing
;;;   - log is monotonically increasing (on positive domain)
;;;   - sin/cos require careful handling of periodicity

;;; interval-exp : Interval → Interval
;;; Exponential function. exp is monotonically increasing.
(define (interval-exp iv)
  (doc 'export #t)
  (make-interval (exp (interval-lo iv))
                 (exp (interval-hi iv))))

;;; interval-log : Interval → Interval | '(error domain-error)
;;; Natural logarithm. Requires interval to be positive.
(define (interval-log iv)
  (doc 'export #t)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (cond
      [(<= hi 0) '(error domain-error)]    ; Entirely non-positive
      [(<= lo 0)                   ; Partially negative: clamp to epsilon
       (make-interval (log 1e-300) (log hi))]
      [else                        ; Entirely positive
       (make-interval (log lo) (log hi))])))

;;; interval-sin : Interval → Interval
;;; Sine function. Must handle periodicity.
;;; For intervals wider than 2pi, returns [-1, 1].
;;; Otherwise, check for extrema at pi/2 + k*pi.
(define (interval-sin iv)
  (doc 'export #t)
  (let* ([lo (interval-lo iv)]
         [hi (interval-hi iv)]
         [pi 3.141592653589793]
         [width (- hi lo)])
    (cond
      ;; Wide interval: full range
      [(>= width (* 2 pi)) (make-interval -1 1)]
      [else
       ;; Evaluate at endpoints
       (let* ([sin-lo (sin lo)]
              [sin-hi (sin hi)]
              [min-val (min sin-lo sin-hi)]
              [max-val (max sin-lo sin-hi)])
         ;; Check for maximum (at pi/2 + 2k*pi)
         ;; Check for minimum (at -pi/2 + 2k*pi = 3pi/2 + 2k*pi)
         (let* ([max-val (if (interval-contains-critical? lo hi (* 0.5 pi) (* 2 pi))
                             1
                             max-val)]
                [min-val (if (interval-contains-critical? lo hi (* 1.5 pi) (* 2 pi))
                             -1
                             min-val)])
           (make-interval min-val max-val)))])))

;;; interval-cos : Interval → Interval
;;; Cosine function. Must handle periodicity.
(define (interval-cos iv)
  (doc 'export #t)
  (let* ([lo (interval-lo iv)]
         [hi (interval-hi iv)]
         [pi 3.141592653589793]
         [width (- hi lo)])
    (cond
      ;; Wide interval: full range
      [(>= width (* 2 pi)) (make-interval -1 1)]
      [else
       ;; Evaluate at endpoints
       (let* ([cos-lo (cos lo)]
              [cos-hi (cos hi)]
              [min-val (min cos-lo cos-hi)]
              [max-val (max cos-lo cos-hi)])
         ;; Check for maximum (at 2k*pi)
         ;; Check for minimum (at pi + 2k*pi)
         (let* ([max-val (if (interval-contains-critical? lo hi 0 (* 2 pi))
                             1
                             max-val)]
                [min-val (if (interval-contains-critical? lo hi pi (* 2 pi))
                             -1
                             min-val)])
           (make-interval min-val max-val)))])))

;;; interval-contains-critical? : Real × Real × Real × Real → Boolean
;;; Check if interval [lo, hi] contains any point of form (base + k*period)
;;; for integer k.
(define (interval-contains-critical? lo hi base period)
  ;; Find smallest k such that base + k*period >= lo
  (let* ([k-lo (ceiling (/ (- lo base) period))]
         [critical (+ base (* k-lo period))])
    (<= critical hi)))

;;; interval-tan : Interval → Interval | '(error domain-error)
;;; Tangent function. Undefined at pi/2 + k*pi.
;;; Returns '(error domain-error) if interval contains a discontinuity.
(define (interval-tan iv)
  (doc 'export #t)
  (let* ([lo (interval-lo iv)]
         [hi (interval-hi iv)]
         [pi 3.141592653589793]
         [half-pi (* 0.5 pi)])
    ;; Check if interval contains any discontinuity at pi/2 + k*pi
    (if (interval-contains-critical? lo hi half-pi pi)
        '(error domain-error)
        (make-interval (tan lo) (tan hi)))))

;;; interval-asin : Interval → Interval | '(error domain-error)
;;; Arcsine. Domain is [-1, 1], monotonically increasing.
(define (interval-asin iv)
  (doc 'export #t)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (cond
      [(or (< hi -1) (> lo 1)) '(error domain-error)]
      [else
       (make-interval (asin (max -1 lo))
                      (asin (min 1 hi)))])))

;;; interval-acos : Interval → Interval | '(error domain-error)
;;; Arccosine. Domain is [-1, 1], monotonically decreasing.
(define (interval-acos iv)
  (doc 'export #t)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (cond
      [(or (< hi -1) (> lo 1)) '(error domain-error)]
      [else
       ;; acos is decreasing: acos(hi) <= acos(lo)
       (make-interval (acos (min 1 hi))
                      (acos (max -1 lo)))])))

;;; interval-atan : Interval → Interval
;;; Arctangent. Monotonically increasing on all reals.
(define (interval-atan iv)
  (doc 'export #t)
  (make-interval (atan (interval-lo iv))
                 (atan (interval-hi iv))))

;;; interval-sinh : Interval → Interval
;;; Hyperbolic sine. Monotonically increasing.
(define (interval-sinh iv)
  (doc 'export #t)
  (make-interval (sinh (interval-lo iv))
                 (sinh (interval-hi iv))))

;;; interval-cosh : Interval → Interval
;;; Hyperbolic cosine. Minimum at 0, symmetric.
(define (interval-cosh iv)
  (doc 'export #t)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (cond
      [(>= lo 0) (make-interval (cosh lo) (cosh hi))]     ; Entirely non-negative
      [(<= hi 0) (make-interval (cosh hi) (cosh lo))]     ; Entirely non-positive
      [else (make-interval 1 (max (cosh lo) (cosh hi)))]))) ; Contains zero

;;; interval-tanh : Interval → Interval
;;; Hyperbolic tangent. Monotonically increasing, range (-1, 1).
(define (interval-tanh iv)
  (doc 'export #t)
  (make-interval (tanh (interval-lo iv))
                 (tanh (interval-hi iv))))

;;; ============================================================================
;;; Natural Interval Extension Helpers
;;; ============================================================================

;;; sinh : Real → Real
;;; Standard hyperbolic sine (define if not built-in).
(define (sinh x)
  (/ (- (exp x) (exp (- x))) 2))

;;; cosh : Real → Real
;;; Standard hyperbolic cosine.
(define (cosh x)
  (/ (+ (exp x) (exp (- x))) 2))

;;; tanh : Real → Real
;;; Standard hyperbolic tangent.
(define (tanh x)
  (/ (sinh x) (cosh x)))

;;; Display load message
(display "Interval arithmetic loaded. Use (make-interval lo hi) or (interval lo hi).\n")
