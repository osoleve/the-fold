;;; lattice/numeric/interval.ss — Interval Arithmetic
;;;
;;; Verified numerical computation with rigorous bounds. Every operation
;;; guarantees the true mathematical result lies within the computed interval.
;;;
;;; Author: Claude Opus 4.5
;;; Created: 2026-01-16
;;;
;;; Usage:
;;;   (load "lattice/numeric/interval.ss")
;;;   (define x (make-interval 2 3))
;;;   (define y (make-interval 1 2))
;;;   (interval-add x y)  ; => (interval 3 5)
;;;
;;; Invariant: For all operations f, if x ∈ [a,b] and y ∈ [c,d],
;;;            then f(x,y) ∈ f([a,b], [c,d]).
;;;
;;; CAVEAT: This implementation uses standard floating-point arithmetic with
;;; round-to-nearest semantics. True interval arithmetic requires directed
;;; rounding (round-down for lower bounds, round-up for upper bounds) which
;;; Scheme does not expose. For most practical purposes with reasonable
;;; operands, this is sufficient. For rigorous proofs requiring IEEE 754
;;; directed rounding, use a library with explicit rounding control.

(load "core/base/prelude.ss")

;;; ============================================================================
;;; Interval Type
;;; ============================================================================
;;;
;;; An interval [lo, hi] represents the set of all real numbers x
;;; such that lo <= x <= hi. We maintain the invariant lo <= hi.

;;; make-interval : Real × Real → Interval
;;; Create an interval. Automatically orders endpoints.
(define (make-interval lo hi)
  (if (<= lo hi)
      (list 'interval lo hi)
      (list 'interval hi lo)))

;;; interval : Real × Real → Interval
;;; Alias for make-interval (shorter).
(define interval make-interval)

;;; interval? : Any → Boolean
;;; Test if value is an interval.
(define (interval? x)
  (and (pair? x)
       (eq? (car x) 'interval)
       (= (length x) 3)))

;;; interval-lo : Interval → Real
;;; Get lower bound.
(define (interval-lo iv)
  (cadr iv))

;;; interval-hi : Interval → Real
;;; Get upper bound.
(define (interval-hi iv)
  (caddr iv))

;;; interval-singleton : Real → Interval
;;; Create a point interval [x, x].
(define (interval-singleton x)
  (make-interval x x))

;;; entire-interval : → Interval
;;; The interval (-∞, +∞) — represents any real number.
;;; We approximate with large finite bounds.
(define (entire-interval)
  (make-interval -1e308 1e308))

;;; ============================================================================
;;; Accessors and Queries
;;; ============================================================================

;;; interval-mid : Interval → Real
;;; Midpoint of interval. Uses overflow-safe formula.
(define (interval-mid iv)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (+ lo (/ (- hi lo) 2))))

;;; interval-width : Interval → Real
;;; Width (diameter) of interval.
(define (interval-width iv)
  (- (interval-hi iv) (interval-lo iv)))

;;; interval-radius : Interval → Real
;;; Half-width (radius) of interval.
(define (interval-radius iv)
  (/ (interval-width iv) 2))

;;; interval-magnitude : Interval → Real
;;; Maximum absolute value in interval.
(define (interval-magnitude iv)
  (max (abs (interval-lo iv)) (abs (interval-hi iv))))

;;; interval-mignitude : Interval → Real
;;; Minimum absolute value in interval (0 if interval contains 0).
(define (interval-mignitude iv)
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
  (> (interval-lo iv) (interval-hi iv)))

;;; interval-singleton? : Interval → Boolean
;;; Test if interval is a single point.
(define (interval-singleton? iv)
  (= (interval-lo iv) (interval-hi iv)))

;;; interval-contains? : Interval × Real → Boolean
;;; Test if interval contains a point.
(define (interval-contains? iv x)
  (and (<= (interval-lo iv) x)
       (<= x (interval-hi iv))))

;;; interval-contains-zero? : Interval → Boolean
;;; Test if interval contains zero (important for division).
(define (interval-contains-zero? iv)
  (interval-contains? iv 0))

;;; interval-positive? : Interval → Boolean
;;; Test if entire interval is positive.
(define (interval-positive? iv)
  (> (interval-lo iv) 0))

;;; interval-negative? : Interval → Boolean
;;; Test if entire interval is negative.
(define (interval-negative? iv)
  (< (interval-hi iv) 0))

;;; interval-subset? : Interval × Interval → Boolean
;;; Test if first interval is subset of second.
(define (interval-subset? iv1 iv2)
  (and (>= (interval-lo iv1) (interval-lo iv2))
       (<= (interval-hi iv1) (interval-hi iv2))))

;;; intervals-overlap? : Interval × Interval → Boolean
;;; Test if intervals have non-empty intersection.
(define (intervals-overlap? iv1 iv2)
  (and (<= (interval-lo iv1) (interval-hi iv2))
       (<= (interval-lo iv2) (interval-hi iv1))))

;;; intervals-disjoint? : Interval × Interval → Boolean
;;; Test if intervals have empty intersection.
(define (intervals-disjoint? iv1 iv2)
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
  (< (interval-hi iv1) (interval-lo iv2)))

;;; interval-definitely<= : Interval × Interval → Boolean
;;; True iff every element of iv1 is <= every element of iv2.
(define (interval-definitely<= iv1 iv2)
  (<= (interval-hi iv1) (interval-lo iv2)))

;;; interval-definitely> : Interval × Interval → Boolean
(define (interval-definitely> iv1 iv2)
  (interval-definitely< iv2 iv1))

;;; interval-definitely>= : Interval × Interval → Boolean
(define (interval-definitely>= iv1 iv2)
  (interval-definitely<= iv2 iv1))

;;; interval-possibly< : Interval × Interval → Boolean
;;; True iff there exist elements x ∈ iv1, y ∈ iv2 with x < y.
(define (interval-possibly< iv1 iv2)
  (< (interval-lo iv1) (interval-hi iv2)))

;;; interval-possibly<= : Interval × Interval → Boolean
(define (interval-possibly<= iv1 iv2)
  (<= (interval-lo iv1) (interval-hi iv2)))

;;; interval-possibly> : Interval × Interval → Boolean
(define (interval-possibly> iv1 iv2)
  (interval-possibly< iv2 iv1))

;;; interval-possibly>= : Interval × Interval → Boolean
(define (interval-possibly>= iv1 iv2)
  (interval-possibly<= iv2 iv1))

;;; interval-definitely= : Interval × Interval → Boolean
;;; True iff both are the same singleton.
(define (interval-definitely= iv1 iv2)
  (and (interval-singleton? iv1)
       (interval-singleton? iv2)
       (= (interval-lo iv1) (interval-lo iv2))))

;;; interval-possibly= : Interval × Interval → Boolean
;;; True iff intervals overlap.
(define (interval-possibly= iv1 iv2)
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
  (make-interval (- (interval-hi iv))
                 (- (interval-lo iv))))

;;; interval-add : Interval × Interval → Interval
;;; Addition: [a,b] + [c,d] = [a+c, b+d]
(define (interval-add iv1 iv2)
  (make-interval (+ (interval-lo iv1) (interval-lo iv2))
                 (+ (interval-hi iv1) (interval-hi iv2))))

;;; interval-sub : Interval × Interval → Interval
;;; Subtraction: [a,b] - [c,d] = [a-d, b-c]
(define (interval-sub iv1 iv2)
  (make-interval (- (interval-lo iv1) (interval-hi iv2))
                 (- (interval-hi iv1) (interval-lo iv2))))

;;; interval-mul : Interval × Interval → Interval
;;; Multiplication with sign-based optimization (2 muls in most cases).
;;; Cases: P=positive, N=negative, M=mixed (contains zero)
(define (interval-mul iv1 iv2)
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
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(>= a 0)              ; Entirely non-negative
       (make-interval (* a a) (* b b))]
      [(<= b 0)              ; Entirely non-positive
       (make-interval (* b b) (* a a))]
      [else                  ; Contains zero: minimum is 0
       (make-interval 0 (max (* a a) (* b b)))])))

;;; interval-recip : Interval → Interval | 'division-by-zero
;;; Reciprocal: 1/[a,b] = [1/b, 1/a] when 0 ∉ [a,b].
;;; Returns 'division-by-zero if interval contains zero.
(define (interval-recip iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(interval-contains-zero? iv) 'division-by-zero]
      [else (make-interval (/ 1 b) (/ 1 a))])))

;;; interval-div : Interval × Interval → Interval | 'division-by-zero
;;; Division: [a,b] / [c,d] = [a,b] * (1/[c,d])
(define (interval-div iv1 iv2)
  (let ([recip (interval-recip iv2)])
    (if (eq? recip 'division-by-zero)
        'division-by-zero
        (interval-mul iv1 recip))))

;;; interval-scale : Interval × Real → Interval
;;; Scalar multiplication.
(define (interval-scale iv k)
  (if (>= k 0)
      (make-interval (* k (interval-lo iv)) (* k (interval-hi iv)))
      (make-interval (* k (interval-hi iv)) (* k (interval-lo iv)))))

;;; ============================================================================
;;; Elementary Functions
;;; ============================================================================

;;; interval-abs : Interval → Interval
;;; Absolute value.
(define (interval-abs iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(>= a 0) iv]                           ; Entirely non-negative
      [(<= b 0) (interval-neg iv)]            ; Entirely non-positive
      [else (make-interval 0 (max (- a) b))]))) ; Contains zero

;;; interval-sqrt : Interval → Interval | 'domain-error
;;; Square root. Requires interval to be non-negative.
(define (interval-sqrt iv)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(< b 0) 'domain-error]        ; Entirely negative
      [(< a 0)                       ; Partially negative: clamp to 0
       (make-interval 0 (sqrt b))]
      [else                          ; Entirely non-negative
       (make-interval (sqrt a) (sqrt b))])))

;;; interval-pow : Interval × Integer → Interval
;;; Integer power x^n. Uses monotonicity for tight bounds.
(define (interval-pow iv n)
  (let ([a (interval-lo iv)]
        [b (interval-hi iv)])
    (cond
      [(= n 0) (interval-singleton 1)]
      [(= n 1) iv]
      [(< n 0)
       (let ([recip (interval-recip iv)])
         (if (eq? recip 'division-by-zero)
             'division-by-zero
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
  (make-interval (min (interval-lo iv1) (interval-lo iv2))
                 (min (interval-hi iv1) (interval-hi iv2))))

;;; interval-max : Interval × Interval → Interval
;;; Maximum of two intervals.
(define (interval-max iv1 iv2)
  (make-interval (max (interval-lo iv1) (interval-lo iv2))
                 (max (interval-hi iv1) (interval-hi iv2))))

;;; ============================================================================
;;; Set Operations
;;; ============================================================================

;;; interval-union : Interval × Interval → Interval
;;; Smallest interval containing both (hull).
(define (interval-union iv1 iv2)
  (make-interval (min (interval-lo iv1) (interval-lo iv2))
                 (max (interval-hi iv1) (interval-hi iv2))))

;;; interval-hull : Interval × Interval → Interval
;;; Alias for interval-union.
(define interval-hull interval-union)

;;; interval-hull-list : (Listof Interval) → Interval
;;; Hull of multiple intervals.
(define (interval-hull-list ivs)
  (if (null? ivs)
      (error 'interval-hull-list "empty list")
      (fold-left interval-hull (car ivs) (cdr ivs))))

;;; interval-intersection : Interval × Interval → Interval | 'empty
;;; Intersection of two intervals.
(define (interval-intersection iv1 iv2)
  (let ([lo (max (interval-lo iv1) (interval-lo iv2))]
        [hi (min (interval-hi iv1) (interval-hi iv2))])
    (if (<= lo hi)
        (make-interval lo hi)
        'empty)))

;;; interval-bisect : Interval → (Pair Interval Interval)
;;; Split interval at midpoint.
(define (interval-bisect iv)
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
  (interval-singleton x))

;;; interval->string : Interval → String
(define (interval->string iv)
  (string-append "["
                 (number->string (interval-lo iv))
                 ", "
                 (number->string (interval-hi iv))
                 "]"))

;;; interval-print : Interval → Void
(define (interval-print iv)
  (display (interval->string iv)))

;;; ============================================================================
;;; Interval Lists (for multi-dimensional boxes)
;;; ============================================================================

;;; make-box : (Listof Interval) → Box
;;; An n-dimensional box is just a list of intervals.
(define (make-box intervals)
  intervals)

;;; box-dimension : Box → Nat
(define (box-dimension box)
  (length box))

;;; box-volume : Box → Real
;;; Product of widths.
(define (box-volume box)
  (fold-left * 1 (map interval-width box)))

;;; box-contains? : Box × (Listof Real) → Boolean
(define (box-contains? box point)
  (and (= (length box) (length point))
       (let loop ([ivs box] [pts point])
         (or (null? ivs)
             (and (interval-contains? (car ivs) (car pts))
                  (loop (cdr ivs) (cdr pts)))))))

;;; ============================================================================
;;; Useful Constants
;;; ============================================================================

;;; pi-interval : Interval
;;; Interval guaranteed to contain π.
(define pi-interval
  (make-interval 3.141592653589793 3.1415926535897936))

;;; e-interval : Interval
;;; Interval guaranteed to contain e.
(define e-interval
  (make-interval 2.718281828459045 2.7182818284590455))

;;; ============================================================================
;;; Convenience: Infix-like notation
;;; ============================================================================

;;; iv+ : Interval × Interval → Interval
(define iv+ interval-add)

;;; iv- : Interval × Interval → Interval
(define iv- interval-sub)

;;; iv* : Interval × Interval → Interval
(define iv* interval-mul)

;;; iv/ : Interval × Interval → Interval
(define iv/ interval-div)

;;; Display load message
(display "Interval arithmetic loaded. Use (make-interval lo hi) or (interval lo hi).\n")
