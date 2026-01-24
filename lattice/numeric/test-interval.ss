;;; lattice/numeric/test-interval.ss — Tests for Interval Arithmetic
;;;
;;; Comprehensive tests verifying correctness of interval operations.

(load "core/testing/test-framework.ss")
(load "lattice/numeric/interval.ss")

;;; ============================================================================
;;; Constructor and Accessor Tests
;;; ============================================================================

(test-group "interval-constructors"
  (define-test "make-interval creates valid interval"
    (let ([iv (make-interval 1 3)])
      (assert-true (interval? iv))
      (assert-equal 1 (interval-lo iv))
      (assert-equal 3 (interval-hi iv))))

  (define-test "make-interval auto-orders endpoints"
    (let ([iv (make-interval 5 2)])
      (assert-equal 2 (interval-lo iv))
      (assert-equal 5 (interval-hi iv))))

  (define-test "interval-singleton creates point interval"
    (let ([iv (interval-singleton 7)])
      (assert-equal 7 (interval-lo iv))
      (assert-equal 7 (interval-hi iv))
      (assert-true (interval-singleton? iv))))

  (define-test "interval alias works"
    (let ([iv (interval 2 4)])
      (assert-equal 2 (interval-lo iv))
      (assert-equal 4 (interval-hi iv)))))

;;; ============================================================================
;;; Accessor Tests
;;; ============================================================================

(test-group "interval-accessors"
  (define-test "interval-mid computes midpoint"
    (assert-equal 5/2 (interval-mid (interval 1 4))))

  (define-test "interval-width computes width"
    (assert-equal 3 (interval-width (interval 1 4))))

  (define-test "interval-radius computes half-width"
    (assert-equal 3/2 (interval-radius (interval 1 4))))

  (define-test "interval-magnitude max absolute value"
    (assert-equal 5 (interval-magnitude (interval -5 3)))
    (assert-equal 7 (interval-magnitude (interval 2 7)))
    (assert-equal 4 (interval-magnitude (interval -4 -1))))

  (define-test "interval-mignitude min absolute value"
    (assert-equal 0 (interval-mignitude (interval -2 3)))  ; Contains 0
    (assert-equal 2 (interval-mignitude (interval 2 5)))   ; Positive
    (assert-equal 1 (interval-mignitude (interval -5 -1))))) ; Negative

;;; ============================================================================
;;; Predicate Tests
;;; ============================================================================

(test-group "interval-predicates"
  (define-test "interval? recognizes intervals"
    (assert-true (interval? (interval 1 2)))
    (assert-false (interval? '(1 2)))
    (assert-false (interval? 42)))

  (define-test "interval-singleton? detects points"
    (assert-true (interval-singleton? (interval 5 5)))
    (assert-false (interval-singleton? (interval 1 2))))

  (define-test "interval-contains? membership test"
    (let ([iv (interval 2 5)])
      (assert-true (interval-contains? iv 3))
      (assert-true (interval-contains? iv 2))   ; Boundary
      (assert-true (interval-contains? iv 5))   ; Boundary
      (assert-false (interval-contains? iv 1))
      (assert-false (interval-contains? iv 6))))

  (define-test "interval-contains-zero? zero detection"
    (assert-true (interval-contains-zero? (interval -1 1)))
    (assert-true (interval-contains-zero? (interval 0 5)))
    (assert-false (interval-contains-zero? (interval 1 5)))
    (assert-false (interval-contains-zero? (interval -5 -1))))

  (define-test "interval-positive? / interval-negative?"
    (assert-true (interval-positive? (interval 1 5)))
    (assert-false (interval-positive? (interval -1 5)))
    (assert-true (interval-negative? (interval -5 -1)))
    (assert-false (interval-negative? (interval -1 5))))

  (define-test "interval-subset?"
    (assert-true (interval-subset? (interval 2 3) (interval 1 5)))
    (assert-true (interval-subset? (interval 2 3) (interval 2 3)))
    (assert-false (interval-subset? (interval 1 5) (interval 2 3))))

  (define-test "intervals-overlap?"
    (assert-true (intervals-overlap? (interval 1 3) (interval 2 5)))
    (assert-true (intervals-overlap? (interval 1 3) (interval 3 5)))  ; Touch
    (assert-false (intervals-overlap? (interval 1 2) (interval 3 4)))))

;;; ============================================================================
;;; Comparison Tests
;;; ============================================================================

(test-group "interval-comparisons"
  (define-test "definitely< strict ordering"
    (assert-true (interval-definitely< (interval 1 2) (interval 3 4)))
    (assert-false (interval-definitely< (interval 1 3) (interval 2 4)))  ; Overlap
    (assert-false (interval-definitely< (interval 1 2) (interval 2 3)))) ; Touch

  (define-test "definitely<= non-strict ordering"
    (assert-true (interval-definitely<= (interval 1 2) (interval 3 4)))
    (assert-true (interval-definitely<= (interval 1 2) (interval 2 3)))  ; Touch
    (assert-false (interval-definitely<= (interval 1 3) (interval 2 4)))) ; Overlap

  (define-test "possibly< existence of ordering"
    (assert-true (interval-possibly< (interval 1 3) (interval 2 5)))
    (assert-true (interval-possibly< (interval 3 5) (interval 1 4)))
    (assert-false (interval-possibly< (interval 3 4) (interval 1 2))))

  (define-test "definitely= requires singletons"
    (assert-true (interval-definitely= (interval-singleton 5)
                                        (interval-singleton 5)))
    (assert-false (interval-definitely= (interval 1 2) (interval 1 2)))
    (assert-false (interval-definitely= (interval-singleton 5)
                                         (interval-singleton 6))))

  (define-test "possibly= is overlap"
    (assert-true (interval-possibly= (interval 1 3) (interval 2 4)))
    (assert-false (interval-possibly= (interval 1 2) (interval 3 4)))))

;;; ============================================================================
;;; Arithmetic Tests
;;; ============================================================================

(test-group "interval-arithmetic"
  (define-test "interval-neg negation"
    (let ([iv (interval-neg (interval 2 5))])
      (assert-equal -5 (interval-lo iv))
      (assert-equal -2 (interval-hi iv))))

  (define-test "interval-add addition"
    (let ([iv (interval-add (interval 1 3) (interval 2 4))])
      (assert-equal 3 (interval-lo iv))
      (assert-equal 7 (interval-hi iv))))

  (define-test "interval-sub subtraction"
    (let ([iv (interval-sub (interval 5 8) (interval 2 3))])
      ;; [5,8] - [2,3] = [5-3, 8-2] = [2, 6]
      (assert-equal 2 (interval-lo iv))
      (assert-equal 6 (interval-hi iv))))

  (define-test "interval-mul positive * positive"
    (let ([iv (interval-mul (interval 2 3) (interval 4 5))])
      (assert-equal 8 (interval-lo iv))
      (assert-equal 15 (interval-hi iv))))

  (define-test "interval-mul with negatives"
    (let ([iv (interval-mul (interval -2 3) (interval -1 4))])
      ;; Products: 2, -8, -3, 12 → min=-8, max=12
      (assert-equal -8 (interval-lo iv))
      (assert-equal 12 (interval-hi iv))))

  (define-test "interval-sqr tighter than mul for zero-containing"
    (let ([iv-sqr (interval-sqr (interval -2 3))]
          [iv-mul (interval-mul (interval -2 3) (interval -2 3))])
      ;; sqr knows minimum is 0 when interval contains 0
      (assert-equal 0 (interval-lo iv-sqr))
      (assert-equal 9 (interval-hi iv-sqr))
      ;; mul is wider: min is -6 (from -2*3)
      (assert-equal -6 (interval-lo iv-mul))))

  (define-test "interval-div normal case"
    (let ([iv (interval-div (interval 6 12) (interval 2 3))])
      ;; [6,12] / [2,3] = [6,12] * [1/3, 1/2] = [2, 6]
      (assert-equal 2 (interval-lo iv))
      (assert-equal 6 (interval-hi iv))))

  (define-test "interval-div by zero returns error"
    (assert-equal '(error division-by-zero)
                  (interval-div (interval 1 2) (interval -1 1))))

  (define-test "interval-scale scalar multiplication"
    (let ([iv (interval-scale (interval 2 4) 3)])
      (assert-equal 6 (interval-lo iv))
      (assert-equal 12 (interval-hi iv)))
    (let ([iv (interval-scale (interval 2 4) -2)])
      (assert-equal -8 (interval-lo iv))
      (assert-equal -4 (interval-hi iv)))))

;;; ============================================================================
;;; Elementary Function Tests
;;; ============================================================================

(test-group "interval-elementary"
  (define-test "interval-abs positive interval"
    (let ([iv (interval-abs (interval 2 5))])
      (assert-equal 2 (interval-lo iv))
      (assert-equal 5 (interval-hi iv))))

  (define-test "interval-abs negative interval"
    (let ([iv (interval-abs (interval -5 -2))])
      (assert-equal 2 (interval-lo iv))
      (assert-equal 5 (interval-hi iv))))

  (define-test "interval-abs crossing zero"
    (let ([iv (interval-abs (interval -3 5))])
      (assert-equal 0 (interval-lo iv))
      (assert-equal 5 (interval-hi iv))))

  (define-test "interval-sqrt positive interval"
    (let ([iv (interval-sqrt (interval 4 9))])
      (assert-equal 2 (interval-lo iv))
      (assert-equal 3 (interval-hi iv))))

  (define-test "interval-sqrt crossing zero clamps"
    (let ([iv (interval-sqrt (interval -1 4))])
      (assert-equal 0 (interval-lo iv))
      (assert-equal 2 (interval-hi iv))))

  (define-test "interval-sqrt negative returns error"
    (assert-equal '(error domain-error)
                  (interval-sqrt (interval -4 -1))))

  (define-test "interval-pow square"
    (let ([iv (interval-pow (interval 2 3) 2)])
      (assert-equal 4 (interval-lo iv))
      (assert-equal 9 (interval-hi iv))))

  (define-test "interval-pow cube"
    (let ([iv (interval-pow (interval 2 3) 3)])
      (assert-equal 8 (interval-lo iv))
      (assert-equal 27 (interval-hi iv))))

  (define-test "interval-pow odd tight bounds"
    ;; Gemini QA: [-1,2]^3 should be [-1,8], not [-4,8]
    ;; Odd powers are monotonic, so bounds are exact
    (let ([iv (interval-pow (interval -1 2) 3)])
      (assert-equal -1 (interval-lo iv))
      (assert-equal 8 (interval-hi iv))))

  (define-test "interval-pow even with zero-crossing"
    ;; Even powers have minimum at 0 when interval crosses zero
    (let ([iv (interval-pow (interval -3 2) 4)])
      (assert-equal 0 (interval-lo iv))
      (assert-equal 81 (interval-hi iv))))  ; (-3)^4 = 81 > 2^4 = 16

  (define-test "interval-pow zero"
    (let ([iv (interval-pow (interval 2 3) 0)])
      (assert-equal 1 (interval-lo iv))
      (assert-equal 1 (interval-hi iv))))

  (define-test "interval-pow negative exponent"
    (let ([iv (interval-pow (interval 2 4) -1)])
      (assert-equal 1/4 (interval-lo iv))
      (assert-equal 1/2 (interval-hi iv))))

  (define-test "interval-min/max"
    (let ([a (interval 1 4)]
          [b (interval 2 3)])
      (let ([mn (interval-min a b)])
        (assert-equal 1 (interval-lo mn))
        (assert-equal 3 (interval-hi mn)))
      (let ([mx (interval-max a b)])
        (assert-equal 2 (interval-lo mx))
        (assert-equal 4 (interval-hi mx))))))

;;; ============================================================================
;;; Set Operation Tests
;;; ============================================================================

(test-group "interval-set-ops"
  (define-test "interval-union hull"
    (let ([iv (interval-union (interval 1 3) (interval 5 7))])
      (assert-equal 1 (interval-lo iv))
      (assert-equal 7 (interval-hi iv))))

  (define-test "interval-intersection overlap"
    (let ([iv (interval-intersection (interval 1 5) (interval 3 7))])
      (assert-equal 3 (interval-lo iv))
      (assert-equal 5 (interval-hi iv))))

  (define-test "interval-intersection disjoint returns empty"
    (assert-equal '(error empty)
                  (interval-intersection (interval 1 2) (interval 4 5))))

  (define-test "interval-bisect splits at midpoint"
    (let* ([iv (interval 0 10)]
           [halves (interval-bisect iv)]
           [left (car halves)]
           [right (cdr halves)])
      (assert-equal 0 (interval-lo left))
      (assert-equal 5 (interval-hi left))
      (assert-equal 5 (interval-lo right))
      (assert-equal 10 (interval-hi right))))

  (define-test "interval-hull-list multiple intervals"
    (let ([iv (interval-hull-list (list (interval 1 2)
                                        (interval 5 6)
                                        (interval 3 4)))])
      (assert-equal 1 (interval-lo iv))
      (assert-equal 6 (interval-hi iv)))))

;;; ============================================================================
;;; Box (Multi-dimensional) Tests
;;; ============================================================================

(test-group "interval-boxes"
  (define-test "box-dimension"
    (let ([box (make-box (list (interval 0 1) (interval 0 1) (interval 0 1)))])
      (assert-equal 3 (box-dimension box))))

  (define-test "box-volume unit cube"
    (let ([box (make-box (list (interval 0 1) (interval 0 1) (interval 0 1)))])
      (assert-equal 1 (box-volume box))))

  (define-test "box-volume rectangle"
    (let ([box (make-box (list (interval 0 2) (interval 0 3)))])
      (assert-equal 6 (box-volume box))))

  (define-test "box-contains? point in box"
    (let ([box (make-box (list (interval 0 1) (interval 0 1)))])
      (assert-true (box-contains? box '(0.5 0.5)))
      (assert-true (box-contains? box '(0 0)))    ; Corner
      (assert-false (box-contains? box '(2 0.5))))))

;;; ============================================================================
;;; Soundness Tests (Verify invariant)
;;; ============================================================================

(test-group "interval-soundness"
  (define-test "addition contains true sum"
    ;; For x ∈ [2,3], y ∈ [4,5], x+y must be in [6,8]
    (let ([iv (interval-add (interval 2 3) (interval 4 5))])
      (assert-true (interval-contains? iv 6))   ; 2+4
      (assert-true (interval-contains? iv 8))   ; 3+5
      (assert-true (interval-contains? iv 7)))) ; 2.5+4.5

  (define-test "multiplication contains true product"
    (let ([iv (interval-mul (interval -2 3) (interval 1 2))])
      ;; Extremes: -2*1=-2, -2*2=-4, 3*1=3, 3*2=6
      (assert-true (interval-contains? iv -4))
      (assert-true (interval-contains? iv 6))
      (assert-true (interval-contains? iv 0))))

  (define-test "square contains true square"
    (let ([iv (interval-sqr (interval -2 3))])
      (assert-true (interval-contains? iv 0))   ; 0^2
      (assert-true (interval-contains? iv 4))   ; (-2)^2
      (assert-true (interval-contains? iv 9)))) ; 3^2

  (define-test "sqrt contains true sqrt"
    (let ([iv (interval-sqrt (interval 4 9))])
      (assert-true (interval-contains? iv 2))
      (assert-true (interval-contains? iv 3))
      (assert-true (interval-contains? iv 2.5)))))

;;; ============================================================================
;;; Directed Rounding Tests
;;; ============================================================================

(test-group "directed-rounding"
  (define-test "fl-next-up increases positive numbers"
    (assert-true (< 1.0 (fl-next-up 1.0)))
    (assert-true (< 0.5 (fl-next-up 0.5)))
    (assert-true (< 100.0 (fl-next-up 100.0))))

  (define-test "fl-next-down decreases positive numbers"
    (assert-true (> 1.0 (fl-next-down 1.0)))
    (assert-true (> 0.5 (fl-next-down 0.5)))
    (assert-true (> 100.0 (fl-next-down 100.0))))

  (define-test "fl-next-up increases negative numbers toward zero"
    (assert-true (< -1.0 (fl-next-up -1.0)))
    (assert-true (< -100.0 (fl-next-up -100.0))))

  (define-test "fl-next-down decreases negative numbers away from zero"
    (assert-true (> -1.0 (fl-next-down -1.0)))
    (assert-true (> -100.0 (fl-next-down -100.0))))

  (define-test "fl-next-up from zero is smallest positive"
    (let ([eps (fl-next-up 0.0)])
      (assert-true (> eps 0))
      (assert-true (< eps 1e-300))))

  (define-test "fl-next-down from zero is smallest negative"
    (let ([eps (fl-next-down 0.0)])
      (assert-true (< eps 0))
      (assert-true (> eps -1e-300)))))

(test-group "rigorous-interval-ops"
  (define-test "rigorous add is at least as wide as standard"
    (let ([iv1 (interval 1 2)]
          [iv2 (interval 3 4)])
      (let ([std (interval-add iv1 iv2)]
            [rig (interval-add-rigorous iv1 iv2)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous sub is at least as wide as standard"
    (let ([iv1 (interval 5 8)]
          [iv2 (interval 1 2)])
      (let ([std (interval-sub iv1 iv2)]
            [rig (interval-sub-rigorous iv1 iv2)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous mul is at least as wide as standard"
    (let ([iv1 (interval 2 3)]
          [iv2 (interval 4 5)])
      (let ([std (interval-mul iv1 iv2)]
            [rig (interval-mul-rigorous iv1 iv2)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous div is at least as wide as standard"
    (let ([iv1 (interval 1 2)]
          [iv2 (interval 3 4)])
      (let ([std (interval-div iv1 iv2)]
            [rig (interval-div-rigorous iv1 iv2)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous sqrt is at least as wide as standard"
    (let ([iv (interval 4 9)])
      (let ([std (interval-sqrt iv)]
            [rig (interval-sqrt-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous sqr is at least as wide as standard"
    (let ([iv (interval 2 3)])
      (let ([std (interval-sqr iv)]
            [rig (interval-sqr-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous ops preserve containment"
    ;; For exact inputs, rigorous result must contain exact result
    (let ([x 1.5] [y 2.5])
      (let ([sum-iv (interval-add-rigorous (interval-singleton x)
                                            (interval-singleton y))])
        (assert-true (interval-contains? sum-iv (+ x y))))
      (let ([prod-iv (interval-mul-rigorous (interval-singleton x)
                                             (interval-singleton y))])
        (assert-true (interval-contains? prod-iv (* x y))))
      (let ([quot-iv (interval-div-rigorous (interval-singleton x)
                                             (interval-singleton y))])
        (assert-true (interval-contains? quot-iv (/ x y)))))))

;;; ============================================================================
;;; Tightness Tests
;;; ============================================================================
;;;
;;; These tests verify bounds quality, not just soundness. For monotonic functions
;;; over an interval [a,b], the true range is exactly [f(a),f(b)] (or reversed
;;; for decreasing functions). The computed interval should match exactly or
;;; be very close. Non-monotonic functions need more careful analysis.

(test-group "tightness-basic"
  ;; Helper: compute overestimation ratio
  ;; For computed [lo,hi] vs true range [true-lo, true-hi]:
  ;; ratio = computed_width / true_width (1.0 = perfect)

  (define-test "sqrt tightness: exact for positive intervals"
    ;; sqrt is monotonically increasing, so bounds should be exact
    (let* ([a 4.0] [b 9.0]
           [iv (interval-sqrt (interval a b))]
           [true-lo (sqrt a)]
           [true-hi (sqrt b)])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) 1e-10)
                   "sqrt lower bound should be exact")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) 1e-10)
                   "sqrt upper bound should be exact")))

  (define-test "exp tightness: exact for finite intervals"
    ;; exp is monotonically increasing
    (let* ([a 0.0] [b 2.0]
           [iv (interval-exp (interval a b))]
           [true-lo (exp a)]
           [true-hi (exp b)])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) 1e-10)
                   "exp lower bound should be exact")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) 1e-10)
                   "exp upper bound should be exact")))

  (define-test "log tightness: exact for positive intervals"
    ;; log is monotonically increasing on (0, inf)
    (let* ([a 1.0] [b (exp 2)]
           [iv (interval-log (interval a b))]
           [true-lo (log a)]
           [true-hi (log b)])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) 1e-10)
                   "log lower bound should be exact")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) 1e-10)
                   "log upper bound should be exact")))

  (define-test "neg tightness: exact"
    ;; negation is exact
    (let* ([a -3.0] [b 5.0]
           [iv (interval-neg (interval a b))])
      (assert-equal -5.0 (interval-lo iv))
      (assert-equal 3.0 (interval-hi iv))))

  (define-test "abs tightness: exact"
    ;; abs is exact for intervals not crossing zero
    (let ([iv (interval-abs (interval 2 5))])
      (assert-equal 2 (interval-lo iv))
      (assert-equal 5 (interval-hi iv)))
    ;; Crossing zero: [0, max(|lo|,|hi|)]
    (let ([iv (interval-abs (interval -3 5))])
      (assert-equal 0 (interval-lo iv))
      (assert-equal 5 (interval-hi iv)))))

(test-group "tightness-trigonometric"
  ;; Trig functions are non-monotonic but have known ranges

  (define-test "sin tightness: small interval not containing extrema"
    ;; For [0, 1] rad, sin is monotonically increasing
    ;; True range is [sin(0), sin(1)] = [0, 0.8414...]
    (let* ([a 0.0] [b 1.0]
           [iv (interval-sin (interval a b))]
           [true-lo (sin a)]
           [true-hi (sin b)])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) 1e-10)
                   "sin lower bound should be exact on [0,1]")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) 1e-10)
                   "sin upper bound should be exact on [0,1]")))

  (define-test "sin tightness: interval containing maximum"
    ;; For [0, pi] rad, sin has maximum at pi/2
    ;; True range is [min(0, 0), 1] = [0, 1]
    (let* ([pi 3.141592653589793]
           [iv (interval-sin (interval 0 pi))])
      (assert-true (< (abs (interval-lo iv)) 1e-10)
                   "sin lower bound should be ~0 on [0,pi]")
      (assert-true (< (abs (- (interval-hi iv) 1.0)) 1e-10)
                   "sin upper bound should be 1 on [0,pi]")))

  (define-test "cos tightness: small interval not containing extrema"
    ;; For [0.5, 1.5] rad, cos is monotonically decreasing
    ;; True range is [cos(1.5), cos(0.5)]
    (let* ([a 0.5] [b 1.5]
           [iv (interval-cos (interval a b))]
           [true-lo (cos b)]  ; cos is decreasing
           [true-hi (cos a)])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) 1e-10)
                   "cos lower bound should match cos(1.5)")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) 1e-10)
                   "cos upper bound should match cos(0.5)")))

  (define-test "cos tightness: interval containing minimum"
    ;; For [2, 4] rad (contains pi ≈ 3.14), cos has minimum at pi
    ;; True range is [-1, max(cos(2), cos(4))]
    (let* ([iv (interval-cos (interval 2 4))])
      (assert-true (< (abs (+ (interval-lo iv) 1.0)) 1e-10)
                   "cos lower bound should be -1 on [2,4]")))

  (define-test "tan tightness: within single branch"
    ;; For [-1, 1] rad (within -pi/2 to pi/2), tan is monotonic
    (let* ([a -1.0] [b 1.0]
           [iv (interval-tan (interval a b))]
           [true-lo (tan a)]
           [true-hi (tan b)])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) 1e-10)
                   "tan lower bound should be exact on [-1,1]")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) 1e-10)
                   "tan upper bound should be exact on [-1,1]"))))

(test-group "tightness-hyperbolic"
  (define-test "sinh tightness: exact (monotonic)"
    ;; sinh is monotonically increasing
    (let* ([a -1.0] [b 2.0]
           [iv (interval-sinh (interval a b))]
           [true-lo (sinh a)]
           [true-hi (sinh b)]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) tolerance)
                   "sinh lower bound should be exact")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) tolerance)
                   "sinh upper bound should be exact")))

  (define-test "cosh tightness: positive interval"
    ;; cosh is increasing for x > 0
    (let* ([a 1.0] [b 3.0]
           [iv (interval-cosh (interval a b))]
           [true-lo (cosh a)]
           [true-hi (cosh b)]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) tolerance)
                   "cosh lower bound should be exact for positive interval")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) tolerance)
                   "cosh upper bound should be exact for positive interval")))

  (define-test "cosh tightness: interval crossing zero"
    ;; cosh has minimum at 0, so [lo, hi] → [1, max(cosh(lo), cosh(hi))]
    (let* ([a -2.0] [b 1.0]
           [iv (interval-cosh (interval a b))]
           [true-lo 1.0]  ; cosh(0) = 1
           [true-hi (max (cosh a) (cosh b))]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) tolerance)
                   "cosh lower bound should be 1 for interval crossing zero")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) tolerance)
                   "cosh upper bound should be max of endpoints")))

  (define-test "tanh tightness: exact (monotonic)"
    ;; tanh is monotonically increasing
    (let* ([a -2.0] [b 2.0]
           [iv (interval-tanh (interval a b))]
           [true-lo (tanh a)]
           [true-hi (tanh b)]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) tolerance)
                   "tanh lower bound should be exact")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) tolerance)
                   "tanh upper bound should be exact"))))

(test-group "tightness-inverse-trig"
  (define-test "asin tightness: exact (monotonic)"
    ;; asin is monotonically increasing on [-1, 1]
    (let* ([a -0.5] [b 0.5]
           [iv (interval-asin (interval a b))]
           [true-lo (asin a)]
           [true-hi (asin b)]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) tolerance)
                   "asin lower bound should be exact")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) tolerance)
                   "asin upper bound should be exact")))

  (define-test "acos tightness: exact (monotonic decreasing)"
    ;; acos is monotonically decreasing on [-1, 1]
    (let* ([a -0.5] [b 0.5]
           [iv (interval-acos (interval a b))]
           [true-lo (acos b)]  ; acos is decreasing
           [true-hi (acos a)]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) tolerance)
                   "acos lower bound should be exact")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) tolerance)
                   "acos upper bound should be exact")))

  (define-test "atan tightness: exact (monotonic)"
    ;; atan is monotonically increasing
    (let* ([a -10.0] [b 10.0]
           [iv (interval-atan (interval a b))]
           [true-lo (atan a)]
           [true-hi (atan b)]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) true-lo)) tolerance)
                   "atan lower bound should be exact")
      (assert-true (< (abs (- (interval-hi iv) true-hi)) tolerance)
                   "atan upper bound should be exact"))))

(test-group "tightness-arithmetic"
  (define-test "add tightness: exact"
    ;; Addition is exact: [a,b] + [c,d] = [a+c, b+d]
    (let* ([iv1 (interval 1 3)]
           [iv2 (interval 4 7)]
           [result (interval-add iv1 iv2)])
      (assert-equal 5 (interval-lo result))
      (assert-equal 10 (interval-hi result))))

  (define-test "sub tightness: exact"
    ;; Subtraction is exact: [a,b] - [c,d] = [a-d, b-c]
    (let* ([iv1 (interval 5 8)]
           [iv2 (interval 1 2)]
           [result (interval-sub iv1 iv2)])
      (assert-equal 3 (interval-lo result))
      (assert-equal 7 (interval-hi result))))

  (define-test "mul tightness: positive intervals exact"
    ;; For positive intervals, mul is exact
    (let* ([iv1 (interval 2 3)]
           [iv2 (interval 4 5)]
           [result (interval-mul iv1 iv2)])
      (assert-equal 8 (interval-lo result))
      (assert-equal 15 (interval-hi result))))

  (define-test "div tightness: positive intervals exact"
    ;; For positive intervals, div is exact
    (let* ([iv1 (interval 6 12)]
           [iv2 (interval 2 3)]
           [result (interval-div iv1 iv2)]
           ;; [6,12] / [2,3] = [6,12] * [1/3, 1/2] = [6*1/3, 12*1/2] = [2, 6]
           )
      (assert-equal 2 (interval-lo result))
      (assert-equal 6 (interval-hi result))))

  (define-test "pow tightness: even powers with zero-crossing"
    ;; x^2 for x in [-2, 3] has minimum at 0
    ;; True range: [0, 9]
    (let* ([iv (interval-pow (interval -2 3) 2)])
      (assert-equal 0 (interval-lo iv))
      (assert-equal 9 (interval-hi iv))))

  (define-test "pow tightness: odd powers exact"
    ;; x^3 for x in [-2, 3] is monotonic
    ;; True range: [-8, 27]
    (let* ([iv (interval-pow (interval -2 3) 3)])
      (assert-equal -8 (interval-lo iv))
      (assert-equal 27 (interval-hi iv))))

  (define-test "sqr tightness: zero-crossing optimal"
    ;; x^2 for x in [-1, 2] should give [0, 4], not [-2, 4]
    (let ([iv (interval-sqr (interval -1 2))])
      (assert-equal 0 (interval-lo iv))
      (assert-equal 4 (interval-hi iv))))

  (define-test "sqr vs mul: sqr is tighter for zero-crossing"
    ;; This is the key advantage of dedicated sqr
    (let* ([iv (interval -2 3)]
           [sqr-result (interval-sqr iv)]
           [mul-result (interval-mul iv iv)])
      ;; sqr knows min is 0
      (assert-equal 0 (interval-lo sqr-result))
      ;; mul doesn't: min is -6 from -2*3
      (assert-equal -6 (interval-lo mul-result))
      ;; Both agree on max
      (assert-equal 9 (interval-hi sqr-result))
      (assert-equal 9 (interval-hi mul-result)))))

(test-group "tightness-overestimation-ratios"
  ;; For non-exact operations, verify overestimation is bounded

  (define-test "mul with mixed signs: reasonable overestimation"
    ;; [−2, 3] × [−1, 4]: products are 2, −8, −3, 12 → true range [−8, 12], width 20
    ;; Interval computes correctly for this case
    (let* ([iv1 (interval -2 3)]
           [iv2 (interval -1 4)]
           [result (interval-mul iv1 iv2)]
           [computed-width (interval-width result)]
           [true-width 20])
      (assert-equal 20 computed-width "mul with mixed signs should be exact")))

  (define-test "recip tightness: positive intervals"
    ;; 1/[2, 4] = [1/4, 1/2] = [0.25, 0.5]
    (let* ([iv (interval-recip (interval 2 4))]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) 0.25)) tolerance))
      (assert-true (< (abs (- (interval-hi iv) 0.5)) tolerance)))))

;;; ============================================================================
;;; interval-log10 Tests
;;; ============================================================================

(test-group "interval-log10"
  (define-test "log10 positive interval"
    ;; log10([1, 100]) = [0, 2]
    (let* ([iv (interval-log10 (interval 1 100))]
           [tolerance 1e-10])
      (assert-true (< (abs (interval-lo iv)) tolerance))
      (assert-true (< (abs (- (interval-hi iv) 2.0)) tolerance))))

  (define-test "log10 powers of 10"
    ;; log10([10, 1000]) = [1, 3]
    (let* ([iv (interval-log10 (interval 10 1000))]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) 1.0)) tolerance))
      (assert-true (< (abs (- (interval-hi iv) 3.0)) tolerance))))

  (define-test "log10 fractional"
    ;; log10([0.1, 1]) = [-1, 0]
    (let* ([iv (interval-log10 (interval 0.1 1))]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) -1.0)) tolerance))
      (assert-true (< (abs (interval-hi iv)) tolerance))))

  (define-test "log10 negative returns error"
    (assert-equal '(error domain-error)
                  (interval-log10 (interval -2 -1))))

  (define-test "log10 crossing zero clamps"
    ;; Should handle negative lower bound
    (let ([iv (interval-log10 (interval -1 10))])
      (assert-true (< (interval-lo iv) -100))  ; Very negative (from epsilon)
      (assert-true (< (abs (- (interval-hi iv) 1.0)) 1e-10)))))

;;; ============================================================================
;;; interval-atan2 Tests
;;; ============================================================================

(test-group "interval-atan2"
  (define-test "atan2 quadrant I (x > 0, y > 0)"
    (let* ([pi 3.141592653589793]
           [iv (interval-atan2 (interval 1 2) (interval 1 2))]
           [tolerance 1e-10])
      ;; atan2([1,2], [1,2]) should be around [atan(1/2), atan(2/1)]
      (assert-true (< (abs (- (interval-lo iv) (atan 0.5))) tolerance))
      (assert-true (< (abs (- (interval-hi iv) (atan 2.0))) tolerance))))

  (define-test "atan2 quadrant IV (x > 0, y < 0)"
    (let* ([iv (interval-atan2 (interval -2 -1) (interval 1 2))]
           [tolerance 1e-10])
      ;; y negative, x positive → negative angles
      (assert-true (< (interval-hi iv) 0))))

  (define-test "atan2 positive x axis"
    ;; atan2(0, 1) = 0
    (let* ([iv (interval-atan2 (interval-singleton 0) (interval-singleton 1))]
           [tolerance 1e-10])
      (assert-true (< (abs (interval-lo iv)) tolerance))
      (assert-true (< (abs (interval-hi iv)) tolerance))))

  (define-test "atan2 positive y axis"
    ;; atan2(1, 0) = pi/2
    (let* ([pi 3.141592653589793]
           [iv (interval-atan2 (interval-singleton 1) (interval-singleton 0))]
           [tolerance 1e-10])
      (assert-true (< (abs (- (interval-lo iv) (/ pi 2))) tolerance))
      (assert-true (< (abs (- (interval-hi iv) (/ pi 2))) tolerance))))

  (define-test "atan2 left half-plane crossing y=0 gives full range"
    ;; When x < 0 and y crosses 0, result wraps around ±π
    (let* ([pi 3.141592653589793]
           [iv (interval-atan2 (interval -1 1) (interval -2 -1))])
      (assert-equal (- pi) (interval-lo iv))
      (assert-equal pi (interval-hi iv))))

  (define-test "atan2 origin is error"
    (assert-equal '(error domain-error)
                  (interval-atan2 (interval-singleton 0) (interval-singleton 0)))))

;;; ============================================================================
;;; Rigorous Transcendental Tests
;;; ============================================================================

(test-group "rigorous-transcendentals"
  (define-test "rigorous exp is at least as wide as standard"
    (let ([iv (interval 0 2)])
      (let ([std (interval-exp iv)]
            [rig (interval-exp-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous log is at least as wide as standard"
    (let ([iv (interval 1 10)])
      (let ([std (interval-log iv)]
            [rig (interval-log-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous log10 is at least as wide as standard"
    (let ([iv (interval 1 100)])
      (let ([std (interval-log10 iv)]
            [rig (interval-log10-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous sin is at least as wide as standard"
    (let ([iv (interval 0 1)])
      (let ([std (interval-sin iv)]
            [rig (interval-sin-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous cos is at least as wide as standard"
    (let ([iv (interval 0 1)])
      (let ([std (interval-cos iv)]
            [rig (interval-cos-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous atan is at least as wide as standard"
    (let ([iv (interval -10 10)])
      (let ([std (interval-atan iv)]
            [rig (interval-atan-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous sinh is at least as wide as standard"
    (let ([iv (interval -1 2)])
      (let ([std (interval-sinh iv)]
            [rig (interval-sinh-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous cosh is at least as wide as standard"
    (let ([iv (interval -1 2)])
      (let ([std (interval-cosh iv)]
            [rig (interval-cosh-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous tanh is at least as wide as standard"
    (let ([iv (interval -2 2)])
      (let ([std (interval-tanh iv)]
            [rig (interval-tanh-rigorous iv)])
        (assert-true (<= (interval-lo rig) (interval-lo std)))
        (assert-true (>= (interval-hi rig) (interval-hi std))))))

  (define-test "rigorous exp preserves containment"
    ;; For exact input, rigorous result must contain exact output
    (let* ([x 1.5]
           [iv (interval-exp-rigorous (interval-singleton x))])
      (assert-true (interval-contains? iv (exp x)))))

  (define-test "rigorous log preserves containment"
    (let* ([x 2.5]
           [iv (interval-log-rigorous (interval-singleton x))])
      (assert-true (interval-contains? iv (log x)))))

  (define-test "rigorous sin preserves containment"
    (let* ([x 0.7]
           [iv (interval-sin-rigorous (interval-singleton x))])
      (assert-true (interval-contains? iv (sin x)))))

  (define-test "rigorous cos preserves containment"
    (let* ([x 0.7]
           [iv (interval-cos-rigorous (interval-singleton x))])
      (assert-true (interval-contains? iv (cos x))))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
