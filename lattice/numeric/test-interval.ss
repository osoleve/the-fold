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
    (assert-equal 'division-by-zero
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
    (assert-equal 'domain-error
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
    (assert-equal 'empty
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
;;; Run Tests
;;; ============================================================================

(run-all-tests)
