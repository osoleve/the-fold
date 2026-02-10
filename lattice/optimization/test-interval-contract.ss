;;; lattice/optimization/test-interval-contract.ss — Tests for Interval Constraint Contractors
;;;
;;; Tests constraint propagation integration with interval optimization.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/optimization/interval-contract.ss")

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (interval-approx= iv1 iv2 tol)
  (and (approx= (interval-lo iv1) (interval-lo iv2) tol)
       (approx= (interval-hi iv1) (interval-hi iv2) tol)))

;;; ============================================================================
;;; Test: Basic Contractors
;;; ============================================================================

(test-group "Basic Contractors"

  (define-test "bound-contractor narrows interval"
    (let* ([box (list (interval -10 10) (interval -10 10))]
           [contractor (make-bound-contractor 0 -5 5)]
           [result (contractor box)])
      (assert-true (interval-approx= (car result) (interval -5 5) 1e-10))))

  (define-test "bound-contractor returns empty for infeasible"
    (let* ([box (list (interval 0 10))]
           [contractor (make-bound-contractor 0 20 30)]
           [result (contractor box)])
      (assert-equal 'empty result)))

  (define-test "equality-contractor creates singleton"
    (let* ([box (list (interval -10 10))]
           [contractor (make-equality-contractor 0 3)]
           [result (contractor box)])
      (assert-true (interval-singleton? (car result)))
      (assert-true (approx= (interval-lo (car result)) 3 1e-10))))

  (define-test "equality-contractor returns empty for infeasible"
    (let* ([box (list (interval 0 10))]
           [contractor (make-equality-contractor 0 -5)]
           [result (contractor box)])
      (assert-equal 'empty result)))
)

;;; ============================================================================
;;; Test: Linear Constraint Contractors
;;; ============================================================================

(test-group "Linear Contractors"

  (define-test "linear-le x + y <= 1 contracts [0,1]x[0,1]"
    ;; x + y <= 1 over [0,1] x [0,1]
    ;; For x: x <= 1 - y, y in [0,1], so x <= 1 - 0 = 1 (no change to upper)
    ;; For y: y <= 1 - x, x in [0,1], so y <= 1 - 0 = 1 (no change)
    ;; But actually the contractor should work...
    (let* ([box (list (interval 0 1) (interval 0 1))]
           [contractor (make-linear-le-contractor '((0 . 1) (1 . 1)) 1)]
           [result (contractor box)])
      (assert-true (not (eq? result 'empty)))))

  (define-test "linear-le 2x + y <= 3 contracts [-1,2]x[-1,2]"
    (let* ([box (list (interval -1 2) (interval -1 2))]
           ;; 2x + y <= 3
           [contractor (make-linear-le-contractor '((0 . 2) (1 . 1)) 3)]
           [result (contractor box)])
      ;; For x: 2x <= 3 - y, y >= -1, so 2x <= 3 - (-1) = 4, x <= 2 (no change)
      ;; For y: y <= 3 - 2x, x >= -1, so y <= 3 - 2*(-1) = 5, but y <= 2 already
      ;; The constraint is satisfied by the whole box
      (assert-true (not (eq? result 'empty)))))

  (define-test "linear-le x + y <= 0 contracts [0,1]x[0,1] to corner"
    (let* ([box (list (interval 0 1) (interval 0 1))]
           ;; x + y <= 0, with x,y >= 0 means x=y=0
           [contractor (make-linear-le-contractor '((0 . 1) (1 . 1)) 0)]
           [result (contractor box)])
      ;; For x: x <= 0 - y, y >= 0, so x <= 0
      ;; For y: y <= 0 - x, x >= 0, so y <= 0
      (assert-true (approx= (interval-hi (car result)) 0 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 0 1e-10))))

  (define-test "linear-ge contracts correctly"
    (let* ([box (list (interval 0 10) (interval 0 10))]
           ;; x + y >= 5
           [contractor (make-linear-ge-contractor '((0 . 1) (1 . 1)) 5)]
           [result (contractor box)])
      ;; For x: x >= 5 - y, y <= 10, so x >= 5 - 10 = -5, but x >= 0 already
      ;; For y: y >= 5 - x, x <= 10, so y >= 5 - 10 = -5, but y >= 0 already
      (assert-true (not (eq? result 'empty)))))

  (define-test "linear-eq contracts both directions"
    (let* ([box (list (interval 0 10) (interval 0 10))]
           ;; x + y = 5
           [contractor (make-linear-eq-contractor '((0 . 1) (1 . 1)) 5)]
           [result (contractor box)])
      ;; x + y = 5, with x,y in [0,10]
      ;; x = 5 - y, y in [0,10], so x in [-5, 5] ∩ [0,10] = [0,5]
      ;; y = 5 - x, x in [0,5], so y in [0, 5]
      (assert-true (approx= (interval-hi (car result)) 5 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 5 1e-10))))

  (define-test "linear-le infeasible returns empty"
    (let* ([box (list (interval 5 10) (interval 5 10))]
           ;; x + y <= 5, but x,y >= 5 means x+y >= 10
           [contractor (make-linear-le-contractor '((0 . 1) (1 . 1)) 5)]
           [result (contractor box)])
      (assert-equal 'empty result)))

  (define-test "zero coefficient variable is unchanged"
    ;; Constraint 0*x + y <= 5: x should be unchanged, y should contract
    (let* ([box (list (interval -10 10) (interval 0 10))]
           [contractor (make-linear-le-contractor '((0 . 0) (1 . 1)) 5)]
           [result (contractor box)])
      ;; x unchanged (coef=0), y contracted to [0, 5]
      (assert-true (interval-approx= (car result) (interval -10 10) 1e-10))
      (assert-true (interval-approx= (cadr result) (interval 0 5) 1e-10))))
)

;;; ============================================================================
;;; Test: Sphere Contractors
;;; ============================================================================

(test-group "Sphere Contractors"

  (define-test "sphere-contractor keeps feasible region"
    (let* ([box (list (interval -2 2) (interval -2 2))]
           ;; x² + y² <= 4 (circle of radius 2)
           [contractor (make-sphere-contractor '(0 0) 4)]
           [result (contractor box)])
      ;; Box [-2,2]² fits entirely in circle of radius 2
      (assert-true (not (eq? result 'empty)))))

  (define-test "sphere-contractor shrinks box"
    (let* ([box (list (interval -5 5) (interval -5 5))]
           ;; x² + y² <= 4 (circle of radius 2)
           [contractor (make-sphere-contractor '(0 0) 4)]
           [result (contractor box)])
      ;; Should shrink to approximately [-2, 2] in each dimension
      (assert-true (< (interval-hi (car result)) 5))
      (assert-true (> (interval-lo (car result)) -5))))

  (define-test "sphere-contractor returns empty for infeasible"
    (let* ([box (list (interval 5 10) (interval 5 10))]
           ;; x² + y² <= 4, but x,y >= 5 means x²+y² >= 50
           [contractor (make-sphere-contractor '(0 0) 4)]
           [result (contractor box)])
      (assert-equal 'empty result)))

  (define-test "sphere-contractor with offset center"
    (let* ([box (list (interval 0 10) (interval 0 10))]
           ;; (x-3)² + (y-3)² <= 4 (circle centered at (3,3))
           [contractor (make-sphere-contractor '(3 3) 4)]
           [result (contractor box)])
      ;; Should keep region around (3,3)
      (assert-true (not (eq? result 'empty)))
      (assert-true (interval-contains? (car result) 3))
      (assert-true (interval-contains? (cadr result) 3))))
)

;;; ============================================================================
;;; Test: Contractor Combinators
;;; ============================================================================

(test-group "Contractor Combinators"

  (define-test "contract-all applies multiple contractors"
    (let* ([box (list (interval -10 10) (interval -10 10))]
           [c1 (make-bound-contractor 0 -5 5)]
           [c2 (make-bound-contractor 1 0 3)]
           [result (contract-all (list c1 c2) box)])
      (assert-true (interval-approx= (car result) (interval -5 5) 1e-10))
      (assert-true (interval-approx= (cadr result) (interval 0 3) 1e-10))))

  (define-test "contract-all reaches fixpoint"
    (let* ([box (list (interval 0 10) (interval 0 10))]
           ;; x + y = 5, iterating should reach fixpoint
           [contractor (make-linear-eq-contractor '((0 . 1) (1 . 1)) 5)]
           [result (contract-all (list contractor) box)])
      ;; After fixpoint: x in [0,5], y in [0,5]
      (assert-true (approx= (interval-hi (car result)) 5 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 5 1e-10))))

  (define-test "contract-all returns empty if any contractor fails"
    (let* ([box (list (interval 0 5) (interval 0 5))]
           [c1 (make-bound-contractor 0 0 3)]  ; Feasible
           [c2 (make-bound-contractor 0 10 20)]  ; Infeasible
           [result (contract-all (list c1 c2) box)])
      (assert-equal 'empty result)))
)

;;; ============================================================================
;;; Test: Constrained Optimization
;;; ============================================================================

(test-group "Constrained Optimization"

  (define-test "minimize x² with x >= 1 finds minimum at x=1"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [contractors (list (make-bound-contractor 0 1 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize-constrained f-interval box criteria contractors)])
      ;; Minimum of x² for x >= 1 is at x=1 with value 1
      (assert-true (approx= (ior-best-upper result) 1 1e-4))
      (let ([pt (ior-best-point result)])
        (assert-true (approx= (car pt) 1 1e-3)))))

  (define-test "minimize sphere with linear constraint"
    ;; Minimize x² + y² subject to x + y >= 2
    ;; Analytical: minimum on line x + y = 2, at x = y = 1, value = 2
    (let* ([f-interval interval-sphere]
           [box (list (interval -5 5) (interval -5 5))]
           [contractors (list (make-linear-ge-contractor '((0 . 1) (1 . 1)) 2))]
           [criteria (make-interval-convergence 1e-3 20000)]
           [result (interval-minimize-constrained f-interval box criteria contractors)])
      (assert-true (approx= (ior-best-upper result) 2 0.1))
      (let ([pt (ior-best-point result)])
        (assert-true (approx= (car pt) 1 0.2))
        (assert-true (approx= (cadr pt) 1 0.2)))))

  (define-test "infeasible constraints return infeasible"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval 0 5))]
           [contractors (list (make-bound-contractor 0 10 20))]  ; No overlap
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize-constrained f-interval box criteria contractors)])
      (assert-equal 'infeasible (ior-reason result))))

  (define-test "sphere constraint prunes search space"
    ;; Minimize x² + y² subject to (x-2)² + (y-2)² <= 1
    ;; The constraint is a circle of radius 1 centered at (2,2)
    ;; The closest point to origin in this circle is at (2-1/√2, 2-1/√2) ≈ (1.29, 1.29)
    ;; with value ≈ 3.34
    (let* ([f-interval interval-sphere]
           [box (list (interval 0 5) (interval 0 5))]
           [contractors (list (make-sphere-contractor '(2 2) 1))]
           [criteria (make-interval-convergence 1e-2 20000)]
           [result (interval-minimize-constrained f-interval box criteria contractors)])
      ;; The minimum should be around 3.17 (= (2 - 1/√2)² * 2)
      (assert-true (> (ior-best-upper result) 3))
      (assert-true (< (ior-best-upper result) 4))))
)

;;; ============================================================================
;;; Test: Box Constraints Helper
;;; ============================================================================

(test-group "Box Constraints"

  (define-test "make-box-constraints creates bound contractors"
    (let* ([bounds '((0 . 5) (-3 . 3))]
           [contractors (make-box-constraints bounds)]
           [box (list (interval -10 10) (interval -10 10))]
           [result (contract-all contractors box)])
      (assert-true (interval-approx= (car result) (interval 0 5) 1e-10))
      (assert-true (interval-approx= (cadr result) (interval -3 3) 1e-10))))
)

;;; ============================================================================
;;; Tightness Tests
;;; ============================================================================
;;;
;;; Verify contractors produce optimal (or near-optimal) contractions.
;;; A tight contractor returns the smallest box guaranteed to contain
;;; all feasible points. Under-contraction is safe but wasteful.

(test-group "Tightness: Bound Contractors"

  (define-test "bound-contractor is exactly tight"
    ;; Bound contractor simply intersects, which is mathematically exact
    (let* ([box (list (interval -10 10))]
           [contractor (make-bound-contractor 0 -3 7)]
           [result (contractor box)])
      ;; Should be exactly [-3, 7]
      (assert-equal -3 (interval-lo (car result)))
      (assert-equal 7 (interval-hi (car result)))))

  (define-test "equality-contractor produces singleton"
    ;; The tightest possible interval containing only value v is [v, v]
    (let* ([box (list (interval -10 10))]
           [contractor (make-equality-contractor 0 2.5)]
           [result (contractor box)])
      (assert-equal 2.5 (interval-lo (car result)))
      (assert-equal 2.5 (interval-hi (car result)))))
)

(test-group "Tightness: Linear Contractors"

  (define-test "linear-le single variable is exact"
    ;; Constraint: 2x <= 6, box [0, 10]
    ;; Exact solution: x <= 3, so [0, 3]
    (let* ([box (list (interval 0 10))]
           [contractor (make-linear-le-contractor '((0 . 2)) 6)]
           [result (contractor box)])
      (assert-equal 0 (interval-lo (car result)))
      (assert-true (approx= (interval-hi (car result)) 3 1e-10))))

  (define-test "linear-ge single variable is exact"
    ;; Constraint: 2x >= 4, box [0, 10]
    ;; Exact solution: x >= 2, so [2, 10]
    (let* ([box (list (interval 0 10))]
           [contractor (make-linear-ge-contractor '((0 . 2)) 4)]
           [result (contractor box)])
      (assert-true (approx= (interval-lo (car result)) 2 1e-10))
      (assert-equal 10 (interval-hi (car result)))))

  (define-test "linear-eq two variables contracts optimally"
    ;; Constraint: x + y = 10, box [0, 20] x [0, 20]
    ;; For x: x = 10 - y, y in [0, 20], so x in [-10, 10] ∩ [0, 20] = [0, 10]
    ;; For y: y = 10 - x, x in [0, 10], so y in [0, 10]
    (let* ([box (list (interval 0 20) (interval 0 20))]
           [contractor (make-linear-eq-contractor '((0 . 1) (1 . 1)) 10)]
           [result (contract-all (list contractor) box)])
      ;; Both should be [0, 10] after fixpoint
      (assert-true (approx= (interval-lo (car result)) 0 1e-10))
      (assert-true (approx= (interval-hi (car result)) 10 1e-10))
      (assert-true (approx= (interval-lo (cadr result)) 0 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 10 1e-10))))

  (define-test "linear-le two variables tightness"
    ;; Constraint: x + y <= 8, box [0, 10] x [0, 10]
    ;; For x: x <= 8 - y, y >= 0, so x <= 8
    ;; For y: y <= 8 - x, x >= 0, so y <= 8
    ;; Optimal contraction: [0, 8] x [0, 8]
    (let* ([box (list (interval 0 10) (interval 0 10))]
           [contractor (make-linear-le-contractor '((0 . 1) (1 . 1)) 8)]
           [result (contractor box)])
      (assert-true (approx= (interval-hi (car result)) 8 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 8 1e-10))))

  (define-test "linear with negative coefficient"
    ;; Constraint: x - y <= 5, box [-10, 10] x [-10, 10]
    ;; For x: x <= 5 + y, y >= -10, so x <= 5 + (-10) = -5? No wait...
    ;; Actually: x - y <= 5 means x <= 5 + y
    ;; If y in [-10, 10], then x <= 5 + 10 = 15 (but x <= 10 anyway)
    ;; For y: -y <= 5 - x, so y >= x - 5
    ;; If x in [-10, 10], then y >= -10 - 5 = -15 (but y >= -10 anyway)
    ;; No actual contraction expected here
    (let* ([box (list (interval -10 10) (interval -10 10))]
           [contractor (make-linear-le-contractor '((0 . 1) (1 . -1)) 5)]
           [result (contractor box)])
      ;; Should not shrink much (constraint is satisfied by whole box)
      (assert-true (not (eq? result 'empty)))
      (assert-true (interval-approx= (car result) (interval -10 10) 1e-10))
      (assert-true (interval-approx= (cadr result) (interval -10 10) 1e-10))))

  (define-test "linear constraint forces tight contraction"
    ;; Constraint: 3x + 2y <= 12, box [0, 10] x [0, 10]
    ;; For x: 3x <= 12 - 2y, y >= 0, so 3x <= 12, x <= 4
    ;; For y: 2y <= 12 - 3x, x >= 0, so 2y <= 12, y <= 6
    (let* ([box (list (interval 0 10) (interval 0 10))]
           [contractor (make-linear-le-contractor '((0 . 3) (1 . 2)) 12)]
           [result (contractor box)])
      (assert-true (approx= (interval-hi (car result)) 4 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 6 1e-10))))
)

(test-group "Tightness: Sphere Contractors"

  (define-test "sphere contractor exact for axis-aligned case"
    ;; Constraint: x² + y² <= 9 (radius 3), box [-5, 5] x [-5, 5]
    ;; For x with y at origin: |x| <= 3, so x in [-3, 3]
    ;; Similarly for y
    ;; But sphere contractor uses minimum distance from other dims,
    ;; so with y in [-5, 5], min distance from 0 is 0, giving x in [-3, 3]
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [contractor (make-sphere-contractor '(0 0) 9)]
           [result (contractor box)])
      ;; Each dimension should be approximately [-3, 3]
      (assert-true (approx= (interval-lo (car result)) -3 1e-10))
      (assert-true (approx= (interval-hi (car result)) 3 1e-10))
      (assert-true (approx= (interval-lo (cadr result)) -3 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 3 1e-10))))

  (define-test "sphere contractor with offset center"
    ;; Constraint: (x-2)² + y² <= 4 (radius 2, centered at (2,0))
    ;; Box [0, 10] x [-5, 5]
    ;; For x: (x-2)² <= 4 - y², with y at min distance 0, so (x-2)² <= 4
    ;;        x in [0, 4]
    ;; For y: y² <= 4 - (x-2)², with x closest to 2 being 2, so y² <= 4
    ;;        y in [-2, 2]
    (let* ([box (list (interval 0 10) (interval -5 5))]
           [contractor (make-sphere-contractor '(2 0) 4)]
           [result (contractor box)])
      (assert-true (approx= (interval-lo (car result)) 0 1e-10))
      (assert-true (approx= (interval-hi (car result)) 4 1e-10))
      (assert-true (approx= (interval-lo (cadr result)) -2 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 2 1e-10))))

  (define-test "sphere contractor 3D case"
    ;; Constraint: x² + y² + z² <= 25 (radius 5)
    ;; Box [-10, 10]³
    ;; Each dimension should contract to [-5, 5]
    (let* ([box (list (interval -10 10) (interval -10 10) (interval -10 10))]
           [contractor (make-sphere-contractor '(0 0 0) 25)]
           [result (contractor box)])
      (assert-true (approx= (interval-lo (car result)) -5 1e-10))
      (assert-true (approx= (interval-hi (car result)) 5 1e-10))
      (assert-true (approx= (interval-lo (cadr result)) -5 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 5 1e-10))
      (assert-true (approx= (interval-lo (caddr result)) -5 1e-10))
      (assert-true (approx= (interval-hi (caddr result)) 5 1e-10))))

  (define-test "sphere contractor partial overlap"
    ;; Constraint: x² + y² <= 4 (radius 2)
    ;; Box [1, 5] x [-3, 3]
    ;; For x: x in [1, 5], x² in [1, 25], need x² + y² <= 4
    ;;        With y at min distance 0: x² <= 4, x in [-2, 2]
    ;;        Intersect with [1, 5]: x in [1, 2]
    ;; For y: With x in [1, 2], min (x-0)² is 1, so y² <= 3, y in [-√3, √3]
    (let* ([box (list (interval 1 5) (interval -3 3))]
           [contractor (make-sphere-contractor '(0 0) 4)]
           [result (contractor box)])
      (assert-true (approx= (interval-lo (car result)) 1 1e-10))
      (assert-true (approx= (interval-hi (car result)) 2 1e-10))
      ;; y bounds depend on implementation details of fixpoint iteration
      (assert-true (< (interval-hi (cadr result)) 3))))
)

(test-group "Tightness: Combined Contractors"

  (define-test "multiple contractors tighten iteratively"
    ;; Constraints: x + y <= 8, x >= 2, y >= 3
    ;; Box [0, 10] x [0, 10]
    ;; After bounds: [2, 10] x [3, 10]
    ;; After x + y <= 8 with y >= 3: x <= 8 - 3 = 5, so x in [2, 5]
    ;; After x + y <= 8 with x >= 2: y <= 8 - 2 = 6, so y in [3, 6]
    (let* ([box (list (interval 0 10) (interval 0 10))]
           [c1 (make-bound-contractor 0 2 10)]
           [c2 (make-bound-contractor 1 3 10)]
           [c3 (make-linear-le-contractor '((0 . 1) (1 . 1)) 8)]
           [result (contract-all (list c1 c2 c3) box)])
      (assert-true (approx= (interval-lo (car result)) 2 1e-10))
      (assert-true (approx= (interval-hi (car result)) 5 1e-10))
      (assert-true (approx= (interval-lo (cadr result)) 3 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 6 1e-10))))

  (define-test "fixpoint tightens linear equality"
    ;; Constraint: x + y = 6, box [0, 10] x [0, 10]
    ;; Round 1: x in [0, 6], y in [0, 6]
    ;; Round 2: x in [0, 6], y in [0, 6] (fixpoint)
    (let* ([box (list (interval 0 10) (interval 0 10))]
           [contractor (make-linear-eq-contractor '((0 . 1) (1 . 1)) 6)]
           [result (contract-fixpoint (list contractor) box 10)])
      (assert-true (approx= (interval-lo (car result)) 0 1e-10))
      (assert-true (approx= (interval-hi (car result)) 6 1e-10))
      (assert-true (approx= (interval-lo (cadr result)) 0 1e-10))
      (assert-true (approx= (interval-hi (cadr result)) 6 1e-10))))

  (define-test "overestimation ratio bounded for linear contractors"
    ;; For linear constraints, contractors should achieve optimal or near-optimal
    ;; bounds. Measure how much larger the result is vs the true feasible region.
    ;; Constraint: x + 2y <= 10, x >= 0, y >= 0
    ;; True feasible: triangle with vertices (0,0), (10,0), (0,5)
    ;; Area = 25
    ;; Box after contraction: [0, 10] x [0, 5], area = 50
    ;; Overestimation ratio = 50/25 = 2
    (let* ([box (list (interval 0 20) (interval 0 20))]
           [c1 (make-bound-contractor 0 0 20)]
           [c2 (make-bound-contractor 1 0 20)]
           [c3 (make-linear-le-contractor '((0 . 1) (1 . 2)) 10)]
           [result (contract-all (list c1 c2 c3) box)]
           [result-area (* (interval-width (car result))
                           (interval-width (cadr result)))]
           [true-area 25]
           [ratio (/ result-area true-area)])
      ;; Ratio should be 2 (box vs triangle)
      (assert-true (approx= ratio 2 0.1))))
)

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
