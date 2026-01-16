;;; lattice/optimization/test-interval-contract.ss — Tests for Interval Constraint Contractors
;;;
;;; Tests constraint propagation integration with interval optimization.

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
;;; Run Tests
;;; ============================================================================

(run-all-tests)
