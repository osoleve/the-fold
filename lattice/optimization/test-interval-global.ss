;;; lattice/optimization/test-interval-global.ss — Tests for Interval Global Optimization
;;;
;;; Tests the branch-and-bound global optimizer with various test functions.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/optimization/interval-global.ss")

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

;;; Check if a point is within a box
(define (point-in-box? point box)
  (and (= (length point) (length box))
       (let loop ([pts point] [ivs box])
         (or (null? pts)
             (and (<= (interval-lo (car ivs)) (car pts))
                  (<= (car pts) (interval-hi (car ivs)))
                  (loop (cdr pts) (cdr ivs)))))))

;;; Check if a point is within any of the candidate boxes
(define (point-in-boxes? point boxes)
  (exists (lambda (box) (point-in-box? point box)) boxes))

;;; Check if value is close to expected
(define (approx= a b tol)
  (< (abs (- a b)) tol))

;;; ============================================================================
;;; Test: 1D Quadratic (x²)
;;; ============================================================================

(test-group "1D Quadratic"

  (define-test "x² over [-5, 5] finds minimum near 0"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           ;; Using new API: f-scalar is optional
           [result (interval-minimize f-interval box criteria)])
      ;; Global minimum is at x=0 with value 0
      (assert-true (< (ior-best-upper result) 1e-10))
      (let ([best-pt (ior-best-point result)])
        (assert-true (< (abs (car best-pt)) 1e-3)))))

  (define-test "x² converges with tight tolerance"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -10 10))]
           [criteria (make-interval-convergence 1e-8 50000)]
           [result (interval-minimize f-interval box criteria)])
      (assert-true (< (ior-best-upper result) 1e-14))))

  (define-test "x² with explicit f-scalar (backward compat)"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [f-scalar (lambda (pt) (* (car pt) (car pt)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           ;; Old API still works with optional f-scalar
           [result (interval-minimize f-interval box criteria f-scalar)])
      (assert-true (< (ior-best-upper result) 1e-10))))
)

;;; ============================================================================
;;; Test: 1D Absolute Value (|x|)
;;; ============================================================================

(define (interval-abs-custom box)
  (interval-abs (car box)))

(define (scalar-abs-custom pt)
  (abs (car pt)))

(test-group "1D Absolute Value"

  (define-test "|x| over [-3, 3] finds minimum near 0"
    (let* ([box (list (interval -3 3))]
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize interval-abs-custom box criteria)])
      (assert-true (< (ior-best-upper result) 1e-5))))

  (define-test "|x| over [1, 5] finds minimum at boundary"
    (let* ([box (list (interval 1 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize interval-abs-custom box criteria)])
      ;; Minimum is at x=1 with value 1
      (assert-true (< (abs (- (ior-best-upper result) 1)) 1e-5))))
)

;;; ============================================================================
;;; Test: 2D Sphere Function
;;; ============================================================================

(test-group "2D Sphere"

  (define-test "Sphere over [-5,5]² finds minimum at origin"
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 20000)]
           [result (interval-minimize interval-sphere box criteria)])
      (assert-true (< (ior-best-upper result) 1e-6))
      (let ([pt (ior-best-point result)])
        (assert-true (and (< (abs (car pt)) 1e-2)
                          (< (abs (cadr pt)) 1e-2))))))

  (define-test "Sphere with asymmetric domain"
    (let* ([box (list (interval -2 5) (interval -3 4))]
           [criteria (make-interval-convergence 1e-4 20000)]
           [result (interval-minimize interval-sphere box criteria)])
      ;; Global minimum is at origin with value 0
      ;; Tolerance slightly relaxed since asymmetric domain needs more bisections
      (assert-true (< (ior-best-upper result) 1e-5))))
)

;;; ============================================================================
;;; Test: 2D Rosenbrock Function
;;; ============================================================================

(test-group "2D Rosenbrock"

  (define-test "Rosenbrock over [-2,2]² finds minimum near (1,1)"
    (let* ([box (list (interval -2 2) (interval -2 2))]
           [criteria (make-interval-convergence 1e-2 50000)]
           [result (interval-minimize interval-rosenbrock box criteria)])
      ;; Global minimum at (1,1) with value 0
      (assert-true (< (ior-best-upper result) 1e-2))
      (let ([pt (ior-best-point result)])
        (assert-true (and (< (abs (- (car pt) 1)) 0.1)
                          (< (abs (- (cadr pt) 1)) 0.1))))))

  (define-test "Rosenbrock finds correct value at minimum"
    (let* ([box (list (interval 0.5 1.5) (interval 0.5 1.5))]
           [criteria (make-interval-convergence 1e-4 30000)]
           [result (interval-minimize interval-rosenbrock box criteria)])
      (assert-true (< (ior-best-upper result) 1e-4))))
)

;;; ============================================================================
;;; Test: Rastrigin Function (multimodal)
;;; ============================================================================

(test-group "Rastrigin (multimodal)"

  (define-test "1D Rastrigin finds global minimum near 0"
    (let* ([box (list (interval -5.12 5.12))]
           [criteria (make-interval-convergence 1e-3 30000)]
           ;; Use explicit f-scalar for Rastrigin since interval version is conservative
           [result (interval-minimize interval-rastrigin box criteria scalar-rastrigin)])
      ;; Global minimum at origin with value 0
      (let ([pt (ior-best-point result)])
        (assert-true (< (abs (car pt)) 0.5)))))

  (define-test "2D Rastrigin finds global minimum near origin"
    (let* ([box (list (interval -2 2) (interval -2 2))]
           [criteria (make-interval-convergence 1e-2 50000)]
           [result (interval-minimize interval-rastrigin box criteria scalar-rastrigin)])
      (let ([pt (ior-best-point result)])
        (assert-true (and (< (abs (car pt)) 0.5)
                          (< (abs (cadr pt)) 0.5))))))
)

;;; ============================================================================
;;; Test: Maximization
;;; ============================================================================

(define (interval-neg-sphere box)
  (interval-neg (interval-sphere box)))

(define (scalar-neg-sphere pt)
  (- (scalar-sphere pt)))

(test-group "Maximization"

  (define-test "Maximize -x² finds maximum at 0"
    (let* ([f-interval (lambda (box) (interval-neg (interval-sqr (car box))))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           ;; New API: f-scalar optional for maximize too
           [result (interval-maximize f-interval box criteria)])
      ;; Maximum of -x² is 0 at x=0
      (assert-true (> (ior-best-upper result) -1e-10))))
)

;;; ============================================================================
;;; Test: Edge Cases
;;; ============================================================================

(test-group "Edge Cases"

  (define-test "Single point interval returns that point"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval 3 3))]  ; Point interval
           [criteria (make-interval-convergence 1e-6 100)]
           [result (interval-minimize f-interval box criteria)])
      (let ([pt (ior-best-point result)])
        (assert-true (approx= (car pt) 3 1e-10)))))

  (define-test "Constant function over box"
    (let* ([f-interval (lambda (box) (interval-singleton 42))]
           [box (list (interval -10 10))]
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize f-interval box criteria)])
      (assert-true (approx= (ior-best-upper result) 42 1e-10))))

  (define-test "Max iterations terminates"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -1000 1000))]
           [criteria (make-interval-convergence 1e-20 100)]  ; Very tight tol, few iters
           [result (interval-minimize f-interval box criteria)])
      (assert-equal 'max-iterations (ior-reason result))))
)

;;; ============================================================================
;;; Test: Result Structure
;;; ============================================================================

(test-group "Result Structure"

  (define-test "Result contains expected fields"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 1000)]
           [result (interval-minimize f-interval box criteria)])
      (assert-true (interval-opt-result? result))
      (assert-true (list? (ior-candidates result)))
      (assert-true (number? (ior-best-upper result)))
      (assert-true (integer? (ior-iterations result)))
      (assert-true (symbol? (ior-reason result)))))

  (define-test "Candidates are within original box"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [original-box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 1000)]
           [result (interval-minimize f-interval original-box criteria)]
           [candidates (ior-candidates result)])
      (assert-true (for-all (lambda (cand)
                              (let ([iv (car cand)])
                                (and (>= (interval-lo iv) -5)
                                     (<= (interval-hi iv) 5))))
                            candidates))))

  (define-test "ior-solution-box returns hull of candidates"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 1000)]
           [result (interval-minimize f-interval box criteria)]
           [sol-box (ior-solution-box result)])
      (assert-true (list? sol-box))
      (assert-true (interval? (car sol-box)))))
)

;;; ============================================================================
;;; Test: Enclosure Property (Gemini QA recommendation)
;;; ============================================================================
;;;
;;; The fundamental guarantee: the true global minimum is contained within
;;; one of the returned candidate boxes.

(test-group "Enclosure Property"

  (define-test "x² enclosure: origin in candidates"
    ;; True minimum at x=0
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize f-interval box criteria)]
           [candidates (ior-candidates result)])
      ;; The point (0) must be in at least one candidate box
      (assert-true (point-in-boxes? '(0) candidates))))

  (define-test "Sphere enclosure: origin in candidates"
    ;; True minimum at (0, 0)
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 20000)]
           [result (interval-minimize interval-sphere box criteria)]
           [candidates (ior-candidates result)])
      (assert-true (point-in-boxes? '(0 0) candidates))))

  (define-test "Rosenbrock enclosure: (1,1) in candidates"
    ;; True minimum at (1, 1)
    (let* ([box (list (interval -2 2) (interval -2 2))]
           [criteria (make-interval-convergence 1e-2 50000)]
           [result (interval-minimize interval-rosenbrock box criteria)]
           [candidates (ior-candidates result)])
      (assert-true (point-in-boxes? '(1 1) candidates))))

  (define-test "Solution box contains true minimum"
    ;; ior-solution-box should contain the true minimum
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 20000)]
           [result (interval-minimize interval-sphere box criteria)]
           [sol-box (ior-solution-box result)])
      ;; Origin should be in the solution box
      (assert-true (point-in-box? '(0 0) sol-box))))
)

;;; ============================================================================
;;; Tightness Tests
;;; ============================================================================
;;;
;;; Verify the optimizer returns tight bounds on the global minimum.
;;; A tight result has:
;;; 1. best-upper close to the true minimum value
;;; 2. Candidate boxes small (width below tolerance)
;;; 3. Gap between lower and upper bounds small

(test-group "Tightness: Bound Quality"

  (define-test "x² bound accuracy"
    ;; True minimum: f(0) = 0
    ;; Optimizer should find value very close to 0
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -10 10))]
           [criteria (make-interval-convergence 1e-8 50000)]
           [result (interval-minimize f-interval box criteria)])
      ;; best-upper should be extremely close to 0
      (assert-true (< (ior-best-upper result) 1e-14))
      ;; Reason should be exhausted (converged), not max-iterations
      (assert-equal 'exhausted (ior-reason result))))

  (define-test "Sphere bound accuracy"
    ;; True minimum: f(0,0) = 0
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [criteria (make-interval-convergence 1e-6 30000)]
           [result (interval-minimize interval-sphere box criteria)])
      ;; Should achieve tight bound
      (assert-true (< (ior-best-upper result) 1e-10))))

  (define-test "Rosenbrock bound accuracy"
    ;; True minimum: f(1,1) = 0
    (let* ([box (list (interval 0.5 1.5) (interval 0.5 1.5))]
           [criteria (make-interval-convergence 1e-6 50000)]
           [result (interval-minimize interval-rosenbrock box criteria)])
      ;; Should get very close to 0
      (assert-true (< (ior-best-upper result) 1e-6))))
)

(test-group "Tightness: Candidate Box Width"

  (define-test "Candidate boxes converge by width or gap"
    ;; Candidates converge when EITHER width < tol OR gap < gap-tol
    ;; This test verifies all candidates meet at least one criterion
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [tol 1e-4]
           [criteria (make-interval-convergence tol 10000 tol)]  ; width-tol = gap-tol
           [result (interval-minimize f-interval box criteria)]
           [candidates (ior-candidates result)])
      ;; All candidate boxes should satisfy at least one convergence criterion
      (assert-true (for-all (lambda (cand)
                              (let* ([width (interval-width (car cand))]
                                     [f-iv (f-interval cand)]
                                     [gap (- (interval-hi f-iv) (interval-lo f-iv))])
                                (or (<= width tol) (<= gap tol))))
                            candidates))))

  (define-test "2D candidate boxes reasonably small"
    ;; For 2D, verify candidates are reasonably small (not necessarily at tolerance)
    (let* ([box (list (interval -3 3) (interval -3 3))]
           [tol 1e-3]
           [criteria (make-interval-convergence tol 30000)]
           [result (interval-minimize interval-sphere box criteria)]
           [candidates (ior-candidates result)])
      ;; All candidate boxes should have max-width much smaller than original
      (assert-true (for-all (lambda (cand)
                              (let ([widths (map interval-width cand)])
                                ;; Should be at least 10x smaller than original width 6
                                (< (apply max widths) 0.6)))
                            candidates))))
)

(test-group "Tightness: Solution Box"

  (define-test "Solution box much smaller than original"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [tol 1e-4]
           [criteria (make-interval-convergence tol 10000)]
           [result (interval-minimize f-interval box criteria)]
           [sol-box (ior-solution-box result)])
      ;; Solution box is hull of candidates, should be much smaller than original
      ;; Original width is 10, solution should be << 1 for x²
      (assert-true (< (interval-width (car sol-box)) 0.1))))

  (define-test "2D solution box reasonably tight"
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [tol 1e-3]
           [criteria (make-interval-convergence tol 30000)]
           [result (interval-minimize interval-sphere box criteria)]
           [sol-box (ior-solution-box result)])
      ;; Solution box should be much smaller than original
      (assert-true (< (interval-width (car sol-box)) 1))
      (assert-true (< (interval-width (cadr sol-box)) 1))))
)

(test-group "Tightness: Iteration Efficiency"

  (define-test "Simple function converges quickly"
    ;; x² should not need many iterations for moderate tolerance
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -10 10))]
           [criteria (make-interval-convergence 1e-4 10000)]
           [result (interval-minimize f-interval box criteria)])
      ;; Should converge in reasonable number of iterations
      (assert-true (< (ior-iterations result) 5000))))

  (define-test "2D sphere converges reasonably"
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [criteria (make-interval-convergence 1e-3 30000)]
           [result (interval-minimize interval-sphere box criteria)])
      ;; 2D takes more iterations but should still converge
      (assert-true (< (ior-iterations result) 20000))))
)

(test-group "Tightness: Overestimation Bounds"

  (define-test "Interval evaluation not overly pessimistic"
    ;; For x² over [0, 2], interval gives [0, 4]
    ;; True range is [0, 4], so interval is exact here
    (let* ([box (list (interval 0 2))]
           [f-iv (interval-sqr (car box))])
      (assert-equal 0 (interval-lo f-iv))
      (assert-equal 4 (interval-hi f-iv))))

  (define-test "Sphere interval reasonably tight"
    ;; For x² + y² over [0,1] x [0,1]
    ;; True range is [0, 2]
    ;; Interval: sqr([0,1]) + sqr([0,1]) = [0,1] + [0,1] = [0,2]
    (let* ([box (list (interval 0 1) (interval 0 1))]
           [f-iv (interval-sphere box)])
      (assert-equal 0 (interval-lo f-iv))
      (assert-equal 2 (interval-hi f-iv))))
)

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
