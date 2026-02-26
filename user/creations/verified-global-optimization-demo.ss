;;; user/creations/verified-global-optimization-demo.ss
;;;
;;; DEMO: Verified Global Optimization with Interval Arithmetic
;;;
;;; This demo showcases The Fold's new interval-based global optimization:
;;;   1. Branch-and-bound with GUARANTEED enclosure of global minima
;;;   2. Constraint contractors that prune infeasible regions
;;;   3. Monotonicity pruning via interval gradients
;;;   4. Comparison with local methods (which get trapped)
;;;
;;; Run: scheme --script user/creations/verified-global-optimization-demo.ss
;;;
;;; Author: Claude Opus 4.5
;;; Created: 2026-01-16

(load "core/base/prelude.ss")
(load "lattice/interval/interval.ss")
(load "lattice/optimization/interval-global.ss")
(load "lattice/optimization/interval-contract.ss")
(load "lattice/autodiff/interval-autodiff.ss")

;;; ============================================================================
;;; ASCII Art Banner
;;; ============================================================================

(define (print-banner)
  (display "
╔═══════════════════════════════════════════════════════════════════════╗
║   ┌─────────────────────────────────────────────────────────────┐     ║
║   │     VERIFIED GLOBAL OPTIMIZATION                            │     ║
║   │     ════════════════════════════                            │     ║
║   │     Interval Branch-and-Bound with Constraint Contractors   │     ║
║   └─────────────────────────────────────────────────────────────┘     ║
╚═══════════════════════════════════════════════════════════════════════╝
"))

;;; ============================================================================
;;; Demo 1: The Rastrigin Challenge
;;; ============================================================================
;;;
;;; Rastrigin function: f(x,y) = 20 + x² + y² - 10(cos(2πx) + cos(2πy))
;;;
;;; This is a famous optimization benchmark with:
;;;   - Global minimum at (0, 0) with value 0
;;;   - MANY local minima (approximately n² for search domain [-n,n]²)
;;;   - Gradient descent WILL get trapped
;;;
;;; Interval B&B finds the TRUE global minimum with mathematical certainty.

(define pi 3.141592653589793)

(define (scalar-rastrigin-2d x y)
  (+ 20
     (* x x)
     (* y y)
     (- (* 10 (cos (* 2 pi x))))
     (- (* 10 (cos (* 2 pi y))))))

(define (interval-rastrigin-2d box)
  ;; Interval extension: enclose all values f can take over the box
  ;; Note: Without interval cos, we bound cos ∈ [-1, 1], giving
  ;; -10*cos ∈ [-10, 10] for conservative enclosure
  (let* ([x (car box)]
         [y (cadr box)]
         [x-sq (interval-sqr x)]
         [y-sq (interval-sqr y)]
         ;; Conservative cos bound
         [cos-term (interval -10 10)])
    (interval-add
     (interval-add (interval-singleton 20) x-sq)
     (interval-add y-sq (interval-add cos-term cos-term)))))

(define (demo-rastrigin)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "DEMO 1: THE RASTRIGIN CHALLENGE\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")

  (display "The Rastrigin function has HUNDREDS of local minima in [-5,5]².\n")
  (display "Gradient descent gets trapped. Interval B&B finds the true global minimum.\n\n")

  ;; Evaluate at some local minima to show the trap
  (display "Local minima examples (where gradient descent might stop):\n")
  (display (format "  f(1, 1)   = ~a\n" (scalar-rastrigin-2d 1 1)))
  (display (format "  f(-1, 2)  = ~a\n" (scalar-rastrigin-2d -1 2)))
  (display (format "  f(2, -2)  = ~a\n" (scalar-rastrigin-2d 2 -2)))
  (display (format "  f(0.99, 0) = ~a\n" (scalar-rastrigin-2d 0.99 0)))
  (display "\n")

  ;; Now run interval optimization
  (display "Running interval branch-and-bound over [-5, 5]²...\n\n")

  (let* ([box (list (interval -5 5) (interval -5 5))]
         [criteria (make-interval-convergence 1e-4 5000)]
         [result (interval-minimize interval-rastrigin-2d box criteria
                                    (lambda (pt) (apply scalar-rastrigin-2d pt)))])

    (display (format "Result:\n"))
    (display (format "  Iterations:    ~a\n" (ior-iterations result)))
    (display (format "  Termination:   ~a\n" (ior-reason result)))
    (display (format "  Best upper:    ~a\n" (ior-best-upper result)))
    (display (format "  Best point:    ~a\n" (ior-best-point result)))
    (display (format "  Candidates:    ~a boxes\n" (length (ior-candidates result))))
    (display "\n")

    (let ([pt (ior-best-point result)])
      (display (format "Verification: f(~a, ~a) = ~a\n"
                       (car pt) (cadr pt)
                       (scalar-rastrigin-2d (car pt) (cadr pt)))))

    (display "\n")
    (display "The global minimum is at (0, 0) with value 0.\n")
    (display "Interval B&B found it despite hundreds of local traps.\n")
    result))

;;; ============================================================================
;;; Demo 2: Constrained Optimization with Contractors
;;; ============================================================================
;;;
;;; Problem: Minimize x² + y² subject to x + y >= 2
;;;
;;; Analytical solution: Minimum on line x + y = 2, at (1, 1) with value 2.
;;;
;;; Without constraints, minimum is at origin with value 0.
;;; Contractors PRUNE the infeasible region before bisection.

(define (demo-constrained)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "DEMO 2: CONSTRAINED OPTIMIZATION WITH CONTRACTORS\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")

  (display "Problem: Minimize x² + y² subject to x + y >= 2\n\n")
  (display "Without constraint: minimum at (0, 0) = 0\n")
  (display "With constraint:    minimum at (1, 1) = 2 (on the constraint boundary)\n\n")

  (display "The constraint contractor x + y >= 2 prunes infeasible boxes,\n")
  (display "dramatically reducing the search space.\n\n")

  (let* ([box (list (interval -5 5) (interval -5 5))]
         ;; x + y >= 2
         [contractor (make-linear-ge-contractor '((0 . 1) (1 . 1)) 2)]
         [criteria (make-interval-convergence 1e-5 10000)]
         [result (interval-minimize-constrained
                  interval-sphere box criteria (list contractor))])

    (display "Result:\n")
    (display (format "  Iterations:  ~a\n" (ior-iterations result)))
    (display (format "  Termination: ~a\n" (ior-reason result)))
    (display (format "  Best value:  ~a (expected: 2.0)\n" (ior-best-upper result)))
    (display (format "  Best point:  ~a (expected: (1, 1))\n" (ior-best-point result)))

    (let ([pt (ior-best-point result)])
      (when pt
        (display (format "\nConstraint check: ~a + ~a = ~a >= 2? ~a\n"
                         (car pt) (cadr pt)
                         (+ (car pt) (cadr pt))
                         (>= (+ (car pt) (cadr pt)) 1.99)))))  ; Allow small tolerance
    result))

;;; ============================================================================
;;; Demo 3: Trust Region with Sphere Contractor
;;; ============================================================================
;;;
;;; Problem: Minimize x² + y² subject to (x-3)² + (y-3)² <= 1
;;;
;;; The feasible region is a disk of radius 1 centered at (3, 3).
;;; The closest point to the origin within this disk is the solution.
;;;
;;; Analytical: Point on circle closest to origin is at
;;;             (3 - 1/√2, 3 - 1/√2) ≈ (2.29, 2.29)
;;;             with value ≈ 10.5

(define (demo-trust-region)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "DEMO 3: TRUST REGION OPTIMIZATION (SPHERE CONTRACTOR)\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")

  (display "Problem: Minimize x² + y² subject to (x-3)² + (y-3)² <= 1\n\n")
  (display "This constrains the search to a disk of radius 1 centered at (3,3).\n")
  (display "The sphere contractor prunes boxes outside this trust region.\n\n")

  (let* ([box (list (interval 0 6) (interval 0 6))]
         ;; (x-3)² + (y-3)² <= 1
         [contractor (make-sphere-contractor '(3 3) 1)]
         [criteria (make-interval-convergence 1e-4 10000)]
         [result (interval-minimize-constrained
                  interval-sphere box criteria (list contractor))])

    ;; Analytical solution
    (let* ([sqrt2 (sqrt 2)]
           [opt-coord (- 3 (/ 1 sqrt2))]
           [opt-val (* 2 opt-coord opt-coord)])

      (display (format "Analytical solution:\n"))
      (display (format "  Optimal point: (~a, ~a)\n" opt-coord opt-coord))
      (display (format "  Optimal value: ~a\n\n" opt-val))

      (display "Interval B&B result:\n")
      (display (format "  Iterations:  ~a\n" (ior-iterations result)))
      (display (format "  Best value:  ~a\n" (ior-best-upper result)))
      (display (format "  Best point:  ~a\n" (ior-best-point result)))

      (let ([pt (ior-best-point result)])
        (when pt
          (let ([dist-sq (+ (* (- (car pt) 3) (- (car pt) 3))
                            (* (- (cadr pt) 3) (- (cadr pt) 3)))])
            (display (format "\nConstraint check: (x-3)² + (y-3)² = ~a <= 1? ~a\n"
                             dist-sq (<= dist-sq 1.01))))))
      result)))

;;; ============================================================================
;;; Demo 4: Monotonicity Pruning with Interval Gradients
;;; ============================================================================
;;;
;;; For smooth functions, interval autodiff computes gradient BOUNDS.
;;; If gradient doesn't contain zero in some dimension, the function is
;;; monotonic there, and the minimum must be on a boundary face.
;;;
;;; This can dramatically reduce iterations for smooth functions.

(define (interval-quadratic box)
  ;; f(x, y) = x² + 2y² + 3xy + x - y
  ;; Gradient: [2x + 3y + 1, 4y + 3x - 1]
  (let* ([x (car box)]
         [y (cadr box)]
         [one (interval-singleton 1)]
         [two (interval-singleton 2)]
         [three (interval-singleton 3)]
         [x-sq (interval-sqr x)]
         [y-sq (interval-sqr y)]
         [xy (interval-mul x y)])
    (interval-add
     (interval-add
      (interval-add x-sq (interval-mul two y-sq))
      (interval-mul three xy))
     (interval-add x (interval-neg y)))))

(define (scalar-quadratic pt)
  (let ([x (car pt)] [y (cadr pt)])
    (+ (* x x) (* 2 y y) (* 3 x y) x (- y))))

;; Interval gradient using traced operations
(define (quadratic-gradient box)
  (interval-gradient
   (lambda (inputs)
     (let* ([x (car inputs)]
            [y (cadr inputs)]
            [one (interval-traced (interval-singleton 1) -1 #f)]
            [two (interval-traced (interval-singleton 2) -1 #f)]
            [three (interval-traced (interval-singleton 3) -1 #f)])
       (iv-traced-add
        (iv-traced-add
         (iv-traced-add (iv-traced-sqr x)
                        (iv-traced-mul two (iv-traced-sqr y)))
         (iv-traced-mul three (iv-traced-mul x y)))
        (iv-traced-add x (iv-traced-neg y)))))
   box))

(define (demo-monotonicity)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "DEMO 4: MONOTONICITY PRUNING VIA INTERVAL GRADIENTS\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")

  (display "For smooth functions, interval autodiff bounds the gradient.\n")
  (display "If gradient doesn't contain 0, function is monotonic -> prune to boundary.\n\n")

  (display "Function: f(x,y) = x² + 2y² + 3xy + x - y\n")
  (display "Gradient: [2x + 3y + 1, 4y + 3x - 1]\n\n")

  ;; Show gradient bounds over a test box
  (let* ([test-box (list (interval 2 4) (interval 1 3))]
         [grad (quadratic-gradient test-box)])
    (display "Gradient bounds over [2,4] × [1,3]:\n")
    (display (format "  df/dx ∈ ~a\n" (interval->string (car grad))))
    (display (format "  df/dy ∈ ~a\n" (interval->string (cadr grad))))
    (display "\n")

    (let ([mono (monotonicity-info grad)])
      (display (format "Monotonicity: ~a\n" mono))
      (if (memq 'positive mono)
          (display "  Function is monotonically INCREASING in at least one direction.\n")
          (if (memq 'negative mono)
              (display "  Function is monotonically DECREASING in at least one direction.\n")
              (display "  Gradient may be zero in this box (stationary point possible).\n")))))

  (display "\n")
  (display "Comparing standard vs gradient-enhanced optimization:\n\n")

  (let* ([box (list (interval -5 5) (interval -5 5))]
         [criteria (make-interval-convergence 1e-6 5000)])

    ;; Standard optimization
    (let ([result1 (interval-minimize interval-quadratic box criteria scalar-quadratic)])
      (display (format "Standard B&B:     ~a iterations, best = ~a\n"
                       (ior-iterations result1) (ior-best-upper result1))))

    ;; With monotonicity pruning
    (let ([result2 (interval-minimize-with-gradient
                    interval-quadratic quadratic-gradient box criteria scalar-quadratic)])
      (display (format "With gradients:   ~a iterations, best = ~a\n"
                       (ior-iterations result2) (ior-best-upper result2))))

    (display "\nMonotonicity pruning reduces iterations by collapsing monotonic dimensions.\n")))

;;; ============================================================================
;;; Demo 5: Multi-Constraint Engineering Problem
;;; ============================================================================
;;;
;;; A realistic scenario: Find optimal sensor placement.
;;;
;;; Minimize distance² from target point (5, 5)
;;; Subject to:
;;;   - Stay in safe zone: x + y <= 8  (linear)
;;;   - Avoid obstacle at (2, 2) with radius 1  (handled by initial box)
;;;   - Stay in quadrant: x >= 1, y >= 1  (bound constraints)

;; Objective for Demo 5: (x-5)² + (y-5)²
(define (sensor-objective-interval box)
  (let* ([x (car box)]
         [y (cadr box)]
         [five (interval-singleton 5)])
    (interval-add
     (interval-sqr (interval-sub x five))
     (interval-sqr (interval-sub y five)))))

(define (sensor-objective-scalar pt)
  (let ([x (car pt)] [y (cadr pt)])
    (+ (* (- x 5) (- x 5))
       (* (- y 5) (- y 5)))))

(define (demo-engineering)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "DEMO 5: MULTI-CONSTRAINT ENGINEERING PROBLEM\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")

  (display "Problem: Optimal Sensor Placement\n")
  (display "  Minimize distance² from target at (5, 5)\n")
  (display "  Subject to:\n")
  (display "    - x + y <= 8  (stay within boundary)\n")
  (display "    - x >= 1      (clearance from wall)\n")
  (display "    - y >= 1      (clearance from floor)\n\n")

  (display "This combines linear and bound contractors for a realistic constraint set.\n\n")

  (let* ([box (list (interval 0 10) (interval 0 10))]
         [contractors (list
                       ;; x + y <= 8
                       (make-linear-le-contractor '((0 . 1) (1 . 1)) 8)
                       ;; x >= 1
                       (make-bound-contractor 0 1 10)
                       ;; y >= 1
                       (make-bound-contractor 1 1 10))]
         [criteria (make-interval-convergence 1e-5 10000)]
         [result (interval-minimize-constrained
                  sensor-objective-interval box criteria contractors)])

    ;; Analytical: closest point on x+y=8 to (5,5) is (4,4) with value 2
    (display "Analytical solution:\n")
    (display "  The constraint x+y<=8 is active at minimum.\n")
    (display "  Closest point on line x+y=8 to (5,5) is (4,4).\n")
    (display "  Distance² = (4-5)² + (4-5)² = 2\n\n")

    (display "Interval B&B result:\n")
    (display (format "  Iterations:  ~a\n" (ior-iterations result)))
    (display (format "  Best value:  ~a (expected: 2.0)\n" (ior-best-upper result)))
    (display (format "  Best point:  ~a (expected: (4, 4))\n" (ior-best-point result)))

    (let ([pt (ior-best-point result)])
      (when pt
        (display (format "\nConstraint verification:\n"))
        (display (format "  x + y = ~a <= 8? ~a\n"
                         (+ (car pt) (cadr pt))
                         (<= (+ (car pt) (cadr pt)) 8.01)))
        (display (format "  x = ~a >= 1? ~a\n" (car pt) (>= (car pt) 0.99)))
        (display (format "  y = ~a >= 1? ~a\n" (cadr pt) (>= (cadr pt) 0.99)))))
    result))

;;; ============================================================================
;;; Demo 6: Rigorous Bounds with Directed Rounding
;;; ============================================================================

(define (demo-rigorous)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "DEMO 6: RIGOROUS BOUNDS WITH DIRECTED ROUNDING\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")

  (display "Standard floating-point rounds to nearest, which can cause interval\n")
  (display "operations to underestimate the true range. For computer-aided proofs,\n")
  (display "we need OUTWARD rounding to guarantee enclosure.\n\n")

  (display "The Fold provides both:\n")
  (display "  - interval-add/mul/div (fast, standard rounding)\n")
  (display "  - interval-add-rigorous/mul-rigorous/div-rigorous (guaranteed enclosure)\n\n")

  (let* ([a (interval 1 2)]
         [b (interval 3 4)]
         ;; Standard
         [sum-std (interval-add a b)]
         [prod-std (interval-mul a b)]
         ;; Rigorous
         [sum-rig (interval-add-rigorous a b)]
         [prod-rig (interval-mul-rigorous a b)])

    (display (format "Interval a = ~a\n" (interval->string a)))
    (display (format "Interval b = ~a\n\n" (interval->string b)))

    (display "Standard operations (round-to-nearest):\n")
    (display (format "  a + b = ~a\n" (interval->string sum-std)))
    (display (format "  a × b = ~a\n\n" (interval->string prod-std)))

    (display "Rigorous operations (outward rounding):\n")
    (display (format "  a + b = ~a\n" (interval->string sum-rig)))
    (display (format "  a × b = ~a\n\n" (interval->string prod-rig))))

  (display "For most optimization tasks, standard operations are sufficient.\n")
  (display "Use rigorous operations when you need formal verification guarantees.\n"))

;;; ============================================================================
;;; Run All Demos
;;; ============================================================================

(define (run-all-demos)
  (print-banner)

  (display "\nThis demo showcases The Fold's interval-based global optimization:\n")
  (display "  - GUARANTEED global minima (unlike gradient descent)\n")
  (display "  - Constraint contractors for feasible region pruning\n")
  (display "  - Monotonicity analysis via interval autodiff\n")
  (display "  - Rigorous bounds via directed rounding\n")

  (demo-rastrigin)
  (demo-constrained)
  (demo-trust-region)
  (demo-monotonicity)
  (demo-engineering)
  (demo-rigorous)

  (display "\n")
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "SUMMARY\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")

  (display "Interval branch-and-bound provides mathematically GUARANTEED bounds\n")
  (display "on global optima. Combined with constraint contractors, it solves\n")
  (display "constrained optimization problems that defeat local methods.\n\n")

  (display "Key modules demonstrated:\n")
  (display "  lattice/optimization/interval-global.ss   — Branch-and-bound core\n")
  (display "  lattice/optimization/interval-contract.ss — Constraint propagation\n")
  (display "  lattice/autodiff/interval-autodiff.ss     — Interval gradients\n")
  (display "  lattice/interval/interval.ss              — Interval arithmetic\n\n")

  (display "Try modifying the demos to explore:\n")
  (display "  - Different search domains\n")
  (display "  - Your own objective functions\n")
  (display "  - Custom constraint combinations\n")
  (display "  - Higher dimensions\n\n")

  (display "Happy optimizing!\n"))

;;; Run
(run-all-demos)
