;;; core/dynamics/test-stability.ss — Stability Analysis Tests
;;;
;;; Tests for stability analysis:
;;;   - Fixed point detection
;;;   - Jacobian computation
;;;   - Eigenvalue analysis
;;;   - Stability classification
;;;   - Linear and nonlinear systems
;;;
;;; Run from project root: scheme --script core/dynamics/test-stability.ss

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")  ;; Required for QR algorithm
(load "lattice/linalg/matrix-eigen.ss")
(load "lattice/numeric/complex.ss")
(load "lattice/sim/dynamics/ode-system.ss")
(load "lattice/sim/dynamics/stability.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(define (test-approx name expected actual tolerance)
  (display "  ")
  (display name)
  (display ": ")
  (if (< (abs (- expected actual)) tolerance)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)
       (display "\n    diff: ")
       (display (abs (- expected actual)))))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Fixed Point Detection
;;; ====
(test-section "Fixed Point Detection")

(define harmonic (harmonic-oscillator 1.0))

(test "origin is fixed point for harmonic oscillator"
      #t
      (is-fixed-point? harmonic (vector 0.0 0.0) 1e-6))

(test "non-zero point is not fixed point"
      #f
      (is-fixed-point? harmonic (vector 1.0 0.0) 1e-6))

;;; Test Lotka-Volterra equilibrium
(define lv (lotka-volterra 1.0 0.1 0.1 0.02))
(define lv-eq (vector 5.0 10.0))  ; Known equilibrium

(test "Lotka-Volterra equilibrium is fixed point"
      #t
      (is-fixed-point? lv lv-eq 1e-6))

;;; ====
;;; Jacobian Computation
;;; ====
(test-section "Jacobian Computation")

;;; Linear system: dx/dt = Ax should have Jacobian = A
(define A-linear (list 'matrix 2 2 (vector 1 2 3 4)))
(define linear-sys (linear-ode A-linear))
(define jac-linear (compute-jacobian linear-sys (vector 1.0 1.0) 1e-6))

(test "linear system Jacobian is constant"
      #t
      (let* ([A-data (matrix-data A-linear)]
             [J-data (matrix-data jac-linear)])
            (< (vec-norm (vec-sub (list->vector (vector->list A-data))
                                  (list->vector (vector->list J-data))))
               0.01)))

;;; Harmonic oscillator: dx/dt = v, dv/dt = -ω²x
;;; Jacobian at any point: J = [[0, 1], [-ω², 0]]
(define jac-harmonic (compute-jacobian harmonic (vector 0.0 0.0) 1e-6))

(test-approx "harmonic oscillator J[0,0]"
             0.0
             (matrix-ref jac-harmonic 0 0)
             0.01)

(test-approx "harmonic oscillator J[0,1]"
             1.0
             (matrix-ref jac-harmonic 0 1)
             0.01)

(test-approx "harmonic oscillator J[1,0]"
             -1.0
             (matrix-ref jac-harmonic 1 0)
             0.01)

(test-approx "harmonic oscillator J[1,1]"
             0.0
             (matrix-ref jac-harmonic 1 1)
             0.01)

;;; ====
;;; Stability Classification - Linear Systems
;;; ====
(test-section "Linear System Stability")

;;; Stable node: both eigenvalues negative real
(define stable-node-A (list 'matrix 2 2 (vector -1 0 0 -2)))
(define stable-node-result (analyze-linear-system stable-node-A))

(test "stable node classification"
      'stable-node
      (car stable-node-result))

;;; Unstable node: both eigenvalues positive real
(define unstable-node-A (list 'matrix 2 2 (vector 1 0 0 2)))
(define unstable-node-result (analyze-linear-system unstable-node-A))

(test "unstable node classification"
      'unstable-node
      (car unstable-node-result))

;;; Saddle point: eigenvalues with opposite signs
(define saddle-A (list 'matrix 2 2 (vector 1 0 0 -1)))
(define saddle-result (analyze-linear-system saddle-A))

(test "saddle point classification"
      'saddle
      (car saddle-result))

;;; Stable spiral: complex eigenvalues with negative real part
(define stable-spiral-A (list 'matrix 2 2 (vector -0.5 2 -2 -0.5)))
(define stable-spiral-result (analyze-linear-system stable-spiral-A))

(test "stable spiral classification"
      'stable-spiral
      (car stable-spiral-result))

;;; Unstable spiral: complex eigenvalues with positive real part
(define unstable-spiral-A (list 'matrix 2 2 (vector 0.5 2 -2 0.5)))
(define unstable-spiral-result (analyze-linear-system unstable-spiral-A))

(test "unstable spiral classification"
      'unstable-spiral
      (car unstable-spiral-result))

;;; Center: purely imaginary eigenvalues
(define center-A (list 'matrix 2 2 (vector 0 1 -1 0)))
(define center-result (analyze-linear-system center-A))

(test "center classification"
      'center
      (car center-result))

;;; ====
;;; Stability Classification - Nonlinear Systems
;;; ====
(test-section "Nonlinear System Stability")

;;; Harmonic oscillator has center at origin
(define harmonic-stability (analyze-stability harmonic (vector 0.0 0.0) 1e-6))

(test "harmonic oscillator is center"
      'center
      (car harmonic-stability))

;;; Damped oscillator has stable spiral at origin
(define damped (damped-oscillator 1.0 0.5))
(define damped-stability (analyze-stability damped (vector 0.0 0.0) 1e-6))

(test "damped oscillator is stable spiral"
      'stable-spiral
      (car damped-stability))

;;; Van der Pol has unstable origin (limit cycle system)
(define vdp (van-der-pol 1.0))
(define vdp-stability (analyze-stability vdp (vector 0.0 0.0) 1e-6))

(test "Van der Pol origin is unstable"
      #t
      (unstable? (car vdp-stability)))

;;; ====
;;; Stability Predicates
;;; ====
(test-section "Stability Predicates")

(test "stable node is stable"
      #t
      (stable? 'stable-node))

(test "stable spiral is stable"
      #t
      (stable? 'stable-spiral))

(test "saddle is unstable"
      #t
      (unstable? 'saddle))

(test "unstable node is unstable"
      #t
      (unstable? 'unstable-node))

(test "center is not asymptotically stable"
      #f
      (asymptotically-stable? 'center))

(test "stable node is asymptotically stable"
      #t
      (asymptotically-stable? 'stable-node))

;;; ====
;;; Matrix Inversion
;;; ====
(test-section "Matrix Inversion")

(define test-matrix (list 'matrix 2 2 (vector 1 2 3 4)))
(define inv-matrix (matrix-invert-simple test-matrix))
(define product (matrix-mul test-matrix inv-matrix))

(test-approx "matrix inverse identity (0,0)"
             1.0
             (matrix-ref product 0 0)
             0.01)

(test-approx "matrix inverse identity (0,1)"
             0.0
             (matrix-ref product 0 1)
             0.01)

(test-approx "matrix inverse identity (1,0)"
             0.0
             (matrix-ref product 1 0)
             0.01)

(test-approx "matrix inverse identity (1,1)"
             1.0
             (matrix-ref product 1 1)
             0.01)

;;; ====
;;; Linearization
;;; ====
(test-section "Linearization")

(define pendulum-sys (pendulum 1.0))
(define pend-jac-upright (linearize-at-equilibrium pendulum-sys (vector 0.0 0.0) 1e-6))

(test "pendulum linearization at upright"
      #t
      ;; At θ=0: d²θ/dt² = -sin(θ) ≈ -θ, so J ≈ [[0, 1], [-1, 0]]
      (and (< (abs (matrix-ref pend-jac-upright 0 1)) 1.01)
           (> (abs (matrix-ref pend-jac-upright 0 1)) 0.99)))

;;; ====
;;; Fixed Point Refinement
;;; ====
(test-section "Fixed Point Refinement")

(define lv-approx (vector 4.9 10.1))  ; Close to actual equilibrium
(define lv-refined (refine-fixed-point lv lv-approx 1e-8 1e-6))

(test "fixed point refinement improves accuracy"
      #t
      (if lv-refined
          (< (vec-norm (vec-sub lv-refined lv-eq))
             (vec-norm (vec-sub lv-approx lv-eq)))
          #f))

;;; ====
;;; Classify Stability 2D
;;; ====
(test-section "Stability Classification 2D")

(define real-neg-evals (list (make-complex -1.0 0.0) (make-complex -2.0 0.0)))
(test "classify real negative eigenvalues"
      'stable-node
      (classify-stability-2d real-neg-evals))

(define real-pos-evals (list (make-complex 1.0 0.0) (make-complex 2.0 0.0)))
(test "classify real positive eigenvalues"
      'unstable-node
      (classify-stability-2d real-pos-evals))

(define mixed-evals (list (make-complex -1.0 0.0) (make-complex 1.0 0.0)))
(test "classify mixed sign eigenvalues"
      'saddle
      (classify-stability-2d mixed-evals))

(define complex-stable-evals (list (make-complex -0.5 2.0) (make-complex -0.5 -2.0)))
(test "classify complex stable eigenvalues"
      'stable-spiral
      (classify-stability-2d complex-stable-evals))

(define complex-unstable-evals (list (make-complex 0.5 2.0) (make-complex 0.5 -2.0)))
(test "classify complex unstable eigenvalues"
      'unstable-spiral
      (classify-stability-2d complex-unstable-evals))

(define imaginary-evals (list (make-complex 0.0 1.0) (make-complex 0.0 -1.0)))
(test "classify purely imaginary eigenvalues"
      'center
      (classify-stability-2d imaginary-evals))

;;; ====
;;; Higher-Dimensional Stability
;;; ====
(test-section "Higher-Dimensional Stability")

(define stable-evals-3d (list (make-complex -1.0 0.0)
                              (make-complex -2.0 0.0)
                              (make-complex -0.5 0.0)))

(test "3D system with all negative real parts is stable"
      'stable
      (classify-stability-nd stable-evals-3d))

(define unstable-evals-3d (list (make-complex 1.0 0.0)
                                (make-complex 0.5 0.0)
                                (make-complex 2.0 0.0)))

(test "3D system with all positive real parts is unstable"
      'unstable
      (classify-stability-nd unstable-evals-3d))

(define saddle-evals-3d (list (make-complex -1.0 0.0)
                              (make-complex 0.5 0.0)
                              (make-complex -0.3 0.0)))

(test "3D system with mixed signs is saddle-type"
      'saddle-type
      (classify-stability-nd saddle-evals-3d))

;;; ====
;;; Critical Regression Test: Magnitude vs Real Part (fold-zxrn)
;;; ====
(test-section "Regression: Eigenvalue Real Part vs Magnitude")

;;; This tests the fix for fold-zxrn:
;;; Power iteration finds eigenvalue with largest MAGNITUDE, but stability
;;; depends on eigenvalue with largest REAL PART.
;;;
;;; Example: eigenvalues {-10, 0.1, -0.5}
;;;   - Power iteration finds -10 (|λ|=10) → would classify as stable
;;;   - But max real part is 0.1 > 0 → system is UNSTABLE
;;;
;;; The fix uses QR algorithm to find ALL eigenvalues, then checks max real part.

;; Create a 3x3 matrix with known eigenvalues via diagonal form
;; A = diag(-10, 0.1, -0.5) → eigenvalues are exactly -10, 0.1, -0.5
(define misleading-eigs-A (list 'matrix 3 3 (vector -10.0 0 0
                                                     0 0.1 0
                                                     0 0 -0.5)))
(define misleading-sys (linear-ode misleading-eigs-A))

(define misleading-result
  (analyze-stability-nd misleading-sys (vector 0.0 0.0 0.0) 1e-6))

(test "3D system with eigenvalues {-10, 0.1, -0.5} is UNSTABLE (real part 0.1 > 0)"
      'saddle-type
      (car misleading-result))

;; Verify that a truly stable system is classified correctly
(define truly-stable-A (list 'matrix 3 3 (vector -10.0 0 0
                                                  0 -0.1 0
                                                  0 0 -0.5)))
(define truly-stable-sys (linear-ode truly-stable-A))

(define truly-stable-result
  (analyze-stability-nd truly-stable-sys (vector 0.0 0.0 0.0) 1e-6))

(test "3D system with eigenvalues {-10, -0.1, -0.5} is STABLE (all real parts < 0)"
      'stable
      (car truly-stable-result))

;; Test with a non-diagonal matrix that has complex eigenvalues
;; Using a rotation-like matrix with slight damping
(define complex-3d-A (list 'matrix 3 3 (vector -0.1  1.0  0.0
                                                -1.0 -0.1  0.0
                                                 0.0  0.0 -0.5)))
(define complex-3d-sys (linear-ode complex-3d-A))

(define complex-3d-result
  (analyze-stability-nd complex-3d-sys (vector 0.0 0.0 0.0) 1e-6))

(test "3D system with complex eigenvalues (damped rotation + decay) is stable"
      'stable
      (car complex-3d-result))

;;; ====
;;; Edge Cases
;;; ====
(test-section "Edge Cases")

(test "degenerate eigenvalues (both zero)"
      'degenerate
      (classify-stability-2d (list (make-complex 0.0 0.0)
                                   (make-complex 0.0 0.0))))

(test "one zero eigenvalue"
      'degenerate
      (classify-stability-2d (list (make-complex 0.0 0.0)
                                   (make-complex -1.0 0.0))))

;;; ====
;;; Robust Newton Solver
;;; ====
(test-section "Robust Newton Solver")

;; Test: find-fixed-point-robust works on regular systems
(define simple-sys (make-autonomous-ode
                    (lambda (x) (vec-scale -1.0 x))
                    2))

(define robust-result (find-fixed-point-robust
                       simple-sys
                       (vector 1.0 1.0)
                       1e-8
                       1e-6
                       20))

(test "robust Newton finds origin for -x system"
      #t
      (and robust-result
           (< (vec-norm robust-result) 1e-6)))

;; Test: find-fixed-point-robust handles near-singular Jacobian
;; Use a 2D system where SVD works reliably
;; Transcritical in 2D: dx/dt = rx - x^2, dy/dt = -y
(define (near-singular-2d-sys state)
  (let* ([x (vector-ref state 0)]
         [y (vector-ref state 1)]
         [r 0.01])  ; Close to bifurcation
    (vector (- (* r x) (* x x))   ; Transcritical in x
            (* -1.0 y))))         ; Stable in y

(define transcrit-2d-sys (make-autonomous-ode near-singular-2d-sys 2))

;; Fixed point at (r, 0) = (0.01, 0), Jacobian has -r on diagonal for x component
(define transcrit-robust (find-fixed-point-robust
                          transcrit-2d-sys
                          (vector 0.1 0.1)  ; Start from non-equilibrium
                          1e-6
                          1e-6
                          50))

(test "robust Newton finds fixed point in 2D near-singular system"
      #t
      (and transcrit-robust
           (< (abs (- (vector-ref transcrit-robust 0) 0.01)) 1e-4)
           (< (abs (vector-ref transcrit-robust 1)) 1e-6)))

;; Test: Levenberg-Marquardt solver
(define lm-result (find-fixed-point-levenberg-marquardt
                   simple-sys
                   (vector 1.0 1.0)
                   1e-8
                   1e-6
                   50))

(test "Levenberg-Marquardt finds origin for -x system"
      #t
      (and lm-result
           (< (vec-norm lm-result) 1e-6)))

;; Test: Levenberg-Marquardt near singularity (same 2D transcritical system)
(define lm-transcrit (find-fixed-point-levenberg-marquardt
                      transcrit-2d-sys
                      (vector 0.1 0.1)
                      1e-6
                      1e-6
                      100))

(test "Levenberg-Marquardt finds fixed point in 2D near-singular system"
      #t
      (and lm-transcrit
           (< (abs (- (vector-ref lm-transcrit 0) 0.01)) 1e-3)
           (< (abs (vector-ref lm-transcrit 1)) 1e-5)))

;; Test: Matrix utilities
(define test-matrix (list 'matrix 2 2 (vector 1.0 0.0 0.0 2.0)))
(define damped (matrix-add-diagonal test-matrix 0.1))

(test-approx "matrix-add-diagonal adds to (0,0)"
             1.1
             (matrix-ref damped 0 0)
             1e-10)

(test-approx "matrix-add-diagonal adds to (1,1)"
             2.1
             (matrix-ref damped 1 1)
             1e-10)

(test-approx "matrix-add-diagonal leaves off-diagonal unchanged"
             0.0
             (matrix-ref damped 0 1)
             1e-10)

;; Test: Linear system solver
(define a-simple (list 'matrix 2 2 (vector 2.0 1.0 1.0 3.0)))
(define b-simple (vector 5.0 7.0))
(define x-solve (solve-linear-system a-simple b-simple))

(test "solve-linear-system returns vector"
      #t
      (vector? x-solve))

;; Verify solution: Ax = b
(define ax-check (matrix-vec-mul a-simple x-solve))
(test-approx "solve-linear-system: Ax = b (component 0)"
             (vector-ref b-simple 0)
             (vector-ref ax-check 0)
             1e-8)

(test-approx "solve-linear-system: Ax = b (component 1)"
             (vector-ref b-simple 1)
             (vector-ref ax-check 1)
             1e-8)

(newline)
(display "All stability analysis tests completed!")
(newline)
