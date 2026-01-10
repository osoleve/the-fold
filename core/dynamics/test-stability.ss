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

(load "core/base/prelude.ss")
(load "core/linalg/vec.ss")
(load "core/linalg/matrix.ss")
(load "core/linalg/matrix-eigen.ss")
(load "core/numeric/complex.ss")
(load "core/dynamics/ode-system.ss")
(load "core/dynamics/stability.ss")

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

;;; ============================================================
;;; Fixed Point Detection
;;; ============================================================
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

;;; ============================================================
;;; Jacobian Computation
;;; ============================================================
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

;;; ============================================================
;;; Stability Classification - Linear Systems
;;; ============================================================
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

;;; ============================================================
;;; Stability Classification - Nonlinear Systems
;;; ============================================================
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

;;; ============================================================
;;; Stability Predicates
;;; ============================================================
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

;;; ============================================================
;;; Matrix Inversion
;;; ============================================================
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

;;; ============================================================
;;; Linearization
;;; ============================================================
(test-section "Linearization")

(define pendulum-sys (pendulum 1.0))
(define pend-jac-upright (linearize-at-equilibrium pendulum-sys (vector 0.0 0.0) 1e-6))

(test "pendulum linearization at upright"
      #t
      ;; At θ=0: d²θ/dt² = -sin(θ) ≈ -θ, so J ≈ [[0, 1], [-1, 0]]
      (and (< (abs (matrix-ref pend-jac-upright 0 1)) 1.01)
           (> (abs (matrix-ref pend-jac-upright 0 1)) 0.99)))

;;; ============================================================
;;; Fixed Point Refinement
;;; ============================================================
(test-section "Fixed Point Refinement")

(define lv-approx (vector 4.9 10.1))  ; Close to actual equilibrium
(define lv-refined (refine-fixed-point lv lv-approx 1e-8 1e-6))

(test "fixed point refinement improves accuracy"
      #t
      (if lv-refined
          (< (vec-norm (vec-sub lv-refined lv-eq))
             (vec-norm (vec-sub lv-approx lv-eq)))
          #f))

;;; ============================================================
;;; Classify Stability 2D
;;; ============================================================
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

;;; ============================================================
;;; Higher-Dimensional Stability
;;; ============================================================
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

;;; ============================================================
;;; Edge Cases
;;; ============================================================
(test-section "Edge Cases")

(test "degenerate eigenvalues (both zero)"
      'degenerate
      (classify-stability-2d (list (make-complex 0.0 0.0)
                                   (make-complex 0.0 0.0))))

(test "one zero eigenvalue"
      'degenerate
      (classify-stability-2d (list (make-complex 0.0 0.0)
                                   (make-complex -1.0 0.0))))

(newline)
(display "All stability analysis tests completed!")
(newline)
