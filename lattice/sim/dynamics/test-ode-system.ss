;;; core/dynamics/test-ode-system.ss — ODE System Tests
;;;
;;; Tests for ODE system representation:
;;;   - System construction (autonomous/non-autonomous)
;;;   - Vector field evaluation
;;;   - Phase space utilities
;;;   - Standard systems (oscillators, Lorenz, etc.)
;;;   - Equilibrium detection
;;;   - System composition
;;;
;;; Run from project root: scheme --script core/dynamics/test-ode-system.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/sim/dynamics/ode-system.ss")

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
;;; ODE System Construction
;;; ============================================================
(test-section "ODE System Construction")

(define simple-autonomous
  (make-autonomous-ode
   (lambda (state) (vector (* 2 (vector-ref state 0))))
   1))

(test "autonomous ODE is recognized"
      #t
      (ode-system? simple-autonomous))

(test "autonomous ODE dimension"
      1
      (ode-dimension simple-autonomous))

(test "autonomous flag is set"
      #t
      (ode-autonomous? simple-autonomous))

(define simple-nonautonomous
  (make-nonautonomous-ode
   (lambda (t state)
           (vector (* t (vector-ref state 0))))
   1))

(test "non-autonomous ODE is recognized"
      #t
      (ode-system? simple-nonautonomous))

(test "non-autonomous flag is set"
      #f
      (ode-autonomous? simple-nonautonomous))

;;; ============================================================
;;; Vector Field Evaluation
;;; ============================================================
(test-section "Vector Field Evaluation")

(define growth-sys (exponential-growth 2.0))

(define growth-field
  (eval-vector-field growth-sys 0.0 5.0))

(test "exponential growth vector field"
      #t
      (= growth-field 10.0))

(define osc-sys (harmonic-oscillator 1.0))

(define osc-field
  (eval-vector-field osc-sys 0.0 (vector 3.0 4.0)))

(test "harmonic oscillator field dimension"
      2
      (vector-length osc-field))

(test "harmonic oscillator dx/dt = v"
      4.0
      (vector-ref osc-field 0))

(test "harmonic oscillator dv/dt = -ω²x"
      -3.0
      (vector-ref osc-field 1))

;;; Test forced oscillator (non-autonomous)
(define forced-osc (forced-oscillator 1.0 2.0 0.5))

(define forced-field-t0
  (eval-vector-field forced-osc 0.0 (vector 1.0 0.0)))

(test "forced oscillator at t=0"
      #t
      (and (= (vector-ref forced-field-t0 0) 0.0)
           (= (vector-ref forced-field-t0 1) (+ -1.0 (* 0.5 (cos 0.0))))))

;;; ============================================================
;;; Batch Vector Field Evaluation
;;; ============================================================
(test-section "Batch Vector Field Evaluation")

(define batch-states (list (vector 1.0 0.0) (vector 0.0 1.0) (vector 2.0 2.0)))
(define batch-fields (eval-vector-field-batch osc-sys 0.0 batch-states))

(test "batch evaluation returns correct number of results"
      3
      (length batch-fields))

(test "batch evaluation field 1"
      #t
      (and (= (vector-ref (car batch-fields) 0) 0.0)
           (= (vector-ref (car batch-fields) 1) -1.0)))

;;; ============================================================
;;; Standard Systems - Harmonic Oscillator
;;; ============================================================
(test-section "Harmonic Oscillator")

(define ho (harmonic-oscillator 2.0))

(test "harmonic oscillator dimension"
      2
      (ode-dimension ho))

(test "harmonic oscillator is autonomous"
      #t
      (ode-autonomous? ho))

(define ho-field (eval-vector-field ho 0.0 (vector 1.0 2.0)))

(test "harmonic oscillator state derivative"
      #t
      (and (= (vector-ref ho-field 0) 2.0)
           (= (vector-ref ho-field 1) -4.0)))

;;; ============================================================
;;; Standard Systems - Damped Oscillator
;;; ============================================================
(test-section "Damped Oscillator")

(define damped (damped-oscillator 1.0 0.5))

(test "damped oscillator dimension"
      2
      (ode-dimension damped))

(define damped-field (eval-vector-field damped 0.0 (vector 1.0 2.0)))

(test "damped oscillator includes damping term"
      #t
      (let ([dv-dt (vector-ref damped-field 1)])
           ;; dv/dt = -2ζωv - ω²x = -2*0.5*1*2 - 1*1 = -2 - 1 = -3
           (= dv-dt -3.0)))

;;; ============================================================
;;; Standard Systems - Lotka-Volterra
;;; ============================================================
(test-section "Lotka-Volterra Predator-Prey")

(define lv (lotka-volterra 1.0 0.1 0.1 0.02))

(test "Lotka-Volterra dimension"
      2
      (ode-dimension lv))

(test "Lotka-Volterra is autonomous"
      #t
      (ode-autonomous? lv))

(define lv-field (eval-vector-field lv 0.0 (vector 10.0 5.0)))

(test "Lotka-Volterra prey dynamics"
      #t
      ;; dx/dt = αx - βxy = 1*10 - 0.1*10*5 = 10 - 5 = 5
      (= (vector-ref lv-field 0) 5.0))

(test "Lotka-Volterra predator dynamics"
      #t
      ;; dy/dt = δxy - γy = 0.02*10*5 - 0.1*5 = 1 - 0.5 = 0.5
      (= (vector-ref lv-field 1) 0.5))

;;; ============================================================
;;; Standard Systems - Lorenz System
;;; ============================================================
(test-section "Lorenz Chaotic System")

(define lorenz (lorenz-system 10.0 28.0 (/ 8.0 3.0)))

(test "Lorenz system dimension"
      3
      (ode-dimension lorenz))

(define lorenz-field (eval-vector-field lorenz 0.0 (vector 1.0 1.0 1.0)))

(test "Lorenz dx/dt = σ(y - x)"
      0.0
      (vector-ref lorenz-field 0))

(test "Lorenz dy/dt computation"
      #t
      ;; dy/dt = x(ρ - z) - y = 1*(28 - 1) - 1 = 26
      (= (vector-ref lorenz-field 1) 26.0))

;;; ============================================================
;;; Standard Systems - Van der Pol
;;; ============================================================
(test-section "Van der Pol Oscillator")

(define vdp (van-der-pol 1.0))

(test "Van der Pol dimension"
      2
      (ode-dimension vdp))

(define vdp-field (eval-vector-field vdp 0.0 (vector 0.5 1.0)))

(test "Van der Pol nonlinear damping"
      #t
      ;; dv/dt = μ(1 - x²)v - x = 1*(1 - 0.25)*1 - 0.5 = 0.75 - 0.5 = 0.25
      (= (vector-ref vdp-field 1) 0.25))

;;; ============================================================
;;; Standard Systems - Pendulum
;;; ============================================================
(test-section "Simple Pendulum")

(define pend (pendulum 1.0))

(test "pendulum dimension"
      2
      (ode-dimension pend))

(define pend-field-upright (eval-vector-field pend 0.0 (vector 0.0 1.0)))

(test "pendulum at upright position"
      #t
      ;; At θ=0: dθ/dt = ω = 1.0, dω/dt = -sin(0) = 0
      (and (= (vector-ref pend-field-upright 0) 1.0)
           (= (vector-ref pend-field-upright 1) 0.0)))

(define pend-field-down (eval-vector-field pend 0.0 (vector 3.14159265 0.0)))

(test "pendulum near down position has small restoring force"
      #t
      ;; sin(π) ≈ 0 due to floating point, so restoring force is small
      (< (abs (vector-ref pend-field-down 1)) 0.01))

;;; ============================================================
;;; Linear ODE System
;;; ============================================================
(test-section "Linear ODE System")

(define A (list 'matrix 2 2 (vector 0 1 -1 0)))
(define lin-sys (linear-ode A))

(test "linear ODE dimension matches matrix"
      2
      (ode-dimension lin-sys))

(define lin-field (eval-vector-field lin-sys 0.0 (vector 1.0 2.0)))

(test "linear ODE applies matrix correctly"
      #t
      (and (= (vector-ref lin-field 0) 2.0)
           (= (vector-ref lin-field 1) -1.0)))

;;; ============================================================
;;; Vector Field Norm
;;; ============================================================
(test-section "Vector Field Norm")

(define norm-test-sys (harmonic-oscillator 1.0))

(define norm-at-origin (vector-field-norm norm-test-sys 0.0 (vector 0.0 0.0)))

(test "vector field norm at equilibrium"
      #t
      (< norm-at-origin 1e-10))

(define norm-nonzero (vector-field-norm norm-test-sys 0.0 (vector 3.0 4.0)))

(test "vector field norm away from equilibrium"
      #t
      (> norm-nonzero 4.0))

;;; ============================================================
;;; Equilibrium Detection
;;; ============================================================
(test-section "Equilibrium Detection")

(define eq-test-sys (harmonic-oscillator 1.0))

(test "origin is equilibrium for harmonic oscillator"
      #t
      (is-equilibrium? eq-test-sys (vector 0.0 0.0) 1e-6))

(test "non-zero state is not equilibrium"
      #f
      (is-equilibrium? eq-test-sys (vector 1.0 0.0) 1e-6))

;;; Test Lotka-Volterra equilibrium
(define lv-eq (lotka-volterra 1.0 0.1 0.1 0.02))

;;; Equilibrium: x = γ/δ = 0.1/0.02 = 5, y = α/β = 1.0/0.1 = 10
(test "Lotka-Volterra equilibrium point"
      #t
      (is-equilibrium? lv-eq (vector 5.0 10.0) 1e-6))

;;; ============================================================
;;; Phase Space Grid
;;; ============================================================
(test-section "Phase Space Grid")

(define grid-2x2 (make-phase-space-grid 0.0 1.0 2 0.0 1.0 2))

(test "2x2 grid has 4 points"
      4
      (length grid-2x2))

(test "grid points are vectors"
      #t
      (andmap vector? grid-2x2))

(define grid-3x3 (make-phase-space-grid -1.0 1.0 3 -1.0 1.0 3))

(test "3x3 grid has 9 points"
      9
      (length grid-3x3))

;;; ============================================================
;;; Vector Field Grid Computation
;;; ============================================================
(test-section "Vector Field Grid")

(define simple-sys (harmonic-oscillator 1.0))
(define test-grid (list (vector 1.0 0.0) (vector 0.0 1.0)))
(define grid-fields (compute-vector-field-grid simple-sys 0.0 test-grid))

(test "grid computation returns field for each point"
      2
      (length grid-fields))

(test "grid field values are vectors"
      #t
      (andmap vector? grid-fields))

;;; ============================================================
;;; Find Equilibria from Grid
;;; ============================================================
(test-section "Find Equilibria from Grid")

(define eq-search-grid
  (make-phase-space-grid -2.0 2.0 5 -2.0 2.0 5))

(define found-equilibria
  (find-equilibria-grid eq-test-sys eq-search-grid 0.1))

(test "finds equilibrium near origin"
      #t
      (> (length found-equilibria) 0))

;;; ============================================================
;;; System Coupling
;;; ============================================================
(test-section "System Coupling")

(define sys1 (exponential-growth 1.0))
(define sys2 (exponential-growth 2.0))
(define coupled (couple-ode-systems sys1 sys2))

(test "coupled system dimension is sum"
      2
      (ode-dimension coupled))

(test "coupled system is autonomous if both are"
      #t
      (ode-autonomous? coupled))

(define coupled-field
  (eval-vector-field coupled 0.0 (vector 3.0 4.0)))

(test "coupled system evaluates first component"
      3.0
      (vector-ref coupled-field 0))

(test "coupled system evaluates second component"
      8.0
      (vector-ref coupled-field 1))

;;; ============================================================
;;; Time Reversal
;;; ============================================================
(test-section "Time Reversal")

(define forward-sys (exponential-growth 2.0))
(define backward-sys (reverse-time-ode forward-sys))

(define forward-field (eval-vector-field forward-sys 0.0 5.0))
(define backward-field (eval-vector-field backward-sys 0.0 5.0))

(test "time-reversed system negates field"
      #t
      (= backward-field (- forward-field)))

(test "time-reversed system has same dimension"
      1
      (ode-dimension backward-sys))

;;; ============================================================
;;; Forced Oscillator (Non-Autonomous)
;;; ============================================================
(test-section "Forced Oscillator")

(define forced (forced-oscillator 1.0 2.0 1.0))

(test "forced oscillator is non-autonomous"
      #f
      (ode-autonomous? forced))

(test "forced oscillator dimension"
      2
      (ode-dimension forced))

(define forced-t0 (eval-vector-field forced 0.0 (vector 0.0 0.0)))
(define forced-t-pi-half (eval-vector-field forced (/ 3.14159265 2.0) (vector 0.0 0.0)))

(test "forcing term varies with time"
      #t
      ;; At t=0: forcing = A*cos(0) = 1.0
      ;; At t=π/2: forcing = A*cos(π) = -1.0 (since ω_drive=2.0)
      ;; Difference should be about 2.0
      (> (abs (- (vector-ref forced-t0 1)
                 (vector-ref forced-t-pi-half 1)))
         1.5))

;;; ============================================================
;;; Edge Cases
;;; ============================================================
(test-section "Edge Cases")

(test "scalar state works for 1D systems"
      #t
      (let ([scalar-sys (exponential-growth 3.0)])
           (number? (eval-vector-field scalar-sys 0.0 2.0))))

(test "zero-dimensional state handled"
      #t
      (let* ([zero-sys (make-autonomous-ode (lambda (s) s) 0)]
             [field (eval-vector-field zero-sys 0.0 (vector))])
            (vector? field)))

(newline)
(display "All ODE system tests completed!")
(newline)
