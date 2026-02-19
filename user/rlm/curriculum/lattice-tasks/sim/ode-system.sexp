;;; ode-system.sexp — Development tasks for lattice/sim/dynamics/ode-system.ss
;;;
;;; Module: ode-system (tier 0, depends on prelude, vec, matrix)
;;; 300 lines, ~20 exported functions
;;; Pure ODE system representation: autonomous and non-autonomous systems,
;;; vector field evaluation, standard named systems (harmonic oscillator,
;;; Lotka-Volterra, Lorenz, Van der Pol, pendulum), equilibrium detection,
;;; system coupling, and time reversal.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   e68b9849 feat(module): migrate sim to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement eval-vector-field
  ;; ──────────────────────────────────────────────
  ;; Evaluate an ODE system's derivative at a point.

  (task
    (id "sim/ode-system/eval-vector-field-impl")
    (module ode-system)
    (tier 0)
    (type implement)
    (description "Implement eval-vector-field: evaluate the vector field (derivative function) of an ODE system at a given time and state. An ODE system stores its vector field as a function of two arguments (t, state). Extract the function with ode-vector-field, then call it with t and state. This is the fundamental evaluation primitive — every solver, phase portrait computation, and equilibrium test calls this. It's a one-line function: extract and apply.")
    (context "Available: ode-vector-field. Function extraction and application.")
    (before "(define (eval-vector-field sys t state)
  ;; TODO: evaluate the vector field at (t, state)
  0)")
    (test-file "lattice/sim/dynamics/test-ode-system.ss")
    (test-forms (";; Exponential growth: dx/dt = 2x at x=5 gives 10
(let ([sys (exponential-growth 2.0)])
  (assert-equal 10.0 (eval-vector-field sys 0.0 5.0)))"
                 ";; Harmonic oscillator at (3, 4): dx/dt=v=4, dv/dt=-x=-3
(let* ([sys (harmonic-oscillator 1.0)]
       [field (eval-vector-field sys 0.0 (vector 3.0 4.0))])
  (assert-equal 4.0 (vector-ref field 0))
  (assert-equal -3.0 (vector-ref field 1)))"
                 ";; Works with non-autonomous systems (time matters)
(let* ([sys (forced-oscillator 1.0 2.0 0.5)]
       [f0 (eval-vector-field sys 0.0 (vector 0.0 0.0))]
       [f1 (eval-vector-field sys 1.0 (vector 0.0 0.0))])
  ;; Different times -> different forcing -> different results
  (assert-false (= (vector-ref f0 1) (vector-ref f1 1))))"))
    (available-modules (prelude vec matrix))
    (hints ("Extract: (ode-vector-field sys) returns the function f(t, state)"
            "Apply: call it with t and state"
            "One-line: ((ode-vector-field sys) t state)"))
    (reference-solution "(define (eval-vector-field sys t state)
  ((ode-vector-field sys) t state))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement harmonic-oscillator
  ;; ──────────────────────────────────────────────
  ;; Create a standard harmonic oscillator ODE.

  (task
    (id "sim/ode-system/harmonic-oscillator-impl")
    (module ode-system)
    (tier 0)
    (type implement)
    (description "Implement harmonic-oscillator: create an autonomous ODE system for the simple harmonic oscillator d^2x/dt^2 = -omega^2*x. Rewrite as a first-order system with state [x, v]: dx/dt = v, dv/dt = -omega^2*x. The vector field lambda takes a state vector and returns (vector v (- (* omega omega x))). Use make-autonomous-ode with dimension 2. Extract x and v from the state with vector-ref at indices 0 and 1. The negative sign in dv/dt creates the oscillation — it's a restoring force proportional to displacement.")
    (context "Available: make-autonomous-ode, vector, vector-ref, -, *, lambda. Vector field construction from physics.")
    (before "(define (harmonic-oscillator omega)
  ;; TODO: SHO as first-order system [x, v]
  (make-autonomous-ode (lambda (state) state) 2))")
    (test-file "lattice/sim/dynamics/test-ode-system.ss")
    (test-forms (";; Dimension is 2 (position + velocity)
(assert-equal 2 (ode-dimension (harmonic-oscillator 1.0)))"
                 ";; Is autonomous
(assert-true (ode-autonomous? (harmonic-oscillator 1.0)))"
                 ";; dx/dt = v (velocity becomes position derivative)
(let* ([ho (harmonic-oscillator 1.0)]
       [field (eval-vector-field ho 0.0 (vector 3.0 4.0))])
  (assert-equal 4.0 (vector-ref field 0)))"
                 ";; dv/dt = -omega^2 * x (restoring force)
(let* ([ho (harmonic-oscillator 2.0)]
       [field (eval-vector-field ho 0.0 (vector 1.0 2.0))])
  ;; dv/dt = -4*1 = -4
  (assert-equal -4.0 (vector-ref field 1)))"))
    (available-modules (prelude vec matrix))
    (hints ("State: (vector-ref state 0) = x, (vector-ref state 1) = v"
            "dx/dt = v"
            "dv/dt = (- (* omega omega x)) — note the negation"
            "Return: (vector v (- (* omega omega x)))"
            "Wrap in make-autonomous-ode with dimension 2"))
    (reference-solution "(define (harmonic-oscillator omega)
  (make-autonomous-ode
   (lambda (state)
     (let ([x (vector-ref state 0)]
           [v (vector-ref state 1)])
       (vector v (- (* omega omega x)))))
   2))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement is-equilibrium?
  ;; ──────────────────────────────────────────────
  ;; Check if a point is a fixed point of the ODE.

  (task
    (id "sim/ode-system/is-equilibrium-impl")
    (module ode-system)
    (tier 0)
    (type implement)
    (description "Implement is-equilibrium?: check if a state is an equilibrium point (fixed point) of an ODE system within a given tolerance. At an equilibrium, the vector field is zero: f(x) = 0, so the system doesn't evolve. Compute the norm of the vector field at the given state using vector-field-norm (which handles both scalar and vector states), then check if it's below the tolerance. Use time t=0 for evaluation (equilibria of autonomous systems are time-independent). This is used by find-equilibria-grid to search for fixed points over a grid.")
    (context "Available: vector-field-norm, <. Norm computation + comparison.")
    (before "(define (is-equilibrium? sys state tolerance)
  ;; TODO: is ||f(x)|| < tolerance?
  #f)")
    (test-file "lattice/sim/dynamics/test-ode-system.ss")
    (test-forms (";; Origin is equilibrium for harmonic oscillator
(assert-true (is-equilibrium? (harmonic-oscillator 1.0) (vector 0.0 0.0) 1e-6))"
                 ";; Non-zero state is not equilibrium
(assert-false (is-equilibrium? (harmonic-oscillator 1.0) (vector 1.0 0.0) 1e-6))"
                 ";; Lotka-Volterra equilibrium: x=gamma/delta, y=alpha/beta
(let ([lv (lotka-volterra 1.0 0.1 0.1 0.02)])
  (assert-true (is-equilibrium? lv (vector 5.0 10.0) 1e-6)))"
                 ";; Origin is equilibrium for Lotka-Volterra (trivial extinction)
(let ([lv (lotka-volterra 1.0 0.1 0.1 0.02)])
  (assert-true (is-equilibrium? lv (vector 0.0 0.0) 1e-6)))"))
    (available-modules (prelude vec matrix))
    (hints ("Compute field norm: (vector-field-norm sys 0 state)"
            "Compare: (< field-norm tolerance)"
            "This is a two-line function: let + comparison"))
    (reference-solution "(define (is-equilibrium? sys state tolerance)
  (let ([field-norm (vector-field-norm sys 0 state)])
    (< field-norm tolerance)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement couple-ode-systems
  ;; ──────────────────────────────────────────────
  ;; Combine two ODE systems into a higher-dimensional one.

  (task
    (id "sim/ode-system/couple-ode-systems-impl")
    (module ode-system)
    (tier 0)
    (type implement)
    (description "Implement couple-ode-systems: combine two ODE systems into a single higher-dimensional system by concatenating their state vectors. If sys1 has dimension d1 and sys2 has dimension d2, the coupled system has dimension d1+d2 with state [x1, x2]. The vector field splits the concatenated state into two parts using vec-take and vec-drop, evaluates each original vector field separately, then concatenates the derivatives with vec-append. The coupled system is autonomous only if both subsystems are autonomous. This is uncoupled coupling — the subsystems don't interact. Use for modeling independent parallel processes.")
    (context "Available: ode-dimension, ode-vector-field, ode-autonomous?, make-ode-system, vec-take, vec-drop, vec-append, and, +, let*, lambda. State splitting + derivative concatenation.")
    (before "(define (couple-ode-systems sys1 sys2)
  ;; TODO: concatenate two ODE systems
  sys1)")
    (test-file "lattice/sim/dynamics/test-ode-system.ss")
    (test-forms (";; Dimensions add
(let* ([s1 (exponential-growth 1.0)]
       [s2 (exponential-growth 2.0)]
       [coupled (couple-ode-systems s1 s2)])
  (assert-equal 2 (ode-dimension coupled)))"
                 ";; Autonomous if both are
(let* ([s1 (exponential-growth 1.0)]
       [s2 (exponential-growth 2.0)]
       [coupled (couple-ode-systems s1 s2)])
  (assert-true (ode-autonomous? coupled)))"
                 ";; Evaluates each component independently
(let* ([s1 (exponential-growth 1.0)]
       [s2 (exponential-growth 2.0)]
       [coupled (couple-ode-systems s1 s2)]
       [field (eval-vector-field coupled 0.0 (vector 3.0 4.0))])
  ;; First component: 1.0 * 3.0 = 3.0
  (assert-equal 3.0 (vector-ref field 0))
  ;; Second component: 2.0 * 4.0 = 8.0
  (assert-equal 8.0 (vector-ref field 1)))"
                 ";; Coupling 2D + 3D gives 5D
(let* ([ho (harmonic-oscillator 1.0)]
       [lorenz (lorenz-system 10.0 28.0 (/ 8.0 3.0))]
       [coupled (couple-ode-systems ho lorenz)])
  (assert-equal 5 (ode-dimension coupled)))"))
    (available-modules (prelude vec matrix))
    (hints ("Extract dimensions: (ode-dimension sys1), (ode-dimension sys2)"
            "Extract vector fields: (ode-vector-field sys1), (ode-vector-field sys2)"
            "Autonomy: (and (ode-autonomous? sys1) (ode-autonomous? sys2))"
            "Split state: (vec-take state dim1) and (vec-drop state dim1)"
            "Evaluate: (f1 t state1) and (f2 t state2)"
            "Concatenate: (vec-append deriv1 deriv2)"
            "Wrap in make-ode-system with (+ dim1 dim2) and auto?"))
    (reference-solution "(define (couple-ode-systems sys1 sys2)
  (let ([dim1 (ode-dimension sys1)]
        [dim2 (ode-dimension sys2)]
        [f1 (ode-vector-field sys1)]
        [f2 (ode-vector-field sys2)]
        [auto? (and (ode-autonomous? sys1) (ode-autonomous? sys2))])
    (make-ode-system
     (lambda (t state)
       (let* ([state1 (vec-take state dim1)]
              [state2 (vec-drop state dim1)]
              [deriv1 (f1 t state1)]
              [deriv2 (f2 t state2)])
         (vec-append deriv1 deriv2)))
     (+ dim1 dim2)
     auto?)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement reverse-time-ode
  ;; ──────────────────────────────────────────────
  ;; Negate the vector field to reverse time.

  (task
    (id "sim/ode-system/reverse-time-ode-impl")
    (module ode-system)
    (tier 0)
    (type implement)
    (description "Implement reverse-time-ode: create the time-reversed version of an ODE system by negating its vector field. If the original system is dx/dt = f(t, x), the reversed system is dx/dt = -f(t, x). Evaluate the original vector field, then negate the result: if it's a vector, use vec-negate; if it's a scalar, use (- field). Preserves the dimension and autonomy flag from the original system. Time reversal is useful for backward integration (tracing trajectories to their origin) and analyzing time-reversibility of physical systems.")
    (context "Available: ode-vector-field, ode-dimension, ode-autonomous?, make-ode-system, vec-negate, -, vector?, if, lambda, let. Evaluate + negate pattern.")
    (before "(define (reverse-time-ode sys)
  ;; TODO: negate vector field for time reversal
  sys)")
    (test-file "lattice/sim/dynamics/test-ode-system.ss")
    (test-forms (";; Reversed field is negative of forward
(let* ([sys (exponential-growth 2.0)]
       [rev (reverse-time-ode sys)])
  (assert-equal -10.0 (eval-vector-field rev 0.0 5.0)))"
                 ";; Same dimension
(let* ([sys (harmonic-oscillator 1.0)]
       [rev (reverse-time-ode sys)])
  (assert-equal 2 (ode-dimension rev)))"
                 ";; Vector field properly negated for 2D system
(let* ([sys (harmonic-oscillator 1.0)]
       [rev (reverse-time-ode sys)]
       [fwd (eval-vector-field sys 0.0 (vector 1.0 2.0))]
       [bwd (eval-vector-field rev 0.0 (vector 1.0 2.0))])
  (assert-equal (- (vector-ref fwd 0)) (vector-ref bwd 0))
  (assert-equal (- (vector-ref fwd 1)) (vector-ref bwd 1)))"
                 ";; Double reversal gives back original
(let* ([sys (exponential-growth 3.0)]
       [rev2 (reverse-time-ode (reverse-time-ode sys))])
  (assert-equal 6.0 (eval-vector-field rev2 0.0 2.0)))"))
    (available-modules (prelude vec matrix))
    (hints ("Extract: (ode-vector-field sys), (ode-dimension sys), (ode-autonomous? sys)"
            "New vector field: evaluate original then negate"
            "(let ([field (f t state)]) ...)"
            "Negate: (if (vector? field) (vec-negate field) (- field))"
            "Wrap in make-ode-system with same dim and auto?"))
    (reference-solution "(define (reverse-time-ode sys)
  (let ([f (ode-vector-field sys)]
        [dim (ode-dimension sys)]
        [auto? (ode-autonomous? sys)])
    (make-ode-system
     (lambda (t state)
       (let ([field (f t state)])
         (if (vector? field)
             (vec-negate field)
             (- field))))
     dim
     auto?)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
