((name "control-systems")
 (description "Control theory and state space models for dynamical systems.
  Implements linear time-invariant (LTI) systems, system composition,
  and discrete-time simulation.")

 (modules
  ((file "state-space.ss")
   (purpose "State space representation of dynamical systems")
   (exports
    (make-state-space "Create a state space model from A, B, C, D matrices")
    (ss-A "Extract state transition matrix")
    (ss-B "Extract input matrix")
    (ss-C "Extract output matrix")
    (ss-D "Extract feedthrough matrix")
    (ss-step "Compute one time step: x' = Ax + Bu, y = Cx + Du")
    (ss-simulate "Simulate system over a sequence of inputs")
    (ss-series "Connect two systems in series (cascade)")
    (ss-parallel "Connect two systems in parallel")
    (ss-feedback "Create a feedback loop around a system")
    (ss-poles "Compute system poles (eigenvalues of A)")
    (ss-is-stable? "Check if all poles have magnitude < 1 (discrete)")
    (ss-controllability-matrix "Compute controllability matrix")
    (ss-observability-matrix "Compute observability matrix")
    (ss-is-controllable? "Check system controllability")
    (ss-is-observable? "Check system observability"))
   (example
    ";; First-order lag: x' = 0.9x + 0.1u, y = x
     (define lag (make-state-space
                   #(#(0.9))    ; A
                   #(#(0.1))    ; B
                   #(#(1.0))    ; C
                   #(#(0.0)))) ; D
     (ss-simulate lag #(1 1 1 1 1) #(0))
     ;; => sequence of outputs approaching 1")))

 (dependencies
  ("lattice/linalg/matrix.ss" "Matrix operations")
  ("lattice/linalg/matrix-decomp.ss" "Eigenvalue computation")
  ("lattice/linalg/vec.ss" "Vector operations"))

 (notes
  "State space models are the foundation of modern control theory.
   The representation x' = Ax + Bu, y = Cx + Du captures linear
   time-invariant systems in a form amenable to analysis and design.

   For discrete-time systems, stability requires all eigenvalues of A
   to lie within the unit circle.

   Test suite: 21 tests covering simulation, composition, and stability."))
