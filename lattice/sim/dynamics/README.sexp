((name "dynamics")
 (purpose "Dynamical systems analysis, simulation, and bifurcation theory")
 (description
  "Pure implementation of continuous and discrete dynamical systems.
   Supports ODE systems, discrete maps, stability analysis via
   linearization, chaos detection (Lyapunov exponents, Poincare sections),
   and comprehensive bifurcation analysis (saddle-node, transcritical,
   pitchfork, Hopf, period-doubling). Includes parameter continuation,
   bifurcation diagrams, and normal form systems.")
 (modules
  ((discrete.ss "Discrete dynamical systems: iterated maps, orbits, cobweb diagrams")
   (ode-system.ss "ODE systems: vector fields, flow functions, phase space grids")
   (stability.ss "Stability analysis: Jacobians, eigenvalues, classification")
   (chaos.ss "Chaos detection: Lyapunov exponents, Poincare sections, strange attractors")
   (bifurcation.ss "Bifurcation analysis: continuation, detection, diagrams, normal forms")
   (attractor-render.ss "ASCII visualization of strange attractors")))
 (dependencies (base linalg numeric complex))
 (load-order "ode-system.ss -> stability.ss -> chaos.ss -> bifurcation.ss"))
