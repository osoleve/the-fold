((name "dynamics")
 (purpose "Dynamical systems analysis and simulation")
 (description
  "Pure implementation of continuous and discrete dynamical systems.
   Supports ODE systems, discrete maps, stability analysis via
   linearization, and standard systems (Lorenz, Lotka-Volterra,
   Van der Pol, etc.). Includes fixed point detection, eigenvalue
   classification, and phase portrait utilities.")
 (modules
  ((discrete.ss "Discrete dynamical systems: iterated maps, orbits, bifurcations")
   (ode-system.ss "ODE systems: vector fields, flow functions, phase space grids")
   (stability.ss "Stability analysis: Jacobians, eigenvalues, classification")))
 (dependencies (base linalg numeric))
 (load-order "discrete.ss, ode-system.ss -> stability.ss"))
