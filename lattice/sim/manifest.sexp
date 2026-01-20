;;; lattice/sim/manifest.sexp — Simulation Streams Skill Manifest

(skill sim
  (version "0.1.0")
  (tier 2)
  (path "lattice/sim")
  (purity total)  ; Pure stream-based simulation
  (stability experimental)
  (fuel-bound "O(n) per simulation step")
  (deps (data linalg random numeric fp))

  (description
   "Stream-based simulation framework for continuous dynamical systems.
    Provides lazy simulation streams, physics state management, numerical
    integrators, and tools for sampling, querying, and composing simulations.
    Supports n-body systems and modular force functions.")

  (keywords (simulation streams dynamics physics lazy
             integrators n-body continuous time))
  (aliases (simulation simulation-stream))

  (exports
   (simulation-stream sim-unfold simulate sim-scan))

  (modules
   (simulation-stream "simulation-stream.ss" "Core simulation stream abstraction and utilities")))
