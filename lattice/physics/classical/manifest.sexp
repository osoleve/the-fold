;;; lattice/physics/classical/manifest.sexp — Classical 2D Physics Skill Manifest

(skill physics/classical
  (version "0.1.0")
  (tier 2)
  (path "lattice/physics/classical")
  (purity partial)  ; World simulation uses mutation
  (stability experimental)
  (fuel-bound "O(n^2 x iterations) for n entities")
  (deps (linalg geometry random))

  (description
   "Classical 2D rigid body physics simulation with collision detection,
    constraint solving, and particle systems. Provides a complete physics
    world with spatial hashing, materials, and ASCII rendering.")

  (keywords (physics simulation rigid-body 2d collision particles
             constraints world spatial-hash ascii-render graph-coloring
             constraint-islands scc parallel-solving))
  (aliases (physics-2d classical-physics))

  (exports
   ;; No exports annotated with (doc 'export #t) yet
   )

  (modules
   (ode-integrators "ode-integrators.ss" "Generic ODE integrators: Euler, Midpoint, RK4, Symplectic Verlet, Leapfrog")
   (integrators "integrators.ss" "2D physics integrators with vec2 support (wraps ode-integrators)")
   (rigid-body "rigid-body.ss" "2D rigid body with rotation and inertia")
   (collision-detection "collision-detection.ss" "AABB, circle, polygon collision with spatial hashing")
   (collision-response "collision-response.ss" "Impulse-based collision resolution with friction")
   (constraints "constraints.ss" "Constraint data structures for joints")
   (constraint-solver "constraint-solver.ss" "Iterative constraint solver with Baumgarte stabilization")
   (constraint-graph "constraint-graph.ss" "Graph analysis: islands, SCCs, coloring for parallel solving")
   (world "world.ss" "Physics world: entities, simulation stepping, queries")
   (particles "particles.ss" "Particle systems with emitters and force fields")
   (raycasting "raycasting.ss" "2D raycasting against shapes")
   (ascii-renderer "ascii-renderer.ss" "ASCII-art physics visualization")))
