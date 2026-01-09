;;; lattice/physics/classical/manifest.sexp — Classical 2D Physics Skill Manifest

(skill physics/classical
  (version "0.1.0")
  (tier 2)
  (path "lattice/physics/classical")
  (purity partial)  ; World simulation uses mutation
  (stability experimental)
  (fuel-bound "O(n^2 x iterations) for n entities")
  (deps (linalg geometry))

  (description
   "Classical 2D rigid body physics simulation with collision detection,
    constraint solving, and particle systems. Provides a complete physics
    world with spatial hashing, materials, and ASCII rendering.")

  (keywords (physics simulation rigid-body 2d collision particles
             constraints world spatial-hash ascii-render))
  (aliases (physics-2d classical-physics))

  (exports
   (integrators body-2d time-acc forces energy)
   (rigid-body angular-motion inertia)
   (collision-detection shapes spatial-hash manifolds)
   (collision-response materials impulse resolution)
   (constraints distance revolute spring weld)
   (constraint-solver baumgarte position-correction)
   (world entities simulation queries)
   (particles emitters force-fields systems)
   (raycasting ray-shape intersections)
   (ascii-renderer render-config debug-options))

  (modules
   (integrators "integrators.ss" "Numerical integrators: Euler, Symplectic, Verlet, RK4")
   (rigid-body "rigid-body.ss" "2D rigid body with rotation and inertia")
   (collision-detection "collision-detection.ss" "AABB, circle, polygon collision with spatial hashing")
   (collision-response "collision-response.ss" "Impulse-based collision resolution with friction")
   (constraints "constraints.ss" "Constraint data structures for joints")
   (constraint-solver "constraint-solver.ss" "Iterative constraint solver with Baumgarte stabilization")
   (world "world.ss" "Physics world: entities, simulation stepping, queries")
   (particles "particles.ss" "Particle systems with emitters and force fields")
   (raycasting "raycasting.ss" "2D raycasting against shapes")
   (ascii-renderer "ascii-renderer.ss" "ASCII-art physics visualization")))
