;;; lattice/physics/diff/manifest.sexp — Differentiable 2D Physics Skill Manifest

(skill physics/diff
  (version "0.1.0")
  (path "lattice/physics/diff")
  (purity partial)  ; Simulation may require fuel bounds
  (stability experimental)
  (fuel-bound "O(n² × steps) for n bodies")
  (deps (linalg autodiff geometry))

  (description
   "Differentiable 2D rigid body physics simulation. Supports automatic
    differentiation through physics rollouts for gradient-based optimization
    of trajectories, control policies, and physical parameters.")

  (keywords (physics simulation rigid-body differentiable 2d collision
             autodiff trajectory optimization control))
  (aliases (diff-physics diff-sim))

  (concepts
    (concept differentiable-physics
      (description "2D differentiable physics simulation where autodiff traces through rollouts, enabling gradient-based policy and trajectory optimization.")
      (parent physics-simulation)
      (synonyms differentiable-physics-2d diff-physics diff-sim physics/diff)))

  (exports
   (traced-vec2 traced-vec2))

  (modules
   (dynamics-ops "dynamics-ops.ss" "Dimension-agnostic rollout loop machinery (shared by 2D and 3D)")
   (traced-vec2 "traced-vec2.ss" "AD-enabled 2D vectors")
   (traced-body "traced-body.ss" "Differentiable rigid body state")
   (traced-integrators "traced-integrators.ss" "Symplectic integrators with AD")
   (diff-collision "diff-collision.ss" "Differentiable collision detection")
   (smooth-collision "smooth-collision.ss" "Smooth collision approximations")
   (diff-constraints "diff-constraints.ss" "Differentiable constraint solving")
   (rollout "rollout.ss" "Physics rollout with gradient tracking")
   (optimize "optimize.ss" "Gradient-based trajectory optimization")))
