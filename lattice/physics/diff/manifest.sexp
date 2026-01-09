;;; lattice/physics/diff/manifest.sexp — Differentiable 2D Physics Skill Manifest

(skill physics/diff
  (version "0.1.0")
  (tier 2)
  (purity partial)  ; Simulation may require fuel bounds
  (fuel-bound "O(n² × steps) for n bodies")
  (deps (linalg autodiff geometry))

  (description
   "Differentiable 2D rigid body physics simulation. Supports automatic
    differentiation through physics rollouts for gradient-based optimization
    of trajectories, control policies, and physical parameters.")

  (exports
   (traced-body traced-vec2 traced-integrators)
   (diff-collision smooth-collision diff-constraints)
   (rollout optimize))

  (modules
   (traced-vec2 "traced-vec2.ss" "AD-enabled 2D vectors")
   (traced-body "traced-body.ss" "Differentiable rigid body state")
   (traced-integrators "traced-integrators.ss" "Symplectic integrators with AD")
   (diff-collision "diff-collision.ss" "Differentiable collision detection")
   (smooth-collision "smooth-collision.ss" "Smooth collision approximations")
   (diff-constraints "diff-constraints.ss" "Differentiable constraint solving")
   (rollout "rollout.ss" "Physics rollout with gradient tracking")
   (optimize "optimize.ss" "Gradient-based trajectory optimization")))
