;;; lattice/physics/diff3d/manifest.sexp — Differentiable 3D Physics Skill Manifest

(skill physics/diff3d
  (version "0.1.0")
  (tier 2)
  (path "lattice/physics/diff3d")
  (purity partial)  ; Simulation may require fuel bounds
  (stability experimental)
  (fuel-bound "O(n^2 x steps) for n bodies")
  (deps (linalg autodiff geometry))

  (description
   "Differentiable 3D rigid body physics simulation with quaternion rotations.
    Supports automatic differentiation through physics rollouts for gradient-based
    optimization of trajectories, control policies, and physical parameters.
    Includes soft contact models, constraints, and checkpointed gradient computation.")

  (keywords (physics simulation rigid-body differentiable 3d quaternion
             autodiff trajectory optimization collision constraints))
  (aliases (diff-physics-3d diff-sim-3d))

  (exports
   (traced-vec3 traced-vec3))

  (modules
   (traced-vec3 "traced-vec3.ss" "AD-enabled 3D vectors with smooth operations")
   (traced-quaternion "traced-quaternion.ss" "AD-enabled quaternions for rotation")
   (traced-body3d "traced-body3d.ss" "Differentiable 3D rigid body state with inertia tensors")
   (traced-integrators3d "traced-integrators3d.ss" "Symplectic integrators with AD for 3D bodies")
   (smooth-collision3d "smooth-collision3d.ss" "Smooth SDF-based collision with soft contact")
   (diff-collision3d "diff-collision3d.ss" "Differentiable 3D collision detection and response")
   (diff-constraints3d "diff-constraints3d.ss" "Differentiable 3D constraint solving")
   (rollout3d "rollout3d.ss" "Physics rollout with gradient tracking and checkpointing")
   (optimize3d "optimize3d.ss" "Gradient-based trajectory and parameter optimization")))
