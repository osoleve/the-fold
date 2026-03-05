;;; skills/physics/diff3d.sexp — Skill curation file

(skill physics/diff3d
  (description
    "Differentiable 3D rigid body physics simulation with quaternion rotations.\n    Supports automatic differentiation through physics rollouts for gradient-based\n    optimization of trajectories, control policies, and physical parameters.\n    Includes soft contact models, constraints, and checkpointed gradient computation.")

  (keywords (physics simulation rigid-body differentiable 3d quaternion autodiff trajectory optimization collision constraints))

  (aliases (diff-physics-3d diff-sim-3d))

  (concepts (differentiable-physics-3d))

  (modules
    traced-vec3
    traced-quaternion
    traced-body3d
    traced-integrators3d
    smooth-collision3d
    diff-collision3d
    diff-constraints3d
    rollout3d
    optimize3d
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
