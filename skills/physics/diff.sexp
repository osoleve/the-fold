;;; skills/physics/diff.sexp — Skill curation file

(skill physics/diff
  (description
    "Differentiable 2D rigid body physics simulation. Supports automatic\n    differentiation through physics rollouts for gradient-based optimization\n    of trajectories, control policies, and physical parameters.")

  (keywords (physics simulation rigid-body differentiable 2d collision autodiff trajectory optimization control))

  (aliases (diff-physics diff-sim))

  (concepts (differentiable-physics))

  (modules
    dynamics-ops
    traced-vec2
    traced-body
    traced-integrators
    diff-collision
    smooth-collision
    diff-constraints
    rollout
    optimize
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
