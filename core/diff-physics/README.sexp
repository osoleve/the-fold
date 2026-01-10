;;; core/diff-physics/README.sexp --- Differentiable Physics Module
;;;
;;; Module documentation in S-expression format.

(module diff-physics
  (description "Differentiable 2D physics simulation with automatic differentiation support")

  (purpose
    "Enable gradient-based optimization through physics simulations for:"
    "- Trajectory optimization (find controls to reach goals)"
    "- Inverse physics (infer parameters from observations)"
    "- Sensitivity analysis (understand parameter effects)"
    "- Neural physics integration (learn dynamics)")

  (files
    (traced-vec2.ss "Differentiable 2D vector operations")
    (traced-body.ss "Differentiable rigid body state")
    (traced-integrators.ss "Differentiable Euler/Verlet integration")
    (smooth-collision.ss "SDF-based soft contact model")
    (diff-collision.ss "Collision response with gradients")
    (diff-constraints.ss "Spring/joint constraints with gradients")
    (rollout.ss "Multi-step simulation with checkpointing")
    (optimize.ss "Trajectory optimization API")
    (test-diff-physics.ss "Test suite"))

  (key-design-decisions
    (soft-contact "Uses SDFs and softplus for smooth, differentiable contacts")
    (truncated-unrolling "Constraint solver uses 4-iteration unrolling for gradients")
    (checkpointing "O(sqrt(T)) memory for long rollouts"))

  (dependencies
    (core/base/prelude.ss "Scheme prelude")
    (core/linalg/vec2.ss "2D vector math")
    (core/autodiff/reverse-diff.ss "Reverse-mode automatic differentiation")
    (user/physics/rigid-body.ss "Rigid body data structure"))

  (usage-example
    ";;; Trajectory optimization: find velocity to hit target"
    "(define result (optimize-initial-velocity"
    "  (vec2 0 0)      ; start position"
    "  (vec2 10 5)     ; target position"
    "  (vec2 0 -9.8)   ; gravity"
    "  (/ 1 60)        ; dt"
    "  100             ; steps"
    "  0.1             ; learning rate"
    "  100))           ; iterations")

  (author "Shepherd (Opus)")
  (tier "core")
  (test-command "scheme --script core/diff-physics/test-diff-physics.ss"))
