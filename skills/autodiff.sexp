;;; skills/autodiff.sexp — Skill curation file

(skill autodiff
  (description
    "Reverse-mode automatic differentiation (backpropagation) using\n    computational graphs. Supports gradient computation, Jacobian/Hessian\n    matrices, higher-order derivatives, and a differentiable type class\n    for generic AD code. Includes sparse autodiff for large systems,\n    interval autodiff for verified optimization with monotonicity pruning,\n    and traced-optics for computing gradients through optic-focused paths.")

  (keywords (autodiff automatic-differentiation gradient backpropagation reverse-mode forward-mode jacobian hessian interval-gradient computational-graph tape dual-numbers hyperdual monotonicity))

  (aliases (ad diff reverse-diff backprop))

  (concepts (automatic-differentiation))

  (modules
    higher-order-diff
    differentiable
    differentiable-signal
    sparse-autodiff
    typed-gradients
    profiling
    symbolic-diff
    interval-autodiff
    traced-optics
    optic-functors
    variational-inference
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
