;;; skills/pde.sexp — Skill curation file

(skill pde
  (description
    "Numerical methods for partial differential equations. Finite Element Method\n    (FEM) for elliptic PDEs on triangular meshes with P1 elements. Time stepping\n    schemes for parabolic and hyperbolic PDEs (Forward/Backward Euler,\n    Crank-Nicolson, Method of Lines with RK4). Finite difference methods for\n    regular grids. Spectral methods via Chebyshev collocation. CFL stability\n    analysis and adaptive time stepping.")

  (keywords (pde partial-differential-equations finite-element-method fem finite-difference spectral-methods chebyshev-collocation time-stepping forward-euler backward-euler crank-nicolson method-of-lines rk4 cfl-condition parabolic hyperbolic poisson laplace triangular-mesh p1-elements sparse-solver conjugate-gradient adaptive))

  (aliases (fem finite-element))

  (concepts (pde-methods))

  (modules
    fem
    pde-time
    finite-diff
    spectral-pde
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
