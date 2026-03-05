;;; skills/linalg.sexp — Skill curation file

(skill linalg
  (description
    "Pure functional linear algebra library with vectors, matrices,\n    decompositions, solvers, and quaternions.")

  (keywords (linear-algebra matrix vector quaternion decomposition lu qr cholesky eigenvalue svd sparse solver numerical toeplitz levinson-durbin spectral-clustering))

  (aliases (la lin-alg linear matrix-math))

  (concepts (linear-algebra matrix-theory vector-spaces eigenvalue-theory sparse-methods spectral-methods))

  (modules
    vec
    vec2
    vec3
    quaternion
    matrix
    matrix-decomp
    matrix-solvers
    matrix-eigen
    iterative-solvers
    sparse
    graph-laplacian
    svd
    iteration
    numeric-instances
    dep-linalg
    vec-common
    integer-matrix
    matrix-blocked
    matrix-blocked-decomp
    matrix-optics
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
