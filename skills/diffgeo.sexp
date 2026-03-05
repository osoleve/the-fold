;;; skills/diffgeo.sexp — Skill curation file

(skill diffgeo
  (description
    "Differential geometry foundations for smooth manifolds. Provides coordinate\n    charts, atlases, transition functions, Jacobian computation, tangent and\n    cotangent spaces, pushforward and pullback operations, and tangent/cotangent\n    bundles. Includes standard coordinate systems (polar, spherical, cylindrical),\n    Lie groups (SO(2), SO(3), SE(2), SE(3)) with exponential/logarithm maps,\n    adjoint representations, and Baker-Campbell-Hausdorff approximations.\n    Also provides Riemannian metric tensors, Christoffel symbols, Riemann curvature\n    tensor, Ricci tensor, scalar curvature, and surface curvatures (Gaussian, mean,\n    principal) for 2D surfaces embedded in R³. Symbolic metric derivatives eliminate\n    numerical differentiation error for standard coordinate systems. Geodesic computation\n    includes numerical geodesic tracing, exponential/logarithm maps, parallel transport,\n    and geodesic distance. Differential forms support includes k-forms, exterior\n    algebra with wedge product, interior product, exterior derivative, Hodge star\n    operator (Euclidean), and integration over parameterized curves and surfaces.")

  (keywords (differential-geometry manifold chart atlas coordinates transition-function jacobian polar spherical cylindrical tangent-vector cotangent-vector tangent-space cotangent-space tangent-bundle cotangent-bundle pushforward pullback differential lie-group lie-algebra so2 so3 se2 se3 rotation rigid-transformation exponential-map logarithm-map rodrigues adjoint bch interpolation slerp curvature metric riemannian christoffel riemann-tensor ricci-tensor scalar-curvature gaussian-curvature mean-curvature principal-curvature surface fundamental-form geodesic parallel-transport geodesic-distance differential-forms k-form exterior-algebra wedge-product interior-product exterior-derivative hodge-star volume-form integration de-rham symbolic-differentiation exact-derivatives))

  (aliases (diffgeom manifolds smooth-manifolds lie-groups riemannian-geometry geodesics exterior-calculus cartan-calculus))

  (concepts (differential-geometry manifold-theory riemannian-geometry lie-groups exterior-calculus geodesics))

  (modules
    charts
    tangent
    lie-groups
    curvature
    geodesics
    forms
    symbolic-metrics
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
