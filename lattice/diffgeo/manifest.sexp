(skill diffgeo
  (version "0.7.0")
  (tier 1)
  (path "lattice/diffgeo")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n⁴) for Riemann tensor, O(n²) for Christoffel symbols, O(k·n²) for geodesic tracing, O(C(n,k)²) for wedge product")
  (deps (linalg))

  (description
   "Differential geometry foundations for smooth manifolds. Provides coordinate
    charts, atlases, transition functions, Jacobian computation, tangent and
    cotangent spaces, pushforward and pullback operations, and tangent/cotangent
    bundles. Includes standard coordinate systems (polar, spherical, cylindrical),
    Lie groups (SO(2), SO(3), SE(2), SE(3)) with exponential/logarithm maps,
    adjoint representations, and Baker-Campbell-Hausdorff approximations.
    Also provides Riemannian metric tensors, Christoffel symbols, Riemann curvature
    tensor, Ricci tensor, scalar curvature, and surface curvatures (Gaussian, mean,
    principal) for 2D surfaces embedded in R³. Symbolic metric derivatives eliminate
    numerical differentiation error for standard coordinate systems. Geodesic computation
    includes numerical geodesic tracing, exponential/logarithm maps, parallel transport,
    and geodesic distance. Differential forms support includes k-forms, exterior
    algebra with wedge product, interior product, exterior derivative, Hodge star
    operator (Euclidean), and integration over parameterized curves and surfaces.")

  (keywords (differential-geometry manifold chart atlas coordinates
             transition-function jacobian polar spherical cylindrical
             tangent-vector cotangent-vector tangent-space cotangent-space
             tangent-bundle cotangent-bundle pushforward pullback differential
             lie-group lie-algebra so2 so3 se2 se3 rotation rigid-transformation
             exponential-map logarithm-map rodrigues adjoint bch interpolation slerp
             curvature metric riemannian christoffel riemann-tensor ricci-tensor
             scalar-curvature gaussian-curvature mean-curvature principal-curvature
             surface fundamental-form geodesic parallel-transport geodesic-distance
             differential-forms k-form exterior-algebra wedge-product interior-product
             exterior-derivative hodge-star volume-form integration de-rham
             symbolic-differentiation exact-derivatives))
  (aliases (diffgeom manifolds smooth-manifolds lie-groups riemannian-geometry geodesics
            exterior-calculus cartan-calculus))

  (concepts
    (concept differential-geometry
      (description "The mathematics of smooth manifolds, coordinate charts, tangent spaces, curvature, and geodesics.")
      (parent mathematics)
      (synonyms diffgeo diffgeom manifolds smooth-manifolds))
    (concept manifold-theory
      (description "Coordinate charts, atlases, transition functions, and the foundational apparatus of smooth manifolds.")
      (parent differential-geometry))
    (concept riemannian-geometry
      (description "Metric tensors, Christoffel symbols, Riemann curvature tensor, Ricci tensor, and surface curvatures.")
      (parent differential-geometry)
      (synonyms riemannian-metric christoffel-symbols riemann-tensor ricci-tensor scalar-curvature))
    (concept lie-groups
      (description "Smooth groups SO(2), SO(3), SE(2), SE(3) with exponential/log maps, adjoints, and BCH approximations.")
      (parent differential-geometry)
      (synonyms lie-group so2 so3 se2 se3 exponential-map))
    (concept exterior-calculus
      (description "Differential forms, wedge product, exterior derivative, Hodge star, and integration of forms over manifolds.")
      (parent differential-geometry)
      (synonyms differential-forms wedge-product exterior-derivative hodge-star cartan-calculus))
    (concept geodesics
      (description "Shortest paths on Riemannian manifolds: numerical tracing, exponential maps, parallel transport, geodesic distance.")
      (parent differential-geometry)))

  (exports
   (charts
    chart? chart-name chart-dim chart-domain-pred chart-coord-map chart-inverse-map
    make-chart chart-contains? chart-apply chart-apply-inverse
    make-transition transition-apply
    jacobian-numerical transition-jacobian jacobian-determinant
    atlas? atlas-name atlas-charts atlas-dim atlas-chart-count
    make-atlas atlas-add-chart atlas-find-chart atlas-find-chart-by-name atlas-coords
    charts-overlap? transition-smooth?
    make-identity-chart make-polar-chart make-cartesian-chart
    make-spherical-chart make-cylindrical-chart)

   (tangent
    ;; Tangent vectors
    tangent-vector? tangent-vector-point tangent-vector-chart tangent-vector-components
    tangent-vector-dim make-tangent-vector tangent-zero tangent-basis-vector
    tangent-add tangent-sub tangent-scale tangent-negate tangent-change-chart
    ;; Cotangent vectors
    cotangent-vector? cotangent-vector-point cotangent-vector-chart cotangent-vector-components
    cotangent-vector-dim make-cotangent-vector cotangent-zero cotangent-basis-form
    cotangent-add cotangent-sub cotangent-scale cotangent-negate cotangent-change-chart
    ;; Pairing
    covector-apply
    ;; Pushforward/pullback
    pushforward pullback-at
    ;; Tangent space
    tangent-space? tangent-space-point tangent-space-chart tangent-space-dim
    make-tangent-space tangent-space-basis tangent-space-vector
    ;; Cotangent space
    cotangent-space? cotangent-space-point cotangent-space-chart cotangent-space-dim
    make-cotangent-space cotangent-space-basis cotangent-space-form
    ;; Tangent bundle
    tangent-bundle? tangent-bundle-atlas tangent-bundle-dim
    make-tangent-bundle tangent-bundle-fiber tangent-bundle-section
    ;; Cotangent bundle
    cotangent-bundle? cotangent-bundle-atlas cotangent-bundle-dim
    make-cotangent-bundle cotangent-bundle-fiber cotangent-bundle-section
    ;; Differential
    differential
    ;; Lie bracket
    lie-bracket lie-bracket-field)

   (lie-groups
    ;; Constants/utilities
    identity-matrix matrix-set! matrix-trace skew-symmetric?
    ;; SO(2) - 2D rotations
    so2? so2-matrix make-so2 so2-identity so2-angle
    so2-compose so2-inverse so2-act
    so2-alg? so2-alg-omega make-so2-alg so2-alg-to-matrix so2-matrix-to-alg
    so2-exp so2-log
    ;; SO(3) - 3D rotations
    so3? so3-matrix make-so3-from-matrix make-so3 so3-identity
    so3-compose so3-inverse so3-act
    so3-alg? so3-alg-vec make-so3-alg so3-hat so3-vee
    so3-alg-to-matrix so3-matrix-to-alg so3-exp so3-log so3-axis-angle
    ;; SE(2) - 2D rigid transformations
    se2? se2-rotation se2-translation make-se2 make-se2-from-angle
    se2-identity se2-to-matrix matrix-to-se2
    se2-compose se2-inverse se2-act
    se2-alg? se2-alg-omega se2-alg-v make-se2-alg se2-alg-to-matrix
    se2-exp se2-log
    ;; SE(3) - 3D rigid transformations
    se3? se3-rotation se3-translation make-se3 se3-identity
    se3-to-matrix matrix-to-se3 se3-compose se3-inverse se3-act
    se3-alg? se3-alg-omega se3-alg-v make-se3-alg se3-alg-to-matrix
    se3-exp se3-log
    ;; Adjoint representation
    so3-adjoint se3-adjoint vec-cross
    ;; Baker-Campbell-Hausdorff
    so3-bracket so3-bch-2 so3-bch-4
    ;; Interpolation
    so3-interpolate se3-interpolate)

   (curvature
    ;; Metric tensors
    metric? metric-chart metric-fn metric-dim make-metric
    metric-at metric-at-point metric-inverse
    metric-inner-product metric-norm metric-determinant
    make-euclidean-metric make-polar-metric make-spherical-metric make-cylindrical-metric
    ;; Christoffel symbols
    christoffel-symbols christoffel-ref
    ;; Curvature tensors
    riemann-tensor riemann-ref ricci-tensor scalar-curvature sectional-curvature
    ;; Surfaces
    surface? surface-param make-surface surface-at
    first-fundamental-form second-fundamental-form
    gaussian-curvature mean-curvature principal-curvatures
    surface-classify classify-point-curvature
    surface-normal surface-partial-u surface-partial-v
    ;; Standard surfaces
    make-sphere-surface make-torus-surface make-paraboloid-surface make-saddle-surface)

   (symbolic-metrics
    ;; Symbolic metric construction
    metric-symbolic? make-metric-symbolic metric-symbolic-deriv-fn
    metric-chart* metric-fn* metric-dim* metric-at*
    ;; Exact metric constructors
    make-polar-metric-exact make-spherical-metric-exact
    make-cylindrical-metric-exact make-euclidean-metric-exact
    ;; Symbolic Christoffel computation
    christoffel-symbols-exact christoffel-symbols*
    ;; Pre-computed exact Christoffels
    polar-christoffel-exact spherical-christoffel-exact)

   (geodesics
    ;; Geodesic state
    make-geodesic-state geodesic-state? geodesic-state-coords geodesic-state-velocity
    ;; Geodesic ODE
    geodesic-acceleration geodesic-derivative
    ;; Numerical integration
    rk4-geodesic-step trace-geodesic trace-geodesic-final
    ;; Exponential map
    exp-map exp-map-t
    ;; Logarithm map
    log-map
    ;; Parallel transport
    parallel-transport parallel-transport-along-geodesic
    ;; Distance
    geodesic-distance geodesic-length
    ;; Interpolation
    geodesic-interpolate
    ;; Utilities
    geodesic-spray
    ;; Caching
    clear-christoffel-cache! reset-christoffel-cache-stats! christoffel-cache-stats
    cached-christoffel-symbols)

   (forms
    ;; Multi-index utilities
    multi-index? multi-index-degree generate-multi-indices
    multi-index->offset offset->multi-index binomial
    ;; Permutation sign
    permutation-sign shuffle-sign
    ;; k-form representation
    k-form? k-form-point k-form-chart k-form-degree k-form-components k-form-dim
    make-k-form k-form-zero k-form-basis
    ;; Conversions
    cotangent->1-form 1-form->cotangent
    ;; Arithmetic
    k-form-add k-form-sub k-form-scale k-form-negate
    ;; Chart change
    k-form-change-chart
    ;; Wedge product
    wedge wedge*
    ;; Interior product
    interior-product
    ;; Exterior derivative
    exterior-derivative d-scalar
    ;; Evaluation
    k-form-apply
    ;; Hodge star
    hodge-star-euclidean volume-form
    ;; Pullback
    pullback-k-form
    ;; Integration
    integrate-1-form-line integrate-2-form-surface))

  (modules
   (charts "charts.ss"
    "Coordinate charts, atlases, transition functions, and Jacobian computation.
     Foundation for smooth manifold representation.")
   (tangent "tangent.ss"
    "Tangent and cotangent spaces, vectors, pushforward/pullback operations,
     and bundle constructions. Foundation for vector fields and differential forms.")
   (lie-groups "lie-groups.ss"
    "Lie groups (SO(2), SO(3), SE(2), SE(3)) and their Lie algebras. Provides
     exponential/logarithm maps via Rodrigues formula, adjoint representations,
     Baker-Campbell-Hausdorff approximations, and SLERP interpolation.")
   (curvature "curvature.ss"
    "Riemannian curvature computations. Metric tensors, Christoffel symbols,
     Riemann curvature tensor, Ricci tensor, scalar curvature, and surface
     curvatures (Gaussian, mean, principal) with standard surfaces (sphere,
     torus, paraboloid, saddle).")
   (geodesics "geodesics.ss"
    "Geodesic computation on Riemannian manifolds. Numerical geodesic tracing
     via RK4, exponential map (shoot geodesic with initial velocity), logarithm
     map (inverse via shooting method), parallel transport along geodesics,
     geodesic distance, and geodesic interpolation. Includes Christoffel symbol
     caching for improved performance during repeated evaluations.")
   (forms "forms.ss"
    "Differential forms and exterior calculus. k-forms as alternating multilinear
     maps, exterior algebra with wedge product (antisymmetric tensor product),
     interior product (contraction with vectors), exterior derivative (generalized
     gradient), Hodge star operator (for Euclidean metrics), and numerical
     integration of forms over parameterized curves and surfaces. Supports the
     fundamental identity d(dω) = 0 and Stokes' theorem applications.")
   (symbolic-metrics "symbolic-metrics.ss"
    "Exact symbolic metric derivatives for standard coordinate systems (polar,
     spherical, cylindrical, Euclidean). Eliminates numerical differentiation error
     in Christoffel symbol computation. Provides christoffel-symbols* which auto-detects
     symbolic vs numerical metrics. Includes pre-computed exact Christoffel symbols
     for polar and spherical coordinates.")))
