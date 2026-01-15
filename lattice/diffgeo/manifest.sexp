(skill diffgeo
  (version "0.1.0")
  (tier 1)
  (path "lattice/diffgeo")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n²) for Jacobian computation, O(k) for atlas operations with k charts")
  (deps (linalg))

  (description
   "Differential geometry foundations for smooth manifolds. Provides coordinate
    charts, atlases, transition functions, and Jacobian computation. Includes
    standard coordinate systems (polar, spherical, cylindrical) and tools for
    manifold representation.")

  (keywords (differential-geometry manifold chart atlas coordinates
             transition-function jacobian polar spherical cylindrical))
  (aliases (diffgeom manifolds smooth-manifolds))

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
    make-spherical-chart make-cylindrical-chart))

  (modules
   (charts "charts.ss"
    "Coordinate charts, atlases, transition functions, and Jacobian computation.
     Foundation for smooth manifold representation.")))
