(skill interpolate
  (version "0.1.0")
  (path "lattice/interpolate")
  (purity total)
  (stability stable)
  (fuel-bound (max (quadratic n) (log-linear n)))
  (deps (numeric linalg))

  (description
   "Numerical interpolation and curve fitting. Linear, Lagrange, and Newton
    polynomial interpolation. Hermite and natural cubic spline interpolation.
    Bezier curves (linear, quadratic, cubic, arbitrary degree). Least squares
    polynomial fitting, linear regression. Chebyshev approximation and nodes.
    B-spline basis functions and curves.")

  (keywords (interpolation spline bezier hermite lagrange newton
             curve-fitting regression chebyshev b-spline
             approximation least-squares polyfit))
  (aliases (interp))

  (concepts
    (concept interpolation-approximation
      (description "Constructing smooth functions from discrete data: polynomial interpolation, splines, Bezier, Chebyshev, and B-splines.")
      (parent numerical-computing)
      (synonyms interp interpolation spline bezier hermite lagrange)))

  (exports
   (interpolate
    ;; Utilities
    binary-search-segment thomas-algorithm
    ;; Linear interpolation
    lerp lerp-inverse interp-linear
    ;; Polynomial interpolation
    lagrange-basis interp-lagrange
    divided-differences interp-newton
    ;; Hermite interpolation
    interp-hermite hermite-tangent-estimate
    ;; Cubic splines
    cubic-spline-natural spline-eval interp-cubic-spline
    ;; Bezier curves
    bezier-linear bezier-quadratic bezier-cubic
    bezier-general bezier-derivative
    ;; Least squares and fitting
    polyfit linreg linreg-r2
    ;; Chebyshev approximation
    chebyshev-nodes chebyshev-nodes-interval
    chebyshev-t chebyshev-coeffs chebyshev-eval
    ;; B-splines
    bspline-basis bspline-curve))

  (modules
   (interpolate "interpolate.ss"
    "Numerical interpolation and curve fitting. Linear, Lagrange, and Newton
     polynomial interpolation. Hermite and natural cubic spline interpolation.
     Bezier curves (linear, quadratic, cubic, arbitrary degree). Least squares
     polynomial fitting, linear regression. Chebyshev approximation and nodes.
     B-spline basis functions and curves.")))
