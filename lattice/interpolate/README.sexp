((name "interpolate")
(purpose "Numerical interpolation, curve fitting, and approximation")
(description "Linear, Lagrange, Newton polynomial, Hermite, and natural cubic spline\ninterpolation. Bezier curves (linear through arbitrary degree via de Casteljau).\nLeast squares polynomial fitting and linear regression. Chebyshev approximation\nnodes, polynomials, and Clenshaw evaluation. B-spline basis functions and curves.")
(modules
  ((interpolate.ss "All interpolation, fitting, and approximation methods - 75 tests")))
(dependencies (numeric linalg))
(notes "Split from numeric skill for clearer tier separation"
       "Uses linalg for matrix operations in least squares fitting"
       "Chebyshev nodes are optimal for polynomial interpolation (minimizes Runge phenomenon)"))
