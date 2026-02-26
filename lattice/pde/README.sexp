((name "pde")
(purpose "Numerical methods for partial differential equations")
(description "Finite Element Method (FEM) for elliptic PDEs on triangular meshes\nwith P1 elements and conjugate gradient solver. Time stepping schemes for\nparabolic and hyperbolic PDEs (Forward/Backward Euler, Crank-Nicolson,\nMethod of Lines with RK4). Finite difference methods on regular grids.\nSpectral methods via Chebyshev and Fourier collocation.")
(modules
  ((fem.ss "FEM for 2D elliptic PDEs on triangular meshes")
  (pde-time.ss "Time stepping: Euler, Crank-Nicolson, Method of Lines, RK4")
  (finite-diff.ss "Finite difference operators, heat/wave equation solvers")
  (spectral-pde.ss "Spectral methods via Chebyshev/Fourier collocation")))
(dependencies (numeric linalg geometry))
(notes "Tier 2 skill - composes linalg + geometry + numeric foundation"
       "FEM requires mesh-gen from geometry, sparse/iterative-solvers from linalg"
       "Split from numeric skill - these are composed methods, not primitives"))
