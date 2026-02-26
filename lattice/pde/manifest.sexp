(skill pde
  (version "0.1.0")
  (path "lattice/pde")
  (purity total)
  (stability stable)
  (fuel-bound (max (quadratic n) (cubic n)))
  (deps (numeric linalg geometry))

  (description
   "Numerical methods for partial differential equations. Finite Element Method
    (FEM) for elliptic PDEs on triangular meshes with P1 elements. Time stepping
    schemes for parabolic and hyperbolic PDEs (Forward/Backward Euler,
    Crank-Nicolson, Method of Lines with RK4). Finite difference methods for
    regular grids. Spectral methods via Chebyshev collocation. CFL stability
    analysis and adaptive time stepping.")

  (keywords (pde partial-differential-equations finite-element-method fem
             finite-difference spectral-methods chebyshev-collocation
             time-stepping forward-euler backward-euler crank-nicolson
             method-of-lines rk4 cfl-condition parabolic hyperbolic
             poisson laplace triangular-mesh p1-elements
             sparse-solver conjugate-gradient adaptive))
  (aliases (fem finite-element))

  (concepts
    (concept pde-methods
      (description "Numerical methods for partial differential equations: FEM on triangular meshes, finite differences, spectral methods, and time stepping.")
      (parent numerical-computing)
      (synonyms fem pde-time finite-element-method poisson-equation elliptic-pde pde-time-stepping forward-euler backward-euler crank-nicolson method-of-lines rk4)))

  (exports
   (fem
    ;; Mesh construction
    make-fem-mesh fem-mesh? fem-mesh-nodes fem-mesh-elements
    fem-mesh-num-nodes fem-mesh-num-elements fem-mesh-node fem-mesh-element
    fem-mesh-element-nodes make-unit-square-mesh make-disk-mesh
    ;; Element computations
    element-area basis-gradients element-stiffness element-mass element-load
    ;; Assembly
    assemble-stiffness assemble-mass assemble-load
    ;; Boundary conditions
    find-boundary-nodes apply-dirichlet-penalty!
    ;; Solver
    sparse-cg fem-solve-poisson fem-solve-poisson-full
    ;; Post-processing
    fem-solution-at fem-render-solution fem-l2-error)

   (pde-time
    ;; Vector operations
    pde-vec-add pde-vec-sub pde-vec-scale pde-vec-madd pde-vec-dot pde-vec-norm
    ;; Forward Euler (explicit)
    forward-euler-step forward-euler-matrix-step forward-euler-mass-step
    ;; Backward Euler (implicit)
    backward-euler-linear-step backward-euler-identity-step
    ;; Crank-Nicolson (implicit, 2nd order)
    crank-nicolson-step crank-nicolson-identity-step
    ;; Method of Lines
    mol-rhs mol-euler-step mol-rk4-step
    ;; Stability analysis
    cfl-parabolic cfl-hyperbolic estimate-mesh-spacing
    ;; Adaptive time stepping
    adaptive-euler-step integrate-adaptive
    ;; Sparse utilities
    sparse-csr-identity sparse-cg-solve sparse-csr-diagonal-vec
    ;; Factory and driver
    make-time-stepper integrate-pde)

   (finite-diff
    diff-forward diff-backward diff-central diff2-central
    apply-stencil-1d stencil-d1-forward stencil-d1-backward stencil-d1-central stencil-d2-central
    laplacian-2d apply-dirichlet-1d apply-neumann-1d apply-periodic-1d
    laplacian-matrix-1d laplacian-matrix-2d
    heat-step-explicit-1d solve-heat-1d
    wave-step-explicit-1d solve-wave-1d)

   (spectral-pde
    fourier-wavenumbers fourier-diff fourier-diff2 fourier-laplacian
    fourier-heat-rhs fourier-advection-rhs
    fourier-heat-exact-step fourier-advection-exact-step fourier-integrate
    spectral-chebyshev-nodes spectral-chebyshev-nodes-interval chebyshev-poly
    chebyshev-diff-matrix chebyshev-diff-matrix-interval chebyshev-diff2-matrix
    chebyshev-diff chebyshev-diff2 chebyshev-poisson-1d))

  (modules
   (fem "fem.ss"
    "Finite Element Method for solving elliptic PDEs on 2D triangular meshes.
     P1 (linear) elements with sparse matrix assembly and conjugate gradient solver.
     Solves Poisson equation with Dirichlet boundary conditions.")
   (pde-time "pde-time.ss"
    "Time stepping schemes for time-dependent PDEs. Forward Euler, Backward Euler,
     Crank-Nicolson. Method of Lines with RK4. CFL stability conditions.
     Adaptive time stepping with Richardson extrapolation.")
   (finite-diff "finite-diff.ss"
    "Finite difference methods for PDEs on regular grids. Gradient, Laplacian,
     divergence, curl operators. Poisson solver and diffusion stepping.")
   (spectral-pde "spectral-pde.ss"
    "Spectral methods for PDEs via Chebyshev collocation. Differentiation matrices,
     BVP solvers, eigenvalue problems, and spectral time stepping.")))
