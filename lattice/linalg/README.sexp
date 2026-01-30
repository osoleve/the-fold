((name "linalg")
(purpose "Linear algebra operations")
(description "Comprehensive linear algebra library including vectors,\nmatrices, decompositions (LU, QR, Cholesky), eigenvalues, sparse matrices,\nand linear equation solvers. Dense matrices use row-major flat vectors for\ncache efficiency. Sparse matrices support COO, CSR, and CSC formats.")
(vector-system
 (overview "Three-level vector abstraction:\n    1. vec.ss - Generic n-dimensional vectors using Scheme vectors\n    2. vec2.ss - Concrete 2D vectors for physics/graphics (tagged lists)\n    3. vec3.ss - Concrete 3D vectors for physics/graphics (tagged lists)\n\n    vec2 and vec3 share common infrastructure via vec-common.ss macros,\n    eliminating code duplication while maintaining full performance.")
 (consolidation
  (approach "Macro-based code generation")
  (rationale "Vec2 and vec3 share identical operations (add, sub, dot,\n     magnitude, normalize, lerp, project, etc.) but differ in dimension.\n     Macros generate these operations, eliminating source duplication\n     while producing identical runtime code (zero overhead).")
  (results
   (vec2
    (before "405 lines")
    (after "182 lines")
    (reduction "55%"))
   (vec3
    (before "505 lines")
    (after "123 lines")
    (reduction "76%")))))
(modules 
  ((vec-common.ss "Macro infrastructure for generating vector operations")
  (vec.ss "Generic n-dimensional vector operations - 56 tests")
  (vec2.ss "2D graphics/physics vectors - 52 tests")
  (vec3.ss "3D graphics/physics vectors - 41 tests")
  (quaternion.ss "Quaternion rotations (uses vec3) - 34 tests")
  (matrix.ss "Matrix operations - 55 tests")
  (matrix-decomp.ss "LU, QR, Cholesky decompositions - 40 tests")
  (matrix-solvers.ss "Linear equation solvers, Levinson-Durbin - 48 tests")
  (matrix-eigen.ss "Eigenvalue/eigenvector computation - 34 tests")
  (sparse.ss "Sparse matrix formats (COO, CSR, CSC) - 127 tests")
  (iterative-solvers.ss "CG, GMRES iterative methods")
  (iteration.ss "Iteration macros for vectors/matrices")
  (graph-laplacian.ss "Graph Laplacian, spectral clustering, k-means - 51 tests")
  (dep-linalg.ss "Dependent-typed linear algebra")))
(dependencies (base))
(graph-laplacian
 (overview "Spectral graph theory tools for graph analysis and clustering.")
 (laplacian-types 
   ((unnormalized "L = D - A, eigenvalues in [0, 2*d_max]")
   (normalized "L_sym = I - D^(-1/2) A D^(-1/2), eigenvalues in [0, 2]")
   (random-walk "L_rw = I - D^(-1) A, related to Markov chains")))
 (spectral-clustering "Partition graphs into communities using eigenvector embedding.\n    - spectral-clustering: Normalized (Ng-Jordan-Weiss algorithm)\n    - spectral-clustering-unnorm: Unnormalized variant\n    - spectral-partition-k: Recursive bisection for k partitions")
 (clustering-algorithms 
   ((kmeans-cluster "K-means with k-means++ initialization")
   (label-propagation "In graph-community.ss for comparison")))
 (quality-metrics 
   ((conductance "cut(S,S̄) / min(vol(S), vol(S̄)) - lower is better")
   (ratio-cut "cut/|S| + cut/|S̄| - balanced partition objective")
   (normalized-cut "cut/vol(S) + cut/vol(S̄) - spectral clustering objective")
   (modularity "Q score in graph-community.ss")))
 (spectral-properties 
   ((algebraic-connectivity "λ₂ - measures graph connectivity")
   (fiedler-vector "Eigenvector of λ₂ for graph partitioning")
   (effective-resistance "Network distance between nodes"))))
(usage-examples 
  ((solving-linear-systems "Solve Ax = b using LU decomposition" "(load \"lattice/linalg/matrix.ss\")\n     (load \"lattice/linalg/matrix-solvers.ss\")\n\n     (define A (list->matrix '((4 3) (6 3)) 2 2))\n     (define b (make-vec '(10 12)))\n     (solve-linear-system A b)  ; => #(1 2)")
  (eigenvalue-computation "Find eigenvalues and eigenvectors" "(load \"lattice/linalg/matrix-eigen.ss\")\n\n     (define M (list->matrix '((2 1) (1 2)) 2 2))\n     (matrix-eigenvalues M)     ; => (3.0 1.0)\n     (matrix-eigenvectors M)    ; => matrix of eigenvectors")
  (svd-decomposition "Singular value decomposition for data analysis" "(load \"lattice/linalg/svd.ss\")\n\n     (define A (list->matrix '((1 2) (3 4) (5 6)) 3 2))\n     (svd A)  ; => (U S Vt) where A = U * diag(S) * Vt")
  (sparse-matrices "Efficient sparse matrix operations" "(load \"lattice/linalg/sparse.ss\")\n\n     (define edges '((0 1 1.0) (1 2 2.0) (2 0 3.0)))\n     (define sp (dense->sparse-csr (edges->adjacency-matrix edges 3)))\n     (sparse-csr-mv sp v)  ; matrix-vector multiply")
  (quaternion-rotations "3D rotations using quaternions" "(load \"lattice/linalg/quaternion.ss\")\n\n     (define q (quat-from-axis-angle (make-vec3 0 1 0) (/ pi 2)))\n     (quat-rotate q (make-vec3 1 0 0))  ; rotate 90° around Y axis")
  (spectral-clustering "Community detection using spectral methods" "(load \"lattice/data/graph-matrix.ss\")\n     (load \"lattice/linalg/graph-laplacian.ss\")\n\n     ;; Build adjacency matrix for social network\n     (define adj (edges->adjacency-matrix edges n #t))\n\n     ;; Spectral clustering into k communities\n     (define labels (spectral-clustering adj k))\n\n     ;; Evaluate partition quality\n     (define cluster-0 (filter (lambda (i) (= (vector-ref labels i) 0))\n                               (iota n)))\n     (conductance adj cluster-0)     ; lower is better\n     (normalized-cut adj cluster-0)  ; spectral objective")))
(toeplitz-solvers
 (overview "Specialized solvers for Toeplitz systems (constant diagonals).")
 (levinson-durbin "Solve Yule-Walker equations for AR model fitting.\n    Input: r = [r_0, r_1, ..., r_p] autocorrelations\n    Output: phi = [phi_1, ..., phi_p] AR coefficients\n    Complexity: O(p²) vs O(p³) for general LU decomposition.")
 (levinson-durbin-general "Solve Tx = b for any symmetric positive definite Toeplitz matrix T.\n    Input: r = [r_0, ..., r_{n-1}] defining T where T[i,j] = r_{|i-j|}\n           b = right-hand side vector\n    Output: solution vector x"))
(performance-notes "Dense matrices use row-major flat vectors for cache efficiency.\n   Matrix operations are O(n³) for most decompositions.\n   Toeplitz systems use Levinson-Durbin at O(n²).\n   Use sparse formats (CSR/CSC) for matrices with >90% zeros.\n   Iterative solvers (CG, GMRES) are preferred for large sparse systems.\n   Eigenvalue computation uses QR iteration with Householder transforms.")
(representation
 (vec "Scheme vector: #(element ...)")
 (vec2 "Tagged list: (vec2 x y)")
 (vec3 "Tagged list: (vec3 x y z)")
 (dense-matrix "(matrix rows cols data) with flat row-major data")
 (sparse-coo "(sparse-coo rows cols row-idx col-idx values)")
 (sparse-csr "(sparse-csr rows cols row-ptrs col-idx values)")
 (sparse-csc "(sparse-csc rows cols col-ptrs row-idx values)"))
(total-tests 790))
