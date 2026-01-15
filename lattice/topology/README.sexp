((name "topology")
 (purpose "Computational topology and topological data analysis")
 (description "Simplicial complexes, homology groups, and Betti numbers.
Provides foundational data structures for algebraic topology computations
using Z_2 coefficients. Supports topological data analysis (TDA).")
 (modules
  ((simplicial-complex.ss "Core simplicial complex data structures:
     - Simplex: make-simplex, simplex-dim, simplex-vertices, simplex-face?
     - Convenience: vertex, edge, triangle, tetrahedron
     - Face enumeration: simplex-facets, simplex-faces-of-dim, simplex-all-faces
     - Simplicial complex: sc-from-simplices, sc-add-simplex, sc-contains?
     - Iteration: sc-simplices, sc-simplices-dim, sc-vertices, sc-edges, sc-faces
     - Subcomplex ops: sc-skeleton, sc-star, sc-link, sc-closed-star
     - Boundary: simplex-boundary, chain-boundary (for homology computation)
     - Invariants: sc-euler, sc-f-vector
     - Filtration: make-filtered-simplex (for persistent homology)
     - Standard complexes: sc-simplex, sc-boundary-of-simplex, sc-discrete")
   (homology.ss "Homology computation over Z_2 coefficients:
     - Z_2 matrices: z2-matrix, z2-rank, z2-nullity
     - Boundary matrices: sc-boundary-matrix
     - Betti numbers: sc-betti, sc-betti-numbers, sc-homology-summary
     - Standard spaces: make-sphere, make-torus, make-klein-bottle, make-projective-plane
     - Connected components: sc-connected-components (alternative to B_0)")))
 (tests
  ((test-simplicial-complex.ss "Tests for simplicial complex operations")
   (test-homology.ss "Tests for homology and Betti number computation")))
 (dependencies (data)))
