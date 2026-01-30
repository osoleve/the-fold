((name "topology")
(purpose "Computational topology and topological data analysis")
(description "Simplicial complexes, homology groups, and Betti numbers.\nProvides foundational data structures for algebraic topology computations\nusing Z_2 coefficients. Supports topological data analysis (TDA).")
(modules 
  ((simplicial-complex.ss "Core simplicial complex data structures:\n     - Simplex: make-simplex, simplex-dim, simplex-vertices, simplex-face?\n     - Convenience: vertex, edge, triangle, tetrahedron\n     - Face enumeration: simplex-facets, simplex-faces-of-dim, simplex-all-faces\n     - Simplicial complex: sc-from-simplices, sc-add-simplex, sc-contains?\n     - Iteration: sc-simplices, sc-simplices-dim, sc-vertices, sc-edges, sc-faces\n     - Subcomplex ops: sc-skeleton, sc-star, sc-link, sc-closed-star\n     - Boundary: simplex-boundary, chain-boundary (for homology computation)\n     - Invariants: sc-euler, sc-f-vector\n     - Filtration: make-filtered-simplex (for persistent homology)\n     - Standard complexes: sc-simplex, sc-boundary-of-simplex, sc-discrete")
  (homology.ss "Homology computation over Z_2 coefficients:\n     - Z_2 matrices: z2-matrix, z2-rank, z2-nullity\n     - Boundary matrices: sc-boundary-matrix\n     - Betti numbers: sc-betti, sc-betti-numbers, sc-homology-summary\n     - Standard spaces: make-sphere, make-torus, make-klein-bottle, make-projective-plane\n     - Connected components: sc-connected-components (alternative to B_0)")))
(tests 
  ((test-simplicial-complex.ss "Tests for simplicial complex operations")
  (test-homology.ss "Tests for homology and Betti number computation")))
(dependencies (data)))
