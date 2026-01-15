((name "topology")
 (purpose "Computational topology and topological data analysis")
 (description "Simplicial complexes, boundary operators, and topological invariants.
Provides the foundational data structures for algebraic topology computations
including homology groups, Betti numbers, and persistent homology.")
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
     - Standard complexes: sc-simplex, sc-boundary-of-simplex, sc-discrete")))
 (tests
  ((test-simplicial-complex.ss "Tests for simplicial complex operations")))
 (dependencies (data)))
