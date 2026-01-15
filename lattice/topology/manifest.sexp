(skill topology
  (version "0.2.1")
  (tier 1)
  (path "lattice/topology")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n³) for homology via Gaussian elimination, O(N*k) for boundary matrix construction")
  (deps (data))
  (description "Computational topology: simplicial complexes, homology groups, and Betti numbers.
Computes topological invariants over Z₂ coefficients. Foundation for persistent homology and TDA.")
  (keywords (topology simplicial-complex homology betti-numbers boundary euler-characteristic
             star link skeleton filtration tda z2-coefficients))
  (aliases (topo simplicial))
  (exports
    (simplicial-complex
      ;; Simplex constructors and predicates
      make-simplex simplex? simplex-vertices simplex-dim simplex-empty?
      simplex-equal? simplex-contains-vertex? simplex-face?
      ;; Convenience constructors
      vertex edge triangle tetrahedron
      ;; Face enumeration
      simplex-facets simplex-faces-of-dim simplex-all-faces simplex-proper-faces
      ;; Simplicial complex
      sc-empty sc? sc-from-simplices sc-add-simplex sc-contains?
      sc-simplices sc-simplices-dim sc-vertices sc-edges sc-faces
      sc-count sc-count-dim sc-max-dim
      ;; Skeleton and subcomplex
      sc-skeleton sc-star sc-closed-star sc-link
      ;; Boundary operator
      simplex-boundary chain-boundary chain-empty chain-add-term
      ;; Topological invariants
      sc-euler sc-f-vector
      ;; Filtration support
      make-filtered-simplex filtered-simplex? filtered-simplex-base filtered-simplex-value
      ;; Standard complexes
      sc-simplex sc-boundary-of-simplex sc-discrete)
    (homology
      ;; Z₂ matrix operations
      z2-matrix z2-matrix? z2-matrix-rows z2-matrix-cols z2-matrix-ref
      z2-matrix-zero z2-matrix-copy z2-matrix-print
      ;; Rank computation
      z2-rank z2-nullity
      ;; Boundary matrices
      sc-boundary-matrix sc-boundary-matrix-print
      ;; Betti numbers (main interface)
      sc-betti sc-betti-numbers sc-homology-summary
      ;; Standard topological spaces
      make-sphere make-torus make-klein-bottle make-projective-plane
      ;; Connected components (β₀ alternative)
      sc-connected-components))
  (modules
    (simplicial-complex "simplicial-complex.ss"
      "Core simplicial complex data structures and operations")
    (homology "homology.ss"
      "Homology groups and Betti numbers over Z₂ coefficients")))
