(skill topology
  (version "0.1.0")
  (tier 1)
  (path "lattice/topology")
  (purity total)
  (stability experimental)
  (fuel-bound "O(2^n) for n-simplex face enumeration")
  (deps (data))
  (description "Computational topology: simplicial complexes, boundary operators, and topological invariants.
Foundation for persistent homology and topological data analysis.")
  (keywords (topology simplicial-complex homology boundary euler-characteristic
             star link skeleton filtration tda))
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
      sc-simplex sc-boundary-of-simplex sc-discrete))
  (modules
    (simplicial-complex "simplicial-complex.ss"
      "Core simplicial complex data structures and operations")))
