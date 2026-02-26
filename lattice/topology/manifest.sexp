(skill topology
  (version "0.4.0")
  (path "lattice/topology")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n³) for homology/persistence, O(n^(d+1)) for Rips construction")
  (deps (data linalg))
  (description "Computational topology and topological data analysis.
Includes simplicial complexes, homology over Z₂, persistent homology,
Vietoris-Rips filtrations, persistence diagrams, and barcodes.")
  (keywords (topology simplicial-complex homology betti-numbers boundary euler-characteristic
             star link skeleton filtration tda z2-coefficients persistent-homology
             persistence-diagram barcode vietoris-rips bottleneck-distance))
  (aliases (topo simplicial persistent tda))

  (concepts
    (concept computational-topology
      (description "Discrete topological invariants: simplicial complexes, homology, Betti numbers, and persistent homology.")
      (parent mathematics)
      (synonyms topology topo))
    (concept simplicial-homology
      (description "Chain complexes, boundary operators, Betti numbers, and Euler characteristic of simplicial complexes over Z2.")
      (parent computational-topology)
      (synonyms simplicial-complex simplicial betti-numbers euler-characteristic homology))
    (concept persistent-homology
      (description "Topological data analysis via filtrations: Vietoris-Rips complexes, persistence diagrams, and barcodes.")
      (parent computational-topology)
      (synonyms tda barcode persistence-diagram vietoris-rips persistent)))

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
      ;; Rank and null space computation
      z2-rank z2-nullity z2-rref z2-null-space
      ;; Boundary matrices
      sc-boundary-matrix sc-boundary-matrix-print
      ;; Betti numbers (main interface)
      sc-betti sc-betti-numbers sc-homology-summary
      ;; Standard topological spaces
      make-sphere make-torus make-klein-bottle make-projective-plane
      ;; Connected components (β₀ alternative)
      sc-connected-components)
    (persistent
      ;; Filtration construction
      make-filtration filtration? filtration-pairs filtration-simplices filtration-size
      ;; Vietoris-Rips complex from point cloud
      rips-filtration euclidean-distance
      ;; Persistence computation (main interface)
      persistence-reduce compute-persistence
      ;; Persistence diagram
      make-persistence-diagram persistence-diagram? diagram-points diagram-points-dim
      diagram-betti persistence-summary
      ;; Barcode representation
      diagram->barcode barcode? barcode-intervals barcode-print
      ;; Distance metrics
      diagram-bottleneck)
    (topo-landscape
      sample-grid-2d sample-grid-nd sample-around-points
      sublevel-filtration landscape-persistence
      landscape-betti count-basins count-saddles
      landscape-signature landscape-complexity
      landscape-distance landscape-similar?
      analyze-landscape-2d analyze-landscape-nd trajectory-persistence))
  (modules
    (simplicial-complex "simplicial-complex.ss"
      "Core simplicial complex data structures and operations")
    (homology "homology.ss"
      "Homology groups and Betti numbers over Z₂ coefficients")
    (persistent "persistent.ss"
      "Persistent homology, Vietoris-Rips filtrations, and TDA")
    (topo-landscape "topo-landscape.ss"
      "Topological landscape analysis via persistent homology")))
