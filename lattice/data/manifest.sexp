;;; lattice/data/manifest.sexp — Data Structures Skill Manifest

(skill data
  (version "0.1.0")
  (tier 0)
  (purity total)
  (fuel-bound "O(log n) for balanced structures, O(n) for linear")
  (deps ())  ; Tier 0 - no lattice dependencies

  (description
   "Fundamental data structures: heaps, balanced trees, graphs,
    hash tables, and collection utilities.")

  (exports
   (data-structures graph-algorithms collection-utils pagerank graph-matrix))

  (modules
   (data-structures "data-structures.ss" "Core data structure implementations")
   (graph-algorithms "graph-algorithms.ss" "BFS, DFS, shortest paths, spanning trees")
   (collection-utils "collection-utils.ss" "Higher-order collection operations")
   (pagerank "pagerank.ss" "PageRank and graph centrality measures")
   (graph-matrix "graph-matrix.ss" "Adjacency and Laplacian matrices")))
