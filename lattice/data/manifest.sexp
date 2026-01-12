;;; lattice/data/manifest.sexp — Data Structures Skill Manifest

(skill data
  (version "0.1.0")
  (tier 0)
  (path "lattice/data")
  (purity total)
  (stability stable)
  (fuel-bound "O(log n) for balanced structures, O(n) for linear")
  (deps ())  ; Tier 0 - no lattice dependencies

  (description
   "Fundamental data structures: heaps, balanced trees, graphs,
    hash tables, and collection utilities.")

  (keywords (data-structure graph heap tree hash-table queue stack
             bfs dfs shortest-path pagerank collection adjacency-matrix
             floyd-warshall dijkstra transitive-closure graph-distance))
  (aliases (ds structures collections))

  (exports
   (data-structures graph-algorithms collection-utils pagerank graph-matrix))

  (modules
   (data-structures "data-structures.ss" "Core data structure implementations")
   (graph-algorithms "graph-algorithms.ss" "BFS, DFS, shortest paths, spanning trees")
   (collection-utils "collection-utils.ss" "Higher-order collection operations")
   (pagerank "pagerank.ss" "PageRank and graph centrality measures")
   (graph-matrix "graph-matrix.ss" "Adjacency matrices, distance algorithms, graph metrics")))
