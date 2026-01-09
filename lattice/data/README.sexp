((name "data")
 (purpose "Data structures and algorithms")
 (description "General-purpose data structures and graph algorithms.
Provides building blocks for higher-level abstractions.")
 (modules
  ((data-structures.ss "Persistent data structures: Stack, Queue, Set, Dict")
   (collection-utils.ss "Block collection manipulation utilities")
   (graph-algorithms.ss "Graph traversal, pathfinding, and analysis")
   (pagerank.ss "PageRank and graph centrality measures")
   (graph-matrix.ss "Adjacency matrix representation and distance algorithms:
     - Edge list to matrix conversion (dense and sparse)
     - Graph generators: complete, cycle, path, star, bipartite
     - Degree matrices and graph transformations
     - Matrix-based distance algorithms:
       * matrix-power-fast: O(log k) binary exponentiation for A^k
       * transitive-closure: Warshall's algorithm for reachability
       * floyd-warshall: All-pairs shortest paths (weighted)
       * has-negative-cycle?: Detect negative cycles
       * distance-matrix-unweighted: BFS-style distances via matrix powers
     - Graph metrics from distance matrix:
       * graph-eccentricity: Max distance from a node
       * graph-diameter: Longest shortest path
       * graph-radius: Smallest eccentricity
       * graph-center: Nodes with eccentricity = radius")))
 (tests
  ((test-data-structures.ss "Tests for Stack, Queue, Set, Dict")
   (test-collection-utils.ss "Tests for collection utilities")
   (test-graph-algorithms.ss "Tests for graph algorithms")))
 (dependencies (base block sha256)))
