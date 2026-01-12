((name "data")
 (purpose "Data structures and algorithms")
 (description "General-purpose data structures and graph algorithms.
Provides building blocks for higher-level abstractions.")
 (modules
  ((data-structures.ss "Persistent data structures: Stack, Queue, Set, Dict")
   (collection-utils.ss "Block collection manipulation utilities")
   (graph-algorithms.ss "Graph traversal, pathfinding, and analysis")
   (pagerank.ss "PageRank importance scoring")
   (graph-matrix.ss "Adjacency matrix representation and distance algorithms:
     - Edge list to matrix conversion (dense and sparse)
     - Graph generators: complete, cycle, path, star, bipartite
     - Degree matrices and graph transformations
     - Single-source shortest paths:
       * dijkstra: O(n²) weighted shortest paths from source
       * dijkstra-path: Reconstruct path from source to target
       * dijkstra-distance: Get distance between two nodes
     - All-pairs shortest paths:
       * floyd-warshall: O(n³) all-pairs weighted shortest paths
       * has-negative-cycle?: Detect negative cycles
       * distance-matrix-unweighted: BFS-style distances via matrix powers
       * transitive-closure: Warshall's algorithm for reachability
       * matrix-power-fast: O(log k) binary exponentiation for A^k
     - Graph metrics from distance matrix:
       * graph-eccentricity: Max distance from a node
       * graph-diameter: Longest shortest path
       * graph-radius: Smallest eccentricity
       * graph-center: Nodes with eccentricity = radius")
   (centrality.ss "Matrix-based graph centrality measures:
     - eigenvector-centrality: Dominant eigenvector of adjacency matrix
     - katz-centrality: Attenuated walk counts with baseline
     - closeness-centrality: Inverse of average distance to all nodes
     - betweenness-centrality: Brandes' algorithm for path betweenness
     - Utility functions: rank-by-centrality, top-k-central, centrality-correlation")
   (graph-community.ss "Community detection and spanning tree algorithms:
     - label-propagation: Fast community detection via neighbor label voting
     - modularity: Quality metric for community partitions
     - prim-mst: O(n^2) minimum spanning tree from adjacency matrix
     - kruskal-mst: O(m log m) MST from edge list with union-find
     - connected-components: BFS-based component labeling
     - is-connected?: Test graph connectivity")))
 (tests
  ((test-data-structures.ss "Tests for Stack, Queue, Set, Dict")
   (test-collection-utils.ss "Tests for collection utilities")
   (test-graph-algorithms.ss "Tests for graph algorithms")
   (test-pagerank.ss "Tests for PageRank")
   (test-centrality.ss "Tests for centrality measures")
   (test-graph-community.ss "Tests for community detection and MST")))
 (dependencies (base block sha256)))
