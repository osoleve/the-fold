((name "data")
 (purpose "Data structures and algorithms")
 (description "General-purpose data structures and graph algorithms.
Provides building blocks for higher-level abstractions.")
 (modules
  ((data-structures.ss "Persistent data structures: Stack, Queue, Set, Dict")
   (collection-utils.ss "Block collection manipulation utilities")
   (graph-algorithms.ss "Graph traversal, pathfinding, and analysis")))
 (tests
  ((test-data-structures.ss "Tests for Stack, Queue, Set, Dict")
   (test-collection-utils.ss "Tests for collection utilities")
   (test-graph-algorithms.ss "Tests for graph algorithms")))
 (dependencies (base block sha256)))
