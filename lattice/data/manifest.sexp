;;; lattice/data/manifest.sexp — Data Structures Skill Manifest

(skill data
  (version "0.2.0")
  (tier 0)
  (path "lattice/data")
  (purity total)
  (stability stable)
  (fuel-bound "O(log n) for balanced structures, O((V+E) log V) for graph algorithms, O(n³) for homology")
  (deps ())  ; Tier 0 - no skill-level deps (graph-algorithms has file-level dep on topology)

  (description
   "Fundamental data structures: heaps, balanced trees, graphs,
    dictionaries, and collection utilities.")

  (keywords (data-structure graph heap priority-queue leftist-heap heapsort
             sort merge-sort quicksort insertion-sort stable-sort median
             avl-tree balanced-tree ordered-map range-query bst
             tree dictionary alist queue stack bfs dfs shortest-path pagerank
             collection adjacency-matrix floyd-warshall dijkstra
             transitive-closure graph-distance eigenvector-centrality
             katz-centrality closeness-centrality betweenness-centrality
             community-detection label-propagation modularity modularity-ilp
             minimum-spanning-tree mst prim kruskal union-find
             homology betti-numbers simplicial-complex cycle-basis euler-characteristic))
  (aliases (ds structures collections))

  (exports
   (avl-tree sort heap stack queue set dict graph-algorithms collection-utils pagerank graph-matrix centrality graph-community))

  (modules
   (avl-tree "avl-tree.ss" "Self-balancing AVL tree with O(log n) operations")
   (sort "sort.ss" "Sorting algorithms: merge sort, quicksort, insertion sort")
   (heap "heap.ss" "Leftist heap and priority queue with O(log n) operations")
   (stack "stack.ss" "LIFO stack operations")
   (queue "queue.ss" "FIFO queue with amortized O(1) ops")
   (set "set.ss" "Unordered collection with no duplicates")
   (dict "dict.ss" "Key-value dictionary/map operations")
   (graph-algorithms "graph-algorithms.ss" "BFS, DFS, shortest paths, spanning trees, homology")
   (collection-utils "collection-utils.ss" "Higher-order collection operations")
   (pagerank "pagerank.ss" "PageRank importance scoring")
   (graph-matrix "graph-matrix.ss" "Adjacency matrices, Dijkstra O((V+E) log V), Floyd-Warshall, graph metrics")
   (centrality "centrality.ss" "Eigenvector, Katz, closeness, betweenness centrality")
   (graph-community "graph-community.ss" "Community detection, Prim MST O((V+E) log V), Kruskal, modularity")))
