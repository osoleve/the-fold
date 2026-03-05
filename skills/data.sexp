;;; skills/data.sexp — Skill curation file

(skill data
  (description
    "Fundamental data structures: heaps, balanced trees, graphs,\n    dictionaries, collection utilities, and unified collection protocols.")

  (keywords (data-structure graph heap priority-queue leftist-heap heapsort sort merge-sort quicksort insertion-sort stable-sort median heapsort vector-sort avl-tree balanced-tree ordered-map range-query bst tree dictionary alist queue stack bfs dfs shortest-path pagerank collection adjacency-matrix floyd-warshall dijkstra transitive-closure graph-distance eigenvector-centrality katz-centrality closeness-centrality betweenness-centrality community-detection label-propagation modularity modularity-ilp minimum-spanning-tree mst prim kruskal union-find homology betti-numbers simplicial-complex cycle-basis euler-characteristic maximum-matching blossom edmonds matching general-matching kdtree kd-tree spatial nearest-neighbor knn range-query delete quadtree spatial-index point-query 2d dynamic protocol polymorphism dispatch generic collection-protocol keyed-collection spatial-collection priority-collection))

  (aliases (ds structures collections))

  (concepts (data-structures tree-structures graph-structures spatial-structures priority-structures sequential-structures sorting-algorithms graph-algorithms string-algorithms search-algorithms))

  (modules
    avl-tree
    sort
    heap
    stack
    queue
    set
    dict
    collection-utils
    graph-primitives
    graph-homology
    pagerank
    graph-matrix
    centrality
    graph-community
    graph-layout
    kdtree
    quadtree
    collection-protocol
    collection-impl
    graph-bridge
    graph-filtration
    max-flow
    graph-matching
    random-graphs
    spectral-community
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
