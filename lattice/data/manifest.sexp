;;; lattice/data/manifest.sexp — Data Structures Skill Manifest

(skill data
  (version "0.4.0")
  (tier 0)
  (path "lattice/data")
  (purity total)
  (stability stable)
  (fuel-bound "O(log n) for balanced structures, O((V+E) log V) for graph algorithms, O(n³) for graph homology")
  (deps (fp))  ; collection-protocol depends on fp/protocol

  (description
   "Fundamental data structures: heaps, balanced trees, graphs,
    dictionaries, collection utilities, and unified collection protocols.")

  (keywords (data-structure graph heap priority-queue leftist-heap heapsort
             sort merge-sort quicksort insertion-sort stable-sort median heapsort vector-sort
             avl-tree balanced-tree ordered-map range-query bst
             tree dictionary alist queue stack bfs dfs shortest-path pagerank
             collection adjacency-matrix floyd-warshall dijkstra
             transitive-closure graph-distance eigenvector-centrality
             katz-centrality closeness-centrality betweenness-centrality
             community-detection label-propagation modularity modularity-ilp
             minimum-spanning-tree mst prim kruskal union-find
             homology betti-numbers simplicial-complex cycle-basis euler-characteristic
             kdtree kd-tree spatial nearest-neighbor knn range-query delete
             quadtree spatial-index point-query 2d dynamic
             protocol polymorphism dispatch generic collection-protocol
             keyed-collection spatial-collection priority-collection))
  (aliases (ds structures collections))

  (exports
   (avl-tree
     avl-empty avl-empty? avl-node? avl-height avl-key avl-value avl-left avl-right
     avl-balance-factor make-avl-node
     avl-lookup avl-lookup-by avl-contains? avl-contains-by?
     avl-insert avl-insert-by avl-delete avl-delete-by
     avl-delete-min avl-delete-min-by
     avl-min avl-max avl-min-value avl-max-value avl-min-node avl-max-node
     avl-size avl->list avl-keys avl-values
     list->avl avl-from-keys
     avl-fold avl-fold-right avl-map avl-filter
     avl-range avl-range-by avl-keys-between
     avl-less-than avl-greater-than
     avl-set-insert avl-set-delete avl-set-member?
     avl-set-union avl-set-intersection avl-set-difference
     avl-valid?)

   (sort
     merge-sort merge-sort-by sort sort-by stable-sort stable-sort-by
     quicksort quicksort-by quicksort-desc
     insertion-sort insertion-sort-by insert-sorted
     sort-by-key sort-by-key-desc sort-desc sort-unique
     sorted? sorted-by? sorted-desc? unique-sorted
     nth-smallest median min-element max-element
     min-by max-by take-sorted top-k bottom-k
     vector-sort-by vector-sort-by!)

   (heap
     heap-empty heap-empty? heap-node? heap-rank heap-value heap-left heap-right
     make-heap-node
     heap-merge heap-insert heap-min heap-peek heap-delete-min heap-pop
     heap-merge-max heap-insert-max heap-max heap-delete-max heap-pop-max
     heap-merge-by heap-insert-by heap-delete-top-by
     heap-size heap-fold
     list->heap list->heap-max list->heap-by heap->list heap->list-max heapify
     heapsort heapsort-desc heapsort-by
     pq-empty pq-empty? pq-insert pq-peek pq-pop pq-size pq-from-list pq-to-list)

   (stack
     stack-empty stack-empty? stack-push stack-pop stack-peek
     stack-size stack->list list->stack)

   (queue
     queue-empty queue-empty? queue-enqueue queue-dequeue queue-peek
     queue-size queue->list list->queue)

   (set
     set-empty set-empty? set-member? set-add set-remove
     set-union set-intersection set-difference set-subset?
     set-size set->list list->set)

   (dict
     dict-empty dict-empty? dict-lookup dict-has-key?
     dict-assoc dict-dissoc
     dict-keys dict-values dict-entries
     dict-merge dict-map-values dict-filter dict-size
     dict->alist alist->dict)

   (collection-utils
     foldr
     collection-hashes collection-size collection-empty?
     map-collection filter-collection fold-collection foldr-collection
     for-each-collection collection-find collection-any? collection-all?
     collection-count-matching collection-partition collection-group-by
     make-collection-from-blocks collection-add collection-remove collection-merge)

   (graph-primitives
     bytevector-hash
     make-visited visited-add visited-contains? visited-remove
     hash-equal? hash-in-list? remove-hash unique-hashes
     path-length unique-cycles)

   (graph-homology
     graph->simplicial-complex graph-adjacency->simplicial-complex
     graph-betti-numbers graph-betti-numbers-from-adjacency
     cycle-basis-homology
     graph-euler-characteristic graph-cycle-rank
     graph-is-tree? graph-is-forest?)

   (pagerank
     pagerank pagerank-from-matrix personalized-pagerank
     adjacency->transition make-google-matrix
     top-k-nodes)

   (graph-matrix
     edge-list? edge-weighted? edge-from edge-to edge-weight infer-node-count
     edges->adjacency-matrix adjacency-matrix->edges
     make-adjacency-matrix adjacency-matrix-add-edge! adjacency-matrix-remove-edge!
     adjacency-matrix-has-edge? adjacency-matrix-edge-weight
     edges->sparse-adjacency sparse-adjacency->edges
     adjacency-matrix-node-count adjacency-matrix-edge-count
     adjacency-out-degree adjacency-in-degree
     adjacency-neighbors adjacency-neighbors-with-weights
     adjacency-transpose adjacency-symmetrize adjacency-symmetrize-sparse
     degree-matrix degree-matrix-sparse
     complete-graph cycle-graph path-graph star-graph bipartite-graph
     adjacency-power adjacency-reachability
     transitive-closure floyd-warshall has-negative-cycle?
     dijkstra dijkstra-path dijkstra-distance dijkstra-naive
     distance-matrix-unweighted
     graph-eccentricity graph-diameter graph-radius graph-center
     adjacency-to-sparse adjacency-to-dense adjacency-density)

   (centrality
     eigenvector-centrality eigenvector-centrality-from-edges
     katz-centrality katz-centrality-from-edges
     closeness-centrality closeness-centrality-from-adj closeness-centrality-from-edges
     betweenness-centrality betweenness-centrality-from-edges
     rank-by-centrality top-k-central centrality-correlation
     all-centralities centrality-summary)

   (graph-community
     label-propagation communities->partition num-communities
     modularity modularity-ilp
     prim-mst prim-mst-naive mst-weight kruskal-mst mst-from-adjacency
     connected-components is-connected?
     community-induced-edges community-betti-numbers all-communities-betti
     community-homology-quality aggregate-community-quality
     community-homology-report)

   (graph-layout
     make-graph-node node-id node-pos node-vel node-x node-y
     node-set-pos node-set-vel
     node-pos-lens node-vel-lens node-x-lens node-y-lens
     make-layout-graph graph-nodes graph-edges graph-set-nodes graph-find-node
     graph-nodes-lens graph-nodes-each graph-all-positions graph-all-velocities
     run-layout layout-bounds normalize-layout
     hierarchical-layout layout-from-adjacency)

   (kdtree
     kdtree-empty kdtree-empty? kdtree-node? kdtree-point kdtree-left kdtree-right kdtree-depth
     point-dimension point-coord point-distance-sq point-distance
     kdtree-build list->kdtree
     kdtree-nearest kdtree-knn kdtree-range kdtree-radius
     kdtree-insert kdtree-insert-2d kdtree-insert-3d
     kdtree-delete kdtree-delete-2d kdtree-delete-3d
     kdtree-member? kdtree-size kdtree-height kdtree-balanced?
     kdtree-fold kdtree->list)

   (quadtree
     quadtree-empty quadtree-empty? quadtree-leaf? quadtree-node?
     make-bounds bounds? bounds-cx bounds-cy bounds-hw bounds-hh
     bounds-contains? bounds-intersects?
     quadtree-leaf-bounds quadtree-leaf-points
     quadtree-bounds quadtree-ne quadtree-nw quadtree-se quadtree-sw
     quadtree-create quadtree-build quadtree-build-auto list->quadtree
     quadtree-insert
     quadtree-range quadtree-range-rect quadtree-radius
     quadtree-nearest quadtree-knn
     quadtree-size quadtree-depth quadtree-fold quadtree->list
     quadtree-delete quadtree-delete-if quadtree-member?)

   (collection-protocol
     coll-empty? coll-size coll-fold coll-to-list
     coll-count coll-any? coll-all? coll-filter-list coll-map-list
     coll-find coll-partition coll-sum coll-product
     keyed-lookup keyed-ref keyed-insert keyed-delete keyed-contains?
     keyed-keys keyed-values
     spatial-nearest spatial-knn spatial-range spatial-radius spatial-contains?
     prio-peek prio-pop prio-insert prio-merge
     coll-protocols)

   (collection-impl))

  (modules
   (avl-tree "avl-tree.ss" "Self-balancing AVL tree with O(log n) operations")
   (sort "sort.ss" "Sorting algorithms for lists and vectors: merge sort, quicksort, insertion sort, heapsort")
   (heap "heap.ss" "Leftist heap and priority queue with O(log n) operations")
   (stack "stack.ss" "LIFO stack operations")
   (queue "queue.ss" "FIFO queue with amortized O(1) ops")
   (set "set.ss" "Unordered collection with no duplicates")
   (dict "dict.ss" "Key-value dictionary/map operations")
   (collection-utils "collection-utils.ss" "Higher-order collection operations")
   ;; Graph modules moved to graph/ subdirectory
   (graph-primitives "graph/graph-primitives.ss" "Pure data structures: visited sets, queues, stacks, hash/cycle utilities")
   (graph-homology "graph/graph-homology.ss" "Homology-based cycle analysis: Betti numbers, cycle basis, tree/forest detection")
   (pagerank "graph/pagerank.ss" "PageRank importance scoring")
   (graph-matrix "graph/graph-matrix.ss" "Adjacency matrices, Dijkstra O((V+E) log V), Floyd-Warshall, graph metrics")
   (centrality "graph/centrality.ss" "Eigenvector, Katz, closeness, betweenness centrality")
   (graph-community "graph/graph-community.ss" "Community detection, Prim MST O((V+E) log V), Kruskal, modularity")
   (graph-layout "graph/graph-layout.ss" "Force-directed graph layout algorithms")
   (kdtree "kdtree.ss" "K-d tree for O(log n) nearest neighbor and range queries in any dimension")
   (quadtree "quadtree.ss" "Quadtree for 2D spatial queries with uniform subdivision")
   (collection-protocol "collection-protocol.ss" "Unified protocols for collection operations: core, keyed, spatial, priority")
   (collection-impl "collection-impl.ss" "Protocol implementations for AVL, heap, kdtree, quadtree")))
