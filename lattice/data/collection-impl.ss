;;; lattice/data/collection-impl.ss — Protocol Implementations for Data Structures
;;; @module collection-impl
;;; @requires collection-protocol, avl-tree, heap, kdtree, quadtree

(load "lattice/data/collection-protocol.ss")
(load "lattice/data/avl-tree.ss")
(load "lattice/data/heap.ss")
(load "lattice/data/kdtree.ss")
(load "lattice/data/quadtree.ss")
(load "lattice/fp/meta/combinators.ss")  ; For just/nothing

(doc 'module 'collection-impl)
(doc 'description "Protocol implementations for lattice data structures.
Registers AVL trees, heaps, k-d trees, and quadtrees with the collection protocol system.

After loading this module, generic operations work across all collection types:
  (coll-size my-avl)
  (coll-size my-heap)
  (coll-fold my-kdtree fn init)
  (coll-filter-list my-quadtree pred)")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'partial)

;;; ============================================================
;;; Empty Collection Implementations
;;; ============================================================
;;; Empty collections are symbols (e.g., 'avl-empty, 'heap-empty).
;;; The protocol system now treats symbols as their own type tags.

(doc 'section 'empty-implementations)

;; AVL empty
(implement-protocol! 'coll-empty? 'avl-empty (lambda (tree) #t))
(implement-protocol! 'coll-size 'avl-empty (lambda (tree) 0))
(implement-protocol! 'coll-fold 'avl-empty (lambda (tree fn init) init))
(implement-protocol! 'coll-to-list 'avl-empty (lambda (tree) '()))
(implement-protocol! 'keyed-lookup 'avl-empty (lambda (tree key) #f))
(implement-protocol! 'keyed-insert 'avl-empty
  (lambda (tree key value) (avl-insert key value avl-empty)))
(implement-protocol! 'keyed-delete 'avl-empty (lambda (tree key) tree))
(implement-protocol! 'keyed-contains? 'avl-empty (lambda (tree key) #f))
(implement-protocol! 'keyed-keys 'avl-empty (lambda (tree) '()))
(implement-protocol! 'keyed-values 'avl-empty (lambda (tree) '()))

;; Heap empty
(implement-protocol! 'coll-empty? 'heap-empty (lambda (heap) #t))
(implement-protocol! 'coll-size 'heap-empty (lambda (heap) 0))
(implement-protocol! 'coll-fold 'heap-empty (lambda (heap fn init) init))
(implement-protocol! 'coll-to-list 'heap-empty (lambda (heap) '()))
(implement-protocol! 'prio-peek 'heap-empty
  (lambda (heap) (error 'prio-peek "Cannot peek empty heap")))
(implement-protocol! 'prio-pop 'heap-empty
  (lambda (heap) (error 'prio-pop "Cannot pop empty heap")))
(implement-protocol! 'prio-insert 'heap-empty
  (lambda (heap elem) (heap-insert elem heap-empty)))
(implement-protocol! 'prio-merge 'heap-empty
  (lambda (h1 h2) h2))  ; Merging empty with h2 returns h2

;; KDTree empty
(implement-protocol! 'coll-empty? 'kdtree-empty (lambda (tree) #t))
(implement-protocol! 'coll-size 'kdtree-empty (lambda (tree) 0))
(implement-protocol! 'coll-fold 'kdtree-empty (lambda (tree fn init) init))
(implement-protocol! 'coll-to-list 'kdtree-empty (lambda (tree) '()))
(implement-protocol! 'spatial-nearest 'kdtree-empty (lambda (tree query) #f))
(implement-protocol! 'spatial-knn 'kdtree-empty (lambda (tree query k) '()))
(implement-protocol! 'spatial-range 'kdtree-empty (lambda (tree min-pt max-pt) '()))
(implement-protocol! 'spatial-radius 'kdtree-empty (lambda (tree center radius) '()))
(implement-protocol! 'spatial-contains? 'kdtree-empty (lambda (tree point) #f))

;; Quadtree empty
(implement-protocol! 'coll-empty? 'quadtree-empty (lambda (tree) #t))
(implement-protocol! 'coll-size 'quadtree-empty (lambda (tree) 0))
(implement-protocol! 'coll-fold 'quadtree-empty (lambda (tree fn init) init))
(implement-protocol! 'coll-to-list 'quadtree-empty (lambda (tree) '()))
(implement-protocol! 'spatial-nearest 'quadtree-empty (lambda (tree query) #f))
(implement-protocol! 'spatial-knn 'quadtree-empty (lambda (tree query k) '()))
(implement-protocol! 'spatial-range 'quadtree-empty (lambda (tree min-pt max-pt) '()))
(implement-protocol! 'spatial-radius 'quadtree-empty (lambda (tree center radius) '()))
(implement-protocol! 'spatial-contains? 'quadtree-empty (lambda (tree point) #f))

;;; ============================================================
;;; AVL Tree Implementation
;;; ============================================================
;;; AVL implements: Core + Keyed protocols
;;; Fold traversal: in-order by key (sorted ascending)

(doc 'section 'avl-implementation)

;; Core protocols
(implement-protocol! 'coll-empty? 'avl-node
  (lambda (tree) #f))  ; Non-empty nodes are never empty

(implement-protocol! 'coll-size 'avl-node
  (lambda (tree) (avl-size tree)))

(implement-protocol! 'coll-fold 'avl-node
  (lambda (tree fn init)
    ;; Wrap avl-fold to provide (key . value) pairs to fn
    (avl-fold (lambda (acc k v) (fn acc (cons k v)))
              init tree)))

(implement-protocol! 'coll-to-list 'avl-node
  (lambda (tree) (avl->list tree)))

;; Keyed protocols
(implement-protocol! 'keyed-lookup 'avl-node
  (lambda (tree key) (avl-lookup key tree)))

(implement-protocol! 'keyed-insert 'avl-node
  (lambda (tree key value) (avl-insert key value tree)))

(implement-protocol! 'keyed-delete 'avl-node
  (lambda (tree key) (avl-delete key tree)))

(implement-protocol! 'keyed-contains? 'avl-node
  (lambda (tree key) (avl-contains? key tree)))

(implement-protocol! 'keyed-keys 'avl-node
  (lambda (tree) (avl-keys tree)))

(implement-protocol! 'keyed-values 'avl-node
  (lambda (tree) (avl-values tree)))

;;; ============================================================
;;; Heap Implementation
;;; ============================================================
;;; Heap implements: Core + Priority protocols
;;; Fold traversal: unspecified tree order (NOT priority order; use prio-pop for that)

(doc 'section 'heap-implementation)

;; Core protocols
(implement-protocol! 'coll-empty? 'heap-node
  (lambda (heap) #f))  ; Non-empty nodes are never empty

(implement-protocol! 'coll-size 'heap-node
  (lambda (heap) (heap-size heap)))

(implement-protocol! 'coll-fold 'heap-node
  (lambda (heap fn init)
    ;; Direct tree traversal - O(n) instead of O(n log n)
    ;; Note: Order is unspecified (not heap order). Use prio-pop for ordered access.
    (heap-fold fn init heap)))

(implement-protocol! 'coll-to-list 'heap-node
  (lambda (heap) (heap->list heap)))

;; Priority protocols
(implement-protocol! 'prio-peek 'heap-node
  (lambda (heap) (heap-min heap)))

(implement-protocol! 'prio-pop 'heap-node
  (lambda (heap)
    (let-values ([(new-heap elem) (heap-pop heap)])
      (values new-heap elem))))

(implement-protocol! 'prio-insert 'heap-node
  (lambda (heap elem) (heap-insert elem heap)))

(implement-protocol! 'prio-merge 'heap-node
  (lambda (h1 h2) (heap-merge h1 h2)))

;;; ============================================================
;;; KD-Tree Implementation
;;; ============================================================
;;; KD-Tree implements: Core + Spatial protocols
;;; Fold traversal: in-order (left subtree, node, right subtree)

(doc 'section 'kdtree-implementation)

;; Core protocols
(implement-protocol! 'coll-empty? 'kdtree-node
  (lambda (tree) #f))

(implement-protocol! 'coll-size 'kdtree-node
  (lambda (tree) (kdtree-size tree)))

(implement-protocol! 'coll-fold 'kdtree-node
  (lambda (tree fn init)
    (kdtree-fold fn init tree)))

(implement-protocol! 'coll-to-list 'kdtree-node
  (lambda (tree) (kdtree->list tree)))

;; Spatial protocols
(implement-protocol! 'spatial-nearest 'kdtree-node
  (lambda (tree query) (kdtree-nearest tree query)))

(implement-protocol! 'spatial-knn 'kdtree-node
  (lambda (tree query k) (kdtree-knn tree query k)))

(implement-protocol! 'spatial-range 'kdtree-node
  (lambda (tree min-pt max-pt) (kdtree-range tree min-pt max-pt)))

(implement-protocol! 'spatial-radius 'kdtree-node
  (lambda (tree center radius) (kdtree-radius tree center radius)))

(implement-protocol! 'spatial-contains? 'kdtree-node
  (lambda (tree point)
    (kdtree-member? tree point (point-dimension point))))

;;; ============================================================
;;; Quadtree Implementation
;;; ============================================================
;;; Quadtree implements: Core + Spatial protocols
;;; Note: Quadtree has two node types: quadtree-leaf and quadtree-node
;;; Fold traversal: spatial order (SW -> SE -> NW -> NE, bottom-to-top)

(doc 'section 'quadtree-implementation)

;; Core protocols - leaf nodes
(implement-protocol! 'coll-empty? 'quadtree-leaf
  (lambda (tree) (null? (quadtree-leaf-points tree))))

(implement-protocol! 'coll-size 'quadtree-leaf
  (lambda (tree) (quadtree-size tree)))

(implement-protocol! 'coll-fold 'quadtree-leaf
  (lambda (tree fn init)
    (quadtree-fold fn init tree)))

(implement-protocol! 'coll-to-list 'quadtree-leaf
  (lambda (tree) (quadtree->list tree)))

;; Core protocols - internal nodes
(implement-protocol! 'coll-empty? 'quadtree-node
  (lambda (tree) (= 0 (quadtree-size tree))))  ; Check logical emptiness, not structural

(implement-protocol! 'coll-size 'quadtree-node
  (lambda (tree) (quadtree-size tree)))

(implement-protocol! 'coll-fold 'quadtree-node
  (lambda (tree fn init)
    (quadtree-fold fn init tree)))

(implement-protocol! 'coll-to-list 'quadtree-node
  (lambda (tree) (quadtree->list tree)))

;; Spatial protocols - leaf nodes
(implement-protocol! 'spatial-nearest 'quadtree-leaf
  (lambda (tree query)
    (quadtree-nearest tree (car query) (cadr query))))

(implement-protocol! 'spatial-knn 'quadtree-leaf
  (lambda (tree query k)
    (quadtree-knn tree (car query) (cadr query) k)))

(implement-protocol! 'spatial-range 'quadtree-leaf
  (lambda (tree min-pt max-pt)
    (quadtree-range-rect tree
                         (car min-pt) (cadr min-pt)
                         (car max-pt) (cadr max-pt))))

(implement-protocol! 'spatial-radius 'quadtree-leaf
  (lambda (tree center radius)
    (quadtree-radius tree (car center) (cadr center) radius)))

(implement-protocol! 'spatial-contains? 'quadtree-leaf
  (lambda (tree point)
    (quadtree-member? tree (car point) (cadr point))))

;; Spatial protocols - internal nodes
(implement-protocol! 'spatial-nearest 'quadtree-node
  (lambda (tree query)
    (quadtree-nearest tree (car query) (cadr query))))

(implement-protocol! 'spatial-knn 'quadtree-node
  (lambda (tree query k)
    (quadtree-knn tree (car query) (cadr query) k)))

(implement-protocol! 'spatial-range 'quadtree-node
  (lambda (tree min-pt max-pt)
    (quadtree-range-rect tree
                         (car min-pt) (cadr min-pt)
                         (car max-pt) (cadr max-pt))))

(implement-protocol! 'spatial-radius 'quadtree-node
  (lambda (tree center radius)
    (quadtree-radius tree (car center) (cadr center) radius)))

(implement-protocol! 'spatial-contains? 'quadtree-node
  (lambda (tree point)
    (quadtree-member? tree (car point) (cadr point))))

;;; ============================================================
;;; Protocol Coverage Summary
;;; ============================================================

(doc 'coverage "
| Protocol           | AVL | Heap | KDTree | Quadtree |
|--------------------|-----|------|--------|----------|
| coll-empty?        |  ✓  |  ✓   |   ✓    |    ✓     |
| coll-size          |  ✓  |  ✓   |   ✓    |    ✓     |
| coll-fold          |  ✓  |  ✓   |   ✓    |    ✓     |
| coll-to-list       |  ✓  |  ✓   |   ✓    |    ✓     |
| keyed-lookup       |  ✓  |      |        |          |
| keyed-insert       |  ✓  |      |        |          |
| keyed-delete       |  ✓  |      |        |          |
| keyed-contains?    |  ✓  |      |        |          |
| keyed-keys         |  ✓  |      |        |          |
| keyed-values       |  ✓  |      |        |          |
| spatial-nearest    |     |      |   ✓    |    ✓     |
| spatial-knn        |     |      |   ✓    |    ✓     |
| spatial-range      |     |      |   ✓    |    ✓     |
| spatial-radius     |     |      |   ✓    |    ✓     |
| spatial-contains?  |     |      |   ✓    |    ✓     |
| prio-peek          |     |  ✓   |        |          |
| prio-pop           |     |  ✓   |        |          |
| prio-insert        |     |  ✓   |        |          |
| prio-merge         |     |  ✓   |        |          |

Fold traversal order:
  AVL:      in-order by key (sorted ascending)
  Heap:     unspecified tree traversal (NOT priority order)
  KDTree:   in-order (left, node, right)
  Quadtree: spatial (SW -> SE -> NW -> NE)
")

(display "  AVL:      Core + Keyed protocols\n")
(display "  Heap:     Core + Priority protocols\n")
(display "  KDTree:   Core + Spatial protocols\n")
(display "  Quadtree: Core + Spatial protocols\n")
