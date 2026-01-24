;;; lattice/data/kdtree.ss — K-D Tree for Spatial Queries
;;; @module kdtree
;;; @requires prelude

(load "core/base/prelude.ss")
(load "lattice/data/heap.ss")

(doc 'module 'kdtree)
(doc 'description "K-dimensional tree for efficient spatial queries on point sets.
Supports nearest neighbor search in O(log n) average, k-NN queries, and range queries
in O(√n + k) where k is result size. Points are represented as vectors (lists of coordinates).")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'total)

;;; ============================================================
;;; Core Representation
;;; ============================================================

(doc 'section 'core-representation)

(define kdtree-empty 'kdtree-empty)
(doc kdtree-empty 'type 'KDTree)
(doc kdtree-empty 'description "The empty k-d tree")

(define (kdtree-empty? tree)
  (doc 'type '(-> KDTree Boolean))
  (doc 'description "Check if tree is empty")
  (eq? tree 'kdtree-empty))

(define (kdtree-node point left right depth)
  (doc 'type '(-> Point KDTree KDTree Nat KDTree))
  (doc 'description "Internal node: point, left child, right child, depth (determines split axis)")
  (list 'kdtree-node point left right depth))

(define (kdtree-node? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'kdtree-node)))

(define (kdtree-point node)
  (doc 'type '(-> KDTree Point))
  (cadr node))

(define (kdtree-left node)
  (doc 'type '(-> KDTree KDTree))
  (caddr node))

(define (kdtree-right node)
  (doc 'type '(-> KDTree KDTree))
  (cadddr node))

(define (kdtree-depth node)
  (doc 'type '(-> KDTree Nat))
  (car (cddddr node)))

;;; ============================================================
;;; Point Utilities
;;; ============================================================

(doc 'section 'point-utilities)

(define (point-dimension pt)
  (doc 'type '(-> Point Nat))
  (doc 'description "Get dimensionality of a point")
  (length pt))

(define (point-coord pt axis)
  (doc 'type '(-> Point Nat Num))
  (doc 'description "Get coordinate along axis (0-indexed)")
  (list-ref pt axis))

(define (point-distance-sq p1 p2)
  (doc 'type '(-> Point Point Num))
  (doc 'description "Squared Euclidean distance between points")
  ;; Tail-recursive to avoid allocation in hot path
  (let loop ([a p1] [b p2] [sum 0])
    (if (null? a)
        sum
        (let ([d (- (car a) (car b))])
          (loop (cdr a) (cdr b) (+ sum (* d d)))))))

(define (point-distance p1 p2)
  (doc 'type '(-> Point Point Num))
  (doc 'description "Euclidean distance between points")
  (sqrt (point-distance-sq p1 p2)))

;;; ============================================================
;;; Construction
;;; ============================================================

(doc 'section 'construction)

(define (kdtree-build points)
  (doc 'type '(-> (List Point) KDTree))
  (doc 'description "Build k-d tree from list of points. O(n log² n) using sorting-based median.
Points must all have the same dimensionality. Note: True O(n log n) would require
linear-time median selection (median-of-medians); this implementation uses sorting.")
  (if (null? points)
      kdtree-empty
      (let ([k (point-dimension (car points))])
        (kdtree-build-rec points k 0))))

;; Alias for API consistency with list->vector, list->string, etc.
(define list->kdtree kdtree-build)

(define (kdtree-build-rec points k depth)
  ;; Build recursively, splitting on axis = depth mod k
  (cond
    [(null? points) kdtree-empty]
    [(null? (cdr points))
     (kdtree-node (car points) kdtree-empty kdtree-empty depth)]
    [else
     (let* ([axis (modulo depth k)]
            [sorted (sort-by-axis points axis)]
            [mid (quotient (length sorted) 2)]
            [median-point (list-ref sorted mid)]
            [left-pts (take sorted mid)]
            [right-pts (drop sorted (+ mid 1))])
       (kdtree-node median-point
                    (kdtree-build-rec left-pts k (+ depth 1))
                    (kdtree-build-rec right-pts k (+ depth 1))
                    depth))]))

(define (sort-by-axis points axis)
  (doc 'type '(-> (List Point) Nat (List Point)))
  (doc 'description "Sort points by coordinate along given axis")
  (sort (lambda (p1 p2) (< (point-coord p1 axis) (point-coord p2 axis))) points))

(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))

;;; ============================================================
;;; Nearest Neighbor Search
;;; ============================================================

(doc 'section 'nearest-neighbor)

(define (kdtree-nearest tree query)
  (doc 'type '(-> KDTree Point (Maybe Point)))
  (doc 'description "Find nearest point to query. Returns #f for empty tree.
O(log n) average case, O(n) worst case for degenerate trees.")
  (if (kdtree-empty? tree)
      #f
      (let ([k (point-dimension query)])
        (car (kdtree-nearest-rec tree query k +inf.0 #f)))))

(define (kdtree-nearest-rec tree query k best-dist best-point)
  ;; Returns (point . distance-sq)
  (if (kdtree-empty? tree)
      (cons best-point best-dist)
      (let* ([node-point (kdtree-point tree)]
             [depth (kdtree-depth tree)]
             [axis (modulo depth k)]
             [dist-sq (point-distance-sq query node-point)]
             ;; Update best if this node is closer
             [new-best-dist (if (< dist-sq best-dist) dist-sq best-dist)]
             [new-best-point (if (< dist-sq best-dist) node-point best-point)]
             ;; Determine which side to search first
             [query-coord (point-coord query axis)]
             [node-coord (point-coord node-point axis)]
             [go-left? (< query-coord node-coord)]
             [first-child (if go-left? (kdtree-left tree) (kdtree-right tree))]
             [second-child (if go-left? (kdtree-right tree) (kdtree-left tree))])
        ;; Search the closer side first
        (let* ([result1 (kdtree-nearest-rec first-child query k new-best-dist new-best-point)]
               [best-after-first (cdr result1)]
               [point-after-first (car result1)]
               ;; Check if we need to search the other side
               [split-dist-sq (let ([d (- query-coord node-coord)]) (* d d))])
          (if (< split-dist-sq best-after-first)
              ;; The splitting plane is closer than best, search other side
              (kdtree-nearest-rec second-child query k best-after-first point-after-first)
              ;; No need to search other side
              result1)))))

;;; ============================================================
;;; K-Nearest Neighbors
;;; ============================================================

(doc 'section 'k-nearest-neighbors)

;; Comparator for (distance-sq . point) pairs - max-heap by distance
(define (knn-pair-greater? p1 p2)
  (>= (car p1) (car p2)))

(define (kdtree-knn tree query k-count)
  (doc 'type '(-> KDTree Point Nat (List Point)))
  (doc 'description "Find k nearest points to query. Returns list sorted by distance (closest first).
O(k log n) average case.")
  (if (kdtree-empty? tree)
      '()
      (let* ([dim (point-dimension query)]
             ;; Use a max-heap (by distance) to track k closest points
             ;; Heap elements are (distance-sq . point) pairs
             [result-heap (kdtree-knn-rec tree query dim k-count heap-empty)]
             ;; Extract points in farthest-to-closest order, then reverse
             [pairs (knn-heap->list result-heap)])
        (reverse (map cdr pairs)))))

;; Extract all pairs from k-NN heap (max-heap by distance)
(define (knn-heap->list heap)
  (if (heap-empty? heap)
      '()
      (cons (heap-value heap)
            (knn-heap->list (heap-delete-top-by knn-pair-greater? heap)))))

(define (kdtree-knn-rec tree query k k-count heap)
  ;; heap contains up to k-count (distance-sq . point) pairs as a max-heap
  (if (kdtree-empty? tree)
      heap
      (let* ([node-point (kdtree-point tree)]
             [depth (kdtree-depth tree)]
             [axis (modulo depth k)]
             [dist-sq (point-distance-sq query node-point)]
             ;; Get threshold: max distance in heap if full, else infinity
             [current-size (heap-size heap)]
             [threshold (if (or (= current-size 0) (< current-size k-count))
                           +inf.0
                           (car (heap-value heap)))]  ; car gets distance from pair at root
             ;; Add this point if closer than threshold
             [new-heap (if (< dist-sq threshold)
                          (let ([h (heap-insert-by knn-pair-greater? (cons dist-sq node-point) heap)])
                            ;; If over capacity, remove farthest
                            (if (> (heap-size h) k-count)
                                (heap-delete-top-by knn-pair-greater? h)
                                h))
                          heap)]
             ;; Determine search order
             [query-coord (point-coord query axis)]
             [node-coord (point-coord node-point axis)]
             [go-left? (< query-coord node-coord)]
             [first-child (if go-left? (kdtree-left tree) (kdtree-right tree))]
             [second-child (if go-left? (kdtree-right tree) (kdtree-left tree))])
        ;; Search closer side first
        (let* ([heap-after-first (kdtree-knn-rec first-child query k k-count new-heap)]
               ;; Check if we need other side
               [size-after (heap-size heap-after-first)]
               [new-threshold (if (or (= size-after 0) (< size-after k-count))
                                 +inf.0
                                 (car (heap-value heap-after-first)))]
               [split-dist-sq (let ([d (- query-coord node-coord)]) (* d d))])
          (if (< split-dist-sq new-threshold)
              (kdtree-knn-rec second-child query k k-count heap-after-first)
              heap-after-first)))))

;;; ============================================================
;;; Range Queries
;;; ============================================================

(doc 'section 'range-queries)

(define (kdtree-range tree min-point max-point)
  (doc 'type '(-> KDTree Point Point (List Point)))
  (doc 'description "Find all points within axis-aligned bounding box [min, max].
O(√n + k) where k is number of results.")
  (if (kdtree-empty? tree)
      '()
      (let ([k (point-dimension min-point)])
        (kdtree-range-rec tree min-point max-point k))))

(define (kdtree-range-rec tree min-pt max-pt k)
  (if (kdtree-empty? tree)
      '()
      (let* ([node-point (kdtree-point tree)]
             [depth (kdtree-depth tree)]
             [axis (modulo depth k)]
             [node-coord (point-coord node-point axis)]
             [min-coord (point-coord min-pt axis)]
             [max-coord (point-coord max-pt axis)]
             ;; Check if this point is in range
             [in-range? (point-in-box? node-point min-pt max-pt)]
             ;; Determine which subtrees to search
             [search-left? (<= min-coord node-coord)]
             [search-right? (>= max-coord node-coord)])
        (append
         (if in-range? (list node-point) '())
         (if search-left?
             (kdtree-range-rec (kdtree-left tree) min-pt max-pt k)
             '())
         (if search-right?
             (kdtree-range-rec (kdtree-right tree) min-pt max-pt k)
             '())))))

(define (point-in-box? point min-pt max-pt)
  (doc 'type '(-> Point Point Point Boolean))
  (doc 'description "Check if point is within axis-aligned bounding box [min, max] (inclusive)")
  (let loop ([pt point] [lo min-pt] [hi max-pt])
    (cond
      [(null? pt) #t]
      [(or (< (car pt) (car lo)) (> (car pt) (car hi))) #f]
      [else (loop (cdr pt) (cdr lo) (cdr hi))])))

(define (kdtree-radius tree center radius)
  (doc 'type '(-> KDTree Point Num (List Point)))
  (doc 'description "Find all points within radius of center point.
Uses range query for initial filtering, then distance check.")
  (if (kdtree-empty? tree)
      '()
      (let* ([k (point-dimension center)]
             ;; Create bounding box that contains the sphere
             [min-pt (map (lambda (c) (- c radius)) center)]
             [max-pt (map (lambda (c) (+ c radius)) center)]
             ;; Get candidates from range query
             [candidates (kdtree-range tree min-pt max-pt)]
             [radius-sq (* radius radius)])
        ;; Filter to actual sphere
        (filter (lambda (pt) (<= (point-distance-sq pt center) radius-sq))
                candidates))))

;;; ============================================================
;;; Tree Statistics
;;; ============================================================

(doc 'section 'statistics)

(define (kdtree-size tree)
  (doc 'type '(-> KDTree Nat))
  (doc 'description "Count number of points in tree. O(n)")
  (if (kdtree-empty? tree)
      0
      (+ 1 (kdtree-size (kdtree-left tree))
           (kdtree-size (kdtree-right tree)))))

(define (kdtree-height tree)
  (doc 'type '(-> KDTree Nat))
  (doc 'description "Get height of tree. O(n)")
  (if (kdtree-empty? tree)
      0
      (+ 1 (max (kdtree-height (kdtree-left tree))
                (kdtree-height (kdtree-right tree))))))

(define (kdtree-balanced? tree)
  (doc 'type '(-> KDTree Boolean))
  (doc 'description "Check if tree is approximately balanced (height ~ log n)")
  (if (kdtree-empty? tree)
      #t
      (let ([h (kdtree-height tree)]
            [n (kdtree-size tree)])
        (and (> n 0)
             (<= h (* 2 (+ 1 (floor (log n 2)))))))))

(define (kdtree-fold proc init tree)
  (doc 'type '(-> (-> a Point a) a KDTree a))
  (doc 'description "Left fold over all points in tree. (proc acc point) called for each point.
In-order traversal (left subtree, node, right subtree). O(n).
Signature matches fold-left convention: accumulator first.")
  (if (kdtree-empty? tree)
      init
      (kdtree-fold proc
                   (proc (kdtree-fold proc init (kdtree-left tree))
                         (kdtree-point tree))
                   (kdtree-right tree))))

(define (kdtree->list tree)
  (doc 'type '(-> KDTree (List Point)))
  (doc 'description "Extract all points from tree (in-order traversal). O(n)")
  (reverse (kdtree-fold (lambda (acc pt) (cons pt acc)) '() tree)))

;;; ============================================================
;;; Insertion (for incremental building)
;;; ============================================================

(doc 'section 'insertion)

(define (kdtree-insert tree point k)
  (doc 'type '(-> KDTree Point Nat KDTree))
  (doc 'description "Insert a point into existing tree. O(log n) average.
Note: Repeated insertions can unbalance tree. For bulk construction, use kdtree-build.
Parameter k is dimensionality.")
  (kdtree-insert-rec tree point k 0))

(define (kdtree-insert-rec tree point k depth)
  (if (kdtree-empty? tree)
      (kdtree-node point kdtree-empty kdtree-empty depth)
      (let* ([axis (modulo depth k)]
             [node-point (kdtree-point tree)]
             [pt-coord (point-coord point axis)]
             [nd-coord (point-coord node-point axis)])
        (if (< pt-coord nd-coord)
            (kdtree-node node-point
                        (kdtree-insert-rec (kdtree-left tree) point k (+ depth 1))
                        (kdtree-right tree)
                        depth)
            (kdtree-node node-point
                        (kdtree-left tree)
                        (kdtree-insert-rec (kdtree-right tree) point k (+ depth 1))
                        depth)))))

(define (kdtree-insert-2d tree point)
  (doc 'type '(-> KDTree Point KDTree))
  (doc 'description "Convenience: insert into 2D tree")
  (kdtree-insert tree point 2))

(define (kdtree-insert-3d tree point)
  (doc 'type '(-> KDTree Point KDTree))
  (doc 'description "Convenience: insert into 3D tree")
  (kdtree-insert tree point 3))

;;; ============================================================
;;; Deletion
;;; ============================================================

(doc 'section 'deletion)

(define (kdtree-delete tree point k)
  (doc 'type '(-> KDTree Point Nat KDTree))
  (doc 'description "Delete a point from the k-d tree. O(log n) average.
Unlike BSTs, k-d tree deletion requires finding replacement from subtree
that preserves invariant across all axes. Parameter k is dimensionality.
Returns unchanged tree if point not found.")
  (kdtree-delete-rec tree point k 0))

(define (kdtree-delete-2d tree point)
  (doc 'type '(-> KDTree Point KDTree))
  (doc 'description "Convenience: delete from 2D tree")
  (kdtree-delete tree point 2))

(define (kdtree-delete-3d tree point)
  (doc 'type '(-> KDTree Point KDTree))
  (doc 'description "Convenience: delete from 3D tree")
  (kdtree-delete tree point 3))

(define (kdtree-delete-rec tree point k depth)
  ;; Delete point from tree, maintaining k-d tree invariant
  ;; When coordinates are equal on split axis, point could be in either subtree
  (if (kdtree-empty? tree)
      tree  ; point not found
      (let* ([node-point (kdtree-point tree)]
             [axis (modulo depth k)]
             [pt-coord (point-coord point axis)]
             [nd-coord (point-coord node-point axis)])
        (cond
          [(points-equal? point node-point)
           ;; Found the point to delete
           (kdtree-delete-node tree k depth)]
          [(< pt-coord nd-coord)
           (kdtree-node node-point
                        (kdtree-delete-rec (kdtree-left tree) point k (+ depth 1))
                        (kdtree-right tree)
                        depth)]
          [(> pt-coord nd-coord)
           (kdtree-node node-point
                        (kdtree-left tree)
                        (kdtree-delete-rec (kdtree-right tree) point k (+ depth 1))
                        depth)]
          [else
           ;; Equal coordinates - try left first, then right if not found in left
           (let ([left-result (kdtree-delete-rec (kdtree-left tree) point k (+ depth 1))])
             (if (< (kdtree-size left-result) (kdtree-size (kdtree-left tree)))
                 ;; Successfully deleted from left
                 (kdtree-node node-point left-result (kdtree-right tree) depth)
                 ;; Not in left, try right
                 (kdtree-node node-point
                              (kdtree-left tree)
                              (kdtree-delete-rec (kdtree-right tree) point k (+ depth 1))
                              depth)))]))))

(define (kdtree-delete-node tree k depth)
  ;; Delete the node at root of tree, finding appropriate replacement
  (let ([left (kdtree-left tree)]
        [right (kdtree-right tree)]
        [axis (modulo depth k)])
    (cond
      ;; Leaf node (no children): just remove it
      [(and (kdtree-empty? left) (kdtree-empty? right))
       kdtree-empty]

      ;; Has right subtree: find min on axis, swap, delete from right
      [(not (kdtree-empty? right))
       (let ([replacement (kdtree-find-min right axis k (+ depth 1))])
         (kdtree-node replacement
                      left
                      (kdtree-delete-rec right replacement k (+ depth 1))
                      depth))]

      ;; Only left subtree: find min on axis from left, swap to right, delete from left
      ;; This is subtle: we move left subtree to become right subtree after finding min
      ;; because the replacement goes to current position, and remaining left points
      ;; should now be in the right subtree (they're >= replacement on split axis)
      [else
       (let ([replacement (kdtree-find-min left axis k (+ depth 1))])
         (kdtree-node replacement
                      kdtree-empty
                      (kdtree-delete-rec left replacement k (+ depth 1))
                      depth))])))

(define (kdtree-find-min tree axis k depth)
  (doc 'type '(-> KDTree Nat Nat Nat Point))
  (doc 'description "Find the point with minimum value on given axis in tree.
This is non-trivial because the tree is only partitioned on one axis per level,
so minimum on a different axis could be anywhere.")
  (kdtree-find-min-rec tree axis k depth))

(define (kdtree-find-min-rec tree axis k depth)
  ;; Find minimum point on given axis
  (if (kdtree-empty? tree)
      #f  ; shouldn't happen if called on non-empty tree
      (let* ([node-point (kdtree-point tree)]
             [split-axis (modulo depth k)]
             [left (kdtree-left tree)]
             [right (kdtree-right tree)])
        (if (= axis split-axis)
            ;; Split axis matches search axis: min must be in left subtree or current
            (if (kdtree-empty? left)
                node-point
                (point-min-on-axis node-point
                                   (kdtree-find-min-rec left axis k (+ depth 1))
                                   axis))
            ;; Different axis: min could be anywhere, must search both
            (point-min-on-axis
             node-point
             (point-min-on-axis
              (if (kdtree-empty? left) #f (kdtree-find-min-rec left axis k (+ depth 1)))
              (if (kdtree-empty? right) #f (kdtree-find-min-rec right axis k (+ depth 1)))
              axis)
             axis)))))

(define (point-min-on-axis p1 p2 axis)
  ;; Return point with smaller coordinate on axis (handles #f)
  (cond
    [(not p1) p2]
    [(not p2) p1]
    [(< (point-coord p1 axis) (point-coord p2 axis)) p1]
    [else p2]))

(define (points-equal? p1 p2)
  (doc 'type '(-> Point Point Boolean))
  (doc 'description "Check if two points are equal (all coordinates match)")
  (equal? p1 p2))

(define (kdtree-member? tree point k)
  (doc 'type '(-> KDTree Point Nat Boolean))
  (doc 'description "Check if point exists in the k-d tree")
  (kdtree-member-rec tree point k 0))

(define (kdtree-member-rec tree point k depth)
  ;; When coordinates are equal on split axis, point could be in either subtree
  ;; due to how kdtree-build sorts and splits. We must search both in that case.
  (if (kdtree-empty? tree)
      #f
      (let* ([node-point (kdtree-point tree)]
             [axis (modulo depth k)]
             [pt-coord (point-coord point axis)]
             [nd-coord (point-coord node-point axis)])
        (cond
          [(points-equal? point node-point) #t]
          [(< pt-coord nd-coord)
           (kdtree-member-rec (kdtree-left tree) point k (+ depth 1))]
          [(> pt-coord nd-coord)
           (kdtree-member-rec (kdtree-right tree) point k (+ depth 1))]
          [else
           ;; Equal coordinates - must check both subtrees
           (or (kdtree-member-rec (kdtree-left tree) point k (+ depth 1))
               (kdtree-member-rec (kdtree-right tree) point k (+ depth 1)))]))))
