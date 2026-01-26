;;; lattice/data/quadtree.ss — Quadtree for 2D Spatial Queries
;;; @module quadtree
;;; @requires prelude, heap

(load "core/base/prelude.ss")
(load "lattice/data/heap.ss")

(doc 'module 'quadtree)
(doc 'description "Quadtree for efficient 2D spatial queries. Uniformly subdivides space into
four quadrants (NE, NW, SE, SW). Supports point insertion, range queries, and nearest neighbor.
Unlike k-d trees, quadtrees use fixed subdivision rather than median splitting, making them
better for dynamic insertions and spatial locality queries.")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'total)

;;; ============================================================
;;; Core Representation
;;; ============================================================

(doc 'section 'core-representation)

;; Bounding box: (center-x center-y half-width half-height)
(define (make-bounds cx cy hw hh)
  (doc 'type '(-> Num Num Num Num Bounds))
  (doc 'description "Create a bounding box with center (cx,cy) and half-dimensions (hw,hh)")
  (list 'bounds cx cy hw hh))

(define (bounds? x)
  (and (pair? x) (eq? (car x) 'bounds)))

(define (bounds-cx b) (cadr b))
(define (bounds-cy b) (caddr b))
(define (bounds-hw b) (cadddr b))
(define (bounds-hh b) (car (cddddr b)))

(define (bounds-contains? bounds x y)
  (doc 'type '(-> Bounds Num Num Boolean))
  (doc 'description "Check if point (x,y) is within bounds")
  (and (>= x (- (bounds-cx bounds) (bounds-hw bounds)))
       (<= x (+ (bounds-cx bounds) (bounds-hw bounds)))
       (>= y (- (bounds-cy bounds) (bounds-hh bounds)))
       (<= y (+ (bounds-cy bounds) (bounds-hh bounds)))))

(define (bounds-intersects? b1 b2)
  (doc 'type '(-> Bounds Bounds Boolean))
  (doc 'description "Check if two bounding boxes intersect")
  (not (or (> (- (bounds-cx b1) (bounds-hw b1)) (+ (bounds-cx b2) (bounds-hw b2)))
           (< (+ (bounds-cx b1) (bounds-hw b1)) (- (bounds-cx b2) (bounds-hw b2)))
           (> (- (bounds-cy b1) (bounds-hh b1)) (+ (bounds-cy b2) (bounds-hh b2)))
           (< (+ (bounds-cy b1) (bounds-hh b1)) (- (bounds-cy b2) (bounds-hh b2))))))

;; Quadtree node: either a leaf with points or an internal node with 4 children
(define quadtree-empty 'quadtree-empty)

(define (quadtree-empty? tree)
  (doc 'type '(-> Quadtree Boolean))
  (eq? tree 'quadtree-empty))

;; Leaf node: holds up to capacity points before splitting
(define (quadtree-leaf bounds points)
  (doc 'type '(-> Bounds (List Point) Quadtree))
  (list 'quadtree-leaf bounds points))

(define (quadtree-leaf? x)
  (and (pair? x) (eq? (car x) 'quadtree-leaf)))

(define (quadtree-leaf-bounds node) (cadr node))
(define (quadtree-leaf-points node) (caddr node))

;; Internal node: 4 children (NE, NW, SE, SW)
(define (quadtree-node bounds ne nw se sw)
  (doc 'type '(-> Bounds Quadtree Quadtree Quadtree Quadtree Quadtree))
  (list 'quadtree-node bounds ne nw se sw))

(define (quadtree-node? x)
  (and (pair? x) (eq? (car x) 'quadtree-node)))

(define (quadtree-bounds node)
  (cond [(quadtree-leaf? node) (quadtree-leaf-bounds node)]
        [(quadtree-node? node) (cadr node)]
        [else #f]))

(define (quadtree-ne node) (caddr node))
(define (quadtree-nw node) (cadddr node))
(define (quadtree-se node) (car (cddddr node)))
(define (quadtree-sw node) (cadr (cddddr node)))

;;; ============================================================
;;; Construction
;;; ============================================================

(doc 'section 'construction)

(define *quadtree-capacity* 4)
(doc *quadtree-capacity* 'description "Maximum points per leaf before splitting")

(define *quadtree-max-depth* 16)
(doc *quadtree-max-depth* 'description "Maximum tree depth to prevent infinite recursion on duplicate points")

(define (quadtree-create bounds)
  (doc 'type '(-> Bounds Quadtree))
  (doc 'description "Create an empty quadtree with given bounds")
  (quadtree-leaf bounds '()))

(define (quadtree-build points bounds)
  (doc 'type '(-> (List Point) Bounds Quadtree))
  (doc 'description "Build a quadtree from a list of 2D points (each point is (x y))")
  (fold-left (lambda (tree pt)
               (quadtree-insert tree (car pt) (cadr pt) pt))
             (quadtree-create bounds)
             points))

(define (quadtree-build-auto points)
  (doc 'type '(-> (List Point) Quadtree))
  (doc 'description "Build quadtree with auto-computed bounds from points")
  (if (null? points)
      quadtree-empty
      (let* ([xs (map car points)]
             [ys (map cadr points)]
             [min-x (apply min xs)]
             [max-x (apply max xs)]
             [min-y (apply min ys)]
             [max-y (apply max ys)]
             [cx (/ (+ min-x max-x) 2)]
             [cy (/ (+ min-y max-y) 2)]
             [hw (/ (- max-x min-x) 2)]
             [hh (/ (- max-y min-y) 2)]
             ;; Add 1% padding to ensure all points fit
             [hw+ (if (zero? hw) 1 (* hw 1.01))]
             [hh+ (if (zero? hh) 1 (* hh 1.01))])
        (quadtree-build points (make-bounds cx cy hw+ hh+)))))

;; Alias for API consistency with list->vector, list->string, etc.
(define list->quadtree quadtree-build-auto)

;;; ============================================================
;;; Insertion
;;; ============================================================

(doc 'section 'insertion)

(define (quadtree-insert tree x y data)
  (doc 'type '(-> Quadtree Num Num Any Quadtree))
  (doc 'description "Insert a point at (x,y) with associated data into the quadtree")
  (quadtree-insert-at-depth tree x y data 0))

(define (quadtree-insert-at-depth tree x y data depth)
  ;; Internal insert with depth tracking to prevent infinite recursion on duplicates
  (cond
    [(quadtree-empty? tree) tree]  ; can't insert into empty tree (no bounds)
    [(quadtree-leaf? tree)
     (let ([bounds (quadtree-leaf-bounds tree)]
           [points (quadtree-leaf-points tree)])
       (if (not (bounds-contains? bounds x y))
           tree  ; point out of bounds, ignore
           (if (or (< (length points) *quadtree-capacity*)
                   (>= depth *quadtree-max-depth*))
               ;; Room in leaf, or max depth reached - just add without splitting
               (quadtree-leaf bounds (cons (list x y data) points))
               ;; Need to split
               (let ([split-tree (quadtree-split tree depth)])
                 (quadtree-insert-at-depth split-tree x y data (+ depth 1))))))]
    [(quadtree-node? tree)
     (let ([bounds (quadtree-bounds tree)])
       (if (not (bounds-contains? bounds x y))
           tree  ; out of bounds
           ;; Insert into appropriate quadrant
           (let* ([cx (bounds-cx bounds)]
                  [cy (bounds-cy bounds)]
                  [ne (quadtree-ne tree)]
                  [nw (quadtree-nw tree)]
                  [se (quadtree-se tree)]
                  [sw (quadtree-sw tree)]
                  [next-depth (+ depth 1)])
             (cond
               [(and (>= x cx) (>= y cy))
                (quadtree-node bounds (quadtree-insert-at-depth ne x y data next-depth) nw se sw)]
               [(and (< x cx) (>= y cy))
                (quadtree-node bounds ne (quadtree-insert-at-depth nw x y data next-depth) se sw)]
               [(and (>= x cx) (< y cy))
                (quadtree-node bounds ne nw (quadtree-insert-at-depth se x y data next-depth) sw)]
               [else
                (quadtree-node bounds ne nw se (quadtree-insert-at-depth sw x y data next-depth))]))))]))

(define (quadtree-split leaf depth)
  (doc 'type '(-> Quadtree Nat Quadtree))
  (doc 'description "Split a leaf node into 4 children. Depth parameter ensures
correct depth tracking when re-inserting existing points into child nodes.")
  (let* ([bounds (quadtree-leaf-bounds leaf)]
         [points (quadtree-leaf-points leaf)]
         [cx (bounds-cx bounds)]
         [cy (bounds-cy bounds)]
         [hw (/ (bounds-hw bounds) 2)]
         [hh (/ (bounds-hh bounds) 2)]
         ;; Create 4 child bounds
         [ne-bounds (make-bounds (+ cx hw) (+ cy hh) hw hh)]
         [nw-bounds (make-bounds (- cx hw) (+ cy hh) hw hh)]
         [se-bounds (make-bounds (+ cx hw) (- cy hh) hw hh)]
         [sw-bounds (make-bounds (- cx hw) (- cy hh) hw hh)]
         ;; Create empty children
         [ne (quadtree-leaf ne-bounds '())]
         [nw (quadtree-leaf nw-bounds '())]
         [se (quadtree-leaf se-bounds '())]
         [sw (quadtree-leaf sw-bounds '())]
         ;; Create node
         [node (quadtree-node bounds ne nw se sw)]
         ;; Depth for re-inserted points is current depth + 1
         [child-depth (+ depth 1)])
    ;; Re-insert all points at correct depth
    (fold-left (lambda (tree pt)
                 (quadtree-insert-at-depth tree (car pt) (cadr pt) (caddr pt) child-depth))
               node
               points)))

;;; ============================================================
;;; Range Queries
;;; ============================================================

(doc 'section 'range-queries)

(define (quadtree-range tree query-bounds)
  (doc 'type '(-> Quadtree Bounds (List Point)))
  (doc 'description "Find all points within the query bounding box")
  (cond
    [(quadtree-empty? tree) '()]
    [(quadtree-leaf? tree)
     (if (not (bounds-intersects? (quadtree-leaf-bounds tree) query-bounds))
         '()
         (filter (lambda (pt)
                   (bounds-contains? query-bounds (car pt) (cadr pt)))
                 (quadtree-leaf-points tree)))]
    [(quadtree-node? tree)
     (if (not (bounds-intersects? (quadtree-bounds tree) query-bounds))
         '()
         (append (quadtree-range (quadtree-ne tree) query-bounds)
                 (quadtree-range (quadtree-nw tree) query-bounds)
                 (quadtree-range (quadtree-se tree) query-bounds)
                 (quadtree-range (quadtree-sw tree) query-bounds)))]))

(define (quadtree-range-rect tree x1 y1 x2 y2)
  (doc 'type '(-> Quadtree Num Num Num Num (List Point)))
  (doc 'description "Find all points in rectangle from (x1,y1) to (x2,y2)")
  (let* ([min-x (min x1 x2)]
         [max-x (max x1 x2)]
         [min-y (min y1 y2)]
         [max-y (max y1 y2)]
         [cx (/ (+ min-x max-x) 2)]
         [cy (/ (+ min-y max-y) 2)]
         [hw (/ (- max-x min-x) 2)]
         [hh (/ (- max-y min-y) 2)])
    (quadtree-range tree (make-bounds cx cy hw hh))))

(define (quadtree-radius tree cx cy radius)
  (doc 'type '(-> Quadtree Num Num Num (List Point)))
  (doc 'description "Find all points within radius of (cx, cy)")
  (let* ([bounds (make-bounds cx cy radius radius)]
         [candidates (quadtree-range tree bounds)]
         [r-sq (* radius radius)])
    (filter (lambda (pt)
              (let ([dx (- (car pt) cx)]
                    [dy (- (cadr pt) cy)])
                (<= (+ (* dx dx) (* dy dy)) r-sq)))
            candidates)))

;;; ============================================================
;;; Nearest Neighbor
;;; ============================================================

(doc 'section 'nearest-neighbor)

(define (quadtree-nearest tree qx qy)
  (doc 'type '(-> Quadtree Num Num (Maybe Point)))
  (doc 'description "Find the nearest point to (qx, qy). Returns #f if tree is empty.")
  (let ([result (quadtree-nearest-rec tree qx qy +inf.0 #f)])
    (if (car result) (car result) #f)))

(define (quadtree-nearest-rec tree qx qy best-dist best-point)
  ;; Returns (best-point . best-dist-sq)
  (cond
    [(quadtree-empty? tree)
     (cons best-point best-dist)]
    [(quadtree-leaf? tree)
     (let ([points (quadtree-leaf-points tree)])
       (fold-left
        (lambda (acc pt)
          (let* ([dx (- (car pt) qx)]
                 [dy (- (cadr pt) qy)]
                 [d-sq (+ (* dx dx) (* dy dy))])
            (if (< d-sq (cdr acc))
                (cons pt d-sq)
                acc)))
        (cons best-point best-dist)
        points))]
    [(quadtree-node? tree)
     ;; Check if we need to search this node at all
     (let ([bounds (quadtree-bounds tree)])
       (if (> (bounds-min-dist-sq bounds qx qy) best-dist)
           (cons best-point best-dist)  ; pruned
           ;; Search all quadrants, ordered by distance to query
           (let* ([children (list (quadtree-ne tree) (quadtree-nw tree)
                                  (quadtree-se tree) (quadtree-sw tree))]
                  [sorted (sort-by-bounds-dist children qx qy)])
             (fold-left
              (lambda (acc child)
                ;; Note: sort-by-bounds-dist already filters empty children
                (let ([child-bounds (quadtree-bounds child)])
                  (if (> (bounds-min-dist-sq child-bounds qx qy) (cdr acc))
                      acc  ; prune
                      (quadtree-nearest-rec child qx qy (cdr acc) (car acc)))))
              (cons best-point best-dist)
              sorted))))]))

(define (bounds-min-dist-sq bounds qx qy)
  (doc 'type '(-> Bounds Num Num Num))
  (doc 'description "Minimum squared distance from point to bounding box")
  (let* ([cx (bounds-cx bounds)]
         [cy (bounds-cy bounds)]
         [hw (bounds-hw bounds)]
         [hh (bounds-hh bounds)]
         [x-dist (max 0 (- (abs (- qx cx)) hw))]
         [y-dist (max 0 (- (abs (- qy cy)) hh))])
    (+ (* x-dist x-dist) (* y-dist y-dist))))

(define (sort-by-bounds-dist children qx qy)
  (sort (lambda (a b)
          (< (bounds-min-dist-sq (quadtree-bounds a) qx qy)
             (bounds-min-dist-sq (quadtree-bounds b) qx qy)))
        (filter (lambda (c) (not (quadtree-empty? c))) children)))

;;; ============================================================
;;; K-Nearest Neighbors
;;; ============================================================

(doc 'section 'k-nearest-neighbors)

;; Comparator for (distance-sq . point) pairs - max-heap by distance
(define (quadtree-knn-pair-greater? p1 p2)
  (>= (car p1) (car p2)))

(define (quadtree-knn tree qx qy k-count)
  (doc 'type '(-> Quadtree Num Num Nat (List Point)))
  (doc 'description "Find k nearest points to (qx, qy). Returns list sorted by distance (closest first).
O(k log n) average case. Uses max-heap to track k closest candidates with distance-based pruning.")
  (if (or (quadtree-empty? tree) (<= k-count 0))
      '()
      (let* ([result-heap (quadtree-knn-rec tree qx qy k-count heap-empty)]
             [pairs (quadtree-knn-heap->list result-heap)])
        (reverse (map cdr pairs)))))

;; Extract all pairs from k-NN heap (max-heap by distance)
(define (quadtree-knn-heap->list heap)
  (if (heap-empty? heap)
      '()
      (cons (heap-value heap)
            (quadtree-knn-heap->list (heap-delete-top-by quadtree-knn-pair-greater? heap)))))

(define (quadtree-knn-rec tree qx qy k-count heap)
  ;; heap contains up to k-count (distance-sq . point) pairs as a max-heap
  (cond
    [(quadtree-empty? tree) heap]
    [(quadtree-leaf? tree)
     ;; Process all points in leaf
     (fold-left
      (lambda (h pt)
        (let* ([dx (- (car pt) qx)]
               [dy (- (cadr pt) qy)]
               [dist-sq (+ (* dx dx) (* dy dy))]
               [current-size (heap-size h)]
               [threshold (if (or (= current-size 0) (< current-size k-count))
                             +inf.0
                             (car (heap-value h)))])
          (if (< dist-sq threshold)
              (let ([h2 (heap-insert-by quadtree-knn-pair-greater? (cons dist-sq pt) h)])
                (if (> (heap-size h2) k-count)
                    (heap-delete-top-by quadtree-knn-pair-greater? h2)
                    h2))
              h)))
      heap
      (quadtree-leaf-points tree))]
    [(quadtree-node? tree)
     ;; Check if we should prune this subtree
     (let ([bounds (quadtree-bounds tree)]
           [current-size (heap-size heap)])
       (let ([threshold (if (or (= current-size 0) (< current-size k-count))
                           +inf.0
                           (car (heap-value heap)))])
         (if (> (bounds-min-dist-sq bounds qx qy) threshold)
             heap  ; prune: bounds too far
             ;; Search children, sorted by distance (closest first)
             (let* ([children (list (quadtree-ne tree) (quadtree-nw tree)
                                    (quadtree-se tree) (quadtree-sw tree))]
                    [sorted (sort-by-bounds-dist children qx qy)])
               (fold-left
                (lambda (h child)
                  ;; Note: sort-by-bounds-dist already filters empty children
                  (let* ([child-bounds (quadtree-bounds child)]
                         [h-size (heap-size h)]
                         [child-threshold (if (or (= h-size 0) (< h-size k-count))
                                             +inf.0
                                             (car (heap-value h)))])
                    (if (> (bounds-min-dist-sq child-bounds qx qy) child-threshold)
                        h  ; prune this child
                        (quadtree-knn-rec child qx qy k-count h))))
                heap
                sorted)))))]))

;;; ============================================================
;;; Statistics
;;; ============================================================

(doc 'section 'statistics)

(define (quadtree-size tree)
  (doc 'type '(-> Quadtree Nat))
  (doc 'description "Count total points in the quadtree")
  (cond
    [(quadtree-empty? tree) 0]
    [(quadtree-leaf? tree) (length (quadtree-leaf-points tree))]
    [(quadtree-node? tree)
     (+ (quadtree-size (quadtree-ne tree))
        (quadtree-size (quadtree-nw tree))
        (quadtree-size (quadtree-se tree))
        (quadtree-size (quadtree-sw tree)))]))

(define (quadtree-depth tree)
  (doc 'type '(-> Quadtree Nat))
  (doc 'description "Maximum depth of the quadtree")
  (cond
    [(quadtree-empty? tree) 0]
    [(quadtree-leaf? tree) 1]
    [(quadtree-node? tree)
     (+ 1 (max (quadtree-depth (quadtree-ne tree))
               (quadtree-depth (quadtree-nw tree))
               (quadtree-depth (quadtree-se tree))
               (quadtree-depth (quadtree-sw tree))))]))

(define (quadtree-fold proc init tree)
  (doc 'type '(-> (-> a Point a) a Quadtree a))
  (doc 'description "Left fold over all points in quadtree. (proc acc point) called for each point.
Traversal order: SW -> SE -> NW -> NE (bottom-to-top, left-to-right). O(n).
Signature matches fold-left convention: accumulator first.")
  (cond
    [(quadtree-empty? tree) init]
    [(quadtree-leaf? tree)
     (fold-left proc init (quadtree-leaf-points tree))]
    [(quadtree-node? tree)
     (quadtree-fold proc
                    (quadtree-fold proc
                                   (quadtree-fold proc
                                                  (quadtree-fold proc init (quadtree-sw tree))
                                                  (quadtree-se tree))
                                   (quadtree-nw tree))
                    (quadtree-ne tree))]))

(define (quadtree->list tree)
  (doc 'type '(-> Quadtree (List Point)))
  (doc 'description "Extract all points from the quadtree. O(n)")
  (reverse (quadtree-fold (lambda (acc pt) (cons pt acc)) '() tree)))

;;; ============================================================
;;; Deletion
;;; ============================================================

(doc 'section 'deletion)

(define (quadtree-delete tree x y)
  (doc 'type '(-> Quadtree Num Num Quadtree))
  (doc 'description "Delete a point at (x,y) from the quadtree. If multiple points at
that location, removes the first match. Returns unchanged tree if point not found.
After deletion, merges sparse children back into parent when appropriate.")
  (quadtree-delete-if tree x y (lambda (pt) #t)))

(define (quadtree-delete-if tree x y pred)
  (doc 'type '(-> Quadtree Num Num (-> Point Boolean) Quadtree))
  (doc 'description "Delete a point at (x,y) matching predicate. Predicate receives
the full point (x y data). Returns unchanged tree if no match found.")
  (cond
    [(quadtree-empty? tree) tree]
    [(quadtree-leaf? tree)
     (let ([bounds (quadtree-leaf-bounds tree)]
           [points (quadtree-leaf-points tree)])
       (if (not (bounds-contains? bounds x y))
           tree  ; not in this region
           ;; Remove first matching point
           (let ([new-points (delete-first-matching points x y pred)])
             (quadtree-leaf bounds new-points))))]
    [(quadtree-node? tree)
     (let ([bounds (quadtree-bounds tree)])
       (if (not (bounds-contains? bounds x y))
           tree  ; not in this region
           ;; Recurse into appropriate quadrant
           (let* ([cx (bounds-cx bounds)]
                  [cy (bounds-cy bounds)]
                  [ne (quadtree-ne tree)]
                  [nw (quadtree-nw tree)]
                  [se (quadtree-se tree)]
                  [sw (quadtree-sw tree)]
                  [new-tree
                   (cond
                     [(and (>= x cx) (>= y cy))
                      (quadtree-node bounds (quadtree-delete-if ne x y pred) nw se sw)]
                     [(and (< x cx) (>= y cy))
                      (quadtree-node bounds ne (quadtree-delete-if nw x y pred) se sw)]
                     [(and (>= x cx) (< y cy))
                      (quadtree-node bounds ne nw (quadtree-delete-if se x y pred) sw)]
                     [else
                      (quadtree-node bounds ne nw se (quadtree-delete-if sw x y pred))])])
             ;; Try to merge if total points in all children <= capacity
             (quadtree-try-merge new-tree))))]))

(define (delete-first-matching points x y pred)
  ;; Remove first point at (x,y) matching predicate
  (cond
    [(null? points) '()]
    [(and (= (car (car points)) x)
          (= (cadr (car points)) y)
          (pred (car points)))
     (cdr points)]  ; found it, skip this one
    [else
     (cons (car points) (delete-first-matching (cdr points) x y pred))]))

(define (quadtree-try-merge tree)
  (doc 'type '(-> Quadtree Quadtree))
  (doc 'description "If all children are leaves and total points <= capacity, merge into single leaf")
  (if (not (quadtree-node? tree))
      tree
      (let ([ne (quadtree-ne tree)]
            [nw (quadtree-nw tree)]
            [se (quadtree-se tree)]
            [sw (quadtree-sw tree)])
        (if (and (quadtree-leaf? ne) (quadtree-leaf? nw)
                 (quadtree-leaf? se) (quadtree-leaf? sw))
            (let ([total-points (+ (length (quadtree-leaf-points ne))
                                   (length (quadtree-leaf-points nw))
                                   (length (quadtree-leaf-points se))
                                   (length (quadtree-leaf-points sw)))])
              (if (<= total-points *quadtree-capacity*)
                  ;; Merge all points into parent leaf
                  (quadtree-leaf
                   (quadtree-bounds tree)
                   (append (quadtree-leaf-points ne)
                           (quadtree-leaf-points nw)
                           (quadtree-leaf-points se)
                           (quadtree-leaf-points sw)))
                  tree))
            tree))))

(define (quadtree-member? tree x y)
  (doc 'type '(-> Quadtree Num Num Boolean))
  (doc 'description "Check if a point exists at (x,y) in the quadtree")
  (cond
    [(quadtree-empty? tree) #f]
    [(quadtree-leaf? tree)
     (let ([points (quadtree-leaf-points tree)])
       (any? (lambda (pt) (and (= (car pt) x) (= (cadr pt) y))) points))]
    [(quadtree-node? tree)
     (let ([bounds (quadtree-bounds tree)]
           [cx (bounds-cx (quadtree-bounds tree))]
           [cy (bounds-cy (quadtree-bounds tree))])
       (if (not (bounds-contains? bounds x y))
           #f
           (cond
             [(and (>= x cx) (>= y cy)) (quadtree-member? (quadtree-ne tree) x y)]
             [(and (< x cx) (>= y cy)) (quadtree-member? (quadtree-nw tree) x y)]
             [(and (>= x cx) (< y cy)) (quadtree-member? (quadtree-se tree) x y)]
             [else (quadtree-member? (quadtree-sw tree) x y)])))]))

(define (any? pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any? pred (cdr lst))]))
