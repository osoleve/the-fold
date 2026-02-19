;;; quadtree.sexp — Development tasks for lattice/data/quadtree.ss
;;;
;;; Module: quadtree (tier 0, depends on prelude, sort, heap)
;;; 573 lines, ~30 exported functions
;;; Quadtree for 2D spatial queries. Fixed uniform subdivision into
;;; NE/NW/SE/SW quadrants. Insertion, range queries, nearest neighbor,
;;; k-NN with distance-based pruning.
;;;
;;; Git history:
;;;   596c1d4d feat(module): migrate data/, optics/, parallel/ to require
;;;   19df68a3 feat(quadtree): Add spatial-knn for k-nearest neighbor queries
;;;   c0105614 feat(spatial): Add delete operations for kdtree and quadtree

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement bounds-contains?
  ;; ──────────────────────────────────────────────
  ;; AABB point containment — foundation for all queries.

  (task
    (id "data/quadtree/bounds-contains-impl")
    (module quadtree)
    (tier 0)
    (type implement)
    (description "Implement bounds-contains?: check if a point (x, y) lies within an axis-aligned bounding box. A bounding box is defined by its center (cx, cy) and half-dimensions (hw, hh). The box spans from (cx - hw) to (cx + hw) on x, and (cy - hh) to (cy + hh) on y, inclusive. Check all four conditions with and: x >= cx-hw, x <= cx+hw, y >= cy-hh, y <= cy+hh. Use bounds-cx, bounds-cy, bounds-hw, bounds-hh to extract components.")
    (context "Available: prelude (and, >=, <=, +, -), bounds-cx (center x), bounds-cy (center y), bounds-hw (half-width), bounds-hh (half-height). Bounds is (list 'bounds cx cy hw hh).")
    (before "(define (bounds-contains? bounds x y)
  (doc 'type '(-> Bounds Num Num Boolean))
  (doc 'description \"Check if point (x,y) is within bounds\")
  ;; TODO: check all four edge conditions
  #f)")
    (test-file "lattice/data/test-quadtree.ss")
    (test-forms (";; Center point is inside
(assert-true (bounds-contains? (make-bounds 5 5 5 5) 5 5))"
                 ";; Corner points are inside (inclusive)
(assert-true (bounds-contains? (make-bounds 5 5 5 5) 0 0))
(assert-true (bounds-contains? (make-bounds 5 5 5 5) 10 10))"
                 ";; Outside points
(assert-false (bounds-contains? (make-bounds 5 5 5 5) 11 5))
(assert-false (bounds-contains? (make-bounds 5 5 5 5) 5 -1))"
                 ";; Non-square bounds
(let ([b (make-bounds 0 0 10 5)])
  (assert-true (bounds-contains? b 9 4))
  (assert-false (bounds-contains? b 9 6)))"))
    (available-modules (prelude))
    (hints ("Extract: cx = (bounds-cx bounds), cy = (bounds-cy bounds), etc."
            "Left edge: x >= (- cx hw)"
            "Right edge: x <= (+ cx hw)"
            "Bottom edge: y >= (- cy hh)"
            "Top edge: y <= (+ cy hh)"
            "Combine all four with (and ...)"))
    (reference-solution "(define (bounds-contains? bounds x y)
  (and (>= x (- (bounds-cx bounds) (bounds-hw bounds)))
       (<= x (+ (bounds-cx bounds) (bounds-hw bounds)))
       (>= y (- (bounds-cy bounds) (bounds-hh bounds)))
       (<= y (+ (bounds-cy bounds) (bounds-hh bounds)))))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement bounds-intersects?
  ;; ──────────────────────────────────────────────
  ;; AABB overlap test — key for range query pruning.

  (task
    (id "data/quadtree/bounds-intersects-impl")
    (module quadtree)
    (tier 0)
    (type implement)
    (description "Implement bounds-intersects?: check if two axis-aligned bounding boxes overlap. Two AABBs do NOT intersect if any of these separating conditions hold: b1's left edge is right of b2's right edge, b1's right edge is left of b2's left edge, b1's bottom is above b2's top, or b1's top is below b2's bottom. Return (not (or ...all four separating conditions...)). This is the standard AABB intersection test used in collision detection and spatial query pruning.")
    (context "Available: prelude (not, or, >, <, +, -), bounds-cx, bounds-cy, bounds-hw, bounds-hh.")
    (before "(define (bounds-intersects? b1 b2)
  (doc 'type '(-> Bounds Bounds Boolean))
  (doc 'description \"Check if two bounding boxes intersect\")
  ;; TODO: check separating axis conditions
  #f)")
    (test-file "lattice/data/test-quadtree.ss")
    (test-forms (";; Overlapping boxes
(assert-true (bounds-intersects? (make-bounds 0 0 5 5) (make-bounds 3 3 5 5)))"
                 ";; Symmetric
(assert-true (bounds-intersects? (make-bounds 3 3 5 5) (make-bounds 0 0 5 5)))"
                 ";; Disjoint boxes
(assert-false (bounds-intersects? (make-bounds 0 0 5 5) (make-bounds 20 20 1 1)))"
                 ";; Touching edges (should intersect)
(assert-true (bounds-intersects? (make-bounds 0 0 5 5) (make-bounds 10 0 5 5)))"
                 ";; Same box
(assert-true (bounds-intersects? (make-bounds 5 5 3 3) (make-bounds 5 5 3 3)))"))
    (available-modules (prelude))
    (hints ("Four separating conditions (if ANY is true, no intersection):"
            "1. b1-left > b2-right: (> (- cx1 hw1) (+ cx2 hw2))"
            "2. b1-right < b2-left: (< (+ cx1 hw1) (- cx2 hw2))"
            "3. b1-bottom > b2-top: (> (- cy1 hh1) (+ cy2 hh2))"
            "4. b1-top < b2-bottom: (< (+ cy1 hh1) (- cy2 hh2))"
            "Return (not (or cond1 cond2 cond3 cond4))"))
    (reference-solution "(define (bounds-intersects? b1 b2)
  (not (or (> (- (bounds-cx b1) (bounds-hw b1)) (+ (bounds-cx b2) (bounds-hw b2)))
           (< (+ (bounds-cx b1) (bounds-hw b1)) (- (bounds-cx b2) (bounds-hw b2)))
           (> (- (bounds-cy b1) (bounds-hh b1)) (+ (bounds-cy b2) (bounds-hh b2)))
           (< (+ (bounds-cy b1) (bounds-hh b1)) (- (bounds-cy b2) (bounds-hh b2))))))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement bounds-min-dist-sq
  ;; ──────────────────────────────────────────────
  ;; Minimum distance from point to AABB — the pruning primitive.

  (task
    (id "data/quadtree/bounds-min-dist-sq-impl")
    (module quadtree)
    (tier 0)
    (type implement)
    (description "Implement bounds-min-dist-sq: compute the minimum squared distance from a point (qx, qy) to a bounding box. If the point is inside the box, the distance is 0. Otherwise, for each axis: compute the gap between the point and the nearest edge. On x: if the point is within the x-range of the box, x-dist = 0. Otherwise x-dist = |qx - cx| - hw (the gap). Similarly for y. The formula: x-dist = max(0, |qx - cx| - hw), y-dist = max(0, |qy - cy| - hh). Return x-dist^2 + y-dist^2. This is the key pruning function for nearest-neighbor search: if the minimum possible distance to a bounding box exceeds the current best, skip the entire subtree.")
    (context "Available: prelude (max, abs, -, +, *), bounds-cx, bounds-cy, bounds-hw, bounds-hh.")
    (before "(define (bounds-min-dist-sq bounds qx qy)
  (doc 'type '(-> Bounds Num Num Num))
  (doc 'description \"Minimum squared distance from point to bounding box\")
  ;; TODO: compute axis-aligned gap distances, return sum of squares
  0)")
    (test-file "lattice/data/test-quadtree.ss")
    (test-forms (";; Point inside box: distance is 0
(assert-equal 0 (bounds-min-dist-sq (make-bounds 5 5 5 5) 5 5))
(assert-equal 0 (bounds-min-dist-sq (make-bounds 5 5 5 5) 3 3))"
                 ";; Point outside one edge: distance to that edge
(assert-equal 1 (bounds-min-dist-sq (make-bounds 5 5 5 5) 11 5))"
                 ";; Point outside corner: distance to corner
(assert-equal 2 (bounds-min-dist-sq (make-bounds 5 5 5 5) 11 11))"
                 ";; Symmetry: same distance from both sides
(assert-equal (bounds-min-dist-sq (make-bounds 5 5 5 5) 11 5)
              (bounds-min-dist-sq (make-bounds 5 5 5 5) -1 5))"))
    (available-modules (prelude))
    (hints ("Extract cx, cy, hw, hh from bounds"
            "x-dist = (max 0 (- (abs (- qx cx)) hw))"
            "y-dist = (max 0 (- (abs (- qy cy)) hh))"
            "Return (+ (* x-dist x-dist) (* y-dist y-dist))"
            "The abs + subtraction gives the gap: if inside, the subtraction goes negative and max clamps to 0"))
    (reference-solution "(define (bounds-min-dist-sq bounds qx qy)
  (let* ([cx (bounds-cx bounds)]
         [cy (bounds-cy bounds)]
         [hw (bounds-hw bounds)]
         [hh (bounds-hh bounds)]
         [x-dist (max 0 (- (abs (- qx cx)) hw))]
         [y-dist (max 0 (- (abs (- qy cy)) hh))])
    (+ (* x-dist x-dist) (* y-dist y-dist))))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement quadtree-size
  ;; ──────────────────────────────────────────────
  ;; Three-case recursion over the quadtree structure.

  (task
    (id "data/quadtree/size-impl")
    (module quadtree)
    (tier 0)
    (type implement)
    (description "Implement quadtree-size: count the total number of points in a quadtree. A quadtree has three cases: (1) empty — return 0, (2) leaf — count the points in the leaf's point list, (3) internal node — recursively sum the sizes of all four children (NE, NW, SE, SW). Use cond to dispatch on the three cases with quadtree-empty?, quadtree-leaf?, and quadtree-node? predicates. For leaves, the points are accessible via (quadtree-leaf-points node).")
    (context "Available: prelude (length, +), quadtree-empty?, quadtree-leaf?, quadtree-node?, quadtree-leaf-points, quadtree-ne, quadtree-nw, quadtree-se, quadtree-sw.")
    (before "(define (quadtree-size tree)
  (doc 'type '(-> Quadtree Nat))
  (doc 'description \"Count total points in the quadtree\")
  ;; TODO: three-case dispatch
  0)")
    (test-file "lattice/data/test-quadtree.ss")
    (test-forms ("(assert-equal 0 (quadtree-size quadtree-empty))"
                 "(assert-equal 1 (quadtree-size (quadtree-build-auto '((5 5)))))"
                 "(assert-equal 5 (quadtree-size (quadtree-build-auto '((1 1) (3 3) (5 5) (7 7) (9 9)))))"
                 ";; After insertion
(let* ([qt (quadtree-build-auto '((1 1) (2 2)))]
       [qt2 (quadtree-insert qt 5 5 '(5 5))])
  (assert-equal 3 (quadtree-size qt2)))"))
    (available-modules (prelude sort heap))
    (hints ("Three cases in cond:"
            "empty? → 0"
            "leaf? → (length (quadtree-leaf-points tree))"
            "node? → sum of four recursive calls on ne, nw, se, sw children"))
    (reference-solution "(define (quadtree-size tree)
  (cond
    [(quadtree-empty? tree) 0]
    [(quadtree-leaf? tree) (length (quadtree-leaf-points tree))]
    [(quadtree-node? tree)
     (+ (quadtree-size (quadtree-ne tree))
        (quadtree-size (quadtree-nw tree))
        (quadtree-size (quadtree-se tree))
        (quadtree-size (quadtree-sw tree)))]))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement quadtree-range
  ;; ──────────────────────────────────────────────
  ;; Range query with AABB intersection pruning.

  (task
    (id "data/quadtree/range-impl")
    (module quadtree)
    (tier 0)
    (type implement)
    (description "Implement quadtree-range: find all points within a query bounding box. Three cases: (1) empty tree — return '(). (2) leaf — if the leaf's bounds don't intersect the query bounds, return '(). Otherwise, filter the leaf's points to keep only those inside the query box. (3) internal node — if the node's bounds don't intersect the query, return '(). Otherwise, recursively search all four children and append results. The intersection test (bounds-intersects?) provides spatial pruning: entire subtrees are skipped when their bounds don't overlap the query region.")
    (context "Available: prelude (append, filter), bounds-intersects?, bounds-contains?, quadtree-empty?, quadtree-leaf?, quadtree-node?, quadtree-leaf-bounds, quadtree-leaf-points, quadtree-bounds, quadtree-ne/nw/se/sw. Points in the quadtree are stored as (x y data) triples.")
    (before "(define (quadtree-range tree query-bounds)
  (doc 'type '(-> Quadtree Bounds (List Point)))
  (doc 'description \"Find all points within the query bounding box\")
  ;; TODO: three-case dispatch with AABB pruning
  '())")
    (test-file "lattice/data/test-quadtree.ss")
    (test-forms (";; Range query on built quadtree
(let* ([qt (quadtree-build-auto '((1 1) (5 5) (9 9) (3 3) (7 7)))]
       [result (quadtree-range qt (make-bounds 4 4 2 2))])
  (assert-equal 2 (length result)))"
                 ";; Empty result when no points in range
(let* ([qt (quadtree-build-auto '((0 0) (10 10)))]
       [result (quadtree-range qt (make-bounds 5 5 1 1))])
  (assert-equal 0 (length result)))"
                 ";; All points in range
(let* ([qt (quadtree-build-auto '((1 1) (2 2) (3 3)))]
       [result (quadtree-range qt (make-bounds 2 2 5 5))])
  (assert-equal 3 (length result)))"
                 ";; Empty tree
(assert-equal '() (quadtree-range quadtree-empty (make-bounds 0 0 10 10)))"))
    (available-modules (prelude sort heap))
    (hints ("Three cases:"
            "empty → '()"
            "leaf → if bounds don't intersect query, return '(). Otherwise filter points with (bounds-contains? query-bounds (car pt) (cadr pt))"
            "node → if bounds don't intersect query, return '(). Otherwise append results from all four children"
            "The bounds-intersects? check is the pruning step — it avoids searching subtrees that can't contain results"))
    (reference-solution "(define (quadtree-range tree query-bounds)
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
                 (quadtree-range (quadtree-sw tree) query-bounds)))]))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 4))

) ;; end tasks
