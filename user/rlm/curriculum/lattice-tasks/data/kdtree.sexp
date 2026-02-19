;;; kdtree.sexp — Development tasks for lattice/data/kdtree.ss
;;;
;;; Module: kdtree (tier 0, depends on prelude, sort, heap)
;;; 558 lines, ~25 exported functions
;;; K-dimensional tree for spatial queries: nearest neighbor, k-NN,
;;; range queries, radius queries. Points as lists of coordinates.
;;;
;;; Git history:
;;;   596c1d4d feat(module): migrate data/, optics/, parallel/ to require
;;;   328ecf8f fix(spatial): Address Gemini QA feedback for kdtree/quadtree delete
;;;   c0105614 feat(spatial): Add delete operations for kdtree and quadtree

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement point-distance-sq
  ;; ──────────────────────────────────────────────
  ;; Foundation for all spatial queries. Squared distance avoids sqrt.

  (task
    (id "data/kdtree/distance-sq-impl")
    (module kdtree)
    (tier 0)
    (type implement)
    (description "Implement point-distance-sq: compute the squared Euclidean distance between two points. A point is a list of coordinates, e.g. '(3 4) for 2D or '(1 2 3) for 3D. The squared distance is the sum of (a_i - b_i)^2 for each coordinate i. Use a tail-recursive loop with an accumulator: walk both lists in parallel, computing the squared difference at each position and adding to a running sum. Return the sum when both lists are exhausted. Squared distance is preferred over actual distance because it avoids an expensive sqrt call and preserves ordering (if d1^2 < d2^2, then d1 < d2).")
    (context "Available: prelude (null?, car, cdr, +, -, *). Points are plain lists of numbers, all the same length. No need for sqrt.")
    (before "(define (point-distance-sq p1 p2)
  (doc 'type '(-> Point Point Num))
  (doc 'description \"Squared Euclidean distance between points\")
  ;; TODO: tail-recursive sum of squared differences
  0)")
    (test-file "lattice/data/test-kdtree.ss")
    (test-forms ("(assert-equal 0 (point-distance-sq '(0 0) '(0 0)))"
                 "(assert-equal 2 (point-distance-sq '(0 0) '(1 1)))"
                 "(assert-equal 25 (point-distance-sq '(0 0) '(3 4)))"
                 "(assert-equal 3 (point-distance-sq '(1 1 1) '(2 2 2)))"
                 ";; Works for higher dimensions
(assert-equal 4 (point-distance-sq '(0 0 0 0) '(1 1 1 1)))"))
    (available-modules (prelude))
    (hints ("Use a named let with three parameters: a (rest of p1), b (rest of p2), sum (accumulator)"
            "Base case: (null? a) → return sum"
            "Each step: d = (- (car a) (car b)), add (* d d) to sum"
            "Tail-recursive: the recursive call is in tail position"))
    (reference-solution "(define (point-distance-sq p1 p2)
  (let loop ([a p1] [b p2] [sum 0])
    (if (null? a)
        sum
        (let ([d (- (car a) (car b))])
          (loop (cdr a) (cdr b) (+ sum (* d d)))))))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement point-in-box?
  ;; ──────────────────────────────────────────────
  ;; Geometric predicate for AABB containment.

  (task
    (id "data/kdtree/point-in-box-impl")
    (module kdtree)
    (tier 0)
    (type implement)
    (description "Implement point-in-box?: check if a point lies within an axis-aligned bounding box defined by min-pt and max-pt (inclusive). A point (x1, x2, ..., xk) is in the box if for every coordinate i: min_i <= x_i <= max_i. Use a tail-recursive loop that walks all three lists in parallel. If any coordinate is out of range (< min or > max), return #f immediately. If all coordinates pass, return #t.")
    (context "Available: prelude (null?, car, cdr, <, >, or, and). All three lists (point, min-pt, max-pt) have the same length. The box is inclusive on both ends.")
    (before "(define (point-in-box? point min-pt max-pt)
  (doc 'type '(-> Point Point Point Boolean))
  (doc 'description \"Check if point is within axis-aligned bounding box [min, max] (inclusive)\")
  ;; TODO: walk all three lists, check each coordinate
  #f)")
    (test-file "lattice/data/test-kdtree.ss")
    (test-forms ("(assert-true (point-in-box? '(5 5) '(0 0) '(10 10)))"
                 "(assert-true (point-in-box? '(0 0) '(0 0) '(10 10)))"
                 "(assert-true (point-in-box? '(10 10) '(0 0) '(10 10)))"
                 "(assert-false (point-in-box? '(11 5) '(0 0) '(10 10)))"
                 "(assert-false (point-in-box? '(-1 5) '(0 0) '(10 10)))"
                 ";; 3D box
(assert-true (point-in-box? '(1 2 3) '(0 0 0) '(5 5 5)))"))
    (available-modules (prelude))
    (hints ("Named let with three list pointers: pt, lo, hi"
            "Base case: (null? pt) → #t (all coordinates checked)"
            "Early exit: if (or (< (car pt) (car lo)) (> (car pt) (car hi))) → #f"
            "Otherwise recurse on (cdr pt) (cdr lo) (cdr hi)"))
    (reference-solution "(define (point-in-box? point min-pt max-pt)
  (let loop ([pt point] [lo min-pt] [hi max-pt])
    (cond
      [(null? pt) #t]
      [(or (< (car pt) (car lo)) (> (car pt) (car hi))) #f]
      [else (loop (cdr pt) (cdr lo) (cdr hi))])))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement kdtree-insert
  ;; ──────────────────────────────────────────────
  ;; BST-style insertion with alternating split axes.

  (task
    (id "data/kdtree/insert-impl")
    (module kdtree)
    (tier 0)
    (type implement)
    (description "Implement kdtree-insert: insert a point into an existing k-d tree. Like BST insertion but the comparison axis alternates at each level: at depth d, compare on axis (modulo d k) where k is the dimensionality. If the point's coordinate on the split axis is less than the node's, go left; otherwise go right. When you reach an empty position, create a new leaf node. Parameters: tree (the k-d tree), point (list of coordinates), k (dimensionality). Use a helper kdtree-insert-rec that tracks depth. Create nodes with (kdtree-node point left right depth).")
    (context "Available: prelude (modulo, <), kdtree-empty (the empty tree sentinel), kdtree-empty? (check if empty), kdtree-node (constructor: point left right depth), kdtree-point, kdtree-left, kdtree-right, kdtree-depth, point-coord (gets coordinate at axis).")
    (before "(define (kdtree-insert tree point k)
  (doc 'type '(-> KDTree Point Nat KDTree))
  (doc 'description \"Insert a point into existing tree. O(log n) average.\")
  ;; TODO: BST-style insertion with alternating split axis
  tree)")
    (test-file "lattice/data/test-kdtree.ss")
    (test-forms (";; Insert into empty tree
(let ([tree (kdtree-insert kdtree-empty '(5 5) 2)])
  (assert-false (kdtree-empty? tree))
  (assert-equal '(5 5) (kdtree-point tree)))"
                 ";; Insert into existing tree
(let* ([tree (kdtree-build '((0 0) (10 10)))]
       [tree2 (kdtree-insert tree '(5 5) 2)])
  (assert-equal 3 (kdtree-size tree2))
  (assert-equal '(5 5) (kdtree-nearest tree2 '(5 5))))"
                 ";; Multiple insertions
(let* ([tree kdtree-empty]
       [tree (kdtree-insert tree '(3 3) 2)]
       [tree (kdtree-insert tree '(1 1) 2)]
       [tree (kdtree-insert tree '(7 7) 2)])
  (assert-equal 3 (kdtree-size tree)))"))
    (available-modules (prelude sort heap))
    (hints ("Write a helper (kdtree-insert-rec tree point k depth)"
            "Base case: if tree is empty, create leaf: (kdtree-node point kdtree-empty kdtree-empty depth)"
            "Compute axis = (modulo depth k)"
            "Compare point-coord of the new point vs node point on this axis"
            "If less: recurse left, keeping right unchanged. Otherwise: recurse right, keeping left unchanged."
            "Rebuild the node with (kdtree-node node-point new-left new-right depth)"))
    (reference-solution "(define (kdtree-insert tree point k)
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
                        depth)))))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement kdtree-range
  ;; ──────────────────────────────────────────────
  ;; Range query with spatial pruning.

  (task
    (id "data/kdtree/range-impl")
    (module kdtree)
    (tier 0)
    (type implement)
    (description "Implement kdtree-range: find all points within an axis-aligned bounding box [min-point, max-point]. The key optimization is spatial pruning: at each node, the split axis and coordinate tell you which subtrees might contain results. If the node's coordinate on the split axis is less than the minimum bound, only the right subtree can contain results. If it's greater than the maximum bound, only the left subtree can. Otherwise, search both. For each visited node, also check if its point is inside the box using point-in-box?. Return the list of matching points.")
    (context "Available: prelude (append, list, modulo, <=, >=), point-in-box? (checks if point is in AABB), point-coord (gets coordinate at axis), kdtree-empty?, kdtree-point, kdtree-left, kdtree-right, kdtree-depth, point-dimension.")
    (before "(define (kdtree-range tree min-point max-point)
  (doc 'type '(-> KDTree Point Point (List Point)))
  (doc 'description \"Find all points within axis-aligned bounding box [min, max].\")
  ;; TODO: recursive search with spatial pruning on split axis
  '())")
    (test-file "lattice/data/test-kdtree.ss")
    (test-forms (";; Range that includes subset of points
(let ([tree (kdtree-build '((1 1) (5 5) (9 9) (3 3) (7 7)))])
  (let ([result (kdtree-range tree '(2 2) '(6 6))])
    (assert-equal 2 (length result))
    (assert-true (not (not (member '(3 3) result))))
    (assert-true (not (not (member '(5 5) result))))))"
                 ";; Empty result
(let ([tree (kdtree-build '((0 0) (10 10)))])
  (assert-equal 0 (length (kdtree-range tree '(4 4) '(6 6)))))"
                 ";; All points in range
(let ([tree (kdtree-build '((1 1) (2 2) (3 3)))])
  (assert-equal 3 (length (kdtree-range tree '(0 0) '(10 10)))))"
                 ";; Empty tree
(assert-equal '() (kdtree-range kdtree-empty '(0 0) '(10 10)))"))
    (available-modules (prelude sort heap))
    (hints ("Write a helper kdtree-range-rec that takes tree, min-pt, max-pt, k (dimensionality)"
            "Base case: empty tree → '()"
            "At each node: get axis = (modulo depth k), node-coord on that axis"
            "Decide which subtrees to search: search-left? if min-coord <= node-coord, search-right? if max-coord >= node-coord"
            "Check if current node's point is in the box with point-in-box?"
            "Append results: (if in-range? (list node-point) '()) plus left results plus right results"))
    (reference-solution "(define (kdtree-range tree min-point max-point)
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
             [in-range? (point-in-box? node-point min-pt max-pt)]
             [search-left? (<= min-coord node-coord)]
             [search-right? (>= max-coord node-coord)])
        (append
         (if in-range? (list node-point) '())
         (if search-left?
             (kdtree-range-rec (kdtree-left tree) min-pt max-pt k)
             '())
         (if search-right?
             (kdtree-range-rec (kdtree-right tree) min-pt max-pt k)
             '())))))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement kdtree-fold
  ;; ──────────────────────────────────────────────
  ;; Tree traversal — tests understanding of the tree structure.

  (task
    (id "data/kdtree/fold-impl")
    (module kdtree)
    (tier 0)
    (type implement)
    (description "Implement kdtree-fold: a left fold over all points in the tree using in-order traversal (left subtree, then node, then right subtree). The fold function (proc acc point) is called for each point, threading an accumulator through. This follows the fold-left convention where the accumulator is the first argument. For an empty tree, return the initial value unchanged. For a non-empty tree: first fold over the left subtree, then apply proc to the accumulated result and the current node's point, then fold over the right subtree with that result.")
    (context "Available: prelude, kdtree-empty?, kdtree-point, kdtree-left, kdtree-right. The proc signature is (-> Acc Point Acc) — accumulator first, point second.")
    (before "(define (kdtree-fold proc init tree)
  (doc 'type '(-> (-> a Point a) a KDTree a))
  (doc 'description \"Left fold over all points in tree (in-order traversal)\")
  ;; TODO: in-order traversal threading accumulator
  init)")
    (test-file "lattice/data/test-kdtree.ss")
    (test-forms (";; Count points
(let ([tree (kdtree-build '((1 2) (3 4) (5 6)))])
  (assert-equal 3 (kdtree-fold (lambda (acc pt) (+ 1 acc)) 0 tree)))"
                 ";; Sum x coordinates
(let ([tree (kdtree-build '((1 2) (3 4) (5 6)))])
  (assert-equal 9 (kdtree-fold (lambda (acc pt) (+ (car pt) acc)) 0 tree)))"
                 ";; Collect all points
(let ([tree (kdtree-build '((1 2) (3 4) (5 6)))])
  (assert-equal 3 (length (kdtree-fold (lambda (acc pt) (cons pt acc)) '() tree))))"
                 ";; Empty tree
(assert-equal 42 (kdtree-fold (lambda (acc pt) (+ 1 acc)) 42 kdtree-empty))"))
    (available-modules (prelude sort heap))
    (hints ("Base case: empty tree → return init"
            "In-order traversal: first fold over left subtree to get acc1"
            "Apply proc to acc1 and current node's point to get acc2"
            "Fold over right subtree starting from acc2"
            "The recursion is: (kdtree-fold proc (proc (kdtree-fold proc init left) point) right)"))
    (reference-solution "(define (kdtree-fold proc init tree)
  (if (kdtree-empty? tree)
      init
      (kdtree-fold proc
                   (proc (kdtree-fold proc init (kdtree-left tree))
                         (kdtree-point tree))
                   (kdtree-right tree))))")
    (alternatives ())
    (source-commit "596c1d4d")
    (difficulty 3))

) ;; end tasks
