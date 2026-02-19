;;; convex-hull.sexp — Development tasks for lattice/geometry/convex-hull.ss
;;;
;;; Module: convex-hull (tier 0, depends on prelude, mesh-gen, sort)
;;; 491 lines, ~25 exported functions
;;; 2D convex hull algorithms: Graham scan, Quickhull, shoelace area,
;;; point-in-hull test, centroid, diameter, Minkowski operations.
;;;
;;; Git history:
;;;   9804fcec feat(geometry): Add 2D convex hull algorithms
;;;   8d2b82ac perf(geometry): Fix O(n^2)/O(n^3) complexity in hull utilities
;;;   d053e589 fix(geometry): Handle zero-length edges in polygon-edges
;;;   a6302df0 perf(geometry): Optimize convex hull operations

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement cross-product-2d
  ;; ──────────────────────────────────────────────
  ;; The fundamental geometric predicate. Everything in
  ;; computational geometry builds on orientation tests.

  (task
    (id "geometry/convex-hull/cross-product-2d-impl")
    (module convex-hull)
    (tier 0)
    (type implement)
    (description "Implement cross-product-2d: compute the 2D cross product (p2-p1) x (p3-p1). This gives the signed area of the parallelogram formed by the two vectors from p1 to p2 and p1 to p3. Positive means p3 is to the LEFT of the line p1->p2 (counterclockwise turn). Zero means collinear. Negative means clockwise turn. Extract x,y coordinates from each point using point2-x and point2-y, then compute: (x2-x1)*(y3-y1) - (y2-y1)*(x3-x1).")
    (context "Available: prelude (-, *), point2-x and point2-y (extract coordinates from a Point2), make-point2 (constructor). A Point2 is an opaque record with x and y fields.")
    (before "(define (cross-product-2d p1 p2 p3)
  (doc 'type '(-> Point2 Point2 Point2 Number))
  (doc 'description \"Compute cross product (p2-p1) x (p3-p1)\")
  ;; TODO: extract coords, compute signed area
  0)")
    (test-file "lattice/geometry/test-convex-hull.ss")
    (test-forms (";; CCW turn gives positive value
(let ([p1 (make-point2 0 0)]
      [p2 (make-point2 1 0)]
      [p3 (make-point2 0 1)])
  (assert-true (> (cross-product-2d p1 p2 p3) 0)))"
                 ";; CW turn gives negative value
(let ([p1 (make-point2 0 0)]
      [p2 (make-point2 0 1)]
      [p3 (make-point2 1 0)])
  (assert-true (< (cross-product-2d p1 p2 p3) 0)))"
                 ";; Collinear points give zero
(let ([p1 (make-point2 0 0)]
      [p2 (make-point2 1 0)]
      [p3 (make-point2 2 0)])
  (assert-equal 0 (cross-product-2d p1 p2 p3)))"
                 ";; Specific value: unit square diagonal
(let ([p1 (make-point2 0 0)]
      [p2 (make-point2 1 0)]
      [p3 (make-point2 0 1)])
  (assert-equal 1 (cross-product-2d p1 p2 p3)))"))
    (available-modules (prelude mesh-gen))
    (hints ("Extract all 6 coordinates: x1,y1 from p1, x2,y2 from p2, x3,y3 from p3"
            "The formula is: (x2-x1)*(y3-y1) - (y2-y1)*(x3-x1)"
            "Use let to bind all coordinates, then one arithmetic expression"))
    (reference-solution "(define (cross-product-2d p1 p2 p3)
  (let ([x1 (point2-x p1)] [y1 (point2-y p1)]
        [x2 (point2-x p2)] [y2 (point2-y p2)]
        [x3 (point2-x p3)] [y3 (point2-y p3)])
    (- (* (- x2 x1) (- y3 y1))
       (* (- y2 y1) (- x3 x1)))))")
    (alternatives ())
    (source-commit "9804fcec")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement find-lowest-point
  ;; ──────────────────────────────────────────────
  ;; Starting point for Graham scan. Classic fold-left pattern.

  (task
    (id "geometry/convex-hull/find-lowest-point-impl")
    (module convex-hull)
    (tier 0)
    (type implement)
    (description "Implement find-lowest-point: find the point with the lowest y-coordinate from a non-empty list of 2D points. If multiple points share the lowest y, choose the one with the smallest x-coordinate (leftmost). This is the starting point for Graham scan. Use fold-left over the list, comparing each point against the current best. The comparison logic: prefer lower y; on tie, prefer smaller x.")
    (context "Available: prelude (fold-left, car, cdr, <, =, and), point2-x and point2-y (extract coordinates from a Point2).")
    (before "(define (find-lowest-point points)
  (doc 'type '(-> (Listof Point2) Point2))
  (doc 'description \"Find the point with lowest y-coordinate (leftmost if tie)\")
  ;; TODO: fold-left to find minimum y, break ties by x
  (car points))")
    (test-file "lattice/geometry/test-convex-hull.ss")
    (test-forms (";; Single point returns itself
(let ([p (make-point2 3 5)])
  (assert-equal p (find-lowest-point (list p))))"
                 ";; Finds lowest y
(let ([pts (list (make-point2 0 5) (make-point2 1 0) (make-point2 2 3))])
  (let ([low (find-lowest-point pts)])
    (assert-equal 0 (point2-y low))))"
                 ";; Breaks ties by leftmost x
(let ([pts (list (make-point2 3 0) (make-point2 1 0) (make-point2 2 5))])
  (let ([low (find-lowest-point pts)])
    (assert-equal 1 (point2-x low))))"
                 ";; Works with negative coordinates
(let ([pts (list (make-point2 0 0) (make-point2 -1 -2) (make-point2 1 -2))])
  (let ([low (find-lowest-point pts)])
    (assert-equal -1 (point2-x low))))"))
    (available-modules (prelude mesh-gen))
    (hints ("Use fold-left with (car points) as initial value and (cdr points) as the list"
            "The lambda takes (best p) and returns the better of the two"
            "First check: (< (point2-y p) (point2-y best)) → p wins"
            "Tie-break: same y but (< (point2-x p) (point2-x best)) → p wins"
            "Otherwise: best stays"))
    (reference-solution "(define (find-lowest-point points)
  (fold-left
   (lambda (best p)
     (cond
       [(< (point2-y p) (point2-y best)) p]
       [(and (= (point2-y p) (point2-y best))
             (< (point2-x p) (point2-x best))) p]
       [else best]))
   (car points)
   (cdr points)))")
    (alternatives ())
    (source-commit "9804fcec")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement convex-hull-area
  ;; ──────────────────────────────────────────────
  ;; The shoelace formula — elegant polygon area computation.

  (task
    (id "geometry/convex-hull/area-impl")
    (module convex-hull)
    (tier 0)
    (type implement)
    (description "Implement convex-hull-area: compute the area of a convex polygon using the shoelace formula. Given hull vertices in order, the signed area is (1/2) * |sum over consecutive pairs (x_i * y_{i+1} - x_{i+1} * y_i)|. The last pair wraps around: the last vertex pairs with the first. If fewer than 3 vertices, area is 0. Loop through the vertex list, accumulating the cross terms. The final pair connects the last vertex back to the first. Take the absolute value and divide by 2.")
    (context "Available: prelude (null?, car, cdr, <, +, -, *, /, abs, length), point2-x, point2-y. The hull is a list of Point2 in order (CCW).")
    (before "(define (convex-hull-area hull)
  (doc 'type '(-> (Listof Point2) Number))
  (doc 'description \"Compute area of convex hull using shoelace formula\")
  ;; TODO: shoelace formula over consecutive vertex pairs
  0)")
    (test-file "lattice/geometry/test-convex-hull.ss")
    (test-forms (";; Unit square has area 1
(let ([hull (list (make-point2 0 0) (make-point2 1 0)
                  (make-point2 1 1) (make-point2 0 1))])
  (assert-true (< (abs (- (convex-hull-area hull) 1.0)) 1e-10)))"
                 ";; Right triangle with legs 3,4 has area 6
(let ([hull (list (make-point2 0 0) (make-point2 3 0) (make-point2 0 4))])
  (assert-true (< (abs (- (convex-hull-area hull) 6.0)) 1e-10)))"
                 ";; Degenerate: fewer than 3 points gives 0
(assert-equal 0 (convex-hull-area (list (make-point2 0 0) (make-point2 1 1))))"
                 ";; 2x2 square has area 4
(let ([hull (list (make-point2 0 0) (make-point2 2 0)
                  (make-point2 2 2) (make-point2 0 2))])
  (assert-true (< (abs (- (convex-hull-area hull) 4.0)) 1e-10)))"))
    (available-modules (prelude mesh-gen))
    (hints ("If (< (length hull) 3), return 0"
            "Save (car hull) as 'first' for the wrap-around"
            "Loop with a running sum. At each step, p1 = current vertex, p2 = next vertex"
            "For the last vertex, p2 wraps to first: (if (null? (cdr remaining)) first (cadr remaining))"
            "Accumulate: sum += x1*y2 - x2*y1"
            "Return (abs (/ sum 2))"))
    (reference-solution "(define (convex-hull-area hull)
  (if (< (length hull) 3)
      0
      (let ([first (car hull)])
        (abs
         (/ (let loop ([remaining hull] [sum 0])
              (if (null? remaining)
                  sum
                  (let* ([p1 (car remaining)]
                         [p2 (if (null? (cdr remaining)) first (cadr remaining))]
                         [x1 (point2-x p1)] [y1 (point2-y p1)]
                         [x2 (point2-x p2)] [y2 (point2-y p2)])
                    (loop (cdr remaining) (+ sum (- (* x1 y2) (* x2 y1)))))))
            2)))))")
    (alternatives ())
    (source-commit "9804fcec")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement point-in-convex-hull?
  ;; ──────────────────────────────────────────────
  ;; Edge-sign test: walk all edges, check all cross products.

  (task
    (id "geometry/convex-hull/point-in-hull-impl")
    (module convex-hull)
    (tier 0)
    (type implement)
    (description "Implement point-in-convex-hull?: test whether a point p lies inside or on the boundary of a convex hull. The algorithm walks each edge of the hull in order and computes the cross product of (edge direction) x (vector to point). For a convex hull in CCW order, if the point is inside, ALL cross products will be non-negative (>= 0). If any cross product is negative (using a small epsilon for numerical stability: < -1e-10), the point is outside. If fewer than 3 hull vertices, return #f. The last edge connects the final vertex back to the first.")
    (context "Available: prelude (null?, car, cdr, <, -, >=, length), cross-product-2d (computes signed area from 3 points), point2-x, point2-y, make-point2.")
    (before "(define (point-in-convex-hull? p hull)
  (doc 'type '(-> Point2 (Listof Point2) Boolean))
  (doc 'description \"Test if point is inside or on boundary of convex hull\")
  ;; TODO: check sign of cross product at every edge
  #f)")
    (test-file "lattice/geometry/test-convex-hull.ss")
    (test-forms (";; Interior point is inside
(let ([hull (list (make-point2 0 0) (make-point2 1 0)
                  (make-point2 1 1) (make-point2 0 1))])
  (assert-true (point-in-convex-hull? (make-point2 0.5 0.5) hull)))"
                 ";; Exterior point is outside
(let ([hull (list (make-point2 0 0) (make-point2 1 0)
                  (make-point2 1 1) (make-point2 0 1))])
  (assert-false (point-in-convex-hull? (make-point2 2.0 0.5) hull)))"
                 ";; Vertex is on boundary (inside)
(let ([hull (list (make-point2 0 0) (make-point2 1 0)
                  (make-point2 1 1) (make-point2 0 1))])
  (assert-true (point-in-convex-hull? (make-point2 0 0) hull)))"
                 ";; Too few vertices returns false
(assert-false (point-in-convex-hull? (make-point2 0 0)
  (list (make-point2 0 0) (make-point2 1 1))))"))
    (available-modules (prelude mesh-gen sort))
    (hints ("If (< (length hull) 3), return #f"
            "Save (car hull) as 'first' for wrap-around"
            "Loop through vertices: p1 = current, p2 = next (wrapping last to first)"
            "Compute cross = (cross-product-2d p1 p2 p) at each edge"
            "If cross < -1e-10, return #f immediately (point is outside)"
            "If loop completes without returning #f, return #t"))
    (reference-solution "(define (point-in-convex-hull? p hull)
  (if (< (length hull) 3)
      #f
      (let ([first (car hull)])
        (let loop ([remaining hull])
          (if (null? remaining)
              #t
              (let* ([p1 (car remaining)]
                     [p2 (if (null? (cdr remaining)) first (cadr remaining))]
                     [cross (cross-product-2d p1 p2 p)])
                (if (< cross (- 1e-10))
                    #f
                    (loop (cdr remaining)))))))))")
    (alternatives ())
    (source-commit "9804fcec")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement quickhull-recurse
  ;; ──────────────────────────────────────────────
  ;; The recursive heart of Quickhull — divide and conquer.

  (task
    (id "geometry/convex-hull/quickhull-recurse-impl")
    (module convex-hull)
    (tier 0)
    (type implement)
    (description "Implement quickhull-recurse: the recursive step of the Quickhull algorithm. Given a set of points and a line segment from a to b, find all hull points between a and b. Algorithm: (1) Filter points that are strictly above (left of) the line a->b using points-above-line. (2) If no points are above, return empty list (base case). (3) Otherwise, find the point c that is furthest from line a->b using find-furthest-from-line. (4) Recursively find hull points between a and c, then between c and b. (5) Return: (append left-hull (list c) right-hull). This is analogous to quicksort's partition step but in 2D.")
    (context "Available: prelude (null?, list, append), points-above-line (filters points strictly left of line a->b), find-furthest-from-line (finds point maximizing distance from line a->b). Both take (points a b) as arguments.")
    (before "(define (quickhull-recurse points a b)
  (doc 'type '(-> (Listof Point2) Point2 Point2 (Listof Point2)))
  (doc 'description \"Recursive step: find hull points between a and b\")
  ;; TODO: filter above, find furthest, recurse on both sides
  '())")
    (test-file "lattice/geometry/test-convex-hull.ss")
    (test-forms (";; Square with interior: upper hull between left and right
(let* ([pts (list (make-point2 0 0) (make-point2 1 0)
                  (make-point2 0.5 0.5) (make-point2 1 1) (make-point2 0 1))]
       [result (quickhull-recurse pts (make-point2 0 0) (make-point2 1 0))])
  ;; Should find upper hull points between (0,0) and (1,0)
  (assert-true (>= (length result) 1)))"
                 ";; No points above the line → empty result
(let ([pts (list (make-point2 0.5 -1))]
      [a (make-point2 0 0)]
      [b (make-point2 1 0)])
  (assert-equal '() (quickhull-recurse pts a b)))"
                 ";; Quickhull produces same vertex set as Graham scan
(let* ([pts (list (make-point2 0 0) (make-point2 10 0)
                  (make-point2 10 10) (make-point2 0 10)
                  (make-point2 5 5) (make-point2 3 2))]
       [qh (quickhull pts)]
       [gs (graham-scan pts)])
  (assert-equal (length gs) (length qh)))"))
    (available-modules (prelude mesh-gen sort))
    (hints ("Step 1: (let ([above (points-above-line points a b)]) ...)"
            "Step 2: (if (null? above) '() ...) — base case"
            "Step 3: (let ([c (find-furthest-from-line above a b)]) ...)"
            "Step 4: Recurse on both halves with above (not original points)"
            "Return: (append (quickhull-recurse above a c) (list c) (quickhull-recurse above c b))"))
    (reference-solution "(define (quickhull-recurse points a b)
  (let ([above (points-above-line points a b)])
    (if (null? above)
        '()
        (let ([c (find-furthest-from-line above a b)])
          (append (quickhull-recurse above a c)
                  (list c)
                  (quickhull-recurse above c b))))))")
    (alternatives ())
    (source-commit "9804fcec")
    (difficulty 4))

) ;; end tasks
