;;; lattice/geometry/convex-hull.ss --- 2D convex hull algorithms
;;; @module convex-hull
;;; @requires prelude geometry/mesh-gen sort

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'mesh-gen)
(require 'sort)

(doc 'module 'convex-hull)
(doc 'description "Convex hull algorithms: Graham scan, Quickhull, and utilities")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Section: Geometric Predicates for Convex Hull
;;; ============================================================

(doc 'section 'predicates)

(doc cross-product-2d 'type '(-> Point2 Point2 Point2 Number))
(doc cross-product-2d 'description
     "Compute cross product (p2-p1) × (p3-p1).
      Positive = p3 is left of p1→p2 (CCW turn).
      Zero = collinear.
      Negative = p3 is right of p1→p2 (CW turn).")
(define (cross-product-2d p1 p2 p3)
  (doc 'export #t)
  (let ([x1 (point2-x p1)] [y1 (point2-y p1)]
        [x2 (point2-x p2)] [y2 (point2-y p2)]
        [x3 (point2-x p3)] [y3 (point2-y p3)])
    (- (* (- x2 x1) (- y3 y1))
       (* (- y2 y1) (- x3 x1)))))

(doc ccw? 'type '(-> Point2 Point2 Point2 Boolean))
(doc ccw? 'description "True if p1→p2→p3 makes a counterclockwise turn")
(define (ccw? p1 p2 p3)
  (doc 'export #t)
  (> (cross-product-2d p1 p2 p3) 0))

(doc collinear? 'type '(-> Point2 Point2 Point2 Boolean))
(doc collinear? 'description "True if p1, p2, p3 are collinear (within epsilon)")
(define (collinear? p1 p2 p3)
  (doc 'export #t)
  (< (abs (cross-product-2d p1 p2 p3)) 1e-10))

(doc point2-distance-sq 'type '(-> Point2 Point2 Number))
(doc point2-distance-sq 'description "Squared Euclidean distance between two 2D points")
(define (point2-distance-sq p1 p2)
  (doc 'export #t)
  (let ([dx (- (point2-x p2) (point2-x p1))]
        [dy (- (point2-y p2) (point2-y p1))])
    (+ (* dx dx) (* dy dy))))

;;; ============================================================
;;; Section: Graham Scan Algorithm
;;; ============================================================

(doc 'section 'graham-scan)

(doc find-lowest-point 'type '(-> (Listof Point2) Point2))
(doc find-lowest-point 'description
     "Find the point with lowest y-coordinate (leftmost if tie)")
(define (find-lowest-point points)
  (doc 'export #t)
  (fold-left
   (lambda (best p)
     (cond
       [(< (point2-y p) (point2-y best)) p]
       [(and (= (point2-y p) (point2-y best))
             (< (point2-x p) (point2-x best))) p]
       [else best]))
   (car points)
   (cdr points)))

(doc polar-angle 'type '(-> Point2 Point2 Number))
(doc polar-angle 'description "Compute polar angle of p relative to origin")
(define (polar-angle origin p)
  (doc 'export #t)
  (atan (- (point2-y p) (point2-y origin))
        (- (point2-x p) (point2-x origin))))

(doc sort-by-polar-angle 'type '(-> Point2 (Listof Point2) (Listof Point2)))
(doc sort-by-polar-angle 'description
     "Sort points by polar angle around origin. For collinear points, keep furthest.")
(define (sort-by-polar-angle origin points)
  (doc 'export #t)
  ;; Remove origin from points
  (let* ([others (filter (lambda (p)
                           (not (and (= (point2-x p) (point2-x origin))
                                     (= (point2-y p) (point2-y origin)))))
                         points)]
         ;; Sort by polar angle
         [sorted (sort-by
                  (lambda (a b)
                    (let ([angle-a (polar-angle origin a)]
                          [angle-b (polar-angle origin b)])
                      (if (< (abs (- angle-a angle-b)) 1e-10)
                          ;; Collinear: keep point further from origin
                          (> (point2-distance-sq origin a)
                             (point2-distance-sq origin b))
                          (< angle-a angle-b))))
                  others)])
    ;; Remove collinear points except the furthest
    (remove-collinear-except-furthest origin sorted)))

(doc remove-collinear-except-furthest 'type '(-> Point2 (Listof Point2) (Listof Point2)))
(doc remove-collinear-except-furthest 'description
     "From a polar-angle-sorted list, remove collinear points except the furthest from origin")
(define (remove-collinear-except-furthest origin sorted)
  (doc 'export #t)
  (if (null? sorted)
      '()
      (let loop ([remaining (cdr sorted)]
                 [current (car sorted)]
                 [result '()])
        (if (null? remaining)
            (reverse (cons current result))
            (let ([next (car remaining)])
              (if (< (abs (- (polar-angle origin current)
                             (polar-angle origin next)))
                     1e-10)
                  ;; Collinear: keep the one further from origin
                  (if (> (point2-distance-sq origin next)
                         (point2-distance-sq origin current))
                      (loop (cdr remaining) next result)
                      (loop (cdr remaining) current result))
                  ;; Not collinear: keep current, move to next
                  (loop (cdr remaining) next (cons current result))))))))

(doc graham-scan 'type '(-> (Listof Point2) (Listof Point2)))
(doc graham-scan 'description
     "Compute convex hull using Graham scan algorithm.
      Returns vertices in counterclockwise order.
      O(n log n) time complexity.")
(define (graham-scan points)
  (doc 'export #t)
  (cond
    ;; Edge cases
    [(< (length points) 3) points]
    [else
     (let* ([p0 (find-lowest-point points)]
            [sorted (sort-by-polar-angle p0 points)])
       (if (< (length sorted) 2)
           (cons p0 sorted)
           ;; Main Graham scan loop
           (let loop ([remaining (cdr sorted)]
                      [stack (list (car sorted) p0)])  ; Stack grows backward
             (if (null? remaining)
                 (reverse stack)
                 (let ([next (car remaining)])
                   ;; Pop while we make a clockwise turn
                   (let pop-loop ([s stack])
                     (if (and (>= (length s) 2)
                              (not (ccw? (cadr s) (car s) next)))
                         (pop-loop (cdr s))
                         (loop (cdr remaining) (cons next s)))))))))]))

;;; ============================================================
;;; Section: Quickhull Algorithm
;;; ============================================================

(doc 'section 'quickhull)

(doc point-line-distance 'type '(-> Point2 Point2 Point2 Number))
(doc point-line-distance 'description
     "Signed distance from point p to line through a and b.
      Positive = left of line (when facing from a to b).")
(define (point-line-distance p a b)
  (doc 'export #t)
  (let ([dx (- (point2-x b) (point2-x a))]
        [dy (- (point2-y b) (point2-y a))]
        [px (- (point2-x p) (point2-x a))]
        [py (- (point2-y p) (point2-y a))])
    (/ (- (* dx py) (* dy px))
       (sqrt (+ (* dx dx) (* dy dy))))))

(doc find-extreme-points 'type '(-> (Listof Point2) (values Point2 Point2)))
(doc find-extreme-points 'description "Find leftmost and rightmost points")
(define (find-extreme-points points)
  (doc 'export #t)
  (let loop ([remaining (cdr points)]
             [left (car points)]
             [right (car points)])
    (if (null? remaining)
        (values left right)
        (let ([p (car remaining)])
          (loop (cdr remaining)
                (if (< (point2-x p) (point2-x left)) p left)
                (if (> (point2-x p) (point2-x right)) p right))))))

(doc points-above-line 'type '(-> (Listof Point2) Point2 Point2 (Listof Point2)))
(doc points-above-line 'description "Filter points that are strictly left of (above) line a→b")
(define (points-above-line points a b)
  (doc 'export #t)
  (filter (lambda (p) (> (cross-product-2d a b p) 1e-10)) points))

(doc find-furthest-from-line 'type '(-> (Listof Point2) Point2 Point2 Point2))
(doc find-furthest-from-line 'description "Find point furthest from line a→b")
(define (find-furthest-from-line points a b)
  (doc 'export #t)
  ;; Use cross product magnitude directly to avoid sqrt.
  ;; cross = (b-a) × (p-a) = distance * |b-a|
  ;; Since |b-a| is constant for all comparisons, comparing |cross| works.
  (let ([dx (- (point2-x b) (point2-x a))]
        [dy (- (point2-y b) (point2-y a))])
    (fold-left
     (lambda (best p)
       (let ([cross-best (abs (- (* dx (- (point2-y best) (point2-y a)))
                                 (* dy (- (point2-x best) (point2-x a)))))]
             [cross-p (abs (- (* dx (- (point2-y p) (point2-y a)))
                              (* dy (- (point2-x p) (point2-x a)))))])
         (if (> cross-p cross-best) p best)))
     (car points)
     (cdr points))))

(doc quickhull-recurse 'type '(-> (Listof Point2) Point2 Point2 (Listof Point2)))
(doc quickhull-recurse 'description "Recursive step: find hull points between a and b")
(define (quickhull-recurse points a b)
  (doc 'export #t)
  (let ([above (points-above-line points a b)])
    (if (null? above)
        '()
        (let ([c (find-furthest-from-line above a b)])
          (append (quickhull-recurse above a c)
                  (list c)
                  (quickhull-recurse above c b))))))

(doc quickhull 'type '(-> (Listof Point2) (Listof Point2)))
(doc quickhull 'description
     "Compute convex hull using Quickhull algorithm.
      Returns vertices in counterclockwise order.
      O(n log n) expected, O(n²) worst case.")
(define (quickhull points)
  (doc 'export #t)
  (cond
    [(< (length points) 3) points]
    [else
     (let-values ([(left right) (find-extreme-points points)])
       (if (and (= (point2-x left) (point2-x right))
                (= (point2-y left) (point2-y right)))
           ;; All points coincide
           (list left)
           ;; Build upper and lower hulls
           (let ([upper (quickhull-recurse points left right)]
                 [lower (quickhull-recurse points right left)])
             (append (list left)
                     upper
                     (list right)
                     lower))))]))

;;; ============================================================
;;; Section: Convex Hull Utilities
;;; ============================================================

(doc 'section 'utilities)

(doc convex-hull 'type '(-> (Listof Point2) (Listof Point2)))
(doc convex-hull 'description
     "Compute 2D convex hull (default algorithm: Graham scan).
      Returns vertices in counterclockwise order.")
(define (convex-hull points)
  (doc 'export #t)
  (graham-scan points))

(doc convex-hull-area 'type '(-> (Listof Point2) Number))
(doc convex-hull-area 'description
     "Compute area of convex hull using shoelace formula. O(n) time.")
(define (convex-hull-area hull)
  (doc 'export #t)
  (if (< (length hull) 3)
      0
      (let ([first (car hull)])
        ;; Sliding window: iterate pairs (p1, p2) around the polygon
        (abs
         (/ (let loop ([remaining hull] [sum 0])
              (if (null? remaining)
                  sum
                  (let* ([p1 (car remaining)]
                         [p2 (if (null? (cdr remaining)) first (cadr remaining))]
                         [x1 (point2-x p1)] [y1 (point2-y p1)]
                         [x2 (point2-x p2)] [y2 (point2-y p2)])
                    (loop (cdr remaining) (+ sum (- (* x1 y2) (* x2 y1)))))))
            2)))))

(doc convex-hull-perimeter 'type '(-> (Listof Point2) Number))
(doc convex-hull-perimeter 'description "Compute perimeter of convex hull. O(n) time.")
(define (convex-hull-perimeter hull)
  (doc 'export #t)
  (if (< (length hull) 2)
      0
      (let ([first (car hull)])
        (let loop ([remaining hull] [sum 0])
          (if (null? remaining)
              sum
              (let* ([p1 (car remaining)]
                     [p2 (if (null? (cdr remaining)) first (cadr remaining))])
                (loop (cdr remaining) (+ sum (sqrt (point2-distance-sq p1 p2))))))))))

(doc point-in-convex-hull? 'type '(-> Point2 (Listof Point2) Boolean))
(doc point-in-convex-hull? 'description
     "Test if point is inside or on boundary of convex hull.
      Uses sign of cross products with all edges. O(n) time.")
(define (point-in-convex-hull? p hull)
  (doc 'export #t)
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
                    #f  ; Point is to the right of edge (outside)
                    (loop (cdr remaining)))))))))

(doc convex-hull-centroid 'type '(-> (Listof Point2) Point2))
(doc convex-hull-centroid 'description
     "Compute centroid (center of mass) of convex hull polygon. O(n) time.")
(define (convex-hull-centroid hull)
  (doc 'export #t)
  (if (null? hull)
      (make-point2 0 0)
      (let ([first (car hull)]
            [n (length hull)])
        (let loop ([remaining hull] [cx 0] [cy 0] [a 0])
          (if (null? remaining)
              (if (< (abs a) 1e-10)
                  ;; Degenerate case: use simple average
                  (make-point2 (/ (fold-left + 0 (map point2-x hull)) n)
                               (/ (fold-left + 0 (map point2-y hull)) n))
                  (make-point2 (/ cx (* 3 a))
                               (/ cy (* 3 a))))
              (let* ([p1 (car remaining)]
                     [p2 (if (null? (cdr remaining)) first (cadr remaining))]
                     [x1 (point2-x p1)] [y1 (point2-y p1)]
                     [x2 (point2-x p2)] [y2 (point2-y p2)]
                     [cross (- (* x1 y2) (* x2 y1))])
                (loop (cdr remaining)
                      (+ cx (* (+ x1 x2) cross))
                      (+ cy (* (+ y1 y2) cross))
                      (+ a cross))))))))

(doc convex-hull-diameter 'type '(-> (Listof Point2) Number))
(doc convex-hull-diameter 'description
     "Compute diameter of convex hull (max distance between any two vertices).
      O(n²) time using vector for O(1) access. Rotating calipers would be O(n).")
(define (convex-hull-diameter hull)
  (doc 'export #t)
  (if (< (length hull) 2)
      0
      ;; Convert to vector for O(1) random access, making this O(n²) not O(n³)
      (let* ([v (list->vector hull)]
             [n (vector-length v)])
        (let outer ([i 0] [max-dist 0])
          (if (>= i n)
              (sqrt max-dist)
              (let inner ([j (+ i 1)] [max-d max-dist])
                (if (>= j n)
                    (outer (+ i 1) max-d)
                    (let ([d (point2-distance-sq (vector-ref v i)
                                                  (vector-ref v j))])
                      (inner (+ j 1) (max max-d d))))))))))

;;; ============================================================
;;; Section: Extreme Points and Support
;;; ============================================================

(doc 'section 'extreme-points)

(doc extreme-point 'type '(-> (Listof Point2) Point2 Point2))
(doc extreme-point 'description
     "Find extreme point in given direction (unit vector).
      Returns the point p maximizing (p · direction).")
(define (extreme-point points direction)
  (doc 'export #t)
  (let ([dx (point2-x direction)]
        [dy (point2-y direction)])
    (fold-left
     (lambda (best p)
       (let ([dot-best (+ (* (point2-x best) dx) (* (point2-y best) dy))]
             [dot-p (+ (* (point2-x p) dx) (* (point2-y p) dy))])
         (if (> dot-p dot-best) p best)))
     (car points)
     (cdr points))))

(doc support-function 'type '(-> (Listof Point2) Point2 Number))
(doc support-function 'description
     "Support function h(d) = max{p · d : p ∈ hull}.
      Fundamental for Minkowski operations.")
(define (support-function points direction)
  (doc 'export #t)
  (let* ([dx (point2-x direction)]
         [dy (point2-y direction)]
         [ext (extreme-point points direction)])
    (+ (* (point2-x ext) dx)
       (* (point2-y ext) dy))))

;;; ============================================================
;;; Section: Minkowski Operations
;;; ============================================================

(doc 'section 'minkowski)

(doc minkowski-sum 'type '(-> (Listof Point2) (Listof Point2) (Listof Point2)))
(doc minkowski-sum 'description
     "Compute Minkowski sum of two convex polygons.
      A ⊕ B = {a + b : a ∈ A, b ∈ B}.
      Result is convex if inputs are convex.
      O(n + m) for convex inputs.")
(define (minkowski-sum hull-a hull-b)
  (doc 'export #t)
  (if (or (null? hull-a) (null? hull-b))
      '()
      ;; For two convex polygons in CCW order, merge their edge directions
      (let* ([edges-a (sort-by (lambda (x y) (< (cdr x) (cdr y)))
                                 (polygon-edges hull-a))]
             [edges-b (sort-by (lambda (x y) (< (cdr x) (cdr y)))
                                 (polygon-edges hull-b))]
             [merged (merge-edges-ccw edges-a edges-b)]
             ;; Start from sum of bottom-left vertices
             [start-a (find-lowest-point hull-a)]
             [start-b (find-lowest-point hull-b)]
             [start (make-point2 (+ (point2-x start-a) (point2-x start-b))
                                 (+ (point2-y start-a) (point2-y start-b)))])
        (build-polygon-from-edges start merged))))

(doc normalize-angle 'type '(-> Number Number))
(doc normalize-angle 'description "Normalize angle to [0, 2π)")
(define (normalize-angle a)
  (doc 'export #t)
  (let ([two-pi (* 2 3.141592653589793)])
    (let loop ([angle a])
      (cond
        [(< angle 0) (loop (+ angle two-pi))]
        [(>= angle two-pi) (loop (- angle two-pi))]
        [else angle]))))

(doc polygon-edges 'type '(-> (Listof Point2) (Listof (Pair Point2 Number))))
(doc polygon-edges 'description "Extract edge vectors with their polar angles (normalized to [0, 2π)).
                                 Filters out zero-length edges to avoid atan(0,0) errors.")
(define (polygon-edges hull)
  (doc 'export #t)
  (if (null? hull)
      '()
      (let ([n (length hull)])
        (let loop ([i 0] [result '()])
          (if (>= i n)
              (reverse result)
              (let* ([p1 (list-ref hull i)]
                     [p2 (list-ref hull (modulo (+ i 1) n))]
                     [dx (- (point2-x p2) (point2-x p1))]
                     [dy (- (point2-y p2) (point2-y p1))])
                ;; Skip zero-length edges (duplicate adjacent vertices)
                (if (and (< (abs dx) 1e-10) (< (abs dy) 1e-10))
                    (loop (+ i 1) result)
                    (let* ([edge (make-point2 dx dy)]
                           [angle (normalize-angle (atan dy dx))])
                      (loop (+ i 1) (cons (cons edge angle) result))))))))))

(doc merge-edges-ccw 'type '(-> (Listof (Pair Point2 Number)) (Listof (Pair Point2 Number)) (Listof Point2)))
(doc merge-edges-ccw 'description "Merge two edge lists in CCW (increasing angle) order")
(define (merge-edges-ccw edges-a edges-b)
  (doc 'export #t)
  (let loop ([a edges-a] [b edges-b] [result '()])
    (cond
      [(and (null? a) (null? b)) (reverse result)]
      [(null? a) (loop a (cdr b) (cons (caar b) result))]
      [(null? b) (loop (cdr a) b (cons (caar a) result))]
      [else
       (if (<= (cdar a) (cdar b))
           (loop (cdr a) b (cons (caar a) result))
           (loop a (cdr b) (cons (caar b) result)))])))

(doc build-polygon-from-edges 'type '(-> Point2 (Listof Point2) (Listof Point2)))
(doc build-polygon-from-edges 'description "Build polygon vertices by walking edges from start")
(define (build-polygon-from-edges start edges)
  (doc 'export #t)
  (let loop ([remaining edges]
             [current start]
             [result (list start)])
    (if (null? remaining)
        (reverse (cdr result))  ; Remove duplicate start
        (let* ([edge (car remaining)]
               [next (make-point2 (+ (point2-x current) (point2-x edge))
                                  (+ (point2-y current) (point2-y edge)))])
          (loop (cdr remaining) next (cons next result))))))

(doc minkowski-difference 'type '(-> (Listof Point2) (Listof Point2) (Listof Point2)))
(doc minkowski-difference 'description
     "Compute Minkowski difference A ⊖ B = A ⊕ (-B).
      Used for collision detection: A and B intersect iff origin ∈ (A ⊖ B).")
(define (minkowski-difference hull-a hull-b)
  (doc 'export #t)
  ;; Negating a convex hull produces another convex hull (reflection through origin).
  ;; CCW winding becomes CW, so reverse to restore CCW order.
  ;; This avoids O(n log n) hull recomputation - just O(n) negate + reverse.
  (let ([neg-b (reverse
                (map (lambda (p)
                       (make-point2 (- (point2-x p)) (- (point2-y p))))
                     hull-b))])
    (minkowski-sum hull-a neg-b)))

;;; ============================================================
;;; Section: Collision Detection via GJK-style
;;; ============================================================

(doc 'section 'collision)

(doc hulls-intersect? 'type '(-> (Listof Point2) (Listof Point2) Boolean))
(doc hulls-intersect? 'description
     "Test if two convex hulls intersect.
      Uses Minkowski difference: intersect iff origin ∈ (A ⊖ B).")
(define (hulls-intersect? hull-a hull-b)
  (doc 'export #t)
  (let ([diff (minkowski-difference hull-a hull-b)])
    (point-in-convex-hull? (make-point2 0 0) diff)))
