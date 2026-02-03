;;; lattice/geometry/mesh-gen.ss --- 2D/3D mesh generation algorithms
;;; @module mesh-gen
;;; @requires prelude linalg/vec geometry

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/data/heap.ss")

(doc 'module 'mesh-gen)
(doc 'description "Mesh generation: Delaunay triangulation, quality metrics, and refinement")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Section: 2D Point and Triangle Representation
;;; ============================================================

(doc 'section 'representation)

(doc make-point2 'type '(-> Number Number Point2))
(doc make-point2 'description "Create a 2D point")
(define (make-point2 x y)
  (vector x y))

;;; ============================================================
;;; Section: Triangulation Record
;;; ============================================================

(doc 'section 'triangulation)

;;; A triangulation bundles triangles with adjacency for efficient queries
(doc make-triangulation 'type '(-> (Vector Point2) (List Triangle2) Hashtable (List Edge) Triangulation))
(doc make-triangulation 'description "Create a triangulation with adjacency information")
(define (make-triangulation points triangles adjacency boundary)
  ;; Build point->index map for O(1) lookup in interpolation
  (let ([point-index (make-hashtable equal-hash equal?)]
        [n (vector-length points)])
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (let ([p (vector-ref points i)])
        (hashtable-set! point-index (vertex-key p) i)))
    (vector 'triangulation points triangles adjacency boundary (length triangles) point-index)))

(doc triangulation? 'export #t)
(doc triangulation? 'type '(-> Any Boolean))
(doc triangulation? 'description "Check if value is a triangulation")
(define (triangulation? x)
  (and (vector? x) (>= (vector-length x) 1) (eq? (vector-ref x 0) 'triangulation)))

(doc triangulation-points 'export #t)
(doc triangulation-points 'type '(-> Triangulation (Vector Point2)))
(doc triangulation-points 'description "Get the input points as a vector (indexed by point-id)")
(define (triangulation-points tri) (vector-ref tri 1))

(doc triangulation-triangles 'export #t)
(doc triangulation-triangles 'type '(-> Triangulation (List Triangle2)))
(doc triangulation-triangles 'description "Get the list of triangles")
(define (triangulation-triangles tri) (vector-ref tri 2))

(doc triangulation-adjacency 'type '(-> Triangulation Hashtable))
(doc triangulation-adjacency 'description "Get the adjacency hash (tri → neighbors)")
(define (triangulation-adjacency tri) (vector-ref tri 3))

(doc triangulation-boundary 'export #t)
(doc triangulation-boundary 'type '(-> Triangulation (List Edge)))
(doc triangulation-boundary 'description "Get boundary edges (CCW winding)")
(define (triangulation-boundary tri) (vector-ref tri 4))

(doc triangulation-count 'type '(-> Triangulation Integer))
(doc triangulation-count 'description "Get the number of triangles (cached, O(1))")
(define (triangulation-count tri) (vector-ref tri 5))

(doc triangulation-point-index 'type '(-> Triangulation Hashtable))
(doc triangulation-point-index 'description "Get point->index map for O(1) interpolation lookup")
(define (triangulation-point-index tri) (vector-ref tri 6))

;;; Location record for point query results
(doc make-location 'type '(-> Triangle2 Number Number Number Location))
(doc make-location 'description "Create a location result with triangle and barycentric coords")
(define (make-location triangle u v w)
  (vector 'location triangle u v w))

(doc location? 'export #t)
(doc location? 'type '(-> Any Boolean))
(doc location? 'description "Check if value is a location result")
(define (location? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'location)))

(doc location-triangle 'export #t)
(doc location-triangle 'type '(-> Location Triangle2))
(doc location-triangle 'description "Get the containing triangle")
(define (location-triangle loc) (vector-ref loc 1))

(doc location-bary 'export #t)
(doc location-bary 'type '(-> Location (Vector Number Number Number)))
(doc location-bary 'description "Get barycentric coordinates as #(u v w)")
(define (location-bary loc) (vector (vector-ref loc 2) (vector-ref loc 3) (vector-ref loc 4)))

(define (point2-x p) (vector-ref p 0))
(define (point2-y p) (vector-ref p 1))

(doc vertex-key 'type '(-> Point2 (Cons Number Number)))
(doc vertex-key 'description
     "Create a hashable key for a vertex. Uses cons pair instead of list
      to reduce allocation (1 cell vs 2). For use with equal-hash hashtables.")
(define (vertex-key p)
  (cons (point2-x p) (point2-y p)))

(doc make-tri2 'type '(-> Point2 Point2 Point2 Triangle2))
(doc make-tri2 'description "Create a 2D triangle from three points (CCW order)")
(define (make-tri2 p1 p2 p3)
  (list 'tri2 p1 p2 p3))

(define (tri2? t) (and (pair? t) (eq? (car t) 'tri2)))
(define (tri2-p1 t) (list-ref t 1))
(define (tri2-p2 t) (list-ref t 2))
(define (tri2-p3 t) (list-ref t 3))

(define (tri2-points t)
  (list (tri2-p1 t) (tri2-p2 t) (tri2-p3 t)))

;;; ============================================================
;;; Section: Geometric Predicates
;;; ============================================================

(doc 'section 'predicates)

(doc orient2d 'export #t)
(doc orient2d 'type '(-> Point2 Point2 Point2 Number))
(doc orient2d 'description
     "Compute orientation of point C relative to directed line AB.
      Returns positive if C is left of AB (CCW), negative if right (CW), zero if collinear.
      This is the signed area of triangle ABC times 2.")
(define (orient2d a b c)
  (let ([ax (point2-x a)] [ay (point2-y a)]
        [bx (point2-x b)] [by (point2-y b)]
        [cx (point2-x c)] [cy (point2-y c)])
    (- (* (- bx ax) (- cy ay))
       (* (- by ay) (- cx ax)))))

(doc tri2-signed-area 'type '(-> Triangle2 Number))
(doc tri2-signed-area 'description "Signed area of triangle (positive if CCW)")
(define (tri2-signed-area tri)
  (let* ([p1 (tri2-p1 tri)]
         [p2 (tri2-p2 tri)]
         [p3 (tri2-p3 tri)]
         [x1 (point2-x p1)] [y1 (point2-y p1)]
         [x2 (point2-x p2)] [y2 (point2-y p2)]
         [x3 (point2-x p3)] [y3 (point2-y p3)])
    (* 0.5 (- (* (- x2 x1) (- y3 y1))
              (* (- x3 x1) (- y2 y1))))))

(doc tri2-area 'type '(-> Triangle2 Number))
(doc tri2-area 'description "Absolute area of triangle")
(define (tri2-area tri)
  (abs (tri2-signed-area tri)))

(doc tri2-circumcenter 'type '(-> Triangle2 (Or Point2 False)))
(doc tri2-circumcenter 'description "Compute circumcenter of triangle. Returns #f for degenerate (collinear) triangles.")
(define (tri2-circumcenter tri)
  (let* ([p1 (tri2-p1 tri)]
         [p2 (tri2-p2 tri)]
         [p3 (tri2-p3 tri)]
         [ax (point2-x p1)] [ay (point2-y p1)]
         [bx (point2-x p2)] [by (point2-y p2)]
         [cx (point2-x p3)] [cy (point2-y p3)]
         [d (* 2.0 (+ (* ax (- by cy))
                      (* bx (- cy ay))
                      (* cx (- ay by))))])
    ;; Guard against degenerate (collinear) triangles
    (if (< (abs d) 1e-15)
        #f
        (let* ([a2 (+ (* ax ax) (* ay ay))]
               [b2 (+ (* bx bx) (* by by))]
               [c2 (+ (* cx cx) (* cy cy))]
               [ux (/ (+ (* a2 (- by cy))
                        (* b2 (- cy ay))
                        (* c2 (- ay by)))
                     d)]
               [uy (/ (+ (* a2 (- cx bx))
                        (* b2 (- ax cx))
                        (* c2 (- bx ax)))
                     d)])
          (make-point2 ux uy)))))

(doc tri2-circumradius-sq 'type '(-> Triangle2 (Or Number False)))
(doc tri2-circumradius-sq 'description "Squared circumradius of triangle. Returns #f for degenerate triangles.")
(define (tri2-circumradius-sq tri)
  (let ([cc (tri2-circumcenter tri)])
    (if (not cc)
        #f
        (let* ([p1 (tri2-p1 tri)]
               [dx (- (point2-x cc) (point2-x p1))]
               [dy (- (point2-y cc) (point2-y p1))])
          (+ (* dx dx) (* dy dy))))))

(doc point-in-circumcircle? 'type '(-> Point2 Triangle2 Boolean))
(doc point-in-circumcircle? 'description "Test if point is inside triangle's circumcircle.
  Returns #f for degenerate (collinear) triangles.")
(define (point-in-circumcircle? p tri)
  (let ([cc (tri2-circumcenter tri)])
    ;; Degenerate triangles have no finite circumcircle
    (if (not cc)
        #f
        (let* ([r2 (tri2-circumradius-sq tri)]
               [dx (- (point2-x p) (point2-x cc))]
               [dy (- (point2-y p) (point2-y cc))]
               [d2 (+ (* dx dx) (* dy dy))])
          (< d2 r2)))))

;;; ============================================================
;;; Section: Edge Operations
;;; ============================================================

(doc 'section 'edges)

(define (make-edge p1 p2)
  (list 'edge p1 p2))

(define (edge-p1 e) (list-ref e 1))
(define (edge-p2 e) (list-ref e 2))

(define (points-equal? p1 p2)
  (and (< (abs (- (point2-x p1) (point2-x p2))) 1e-10)
       (< (abs (- (point2-y p1) (point2-y p2))) 1e-10)))

(define (edges-equal? e1 e2)
  ;; Edges are undirected: (a,b) == (b,a)
  (or (and (points-equal? (edge-p1 e1) (edge-p1 e2))
           (points-equal? (edge-p2 e1) (edge-p2 e2)))
      (and (points-equal? (edge-p1 e1) (edge-p2 e2))
           (points-equal? (edge-p2 e1) (edge-p1 e2)))))

(define (tri2-edges tri)
  (let ([p1 (tri2-p1 tri)]
        [p2 (tri2-p2 tri)]
        [p3 (tri2-p3 tri)])
    (list (make-edge p1 p2)
          (make-edge p2 p3)
          (make-edge p3 p1))))

(define (edge-in-list? edge edges)
  (exists (lambda (e) (edges-equal? edge e)) edges))

(define (count-edge edge edges)
  (length (filter (lambda (e) (edges-equal? edge e)) edges)))

;;; Hash-based boundary edge extraction: O(n) instead of O(n²)
;;; Uses toggle semantics: first occurrence adds, second removes
(doc edge-key 'type '(-> Edge (List Number Number Number Number)))
(doc edge-key 'description
     "Create a canonical hash key for an edge. Points are ordered so (a,b) and (b,a) produce the same key.")
(define (edge-key edge)
  (canonical-edge-key (edge-p1 edge) (edge-p2 edge)))

(doc find-boundary-edges-hash 'type '(-> (List Edge) (List Edge)))
(doc find-boundary-edges-hash 'description
     "Find edges appearing exactly once using hash-based toggle.
      O(n) vs O(n²) for filter+count approach.")
(define (find-boundary-edges-hash edges)
  (let ([edge-map (make-hashtable equal-hash equal?)])
    ;; Toggle: add on first occurrence, remove on second
    (for-each
     (lambda (e)
       (let ([key (edge-key e)])
         (if (hashtable-contains? edge-map key)
             (hashtable-delete! edge-map key)
             (hashtable-set! edge-map key e))))
     edges)
    ;; Remaining entries appeared exactly once
    (let-values ([(_ vals) (hashtable-entries edge-map)])
      (vector->list vals))))

;;; ============================================================
;;; Section: Segment Intersection (for Constrained Delaunay)
;;; ============================================================

(doc 'section 'segment-intersection)

(doc segments-properly-intersect? 'export #t)
(doc segments-properly-intersect? 'type '(-> Point2 Point2 Point2 Point2 Boolean))
(doc segments-properly-intersect? 'description
     "Test if segments (a,b) and (c,d) properly intersect (cross in interior).
      Returns #f if segments share an endpoint, are collinear, or don't cross.
      Uses orient2d for robust geometric predicate.")
(define (segments-properly-intersect? a b c d)
  ;; Segments properly intersect iff:
  ;; 1. c and d are on opposite sides of line(a,b), AND
  ;; 2. a and b are on opposite sides of line(c,d)
  ;; "Opposite sides" means orientations have opposite signs (both nonzero)
  (let ([o1 (orient2d a b c)]
        [o2 (orient2d a b d)]
        [o3 (orient2d c d a)]
        [o4 (orient2d c d b)])
    ;; Both pairs must have opposite signs (strictly)
    (and (< (* o1 o2) 0)
         (< (* o3 o4) 0))))

(doc edge-properly-intersects-edge? 'type '(-> Edge Edge Boolean))
(doc edge-properly-intersects-edge? 'description
     "Test if two edges properly intersect (cross in interior, not at endpoints)")
(define (edge-properly-intersects-edge? e1 e2)
  (segments-properly-intersect? (edge-p1 e1) (edge-p2 e1)
                                 (edge-p1 e2) (edge-p2 e2)))

(doc triangle-crosses-edge? 'type '(-> Triangle2 Edge Boolean))
(doc triangle-crosses-edge? 'description
     "Test if any edge of triangle properly crosses the given edge")
(define (triangle-crosses-edge? tri constraint-edge)
  (exists (lambda (tri-edge)
         (edge-properly-intersects-edge? tri-edge constraint-edge))
       (tri2-edges tri)))

(doc triangle-crosses-any-constraint? 'type '(-> Triangle2 (List Edge) Boolean))
(doc triangle-crosses-any-constraint? 'description
     "Test if triangle crosses any constraint edge")
(define (triangle-crosses-any-constraint? tri constraint-edges)
  (exists (lambda (ce) (triangle-crosses-edge? tri ce))
       constraint-edges))

;;; ============================================================
;;; Section: Bowyer-Watson Delaunay Triangulation
;;; ============================================================

(doc 'section 'delaunay)

(doc make-super-triangle 'type '(-> (List Point2) Triangle2))
(doc make-super-triangle 'description "Create super-triangle containing all points")
(define (make-super-triangle points)
  ;; Find bounding box
  (let* ([xs (map point2-x points)]
         [ys (map point2-y points)]
         [min-x (apply min xs)]
         [max-x (apply max xs)]
         [min-y (apply min ys)]
         [max-y (apply max ys)]
         [dx (- max-x min-x)]
         [dy (- max-y min-y)]
         [dmax (max dx dy)]
         [mid-x (/ (+ min-x max-x) 2.0)]
         [mid-y (/ (+ min-y max-y) 2.0)]
         ;; Make triangle much larger than bounding box
         [p1 (make-point2 (- mid-x (* 2.0 dmax)) (- mid-y dmax))]
         [p2 (make-point2 mid-x (+ mid-y (* 2.0 dmax)))]
         [p3 (make-point2 (+ mid-x (* 2.0 dmax)) (- mid-y dmax))])
    (make-tri2 p1 p2 p3)))

(define (triangle-uses-point? tri p)
  (or (points-equal? (tri2-p1 tri) p)
      (points-equal? (tri2-p2 tri) p)
      (points-equal? (tri2-p3 tri) p)))

(define (triangle-uses-super-vertex? tri super-tri)
  (or (triangle-uses-point? tri (tri2-p1 super-tri))
      (triangle-uses-point? tri (tri2-p2 super-tri))
      (triangle-uses-point? tri (tri2-p3 super-tri))))

(doc delaunay-insert-point 'type '(-> (List Triangle2) Point2 (List Triangle2)))
(doc delaunay-insert-point 'description "Insert point into triangulation (Bowyer-Watson step)")
(define (delaunay-insert-point triangles point)
  ;; Find all triangles whose circumcircle contains the point
  (let* ([bad-tris (filter (lambda (tri) (point-in-circumcircle? point tri))
                           triangles)]
         [good-tris (filter (lambda (tri) (not (point-in-circumcircle? point tri)))
                            triangles)]
         ;; Collect all edges of bad triangles
         [all-edges (append-map tri2-edges bad-tris)]
         ;; Find boundary edges using O(n) hash-based toggle
         [boundary-edges (find-boundary-edges-hash all-edges)]
         ;; Create new triangles by connecting boundary edges to point
         [new-tris (map (lambda (e)
                          (make-tri2 (edge-p1 e) (edge-p2 e) point))
                        boundary-edges)])
    (append good-tris new-tris)))

(doc delaunay-insert-point-diff 'type '(-> (List Triangle2) Point2 (Values (List Triangle2) (List Triangle2) (List Triangle2))))
(doc delaunay-insert-point-diff 'description
     "Insert point into triangulation, returning diff information.
      Returns (values new-triangles removed-triangles added-triangles).
      Used by priority-queue-based refinement to efficiently track changes.")
(define (delaunay-insert-point-diff triangles point)
  ;; Find all triangles whose circumcircle contains the point
  (let* ([bad-tris (filter (lambda (tri) (point-in-circumcircle? point tri))
                           triangles)]
         [good-tris (filter (lambda (tri) (not (point-in-circumcircle? point tri)))
                            triangles)]
         ;; Collect all edges of bad triangles
         [all-edges (append-map tri2-edges bad-tris)]
         ;; Find boundary edges using O(n) hash-based toggle
         [boundary-edges (find-boundary-edges-hash all-edges)]
         ;; Create new triangles by connecting boundary edges to point
         [new-tris (map (lambda (e)
                          (make-tri2 (edge-p1 e) (edge-p2 e) point))
                        boundary-edges)])
    (values (append good-tris new-tris) bad-tris new-tris)))

;;; ============================================================
;;; Section: Adjacency Building
;;; ============================================================

(doc 'section 'adjacency)

(define (point2-key p)
  "Create a sortable key for a point"
  (cons (point2-x p) (point2-y p)))

(define (key<? k1 k2)
  (or (< (car k1) (car k2))
      (and (= (car k1) (car k2))
           (< (cdr k1) (cdr k2)))))

(define (canonical-edge-key p1 p2)
  "Return edge key with points in canonical order for hashing"
  (let ([k1 (point2-key p1)]
        [k2 (point2-key p2)])
    (if (key<? k1 k2)
        (list (point2-x p1) (point2-y p1) (point2-x p2) (point2-y p2))
        (list (point2-x p2) (point2-y p2) (point2-x p1) (point2-y p1)))))

(doc build-adjacency 'export #t)
(doc build-adjacency 'type '(-> (List Triangle2) Hashtable))
(doc build-adjacency 'description
     "Build triangle adjacency: hash mapping each triangle to (n12 n23 n31).
      Each neighbor is the triangle sharing that edge, or #f if boundary.")
(define (build-adjacency triangles)
  (let ([edge-map (make-hashtable equal-hash equal?)]
        [tri-vec (list->vector triangles)]
        [n (length triangles)])
    ;; Pass 1: map each edge to list of triangles sharing it
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (let* ([tri (vector-ref tri-vec i)]
             [p1 (tri2-p1 tri)]
             [p2 (tri2-p2 tri)]
             [p3 (tri2-p3 tri)]
             [e12 (canonical-edge-key p1 p2)]
             [e23 (canonical-edge-key p2 p3)]
             [e31 (canonical-edge-key p3 p1)])
        (hashtable-update! edge-map e12 (lambda (v) (cons tri v)) '())
        (hashtable-update! edge-map e23 (lambda (v) (cons tri v)) '())
        (hashtable-update! edge-map e31 (lambda (v) (cons tri v)) '())))
    ;; Pass 2: for each triangle, find its neighbors
    (let ([adj (make-eq-hashtable)])
      (do ([i 0 (+ i 1)])
          ((>= i n) adj)
        (let* ([tri (vector-ref tri-vec i)]
               [p1 (tri2-p1 tri)]
               [p2 (tri2-p2 tri)]
               [p3 (tri2-p3 tri)]
               [find-neighbor
                (lambda (pa pb)
                  (let ([others (hashtable-ref edge-map (canonical-edge-key pa pb) '())])
                    (let ([filtered (filter (lambda (t) (not (eq? t tri))) others)])
                      (if (null? filtered) #f (car filtered)))))]
               [n12 (find-neighbor p1 p2)]
               [n23 (find-neighbor p2 p3)]
               [n31 (find-neighbor p3 p1)])
          (hashtable-set! adj tri (vector n12 n23 n31)))))))

(doc find-boundary-edges 'type '(-> (List Triangle2) Hashtable (List Edge)))
(doc find-boundary-edges 'description "Find edges on the convex hull boundary (CCW order)")
(define (find-boundary-edges triangles adjacency)
  ;; Boundary edges are those where the neighbor is #f
  (let loop ([tris triangles] [edges '()])
    (if (null? tris)
        edges  ; TODO: could sort into CCW order if needed
        (let* ([tri (car tris)]
               [neighbors (hashtable-ref adjacency tri #f)]
               [p1 (tri2-p1 tri)]
               [p2 (tri2-p2 tri)]
               [p3 (tri2-p3 tri)])
          (loop (cdr tris)
                (append
                 (if (not (vector-ref neighbors 0)) (list (make-edge p1 p2)) '())
                 (if (not (vector-ref neighbors 1)) (list (make-edge p2 p3)) '())
                 (if (not (vector-ref neighbors 2)) (list (make-edge p3 p1)) '())
                 edges))))))

(doc triangle-neighbors 'export #t)
(doc triangle-neighbors 'type '(-> Triangulation Triangle2 (Vector Triangle2)))
(doc triangle-neighbors 'description "Get neighbors of a triangle as #(n12 n23 n31), where #f means boundary")
(define (triangle-neighbors tri-data triangle)
  (hashtable-ref (triangulation-adjacency tri-data) triangle #f))

;;; ============================================================
;;; Section: Bowyer-Watson Delaunay Triangulation
;;; ============================================================

(doc delaunay-triangulate 'export #t)
(doc delaunay-triangulate 'type '(-> (List Point2) Triangulation))
(doc delaunay-triangulate 'description
     "Compute Delaunay triangulation of 2D points (Bowyer-Watson algorithm).
      Returns a triangulation record with adjacency for efficient point location.")
(define (delaunay-triangulate points)
  (if (< (length points) 3)
      (make-triangulation (list->vector points) '() (make-eq-hashtable) '())
      (let* ([super-tri (make-super-triangle points)]
             ;; Start with super-triangle
             [triangles (list super-tri)]
             ;; Insert all points
             [triangles (fold-left delaunay-insert-point triangles points)]
             ;; Remove triangles connected to super-triangle vertices
             [triangles (filter (lambda (tri)
                                  (not (triangle-uses-super-vertex? tri super-tri)))
                                triangles)]
             ;; Build adjacency
             [adjacency (build-adjacency triangles)]
             ;; Find boundary edges
             [boundary (find-boundary-edges triangles adjacency)])
        (make-triangulation (list->vector points) triangles adjacency boundary))))

;;; ============================================================
;;; Section: Point Location (Walking Algorithm)
;;; ============================================================

(doc 'section 'location)

(doc barycentric-coords 'export #t)
(doc barycentric-coords 'type '(-> Point2 Triangle2 (Values Number Number Number)))
(doc barycentric-coords 'description "Compute barycentric coordinates of point in triangle.
  Returns (values 1/3 1/3 1/3) for degenerate (collinear) triangles.")
(define (barycentric-coords p tri)
  (let* ([p1 (tri2-p1 tri)]
         [p2 (tri2-p2 tri)]
         [p3 (tri2-p3 tri)]
         [x (point2-x p)] [y (point2-y p)]
         [x1 (point2-x p1)] [y1 (point2-y p1)]
         [x2 (point2-x p2)] [y2 (point2-y p2)]
         [x3 (point2-x p3)] [y3 (point2-y p3)]
         [denom (+ (* (- y2 y3) (- x1 x3))
                   (* (- x3 x2) (- y1 y3)))])
    ;; Guard against degenerate (collinear) triangles
    (if (< (abs denom) 1e-15)
        (values 1/3 1/3 1/3)
        (let* ([u (/ (+ (* (- y2 y3) (- x x3))
                       (* (- x3 x2) (- y y3)))
                    denom)]
               [v (/ (+ (* (- y3 y1) (- x x3))
                       (* (- x1 x3) (- y y3)))
                    denom)]
               [w (- 1.0 u v)])
          (values u v w)))))

(doc locate-point 'export #t)
(doc locate-point 'type '(-> Triangulation Point2 [Triangle2] (Or Location False)))
(doc locate-point 'description
     "Find the triangle containing a point using walking algorithm.
      Optional hint parameter starts the walk from a specific triangle (for spatial locality).
      Returns a location record with triangle and barycentric coords, or #f if outside hull.")
(define locate-point
  (case-lambda
    [(tri-data point)
     (let ([tris (triangulation-triangles tri-data)])
       (if (null? tris)
           #f
           (locate-point tri-data point (car tris))))]
    [(tri-data point hint)
     (let* ([n (triangulation-count tri-data)]  ; O(1) instead of O(n)
            [max-steps (+ 10 (* 2 (exact (ceiling (sqrt n)))))]
            [adj (triangulation-adjacency tri-data)])
       (let loop ([current hint] [steps 0])
         (cond
           [(> steps max-steps) #f]  ; degenerate case, bail
           [(not current) #f]        ; walked off hull
           [else
            (let* ([p1 (tri2-p1 current)]
                   [p2 (tri2-p2 current)]
                   [p3 (tri2-p3 current)]
                   ;; Use orient2d for robust edge crossing decisions
                   [o12 (orient2d p1 p2 point)]
                   [o23 (orient2d p2 p3 point)]
                   [o31 (orient2d p3 p1 point)]
                   ;; Check triangle orientation (may be CW or CCW)
                   [tri-orient (orient2d p1 p2 p3)]
                   ;; For CCW (positive area), inside => all orient >= 0
                   ;; For CW (negative area), inside => all orient <= 0
                   [inside? (if (>= tri-orient 0)
                                (and (>= o12 0) (>= o23 0) (>= o31 0))
                                (and (<= o12 0) (<= o23 0) (<= o31 0)))])
              (cond
                ;; Point is inside (or on boundary)
                [inside?
                 (let-values ([(u v w) (barycentric-coords point current)])
                   (make-location current u v w))]
                ;; Cross the edge where point is on the wrong side
                [else
                 (let ([neighbors (hashtable-ref adj current #f)])
                   ;; For CW, cross when orient > 0; for CCW, when orient < 0
                   (if (>= tri-orient 0)
                       (cond
                         [(< o12 0) (loop (vector-ref neighbors 0) (+ steps 1))]
                         [(< o23 0) (loop (vector-ref neighbors 1) (+ steps 1))]
                         [else      (loop (vector-ref neighbors 2) (+ steps 1))])
                       (cond
                         [(> o12 0) (loop (vector-ref neighbors 0) (+ steps 1))]
                         [(> o23 0) (loop (vector-ref neighbors 1) (+ steps 1))]
                         [else      (loop (vector-ref neighbors 2) (+ steps 1))])))]))])))]))

;;; ============================================================
;;; Section: Interpolation
;;; ============================================================

(doc 'section 'interpolation)

(doc interpolate-at 'export #t)
(doc interpolate-at 'type '(-> Triangulation Point2 (Vector Number) (Or Number False)))
(doc interpolate-at 'description
     "Interpolate a value at a point using the triangulation.
      Values is a vector indexed by point position in triangulation-points.
      Returns interpolated value, or #f if point is outside the convex hull.")
(define (interpolate-at tri-data point values)
  (let ([loc (locate-point tri-data point)])
    (if (not loc)
        #f
        (let* ([tri (location-triangle loc)]
               [bary (location-bary loc)]
               [u (vector-ref bary 0)]
               [v (vector-ref bary 1)]
               [w (vector-ref bary 2)]
               [point-index (triangulation-point-index tri-data)]
               [p1 (tri2-p1 tri)]
               [p2 (tri2-p2 tri)]
               [p3 (tri2-p3 tri)]
               ;; O(1) point index lookup via hash
               [find-idx (lambda (p)
                           (hashtable-ref point-index (vertex-key p) #f))]
               [i1 (find-idx p1)]
               [i2 (find-idx p2)]
               [i3 (find-idx p3)])
          (if (and i1 i2 i3)
              (+ (* u (vector-ref values i1))
                 (* v (vector-ref values i2))
                 (* w (vector-ref values i3)))
              #f)))))

;;; ============================================================
;;; Section: Mesh Quality Metrics
;;; ============================================================

(doc 'section 'quality)

(define (point2-distance p1 p2)
  (let ([dx (- (point2-x p1) (point2-x p2))]
        [dy (- (point2-y p1) (point2-y p2))])
    (sqrt (+ (* dx dx) (* dy dy)))))

(doc tri2-edge-lengths 'type '(-> Triangle2 (List Number)))
(doc tri2-edge-lengths 'description "Return sorted list of edge lengths [shortest, middle, longest]")
(define (tri2-edge-lengths tri)
  (let* ([p1 (tri2-p1 tri)]
         [p2 (tri2-p2 tri)]
         [p3 (tri2-p3 tri)]
         [d12 (point2-distance p1 p2)]
         [d23 (point2-distance p2 p3)]
         [d31 (point2-distance p3 p1)])
    (sort < (list d12 d23 d31))))

(doc tri2-aspect-ratio 'export #t)
(doc tri2-aspect-ratio 'type '(-> Triangle2 Number))
(doc tri2-aspect-ratio 'description "Aspect ratio: longest edge / shortest edge (1.0 = equilateral)")
(define (tri2-aspect-ratio tri)
  (let ([lengths (tri2-edge-lengths tri)])
    (/ (caddr lengths) (car lengths))))

(doc tri2-angles 'export #t)
(doc tri2-angles 'type '(-> Triangle2 (List Number)))
(doc tri2-angles 'description "Return angles of triangle in radians [smallest, middle, largest]")
(define (tri2-angles tri)
  (let* ([lengths (tri2-edge-lengths tri)]
         [a (car lengths)]
         [b (cadr lengths)]
         [c (caddr lengths)]
         ;; Law of cosines: cos(C) = (a² + b² - c²) / (2ab)
         [angle-c (acos (/ (+ (* a a) (* b b) (- (* c c))) (* 2 a b)))]
         [angle-b (acos (/ (+ (* a a) (* c c) (- (* b b))) (* 2 a c)))]
         [angle-a (- 3.141592653589793 angle-b angle-c)])
    (sort < (list angle-a angle-b angle-c))))

(doc tri2-min-angle 'export #t)
(doc tri2-min-angle 'type '(-> Triangle2 Number))
(doc tri2-min-angle 'description "Minimum angle of triangle in radians")
(define (tri2-min-angle tri)
  (car (tri2-angles tri)))

(doc tri2-max-angle 'export #t)
(doc tri2-max-angle 'type '(-> Triangle2 Number))
(doc tri2-max-angle 'description "Maximum angle of triangle in radians")
(define (tri2-max-angle tri)
  (caddr (tri2-angles tri)))

(doc radians->degrees 'type '(-> Number Number))
(define (radians->degrees r)
  (* r (/ 180.0 3.141592653589793)))

(doc mesh-quality-report 'export #t)
(doc mesh-quality-report 'type '(-> (Or (List Triangle2) Triangulation) Void))
(doc mesh-quality-report 'description "Print quality statistics for a mesh")
(define (mesh-quality-report tris-or-tri)
  (let ([triangles (if (triangulation? tris-or-tri)
                       (triangulation-triangles tris-or-tri)
                       tris-or-tri)])
  (if (null? triangles)
      (printf "Empty mesh~n")
      (let* ([aspects (map tri2-aspect-ratio triangles)]
             [min-angles (map tri2-min-angle triangles)]
             [max-angles (map tri2-max-angle triangles)]
             [areas (map tri2-area triangles)])
        (printf "Mesh Quality Report~n")
        (printf "  Triangles: ~a~n" (length triangles))
        (printf "  Total area: ~,2f~n" (apply + areas))
        (printf "  Aspect ratio: min=~,2f  max=~,2f  avg=~,2f~n"
                (apply min aspects)
                (apply max aspects)
                (/ (apply + aspects) (length aspects)))
        (printf "  Min angle: min=~,1f°  max=~,1f°  avg=~,1f°~n"
                (radians->degrees (apply min min-angles))
                (radians->degrees (apply max min-angles))
                (radians->degrees (/ (apply + min-angles) (length min-angles))))
        (printf "  Max angle: min=~,1f°  max=~,1f°  avg=~,1f°~n"
                (radians->degrees (apply min max-angles))
                (radians->degrees (apply max max-angles))
                (radians->degrees (/ (apply + max-angles) (length max-angles))))))))

;;; ============================================================
;;; Section: Mesh Refinement (Ruppert's Algorithm)
;;; ============================================================

(doc 'section 'refinement)

(doc tri2-is-bad? 'type '(-> Triangle2 Number Boolean))
(doc tri2-is-bad? 'description "Check if triangle is 'bad' (min angle below threshold)")
(define (tri2-is-bad? tri min-angle-threshold)
  (< (tri2-min-angle tri) min-angle-threshold))

(doc refine-mesh 'export #t)
(doc refine-mesh 'type '(-> (Or (List Triangle2) Triangulation) Number Nat Triangulation))
(doc refine-mesh 'description "Refine mesh by inserting circumcenters of bad triangles. Returns a triangulation.")
(define (refine-mesh tris-or-tri min-angle-deg max-iterations)
  (let* ([triangles (if (triangulation? tris-or-tri)
                        (triangulation-triangles tris-or-tri)
                        tris-or-tri)]
         [threshold (* min-angle-deg (/ 3.141592653589793 180.0))]
         ;; Run the refinement loop
         [final-tris
          (let loop ([tris triangles] [iter 0])
            (if (>= iter max-iterations)
                tris
                (let ([bad-tris (filter (lambda (t) (tri2-is-bad? t threshold)) tris)])
                  (if (null? bad-tris)
                      tris
                      ;; Insert circumcenter of worst triangle
                      (let* ([worst (car (sort (lambda (a b)
                                                 (< (tri2-min-angle a) (tri2-min-angle b)))
                                               bad-tris))]
                             [cc (tri2-circumcenter worst)]
                             [new-tris (delaunay-insert-point tris cc)])
                        (loop new-tris (+ iter 1)))))))]
         ;; Build adjacency for the refined mesh
         [adjacency (build-adjacency final-tris)]
         [boundary (find-boundary-edges final-tris adjacency)]
         ;; Collect all unique points from triangles
         [all-pts (let ([ht (make-hashtable equal-hash equal?)])
                    (for-each (lambda (tri)
                                (hashtable-set! ht (vertex-key (tri2-p1 tri)) (tri2-p1 tri))
                                (hashtable-set! ht (vertex-key (tri2-p2 tri)) (tri2-p2 tri))
                                (hashtable-set! ht (vertex-key (tri2-p3 tri)) (tri2-p3 tri)))
                              final-tris)
                    (list->vector (let-values ([(keys vals) (hashtable-entries ht)]) (vector->list vals))))])
    (make-triangulation all-pts final-tris adjacency boundary)))

;;; ============================================================
;;; Section: Laplacian Smoothing
;;; ============================================================

(doc 'section 'smoothing)

(doc build-vertex-neighbors 'type '(-> (List Triangle2) Hashtable))
(doc build-vertex-neighbors 'description
     "Build vertex adjacency: hash mapping each vertex to list of neighbor vertices")
(define (build-vertex-neighbors triangles)
  (let ([neighbors (make-hashtable equal-hash equal?)])
    (for-each
     (lambda (tri)
       (let* ([p1 (tri2-p1 tri)]
              [p2 (tri2-p2 tri)]
              [p3 (tri2-p3 tri)]
              [k1 (vertex-key p1)]
              [k2 (vertex-key p2)]
              [k3 (vertex-key p3)])
         ;; Add bidirectional edges
         (hashtable-update! neighbors k1 (lambda (v) (cons p2 (cons p3 v))) '())
         (hashtable-update! neighbors k2 (lambda (v) (cons p1 (cons p3 v))) '())
         (hashtable-update! neighbors k3 (lambda (v) (cons p1 (cons p2 v))) '())))
     triangles)
    neighbors))

(doc find-boundary-vertices 'type '(-> (List Triangle2) Hashtable (List Point2)))
(doc find-boundary-vertices 'description "Find vertices on the mesh boundary")
(define (find-boundary-vertices triangles adjacency)
  ;; A vertex is on the boundary if any of its edges is a boundary edge
  (let ([boundary-verts (make-hashtable equal-hash equal?)]
        [edge-counts (make-hashtable equal-hash equal?)])
    ;; Count occurrences of each edge
    (for-each
     (lambda (tri)
       (let* ([p1 (tri2-p1 tri)]
              [p2 (tri2-p2 tri)]
              [p3 (tri2-p3 tri)])
         (for-each
          (lambda (edge)
            (hashtable-update! edge-counts edge (lambda (n) (+ n 1)) 0))
          (list (canonical-edge-key p1 p2)
                (canonical-edge-key p2 p3)
                (canonical-edge-key p3 p1)))))
     triangles)
    ;; Find edges that appear only once (boundary edges)
    (let-values ([(keys vals) (hashtable-entries edge-counts)])
      (do ([i 0 (+ i 1)])
          ((>= i (vector-length keys)))
        (when (= 1 (vector-ref vals i))
          (let* ([key (vector-ref keys i)]
                 [x1 (list-ref key 0)] [y1 (list-ref key 1)]
                 [x2 (list-ref key 2)] [y2 (list-ref key 3)]
                 [p1 (make-point2 x1 y1)]
                 [p2 (make-point2 x2 y2)])
            (hashtable-set! boundary-verts (vertex-key p1) p1)
            (hashtable-set! boundary-verts (vertex-key p2) p2)))))
    (let-values ([(_ vals) (hashtable-entries boundary-verts)])
      (vector->list vals))))

(doc laplacian-smooth-step 'type '(-> (List Triangle2) Number (List Point2) (List Triangle2)))
(doc laplacian-smooth-step 'description
     "One iteration of Laplacian smoothing. Moves interior vertices toward neighbor centroid.")
(define (laplacian-smooth-step triangles weight boundary-pts)
  (let* ([vertex-neighbors (build-vertex-neighbors triangles)]
         [boundary-set (make-hashtable equal-hash equal?)]
         [new-positions (make-hashtable equal-hash equal?)])
    ;; Mark boundary vertices
    (for-each
     (lambda (p)
       (hashtable-set! boundary-set (vertex-key p) #t))
     boundary-pts)
    ;; Calculate new positions for interior vertices
    (let-values ([(keys _) (hashtable-entries vertex-neighbors)])
      (do ([i 0 (+ i 1)])
          ((>= i (vector-length keys)))
        (let* ([key (vector-ref keys i)]
               [x (car key)] [y (cdr key)]  ; vertex-key is a cons pair
               [p (make-point2 x y)])
          (if (hashtable-ref boundary-set key #f)
              ;; Boundary vertex: don't move
              (hashtable-set! new-positions key p)
              ;; Interior vertex: move toward centroid of neighbors
              (let* ([nbrs (hashtable-ref vertex-neighbors key '())]
                     ;; Remove duplicates
                     [unique-nbrs
                      (let ([seen (make-hashtable equal-hash equal?)])
                        (filter (lambda (n)
                                  (let ([nk (vertex-key n)])
                                    (if (hashtable-ref seen nk #f)
                                        #f
                                        (begin (hashtable-set! seen nk #t) #t))))
                                nbrs))]
                     [n (length unique-nbrs)])
                (if (zero? n)
                    (hashtable-set! new-positions key p)
                    (let* ([cx (/ (apply + (map point2-x unique-nbrs)) n)]
                           [cy (/ (apply + (map point2-y unique-nbrs)) n)]
                           ;; Weighted blend: new = old + weight * (centroid - old)
                           [new-x (+ x (* weight (- cx x)))]
                           [new-y (+ y (* weight (- cy y)))])
                      (hashtable-set! new-positions key (make-point2 new-x new-y)))))))))
    ;; Rebuild triangles with new vertex positions
    (map (lambda (tri)
           (let* ([p1 (tri2-p1 tri)]
                  [p2 (tri2-p2 tri)]
                  [p3 (tri2-p3 tri)]
                  [k1 (vertex-key p1)]
                  [k2 (vertex-key p2)]
                  [k3 (vertex-key p3)])
             (make-tri2 (hashtable-ref new-positions k1 p1)
                        (hashtable-ref new-positions k2 p2)
                        (hashtable-ref new-positions k3 p3))))
         triangles)))

(doc smooth-mesh 'export #t)
(doc smooth-mesh 'type '(-> (Or (List Triangle2) Triangulation) Nat Number Triangulation))
(doc smooth-mesh 'description
     "Apply Laplacian smoothing to improve mesh quality.
      - iterations: number of smoothing passes
      - weight: smoothing strength (0.0-1.0, typical 0.3-0.5)
      Boundary vertices are held fixed.")
(define (smooth-mesh tris-or-tri iterations weight)
  (let* ([triangles (if (triangulation? tris-or-tri)
                        (triangulation-triangles tris-or-tri)
                        tris-or-tri)]
         [adjacency (build-adjacency triangles)]
         [boundary-pts (find-boundary-vertices triangles adjacency)]
         ;; Run smoothing iterations
         [smoothed
          (let loop ([tris triangles] [iter 0])
            (if (>= iter iterations)
                tris
                (loop (laplacian-smooth-step tris weight boundary-pts) (+ iter 1))))]
         ;; Rebuild triangulation structure
         [new-adjacency (build-adjacency smoothed)]
         [new-boundary (find-boundary-edges smoothed new-adjacency)]
         [all-pts (let ([ht (make-hashtable equal-hash equal?)])
                    (for-each (lambda (tri)
                                (hashtable-set! ht (vertex-key (tri2-p1 tri)) (tri2-p1 tri))
                                (hashtable-set! ht (vertex-key (tri2-p2 tri)) (tri2-p2 tri))
                                (hashtable-set! ht (vertex-key (tri2-p3 tri)) (tri2-p3 tri)))
                              smoothed)
                    (list->vector (let-values ([(keys vals) (hashtable-entries ht)]) (vector->list vals))))])
    (make-triangulation all-pts smoothed new-adjacency new-boundary)))

;;; ============================================================
;;; Section: Adaptive Refinement
;;; ============================================================

(doc 'section 'adaptive)

(doc tri2-should-refine? 'type '(-> Triangle2 Number (-> Triangle2 Boolean) Boolean))
(doc tri2-should-refine? 'description "Check if triangle should be refined based on area or custom criterion")
(define (tri2-should-refine? tri max-area custom-pred)
  (or (> (tri2-area tri) max-area)
      (custom-pred tri)))

;;; Priority queue comparator: max-heap by area
;;; Entries are (area . triangle) pairs
(define (area-entry-cmp a b)
  (> (car a) (car b)))

;;; Pop from heap with custom comparator (returns values: new-heap, popped-entry)
(define (heap-pop-by cmp heap)
  (if (heap-empty? heap)
      (error 'heap-pop-by "Cannot pop from empty heap")
      (values (heap-delete-top-by cmp heap)
              (heap-value heap))))

(doc adaptive-refine-mesh 'export #t)
(doc adaptive-refine-mesh 'type '(-> (Or (List Triangle2) Triangulation) Number Nat (-> Triangle2 Boolean) Triangulation))
(doc adaptive-refine-mesh 'description
     "Adaptively refine mesh based on area threshold and custom predicate.
      - max-area: triangles larger than this are refined
      - max-iterations: refinement budget
      - should-refine?: custom predicate for additional refinement criteria
      Inserts circumcenters of triangles needing refinement.
      Uses priority queue for O(log N) per iteration instead of O(N) filtering.")
(define (adaptive-refine-mesh tris-or-tri max-area max-iterations should-refine?)
  (let* ([triangles (if (triangulation? tris-or-tri)
                        (triangulation-triangles tris-or-tri)
                        tris-or-tri)]
         ;; Validity set: tracks which triangles are still in the mesh
         ;; (lazy deletion - triangles removed by Bowyer-Watson are marked invalid)
         [valid (make-eq-hashtable)]
         ;; Mark all initial triangles as valid
         [_ (for-each (lambda (t) (hashtable-set! valid t #t)) triangles)]
         ;; Build initial max-heap of bad triangles ordered by area
         [initial-bad (filter (lambda (t) (tri2-should-refine? t max-area should-refine?)) triangles)]
         [initial-heap (list->heap-by area-entry-cmp
                                      (map (lambda (t) (cons (tri2-area t) t)) initial-bad))]
         ;; Main refinement loop using priority queue
         [final-tris
          (let loop ([tris triangles] [h initial-heap] [iter 0] [skipped-set (make-eq-hashtable)])
            (cond
              ;; Budget exhausted
              [(>= iter max-iterations) tris]
              ;; No more bad triangles
              [(heap-empty? h) tris]
              [else
               ;; Pop largest bad triangle from heap (using custom comparator)
               (let-values ([(h2 entry) (heap-pop-by area-entry-cmp h)])
                 (let ([worst (cdr entry)])
                   (cond
                     ;; Triangle was removed by earlier iteration (lazy deletion)
                     [(not (hashtable-ref valid worst #f))
                      (loop tris h2 iter skipped-set)]
                     ;; Triangle was marked as degenerate (skip to avoid busy loop)
                     [(hashtable-ref skipped-set worst #f)
                      (loop tris h2 iter skipped-set)]
                     [else
                      (let ([cc (tri2-circumcenter worst)])
                        (if cc
                            ;; Insert circumcenter and track changes
                            (let-values ([(new-tris removed added) (delaunay-insert-point-diff tris cc)])
                              ;; Mark removed triangles as invalid
                              (for-each (lambda (t) (hashtable-set! valid t #f)) removed)
                              ;; Mark added triangles as valid
                              (for-each (lambda (t) (hashtable-set! valid t #t)) added)
                              ;; Add bad new triangles to heap (only check new triangles!)
                              (let* ([new-bad (filter (lambda (t) (tri2-should-refine? t max-area should-refine?)) added)]
                                     [h3 (fold-left
                                          (lambda (hp t) (heap-insert-by area-entry-cmp (cons (tri2-area t) t) hp))
                                          h2
                                          new-bad)])
                                (loop new-tris h3 (+ iter 1) skipped-set)))
                            ;; Degenerate circumcenter - mark triangle to skip
                            (begin
                              (hashtable-set! skipped-set worst #t)
                              (loop tris h2 (+ iter 1) skipped-set))))])))]))]
         [adjacency (build-adjacency final-tris)]
         [boundary (find-boundary-edges final-tris adjacency)]
         [all-pts (let ([ht (make-hashtable equal-hash equal?)])
                    (for-each (lambda (tri)
                                (hashtable-set! ht (vertex-key (tri2-p1 tri)) (tri2-p1 tri))
                                (hashtable-set! ht (vertex-key (tri2-p2 tri)) (tri2-p2 tri))
                                (hashtable-set! ht (vertex-key (tri2-p3 tri)) (tri2-p3 tri)))
                              final-tris)
                    (list->vector (let-values ([(keys vals) (hashtable-entries ht)]) (vector->list vals))))])
    (make-triangulation all-pts final-tris adjacency boundary)))

(doc refine-mesh-uniform 'export #t)
(doc refine-mesh-uniform 'type '(-> (Or (List Triangle2) Triangulation) Number Nat Triangulation))
(doc refine-mesh-uniform 'description
     "Uniformly refine mesh until all triangles are below max-area threshold.
      Simpler interface to adaptive-refine-mesh with no custom predicate.")
(define (refine-mesh-uniform tris-or-tri max-area max-iterations)
  (adaptive-refine-mesh tris-or-tri max-area max-iterations (lambda (_) #f)))

;;; ============================================================
;;; Section: Boundary-Constrained Meshing
;;; ============================================================

(doc 'section 'boundary-constrained)

(doc point-in-polygon? 'export #t)
(doc point-in-polygon? 'type '(-> Point2 (List Point2) Boolean))
(doc point-in-polygon? 'description
     "Test if point is inside polygon using ray-casting algorithm.
      Polygon should be a list of vertices in order (CCW or CW).")
(define (point-in-polygon? p polygon)
  (let ([x (point2-x p)]
        [y (point2-y p)]
        [n (length polygon)])
    (let loop ([i 0] [inside? #f])
      (if (>= i n)
          inside?
          (let* ([j (modulo (+ i 1) n)]
                 [pi (list-ref polygon i)]
                 [pj (list-ref polygon j)]
                 [xi (point2-x pi)] [yi (point2-y pi)]
                 [xj (point2-x pj)] [yj (point2-y pj)]
                 ;; Ray casting: check if horizontal ray from p crosses edge i->j
                 [crosses? (and (not (eq? (> yi y) (> yj y)))
                               (< x (+ xi (/ (* (- xj xi) (- y yi))
                                             (- yj yi)))))])
            (loop (+ i 1) (if crosses? (not inside?) inside?)))))))

(doc polygon-centroid 'type '(-> (List Point2) Point2))
(doc polygon-centroid 'description "Compute centroid of polygon")
(define (polygon-centroid polygon)
  (let ([n (length polygon)])
    (if (zero? n)
        (make-point2 0 0)
        (make-point2 (/ (apply + (map point2-x polygon)) n)
                     (/ (apply + (map point2-y polygon)) n)))))

(doc polygon-area 'type '(-> (List Point2) Number))
(doc polygon-area 'description "Compute signed area of polygon (positive if CCW)")
(define (polygon-area polygon)
  (let ([n (length polygon)])
    (if (< n 3)
        0
        (* 0.5
           (let loop ([i 0] [sum 0])
             (if (>= i n)
                 sum
                 (let* ([j (modulo (+ i 1) n)]
                        [pi (list-ref polygon i)]
                        [pj (list-ref polygon j)])
                   (loop (+ i 1)
                         (+ sum
                            (- (* (point2-x pi) (point2-y pj))
                               (* (point2-x pj) (point2-y pi))))))))))))

(doc generate-interior-points 'type '(-> (List Point2) Number (List Point2)))
(doc generate-interior-points 'description
     "Generate Poisson-like distributed points inside polygon with target spacing")
(define (generate-interior-points polygon spacing)
  (let* ([xs (map point2-x polygon)]
         [ys (map point2-y polygon)]
         [x-min (apply min xs)]
         [x-max (apply max xs)]
         [y-min (apply min ys)]
         [y-max (apply max ys)]
         [pts '()])
    ;; Generate grid of candidate points
    (let x-loop ([x (+ x-min (/ spacing 2))] [result '()])
      (if (> x x-max)
          result
          (x-loop (+ x spacing)
                  (let y-loop ([y (+ y-min (/ spacing 2))] [inner result])
                    (if (> y y-max)
                        inner
                        (y-loop (+ y spacing)
                               (let ([p (make-point2 x y)])
                                 (if (point-in-polygon? p polygon)
                                     (cons p inner)
                                     inner))))))))))

(doc polygon-to-constraint-edges 'type '(-> (List Point2) (List Edge)))
(doc polygon-to-constraint-edges 'description
     "Convert polygon vertices to list of boundary constraint edges")
(define (polygon-to-constraint-edges polygon)
  (let ([n (length polygon)])
    (if (< n 2)
        '()
        (let loop ([i 0] [edges '()])
          (if (>= i n)
              edges
              (let* ([p1 (list-ref polygon i)]
                     [p2 (list-ref polygon (modulo (+ i 1) n))])
                (loop (+ i 1) (cons (make-edge p1 p2) edges))))))))

(doc triangulate-polygon 'export #t)
(doc triangulate-polygon 'type '(-> (List Point2) Number Triangulation))
(doc triangulate-polygon 'description
     "Generate constrained Delaunay triangulation inside a polygon boundary.
      - polygon: list of boundary vertices in order
      - spacing: approximate interior point spacing
      Returns triangulation covering polygon interior only.

      Uses constraint-aware filtering: triangles that cross boundary edges
      are removed before centroid-based interior filtering. This correctly
      handles concave polygons.")
(define (triangulate-polygon polygon spacing)
  (if (< (length polygon) 3)
      (make-triangulation (list->vector polygon) '() (make-eq-hashtable) '())
      (let* ([interior-pts (generate-interior-points polygon spacing)]
             [all-pts (append polygon interior-pts)]
             [full-tri (delaunay-triangulate all-pts)]
             [all-tris (triangulation-triangles full-tri)]
             ;; Build constraint edges from polygon boundary
             [constraint-edges (polygon-to-constraint-edges polygon)]
             ;; Step 1: Remove triangles that cross any constraint edge
             [non-crossing-tris
              (filter (lambda (tri)
                        (not (triangle-crosses-any-constraint? tri constraint-edges)))
                      all-tris)]
             ;; Step 2: Filter to triangles whose centroid is inside polygon
             [inside-tris
              (filter (lambda (tri)
                        (let* ([p1 (tri2-p1 tri)]
                               [p2 (tri2-p2 tri)]
                               [p3 (tri2-p3 tri)]
                               [cx (/ (+ (point2-x p1) (point2-x p2) (point2-x p3)) 3)]
                               [cy (/ (+ (point2-y p1) (point2-y p2) (point2-y p3)) 3)]
                               [centroid (make-point2 cx cy)])
                          (point-in-polygon? centroid polygon)))
                      non-crossing-tris)]
             [adjacency (build-adjacency inside-tris)]
             [boundary (find-boundary-edges inside-tris adjacency)]
             [pts-vec (let ([ht (make-hashtable equal-hash equal?)])
                        (for-each (lambda (tri)
                                    (hashtable-set! ht (vertex-key (tri2-p1 tri)) (tri2-p1 tri))
                                    (hashtable-set! ht (vertex-key (tri2-p2 tri)) (tri2-p2 tri))
                                    (hashtable-set! ht (vertex-key (tri2-p3 tri)) (tri2-p3 tri)))
                                  inside-tris)
                        (list->vector (let-values ([(keys vals) (hashtable-entries ht)]) (vector->list vals))))])
        (make-triangulation pts-vec inside-tris adjacency boundary))))

(doc triangulate-polygon-adaptive 'export #t)
(doc triangulate-polygon-adaptive 'type '(-> (List Point2) Number Number Nat Triangulation))
(doc triangulate-polygon-adaptive 'description
     "Generate mesh inside polygon with adaptive refinement.
      - polygon: list of boundary vertices
      - initial-spacing: starting interior point spacing
      - max-area: target maximum triangle area
      - max-iters: refinement budget
      Uses constraint-aware filtering (no triangles cross boundary edges).")
(define (triangulate-polygon-adaptive polygon initial-spacing max-area max-iters)
  (let* ([initial-mesh (triangulate-polygon polygon initial-spacing)]
         [tris (triangulation-triangles initial-mesh)]
         [constraint-edges (polygon-to-constraint-edges polygon)])
    (if (null? tris)
        initial-mesh
        ;; Refine while keeping triangles inside polygon
        (let loop ([current-tris tris] [iter 0])
          (if (>= iter max-iters)
              (let* ([adjacency (build-adjacency current-tris)]
                     [boundary (find-boundary-edges current-tris adjacency)]
                     [pts-vec (let ([ht (make-hashtable equal-hash equal?)])
                                (for-each (lambda (tri)
                                            (hashtable-set! ht (vertex-key (tri2-p1 tri)) (tri2-p1 tri))
                                            (hashtable-set! ht (vertex-key (tri2-p2 tri)) (tri2-p2 tri))
                                            (hashtable-set! ht (vertex-key (tri2-p3 tri)) (tri2-p3 tri)))
                                          current-tris)
                                (list->vector (let-values ([(keys vals) (hashtable-entries ht)]) (vector->list vals))))])
                (make-triangulation pts-vec current-tris adjacency boundary))
              (let ([large-tris (filter (lambda (t) (> (tri2-area t) max-area)) current-tris)])
                (if (null? large-tris)
                    ;; All triangles are small enough
                    (loop current-tris max-iters)
                    ;; Refine the largest triangle
                    (let* ([worst (car (sort (lambda (a b) (> (tri2-area a) (tri2-area b))) large-tris))]
                           [cc (tri2-circumcenter worst)])
                      (if (and cc (point-in-polygon? cc polygon))
                          ;; Insert circumcenter and re-filter (constraint + centroid checks)
                          (let* ([new-tris (delaunay-insert-point current-tris cc)]
                                 [filtered
                                  (filter (lambda (tri)
                                            (and (not (triangle-crosses-any-constraint? tri constraint-edges))
                                                 (let* ([p1 (tri2-p1 tri)]
                                                        [p2 (tri2-p2 tri)]
                                                        [p3 (tri2-p3 tri)]
                                                        [cx (/ (+ (point2-x p1) (point2-x p2) (point2-x p3)) 3)]
                                                        [cy (/ (+ (point2-y p1) (point2-y p2) (point2-y p3)) 3)])
                                                   (point-in-polygon? (make-point2 cx cy) polygon))))
                                          new-tris)])
                            (loop filtered (+ iter 1)))
                          ;; Circumcenter outside polygon, skip this triangle
                          (loop current-tris (+ iter 1)))))))))))

;;; ============================================================
;;; Section: Mesh to 3D Conversion
;;; ============================================================

(doc 'section 'conversion)

(doc triangles-to-3d 'export #t)
(doc triangles-to-3d 'type '(-> (Or (List Triangle2) Triangulation) (-> Point2 Number) (List Triangle3)))
(doc triangles-to-3d 'description "Convert 2D triangles to 3D using height function")
(define (triangles-to-3d tris-or-tri height-fn)
  (let ([tris (if (triangulation? tris-or-tri)
                  (triangulation-triangles tris-or-tri)
                  tris-or-tri)])
  (map (lambda (tri)
         (let* ([p1 (tri2-p1 tri)]
                [p2 (tri2-p2 tri)]
                [p3 (tri2-p3 tri)]
                [v1 (vec3 (point2-x p1) (height-fn p1) (point2-y p1))]
                [v2 (vec3 (point2-x p2) (height-fn p2) (point2-y p2))]
                [v3 (vec3 (point2-x p3) (height-fn p3) (point2-y p3))])
           (triangle3 v1 v2 v3)))
       tris)))

(doc random-points-in-rect 'export #t)
(doc random-points-in-rect 'type '(-> Number Number Number Number Nat (List Point2)))
(doc random-points-in-rect 'description "Generate random points in rectangle")
(define (random-points-in-rect x-min x-max y-min y-max n)
  (let ([x-range (- x-max x-min)]
        [y-range (- y-max y-min)])
    (let loop ([i 0] [pts '()])
      (if (>= i n)
          pts
          (loop (+ i 1)
                (cons (make-point2 (+ x-min (* (random 1000) (/ x-range 1000.0)))
                                   (+ y-min (* (random 1000) (/ y-range 1000.0))))
                      pts))))))

;;; ============================================================
;;; Section: Mesh Visualization (2D ASCII)
;;; ============================================================

(doc 'section 'visualization)

(doc render-mesh-2d 'export #t)
(doc render-mesh-2d 'type '(-> (Or (List Triangle2) Triangulation) Nat Nat String))
(doc render-mesh-2d 'description "Render 2D mesh as ASCII art")
(define (render-mesh-2d tris-or-tri width height)
  (let ([triangles (if (triangulation? tris-or-tri)
                       (triangulation-triangles tris-or-tri)
                       tris-or-tri)])
  (if (null? triangles)
      "Empty mesh\n"
      (let* ([all-points (append-map tri2-points triangles)]
             [xs (map point2-x all-points)]
             [ys (map point2-y all-points)]
             [x-min (apply min xs)]
             [x-max (apply max xs)]
             [y-min (apply min ys)]
             [y-max (apply max ys)]
             [x-range (max 0.001 (- x-max x-min))]
             [y-range (max 0.001 (- y-max y-min))]
             [buffer (make-vector (* width height) #\space)])
        ;; Scale point to buffer coordinates
        (define (scale-point p)
          (let ([x (inexact->exact (round (* (- width 1) (/ (- (point2-x p) x-min) x-range))))]
                [y (inexact->exact (round (* (- height 1) (/ (- (point2-y p) y-min) y-range))))])
            (cons (max 0 (min (- width 1) x))
                  (max 0 (min (- height 1) y)))))
        ;; Draw line between two points
        (define (draw-line p1 p2)
          (let* ([c1 (scale-point p1)]
                 [c2 (scale-point p2)]
                 [x1 (car c1)] [y1 (cdr c1)]
                 [x2 (car c2)] [y2 (cdr c2)]
                 [dx (- x2 x1)]
                 [dy (- y2 y1)]
                 [steps (max 1 (max (abs dx) (abs dy)))])
            (do ([i 0 (+ i 1)])
                ((> i steps))
              (let* ([t (/ i steps)]
                     [x (inexact->exact (round (+ x1 (* t dx))))]
                     [y (inexact->exact (round (+ y1 (* t dy))))]
                     [idx (+ x (* y width))])
                (when (and (>= idx 0) (< idx (* width height)))
                  (vector-set! buffer idx #\.))))))
        ;; Draw all triangle edges
        (for-each (lambda (tri)
                    (draw-line (tri2-p1 tri) (tri2-p2 tri))
                    (draw-line (tri2-p2 tri) (tri2-p3 tri))
                    (draw-line (tri2-p3 tri) (tri2-p1 tri)))
                  triangles)
        ;; Mark vertices
        (for-each (lambda (p)
                    (let* ([c (scale-point p)]
                           [idx (+ (car c) (* (cdr c) width))])
                      (when (and (>= idx 0) (< idx (* width height)))
                        (vector-set! buffer idx #\*))))
                  all-points)
        ;; Convert to string
        (let loop ([y 0] [result '()])
          (if (>= y height)
              (apply string-append (reverse result))
              (loop (+ y 1)
                    (cons (string-append
                           (list->string
                            (let row-loop ([x 0] [row '()])
                              (if (>= x width)
                                  (reverse row)
                                  (row-loop (+ x 1)
                                            (cons (vector-ref buffer (+ x (* y width))) row)))))
                           "\n")
                          result))))))))

(printf "mesh-gen.ss loaded~n")
(printf "  (delaunay-triangulate points)     - Bowyer-Watson triangulation~n")
(printf "  (triangulation-triangles tri)     - Extract triangle list~n")
(printf "  (locate-point tri pt [hint])      - Find containing triangle O(√n)~n")
(printf "  (interpolate-at tri pt vals)      - Barycentric interpolation~n")
(printf "  (tri2-aspect-ratio tri)           - Quality metric~n")
(printf "  (mesh-quality-report tris)        - Print statistics~n")
(printf "  (refine-mesh tris angle iters)    - Ruppert refinement~n")
(printf "  (smooth-mesh tris iters weight)   - Laplacian smoothing~n")
(printf "  (adaptive-refine-mesh ...)        - Area-based refinement~n")
(printf "  (triangulate-polygon poly space)  - Mesh inside polygon~n")
(printf "  (point-in-polygon? pt poly)       - Point containment test~n")
(printf "  (triangles-to-3d tris height-fn)  - Convert to 3D~n")
(printf "  (render-mesh-2d tris w h)         - ASCII visualization~n")
