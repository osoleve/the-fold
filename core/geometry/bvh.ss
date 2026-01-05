;;; core/geometry/bvh.ss — Bounding Volume Hierarchy for Spatial Acceleration
;;;
;;; Provides a BVH (Bounding Volume Hierarchy) implementation for accelerating
;;; geometric queries (ray-triangle intersection, nearest point, etc.).
;;;
;;; A BVH is a tree structure where:
;;; - Each node contains an AABB (axis-aligned bounding box)
;;; - Internal nodes have two children (left and right subtrees)
;;; - Leaf nodes contain a list of primitives (triangles)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - geometry.ss

(load "core/base/prelude.ss")
(load "core/geometry/geometry.ss")

;;; ============================================================
;;; BVH Node Structure
;;; ============================================================

;;; BVH node types:
;;; - Leaf: (bvh-leaf bbox primitives)
;;; - Internal: (bvh-node bbox left right)

;;; bvh-leaf : AABB × (List Triangle3) → BVH
(define (bvh-leaf bbox primitives)
  (list 'bvh-leaf bbox primitives))

;;; bvh-node : AABB × BVH × BVH → BVH
(define (bvh-node bbox left right)
  (list 'bvh-node bbox left right))

;;; bvh-leaf? : α → Bool
(define (bvh-leaf? node)
  (and (pair? node) (eq? (car node) 'bvh-leaf)))

;;; bvh-node? : α → Bool
(define (bvh-node? node)
  (and (pair? node) (eq? (car node) 'bvh-node)))

;;; bvh-bbox : BVH → AABB
(define (bvh-bbox node)
  (cadr node))

;;; bvh-primitives : BVH → (List Triangle3)
(define (bvh-primitives node)
  (if (bvh-leaf? node)
      (caddr node)
      '()))

;;; bvh-left : BVH → BVH | #f
(define (bvh-left node)
  (if (bvh-node? node)
      (caddr node)
      #f))

;;; bvh-right : BVH → BVH | #f
(define (bvh-right node)
  (if (bvh-node? node)
      (cadddr node)
      #f))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; triangle-centroid : Triangle3 → Point3
(define (triangle-centroid tri)
  (let ([p1 (triangle3-p1 tri)]
        [p2 (triangle3-p2 tri)]
        [p3 (triangle3-p3 tri)])
       (vec3-scale (vec3-add (vec3-add p1 p2) p3) (/ 1.0 3.0))))

;;; compute-triangles-bbox : (List Triangle3) → AABB
;;; Compute bounding box containing all triangle vertices
(define (compute-triangles-bbox triangles)
  (if (null? triangles)
      (aabb (vec3-zero) (vec3-zero))
      (let ([points (apply append
                           (map (lambda (tri)
                                        (list (triangle3-p1 tri)
                                              (triangle3-p2 tri)
                                              (triangle3-p3 tri)))
                                triangles))])
           (aabb-from-points points))))

;;; longest-axis : AABB → Nat
;;; Returns 0 for X, 1 for Y, 2 for Z
(define (longest-axis bbox)
  (let* ([extents (aabb-extents bbox)]
         [x (vec3-x extents)]
         [y (vec3-y extents)]
         [z (vec3-z extents)])
        (cond
         [(and (>= x y) (>= x z)) 0]
         [(and (>= y x) (>= y z)) 1]
         [else 2])))

;;; get-axis-coord : Vec3 × Nat → Real
(define (get-axis-coord point axis)
  (case axis
        [(0) (vec3-x point)]
        [(1) (vec3-y point)]
        [(2) (vec3-z point)]
        [else (vec3-x point)]))

;;; ============================================================
;;; BVH Construction
;;; ============================================================

;;; bvh-build : (List Triangle3) × Number → BVH
;;; Build a BVH from a list of triangles
;;; max-leaf-size: maximum number of triangles in a leaf node
(define (bvh-build triangles max-leaf-size)
  (if (<= (length triangles) max-leaf-size)
      ;; Base case: create leaf node
      (let ([bbox (compute-triangles-bbox triangles)])
           (bvh-leaf bbox triangles))
      ;; Recursive case: split and build subtrees
      (let* ([bbox (compute-triangles-bbox triangles)]
             [axis (longest-axis bbox)]
             [centroids (map triangle-centroid triangles)]
             ;; Sort triangles by centroid along chosen axis
             [sorted-tris (map car
                               (list-sort
                                (lambda (a b)
                                        (< (get-axis-coord (cadr a) axis)
                                           (get-axis-coord (cadr b) axis)))
                                (map list triangles centroids)))]
             [n (length sorted-tris)]
             [mid (quotient n 2)]
             [left-tris (list-head sorted-tris mid)]
             [right-tris (list-tail sorted-tris mid)])
            (if (or (null? left-tris) (null? right-tris))
                ;; Split failed, force leaf (degenerate case)
                (bvh-leaf bbox triangles)
                ;; Build subtrees
                (let ([left-child (bvh-build left-tris max-leaf-size)]
                      [right-child (bvh-build right-tris max-leaf-size)])
                     (bvh-node bbox left-child right-child))))))

;;; ============================================================
;;; BVH Traversal
;;; ============================================================

;;; bvh-intersect-ray : BVH × Ray3 → (Triangle3 Number) | #f
;;; Find closest triangle intersection along ray
;;; Returns (triangle t-value) or #f if no intersection
(define (bvh-intersect-ray bvh ray)
  (define (traverse node closest-t closest-tri)
    (cond
     [(not node)
      (if closest-tri (list closest-tri closest-t) #f)]
     
     ;; Check if ray intersects node's bbox
     [(not (intersect-ray-aabb ray (bvh-bbox node)))
      (if closest-tri (list closest-tri closest-t) #f)]
     
     ;; Leaf node: test all triangles
     [(bvh-leaf? node)
      (let loop ([tris (bvh-primitives node)]
                 [best-t closest-t]
                 [best-tri closest-tri])
           (if (null? tris)
               (if best-tri (list best-tri best-t) #f)
               (let* ([tri (car tris)]
                      [t (intersect-ray-triangle ray tri)])
                     (if (and t (or (not best-t) (< t best-t)))
                         (loop (cdr tris) t tri)
                         (loop (cdr tris) best-t best-tri)))))]
     
     ;; Internal node: traverse both children
     [(bvh-node? node)
      (let* ([left-result (traverse (bvh-left node) closest-t closest-tri)]
             [new-closest-t (if left-result (cadr left-result) closest-t)]
             [new-closest-tri (if left-result (car left-result) closest-tri)]
             [right-result (traverse (bvh-right node) new-closest-t new-closest-tri)])
            (or right-result left-result))]
     
     [else (if closest-tri (list closest-tri closest-t) #f)]))
  
  (traverse bvh #f #f))

;;; closest-point-on-segment : Point3 × Point3 × Point3 → Point3
;;; Find closest point on line segment AB to point P
(define (closest-point-on-segment p a b)
  (let* ([ab (vec3-sub b a)]
         [ap (vec3-sub p a)]
         [ab-dot (vec3-dot ab ab)])
        (if (< ab-dot 1e-10)
            ;; Degenerate segment
            a
            (let ([t (/ (vec3-dot ap ab) ab-dot)])
                 (cond
                  [(<= t 0) a]
                  [(>= t 1) b]
                  [else (vec3-add a (vec3-scale ab t))])))))

;;; closest-point-on-triangle : Point3 × Triangle3 → Point3
;;; Find the closest point on a triangle to the given point
(define (closest-point-on-triangle point tri)
  (let* ([a (triangle3-p1 tri)]
         [b (triangle3-p2 tri)]
         [c (triangle3-p3 tri)]
         ;; Project point onto triangle plane
         [normal (triangle-normal tri)]
         [dist (vec3-dot normal (vec3-sub point a))]
         [projected (vec3-sub point (vec3-scale normal dist))]
         ;; Get barycentric coordinates
         [bary (barycentric-coords projected a b c)]
         [u (car bary)]
         [v (cadr bary)]
         [w (caddr bary)])
        ;; Check if point is inside triangle
        (if (and (>= u -0.0001) (>= v -0.0001) (>= w -0.0001))
            ;; Inside: return projected point
            projected
            ;; Outside: find closest point on edges
            (let* ([p-ab (closest-point-on-segment point a b)]
                   [p-bc (closest-point-on-segment point b c)]
                   [p-ca (closest-point-on-segment point c a)]
                   [d-ab (distance-point-point point p-ab)]
                   [d-bc (distance-point-point point p-bc)]
                   [d-ca (distance-point-point point p-ca)])
                  (cond
                   [(and (<= d-ab d-bc) (<= d-ab d-ca)) p-ab]
                   [(and (<= d-bc d-ab) (<= d-bc d-ca)) p-bc]
                   [else p-ca])))))

;;; bvh-closest-point : BVH × Point3 → (Point3 Number Triangle3) | #f
;;; Find closest point on any triangle in the BVH to the given point
;;; Returns (closest-point distance triangle) or #f
(define (bvh-closest-point bvh point)
  (define (traverse node best-dist best-point best-tri)
    (cond
     [(not node)
      (if best-tri (list best-point best-dist best-tri) #f)]
     
     ;; Check if point could be closer to anything in this node
     [(let* ([bbox (bvh-bbox node)]
             [closest-on-box (closest-point-on-aabb point bbox)]
             [dist-to-box (distance-point-point point closest-on-box)])
            (and best-dist (>= dist-to-box best-dist)))
      ;; This node can't contain anything closer
      (if best-tri (list best-point best-dist best-tri) #f)]
     
     ;; Leaf node: check all triangles
     [(bvh-leaf? node)
      (let loop ([tris (bvh-primitives node)]
                 [b-dist best-dist]
                 [b-point best-point]
                 [b-tri best-tri])
           (if (null? tris)
               (if b-tri (list b-point b-dist b-tri) #f)
               (let* ([tri (car tris)]
                      [closest (closest-point-on-triangle point tri)]
                      [dist (distance-point-point point closest)])
                     (if (or (not b-dist) (< dist b-dist))
                         (loop (cdr tris) dist closest tri)
                         (loop (cdr tris) b-dist b-point b-tri)))))]
     
     ;; Internal node: traverse both children
     [(bvh-node? node)
      (let* ([left-result (traverse (bvh-left node) best-dist best-point best-tri)]
             [new-best-dist (if left-result (cadr left-result) best-dist)]
             [new-best-point (if left-result (car left-result) best-point)]
             [new-best-tri (if left-result (caddr left-result) best-tri)]
             [right-result (traverse (bvh-right node) new-best-dist new-best-point new-best-tri)])
            (or right-result left-result))]
     
     [else (if best-tri (list best-point best-dist best-tri) #f)]))
  
  (traverse bvh #f #f #f))

;;; ============================================================
;;; BVH Statistics and Utilities
;;; ============================================================

;;; bvh-depth : BVH → Number
;;; Compute the maximum depth of the BVH tree
(define (bvh-depth bvh)
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) 1]
   [(bvh-node? bvh)
    (+ 1 (max (bvh-depth (bvh-left bvh))
              (bvh-depth (bvh-right bvh))))]
   [else 0]))

;;; bvh-count-nodes : BVH → Number
;;; Count total number of nodes in the BVH
(define (bvh-count-nodes bvh)
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) 1]
   [(bvh-node? bvh)
    (+ 1
       (bvh-count-nodes (bvh-left bvh))
       (bvh-count-nodes (bvh-right bvh)))]
   [else 0]))

;;; bvh-count-leaves : BVH → Number
;;; Count number of leaf nodes
(define (bvh-count-leaves bvh)
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) 1]
   [(bvh-node? bvh)
    (+ (bvh-count-leaves (bvh-left bvh))
       (bvh-count-leaves (bvh-right bvh)))]
   [else 0]))

;;; bvh-count-triangles : BVH → Number
;;; Count total number of triangles in the BVH
(define (bvh-count-triangles bvh)
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) (length (bvh-primitives bvh))]
   [(bvh-node? bvh)
    (+ (bvh-count-triangles (bvh-left bvh))
       (bvh-count-triangles (bvh-right bvh)))]
   [else 0]))
