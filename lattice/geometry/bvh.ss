(load "core/base/prelude.ss")
(load "lattice/geometry/geometry.ss")

(doc 'module 'bvh)
(doc 'description "Bounding Volume Hierarchy for spatial acceleration")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'provides "BVH implementation for accelerating geometric queries (ray-triangle intersection, nearest point, etc.)")
(doc 'note "BVH tree structure: Each node contains AABB, internal nodes have left/right children, leaf nodes contain primitives")

(doc 'section 'bvh-structure)

(doc 'note "BVH node types: Leaf (bvh-leaf bbox primitives), Internal (bvh-node bbox left right)")

(define (bvh-leaf bbox primitives)
  (doc 'export #t)
  (doc 'type '(-> AABB (List Triangle3) BVH))
  (list 'bvh-leaf bbox primitives))

(define (bvh-node bbox left right)
  (doc 'export #t)
  (doc 'type '(-> AABB BVH BVH BVH))
  (list 'bvh-node bbox left right))

(define (bvh-leaf? node)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? node) (eq? (car node) 'bvh-leaf)))

(define (bvh-node? node)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? node) (eq? (car node) 'bvh-node)))

(define (bvh-bbox node)
  (doc 'export #t)
  (doc 'type '(-> BVH AABB))
  (cadr node))

(define (bvh-primitives node)
  (doc 'export #t)
  (doc 'type '(-> BVH (List Triangle3)))
  (if (bvh-leaf? node)
      (caddr node)
      '()))

(define (bvh-left node)
  (doc 'export #t)
  (doc 'type '(-> BVH (Or BVH #f)))
  (if (bvh-node? node)
      (caddr node)
      #f))

(define (bvh-right node)
  (doc 'export #t)
  (doc 'type '(-> BVH (Or BVH #f)))
  (if (bvh-node? node)
      (cadddr node)
      #f))

(doc 'section 'helpers)

(define (triangle-centroid tri)
  (doc 'export #t)
  (doc 'type '(-> Triangle3 Point3))
  (let ([p1 (triangle3-p1 tri)]
        [p2 (triangle3-p2 tri)]
        [p3 (triangle3-p3 tri)])
       (vec3-scale (vec3-add (vec3-add p1 p2) p3) (/ 1.0 3.0))))

(define (compute-triangles-bbox triangles)
  (doc 'export #t)
  (doc 'type '(-> (List Triangle3) AABB))
  (doc 'description "Compute bounding box containing all triangle vertices")
  (if (null? triangles)
      (aabb (vec3-zero) (vec3-zero))
      (let ([points (apply append
                           (map (lambda (tri)
                                        (list (triangle3-p1 tri)
                                              (triangle3-p2 tri)
                                              (triangle3-p3 tri)))
                                triangles))])
           (aabb-from-points points))))

(define (longest-axis bbox)
  (doc 'export #t)
  (doc 'type '(-> AABB Nat))
  (doc 'returns "0 for X, 1 for Y, 2 for Z")
  (let* ([extents (aabb-extents bbox)]
         [x (vec3-x extents)]
         [y (vec3-y extents)]
         [z (vec3-z extents)])
        (cond
         [(and (>= x y) (>= x z)) 0]
         [(and (>= y x) (>= y z)) 1]
         [else 2])))

(define (get-axis-coord point axis)
  (doc 'export #t)
  (doc 'type '(-> Vec3 Nat Real))
  (case axis
        [(0) (vec3-x point)]
        [(1) (vec3-y point)]
        [(2) (vec3-z point)]
        [else (vec3-x point)]))

(doc 'section 'bvh-construction)

(define (bvh-build triangles max-leaf-size)
  (doc 'export #t)
  (doc 'type '(-> (List Triangle3) Number BVH))
  (doc 'description "Build a BVH from a list of triangles")
  (doc 'param 'max-leaf-size "maximum number of triangles in a leaf node")
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

(doc 'section 'bvh-traversal)

(doc bvh-intersect-ray 'export #t)
(doc bvh-intersect-ray 'type '(-> BVH Ray3 (Or (List Triangle3 Number) #f)))
(doc bvh-intersect-ray 'description "Find closest triangle intersection along ray")
(doc bvh-intersect-ray 'returns "(triangle t-value) or #f if no intersection")
(define (bvh-intersect-ray bvh ray)
  (letrec ([traverse
            (lambda (node closest-t closest-tri)
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

               [else (if closest-tri (list closest-tri closest-t) #f)]))])
    (traverse bvh #f #f)))

(define (closest-point-on-segment p a b)
  (doc 'export #t)
  (doc 'type '(-> Point3 Point3 Point3 Point3))
  (doc 'description "Find closest point on line segment AB to point P")
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

(define (closest-point-on-triangle point tri)
  (doc 'export #t)
  (doc 'type '(-> Point3 Triangle3 Point3))
  (doc 'description "Find the closest point on a triangle to the given point")
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

(doc bvh-closest-point 'export #t)
(doc bvh-closest-point 'type '(-> BVH Point3 (Or (List Point3 Number Triangle3) #f)))
(doc bvh-closest-point 'description "Find closest point on any triangle in the BVH to the given point")
(doc bvh-closest-point 'returns "(closest-point distance triangle) or #f")
(define (bvh-closest-point bvh point)
  (letrec ([traverse
            (lambda (node best-dist best-point best-tri)
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

               [else (if best-tri (list best-point best-dist best-tri) #f)]))])
    (traverse bvh #f #f #f)))

(doc 'section 'bvh-statistics)

(define (bvh-depth bvh)
  (doc 'export #t)
  (doc 'type '(-> BVH Number))
  (doc 'description "Compute the maximum depth of the BVH tree")
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) 1]
   [(bvh-node? bvh)
    (+ 1 (max (bvh-depth (bvh-left bvh))
              (bvh-depth (bvh-right bvh))))]
   [else 0]))

(define (bvh-count-nodes bvh)
  (doc 'export #t)
  (doc 'type '(-> BVH Number))
  (doc 'description "Count total number of nodes in the BVH")
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) 1]
   [(bvh-node? bvh)
    (+ 1
       (bvh-count-nodes (bvh-left bvh))
       (bvh-count-nodes (bvh-right bvh)))]
   [else 0]))

(define (bvh-count-leaves bvh)
  (doc 'export #t)
  (doc 'type '(-> BVH Number))
  (doc 'description "Count number of leaf nodes")
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) 1]
   [(bvh-node? bvh)
    (+ (bvh-count-leaves (bvh-left bvh))
       (bvh-count-leaves (bvh-right bvh)))]
   [else 0]))

(define (bvh-count-triangles bvh)
  (doc 'export #t)
  (doc 'type '(-> BVH Number))
  (doc 'description "Count total number of triangles in the BVH")
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) (length (bvh-primitives bvh))]
   [(bvh-node? bvh)
    (+ (bvh-count-triangles (bvh-left bvh))
       (bvh-count-triangles (bvh-right bvh)))]
   [else 0]))
