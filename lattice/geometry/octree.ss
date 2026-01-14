;;; core/geometry/octree.ss — Octree Spatial Partitioning
;;;
;;; Provides octree data structure for spatial partitioning and acceleration
;;; of geometric queries. An octree recursively subdivides 3D space into 8 octants.
;;;
;;; Compared to BVH:
;;; - Octree: uniform spatial subdivision, simpler to build, better for uniform distributions
;;; - BVH: object-space partitioning, more complex, better for non-uniform distributions
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - geometry.ss

(load "core/base/prelude.ss")
(load "lattice/geometry/geometry.ss")

;;; ====
;;; Octree Node Structure
;;; ====

;;; Octree node types:
;;; - Leaf: (octree-leaf center size primitives)
;;; - Internal: (octree-node center size children)
;;; children is a vector of 8 octrees (one per octant)

;;; octree-leaf : Vec3 × Real × (List Triangle3) → Octree
(define (octree-leaf center size primitives)
  (list 'octree-leaf center size primitives))

;;; octree-node : Vec3 × Real × (Vector Octree) → Octree
(define (octree-node center size children)
  (list 'octree-node center size children))

;;; octree-leaf? : α → Bool
(define (octree-leaf? node)
  (and (pair? node) (eq? (car node) 'octree-leaf)))

;;; octree-node? : α → Bool
(define (octree-node? node)
  (and (pair? node) (eq? (car node) 'octree-node)))

;;; octree-center : Octree → Vec3
(define (octree-center node)
  (cadr node))

;;; octree-size : Octree → Real
(define (octree-size node)
  (caddr node))

;;; octree-primitives : Octree → (List Triangle3)
(define (octree-primitives node)
  (if (octree-leaf? node)
      (cadddr node)
      '()))

;;; octree-children : Octree → (Vector Octree) | #f
(define (octree-children node)
  (if (octree-node? node)
      (cadddr node)
      #f))

;;; ====
;;; Octree Construction
;;; ====

;;; octree-build : (List Triangle3) × Point3 × Number × Number × Number → Octree
;;; Build an octree from triangles
;;; center: center of root node
;;; size: half-size of root node
;;; max-depth: maximum subdivision depth
;;; max-leaf-size: maximum triangles per leaf
(define (octree-build triangles center size max-depth max-leaf-size)
  (define (build-recursive tris ctr sz depth)
    (cond
     ;; Base case: max depth or few enough triangles
     [(or (>= depth max-depth) (<= (length tris) max-leaf-size))
      (octree-leaf ctr sz tris)]
     
     ;; Recursive case: subdivide into 8 octants
     [else
      (let* ([half-size (* sz 0.5)]
             [octants (subdivide-octants ctr sz)]
             [tri-sets (partition-triangles-octants tris octants)]
             [children (map (lambda (octant tri-set)
                                    (let ([oct-center (car octant)])
                                         (build-recursive tri-set oct-center half-size (+ depth 1))))
                            octants
                            tri-sets)])
            ;; FIX (fold-lcme): Only collapse if partitioning was ineffective
            ;; Previous logic collapsed when children were leaves, losing benefit
            (let ([total (apply + (map (lambda (c) (length (octree-primitives c))) children))])
                 ;; Collapse only if every triangle in every octant (no benefit)
                 (if (>= total (* 8 (length tris)))
                     (octree-leaf ctr sz tris)
                     ;; Keep internal node - partitioning reduced search space
                     (octree-node ctr sz (list->vector children)))))]])
  
  (build-recursive triangles center size 0))

;;; all-children-leaves? : (List Octree) → Bool
(define (all-children-leaves? children)
  (and (= (length children) 8)
       (andmap octree-leaf? children)))

;;; subdivide-octants : Vec3 × Real → (List (Vec3 × Real))
;;; Return 8 octant centers and sizes
(define (subdivide-octants center size)
  (let* ([cx (vec3-x center)]
         [cy (vec3-y center)]
         [cz (vec3-z center)]
         [hs (* size 0.5)])  ; half of current half-size = quarter-size offset
        (list
         ;; Octant ordering: ---,  --+, -+-, -++, +--,  +-+, ++-, +++
         (list (vec3 (- cx hs) (- cy hs) (- cz hs)) hs)  ; 0: ---
         (list (vec3 (- cx hs) (- cy hs) (+ cz hs)) hs)  ; 1: --+
         (list (vec3 (- cx hs) (+ cy hs) (- cz hs)) hs)  ; 2: -+-
         (list (vec3 (- cx hs) (+ cy hs) (+ cz hs)) hs)  ; 3: -++
         (list (vec3 (+ cx hs) (- cy hs) (- cz hs)) hs)  ; 4: +--
         (list (vec3 (+ cx hs) (- cy hs) (+ cz hs)) hs)  ; 5: +-+
         (list (vec3 (+ cx hs) (+ cy hs) (- cz hs)) hs)  ; 6: ++-
         (list (vec3 (+ cx hs) (+ cy hs) (+ cz hs)) hs)))) ; 7: +++

;;; triangle-intersects-octant? : Triangle3 × Vec3 × Real → Bool
;;; Check if triangle intersects octant (cube centered at point with half-size)
(define (triangle-intersects-octant? tri center size)
  (let* ([octant-aabb (aabb (vec3 (- (vec3-x center) size)
                                  (- (vec3-y center) size)
                                  (- (vec3-z center) size))
                            (vec3 (+ (vec3-x center) size)
                                  (+ (vec3-y center) size)
                                  (+ (vec3-z center) size)))]
         [tri-points (list (triangle3-p1 tri)
                           (triangle3-p2 tri)
                           (triangle3-p3 tri))])
        ;; Simple test: check if any vertex is inside, or if bbox overlaps
        (or (any (lambda (p) (point-in-aabb? p octant-aabb)) tri-points)
            (let ([tri-bbox (aabb-from-points tri-points)])
                 (aabb-overlaps? tri-bbox octant-aabb)))))

;;; aabb-overlaps? : AABB × AABB → Bool
(define (aabb-overlaps? a b)
  (let ([amin (aabb-min a)]
        [amax (aabb-max a)]
        [bmin (aabb-min b)]
        [bmax (aabb-max b)])
       (and (>= (vec3-x amax) (vec3-x bmin))
            (<= (vec3-x amin) (vec3-x bmax))
            (>= (vec3-y amax) (vec3-y bmin))
            (<= (vec3-y amin) (vec3-y bmax))
            (>= (vec3-z amax) (vec3-z bmin))
            (<= (vec3-z amin) (vec3-z bmax)))))

;;; partition-triangles-octants : (List Triangle3) × (List (Vec3 × Real)) → (List (List Triangle3))
;;; Partition triangles into 8 lists (one per octant)
(define (partition-triangles-octants triangles octants)
  (map (lambda (octant)
               (let ([center (car octant)]
                     [size (cadr octant)])
                    (filter (lambda (tri)
                                    (triangle-intersects-octant? tri center size))
                            triangles)))
       octants))

;;; any : (α → Bool) × (List α) → Bool
(define (any pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (any pred (cdr lst)))))

;;; ====
;;; Octree Queries
;;; ====

;;; octree-intersect-ray : Octree × Ray3 → (Triangle3 Number) | #f
;;; Find closest triangle intersection along ray using octree
(define (octree-intersect-ray octree ray)
  (define (traverse node closest-t closest-tri)
    (cond
     [(not node)
      (if closest-tri (list closest-tri closest-t) #f)]
     
     ;; Check if ray intersects node's bbox
     [(not (ray-intersects-octant? ray (octree-center node) (octree-size node)))
      (if closest-tri (list closest-tri closest-t) #f)]
     
     ;; Leaf node: test all triangles
     [(octree-leaf? node)
      (let loop ([tris (octree-primitives node)]
                 [best-t closest-t]
                 [best-tri closest-tri])
           (if (null? tris)
               (if best-tri (list best-tri best-t) #f)
               (let* ([tri (car tris)]
                      [t (intersect-ray-triangle ray tri)])
                     (if (and t (or (not best-t) (< t best-t)))
                         (loop (cdr tris) t tri)
                         (loop (cdr tris) best-t best-tri)))))]
     
     ;; Internal node: traverse children
     [(octree-node? node)
      (let ([children (vector->list (octree-children node))])
           (fold-left (lambda (result child)
                              (let ([tri (if result (car result) closest-tri)]
                                    [t (if result (cadr result) closest-t)])
                                   (or (traverse child t tri) result)))
                      (if closest-tri (list closest-tri closest-t) #f)
                      children))]
     
     [else (if closest-tri (list closest-tri closest-t) #f)]))
  
  (traverse octree #f #f))

;;; ray-intersects-octant? : Ray3 × Vec3 × Real → Bool
(define (ray-intersects-octant? ray center size)
  (let ([octant-aabb (aabb (vec3 (- (vec3-x center) size)
                                 (- (vec3-y center) size)
                                 (- (vec3-z center) size))
                           (vec3 (+ (vec3-x center) size)
                                 (+ (vec3-y center) size)
                                 (+ (vec3-z center) size)))])
       (intersect-ray-aabb ray octant-aabb)))

;;; ====
;;; Octree Statistics
;;; ====

;;; octree-depth : Octree → Number
(define (octree-depth octree)
  (cond
   [(not octree) 0]
   [(octree-leaf? octree) 1]
   [(octree-node? octree)
    (+ 1 (apply max (map octree-depth
                         (vector->list (octree-children octree)))))]
   [else 0]))

;;; octree-count-nodes : Octree → Number
(define (octree-count-nodes octree)
  (cond
   [(not octree) 0]
   [(octree-leaf? octree) 1]
   [(octree-node? octree)
    (+ 1 (apply + (map octree-count-nodes
                       (vector->list (octree-children octree)))))]
   [else 0]))

;;; octree-count-leaves : Octree → Number
(define (octree-count-leaves octree)
  (cond
   [(not octree) 0]
   [(octree-leaf? octree) 1]
   [(octree-node? octree)
    (apply + (map octree-count-leaves
                  (vector->list (octree-children octree))))]
   [else 0]))

;;; octree-count-triangles : Octree → Number
(define (octree-count-triangles octree)
  (cond
   [(not octree) 0]
   [(octree-leaf? octree) (length (octree-primitives octree))]
   [(octree-node? octree)
    (apply + (map octree-count-triangles
                  (vector->list (octree-children octree))))]
   [else 0]))
