(load "core/base/prelude.ss")
(load "lattice/geometry/geometry.ss")

(doc 'module 'octree)
(doc 'description "Octree spatial partitioning for geometric query acceleration")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'provides "Octree data structure recursively subdividing 3D space into 8 octants")
(doc 'note "Compared to BVH: Octree uses uniform spatial subdivision (simpler, better for uniform distributions), BVH uses object-space partitioning (more complex, better for non-uniform distributions)")

(doc 'section 'octree-node-structure)
(doc 'note "Octree node types: Leaf (octree-leaf center size primitives), Internal (octree-node center size children where children is vector of 8 octrees)")

(define (octree-leaf center size primitives)
  (doc 'type '(-> Vec3 Real (List Triangle3) Octree))
  (list 'octree-leaf center size primitives))

(define (octree-node center size children)
  (doc 'type '(-> Vec3 Real (Vector Octree) Octree))
  (list 'octree-node center size children))

(define (octree-leaf? node)
  (doc 'type '(-> α Bool))
  (and (pair? node) (eq? (car node) 'octree-leaf)))

(define (octree-node? node)
  (doc 'type '(-> α Bool))
  (and (pair? node) (eq? (car node) 'octree-node)))

(define (octree-center node)
  (doc 'type '(-> Octree Vec3))
  (cadr node))

(define (octree-size node)
  (doc 'type '(-> Octree Real))
  (caddr node))

(define (octree-primitives node)
  (doc 'type '(-> Octree (List Triangle3)))
  (if (octree-leaf? node)
      (cadddr node)
      '()))

(define (octree-children node)
  (doc 'type '(-> Octree (Maybe (Vector Octree))))
  (if (octree-node? node)
      (cadddr node)
      #f))

(doc 'section 'octree-construction)

(define (octree-build triangles center size max-depth max-leaf-size)
  (doc 'type '(-> (List Triangle3) Point3 Number Number Number Octree))
  (doc 'description "Build an octree from triangles")
  (doc 'param 'center "Center of root node")
  (doc 'param 'size "Half-size of root node")
  (doc 'param 'max-depth "Maximum subdivision depth")
  (doc 'param 'max-leaf-size "Maximum triangles per leaf")
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

(define (all-children-leaves? children)
  (doc 'type '(-> (List Octree) Bool))
  (and (= (length children) 8)
       (andmap octree-leaf? children)))

(define (subdivide-octants center size)
  (doc 'type '(-> Vec3 Real (List (Pair Vec3 Real))))
  (doc 'description "Return 8 octant centers and sizes")
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

(define (triangle-intersects-octant? tri center size)
  (doc 'type '(-> Triangle3 Vec3 Real Bool))
  (doc 'description "Check if triangle intersects octant (cube centered at point with half-size)")
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

(define (aabb-overlaps? a b)
  (doc 'type '(-> AABB AABB Bool))
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

(define (partition-triangles-octants triangles octants)
  (doc 'type '(-> (List Triangle3) (List (Pair Vec3 Real)) (List (List Triangle3))))
  (doc 'description "Partition triangles into 8 lists (one per octant)")
  (map (lambda (octant)
               (let ([center (car octant)]
                     [size (cadr octant)])
                    (filter (lambda (tri)
                                    (triangle-intersects-octant? tri center size))
                            triangles)))
       octants))

(define (any pred lst)
  (doc 'type '(-> (-> α Bool) (List α) Bool))
  (and (not (null? lst))
       (or (pred (car lst))
           (any pred (cdr lst)))))

(doc 'section 'octree-queries)

(define (octree-intersect-ray octree ray)
  (doc 'type '(-> Octree Ray3 (Maybe (Pair Triangle3 Number))))
  (doc 'description "Find closest triangle intersection along ray using octree")
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

(define (ray-intersects-octant? ray center size)
  (doc 'type '(-> Ray3 Vec3 Real Bool))
  (let ([octant-aabb (aabb (vec3 (- (vec3-x center) size)
                                 (- (vec3-y center) size)
                                 (- (vec3-z center) size))
                           (vec3 (+ (vec3-x center) size)
                                 (+ (vec3-y center) size)
                                 (+ (vec3-z center) size)))])
       (intersect-ray-aabb ray octant-aabb)))

(doc 'section 'octree-statistics)

(define (octree-depth octree)
  (doc 'type '(-> Octree Number))
  (cond
   [(not octree) 0]
   [(octree-leaf? octree) 1]
   [(octree-node? octree)
    (+ 1 (apply max (map octree-depth
                         (vector->list (octree-children octree)))))]
   [else 0]))

(define (octree-count-nodes octree)
  (doc 'type '(-> Octree Number))
  (cond
   [(not octree) 0]
   [(octree-leaf? octree) 1]
   [(octree-node? octree)
    (+ 1 (apply + (map octree-count-nodes
                       (vector->list (octree-children octree)))))]
   [else 0]))

(define (octree-count-leaves octree)
  (doc 'type '(-> Octree Number))
  (cond
   [(not octree) 0]
   [(octree-leaf? octree) 1]
   [(octree-node? octree)
    (apply + (map octree-count-leaves
                  (vector->list (octree-children octree))))]
   [else 0]))

(define (octree-count-triangles octree)
  (doc 'type '(-> Octree Number))
  (cond
   [(not octree) 0]
   [(octree-leaf? octree) (length (octree-primitives octree))]
   [(octree-node? octree)
    (apply + (map octree-count-triangles
                  (vector->list (octree-children octree))))]
   [else 0]))
