(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module collision-detection
;;; @requires prelude vec2 hamt
(require 'prelude)
(require 'vec2)
(require 'hamt)

(doc 'module 'collision-detection)
(doc 'description "2D Collision Detection with AABB, Circle, and SAT")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'section 'shape)

(doc "--- AABB (Axis-Aligned Bounding Box) ---")

(doc 'make-aabb 'type 'Vec2 × Vec2 → AABB)
(doc "Create an AABB from min and max corners.")
(define (make-aabb min-corner max-corner)
  (doc 'export #t)
  (list 'aabb min-corner max-corner))

(doc "aabb? : Any → Boolean")
(define (aabb? s)
  (doc 'export #t)
  (and (pair? s) (eq? (car s) 'aabb)))

(define (aabb-min a) (list-ref a 1))
  (doc 'type '(aabb-min : AABB → Vec2))

(define (aabb-max a) (list-ref a 2))
  (doc 'type '(aabb-max : AABB → Vec2))

(define (aabb-center a)
  (doc 'export #t)
  (doc 'type '(aabb-center : AABB → Vec2))
  (vec2-scale (vec2-add (aabb-min a) (aabb-max a)) 0.5))

(define (aabb-half-extents a)
  (doc 'export #t)
  (doc 'type '(aabb-half-extents : AABB → Vec2))
  (vec2-scale (vec2-sub (aabb-max a) (aabb-min a)) 0.5))

(define (aabb-width a)
  (doc 'export #t)
  (doc 'type '(aabb-width : AABB → Number))
  (- (vec2-x (aabb-max a)) (vec2-x (aabb-min a))))

(define (aabb-height a)
  (doc 'export #t)
  (doc 'type '(aabb-height : AABB → Number))
  (- (vec2-y (aabb-max a)) (vec2-y (aabb-min a))))

(doc 'make-aabb-center 'type 'Vec2 × Vec2 → AABB)
(doc "Create AABB from center and half-extents.")
(define (make-aabb-center center half-extents)
  (doc 'export #t)
  (make-aabb (vec2-sub center half-extents)
             (vec2-add center half-extents)))

(doc 'make-aabb-from-points 'type '(List Vec2) → AABB)
(doc "Create AABB enclosing all points.")
(define (make-aabb-from-points points)
  (doc 'export #t)
  (if (null? points)
      (make-aabb (vec2 0 0) (vec2 0 0))
      (let loop ([pts (cdr points)]
                 [min-p (car points)]
                 [max-p (car points)])
           (if (null? pts)
               (make-aabb min-p max-p)
               (loop (cdr pts)
                     (vec2-min min-p (car pts))
                     (vec2-max max-p (car pts)))))))

(doc "--- Circle ---")

(doc 'make-circle 'type 'Vec2 × Number → Circle)
(doc "Create a circle from center and radius.")
(define (make-circle center radius)
  (doc 'export #t)
  (list 'circle center radius))

(doc "circle? : Any → Boolean")
(define (circle? s)
  (doc 'export #t)
  (and (pair? s) (eq? (car s) 'circle)))

(define (circle-center c) (list-ref c 1))
  (doc 'type '(circle-center : Circle → Vec2))

(define (circle-radius c) (list-ref c 2))
  (doc 'type '(circle-radius : Circle → Number))

(doc 'circle-aabb 'type 'Circle → AABB)
(doc "Get bounding box for circle.")
(define (circle-aabb c)
  (doc 'export #t)
  (let ([center (circle-center c)]
        [r (circle-radius c)])
       (make-aabb (vec2-sub center (vec2 r r))
                  (vec2-add center (vec2 r r)))))

(doc "--- Convex Polygon ---")

(doc 'make-polygon 'type '(List Vec2) → Polygon)
(doc "Create a convex polygon from vertices (counter-clockwise).")
(define (make-polygon vertices)
  (doc 'export #t)
  (list 'polygon vertices))

(doc "polygon? : Any → Boolean")
(define (polygon? s)
  (doc 'export #t)
  (and (pair? s) (eq? (car s) 'polygon)))

(define (polygon-vertices p) (list-ref p 1))
  (doc 'type '(polygon-vertices : Polygon → (List Vec2)))

(define (polygon-vertex-count p)
  (doc 'export #t)
  (doc 'type '(polygon-vertex-count : Polygon → Nat))
  (length (polygon-vertices p)))

(doc 'polygon-vertex 'type 'Polygon × Nat → Vec2)
(doc "Get vertex at index (wraps around).")
(define (polygon-vertex p i)
  (doc 'export #t)
  (let* ([verts (polygon-vertices p)]
         [n (length verts)])
        (list-ref verts (modulo i n))))

(doc 'polygon-edge 'type 'Polygon × Nat → (Vec2 × Vec2))
(doc "Get edge at index (as pair of vertices).")
(define (polygon-edge p i)
  (doc 'export #t)
  (list (polygon-vertex p i) (polygon-vertex p (+ i 1))))

(doc 'polygon-edges 'type 'Polygon → (List (Vec2 × Vec2)))
(doc "Get all edges as pairs of vertices.")
(define (polygon-edges p)
  (doc 'export #t)
  (let ([n (polygon-vertex-count p)])
       (let loop ([i 0] [edges '()])
            (if (>= i n)
                (reverse edges)
                (loop (+ i 1) (cons (polygon-edge p i) edges))))))

(doc 'polygon-edge-normal 'type 'Polygon × Nat → Vec2)
(doc "Get outward-facing normal of edge at index.")
(define (polygon-edge-normal p i)
  (doc 'export #t)
  (let* ([edge (polygon-edge p i)]
         [v1 (car edge)]
         [v2 (cadr edge)]
         [edge-vec (vec2-sub v2 v1)])
        ;; Perpendicular, normalized (CCW winding = outward right)
        (vec2-normalize (vec2-rotate-neg-90 edge-vec))))

(doc 'polygon-normals 'type 'Polygon → (List Vec2))
(doc "Get all edge normals.")
(define (polygon-normals p)
  (doc 'export #t)
  (let ([n (polygon-vertex-count p)])
       (let loop ([i 0] [normals '()])
            (if (>= i n)
                (reverse normals)
                (loop (+ i 1) (cons (polygon-edge-normal p i) normals))))))

(doc 'polygon-aabb 'type 'Polygon → AABB)
(doc "Get bounding box for polygon.")
(define (polygon-aabb p)
  (doc 'export #t)
  (make-aabb-from-points (polygon-vertices p)))

(doc 'polygon-center 'type 'Polygon → Vec2)
(doc "Get centroid of polygon.")
(define (polygon-center p)
  (doc 'export #t)
  (let* ([verts (polygon-vertices p)]
         [n (length verts)]
         [sum (fold-left vec2-add (vec2 0 0) verts)])
        (vec2-scale sum (/ 1 n))))

(doc "--- Common Polygon Constructors ---")

(doc 'make-box 'type 'Vec2 × Vec2 → Polygon)
(doc "Create a box from center and half-extents.")
(define (make-box center half-extents)
  (doc 'export #t)
  (let ([hx (vec2-x half-extents)]
        [hy (vec2-y half-extents)]
        [cx (vec2-x center)]
        [cy (vec2-y center)])
       (make-polygon
        (list (vec2 (- cx hx) (- cy hy))  ; bottom-left
              (vec2 (+ cx hx) (- cy hy))  ; bottom-right
              (vec2 (+ cx hx) (+ cy hy))  ; top-right
              (vec2 (- cx hx) (+ cy hy)))))) ; top-left

(doc 'make-regular-polygon 'type 'Vec2 × Number × Nat → Polygon)
(doc "Create regular polygon with n sides at center with radius.")
(define (make-regular-polygon center radius n)
  (doc 'export #t)
  (let* ([angle-step (/ (* 2 3.141592653589793) n)]
         [verts (let loop ([i 0] [vs '()])
                     (if (>= i n)
                         (reverse vs)
                         (let ([angle (* i angle-step)])
                              (loop (+ i 1)
                                    (cons (vec2-add center
                                                    (vec2 (* radius (cos angle))
                                                          (* radius (sin angle))))
                                          vs)))))])
        (make-polygon verts)))

(doc 'section 'point-in-shape)

(doc "point-in-aabb? : Vec2 × AABB → Boolean")
(define (point-in-aabb? point aabb)
  (doc 'export #t)
  (let ([px (vec2-x point)]
        [py (vec2-y point)]
        [min-x (vec2-x (aabb-min aabb))]
        [min-y (vec2-y (aabb-min aabb))]
        [max-x (vec2-x (aabb-max aabb))]
        [max-y (vec2-y (aabb-max aabb))])
       (and (>= px min-x) (<= px max-x)
            (>= py min-y) (<= py max-y))))

(doc "point-in-circle? : Vec2 × Circle → Boolean")
(define (point-in-circle? point circle)
  (doc 'export #t)
  (let ([d-sq (vec2-distance-sq point (circle-center circle))]
        [r (circle-radius circle)])
       (<= d-sq (* r r))))

(doc "point-in-polygon? : Vec2 × Polygon → Boolean")
(doc "Test using ray casting (works for any simple polygon).")
(define (point-in-polygon? point polygon)
  (doc 'export #t)
  (let* ([verts (polygon-vertices polygon)]
         [n (length verts)]
         [px (vec2-x point)]
         [py (vec2-y point)])
        (let loop ([i 0] [inside #f])
             (if (>= i n)
                 inside
                 (let* ([j (modulo (- i 1 n) n)]
                        [vi (list-ref verts i)]
                        [vj (list-ref verts j)]
                        [viy (vec2-y vi)]
                        [vjy (vec2-y vj)]
                        [vix (vec2-x vi)]
                        [vjx (vec2-x vj)])
                       (if (and (not (eq? (> viy py) (> vjy py)))
                                (< px (+ vjx (* (/ (- py vjy) (- viy vjy))
                                                (- vix vjx)))))
                           (loop (+ i 1) (not inside))
                           (loop (+ i 1) inside)))))))

(doc 'section 'collision)

(doc "aabb-aabb? : AABB × AABB → Boolean")
(doc "Test if two AABBs overlap.")
(define (aabb-aabb? a b)
  (doc 'export #t)
  (let ([a-min (aabb-min a)]
        [a-max (aabb-max a)]
        [b-min (aabb-min b)]
        [b-max (aabb-max b)])
       (and (<= (vec2-x a-min) (vec2-x b-max))
            (>= (vec2-x a-max) (vec2-x b-min))
            (<= (vec2-y a-min) (vec2-y b-max))
            (>= (vec2-y a-max) (vec2-y b-min)))))

(doc 'aabb-aabb-manifold 'type 'AABB × AABB → Manifold or #f)
(doc "Generate collision manifold for two AABBs.")
(doc "Returns (normal penetration contact) or #f if no collision.")
(define (aabb-aabb-manifold a b)
  (doc 'export #t)
  (if (not (aabb-aabb? a b))
      #f
      (let* ([a-center (aabb-center a)]
             [b-center (aabb-center b)]
             [a-half (aabb-half-extents a)]
             [b-half (aabb-half-extents b)]
             ;; Calculate overlap on each axis
             [diff (vec2-sub b-center a-center)]
             [overlap-x (- (+ (vec2-x a-half) (vec2-x b-half))
                           (abs (vec2-x diff)))]
             [overlap-y (- (+ (vec2-y a-half) (vec2-y b-half))
                           (abs (vec2-y diff)))])
            ;; Use minimum overlap axis
            (if (< overlap-x overlap-y)
                ;; X-axis separation
                (let* ([sign-x (if (< (vec2-x diff) 0) -1 1)]
                       [normal (vec2 sign-x 0)]
                       [contact (vec2 (+ (vec2-x a-center)
                                         (* (vec2-x a-half) sign-x))
                                      (vec2-y b-center))])
                      (list normal overlap-x contact))
                ;; Y-axis separation
                (let* ([sign-y (if (< (vec2-y diff) 0) -1 1)]
                       [normal (vec2 0 sign-y)]
                       [contact (vec2 (vec2-x b-center)
                                      (+ (vec2-y a-center)
                                         (* (vec2-y a-half) sign-y)))])
                      (list normal overlap-y contact))))))

(doc 'section 'collision)

(doc "circle-circle? : Circle × Circle → Boolean")
(doc "Test if two circles overlap.")
(define (circle-circle? a b)
  (doc 'export #t)
  (let ([dist-sq (vec2-distance-sq (circle-center a) (circle-center b))]
        [r-sum (+ (circle-radius a) (circle-radius b))])
       (<= dist-sq (* r-sum r-sum))))

(doc 'circle-circle-manifold 'type 'Circle × Circle → Manifold or #f)
(doc "Generate collision manifold for two circles.")
(doc "Returns (normal penetration contact) or #f if no collision.")
(define (circle-circle-manifold a b)
  (doc 'export #t)
  (let* ([center-a (circle-center a)]
         [center-b (circle-center b)]
         [radius-a (circle-radius a)]
         [radius-b (circle-radius b)]
         [diff (vec2-sub center-b center-a)]
         [dist-sq (vec2-magnitude-sq diff)]
         [r-sum (+ radius-a radius-b)])
        (if (> dist-sq (* r-sum r-sum))
            #f
            (let* ([dist (sqrt dist-sq)]
                   [normal (if (< dist 0.0001)
                               (vec2 1 0)  ; Arbitrary for coincident
                               (vec2-scale diff (/ 1 dist)))]
                   [penetration (- r-sum dist)]
                   [contact (vec2-add center-a
                                      (vec2-scale normal radius-a))])
                  (list normal penetration contact)))))

(doc 'section 'collision)

(doc "circle-aabb? : Circle × AABB → Boolean")
(doc "Test if circle and AABB overlap.")
(define (circle-aabb? circle aabb)
  (doc 'export #t)
  (let* ([center (circle-center circle)]
         [r (circle-radius circle)]
         [min-p (aabb-min aabb)]
         [max-p (aabb-max aabb)]
         ;; Closest point on AABB to circle center
         [closest-x (max (vec2-x min-p) (min (vec2-x center) (vec2-x max-p)))]
         [closest-y (max (vec2-y min-p) (min (vec2-y center) (vec2-y max-p)))]
         [closest (vec2 closest-x closest-y)]
         [dist-sq (vec2-distance-sq center closest)])
        (<= dist-sq (* r r))))

(doc 'circle-aabb-manifold 'type 'Circle × AABB → Manifold or #f)
(doc "Generate collision manifold for circle vs AABB.")
(define (circle-aabb-manifold circle aabb)
  (doc 'export #t)
  (let* ([center (circle-center circle)]
         [r (circle-radius circle)]
         [min-p (aabb-min aabb)]
         [max-p (aabb-max aabb)]
         [cx (vec2-x center)]
         [cy (vec2-y center)]
         ;; Closest point on AABB
         [closest-x (max (vec2-x min-p) (min cx (vec2-x max-p)))]
         [closest-y (max (vec2-y min-p) (min cy (vec2-y max-p)))]
         [closest (vec2 closest-x closest-y)]
         [diff (vec2-sub center closest)]
         [dist-sq (vec2-magnitude-sq diff)])
        (if (> dist-sq (* r r))
            #f
            (let* ([dist (sqrt dist-sq)]
                   [normal (if (< dist 0.0001)
                               ;; Circle center inside AABB
                               (let ([dx (min (- cx (vec2-x min-p))
                                              (- (vec2-x max-p) cx))]
                                     [dy (min (- cy (vec2-y min-p))
                                              (- (vec2-y max-p) cy))])
                                    (if (< dx dy)
                                        (vec2 (if (< cx (* 0.5 (+ (vec2-x min-p)
                                                                  (vec2-x max-p))))
                                                  -1 1) 0)
                                        (vec2 0 (if (< cy (* 0.5 (+ (vec2-y min-p)
                                                                    (vec2-y max-p))))
                                                    -1 1))))
                               (vec2-scale diff (/ 1 dist)))]
                   [penetration (- r dist)])
                  (list normal penetration closest)))))

(doc 'section 'collision)

(doc "circle-polygon? : Circle × Polygon → Boolean")
(doc "Test if circle and polygon overlap.")
(define (circle-polygon? circle polygon)
  (doc 'export #t)
  ;; Check if center is inside polygon
  (if (point-in-polygon? (circle-center circle) polygon)
      #t
      ;; Check if circle intersects any edge
      (let ([center (circle-center circle)]
            [r (circle-radius circle)]
            [edges (polygon-edges polygon)])
           (let loop ([es edges])
                (if (null? es)
                    #f
                    (let* ([edge (car es)]
                           [v1 (car edge)]
                           [v2 (cadr edge)]
                           [closest (closest-point-on-segment center v1 v2)]
                           [dist-sq (vec2-distance-sq center closest)])
                          (if (<= dist-sq (* r r))
                              #t
                              (loop (cdr es)))))))))

(doc 'closest-point-on-segment 'type 'Vec2 × Vec2 × Vec2 → Vec2)
(doc "Find closest point on line segment to a point.")
(define (closest-point-on-segment point v1 v2)
  (doc 'export #t)
  (let* ([edge (vec2-sub v2 v1)]
         [edge-len-sq (vec2-magnitude-sq edge)])
        (if (< edge-len-sq 0.0001)
            v1  ; Degenerate segment
            (let* ([t (/ (vec2-dot (vec2-sub point v1) edge) edge-len-sq)]
                   [t-clamped (max 0 (min 1 t))])
                  (vec2-add v1 (vec2-scale edge t-clamped))))))

(doc 'circle-polygon-manifold 'type 'Circle × Polygon → Manifold or #f)
(doc "Generate collision manifold for circle vs polygon.")
(define (circle-polygon-manifold circle polygon)
  (doc 'export #t)
  (let ([center (circle-center circle)]
        [r (circle-radius circle)])
       (if (point-in-polygon? center polygon)
           ;; Center inside: find closest edge
           (let* ([edges (polygon-edges polygon)]
                  [normals (polygon-normals polygon)]
                  ;; Find edge with minimum distance to center
                  [best (let loop ([es edges] [ns normals] [best-dist +inf.0] [best-data #f])
                             (if (null? es)
                                 best-data
                                 (let* ([e (car es)]
                                        [n (car ns)]
                                        [v1 (car e)]
                                        [closest (closest-point-on-segment center v1 (cadr e))]
                                        [dist (vec2-distance center closest)])
                                       (if (< dist best-dist)
                                           (loop (cdr es) (cdr ns) dist (list n closest dist))
                                           (loop (cdr es) (cdr ns) best-dist best-data)))))])
                 (if (not best)
                     #f
                     (let ([normal (vec2-neg (car best))]  ; Inward normal
                           [contact (cadr best)]
                           [dist (caddr best)])
                          (list normal (+ r dist) contact))))
           ;; Center outside: check edges
           (let* ([edges (polygon-edges polygon)]
                  [normals (polygon-normals polygon)])
                 (let loop ([es edges] [ns normals])
                      (if (null? es)
                          #f
                          (let* ([e (car es)]
                                 [v1 (car e)]
                                 [v2 (cadr e)]
                                 [closest (closest-point-on-segment center v1 v2)]
                                 [diff (vec2-sub center closest)]
                                 [dist-sq (vec2-magnitude-sq diff)]
                                 [dist (sqrt dist-sq)])
                                (if (and (<= dist-sq (* r r)) (> dist 0.0001))
                                    (let ([normal (vec2-scale diff (/ 1 dist))]
                                          [penetration (- r dist)])
                                         (list normal penetration closest))
                                    (loop (cdr es) (cdr ns))))))))))

(doc 'section 'collision)

(doc 'project-polygon 'type 'Polygon × Vec2 → (Min × Max))
(doc "Project polygon onto axis, return min and max.")
(define (project-polygon polygon axis)
  (doc 'export #t)
  (let* ([verts (polygon-vertices polygon)]
         [first-dot (vec2-dot (car verts) axis)])
        (let loop ([vs (cdr verts)] [pmin first-dot] [pmax first-dot])
             (if (null? vs)
                 (list pmin pmax)
                 (let ([dot (vec2-dot (car vs) axis)])
                      (loop (cdr vs) (min pmin dot) (max pmax dot)))))))

(doc 'project-circle 'type 'Circle × Vec2 → (Min × Max))
(doc "Project circle onto axis.")
(define (project-circle circle axis)
  (doc 'export #t)
  (let* ([center (circle-center circle)]
         [r (circle-radius circle)]
         [center-proj (vec2-dot center axis)])
        (list (- center-proj r) (+ center-proj r))))

(doc 'intervals-overlap 'type '(Min × Max) × (Min × Max) → Number or #f)
(doc "Check if intervals overlap, return overlap amount or #f.")
(define (intervals-overlap a b)
  (doc 'export #t)
  (let ([a-min (car a)]
        [a-max (cadr a)]
        [b-min (car b)]
        [b-max (cadr b)])
       (if (or (< a-max b-min) (< b-max a-min))
           #f
           (min (- a-max b-min) (- b-max a-min)))))

(doc "polygon-polygon? : Polygon × Polygon → Boolean")
(doc "Test if two polygons overlap using SAT.")
(define (polygon-polygon? a b)
  (doc 'export #t)
  (let* ([normals-a (polygon-normals a)]
         [normals-b (polygon-normals b)]
         [all-normals (append normals-a normals-b)])
        (let loop ([ns all-normals])
             (if (null? ns)
                 #t  ; No separating axis found
                 (let* ([axis (car ns)]
                        [proj-a (project-polygon a axis)]
                        [proj-b (project-polygon b axis)]
                        [overlap (intervals-overlap proj-a proj-b)])
                       (if (not overlap)
                           #f  ; Found separating axis
                           (loop (cdr ns))))))))

(doc 'polygon-polygon-manifold 'type 'Polygon × Polygon → Manifold or #f)
(doc "Generate collision manifold using SAT.")
(define (polygon-polygon-manifold a b)
  (doc 'export #t)
  (let* ([normals-a (polygon-normals a)]
         [normals-b (polygon-normals b)]
         [all-normals (append normals-a normals-b)])
        (let loop ([ns all-normals]
                   [min-overlap +inf.0]
                   [best-normal #f])
             (if (null? ns)
                 (if (not best-normal)
                     #f
                     ;; Find contact point
                     (let* ([center-a (polygon-center a)]
                            [center-b (polygon-center b)]
                            [diff (vec2-sub center-b center-a)]
                            ;; Ensure normal points from A to B
                            [normal (if (< (vec2-dot best-normal diff) 0)
                                        (vec2-neg best-normal)
                                        best-normal)]
                            ;; Contact is on edge of A in direction of normal
                            [verts-a (polygon-vertices a)]
                            [contact (let loop ([vs verts-a]
                                                [best-v (car verts-a)]
                                                [best-dot (vec2-dot (car verts-a) normal)])
                                          (if (null? vs)
                                              best-v
                                              (let ([dot (vec2-dot (car vs) normal)])
                                                   (if (> dot best-dot)
                                                       (loop (cdr vs) (car vs) dot)
                                                       (loop (cdr vs) best-v best-dot)))))])
                           (list normal min-overlap contact)))
                 (let* ([axis (car ns)]
                        [proj-a (project-polygon a axis)]
                        [proj-b (project-polygon b axis)]
                        [overlap (intervals-overlap proj-a proj-b)])
                       (if (not overlap)
                           #f  ; Found separating axis
                           (if (< overlap min-overlap)
                               (loop (cdr ns) overlap axis)
                               (loop (cdr ns) min-overlap best-normal))))))))

(doc 'section 'generic)

(define (shape-type s)
  (doc 'export #t)
  (doc 'type '(shape-type : Shape → Symbol))
  (cond [(aabb? s) 'aabb]
        [(circle? s) 'circle]
        [(polygon? s) 'polygon]
        [else 'unknown]))

(doc 'shape-aabb 'type 'Shape → AABB)
(doc "Get bounding box for any shape.")
(define (shape-aabb s)
  (doc 'export #t)
  (cond [(aabb? s) s]
        [(circle? s) (circle-aabb s)]
        [(polygon? s) (polygon-aabb s)]
        [else (make-aabb (vec2 0 0) (vec2 0 0))]))

(doc "shapes-collide? : Shape × Shape → Boolean")
(doc "Test if two shapes overlap.")
(define (shapes-collide? a b)
  (doc 'export #t)
  (let ([type-a (shape-type a)]
        [type-b (shape-type b)])
       (cond
        ;; AABB vs AABB
        [(and (eq? type-a 'aabb) (eq? type-b 'aabb))
         (aabb-aabb? a b)]
        ;; Circle vs Circle
        [(and (eq? type-a 'circle) (eq? type-b 'circle))
         (circle-circle? a b)]
        ;; Circle vs AABB
        [(and (eq? type-a 'circle) (eq? type-b 'aabb))
         (circle-aabb? a b)]
        [(and (eq? type-a 'aabb) (eq? type-b 'circle))
         (circle-aabb? b a)]
        ;; Circle vs Polygon
        [(and (eq? type-a 'circle) (eq? type-b 'polygon))
         (circle-polygon? a b)]
        [(and (eq? type-a 'polygon) (eq? type-b 'circle))
         (circle-polygon? b a)]
        ;; Polygon vs Polygon
        [(and (eq? type-a 'polygon) (eq? type-b 'polygon))
         (polygon-polygon? a b)]
        ;; AABB vs Polygon (convert AABB to polygon)
        [(and (eq? type-a 'aabb) (eq? type-b 'polygon))
         (polygon-polygon? (aabb-to-polygon a) b)]
        [(and (eq? type-a 'polygon) (eq? type-b 'aabb))
         (polygon-polygon? a (aabb-to-polygon b))]
        ;; Unknown
        [else #f])))

(doc 'aabb-to-polygon 'type 'AABB → Polygon)
(doc "Convert AABB to polygon for SAT tests.")
(define (aabb-to-polygon aabb)
  (doc 'export #t)
  (let ([min-p (aabb-min aabb)]
        [max-p (aabb-max aabb)])
       (make-polygon
        (list min-p
              (vec2 (vec2-x max-p) (vec2-y min-p))
              max-p
              (vec2 (vec2-x min-p) (vec2-y max-p))))))

(doc 'shapes-manifold 'type 'Shape × Shape → Manifold or #f)
(doc "Get collision manifold for two shapes.")
(define (shapes-manifold a b)
  (doc 'export #t)
  (let ([type-a (shape-type a)]
        [type-b (shape-type b)])
       (cond
        ;; AABB vs AABB
        [(and (eq? type-a 'aabb) (eq? type-b 'aabb))
         (aabb-aabb-manifold a b)]
        ;; Circle vs Circle
        [(and (eq? type-a 'circle) (eq? type-b 'circle))
         (circle-circle-manifold a b)]
        ;; Circle vs AABB
        [(and (eq? type-a 'circle) (eq? type-b 'aabb))
         (circle-aabb-manifold a b)]
        [(and (eq? type-a 'aabb) (eq? type-b 'circle))
         (let ([m (circle-aabb-manifold b a)])
              (if m (list (vec2-neg (car m)) (cadr m) (caddr m)) #f))]
        ;; Circle vs Polygon
        [(and (eq? type-a 'circle) (eq? type-b 'polygon))
         (circle-polygon-manifold a b)]
        [(and (eq? type-a 'polygon) (eq? type-b 'circle))
         (let ([m (circle-polygon-manifold b a)])
              (if m (list (vec2-neg (car m)) (cadr m) (caddr m)) #f))]
        ;; Polygon vs Polygon
        [(and (eq? type-a 'polygon) (eq? type-b 'polygon))
         (polygon-polygon-manifold a b)]
        ;; AABB vs Polygon
        [(and (eq? type-a 'aabb) (eq? type-b 'polygon))
         (polygon-polygon-manifold (aabb-to-polygon a) b)]
        [(and (eq? type-a 'polygon) (eq? type-b 'aabb))
         (polygon-polygon-manifold a (aabb-to-polygon b))]
        ;; Unknown
        [else #f])))

(doc 'section 'broad)

(doc 'make-spatial-hash 'type 'Number → SpatialHash)
(doc "Create a spatial hash grid with given cell size.")
(doc "Internal table stored in a mutable vector slot holding a HAMT.")
(define (make-spatial-hash cell-size)
  (doc 'export #t)
  (list 'spatial-hash cell-size (vector hamt-empty)))

(doc "spatial-hash? : Any → Boolean")
(define (spatial-hash? s)
  (doc 'export #t)
  (and (pair? s) (eq? (car s) 'spatial-hash)))

(define (spatial-hash-cell-size s) (list-ref s 1))
  (doc 'type '(spatial-hash-cell-size : SpatialHash → Number))

(define (spatial-hash-table s) (vector-ref (list-ref s 2) 0))
  (doc 'type '(spatial-hash-table : SpatialHash → HAMT))

(define (spatial-hash-table-set! s new-table)
  (doc 'export #t)
  (doc 'type '(spatial-hash-table-set! : SpatialHash → HAMT → Void))
  (vector-set! (list-ref s 2) 0 new-table))

(doc 'point-to-cell 'type 'Vec2 × Number → (Int × Int))
(doc "Convert point to cell coordinates.")
(define (point-to-cell point cell-size)
  (doc 'export #t)
  (list (floor (/ (vec2-x point) cell-size))
        (floor (/ (vec2-y point) cell-size))))

(doc 'aabb-to-cells 'type 'AABB × Number → (List (Int × Int)))
(doc "Get all cells an AABB touches.")
(define (aabb-to-cells aabb cell-size)
  (doc 'export #t)
  (let* ([min-cell (point-to-cell (aabb-min aabb) cell-size)]
         [max-cell (point-to-cell (aabb-max aabb) cell-size)]
         [min-x (car min-cell)]
         [min-y (cadr min-cell)]
         [max-x (car max-cell)]
         [max-y (cadr max-cell)])
        (let loop-x ([x min-x] [cells '()])
             (if (> x max-x)
                 cells
                 (loop-x (+ x 1)
                         (let loop-y ([y min-y] [cs cells])
                              (if (> y max-y)
                                  cs
                                  (loop-y (+ y 1) (cons (list x y) cs)))))))))

(doc "spatial-hash-insert! : SpatialHash × Any × AABB → Void")
(doc "Insert an object with its AABB into the spatial hash.")
(define (spatial-hash-insert! hash obj aabb)
  (doc 'export #t)
  (let ([cells (aabb-to-cells aabb (spatial-hash-cell-size hash))])
    (spatial-hash-table-set!
     hash
     (fold-left
      (lambda (tbl cell)
        (hamt-assoc cell (cons obj (hamt-lookup-or cell tbl '())) tbl))
      (spatial-hash-table hash)
      cells))))

(doc "spatial-hash-clear! : SpatialHash → Void")
(doc "Clear all entries from spatial hash.")
(define (spatial-hash-clear! hash)
  (doc 'export #t)
  (spatial-hash-table-set! hash hamt-empty))

(doc 'spatial-hash-query 'type 'SpatialHash × AABB → (List Any))
(doc "Get all objects that might overlap with given AABB.")
(define (spatial-hash-query hash aabb)
  (doc 'export #t)
  (let* ([cell-size (spatial-hash-cell-size hash)]
         [table (spatial-hash-table hash)]
         [cells (aabb-to-cells aabb cell-size)]
         ;; Collect unique objects using HAMT as set
         [result-set
          (fold-left
           (lambda (seen cell)
             (fold-left
              (lambda (s obj) (hamt-assoc obj #t s))
              seen
              (hamt-lookup-or cell table '())))
           hamt-empty
           cells)])
    (hamt-keys result-set)))

(doc 'section 'collision)

(doc 'find-collision-pairs 'type '(List (Object × Shape)) × SpatialHash → (List (Object × Object)))
(doc "Find potential collision pairs using spatial hash.")
(doc "Each item is (object . shape).")
(define (find-collision-pairs items hash)
  (doc 'export #t)
  ;; Clear and rebuild hash
  (spatial-hash-clear! hash)
  (for-each
   (lambda (item)
           (let ([obj (car item)]
                 [shape (cdr item)])
                (spatial-hash-insert! hash obj (shape-aabb shape))))
   items)
  ;; Find pairs using HAMT as set for deduplication
  (let loop ([remaining items] [pairs hamt-empty])
    (if (null? remaining)
        ;; Extract unique pairs
        (hamt-keys pairs)
        (let* ([item (car remaining)]
               [obj (car item)]
               [shape (cdr item)]
               [candidates (spatial-hash-query hash (shape-aabb shape))]
               ;; Accumulate new pairs from candidates
               [new-pairs
                (fold-left
                 (lambda (ps other)
                   (if (eq? obj other)
                       ps
                       (let ([pair-key (if (< (equal-hash obj) (equal-hash other))
                                           (cons obj other)
                                           (cons other obj))])
                         (hamt-assoc pair-key #t ps))))
                 pairs
                 candidates)])
          (loop (cdr remaining) new-pairs)))))

(doc 'narrow-phase 'type '(Object × Object) × (Object → Shape) → Manifold or #f)
(doc "Perform narrow phase collision detection on a pair.")
(define (narrow-phase pair get-shape)
  (doc 'export #t)
  (let ([shape-a (get-shape (car pair))]
        [shape-b (get-shape (cdr pair))])
       (shapes-manifold shape-a shape-b)))
