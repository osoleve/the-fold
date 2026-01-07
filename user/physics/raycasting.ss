;;; user/physics/raycasting.ss — 2D Raycasting for Physics Engine
;;;
;;; Comprehensive raycasting against 2D shapes:
;;;   - AABB (slab method)
;;;   - Circle (quadratic intersection)
;;;   - Convex polygon (edge iteration)
;;;   - Generic shape dispatch
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/linalg/vec2.ss
;;;   - user/physics/collision-detection.ss (shape types)

(load "core/base/prelude.ss")
(load "core/linalg/vec2.ss")
(load "user/physics/collision-detection.ss")

;;; ============================================================
;;; Constants
;;; ============================================================

(define *ray-epsilon* 1e-10)

;;; ============================================================
;;; Ray2 Data Structure
;;; ============================================================

;;; make-ray2 : Vec2 x Vec2 x Number -> Ray2
;;; Create a 2D ray with origin, direction (auto-normalized), and max distance.
(define (make-ray2 origin direction max-dist)
  (list 'ray2 origin (vec2-normalize direction) max-dist))

;;; ray2? : Any -> Boolean
(define (ray2? r)
  (and (pair? r) (eq? (car r) 'ray2)))

;;; ray2-origin : Ray2 -> Vec2
(define (ray2-origin r) (list-ref r 1))

;;; ray2-direction : Ray2 -> Vec2
(define (ray2-direction r) (list-ref r 2))

;;; ray2-max-dist : Ray2 -> Number
(define (ray2-max-dist r) (list-ref r 3))

;;; ray2-point-at : Ray2 x Number -> Vec2
;;; Get point at parameter t along ray.
(define (ray2-point-at ray t)
  (vec2-add (ray2-origin ray)
            (vec2-scale (ray2-direction ray) t)))

;;; ============================================================
;;; HitInfo Data Structure
;;; ============================================================

;;; make-hit-info : Number x Vec2 x Vec2 x Shape -> HitInfo
;;; Create hit result with distance (t), hit point, surface normal, and shape.
(define (make-hit-info distance point normal shape)
  (list 'hit-info distance point normal shape))

;;; hit-info? : Any -> Boolean
(define (hit-info? h)
  (and (pair? h) (eq? (car h) 'hit-info)))

;;; hit-info-distance : HitInfo -> Number
(define (hit-info-distance h) (list-ref h 1))

;;; hit-info-point : HitInfo -> Vec2
(define (hit-info-point h) (list-ref h 2))

;;; hit-info-normal : HitInfo -> Vec2
(define (hit-info-normal h) (list-ref h 3))

;;; hit-info-shape : HitInfo -> Shape
(define (hit-info-shape h) (list-ref h 4))

;;; ============================================================
;;; Ray-AABB Intersection (Slab Method)
;;; ============================================================

;;; ray2-aabb : Ray2 x AABB -> HitInfo | #f
;;; Cast ray against AABB using slab method.
(define (ray2-aabb ray aabb)
  (let* ([origin (ray2-origin ray)]
         [dir (ray2-direction ray)]
         [max-dist (ray2-max-dist ray)]
         [min-p (aabb-min aabb)]
         [max-p (aabb-max aabb)]
         [ox (vec2-x origin)]
         [oy (vec2-y origin)]
         [dx (vec2-x dir)]
         [dy (vec2-y dir)])
        ;; Compute t-values for X slab
        (let*-values
         ([(tx1 tx2 tx-face)
           (if (< (abs dx) *ray-epsilon*)
               ;; Ray parallel to X-slab
               (if (and (>= ox (vec2-x min-p)) (<= ox (vec2-x max-p)))
                   (values -inf.0 +inf.0 'none)
                   (values +inf.0 -inf.0 'none))
               (let* ([inv-dx (/ 1.0 dx)]
                      [t1 (* (- (vec2-x min-p) ox) inv-dx)]
                      [t2 (* (- (vec2-x max-p) ox) inv-dx)])
                     (if (< t1 t2)
                         (values t1 t2 (if (< dx 0) 'right 'left))
                         (values t2 t1 (if (< dx 0) 'left 'right)))))]
          ;; Compute t-values for Y slab
          [(ty1 ty2 ty-face)
           (if (< (abs dy) *ray-epsilon*)
               (if (and (>= oy (vec2-y min-p)) (<= oy (vec2-y max-p)))
                   (values -inf.0 +inf.0 'none)
                   (values +inf.0 -inf.0 'none))
               (let* ([inv-dy (/ 1.0 dy)]
                      [t1 (* (- (vec2-y min-p) oy) inv-dy)]
                      [t2 (* (- (vec2-y max-p) oy) inv-dy)])
                     (if (< t1 t2)
                         (values t1 t2 (if (< dy 0) 'top 'bottom))
                         (values t2 t1 (if (< dy 0) 'bottom 'top)))))])
         ;; Compute intersection of slabs
         (let* ([tmin (max tx1 ty1)]
                [tmax (min tx2 ty2)])
               (cond
                [(> tmin tmax) #f]           ; Miss
                [(< tmax 0) #f]              ; Behind ray
                [(> tmin max-dist) #f]       ; Beyond max distance
                [else
                 ;; Calculate entry point
                 (let* ([t (if (< tmin 0) tmax tmin)]
                        [hit-face (cond
                                   [(< tmin 0) ; Inside, use exit face
                                    (if (= tmax tx2) 'x 'y)]
                                   [(= tmin tx1) 'x]
                                   [else 'y])])
                       (if (and (>= t 0) (<= t max-dist))
                           (let* ([point (ray2-point-at ray t)]
                                  [normal (cond
                                           [(eq? hit-face 'x)
                                            (if (< tmin 0)
                                                ;; Exit normal
                                                (vec2 (if (> dx 0) 1 -1) 0)
                                                ;; Entry normal
                                                (vec2 (if (< dx 0) 1 -1) 0))]
                                           [else
                                            (if (< tmin 0)
                                                (vec2 0 (if (> dy 0) 1 -1))
                                                (vec2 0 (if (< dy 0) 1 -1)))])])
                                 (make-hit-info t point normal aabb))
                           #f))])))))

;;; ============================================================
;;; Ray-Circle Intersection (Quadratic)
;;; ============================================================

;;; ray2-circle : Ray2 x Circle -> HitInfo | #f
;;; Cast ray against circle using quadratic formula.
(define (ray2-circle ray circle)
  (let* ([origin (ray2-origin ray)]
         [dir (ray2-direction ray)]
         [max-dist (ray2-max-dist ray)]
         [center (circle-center circle)]
         [radius (circle-radius circle)])
        ;; Handle degenerate zero-radius circle
        (if (<= radius 0)
            #f
            (let* ([oc (vec2-sub origin center)]
                   ;; Quadratic: |O + tD - C|^2 = r^2
                   ;; a*t^2 + b*t + c = 0 where a=1 (dir normalized)
                   [b (* 2 (vec2-dot oc dir))]
                   [c (- (vec2-magnitude-sq oc) (* radius radius))]
                   [discriminant (- (* b b) (* 4 c))])
                  (cond
                   [(< discriminant 0) #f]  ; No intersection
                   [else
                    (let* ([sqrt-d (sqrt discriminant)]
                           [t1 (/ (- (- b) sqrt-d) 2)]
                           [t2 (/ (+ (- b) sqrt-d) 2)]
                           ;; Choose appropriate t
                           [t (cond
                               [(and (>= t1 0) (<= t1 max-dist)) t1]
                               [(and (>= t2 0) (<= t2 max-dist)) t2]
                               [else #f])])
                          (if (not t)
                              #f
                              (let* ([point (ray2-point-at ray t)]
                                     [normal (vec2-normalize (vec2-sub point center))])
                                    (make-hit-info t point normal circle))))])))))

;;; ============================================================
;;; Ray-Segment Intersection (Helper)
;;; ============================================================

;;; ray2-segment : Vec2 x Vec2 x Vec2 x Vec2 -> (t . u) | #f
;;; Intersect ray with line segment.
;;; Returns (t . u) where t is ray param and u is segment param [0,1].
(define (ray2-segment-intersection ray-origin ray-dir p1 p2)
  (let* ([edge (vec2-sub p2 p1)]
         [origin-to-p1 (vec2-sub p1 ray-origin)]
         [denom (vec2-cross ray-dir edge)])
        (if (< (abs denom) *ray-epsilon*)
            #f  ; Parallel
            (let* ([t (/ (vec2-cross origin-to-p1 edge) denom)]
                   [u (/ (vec2-cross origin-to-p1 ray-dir) denom)])
                  (if (and (>= t 0) (>= u 0) (<= u 1))
                      (cons t u)
                      #f)))))

;;; ============================================================
;;; Ray-Polygon Intersection
;;; ============================================================

;;; ray2-polygon : Ray2 x Polygon -> HitInfo | #f
;;; Cast ray against convex polygon, finding closest edge hit.
(define (ray2-polygon ray polygon)
  (let* ([origin (ray2-origin ray)]
         [dir (ray2-direction ray)]
         [max-dist (ray2-max-dist ray)]
         [verts (polygon-vertices polygon)]
         [n (length verts)])
        (if (point-in-polygon? origin polygon)
            ;; Origin inside: find exit point
            (ray2-polygon-exit ray polygon max-dist)
            ;; Origin outside: find entry point (closest hit)
            (let loop ([i 0] [best-t +inf.0] [best-normal #f])
                 (if (>= i n)
                     (if (< best-t +inf.0)
                         (make-hit-info best-t
                                        (ray2-point-at ray best-t)
                                        best-normal
                                        polygon)
                         #f)
                     (let* ([p1 (list-ref verts i)]
                            [p2 (list-ref verts (modulo (+ i 1) n))]
                            [result (ray2-segment-intersection origin dir p1 p2)])
                           (if (and result
                                    (< (car result) best-t)
                                    (<= (car result) max-dist))
                               (let* ([edge (vec2-sub p2 p1)]
                                      ;; Outward normal (CCW winding)
                                      [normal (vec2-normalize (vec2-rotate-neg-90 edge))])
                                     (loop (+ i 1) (car result) normal))
                               (loop (+ i 1) best-t best-normal))))))))

;;; ray2-polygon-exit : Ray2 x Polygon x Number -> HitInfo | #f
;;; Find exit point when ray origin is inside polygon.
(define (ray2-polygon-exit ray polygon max-dist)
  (let* ([origin (ray2-origin ray)]
         [dir (ray2-direction ray)]
         [verts (polygon-vertices polygon)]
         [n (length verts)])
        (let loop ([i 0] [best-t +inf.0] [best-normal #f])
             (if (>= i n)
                 (if (and (< best-t +inf.0) (<= best-t max-dist))
                     (make-hit-info best-t
                                    (ray2-point-at ray best-t)
                                    best-normal
                                    polygon)
                     #f)
                 (let* ([p1 (list-ref verts i)]
                        [p2 (list-ref verts (modulo (+ i 1) n))]
                        [result (ray2-segment-intersection origin dir p1 p2)])
                       (if (and result (> (car result) *ray-epsilon*) (< (car result) best-t))
                           (let* ([edge (vec2-sub p2 p1)]
                                  ;; Inward normal (we're exiting)
                                  [normal (vec2-neg (vec2-normalize (vec2-rotate-neg-90 edge)))])
                                 (loop (+ i 1) (car result) normal))
                           (loop (+ i 1) best-t best-normal)))))))

;;; ============================================================
;;; Generic Shape Dispatch
;;; ============================================================

;;; ray2-shape : Ray2 x Shape -> HitInfo | #f
;;; Cast ray against any supported shape type.
(define (ray2-shape ray shape)
  (cond
   [(aabb? shape) (ray2-aabb ray shape)]
   [(circle? shape) (ray2-circle ray shape)]
   [(polygon? shape) (ray2-polygon ray shape)]
   [else #f]))

;;; ============================================================
;;; Convenience Constructors
;;; ============================================================

;;; make-ray2-infinite : Vec2 x Vec2 -> Ray2
;;; Create a ray with effectively infinite max distance.
(define (make-ray2-infinite origin direction)
  (make-ray2 origin direction 1e308))

;;; make-ray2-between : Vec2 x Vec2 -> Ray2
;;; Create a ray from start to end point.
(define (make-ray2-between start end)
  (let ([diff (vec2-sub end start)])
       (make-ray2 start diff (vec2-magnitude diff))))
