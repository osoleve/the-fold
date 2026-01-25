;;; lattice/geometry/mesh-gen.ss --- 2D/3D mesh generation algorithms
;;; @module mesh-gen
;;; @requires prelude linalg/vec geometry

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/geometry/geometry.ss")

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

(define (point2-x p) (vector-ref p 0))
(define (point2-y p) (vector-ref p 1))

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

(doc tri2-circumcenter 'type '(-> Triangle2 Point2))
(doc tri2-circumcenter 'description "Compute circumcenter of triangle")
(define (tri2-circumcenter tri)
  (let* ([p1 (tri2-p1 tri)]
         [p2 (tri2-p2 tri)]
         [p3 (tri2-p3 tri)]
         [ax (point2-x p1)] [ay (point2-y p1)]
         [bx (point2-x p2)] [by (point2-y p2)]
         [cx (point2-x p3)] [cy (point2-y p3)]
         [d (* 2.0 (+ (* ax (- by cy))
                      (* bx (- cy ay))
                      (* cx (- ay by))))]
         [a2 (+ (* ax ax) (* ay ay))]
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
    (make-point2 ux uy)))

(doc tri2-circumradius-sq 'type '(-> Triangle2 Number))
(doc tri2-circumradius-sq 'description "Squared circumradius of triangle")
(define (tri2-circumradius-sq tri)
  (let* ([cc (tri2-circumcenter tri)]
         [p1 (tri2-p1 tri)]
         [dx (- (point2-x cc) (point2-x p1))]
         [dy (- (point2-y cc) (point2-y p1))])
    (+ (* dx dx) (* dy dy))))

(doc point-in-circumcircle? 'type '(-> Point2 Triangle2 Boolean))
(doc point-in-circumcircle? 'description "Test if point is inside triangle's circumcircle")
(define (point-in-circumcircle? p tri)
  (let* ([cc (tri2-circumcenter tri)]
         [r2 (tri2-circumradius-sq tri)]
         [dx (- (point2-x p) (point2-x cc))]
         [dy (- (point2-y p) (point2-y cc))]
         [d2 (+ (* dx dx) (* dy dy))])
    (< d2 r2)))

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
  (any (lambda (e) (edges-equal? edge e)) edges))

(define (count-edge edge edges)
  (length (filter (lambda (e) (edges-equal? edge e)) edges)))

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
         [all-edges (apply append (map tri2-edges bad-tris))]
         ;; Find boundary edges (edges that appear exactly once)
         [boundary-edges (filter (lambda (e) (= 1 (count-edge e all-edges)))
                                 all-edges)]
         ;; Create new triangles by connecting boundary edges to point
         [new-tris (map (lambda (e)
                          (make-tri2 (edge-p1 e) (edge-p2 e) point))
                        boundary-edges)])
    (append good-tris new-tris)))

(doc delaunay-triangulate 'export #t)
(doc delaunay-triangulate 'type '(-> (List Point2) (List Triangle2)))
(doc delaunay-triangulate 'description "Compute Delaunay triangulation of 2D points (Bowyer-Watson algorithm)")
(define (delaunay-triangulate points)
  (if (< (length points) 3)
      '()
      (let* ([super-tri (make-super-triangle points)]
             ;; Start with super-triangle
             [triangles (list super-tri)]
             ;; Insert all points
             [triangles (fold-left delaunay-insert-point triangles points)]
             ;; Remove triangles connected to super-triangle vertices
             [triangles (filter (lambda (tri)
                                  (not (triangle-uses-super-vertex? tri super-tri)))
                                triangles)])
        triangles)))

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
(doc mesh-quality-report 'type '(-> (List Triangle2) Void))
(doc mesh-quality-report 'description "Print quality statistics for a mesh")
(define (mesh-quality-report triangles)
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
                (radians->degrees (/ (apply + max-angles) (length max-angles)))))))

;;; ============================================================
;;; Section: Mesh Refinement (Ruppert's Algorithm)
;;; ============================================================

(doc 'section 'refinement)

(doc tri2-is-bad? 'type '(-> Triangle2 Number Boolean))
(doc tri2-is-bad? 'description "Check if triangle is 'bad' (min angle below threshold)")
(define (tri2-is-bad? tri min-angle-threshold)
  (< (tri2-min-angle tri) min-angle-threshold))

(doc refine-mesh 'export #t)
(doc refine-mesh 'type '(-> (List Triangle2) Number Nat (List Triangle2)))
(doc refine-mesh 'description "Refine mesh by inserting circumcenters of bad triangles")
(define (refine-mesh triangles min-angle-deg max-iterations)
  (let ([threshold (* min-angle-deg (/ 3.141592653589793 180.0))])
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
                  (loop new-tris (+ iter 1)))))))))

;;; ============================================================
;;; Section: Mesh to 3D Conversion
;;; ============================================================

(doc 'section 'conversion)

(doc triangles-to-3d 'export #t)
(doc triangles-to-3d 'type '(-> (List Triangle2) (-> Point2 Number) (List Triangle3)))
(doc triangles-to-3d 'description "Convert 2D triangles to 3D using height function")
(define (triangles-to-3d tris height-fn)
  (map (lambda (tri)
         (let* ([p1 (tri2-p1 tri)]
                [p2 (tri2-p2 tri)]
                [p3 (tri2-p3 tri)]
                [v1 (vec3 (point2-x p1) (height-fn p1) (point2-y p1))]
                [v2 (vec3 (point2-x p2) (height-fn p2) (point2-y p2))]
                [v3 (vec3 (point2-x p3) (height-fn p3) (point2-y p3))])
           (triangle3 v1 v2 v3)))
       tris))

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
(doc render-mesh-2d 'type '(-> (List Triangle2) Nat Nat String))
(doc render-mesh-2d 'description "Render 2D mesh as ASCII art")
(define (render-mesh-2d triangles width height)
  (if (null? triangles)
      "Empty mesh\n"
      (let* ([all-points (apply append (map tri2-points triangles))]
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
                          result)))))))

(printf "mesh-gen.ss loaded~n")
(printf "  (delaunay-triangulate points)     - Bowyer-Watson triangulation~n")
(printf "  (tri2-aspect-ratio tri)           - Quality metric~n")
(printf "  (mesh-quality-report tris)        - Print statistics~n")
(printf "  (refine-mesh tris angle iters)    - Ruppert refinement~n")
(printf "  (triangles-to-3d tris height-fn)  - Convert to 3D~n")
(printf "  (render-mesh-2d tris w h)         - ASCII visualization~n")
