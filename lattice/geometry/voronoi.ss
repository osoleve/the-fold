;;; lattice/geometry/voronoi.ss --- Voronoi diagrams via Delaunay duality
;;; @module voronoi
;;; @requires prelude geometry/mesh-gen geometry/convex-hull

(load "core/base/prelude.ss")
(load "lattice/geometry/mesh-gen.ss")
(load "lattice/geometry/convex-hull.ss")

(doc 'module 'voronoi)
(doc 'description "Voronoi diagram computation via Delaunay duality")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Section: Voronoi Data Structures
;;; ============================================================

(doc 'section 'structures)

;;; A Voronoi diagram consists of:
;;; - sites: the input points (each generates a cell)
;;; - vertices: circumcenters of Delaunay triangles
;;; - edges: connections between adjacent circumcenters
;;; - cells: mapping from site index to cell polygon vertices

(doc make-voronoi 'type '(-> (Vector Point2) (Vector Point2) (List VoronoiEdge) (Vector (List Int)) Voronoi))
(doc make-voronoi 'description "Create a Voronoi diagram structure")
(define (make-voronoi sites vertices edges cells)
  (list 'voronoi sites vertices edges cells))

(define (voronoi? v) (and (pair? v) (eq? (car v) 'voronoi)))
(define (voronoi-sites v) (list-ref v 1))
(define (voronoi-vertices v) (list-ref v 2))
(define (voronoi-edges v) (list-ref v 3))
(define (voronoi-cells v) (list-ref v 4))

;;; A Voronoi edge connects two vertices (or extends to infinity)
;;; v1, v2 are vertex indices (-1 means extends to infinity in that direction)
;;; site-left, site-right are the sites on either side of this edge
(doc make-voronoi-edge 'type '(-> Int Int Int Int VoronoiEdge))
(define (make-voronoi-edge v1 v2 site-left site-right)
  (list 'vedge v1 v2 site-left site-right))

(define (vedge-v1 e) (list-ref e 1))
(define (vedge-v2 e) (list-ref e 2))
(define (vedge-site-left e) (list-ref e 3))
(define (vedge-site-right e) (list-ref e 4))

;;; ============================================================
;;; Section: Triangle Adjacency
;;; ============================================================

(doc 'section 'adjacency)

;;; Build an edge->triangles mapping to find adjacent triangles
;;; Key: canonical edge (smaller point first by some ordering)
;;; Value: list of triangle indices sharing this edge

(define (point2-key p)
  ;; Create a sortable key for a point
  (cons (point2-x p) (point2-y p)))

(define (key<? k1 k2)
  (or (< (car k1) (car k2))
      (and (= (car k1) (car k2))
           (< (cdr k1) (cdr k2)))))

(define (canonical-edge p1 p2)
  ;; Return edge with points in canonical order
  (let ([k1 (point2-key p1)]
        [k2 (point2-key p2)])
    (if (key<? k1 k2)
        (cons p1 p2)
        (cons p2 p1))))

(define (edge-key edge)
  ;; Create a hashtable-compatible key from a canonical edge
  (let ([p1 (car edge)]
        [p2 (cdr edge)])
    (list (point2-x p1) (point2-y p1)
          (point2-x p2) (point2-y p2))))

(doc build-edge-adjacency 'type '(-> (Vector Triangle2) Hashtable))
(doc build-edge-adjacency 'description "Build mapping from edges to adjacent triangle indices")
(define (build-edge-adjacency triangles)
  (let ([adj (make-hashtable equal-hash equal?)])
    (do ([i 0 (+ i 1)])
        ((>= i (vector-length triangles)) adj)
      (let* ([tri (vector-ref triangles i)]
             [p1 (tri2-p1 tri)]
             [p2 (tri2-p2 tri)]
             [p3 (tri2-p3 tri)]
             [e1 (edge-key (canonical-edge p1 p2))]
             [e2 (edge-key (canonical-edge p2 p3))]
             [e3 (edge-key (canonical-edge p3 p1))])
        (hashtable-update! adj e1 (lambda (v) (cons i v)) '())
        (hashtable-update! adj e2 (lambda (v) (cons i v)) '())
        (hashtable-update! adj e3 (lambda (v) (cons i v)) '())))))

;;; ============================================================
;;; Section: Site-to-Triangle Mapping
;;; ============================================================

(doc 'section 'site-triangles)

(doc build-site-triangles 'type '(-> (Vector Point2) (Vector Triangle2) (Vector (List Int))))
(doc build-site-triangles 'description "For each site, find all triangles containing it")
(define (build-site-triangles sites triangles)
  (let ([n (vector-length sites)]
        [result (make-vector (vector-length sites) '())])
    (do ([ti 0 (+ ti 1)])
        ((>= ti (vector-length triangles)) result)
      (let ([tri (vector-ref triangles ti)])
        ;; Find which sites match this triangle's vertices
        (do ([si 0 (+ si 1)])
            ((>= si n))
          (let ([site (vector-ref sites si)])
            (when (or (points-equal? site (tri2-p1 tri))
                      (points-equal? site (tri2-p2 tri))
                      (points-equal? site (tri2-p3 tri)))
              (vector-set! result si (cons ti (vector-ref result si))))))))))

;;; ============================================================
;;; Section: Cell Construction
;;; ============================================================

(doc 'section 'cells)

;;; Given a site and its surrounding triangles, construct the Voronoi cell
;;; by ordering the circumcenters around the site

(doc angle-from-site 'type '(-> Point2 Point2 Number))
(doc angle-from-site 'description "Compute angle from site to point")
(define (angle-from-site site point)
  (atan (- (point2-y point) (point2-y site))
        (- (point2-x point) (point2-x site))))

(doc order-cell-vertices 'type '(-> Point2 (List Point2) (List Point2)))
(doc order-cell-vertices 'description "Order cell vertices CCW around site")
(define (order-cell-vertices site vertices)
  (if (< (length vertices) 2)
      vertices
      (list-sort
       (lambda (a b)
         (< (angle-from-site site a)
            (angle-from-site site b)))
       vertices)))

(doc build-cell 'type '(-> Point2 (List Int) (Vector Point2) (List Int)))
(doc build-cell 'description "Build a cell's vertex index list for a site")
(define (build-cell site tri-indices circumcenters)
  ;; Get circumcenters for all triangles adjacent to this site
  (let* ([verts (map (lambda (ti) (vector-ref circumcenters ti)) tri-indices)]
         ;; Order them CCW around the site
         [ordered (order-cell-vertices site verts)])
    ;; Return indices into circumcenters vector
    (map (lambda (v)
           ;; Find index of this vertex
           (let loop ([i 0])
             (if (>= i (vector-length circumcenters))
                 -1  ; shouldn't happen
                 (if (points-equal? v (vector-ref circumcenters i))
                     i
                     (loop (+ i 1))))))
         ordered)))

;;; ============================================================
;;; Section: Main Voronoi Construction
;;; ============================================================

(doc 'section 'construction)

(doc voronoi-diagram 'export #t)
(doc voronoi-diagram 'type '(-> (List Point2) Voronoi))
(doc voronoi-diagram 'description
     "Compute Voronoi diagram from points via Delaunay duality.
      The diagram contains unbounded cells for convex hull vertices.")
(define (voronoi-diagram points)
  (if (< (length points) 3)
      ;; Degenerate cases
      (make-voronoi (list->vector points) '#() '() (make-vector (length points) '()))
      (let* ([sites (list->vector points)]
             [n (vector-length sites)]
             ;; Compute Delaunay triangulation
             [del-tris (delaunay-triangulate points)]
             [triangles (list->vector del-tris)]
             [num-tris (vector-length triangles)]
             ;; Compute circumcenters (Voronoi vertices)
             [circumcenters (let ([v (make-vector num-tris)])
                              (do ([i 0 (+ i 1)])
                                  ((>= i num-tris) v)
                                (vector-set! v i (tri2-circumcenter (vector-ref triangles i)))))]
             ;; Build adjacency structures
             [edge-adj (build-edge-adjacency triangles)]
             [site-tris (build-site-triangles sites triangles)]
             ;; Build Voronoi edges
             [edges (build-voronoi-edges triangles edge-adj sites)]
             ;; Build cells
             [cells (let ([v (make-vector n '())])
                      (do ([i 0 (+ i 1)])
                          ((>= i n) v)
                        (vector-set! v i (build-cell (vector-ref sites i)
                                                     (vector-ref site-tris i)
                                                     circumcenters))))])
        (make-voronoi sites circumcenters edges cells))))

(doc build-voronoi-edges 'type '(-> (Vector Triangle2) Hashtable (Vector Point2) (List VoronoiEdge)))
(doc build-voronoi-edges 'description "Build Voronoi edges from triangle adjacency")
(define (build-voronoi-edges triangles edge-adj sites)
  (let-values ([(keys vals) (hashtable-entries edge-adj)])
    (let loop ([i 0] [edges '()])
      (if (>= i (vector-length keys))
          edges
          (let* ([ek (vector-ref keys i)]
                 [tri-list (vector-ref vals i)]
                 [edge-pts (edge-key->points ek)]
                 [s1 (find-site-index sites (car edge-pts))]
                 [s2 (find-site-index sites (cdr edge-pts))])
            (cond
              ;; Interior edge: connects two circumcenters
              [(= (length tri-list) 2)
               (let ([t1 (car tri-list)]
                     [t2 (cadr tri-list)])
                 (loop (+ i 1) (cons (make-voronoi-edge t1 t2 s1 s2) edges)))]
              ;; Boundary edge: one circumcenter, extends to infinity
              ;; We mark this with -1 for the "infinite" endpoint
              [(= (length tri-list) 1)
               (let ([t1 (car tri-list)])
                 (loop (+ i 1) (cons (make-voronoi-edge t1 -1 s1 s2) edges)))]
              [else (loop (+ i 1) edges)]))))))

(define (edge-key->points key)
  ;; Reconstruct points from edge key (x1 y1 x2 y2)
  (cons (make-point2 (list-ref key 0) (list-ref key 1))
        (make-point2 (list-ref key 2) (list-ref key 3))))

(define (find-site-index sites point)
  (let loop ([i 0])
    (if (>= i (vector-length sites))
        -1
        (if (points-equal? point (vector-ref sites i))
            i
            (loop (+ i 1))))))

;;; ============================================================
;;; Section: Cell Queries
;;; ============================================================

(doc 'section 'queries)

(doc voronoi-cell-polygon 'export #t)
(doc voronoi-cell-polygon 'type '(-> Voronoi Int (List Point2)))
(doc voronoi-cell-polygon 'description
     "Get the polygon vertices for a cell (by site index).
      Returns vertices in CCW order. Empty if cell is unbounded.")
(define (voronoi-cell-polygon vor site-idx)
  (let* ([vertices (voronoi-vertices vor)]
         [cell (vector-ref (voronoi-cells vor) site-idx)])
    (if (exists (lambda (vi) (< vi 0)) cell)
        '()  ; Unbounded cell
        (map (lambda (vi) (vector-ref vertices vi)) cell))))

(doc voronoi-cell-centroid 'export #t)
(doc voronoi-cell-centroid 'type '(-> Voronoi Int (Option Point2)))
(doc voronoi-cell-centroid 'description "Compute centroid of a bounded Voronoi cell")
(define (voronoi-cell-centroid vor site-idx)
  (let ([poly (voronoi-cell-polygon vor site-idx)])
    (if (null? poly)
        #f
        (convex-hull-centroid poly))))

(doc voronoi-neighbors 'export #t)
(doc voronoi-neighbors 'type '(-> Voronoi Int (List Int)))
(doc voronoi-neighbors 'description "Find all neighboring sites of a given site")
(define (voronoi-neighbors vor site-idx)
  (let ([edges (voronoi-edges vor)])
    (fold-right
     (lambda (e acc)
       (cond
         [(= (vedge-site-left e) site-idx)
          (let ([n (vedge-site-right e)])
            (if (and (>= n 0) (not (member n acc)))
                (cons n acc)
                acc))]
         [(= (vedge-site-right e) site-idx)
          (let ([n (vedge-site-left e)])
            (if (and (>= n 0) (not (member n acc)))
                (cons n acc)
                acc))]
         [else acc]))
     '()
     edges)))

(doc voronoi-nearest-site 'export #t)
(doc voronoi-nearest-site 'type '(-> Voronoi Point2 Int))
(doc voronoi-nearest-site 'description "Find the site nearest to a query point (brute force)")
(define (voronoi-nearest-site vor query)
  (let* ([sites (voronoi-sites vor)]
         [n (vector-length sites)])
    (let loop ([i 1]
               [best 0]
               [best-dist (point2-distance-sq query (vector-ref sites 0))])
      (if (>= i n)
          best
          (let ([d (point2-distance-sq query (vector-ref sites i))])
            (if (< d best-dist)
                (loop (+ i 1) i d)
                (loop (+ i 1) best best-dist)))))))

(define (point2-distance-sq p1 p2)
  (let ([dx (- (point2-x p1) (point2-x p2))]
        [dy (- (point2-y p1) (point2-y p2))])
    (+ (* dx dx) (* dy dy))))

;;; ============================================================
;;; Section: Bounded Voronoi (Clipped to Rectangle)
;;; ============================================================

(doc 'section 'bounded)

(doc clip-polygon-to-rect 'type '(-> (List Point2) Number Number Number Number (List Point2)))
(doc clip-polygon-to-rect 'description "Clip a convex polygon to a rectangle using Sutherland-Hodgman")
(define (clip-polygon-to-rect poly x-min x-max y-min y-max)
  (if (null? poly)
      '()
      (let* ([clip1 (clip-to-halfplane poly (lambda (p) (>= (point2-x p) x-min))
                                       (lambda (p1 p2) (intersect-vertical p1 p2 x-min)))]
             [clip2 (clip-to-halfplane clip1 (lambda (p) (<= (point2-x p) x-max))
                                       (lambda (p1 p2) (intersect-vertical p1 p2 x-max)))]
             [clip3 (clip-to-halfplane clip2 (lambda (p) (>= (point2-y p) y-min))
                                       (lambda (p1 p2) (intersect-horizontal p1 p2 y-min)))]
             [clip4 (clip-to-halfplane clip3 (lambda (p) (<= (point2-y p) y-max))
                                       (lambda (p1 p2) (intersect-horizontal p1 p2 y-max)))])
        clip4)))

(define (clip-to-halfplane poly inside? intersect)
  ;; Sutherland-Hodgman for one edge
  (if (null? poly)
      '()
      (let loop ([remaining poly]
                 [prev (car (reverse poly))]
                 [result '()])
        (if (null? remaining)
            (reverse result)
            (let* ([curr (car remaining)]
                   [prev-in (inside? prev)]
                   [curr-in (inside? curr)])
              (cond
                ;; Both inside: add current
                [(and prev-in curr-in)
                 (loop (cdr remaining) curr (cons curr result))]
                ;; Entering: add intersection and current
                [(and (not prev-in) curr-in)
                 (loop (cdr remaining) curr (cons curr (cons (intersect prev curr) result)))]
                ;; Leaving: add intersection
                [(and prev-in (not curr-in))
                 (loop (cdr remaining) curr (cons (intersect prev curr) result))]
                ;; Both outside: skip
                [else
                 (loop (cdr remaining) curr result)]))))))

(define (intersect-vertical p1 p2 x)
  (let* ([x1 (point2-x p1)] [y1 (point2-y p1)]
         [x2 (point2-x p2)] [y2 (point2-y p2)]
         [t (/ (- x x1) (- x2 x1))])
    (make-point2 x (+ y1 (* t (- y2 y1))))))

(define (intersect-horizontal p1 p2 y)
  (let* ([x1 (point2-x p1)] [y1 (point2-y p1)]
         [x2 (point2-x p2)] [y2 (point2-y p2)]
         [t (/ (- y y1) (- y2 y1))])
    (make-point2 (+ x1 (* t (- x2 x1))) y)))

(doc voronoi-bounded 'export #t)
(doc voronoi-bounded 'type '(-> (List Point2) Number Number Number Number Voronoi))
(doc voronoi-bounded 'description
     "Compute Voronoi diagram bounded to a rectangle.
      All cells are clipped to the bounding box, making them finite polygons.")
(define (voronoi-bounded points x-min x-max y-min y-max)
  (let* ([vor (voronoi-diagram points)]
         [sites (voronoi-sites vor)]
         [n (vector-length sites)]
         ;; For unbounded cells (< 3 vertices), compute via bisector clipping
         [bounded-cells
          (let ([v (make-vector n '())])
            (do ([i 0 (+ i 1)])
                ((>= i n) v)
              (let ([cell-verts (voronoi-cell-polygon vor i)])
                (if (< (length cell-verts) 3)
                    ;; Unbounded or degenerate: compute by bisector clipping
                    (let ([bounded (compute-bounded-cell vor i x-min x-max y-min y-max)])
                      (vector-set! v i bounded))
                    ;; Bounded: just clip to rect
                    (vector-set! v i (clip-polygon-to-rect cell-verts x-min x-max y-min y-max))))))])
    ;; Return modified Voronoi with clipped cells stored directly as points
    (list 'voronoi-bounded sites bounded-cells)))

(define (voronoi-bounded? v)
  (and (pair? v) (eq? (car v) 'voronoi-bounded)))

(define (voronoi-bounded-sites v) (list-ref v 1))
(define (voronoi-bounded-cells v) (list-ref v 2))

(doc voronoi-bounded-cell 'export #t)
(doc voronoi-bounded-cell 'type '(-> VoronoiBounded Int (List Point2)))
(doc voronoi-bounded-cell 'description "Get cell polygon from bounded Voronoi")
(define (voronoi-bounded-cell vor site-idx)
  (vector-ref (voronoi-bounded-cells vor) site-idx))

(doc compute-bounded-cell 'type '(-> Voronoi Int Number Number Number Number (List Point2)))
(doc compute-bounded-cell 'description "Compute bounded cell for an unbounded Voronoi cell")
(define (compute-bounded-cell vor site-idx x-min x-max y-min y-max)
  ;; For unbounded cells, we intersect the half-planes with the bounding box
  ;; Start with the bounding box and intersect with each neighbor's bisector
  (let* ([site (vector-ref (voronoi-sites vor) site-idx)]
         [neighbors (voronoi-neighbors vor site-idx)]
         ;; Start with bounding rectangle as polygon
         [rect (list (make-point2 x-min y-min)
                     (make-point2 x-max y-min)
                     (make-point2 x-max y-max)
                     (make-point2 x-min y-max))])
    ;; Clip by each neighbor's bisector half-plane
    (fold-left
     (lambda (poly neighbor-idx)
       (let ([neighbor (vector-ref (voronoi-sites vor) neighbor-idx)])
         (clip-to-bisector poly site neighbor)))
     rect
     neighbors)))

(define (clip-to-bisector poly site neighbor)
  ;; Clip polygon to half-plane closer to site than neighbor
  (let* ([mx (/ (+ (point2-x site) (point2-x neighbor)) 2)]
         [my (/ (+ (point2-y site) (point2-y neighbor)) 2)]
         [dx (- (point2-x neighbor) (point2-x site))]
         [dy (- (point2-y neighbor) (point2-y site))])
    ;; Point is on site's side if dot product with (site - midpoint) > 0
    ;; or equivalently, dot product with (neighbor - site) is < distance²/2
    (clip-to-halfplane
     poly
     (lambda (p)
       ;; p is closer to site than neighbor
       (<= (point2-distance-sq p site)
           (point2-distance-sq p neighbor)))
     (lambda (p1 p2)
       ;; Intersect line p1->p2 with bisector
       (intersect-line-bisector p1 p2 site neighbor)))))

(define (intersect-line-bisector p1 p2 site neighbor)
  ;; Find intersection of line p1->p2 with perpendicular bisector of site-neighbor
  (let* ([mx (/ (+ (point2-x site) (point2-x neighbor)) 2)]
         [my (/ (+ (point2-y site) (point2-y neighbor)) 2)]
         ;; Bisector direction is perpendicular to site->neighbor
         [nx (- (point2-y neighbor) (point2-y site))]  ; perpendicular
         [ny (- (point2-x site) (point2-x neighbor))]  ; perpendicular
         ;; Line direction
         [dx (- (point2-x p2) (point2-x p1))]
         [dy (- (point2-y p2) (point2-y p1))]
         ;; Solve: p1 + t*(p2-p1) lies on bisector
         ;; (p1 + t*d - m) · n = 0
         ;; t = (m - p1) · (neighbor - site) / (d · (neighbor - site))
         [snx (- (point2-x neighbor) (point2-x site))]
         [sny (- (point2-y neighbor) (point2-y site))]
         [denom (+ (* dx snx) (* dy sny))])
    (if (< (abs denom) 1e-10)
        ;; Parallel - shouldn't happen in valid cases
        (make-point2 mx my)
        (let* ([num (+ (* (- mx (point2-x p1)) snx)
                       (* (- my (point2-y p1)) sny))]
               [t (/ num denom)])
          (make-point2 (+ (point2-x p1) (* t dx))
                       (+ (point2-y p1) (* t dy)))))))

;;; ============================================================
;;; Section: Visualization
;;; ============================================================

(doc 'section 'visualization)

(doc render-voronoi 'export #t)
(doc render-voronoi 'type '(-> VoronoiBounded Nat Nat String))
(doc render-voronoi 'description "Render bounded Voronoi diagram as ASCII art")
(define (render-voronoi vor width height)
  (let* ([sites (voronoi-bounded-sites vor)]
         [cells (voronoi-bounded-cells vor)]
         [n (vector-length sites)]
         ;; Find bounds from sites
         [all-pts (let loop ([i 0] [pts '()])
                    (if (>= i n)
                        pts
                        (loop (+ i 1)
                              (append (vector-ref cells i) pts))))]
         [xs (map point2-x all-pts)]
         [ys (map point2-y all-pts)]
         [x-min (if (null? xs) 0 (apply min xs))]
         [x-max (if (null? xs) 1 (apply max xs))]
         [y-min (if (null? ys) 0 (apply min ys))]
         [y-max (if (null? ys) 1 (apply max ys))]
         [x-range (max 0.001 (- x-max x-min))]
         [y-range (max 0.001 (- y-max y-min))]
         [buffer (make-vector (* width height) #\space)])
    ;; Scale point to buffer coordinates
    (define (scale-x x)
      (inexact->exact (round (* (- width 1) (/ (- x x-min) x-range)))))
    (define (scale-y y)
      (inexact->exact (round (* (- height 1) (/ (- y y-min) y-range)))))
    (define (plot x y c)
      (let ([sx (max 0 (min (- width 1) (scale-x x)))]
            [sy (max 0 (min (- height 1) (scale-y y)))])
        (let ([idx (+ sx (* sy width))])
          (when (and (>= idx 0) (< idx (* width height)))
            (vector-set! buffer idx c)))))
    ;; Draw line between two points
    (define (draw-line p1 p2 c)
      (let* ([x1 (scale-x (point2-x p1))]
             [y1 (scale-y (point2-y p1))]
             [x2 (scale-x (point2-x p2))]
             [y2 (scale-y (point2-y p2))]
             [dx (- x2 x1)]
             [dy (- y2 y1)]
             [steps (max 1 (max (abs dx) (abs dy)))])
        (do ([i 0 (+ i 1)])
            ((> i steps))
          (let* ([t (/ i steps)]
                 [x (inexact->exact (round (+ x1 (* t dx))))]
                 [y (inexact->exact (round (+ y1 (* t dy))))]
                 [idx (+ (max 0 (min (- width 1) x))
                         (* (max 0 (min (- height 1) y)) width))])
            (when (and (>= idx 0) (< idx (* width height)))
              (vector-set! buffer idx c))))))
    ;; Draw cell edges
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (let ([cell (vector-ref cells i)])
        (unless (null? cell)
          (let loop ([remaining cell])
            (unless (null? remaining)
              (let ([p1 (car remaining)]
                    [p2 (if (null? (cdr remaining)) (car cell) (cadr remaining))])
                (draw-line p1 p2 #\.)
                (loop (cdr remaining))))))))
    ;; Mark sites
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (let ([site (vector-ref sites i)])
        (plot (point2-x site) (point2-y site)
              (integer->char (+ (char->integer #\0) (modulo i 10))))))
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
                      result))))))

;;; ============================================================
;;; Section: Statistics
;;; ============================================================

(doc 'section 'statistics)

(doc voronoi-cell-areas 'export #t)
(doc voronoi-cell-areas 'type '(-> VoronoiBounded (List Number)))
(doc voronoi-cell-areas 'description "Compute areas of all cells in bounded Voronoi")
(define (voronoi-cell-areas vor)
  (let* ([cells (voronoi-bounded-cells vor)]
         [n (vector-length cells)])
    (let loop ([i 0] [areas '()])
      (if (>= i n)
          (reverse areas)
          (let ([cell (vector-ref cells i)])
            (loop (+ i 1)
                  (cons (if (< (length cell) 3)
                            0
                            (convex-hull-area cell))
                        areas)))))))

(doc voronoi-summary 'export #t)
(doc voronoi-summary 'type '(-> VoronoiBounded Void))
(doc voronoi-summary 'description "Print summary statistics for a bounded Voronoi diagram")
(define (voronoi-summary vor)
  (let* ([sites (voronoi-bounded-sites vor)]
         [n (vector-length sites)]
         [areas (voronoi-cell-areas vor)]
         [total-area (apply + areas)])
    (printf "Voronoi Diagram Summary~n")
    (printf "  Sites: ~a~n" n)
    (printf "  Total area: ~,2f~n" total-area)
    (printf "  Cell areas: min=~,2f  max=~,2f  avg=~,2f~n"
            (apply min areas)
            (apply max areas)
            (/ total-area n))
    (printf "  Std dev: ~,2f~n"
            (let ([mean (/ total-area n)])
              (sqrt (/ (apply + (map (lambda (a) (expt (- a mean) 2)) areas))
                       n))))))

;;; ============================================================
;;; Section: Lloyd's Algorithm (Voronoi Relaxation)
;;; ============================================================

(doc 'section 'relaxation)

(doc lloyd-step 'export #t)
(doc lloyd-step 'type '(-> (List Point2) Number Number Number Number (List Point2)))
(doc lloyd-step 'description
     "One step of Lloyd's algorithm: move each site to its cell's centroid.
      This produces a more uniform distribution of points.")
(define (lloyd-step points x-min x-max y-min y-max)
  (let* ([vor (voronoi-bounded points x-min x-max y-min y-max)]
         [n (vector-length (voronoi-bounded-sites vor))])
    (let loop ([i 0] [new-pts '()])
      (if (>= i n)
          (reverse new-pts)
          (let* ([cell (voronoi-bounded-cell vor i)]
                 [centroid (if (< (length cell) 3)
                               (vector-ref (voronoi-bounded-sites vor) i)
                               (convex-hull-centroid cell))])
            (loop (+ i 1) (cons centroid new-pts)))))))

(doc lloyd-relax 'export #t)
(doc lloyd-relax 'type '(-> (List Point2) Number Number Number Number Nat (List Point2)))
(doc lloyd-relax 'description "Apply Lloyd's algorithm for n iterations")
(define (lloyd-relax points x-min x-max y-min y-max iterations)
  (let loop ([pts points] [i 0])
    (if (>= i iterations)
        pts
        (loop (lloyd-step pts x-min x-max y-min y-max) (+ i 1)))))

(printf "voronoi.ss loaded~n")
(printf "  (voronoi-diagram points)               - Compute Voronoi from points~n")
(printf "  (voronoi-bounded points x0 x1 y0 y1)   - Bounded Voronoi~n")
(printf "  (voronoi-bounded-cell vor i)           - Get cell polygon~n")
(printf "  (voronoi-neighbors vor i)              - Find neighboring sites~n")
(printf "  (voronoi-nearest-site vor query)       - Find nearest site~n")
(printf "  (lloyd-relax points ... iterations)    - Lloyd relaxation~n")
(printf "  (render-voronoi vor w h)               - ASCII visualization~n")
