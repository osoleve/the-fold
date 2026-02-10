;;; lattice/geometry/test-voronoi.ss --- Tests for Voronoi diagrams

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/geometry/voronoi.ss")

(test-group "voronoi-basic"

  (define-test "voronoi-from-3-points"
    ;; Three points form one Delaunay triangle
    ;; The single circumcenter is the Voronoi vertex
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 0.866))]  ; roughly equilateral
           [vor (voronoi-diagram pts)])
      (assert-true (voronoi? vor))
      (assert-equal 3 (vector-length (voronoi-sites vor)))
      ;; One triangle means one vertex (circumcenter)
      (assert-equal 1 (vector-length (voronoi-vertices vor)))))

  (define-test "voronoi-from-4-points-square"
    ;; Square: 2 Delaunay triangles, so 2 Voronoi vertices
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 1 1)
                      (make-point2 0 1))]
           [vor (voronoi-diagram pts)])
      (assert-true (voronoi? vor))
      (assert-equal 4 (vector-length (voronoi-sites vor)))
      (assert-equal 2 (vector-length (voronoi-vertices vor)))))

  (define-test "voronoi-neighbors"
    ;; In a square, each corner is adjacent to 2 others
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 1 1)
                      (make-point2 0 1))]
           [vor (voronoi-diagram pts)]
           [n0 (voronoi-neighbors vor 0)])
      ;; Site 0 should have neighbors (sites 1 and 3 in a square)
      (assert-true (>= (length n0) 2))))

  (define-test "voronoi-nearest-site"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 10 0)
                      (make-point2 5 10))]
           [vor (voronoi-diagram pts)])
      ;; Query point near (0,0) should return site 0
      (assert-equal 0 (voronoi-nearest-site vor (make-point2 0.1 0.1)))
      ;; Query point near (10,0) should return site 1
      (assert-equal 1 (voronoi-nearest-site vor (make-point2 9.9 0.1)))
      ;; Query point near (5,10) should return site 2
      (assert-equal 2 (voronoi-nearest-site vor (make-point2 5 9)))))
)

(test-group "voronoi-bounded"

  (define-test "bounded-voronoi-clips-cells"
    (let* ([pts (list (make-point2 0.5 0.5)
                      (make-point2 0.2 0.2)
                      (make-point2 0.8 0.2)
                      (make-point2 0.8 0.8)
                      (make-point2 0.2 0.8))]
           [vor (voronoi-bounded pts 0 1 0 1)])
      (assert-true (voronoi-bounded? vor))
      (assert-equal 5 (vector-length (voronoi-bounded-sites vor)))
      ;; All cells should be non-empty polygons
      (do ([i 0 (+ i 1)])
          ((>= i 5))
        (let ([cell (voronoi-bounded-cell vor i)])
          (assert-true (>= (length cell) 3))))))

  (define-test "bounded-voronoi-cell-areas-positive"
    (let* ([pts (list (make-point2 0.25 0.25)
                      (make-point2 0.75 0.25)
                      (make-point2 0.75 0.75)
                      (make-point2 0.25 0.75))]
           [vor (voronoi-bounded pts 0 1 0 1)]
           [areas (voronoi-cell-areas vor)])
      ;; All areas should be positive
      (assert-true (for-all (lambda (a) (> a 0)) areas))
      ;; Total area should approximately equal bounding box area (1.0)
      (assert-true (< (abs (- (apply + areas) 1.0)) 0.1))))

  (define-test "bounded-voronoi-render"
    (let* ([pts (list (make-point2 0.5 0.5)
                      (make-point2 0.2 0.2)
                      (make-point2 0.8 0.8))]
           [vor (voronoi-bounded pts 0 1 0 1)]
           [rendered (render-voronoi vor 20 10)])
      ;; Should produce a non-empty string
      (assert-true (> (string-length rendered) 0))
      ;; Should contain site markers (digits)
      (assert-true (or (string-contains rendered "0")
                       (string-contains rendered "1")
                       (string-contains rendered "2")))))
)

(test-group "lloyd-relaxation"

  (define-test "lloyd-single-step"
    (let* ([pts (list (make-point2 0.1 0.1)
                      (make-point2 0.9 0.1)
                      (make-point2 0.5 0.9))]
           [relaxed (lloyd-step pts 0 1 0 1)])
      ;; Should return same number of points
      (assert-equal 3 (length relaxed))
      ;; Points should have moved (centroids differ from original)
      (let ([moved (exists (lambda (pair)
                          (let ([orig (car pair)]
                                [new (cdr pair)])
                            (or (> (abs (- (point2-x orig) (point2-x new))) 0.001)
                                (> (abs (- (point2-y orig) (point2-y new))) 0.001))))
                        (map cons pts relaxed))])
        (assert-true moved))))

  (define-test "lloyd-converges"
    ;; After several iterations, points should be more evenly spaced
    (let* ([pts (list (make-point2 0.1 0.1)
                      (make-point2 0.15 0.1)  ; Two close points
                      (make-point2 0.9 0.9))]
           [relaxed (lloyd-relax pts 0 1 0 1 5)])
      ;; The two close points should have spread apart
      (let* ([d-orig (point2-distance-sq (car pts) (cadr pts))]
             [d-new (point2-distance-sq (car relaxed) (cadr relaxed))])
        ;; Distance should have increased
        (assert-true (> d-new d-orig)))))
)

(test-group "edge-cases"

  (define-test "voronoi-collinear-points"
    ;; Collinear points are degenerate for Delaunay
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 2 0))]
           [vor (voronoi-diagram pts)])
      ;; Should still return a valid (possibly empty) structure
      (assert-true (voronoi? vor))))

  (define-test "voronoi-two-points"
    ;; Two points: degenerate case
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 1))]
           [vor (voronoi-diagram pts)])
      (assert-true (voronoi? vor))
      (assert-equal 2 (vector-length (voronoi-sites vor)))))

  (define-test "voronoi-co-circular-points"
    ;; Four points on a circle share a circumcenter (co-circular)
    ;; This tests the duplicate circumcenter handling
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 1 1)
                      (make-point2 0 1))]  ; Square = all co-circular
           [vor (voronoi-diagram pts)])
      ;; Should have 4 sites
      (assert-equal 4 (vector-length (voronoi-sites vor)))
      ;; Should have 2 vertices (circumcenters) - the square has 2 triangles
      (assert-equal 2 (vector-length (voronoi-vertices vor)))
      ;; All 4 cells should be unbounded (on convex hull)
      (assert-true (voronoi-cell-unbounded? vor 0))
      (assert-true (voronoi-cell-unbounded? vor 1))
      (assert-true (voronoi-cell-unbounded? vor 2))
      (assert-true (voronoi-cell-unbounded? vor 3))))

  (define-test "voronoi-hull-site-many-neighbors"
    ;; A site on the hull with many neighbors should produce valid bounded cell
    ;; Center point surrounded by semi-circle of points
    (let* ([center (make-point2 0.5 0.5)]
           ;; 5 points on an arc around the center (all on hull)
           [arc-pts (list (make-point2 0.2 0.2)
                          (make-point2 0.1 0.5)
                          (make-point2 0.2 0.8)
                          (make-point2 0.5 0.9)
                          (make-point2 0.8 0.8))]
           [pts (cons center arc-pts)]
           [vor (voronoi-bounded pts 0 1 0 1)])
      ;; Center cell (index 0) should be bounded and have valid polygon
      (let ([center-cell (voronoi-bounded-cell vor 0)])
        (assert-true (>= (length center-cell) 3)))
      ;; Hull sites should also produce valid bounded cells
      (do ([i 1 (+ i 1)])
          ((>= i 6))
        (let ([cell (voronoi-bounded-cell vor i)])
          (assert-true (>= (length cell) 3))))))
)

;;; Helper for string-contains
(define (string-contains str substr)
  (let ([len (string-length str)]
        [sublen (string-length substr)])
    (let loop ([i 0])
      (cond
        [(> (+ i sublen) len) #f]
        [(string=? (substring str i (+ i sublen)) substr) #t]
        [else (loop (+ i 1))]))))

(run-all-tests)
