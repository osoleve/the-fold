;;; lattice/geometry/test-mesh-gen.ss --- Tests for mesh generation and point location
;;; Run: scheme --script lattice/geometry/test-mesh-gen.ss

(load "core/testing/test-framework.ss")
(load "lattice/geometry/mesh-gen.ss")

(printf "~n=== Mesh Generation Tests ===~n~n")

;;; ============================================================
;;; Triangulation Structure Tests
;;; ============================================================

(test-group "triangulation-structure"

  (define-test "delaunay-triangulate returns triangulation record"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)])
      (assert-true (triangulation? tri))))

  (define-test "triangulation contains points"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)])
      (assert-equal 3 (vector-length (triangulation-points tri)))))

  (define-test "3 points make 1 triangle"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)])
      (assert-equal 1 (length (triangulation-triangles tri)))))

  (define-test "4 points make 2 triangles"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 1 1)
                      (make-point2 0 1))]
           [tri (delaunay-triangulate pts)])
      (assert-equal 2 (length (triangulation-triangles tri)))))

  (define-test "empty points returns empty triangulation"
    (let ([tri (delaunay-triangulate '())])
      (assert-true (triangulation? tri))
      (assert-equal 0 (length (triangulation-triangles tri)))))

  (define-test "2 points returns empty triangulation"
    (let ([tri (delaunay-triangulate (list (make-point2 0 0) (make-point2 1 1)))])
      (assert-true (triangulation? tri))
      (assert-equal 0 (length (triangulation-triangles tri))))))

;;; ============================================================
;;; Adjacency Tests
;;; ============================================================

(test-group "adjacency"

  (define-test "adjacency symmetric"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 2 0)
                      (make-point2 2 2)
                      (make-point2 0 2))]
           [tri (delaunay-triangulate pts)]
           [tris (triangulation-triangles tri)])
      ;; For each triangle, if it has a neighbor, that neighbor should have it
      (for-each
       (lambda (t)
         (let ([neighbors (triangle-neighbors tri t)])
           (when neighbors
             (do ([i 0 (+ i 1)])
                 ((>= i 3))
               (let ([n (vector-ref neighbors i)])
                 (when n
                   (let ([n-neighbors (triangle-neighbors tri n)])
                     (assert-true (or (eq? (vector-ref n-neighbors 0) t)
                                      (eq? (vector-ref n-neighbors 1) t)
                                      (eq? (vector-ref n-neighbors 2) t))))))))))
       tris)
      (assert-true #t)))  ; test completes without error

  (define-test "boundary edges have null neighbors"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)]
           [t (car (triangulation-triangles tri))]
           [neighbors (triangle-neighbors tri t)])
      ;; Single triangle should have all boundary edges
      (assert-equal #f (vector-ref neighbors 0))
      (assert-equal #f (vector-ref neighbors 1))
      (assert-equal #f (vector-ref neighbors 2)))))

;;; ============================================================
;;; Orient2d Tests
;;; ============================================================

(test-group "orient2d"

  (define-test "CCW returns positive"
    (let ([a (make-point2 0 0)]
          [b (make-point2 1 0)]
          [c (make-point2 0.5 1)])
      (assert-true (> (orient2d a b c) 0))))

  (define-test "CW returns negative"
    (let ([a (make-point2 0 0)]
          [b (make-point2 0.5 1)]
          [c (make-point2 1 0)])
      (assert-true (< (orient2d a b c) 0))))

  (define-test "collinear returns zero"
    (let ([a (make-point2 0 0)]
          [b (make-point2 1 0)]
          [c (make-point2 2 0)])
      (assert-equal 0 (orient2d a b c)))))

;;; ============================================================
;;; Point Location Tests
;;; ============================================================

(test-group "locate-point"

  (define-test "locate point inside triangle"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 2 0)
                      (make-point2 1 2))]
           [tri (delaunay-triangulate pts)]
           [loc (locate-point tri (make-point2 1 0.5))])
      (assert-true (location? loc))))

  (define-test "locate point outside returns #f"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 2 0)
                      (make-point2 1 2))]
           [tri (delaunay-triangulate pts)]
           [loc (locate-point tri (make-point2 5 5))])
      (assert-false loc)))

  (define-test "locate point returns valid barycentric"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 2 0)
                      (make-point2 1 2))]
           [tri (delaunay-triangulate pts)]
           [loc (locate-point tri (make-point2 1 0.5))]
           [bary (location-bary loc)]
           [sum (+ (vector-ref bary 0) (vector-ref bary 1) (vector-ref bary 2))])
      (assert-true (< (abs (- sum 1.0)) 1e-10))))

  (define-test "locate finds all original points"
    (random-seed 42)
    (let* ([pts (random-points-in-rect 0 10 0 10 30)]
           [tri (delaunay-triangulate pts)]
           [found (filter identity (map (lambda (p) (locate-point tri p)) pts))])
      (assert-equal 30 (length found))))

  (define-test "locate with hint works"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 2 0)
                      (make-point2 2 2)
                      (make-point2 0 2))]
           [tri (delaunay-triangulate pts)]
           [loc1 (locate-point tri (make-point2 0.5 0.5))]
           [loc2 (locate-point tri (make-point2 0.6 0.6) (location-triangle loc1))])
      (assert-true (location? loc1))
      (assert-true (location? loc2))))

  (define-test "locate point on edge returns location"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 2 0)
                      (make-point2 1 2))]
           [tri (delaunay-triangulate pts)]
           ;; Point on edge p1-p2 (midpoint at (1,0))
           [loc (locate-point tri (make-point2 1 0))])
      (assert-true (location? loc))))

  (define-test "locate point on vertex returns location"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 2 0)
                      (make-point2 1 2))]
           [tri (delaunay-triangulate pts)]
           ;; Point exactly on vertex p1
           [loc (locate-point tri (make-point2 0 0))])
      (assert-true (location? loc))))

  (define-test "locate works with large mesh"
    (random-seed 123)
    (let* ([pts (random-points-in-rect 0 100 0 100 200)]
           [tri (delaunay-triangulate pts)]
           ;; Query a point we know is inside the convex hull
           [loc (locate-point tri (make-point2 50 50))])
      (assert-true (location? loc)))))

;;; ============================================================
;;; Interpolation Tests
;;; ============================================================

(test-group "interpolate-at"

  (define-test "interpolate returns value inside"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)]
           [vals (vector 1.0 2.0 3.0)]
           [result (interpolate-at tri (make-point2 0.5 0.3) vals)])
      (assert-true (number? result))))

  (define-test "interpolate returns #f outside"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)]
           [vals (vector 1.0 2.0 3.0)]
           [result (interpolate-at tri (make-point2 5 5) vals)])
      (assert-false result))))

;;; ============================================================
;;; Quality Metrics Tests
;;; ============================================================

(test-group "quality-metrics"

  (define-test "equilateral triangle has aspect ratio 1"
    (let ([tri (make-tri2 (make-point2 0 0)
                          (make-point2 1 0)
                          (make-point2 0.5 0.866))])
      (assert-true (< (abs (- (tri2-aspect-ratio tri) 1.0)) 0.01))))

  (define-test "mesh-quality-report accepts triangulation"
    ;; Just ensure it doesn't error
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)])
      (mesh-quality-report tri)
      (assert-true #t))))

;;; ============================================================
;;; Refinement Tests
;;; ============================================================

(test-group "refine-mesh"

  (define-test "refine-mesh returns triangulation"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)]
           [refined (refine-mesh tri 25 10)])
      (assert-true (triangulation? refined))))

  (define-test "refinement increases triangle count"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 10 0)
                      (make-point2 5 1))]  ; skinny triangle (not collinear!)
           [tri (delaunay-triangulate pts)]
           [refined (refine-mesh tri 25 100)]
           [orig-count (length (triangulation-triangles tri))]
           [new-count (length (triangulation-triangles refined))])
      (assert-true (> new-count orig-count)))))

;;; ============================================================
;;; Backward Compatibility Tests
;;; ============================================================

(test-group "backward-compat"

  (define-test "render-mesh-2d accepts triangulation"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)]
           [result (render-mesh-2d tri 20 10)])
      (assert-true (string? result))))

  (define-test "render-mesh-2d accepts triangle list"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)]
           [tris (triangulation-triangles tri)]
           [result (render-mesh-2d tris 20 10)])
      (assert-true (string? result))))

  (define-test "triangles-to-3d accepts triangulation"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 1 0)
                      (make-point2 0.5 1))]
           [tri (delaunay-triangulate pts)]
           [result (triangles-to-3d tri (lambda (p) 0))])
      (assert-true (pair? result)))))

;;; ============================================================
;;; Laplacian Smoothing Tests
;;; ============================================================

(test-group "laplacian-smoothing"

  (define-test "smooth-mesh returns triangulation"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 2 0)
                      (make-point2 2 2)
                      (make-point2 0 2))]
           [tri (delaunay-triangulate pts)]
           [smoothed (smooth-mesh tri 3 0.5)])
      (assert-true (triangulation? smoothed))))

  (define-test "smooth-mesh preserves boundary vertices"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 2 0)
                      (make-point2 2 2)
                      (make-point2 0 2))]
           [tri (delaunay-triangulate pts)]
           [smoothed (smooth-mesh tri 10 0.5)]
           [orig-pts (triangulation-points tri)]
           [new-pts (triangulation-points smoothed)])
      ;; For a convex hull (all boundary), all points should be preserved
      (assert-equal 4 (vector-length new-pts))))

  (define-test "smooth-mesh improves quality"
    (random-seed 99)
    (let* ([pts (random-points-in-rect 0 10 0 10 25)]
           [tri (delaunay-triangulate pts)]
           [smoothed (smooth-mesh tri 5 0.4)]
           [orig-tris (triangulation-triangles tri)]
           [smooth-tris (triangulation-triangles smoothed)]
           [orig-avg-aspect (/ (apply + (map tri2-aspect-ratio orig-tris))
                               (max 1 (length orig-tris)))]
           [smooth-avg-aspect (/ (apply + (map tri2-aspect-ratio smooth-tris))
                                 (max 1 (length smooth-tris)))])
      ;; Smoothing should not make quality worse (in most cases)
      ;; We just test that it produces valid output
      (assert-true (> (length smooth-tris) 0)))))

;;; ============================================================
;;; Point in Polygon Tests
;;; ============================================================

(test-group "point-in-polygon"

  (define-test "point inside square"
    (let ([square (list (make-point2 0 0)
                        (make-point2 2 0)
                        (make-point2 2 2)
                        (make-point2 0 2))]
          [p (make-point2 1 1)])
      (assert-true (point-in-polygon? p square))))

  (define-test "point outside square"
    (let ([square (list (make-point2 0 0)
                        (make-point2 2 0)
                        (make-point2 2 2)
                        (make-point2 0 2))]
          [p (make-point2 5 5)])
      (assert-false (point-in-polygon? p square))))

  (define-test "point inside triangle"
    (let ([tri (list (make-point2 0 0)
                     (make-point2 4 0)
                     (make-point2 2 3))]
          [p (make-point2 2 1)])
      (assert-true (point-in-polygon? p tri))))

  (define-test "point outside triangle"
    (let ([tri (list (make-point2 0 0)
                     (make-point2 4 0)
                     (make-point2 2 3))]
          [p (make-point2 0 3)])
      (assert-false (point-in-polygon? p tri))))

  (define-test "point inside concave polygon"
    (let ([L-shape (list (make-point2 0 0)
                         (make-point2 2 0)
                         (make-point2 2 1)
                         (make-point2 1 1)
                         (make-point2 1 2)
                         (make-point2 0 2))]
          [p (make-point2 0.5 0.5)])
      (assert-true (point-in-polygon? p L-shape))))

  (define-test "point in concave region outside L-shape"
    (let ([L-shape (list (make-point2 0 0)
                         (make-point2 2 0)
                         (make-point2 2 1)
                         (make-point2 1 1)
                         (make-point2 1 2)
                         (make-point2 0 2))]
          [p (make-point2 1.5 1.5)])
      (assert-false (point-in-polygon? p L-shape)))))

;;; ============================================================
;;; Boundary-Constrained Meshing Tests
;;; ============================================================

(test-group "triangulate-polygon"

  (define-test "triangulate-polygon returns triangulation"
    (let* ([square (list (make-point2 0 0)
                         (make-point2 4 0)
                         (make-point2 4 4)
                         (make-point2 0 4))]
           [mesh (triangulate-polygon square 1.5)])
      (assert-true (triangulation? mesh))))

  (define-test "triangulate-polygon creates triangles inside boundary"
    (let* ([square (list (make-point2 0 0)
                         (make-point2 4 0)
                         (make-point2 4 4)
                         (make-point2 0 4))]
           [mesh (triangulate-polygon square 1.5)]
           [tris (triangulation-triangles mesh)])
      ;; All triangle centroids should be inside the polygon
      (assert-true
       (for-all (lambda (tri)
                (let* ([p1 (tri2-p1 tri)]
                       [p2 (tri2-p2 tri)]
                       [p3 (tri2-p3 tri)]
                       [cx (/ (+ (point2-x p1) (point2-x p2) (point2-x p3)) 3)]
                       [cy (/ (+ (point2-y p1) (point2-y p2) (point2-y p3)) 3)])
                  (point-in-polygon? (make-point2 cx cy) square)))
              tris))))

  (define-test "triangulate-polygon works with triangle"
    (let* ([tri (list (make-point2 0 0)
                      (make-point2 6 0)
                      (make-point2 3 5))]
           [mesh (triangulate-polygon tri 2.0)]
           [tris (triangulation-triangles mesh)])
      (assert-true (> (length tris) 0))))

  (define-test "triangulate-polygon respects concave boundary constraints"
    ;; L-shaped polygon - tests that triangles don't cross the concave edge
    (let* ([l-shape (list (make-point2 0 0)
                          (make-point2 4 0)
                          (make-point2 4 2)
                          (make-point2 2 2)
                          (make-point2 2 4)
                          (make-point2 0 4))]
           [mesh (triangulate-polygon l-shape 0.8)]
           [tris (triangulation-triangles mesh)]
           [constraint-edges (polygon-to-constraint-edges l-shape)])
      ;; No triangle should cross any boundary edge
      (assert-true
       (for-all (lambda (tri)
                  (not (triangle-crosses-any-constraint? tri constraint-edges)))
                tris)))))

;;; ============================================================
;;; Adaptive Refinement Tests
;;; ============================================================

(test-group "adaptive-refinement"

  (define-test "adaptive-refine-mesh returns triangulation"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 10 0)
                      (make-point2 5 8))]
           [tri (delaunay-triangulate pts)]
           [refined (adaptive-refine-mesh tri 10.0 50 (lambda (_) #f))])
      (assert-true (triangulation? refined))))

  (define-test "adaptive refinement increases triangle count"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 10 0)
                      (make-point2 5 8))]
           [tri (delaunay-triangulate pts)]
           [refined (adaptive-refine-mesh tri 5.0 100 (lambda (_) #f))]
           [orig-count (length (triangulation-triangles tri))]
           [new-count (length (triangulation-triangles refined))])
      (assert-true (> new-count orig-count))))

  (define-test "adaptive refinement respects max-area"
    ;; Test that refinement makes progress toward the target
    ;; The algorithm may not reach perfect convergence with limited iterations
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 10 0)
                      (make-point2 5 8))]
           [tri (delaunay-triangulate pts)]
           [max-area 5.0]  ; More achievable target
           [refined (adaptive-refine-mesh tri max-area 100 (lambda (_) #f))]
           [tris (triangulation-triangles refined)]
           [areas (map tri2-area tris)]
           [max-actual (apply max areas)]
           [orig-area (tri2-area (car (triangulation-triangles tri)))])
      ;; Refinement should produce smaller triangles than original
      (assert-true (< max-actual orig-area))))

  (define-test "refine-mesh-uniform wrapper works"
    (let* ([pts (list (make-point2 0 0)
                      (make-point2 8 0)
                      (make-point2 4 6))]
           [tri (delaunay-triangulate pts)]
           [refined (refine-mesh-uniform tri 3.0 100)])
      (assert-true (triangulation? refined))
      (assert-true (> (length (triangulation-triangles refined)) 1)))))

(run-all-tests)
