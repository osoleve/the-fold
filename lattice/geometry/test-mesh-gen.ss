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
      (assert-true (location? loc2)))))

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

(run-all-tests)
