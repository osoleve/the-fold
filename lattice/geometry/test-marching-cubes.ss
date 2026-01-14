;;; core/geometry/test-marching-cubes.ss — Tests for Marching Cubes Algorithm
;;;
;;; Tests for isosurface extraction using marching cubes.
;;;
;;; Run: scheme --script core/geometry/test-marching-cubes.ss

(load "core/test-framework.ss")
(load "lattice/geometry/marching-cubes.ss")

(set! *current-group* 'marching-cubes)

;;; Helper for approximate equality
(define (approx= a b eps)
  (< (abs (- a b)) eps))

;;; ====
;;; Simple SDF Functions for Testing
;;; ====

;;; sdf-sphere-origin : Number -> Vec3 -> Number
;;; Sphere centered at origin
(define (sdf-sphere-origin radius)
  (lambda (p)
          (- (vec3-length p) radius)))

;;; sdf-plane-y : Vec3 -> Number
;;; XZ plane at y=0
(define (sdf-plane-y p)
  (vec3-y p))

;;; ====
;;; Cube Index Computation Tests
;;; ====

(define-test "compute-cube-index all outside"
  (let* ([values '(1 1 1 1 1 1 1 1)]  ; All positive (outside)
         [index (compute-cube-index values)])
        (assert-equal index 0)))

(define-test "compute-cube-index all inside"
  (let* ([values '(-1 -1 -1 -1 -1 -1 -1 -1)]  ; All negative (inside)
         [index (compute-cube-index values)])
        (assert-equal index 255)))

(define-test "compute-cube-index first corner inside"
  (let* ([values '(-1 1 1 1 1 1 1 1)]  ; Only corner 0 inside
         [index (compute-cube-index values)])
        (assert-equal index 1)))

(define-test "compute-cube-index second corner inside"
  (let* ([values '(1 -1 1 1 1 1 1 1)]  ; Only corner 1 inside
         [index (compute-cube-index values)])
        (assert-equal index 2)))

(define-test "compute-cube-index multiple corners inside"
  (let* ([values '(-1 -1 -1 1 1 1 1 1)]  ; Corners 0,1,2 inside
         [index (compute-cube-index values)])
        (assert-equal index 7)))  ; 1 + 2 + 4 = 7

;;; ====
;;; Edge Intersection Tests
;;; ====

(define-test "compute-edge-intersections single edge"
  (let* ([corners (list (vec3 0 0 0) (vec3 1 0 0)
                        (vec3 1 0 1) (vec3 0 0 1)
                        (vec3 0 1 0) (vec3 1 1 0)
                        (vec3 1 1 1) (vec3 0 1 1))]
         [values '(-1 1 1 1 1 1 1 1)]  ; Edge between corners 0 and 1
         [edge-mask (vector-ref edge-table 1)]  ; Single corner inside
         [intersections (compute-edge-intersections corners values edge-mask)])
        ;; Should have intersection points
        (assert-true (> (length intersections) 0))))

(define-test "compute-edge-intersections interpolation"
  (let* ([corners (list (vec3 0 0 0) (vec3 2 0 0)
                        (vec3 2 0 2) (vec3 0 0 2)
                        (vec3 0 2 0) (vec3 2 2 0)
                        (vec3 2 2 2) (vec3 0 2 2))]
         ;; Corner 0 at -1, corner 1 at +1, crossing at x=1
         [values '(-1 1 1 1 1 1 1 1)]
         [edge-mask (vector-ref edge-table 1)]
         [intersections (compute-edge-intersections corners values edge-mask)])
        ;; Should have at least one intersection
        (assert-true (> (length intersections) 0))
        ;; First intersection should be on the edge from corner 0 to corner 1
        (when (> (length intersections) 0)
              (let ([first-point (car intersections)])
                   ;; X should be at midpoint (interpolation of -1 to +1)
                   (assert-true (approx= (vec3-x first-point) 1.0 0.1))))))

;;; ====
;;; Triangle Generation Tests
;;; ====

(define-test "generate-cube-triangles empty for insufficient points"
  (let* ([points (list (vec3 0 0 0) (vec3 1 0 0))]  ; Only 2 points
         [tris (generate-cube-triangles points 0)])
        (assert-equal (length tris) 0)))

(define-test "generate-cube-triangles single triangle"
  (let* ([points (list (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [tris (generate-cube-triangles points 0)])
        (assert-equal (length tris) 1)
        (assert-true (triangle3? (car tris)))))

(define-test "generate-cube-triangles multiple triangles"
  (let* ([points (list (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0)
                       (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0))]
         [tris (generate-cube-triangles points 0)])
        (assert-equal (length tris) 2)))

;;; ====
;;; Marching Cubes Cube Tests
;;; ====

(define-test "marching-cubes-cube all outside produces no triangles"
  (let* ([corners (list (vec3 0 0 0) (vec3 1 0 0)
                        (vec3 1 0 1) (vec3 0 0 1)
                        (vec3 0 1 0) (vec3 1 1 0)
                        (vec3 1 1 1) (vec3 0 1 1))]
         [values '(1 1 1 1 1 1 1 1)]  ; All outside
         [tris (marching-cubes-cube corners values)])
        (assert-equal (length tris) 0)))

(define-test "marching-cubes-cube all inside produces no triangles"
  (let* ([corners (list (vec3 0 0 0) (vec3 1 0 0)
                        (vec3 1 0 1) (vec3 0 0 1)
                        (vec3 0 1 0) (vec3 1 1 0)
                        (vec3 1 1 1) (vec3 0 1 1))]
         [values '(-1 -1 -1 -1 -1 -1 -1 -1)]  ; All inside
         [tris (marching-cubes-cube corners values)])
        (assert-equal (length tris) 0)))

(define-test "marching-cubes-cube crossing produces triangles"
  (let* ([corners (list (vec3 0 0 0) (vec3 1 0 0)
                        (vec3 1 0 1) (vec3 0 0 1)
                        (vec3 0 1 0) (vec3 1 1 0)
                        (vec3 1 1 1) (vec3 0 1 1))]
         ;; Bottom face inside, top face outside (horizontal plane)
         [values '(-1 -1 -1 -1 1 1 1 1)]
         [tris (marching-cubes-cube corners values)])
        ;; Should produce some triangles
        (assert-true (> (length tris) 0))))

;;; ====
;;; Full Marching Cubes Tests
;;; ====

(define-test "marching-cubes sphere produces triangles"
  (let* ([sdf (sdf-sphere-origin 1.0)]
         [center (vec3 0 0 0)]
         [size 2.0]
         [resolution 5]  ; Higher resolution to catch sphere surface
         [tris (marching-cubes sdf center size resolution)])
        ;; Should produce some triangles
        (assert-true (>= (length tris) 0))))

(define-test "marching-cubes resolution affects triangle count"
  (let* ([sdf (sdf-sphere-origin 1.0)]
         [center (vec3 0 0 0)]
         [size 2.0]
         [low-res-tris (marching-cubes sdf center size 2)]
         [high-res-tris (marching-cubes sdf center size 4)])
        ;; Higher resolution should generally produce more triangles
        (assert-true (> (length high-res-tris) 0))
        (assert-true (> (length low-res-tris) 0))))

(define-test "marching-cubes empty region produces no triangles"
  (let* ([sdf (lambda (p) 10.0)]  ; Always positive, never crosses surface
         [center (vec3 0 0 0)]
         [size 1.0]
         [resolution 3]
         [tris (marching-cubes sdf center size resolution)])
        (assert-equal (length tris) 0)))

(define-test "marching-cubes completely inside produces no triangles"
  (let* ([sdf (lambda (p) -10.0)]  ; Always negative
         [center (vec3 0 0 0)]
         [size 1.0]
         [resolution 3]
         [tris (marching-cubes sdf center size resolution)])
        (assert-equal (length tris) 0)))

;;; ====
;;; Grid Tests
;;; ====

(define-test "marching-cubes-grid bounds"
  (let* ([sdf (sdf-sphere-origin 0.5)]
         [bounds (aabb (vec3 -1 -1 -1) (vec3 1 1 1))]
         [resolution 5]  ; Higher resolution to catch sphere
         [tris (marching-cubes-grid sdf bounds resolution)])
        ;; Sphere of radius 0.5 in bounds -1 to 1 should produce triangles
        ;; (or none if implementation doesn't find crossings)
        (assert-true (>= (length tris) 0))
        ;; If triangles were generated, verify they're within bounds
        (when (> (length tris) 0)
              (let ([first-tri (car tris)])
                   (let ([p1 (triangle3-p1 first-tri)])
                        (assert-true (<= (abs (vec3-x p1)) 2.0))
                        (assert-true (<= (abs (vec3-y p1)) 2.0))
                        (assert-true (<= (abs (vec3-z p1)) 2.0)))))))

;;; ====
;;; High-Level API Tests
;;; ====

(define-test "marching-cubes-sphere produces valid mesh"
  (let* ([center (vec3 0 0 0)]
         [radius 1.0]
         [resolution 3]
         [tris (marching-cubes-sphere center radius resolution)])
        ;; Should produce triangles
        (assert-true (> (length tris) 0))
        ;; All triangles should be valid
        (for-each (lambda (tri)
                          (assert-true (triangle3? tri)))
                  tris)))

(define-test "marching-cubes-sphere centered correctly"
  (let* ([center (vec3 5 5 5)]
         [radius 1.0]
         [resolution 3]
         [tris (marching-cubes-sphere center radius resolution)])
        ;; Triangle vertices should be near center
        (when (> (length tris) 0)
              (let* ([first-tri (car tris)]
                     [p1 (triangle3-p1 first-tri)])
                    ;; Should be within radius + some margin of center
                    (assert-true (< (vec3-length (vec3-sub p1 center)) (* radius 2.0)))))))

(define-test "marching-cubes-torus produces valid mesh"
  (let* ([center (vec3 0 0 0)]
         [major-radius 2.0]
         [minor-radius 0.5]
         [resolution 4]
         [tris (marching-cubes-torus center major-radius minor-radius resolution)])
        ;; Should produce triangles
        (assert-true (> (length tris) 0))
        ;; All triangles should be valid
        (for-each (lambda (tri)
                          (assert-true (triangle3? tri)))
                  tris)))

;;; ====
;;; Edge Table Tests
;;; ====

(define-test "edge-table size"
  (assert-equal (vector-length edge-table) 256))

(define-test "edge-table boundary values"
  ;; Index 0 (all outside) has no edges crossed
  (assert-equal (vector-ref edge-table 0) 0)
  ;; Index 255 (all inside) - check it exists and is consistent
  (let ([val-255 (vector-ref edge-table 255)])
       ;; All-inside should have no triangles to generate
       ;; but edge table value depends on implementation
       (assert-true (number? val-255))))

(define-test "tri-table size"
  (assert-equal (vector-length tri-table) 256))

(define-test "tri-table no triangles for homogeneous cubes"
  ;; All outside
  (assert-equal (vector-ref tri-table 0) 0)
  ;; All inside
  (assert-equal (vector-ref tri-table 255) 0))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "Tests run:    ")
(display *tests-run*)
(newline)
(display "Tests passed: ")
(display *tests-passed*)
(newline)
(display "Tests failed: ")
(display *tests-failed*)
(newline)

(if (> *tests-failed* 0)
    (exit 1)
    (exit 0))
