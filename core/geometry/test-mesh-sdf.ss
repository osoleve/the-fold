;;; core/geometry/test-mesh-sdf.ss — Tests for Mesh SDF implementation
;;;
;;; Run: scheme --script core/geometry/test-mesh-sdf.ss

(load "core/test-framework.ss")
(load "core/geometry/mesh-sdf.ss")

(set! *current-group* 'mesh-sdf)

;;; Helper for approximate equality
(define (approx= a b eps)
  (< (abs (- a b)) eps))

;;; ============================================================
;;; Mesh Creation Tests
;;; ============================================================

(define-test "Mesh creation from triangles"
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [mesh (make-mesh (list tri))])
        (assert-true (mesh? mesh))
        (assert-equal (mesh-triangle-count mesh) 1)
        (assert-true (> (mesh-bvh-node-count mesh) 0))))

(define-test "Cube mesh creation"
  (let ([mesh (make-mesh-cube 1.0)])
       (assert-true (mesh? mesh))
       (assert-equal (mesh-triangle-count mesh) 12)
       (assert-true (> (mesh-bvh-depth mesh) 0))))

(define-test "Icosphere mesh creation - subdivision 0"
  (let ([mesh (make-mesh-sphere-ico 1.0 0)])
       (assert-true (mesh? mesh))
       (assert-equal (mesh-triangle-count mesh) 20)
       (assert-true (> (mesh-bvh-depth mesh) 0))))

(define-test "Icosphere mesh creation - subdivision 1"
  (let ([mesh (make-mesh-sphere-ico 1.0 1)])
       (assert-true (mesh? mesh))
       (assert-equal (mesh-triangle-count mesh) 80)
       (assert-true (> (mesh-bvh-depth mesh) 0))))

(define-test "Icosphere mesh creation - subdivision 2"
  (let ([mesh (make-mesh-sphere-ico 1.0 2)])
       (assert-true (mesh? mesh))
       (assert-equal (mesh-triangle-count mesh) 320)
       (assert-true (> (mesh-bvh-depth mesh) 0))))

(define-test "Icosphere mesh creation - subdivision 3"
  (let ([mesh (make-mesh-sphere-ico 1.0 3)])
       (assert-true (mesh? mesh))
       (assert-equal (mesh-triangle-count mesh) 1280)
       (assert-true (> (mesh-bvh-depth mesh) 0))))

(define-test "Icosphere vertices lie on sphere surface"
  ;; All vertices should be at the specified radius
  (let* ([radius 2.5]
         [mesh (make-mesh-sphere-ico radius 2)]
         [triangles (mesh-triangles mesh)]
         [eps 0.01]
         ;; Check first triangle
         [tri (car triangles)]
         [p1 (triangle3-p1 tri)]
         [p2 (triangle3-p2 tri)]
         [p3 (triangle3-p3 tri)]
         [mag1 (vec3-length p1)]
         [mag2 (vec3-length p2)]
         [mag3 (vec3-length p3)])
        ;; All vertices should be at radius distance from origin
        (assert-true (< (abs (- mag1 radius)) eps))
        (assert-true (< (abs (- mag2 radius)) eps))
        (assert-true (< (abs (- mag3 radius)) eps))))

;;; ============================================================
;;; Mesh SDF Tests
;;; ============================================================

(define-test "Mesh SDF - point at origin inside cube"
  (let* ([mesh (make-mesh-cube 1.0)]
         [point (vec3 0 0 0)]
         [dist (mesh-sdf mesh point)])
        (assert-true (< dist 0))
        (assert-true (< (abs dist) 1.5))))

(define-test "Mesh SDF - point outside cube"
  (let* ([mesh (make-mesh-cube 1.0)]
         [point (vec3 3 0 0)]
         [dist (mesh-sdf mesh point)])
        (assert-true (> dist 0))
        (assert-true (approx= dist 2.0 0.5))))

(define-test "Mesh SDF - point on cube surface"
  (let* ([mesh (make-mesh-cube 1.0)]
         [point (vec3 1 0 0)]
         [dist (mesh-sdf mesh point)])
        (assert-true (< (abs dist) 0.1))))

(define-test "Mesh SDF gradient at surface"
  (let* ([mesh (make-mesh-cube 1.0)]
         [point (vec3 1.1 0 0)]
         [normal (mesh-sdf-gradient mesh point)])
        (assert-true (approx= (vec3-x normal) 1.0 0.2))
        (assert-true (< (abs (vec3-y normal)) 0.2))
        (assert-true (< (abs (vec3-z normal)) 0.2))))

(define-test "Mesh SDF consistency"
  (let* ([mesh (make-mesh-cube 1.0)]
         [point-inside (vec3 0 0 0)]
         [point-outside (vec3 3 0 0)]
         [dist-inside (mesh-sdf mesh point-inside)]
         [dist-outside (mesh-sdf mesh point-outside)])
        (assert-true (< dist-inside 0))
        (assert-true (> dist-outside 0))
        (assert-true (> dist-outside (abs dist-inside)))))

;;; ============================================================
;;; Mesh Ray Intersection Tests
;;; ============================================================

(define-test "Mesh ray intersection - hit"
  (let* ([mesh (make-mesh-cube 1.0)]
         [ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [result (mesh-intersect-ray mesh ray)])
        (assert-false (equal? result #f))
        (let* ([hit-point (car result)]
               [t (cadr result)])
              (assert-true (> t 0))
              (assert-true (approx= (vec3-z hit-point) -1.0 0.1)))))

(define-test "Mesh ray intersection - miss"
  (let* ([mesh (make-mesh-cube 1.0)]
         [ray (ray3 (vec3 10 10 -5) (vec3 0 0 1))]
         [result (mesh-intersect-ray mesh ray)])
        (assert-false result)))

;;; ============================================================
;;; Mesh Statistics Tests
;;; ============================================================

(define-test "Mesh bounds"
  (let* ([mesh (make-mesh-cube 2.0)]
         [bbox (mesh-bounds mesh)]
         [bmin (aabb-min bbox)]
         [bmax (aabb-max bbox)])
        (assert-true (<= (vec3-x bmin) -2.0))
        (assert-true (>= (vec3-x bmax) 2.0))
        (assert-true (<= (vec3-y bmin) -2.0))
        (assert-true (>= (vec3-y bmax) 2.0))
        (assert-true (<= (vec3-z bmin) -2.0))
        (assert-true (>= (vec3-z bmax) 2.0))))

;;; ============================================================
;;; Summary
;;; ============================================================

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
