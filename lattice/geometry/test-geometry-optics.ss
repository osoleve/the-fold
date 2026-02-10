;;; lattice/geometry/test-geometry-optics.ss — Tests for Geometry Optics
;;; @module test-geometry-optics

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/geometry/geometry-optics.ss")

(display "Testing geometry-optics.ss...\n")

;;; ============================================================
;;; Vec3 Lens Tests
;;; ============================================================

(test-group "vec3-lenses"
  (define-test "vec3-x-lens view"
    (let ([v (vec3 1 2 3)])
      (assert-equal 1 (^. v vec3-x-lens))))

  (define-test "vec3-y-lens view"
    (let ([v (vec3 1 2 3)])
      (assert-equal 2 (^. v vec3-y-lens))))

  (define-test "vec3-z-lens view"
    (let ([v (vec3 1 2 3)])
      (assert-equal 3 (^. v vec3-z-lens))))

  (define-test "vec3-x-lens set"
    (let* ([v (vec3 1 2 3)]
           [v2 (& v (.~ vec3-x-lens 10))])
      (assert-equal 10 (vec3-x v2))
      (assert-equal 2 (vec3-y v2))
      (assert-equal 3 (vec3-z v2))))

  (define-test "vec3-y-lens modify"
    (let* ([v (vec3 1 2 3)]
           [v2 (& v (%~ vec3-y-lens (lambda (y) (* y 2))))])
      (assert-equal 1 (vec3-x v2))
      (assert-equal 4 (vec3-y v2))
      (assert-equal 3 (vec3-z v2)))))

;;; ============================================================
;;; Ray3 Lens Tests
;;; ============================================================

(test-group "ray3-lenses"
  (define-test "ray3-origin-lens view"
    (let ([r (ray3 (vec3 1 2 3) (vec3 0 0 1))])
      (assert-true (vec3-equal? (vec3 1 2 3) (^. r ray3-origin-lens)))))

  (define-test "ray3-direction-lens view"
    (let ([r (ray3 (vec3 1 2 3) (vec3 0 0 1))])
      (assert-true (vec3-equal? (vec3 0 0 1) (^. r ray3-direction-lens)))))

  (define-test "ray3-origin-lens set"
    (let* ([r (ray3 (vec3 1 2 3) (vec3 0 0 1))]
           [r2 (& r (.~ ray3-origin-lens (vec3 5 5 5)))])
      (assert-true (vec3-equal? (vec3 5 5 5) (ray3-origin r2)))
      (assert-true (vec3-equal? (vec3 0 0 1) (ray3-direction r2)))))

  (define-test "composed ray3-origin-x-lens"
    (let* ([r (ray3 (vec3 1 2 3) (vec3 0 0 1))]
           [r2 (& r (%~ ray3-origin-x-lens add1))])
      (assert-equal 2 (vec3-x (ray3-origin r2)))))

  (define-test "manual composition with >>>"
    (let* ([r (ray3 (vec3 1 2 3) (vec3 0 0 1))]
           [lens (>>> ray3-origin-lens vec3-z-lens)]
           [r2 (& r (.~ lens 100))])
      (assert-equal 100 (vec3-z (ray3-origin r2))))))

;;; ============================================================
;;; Plane3 Lens Tests
;;; ============================================================

(test-group "plane3-lenses"
  (define-test "plane3-normal-lens view"
    (let ([p (plane3 (vec3 0 1 0) 5)])
      (assert-true (vec3-equal? (vec3 0 1 0) (^. p plane3-normal-lens)))))

  (define-test "plane3-d-lens view and set"
    (let* ([p (plane3 (vec3 0 1 0) 5)]
           [p2 (& p (.~ plane3-d-lens -3))])
      (assert-equal 5 (^. p plane3-d-lens))
      (assert-equal -3 (^. p2 plane3-d-lens)))))

;;; ============================================================
;;; Triangle3 Lens and Traversal Tests
;;; ============================================================

(test-group "triangle3-optics"
  (define-test "triangle3-p1-lens view"
    (let ([t (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))])
      (assert-true (vec3-equal? (vec3 0 0 0) (^. t triangle3-p1-lens)))))

  (define-test "triangle3-p2-lens set"
    (let* ([t (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
           [t2 (& t (.~ triangle3-p2-lens (vec3 2 0 0)))])
      (assert-true (vec3-equal? (vec3 2 0 0) (triangle3-p2 t2)))))

  (define-test "triangle3-vertices-each to-list"
    (let ([t (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))])
      (assert-equal 3 (length (^.. t triangle3-vertices-each)))))

  (define-test "triangle3-vertices-each modify all"
    (let* ([t (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
           [t2 (traversal-over triangle3-vertices-each
                              (lambda (v) (vec3-add v (vec3 10 10 10)))
                              t)])
      (assert-true (vec3-equal? (vec3 10 10 10) (triangle3-p1 t2)))
      (assert-true (vec3-equal? (vec3 11 10 10) (triangle3-p2 t2)))
      (assert-true (vec3-equal? (vec3 10 11 10) (triangle3-p3 t2))))))

;;; ============================================================
;;; Sphere Lens Tests
;;; ============================================================

(test-group "sphere-lenses"
  (define-test "sphere-center-lens view"
    (let ([s (sphere (vec3 1 2 3) 5)])
      (assert-true (vec3-equal? (vec3 1 2 3) (^. s sphere-center-lens)))))

  (define-test "sphere-radius-lens view"
    (let ([s (sphere (vec3 1 2 3) 5)])
      (assert-equal 5 (^. s sphere-radius-lens))))

  (define-test "sphere-center-x-lens composed"
    (let* ([s (sphere (vec3 1 2 3) 5)]
           [s2 (& s (%~ sphere-center-x-lens (lambda (x) (* x 10))))])
      (assert-equal 10 (vec3-x (sphere-center s2)))))

  (define-test "scale-sphere utility"
    (let* ([s (sphere (vec3 0 0 0) 5)]
           [s2 ((scale-sphere 2) s)])
      (assert-equal 10 (sphere-radius s2)))))

;;; ============================================================
;;; AABB Lens and Traversal Tests
;;; ============================================================

(test-group "aabb-optics"
  (define-test "aabb-min-lens view"
    (let ([b (aabb (vec3 0 0 0) (vec3 1 1 1))])
      (assert-true (vec3-equal? (vec3 0 0 0) (^. b aabb-min-lens)))))

  (define-test "aabb-max-lens set"
    (let* ([b (aabb (vec3 0 0 0) (vec3 1 1 1))]
           [b2 (& b (.~ aabb-max-lens (vec3 2 2 2)))])
      (assert-true (vec3-equal? (vec3 2 2 2) (aabb-max b2)))))

  (define-test "aabb-corners-each to-list"
    (let ([b (aabb (vec3 0 0 0) (vec3 1 1 1))])
      (assert-equal 2 (length (^.. b aabb-corners-each)))))

  (define-test "aabb-corners-each translate both corners"
    (let* ([b (aabb (vec3 0 0 0) (vec3 1 1 1))]
           [offset (vec3 10 10 10)]
           [b2 (traversal-over aabb-corners-each
                              (lambda (c) (vec3-add c offset))
                              b)])
      (assert-true (vec3-equal? (vec3 10 10 10) (aabb-min b2)))
      (assert-true (vec3-equal? (vec3 11 11 11) (aabb-max b2))))))

;;; ============================================================
;;; Type Prism Tests
;;; ============================================================

(test-group "type-prisms"
  (define-test "prism-sphere matches sphere"
    (let ([s (sphere (vec3 0 0 0) 1)])
      (assert-true (just? (preview prism-sphere s)))))

  (define-test "prism-sphere rejects non-sphere"
    (let ([r (ray3 (vec3 0 0 0) (vec3 1 0 0))])
      (assert-true (nothing? (preview prism-sphere r)))))

  (define-test "prism-ray3 matches ray"
    (let ([r (ray3 (vec3 0 0 0) (vec3 1 0 0))])
      (assert-true (just? (preview prism-ray3 r)))))

  (define-test "prism composition: list of mixed shapes -> spheres"
    (let* ([shapes (list (sphere (vec3 0 0 0) 1)
                         (ray3 (vec3 1 1 1) (vec3 0 1 0))
                         (sphere (vec3 2 2 2) 2))]
           [sphere-fold (traversal-compose traversal-each (prism->traversal prism-sphere))]
           [sphere-radii (map sphere-radius (traversal-to-list sphere-fold shapes))])
      (assert-equal '(1 2) sphere-radii))))

;;; ============================================================
;;; Convenience Function Tests
;;; ============================================================

(test-group "convenience-functions"
  (define-test "translate-shape on sphere"
    (let* ([s (sphere (vec3 0 0 0) 1)]
           [s2 ((translate-shape (vec3 5 5 5)) s)])
      (assert-true (vec3-equal? (vec3 5 5 5) (sphere-center s2)))))

  (define-test "translate-shape on triangle"
    (let* ([t (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
           [t2 ((translate-shape (vec3 10 0 0)) t)])
      (assert-true (vec3-equal? (vec3 10 0 0) (triangle3-p1 t2)))
      (assert-true (vec3-equal? (vec3 11 0 0) (triangle3-p2 t2)))))

  (define-test "translate-shape on aabb"
    (let* ([b (aabb (vec3 0 0 0) (vec3 1 1 1))]
           [b2 ((translate-shape (vec3 -5 -5 -5)) b)])
      (assert-true (vec3-equal? (vec3 -5 -5 -5) (aabb-min b2)))
      (assert-true (vec3-equal? (vec3 -4 -4 -4) (aabb-max b2)))))

  (define-test "transform-triangle"
    (let* ([t (triangle3 (vec3 1 0 0) (vec3 0 1 0) (vec3 0 0 1))]
           [t2 (transform-triangle (lambda (v) (vec3-scale v 2)) t)])
      (assert-true (vec3-equal? (vec3 2 0 0) (triangle3-p1 t2)))
      (assert-true (vec3-equal? (vec3 0 2 0) (triangle3-p2 t2)))
      (assert-true (vec3-equal? (vec3 0 0 2) (triangle3-p3 t2))))))

;;; ============================================================
;;; Fold Tests
;;; ============================================================

(test-group "fold-operations"
  (define-test "all-triangle-vertices fold"
    (let* ([tris (list (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))
                       (triangle3 (vec3 0 0 1) (vec3 1 0 1) (vec3 0 1 1)))]
           [verts (fold-to-list all-triangle-vertices tris)])
      (assert-equal 6 (length verts)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
