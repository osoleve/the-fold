;;; lattice/geometry/geometry-optics.ss — Optics for Geometric Primitives
;;; @module geometry-optics
;;; @requires prelude optics geometry

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'optics)
(require 'geometry)

(doc 'module 'geometry-optics)
(doc 'bridges '(geometry optics))
(doc 'description "Composable optics for geometric primitives. Provides lenses, traversals, and prisms for rays, planes, triangles, spheres, and bounding boxes.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Usage: (load \"lattice/geometry/geometry-optics.ss\")")
(doc 'note "View: (^. ray ray3-origin-lens) => Vec3")
(doc 'note "Modify: (& ray (%~ (>>> ray3-origin-lens vec3-x-lens) add1))")
(doc 'note "Compose: (>>> sphere-center-lens vec3-z-lens) for deep access")
(doc 'note "Traverse: (^.. tri triangle3-vertices-each) => list of 3 vertices")

;;; ============================================================
;;; Section 1: Vec3 Lenses (Foundational)
;;; ============================================================

(doc 'section 'vec3-lenses)
(doc 'note "Lenses for accessing Vec3 components")

(doc vec3-x-lens 'type '(Lens Vec3 Number))
(doc vec3-x-lens 'description "Focus on the x component of a Vec3")
(doc vec3-x-lens 'export #t)
(define vec3-x-lens
  (make-lens
   vec3-x
   (lambda (new-x v) (vec3 new-x (vec3-y v) (vec3-z v)))))

(doc vec3-y-lens 'type '(Lens Vec3 Number))
(doc vec3-y-lens 'description "Focus on the y component of a Vec3")
(doc vec3-y-lens 'export #t)
(define vec3-y-lens
  (make-lens
   vec3-y
   (lambda (new-y v) (vec3 (vec3-x v) new-y (vec3-z v)))))

(doc vec3-z-lens 'type '(Lens Vec3 Number))
(doc vec3-z-lens 'description "Focus on the z component of a Vec3")
(doc vec3-z-lens 'export #t)
(define vec3-z-lens
  (make-lens
   vec3-z
   (lambda (new-z v) (vec3 (vec3-x v) (vec3-y v) new-z))))

;;; ============================================================
;;; Section 2: Line3 Lenses
;;; ============================================================

(doc 'section 'line3-lenses)

(doc line3-origin-lens 'type '(Lens Line3 Vec3))
(doc line3-origin-lens 'description "Focus on the origin point of a Line3")
(doc line3-origin-lens 'export #t)
(define line3-origin-lens
  (make-lens
   line3-origin
   (lambda (new-origin l) (line3 new-origin (line3-direction l)))))

(doc line3-direction-lens 'type '(Lens Line3 Vec3))
(doc line3-direction-lens 'description "Focus on the direction vector of a Line3")
(doc line3-direction-lens 'export #t)
(define line3-direction-lens
  (make-lens
   line3-direction
   (lambda (new-dir l) (line3 (line3-origin l) new-dir))))

;;; ============================================================
;;; Section 3: Ray3 Lenses
;;; ============================================================

(doc 'section 'ray3-lenses)

(doc ray3-origin-lens 'type '(Lens Ray3 Vec3))
(doc ray3-origin-lens 'description "Focus on the origin point of a Ray3")
(doc ray3-origin-lens 'export #t)
(define ray3-origin-lens
  (make-lens
   ray3-origin
   (lambda (new-origin r) (ray3 new-origin (ray3-direction r)))))

(doc ray3-direction-lens 'type '(Lens Ray3 Vec3))
(doc ray3-direction-lens 'description "Focus on the direction vector of a Ray3")
(doc ray3-direction-lens 'export #t)
(define ray3-direction-lens
  (make-lens
   ray3-direction
   (lambda (new-dir r) (ray3 (ray3-origin r) new-dir))))

;;; Composed lenses for ray components
(doc ray3-origin-x-lens 'type '(Lens Ray3 Number))
(doc ray3-origin-x-lens 'description "Focus on the x component of ray origin")
(doc ray3-origin-x-lens 'export #t)
(define ray3-origin-x-lens (lens-compose ray3-origin-lens vec3-x-lens))

(doc ray3-origin-y-lens 'type '(Lens Ray3 Number))
(doc ray3-origin-y-lens 'export #t)
(define ray3-origin-y-lens (lens-compose ray3-origin-lens vec3-y-lens))

(doc ray3-origin-z-lens 'type '(Lens Ray3 Number))
(doc ray3-origin-z-lens 'export #t)
(define ray3-origin-z-lens (lens-compose ray3-origin-lens vec3-z-lens))

;;; ============================================================
;;; Section 4: Plane3 Lenses
;;; ============================================================

(doc 'section 'plane3-lenses)

(doc plane3-normal-lens 'type '(Lens Plane3 Vec3))
(doc plane3-normal-lens 'description "Focus on the normal vector of a Plane3")
(doc plane3-normal-lens 'export #t)
(define plane3-normal-lens
  (make-lens
   plane3-normal
   (lambda (new-normal p) (plane3 new-normal (plane3-d p)))))

(doc plane3-d-lens 'type '(Lens Plane3 Number))
(doc plane3-d-lens 'description "Focus on the d coefficient of a Plane3")
(doc plane3-d-lens 'export #t)
(define plane3-d-lens
  (make-lens
   plane3-d
   (lambda (new-d p) (plane3 (plane3-normal p) new-d))))

;;; ============================================================
;;; Section 5: Triangle3 Lenses and Traversals
;;; ============================================================

(doc 'section 'triangle3-lenses)

(doc triangle3-p1-lens 'type '(Lens Triangle3 Vec3))
(doc triangle3-p1-lens 'description "Focus on the first vertex of a Triangle3")
(doc triangle3-p1-lens 'export #t)
(define triangle3-p1-lens
  (make-lens
   triangle3-p1
   (lambda (new-p1 t) (triangle3 new-p1 (triangle3-p2 t) (triangle3-p3 t)))))

(doc triangle3-p2-lens 'type '(Lens Triangle3 Vec3))
(doc triangle3-p2-lens 'description "Focus on the second vertex of a Triangle3")
(doc triangle3-p2-lens 'export #t)
(define triangle3-p2-lens
  (make-lens
   triangle3-p2
   (lambda (new-p2 t) (triangle3 (triangle3-p1 t) new-p2 (triangle3-p3 t)))))

(doc triangle3-p3-lens 'type '(Lens Triangle3 Vec3))
(doc triangle3-p3-lens 'description "Focus on the third vertex of a Triangle3")
(doc triangle3-p3-lens 'export #t)
(define triangle3-p3-lens
  (make-lens
   triangle3-p3
   (lambda (new-p3 t) (triangle3 (triangle3-p1 t) (triangle3-p2 t) new-p3))))

(doc triangle3-vertices-each 'type '(Traversal Triangle3 Vec3))
(doc triangle3-vertices-each 'description "Traverse all three vertices of a triangle")
(doc triangle3-vertices-each 'export #t)
(define triangle3-vertices-each
  (make-traversal
   (lambda (f t)
     (triangle3 (f (triangle3-p1 t))
                (f (triangle3-p2 t))
                (f (triangle3-p3 t))))
   (lambda (t)
     (list (triangle3-p1 t) (triangle3-p2 t) (triangle3-p3 t)))))

;;; ============================================================
;;; Section 6: Circle Lenses (2D)
;;; ============================================================

(doc 'section 'circle-lenses)

(doc circle-center-lens 'type '(Lens Circle Vec2))
(doc circle-center-lens 'description "Focus on the center of a Circle")
(doc circle-center-lens 'export #t)
(define circle-center-lens
  (make-lens
   circle-center
   (lambda (new-center c) (circle new-center (circle-radius c)))))

(doc circle-radius-lens 'type '(Lens Circle Number))
(doc circle-radius-lens 'description "Focus on the radius of a Circle")
(doc circle-radius-lens 'export #t)
(define circle-radius-lens
  (make-lens
   circle-radius
   (lambda (new-radius c) (circle (circle-center c) new-radius))))

;;; ============================================================
;;; Section 7: Sphere Lenses
;;; ============================================================

(doc 'section 'sphere-lenses)

(doc sphere-center-lens 'type '(Lens Sphere Vec3))
(doc sphere-center-lens 'description "Focus on the center of a Sphere")
(doc sphere-center-lens 'export #t)
(define sphere-center-lens
  (make-lens
   sphere-center
   (lambda (new-center s) (sphere new-center (sphere-radius s)))))

(doc sphere-radius-lens 'type '(Lens Sphere Number))
(doc sphere-radius-lens 'description "Focus on the radius of a Sphere")
(doc sphere-radius-lens 'export #t)
(define sphere-radius-lens
  (make-lens
   sphere-radius
   (lambda (new-radius s) (sphere (sphere-center s) new-radius))))

;;; Composed lenses for sphere center components
(doc sphere-center-x-lens 'type '(Lens Sphere Number))
(doc sphere-center-x-lens 'export #t)
(define sphere-center-x-lens (lens-compose sphere-center-lens vec3-x-lens))

(doc sphere-center-y-lens 'type '(Lens Sphere Number))
(doc sphere-center-y-lens 'export #t)
(define sphere-center-y-lens (lens-compose sphere-center-lens vec3-y-lens))

(doc sphere-center-z-lens 'type '(Lens Sphere Number))
(doc sphere-center-z-lens 'export #t)
(define sphere-center-z-lens (lens-compose sphere-center-lens vec3-z-lens))

;;; ============================================================
;;; Section 8: AABB Lenses
;;; ============================================================

(doc 'section 'aabb-lenses)

(doc aabb-min-lens 'type '(Lens AABB Vec3))
(doc aabb-min-lens 'description "Focus on the min corner of an AABB")
(doc aabb-min-lens 'export #t)
(define aabb-min-lens
  (make-lens
   aabb-min
   (lambda (new-min b) (aabb new-min (aabb-max b)))))

(doc aabb-max-lens 'type '(Lens AABB Vec3))
(doc aabb-max-lens 'description "Focus on the max corner of an AABB")
(doc aabb-max-lens 'export #t)
(define aabb-max-lens
  (make-lens
   aabb-max
   (lambda (new-max b) (aabb (aabb-min b) new-max))))

(doc aabb-corners-each 'type '(Traversal AABB Vec3))
(doc aabb-corners-each 'description "Traverse both corners of an AABB")
(doc aabb-corners-each 'export #t)
(define aabb-corners-each
  (make-traversal
   (lambda (f b) (aabb (f (aabb-min b)) (f (aabb-max b))))
   (lambda (b) (list (aabb-min b) (aabb-max b)))))

;;; ============================================================
;;; Section 9: OBB Lenses
;;; ============================================================

(doc 'section 'obb-lenses)

(doc obb-center-lens 'type '(Lens OBB Vec3))
(doc obb-center-lens 'description "Focus on the center of an OBB")
(doc obb-center-lens 'export #t)
(define obb-center-lens
  (make-lens
   obb-center
   (lambda (new-center b) (obb new-center (obb-axes b) (obb-extents b)))))

(doc obb-axes-lens 'type '(Lens OBB (List Vec3)))
(doc obb-axes-lens 'description "Focus on the axes of an OBB")
(doc obb-axes-lens 'export #t)
(define obb-axes-lens
  (make-lens
   obb-axes
   (lambda (new-axes b) (obb (obb-center b) new-axes (obb-extents b)))))

(doc obb-extents-lens 'type '(Lens OBB Vec3))
(doc obb-extents-lens 'description "Focus on the extents (half-sizes) of an OBB")
(doc obb-extents-lens 'export #t)
(define obb-extents-lens
  (make-lens
   obb-extents
   (lambda (new-extents b) (obb (obb-center b) (obb-axes b) new-extents))))

;;; ============================================================
;;; Section 10: Type Prisms (Safe Type-Checked Access)
;;; ============================================================

(doc 'section 'type-prisms)
(doc 'note "Prisms for safely matching specific geometric types")

(doc prism-line3 'type '(Prism Any Line3))
(doc prism-line3 'description "Match Line3 values")
(doc prism-line3 'export #t)
(define prism-line3
  (make-prism
   (lambda (x) (if (line3? x) (just x) nothing))
   identity))

(doc prism-ray3 'type '(Prism Any Ray3))
(doc prism-ray3 'description "Match Ray3 values")
(doc prism-ray3 'export #t)
(define prism-ray3
  (make-prism
   (lambda (x) (if (ray3? x) (just x) nothing))
   identity))

(doc prism-plane3 'type '(Prism Any Plane3))
(doc prism-plane3 'description "Match Plane3 values")
(doc prism-plane3 'export #t)
(define prism-plane3
  (make-prism
   (lambda (x) (if (plane3? x) (just x) nothing))
   identity))

(doc prism-triangle3 'type '(Prism Any Triangle3))
(doc prism-triangle3 'description "Match Triangle3 values")
(doc prism-triangle3 'export #t)
(define prism-triangle3
  (make-prism
   (lambda (x) (if (triangle3? x) (just x) nothing))
   identity))

(doc prism-sphere 'type '(Prism Any Sphere))
(doc prism-sphere 'description "Match Sphere values")
(doc prism-sphere 'export #t)
(define prism-sphere
  (make-prism
   (lambda (x) (if (sphere? x) (just x) nothing))
   identity))

(doc prism-aabb 'type '(Prism Any AABB))
(doc prism-aabb 'description "Match AABB values")
(doc prism-aabb 'export #t)
(define prism-aabb
  (make-prism
   (lambda (x) (if (aabb? x) (just x) nothing))
   identity))

(doc prism-circle 'type '(Prism Any Circle))
(doc prism-circle 'description "Match Circle values")
(doc prism-circle 'export #t)
(define prism-circle
  (make-prism
   (lambda (x) (if (circle? x) (just x) nothing))
   identity))

;;; ============================================================
;;; Section 11: Utility Optics
;;; ============================================================

(doc 'section 'utility-optics)

(doc shapes-each 'type '(-> (List Shape) (Traversal (List Shape) Shape)))
(doc shapes-each 'description "Traverse each shape in a list")
(doc shapes-each 'export #t)
(define shapes-each traversal-each)

(doc triangles-each 'type '(Traversal (List Triangle3) Triangle3))
(doc triangles-each 'description "Traverse each triangle in a mesh")
(doc triangles-each 'export #t)
(define triangles-each traversal-each)

(doc spheres-each 'type '(Traversal (List Sphere) Sphere))
(doc spheres-each 'description "Traverse each sphere in a collection")
(doc spheres-each 'export #t)
(define spheres-each traversal-each)

;;; Fold for all vertices in a triangle list
(doc all-triangle-vertices 'type '(Fold (List Triangle3) Vec3))
(doc all-triangle-vertices 'description "Fold over all vertices in a triangle mesh")
(doc all-triangle-vertices 'todo "Use flat-fold instead of apply-append for large meshes (O(n²) → O(n))")
(doc all-triangle-vertices 'export #t)
(define all-triangle-vertices
  (make-fold
   (lambda (tris)
     (append-map (lambda (t)
                   (list (triangle3-p1 t) (triangle3-p2 t) (triangle3-p3 t)))
                 tris))))

;;; ============================================================
;;; Section 12: Convenience Combinators
;;; ============================================================

(doc 'section 'convenience)

(doc translate-shape 'type '(-> Vec3 (-> Shape Shape)))
(doc translate-shape 'description "Translate a shape by offset (works with any shape that has a center/origin lens)")
(doc translate-shape 'export #t)
(define (translate-shape offset)
  (lambda (shape)
    (cond
      [(sphere? shape)
       (& shape (%~ sphere-center-lens (lambda (c) (vec3-add c offset))))]
      [(aabb? shape)
       (& shape (%~ aabb-corners-each (lambda (c) (vec3-add c offset))))]
      [(ray3? shape)
       (& shape (%~ ray3-origin-lens (lambda (o) (vec3-add o offset))))]
      [(line3? shape)
       (& shape (%~ line3-origin-lens (lambda (o) (vec3-add o offset))))]
      [(triangle3? shape)
       (& shape (%~ triangle3-vertices-each (lambda (v) (vec3-add v offset))))]
      [else shape])))

(doc scale-sphere 'type '(-> Number (-> Sphere Sphere)))
(doc scale-sphere 'description "Scale sphere radius by factor")
(doc scale-sphere 'export #t)
(define (scale-sphere factor)
  (lambda (s)
    (& s (%~ sphere-radius-lens (lambda (r) (* r factor))))))

(doc transform-triangle 'type '(-> (-> Vec3 Vec3) Triangle3 Triangle3))
(doc transform-triangle 'description "Apply transformation to all triangle vertices")
(doc transform-triangle 'export #t)
(define (transform-triangle f tri)
  (traversal-over triangle3-vertices-each f tri))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

(doc 'section 'exports)
(doc 'note "Vec3 lenses: vec3-x-lens, vec3-y-lens, vec3-z-lens")
(doc 'note "Line3 lenses: line3-origin-lens, line3-direction-lens")
(doc 'note "Ray3 lenses: ray3-origin-lens, ray3-direction-lens, ray3-origin-{x,y,z}-lens")
(doc 'note "Plane3 lenses: plane3-normal-lens, plane3-d-lens")
(doc 'note "Triangle3: triangle3-p{1,2,3}-lens, triangle3-vertices-each (traversal)")
(doc 'note "Circle lenses: circle-center-lens, circle-radius-lens")
(doc 'note "Sphere lenses: sphere-center-lens, sphere-radius-lens, sphere-center-{x,y,z}-lens")
(doc 'note "AABB lenses: aabb-min-lens, aabb-max-lens, aabb-corners-each (traversal)")
(doc 'note "OBB lenses: obb-center-lens, obb-axes-lens, obb-extents-lens")
(doc 'note "Type prisms: prism-{line3,ray3,plane3,triangle3,sphere,aabb,circle}")
(doc 'note "Traversals: shapes-each, triangles-each, spheres-each, all-triangle-vertices (fold)")
(doc 'note "Convenience: translate-shape, scale-sphere, transform-triangle")
(doc 'note "Re-exports optics operators: ^., ^?, ^.., .~, %~, &, >>>")

