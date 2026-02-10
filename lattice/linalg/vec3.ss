;;; lattice/linalg/vec3.ss — 3D Vector Operations
;;; @module vec3
;;; @requires prelude vec-common

(require 'prelude)
(require 'vec-common)

(doc 'module 'vec3)
(doc 'description "Pure, functional 3D vector operations for physics and graphics")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "A vec3 is represented as: (vec3 x y z)")
(doc 'note "All operations are pure and return new vectors")
(doc 'note "Uses vec-common.ss macros to generate shared operations")
(doc 'requires '(prelude vec-common))

(doc 'section 'core-type)
(generate-vec3-core
 vec3 vec3? vec3-x vec3-y vec3-z vec3-ref vec3->list list->vec3
 vec3-zero vec3-one vec3-unit-x vec3-unit-y vec3-unit-z)

(doc 'section 'arithmetic)
(generate-vec3-arithmetic
 vec3 vec3-x vec3-y vec3-z
 vec3-add vec3-sub vec3-neg vec3-mul vec3-div vec3-scale vec3-scale-inv)

(doc 'section 'products-and-norms)
(generate-vec3-products
 vec3 vec3-x vec3-y vec3-z vec3-zero vec3-sub vec3-scale vec3-scale-inv
 vec3-dot vec3-cross vec3-triple-scalar vec3-triple-vector
 vec3-magnitude-sq vec3-magnitude vec3-length vec3-distance-sq vec3-distance
 vec3-normalize vec3-normalize-or vec3-set-magnitude)

(doc 'section 'interpolation)
(generate-vec3-interpolation
 vec3 vec3-x vec3-y vec3-z
 vec3-add vec3-scale vec3-normalize vec3-dot
 vec3-lerp vec3-slerp vec3-nlerp)

(doc 'section 'projection)
(generate-vec3-projection
 vec3 vec3-x vec3-y vec3-z vec3-zero
 vec3-add vec3-sub vec3-scale vec3-dot vec3-magnitude-sq
 vec3-project vec3-reject vec3-reflect vec3-refract)

(doc 'section 'comparison)
(generate-vec3-comparison
 vec3-x vec3-y vec3-z vec3-magnitude-sq
 vec3-equal? vec3-nearly-equal? vec3-zero? vec3-unit?)

(doc 'section 'utilities)
(generate-vec3-utilities
 vec3 vec3-x vec3-y vec3-z vec3-magnitude vec3-set-magnitude
 vec3-min vec3-max vec3-clamp vec3-abs vec3-floor vec3-ceil vec3-clamp-magnitude)

(doc 'section 'angles)
(generate-vec3-angles
 vec3-dot vec3-cross vec3-magnitude
 vec3-angle vec3-signed-angle)

(doc 'section 'rotation)
(generate-vec3-rotation
 vec3 vec3-x vec3-y vec3-z vec3-add vec3-scale vec3-cross vec3-dot
 vec3-rotate-x vec3-rotate-y vec3-rotate-z vec3-rotate-axis)

(doc 'section 'basis)
(generate-vec3-basis
 vec3 vec3-x vec3-y vec3-z vec3-normalize vec3-cross vec3-reject
 vec3-orthonormal-basis vec3-gram-schmidt)

(doc 'section 'coordinate-conversions)
(generate-vec3-coords
 vec3 vec3-x vec3-y vec3-z vec3-magnitude
 vec3-from-spherical vec3-from-cylindrical vec3->spherical vec3->cylindrical)
