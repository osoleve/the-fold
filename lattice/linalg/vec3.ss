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
;;; @defines vec3 vec3? vec3-x vec3-y vec3-z vec3-ref vec3->list list->vec3 vec3-zero vec3-one vec3-unit-x vec3-unit-y vec3-unit-z
(generate-vec3-core
 vec3 vec3? vec3-x vec3-y vec3-z vec3-ref vec3->list list->vec3
 vec3-zero vec3-one vec3-unit-x vec3-unit-y vec3-unit-z)

(doc vec3 'type '(-> Number Number Number Vec3))
(doc vec3 'description "Construct a 3D vector from x, y, z components")
(doc vec3? 'type '(-> Any Boolean))
(doc vec3? 'description "Test if value is a Vec3")
(doc vec3-x 'type '(-> Vec3 Number))
(doc vec3-x 'description "Get x component")
(doc vec3-y 'type '(-> Vec3 Number))
(doc vec3-y 'description "Get y component")
(doc vec3-z 'type '(-> Vec3 Number))
(doc vec3-z 'description "Get z component")
(doc vec3-ref 'type '(-> Vec3 Int Number))
(doc vec3-ref 'description "Get component by index (0=x, 1=y, 2=z)")
(doc vec3->list 'type '(-> Vec3 (List Number)))
(doc vec3->list 'description "Convert to list of components")
(doc list->vec3 'type '(-> (List Number) Vec3))
(doc list->vec3 'description "Construct Vec3 from a three-element list")
(doc vec3-zero 'type '(-> Vec3))
(doc vec3-zero 'description "Zero vector (0, 0, 0)")
(doc vec3-one 'type '(-> Vec3))
(doc vec3-one 'description "Ones vector (1, 1, 1)")
(doc vec3-unit-x 'type '(-> Vec3))
(doc vec3-unit-x 'description "Unit vector along x axis")
(doc vec3-unit-y 'type '(-> Vec3))
(doc vec3-unit-y 'description "Unit vector along y axis")
(doc vec3-unit-z 'type '(-> Vec3))
(doc vec3-unit-z 'description "Unit vector along z axis")

(doc 'section 'arithmetic)
;;; @defines vec3-add vec3-sub vec3-neg vec3-mul vec3-div vec3-scale vec3-scale-inv
(generate-vec3-arithmetic
 vec3 vec3-x vec3-y vec3-z
 vec3-add vec3-sub vec3-neg vec3-mul vec3-div vec3-scale vec3-scale-inv)

(doc vec3-add 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-add 'description "Component-wise vector addition")
(doc vec3-sub 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-sub 'description "Component-wise vector subtraction")
(doc vec3-neg 'type '(-> Vec3 Vec3))
(doc vec3-neg 'description "Negate all components")
(doc vec3-mul 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-mul 'description "Component-wise (Hadamard) multiplication")
(doc vec3-div 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-div 'description "Component-wise division")
(doc vec3-scale 'type '(-> Vec3 Number Vec3))
(doc vec3-scale 'description "Multiply vector by scalar")
(doc vec3-scale-inv 'type '(-> Vec3 Number Vec3))
(doc vec3-scale-inv 'description "Divide vector by scalar")

(doc 'section 'products-and-norms)
;;; @defines vec3-dot vec3-cross vec3-triple-scalar vec3-triple-vector vec3-magnitude-sq vec3-magnitude vec3-length vec3-distance-sq vec3-distance vec3-normalize vec3-normalize-or vec3-set-magnitude
(generate-vec3-products
 vec3 vec3-x vec3-y vec3-z vec3-zero vec3-sub vec3-scale vec3-scale-inv
 vec3-dot vec3-cross vec3-triple-scalar vec3-triple-vector
 vec3-magnitude-sq vec3-magnitude vec3-length vec3-distance-sq vec3-distance
 vec3-normalize vec3-normalize-or vec3-set-magnitude)

(doc vec3-dot 'type '(-> Vec3 Vec3 Number))
(doc vec3-dot 'description "Dot product of two vectors")
(doc vec3-cross 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-cross 'description "Cross product (right-hand rule)")
(doc vec3-triple-scalar 'type '(-> Vec3 Vec3 Vec3 Number))
(doc vec3-triple-scalar 'description "Scalar triple product a . (b x c); signed volume of parallelepiped")
(doc vec3-triple-vector 'type '(-> Vec3 Vec3 Vec3 Vec3))
(doc vec3-triple-vector 'description "Vector triple product a x (b x c) via BAC-CAB rule")
(doc vec3-magnitude-sq 'type '(-> Vec3 Number))
(doc vec3-magnitude-sq 'description "Squared magnitude (avoids sqrt)")
(doc vec3-magnitude 'type '(-> Vec3 Number))
(doc vec3-magnitude 'description "Euclidean magnitude (length)")
(doc vec3-length 'type '(-> Vec3 Number))
(doc vec3-length 'description "Alias for vec3-magnitude")
(doc vec3-distance-sq 'type '(-> Vec3 Vec3 Number))
(doc vec3-distance-sq 'description "Squared distance between two points")
(doc vec3-distance 'type '(-> Vec3 Vec3 Number))
(doc vec3-distance 'description "Euclidean distance between two points")
(doc vec3-normalize 'type '(-> Vec3 Vec3))
(doc vec3-normalize 'description "Unit vector in same direction; returns zero for near-zero input")
(doc vec3-normalize-or 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-normalize-or 'description "Normalize with fallback default for near-zero input")
(doc vec3-set-magnitude 'type '(-> Vec3 Number Vec3))
(doc vec3-set-magnitude 'description "Return vector with given magnitude, preserving direction")

(doc 'section 'interpolation)
;;; @defines vec3-lerp vec3-slerp vec3-nlerp
(generate-vec3-interpolation
 vec3 vec3-x vec3-y vec3-z
 vec3-add vec3-scale vec3-normalize vec3-dot
 vec3-lerp vec3-slerp vec3-nlerp)

(doc vec3-lerp 'type '(-> Vec3 Vec3 Number Vec3))
(doc vec3-lerp 'description "Linear interpolation between two vectors; t=0 gives a, t=1 gives b")
(doc vec3-slerp 'type '(-> Vec3 Vec3 Number Vec3))
(doc vec3-slerp 'description "Spherical linear interpolation; preserves angular velocity")
(doc vec3-nlerp 'type '(-> Vec3 Vec3 Number Vec3))
(doc vec3-nlerp 'description "Normalized linear interpolation; fast approximation to slerp")

(doc 'section 'projection)
;;; @defines vec3-project vec3-reject vec3-reflect vec3-refract
(generate-vec3-projection
 vec3 vec3-x vec3-y vec3-z vec3-zero
 vec3-add vec3-sub vec3-scale vec3-dot vec3-magnitude-sq
 vec3-project vec3-reject vec3-reflect vec3-refract)

(doc vec3-project 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-project 'description "Project vector a onto vector b")
(doc vec3-reject 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-reject 'description "Component of a perpendicular to b (rejection)")
(doc vec3-reflect 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-reflect 'description "Reflect vector across a normal")
(doc vec3-refract 'type '(-> Vec3 Vec3 Number (U Vec3 Boolean)))
(doc vec3-refract 'description "Snell's law refraction; returns #f on total internal reflection")

(doc 'section 'comparison)
;;; @defines vec3-equal? vec3-nearly-equal? vec3-zero? vec3-unit?
(generate-vec3-comparison
 vec3-x vec3-y vec3-z vec3-magnitude-sq
 vec3-equal? vec3-nearly-equal? vec3-zero? vec3-unit?)

(doc vec3-equal? 'type '(-> Vec3 Vec3 Boolean))
(doc vec3-equal? 'description "Exact component-wise equality")
(doc vec3-nearly-equal? 'type '(-> Vec3 Vec3 Number Boolean))
(doc vec3-nearly-equal? 'description "Approximate equality within epsilon per component")
(doc vec3-zero? 'type '(-> Vec3 Boolean))
(doc vec3-zero? 'description "Test if vector is near-zero (components < 1e-10)")
(doc vec3-unit? 'type '(-> Vec3 Boolean))
(doc vec3-unit? 'description "Test if vector has unit magnitude (within 1e-10)")

(doc 'section 'utilities)
;;; @defines vec3-min vec3-max vec3-clamp vec3-abs vec3-floor vec3-ceil vec3-clamp-magnitude
(generate-vec3-utilities
 vec3 vec3-x vec3-y vec3-z vec3-magnitude vec3-set-magnitude
 vec3-min vec3-max vec3-clamp vec3-abs vec3-floor vec3-ceil vec3-clamp-magnitude)

(doc vec3-min 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-min 'description "Component-wise minimum")
(doc vec3-max 'type '(-> Vec3 Vec3 Vec3))
(doc vec3-max 'description "Component-wise maximum")
(doc vec3-clamp 'type '(-> Vec3 Vec3 Vec3 Vec3))
(doc vec3-clamp 'description "Clamp each component between min and max vectors")
(doc vec3-abs 'type '(-> Vec3 Vec3))
(doc vec3-abs 'description "Absolute value of each component")
(doc vec3-floor 'type '(-> Vec3 Vec3))
(doc vec3-floor 'description "Floor each component")
(doc vec3-ceil 'type '(-> Vec3 Vec3))
(doc vec3-ceil 'description "Ceiling each component")
(doc vec3-clamp-magnitude 'type '(-> Vec3 Number Number Vec3))
(doc vec3-clamp-magnitude 'description "Clamp vector magnitude between min and max")

(doc 'section 'angles)
;;; @defines vec3-angle vec3-signed-angle
(generate-vec3-angles
 vec3-dot vec3-cross vec3-magnitude
 vec3-angle vec3-signed-angle)

(doc vec3-angle 'type '(-> Vec3 Vec3 Number))
(doc vec3-angle 'description "Unsigned angle between two vectors in radians")
(doc vec3-signed-angle 'type '(-> Vec3 Vec3 Vec3 Number))
(doc vec3-signed-angle 'description "Signed angle from a to b around axis, in radians")

(doc 'section 'rotation)
;;; @defines vec3-rotate-x vec3-rotate-y vec3-rotate-z vec3-rotate-axis
(generate-vec3-rotation
 vec3 vec3-x vec3-y vec3-z vec3-add vec3-scale vec3-cross vec3-dot
 vec3-rotate-x vec3-rotate-y vec3-rotate-z vec3-rotate-axis)

(doc vec3-rotate-x 'type '(-> Vec3 Number Vec3))
(doc vec3-rotate-x 'description "Rotate around x axis by angle in radians")
(doc vec3-rotate-y 'type '(-> Vec3 Number Vec3))
(doc vec3-rotate-y 'description "Rotate around y axis by angle in radians")
(doc vec3-rotate-z 'type '(-> Vec3 Number Vec3))
(doc vec3-rotate-z 'description "Rotate around z axis by angle in radians")
(doc vec3-rotate-axis 'type '(-> Vec3 Vec3 Number Vec3))
(doc vec3-rotate-axis 'description "Rotate around arbitrary axis via Rodrigues' formula")

(doc 'section 'basis)
;;; @defines vec3-orthonormal-basis vec3-gram-schmidt
(generate-vec3-basis
 vec3 vec3-x vec3-y vec3-z vec3-normalize vec3-cross vec3-reject
 vec3-orthonormal-basis vec3-gram-schmidt)

(doc vec3-orthonormal-basis 'type '(-> Vec3 (List Vec3)))
(doc vec3-orthonormal-basis 'description "Build orthonormal basis (tangent1, tangent2, normal) from a direction")
(doc vec3-gram-schmidt 'type '(-> Vec3 Vec3 (List Vec3)))
(doc vec3-gram-schmidt 'description "Gram-Schmidt orthonormalization of two vectors")

(doc 'section 'coordinate-conversions)
;;; @defines vec3-from-spherical vec3-from-cylindrical vec3->spherical vec3->cylindrical
(generate-vec3-coords
 vec3 vec3-x vec3-y vec3-z vec3-magnitude
 vec3-from-spherical vec3-from-cylindrical vec3->spherical vec3->cylindrical)

(doc vec3-from-spherical 'type '(-> Number Number Number Vec3))
(doc vec3-from-spherical 'description "Construct from spherical coordinates (r, theta, phi)")
(doc vec3-from-cylindrical 'type '(-> Number Number Number Vec3))
(doc vec3-from-cylindrical 'description "Construct from cylindrical coordinates (r, theta, z)")
(doc vec3->spherical 'type '(-> Vec3 (List Number)))
(doc vec3->spherical 'description "Convert to spherical coordinates (r, theta, phi)")
(doc vec3->cylindrical 'type '(-> Vec3 (List Number)))
(doc vec3->cylindrical 'description "Convert to cylindrical coordinates (r, theta, z)")
