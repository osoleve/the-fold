;;; lattice/linalg/vec2.ss — 2D Vector Operations
;;; @module vec2
;;; @requires vec-common

(require 'vec-common)

(doc 'module 'vec2)
(doc 'description "Pure, functional 2D vector operations for physics and graphics")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "A vec2 is represented as: (vec2 x y)")
(doc 'note "All operations are pure and return new vectors")
(doc 'note "Uses vec-common.ss macros to generate shared operations")
(doc 'requires '(prelude vec-common))


(doc 'section 'core-type)
;;; @defines vec2 vec2? vec2-x vec2-y vec2->list list->vec2 vec2-zero vec2-one vec2-unit-x vec2-unit-y
(generate-vec2-core
 vec2 vec2? vec2-x vec2-y vec2->list list->vec2
 vec2-zero vec2-one vec2-unit-x vec2-unit-y)

(doc vec2 'type '(-> Number Number Vec2))
(doc vec2 'description "Construct a 2D vector from x and y components")
(doc vec2? 'type '(-> Any Boolean))
(doc vec2? 'description "Test if value is a Vec2")
(doc vec2-x 'type '(-> Vec2 Number))
(doc vec2-x 'description "Get x component")
(doc vec2-y 'type '(-> Vec2 Number))
(doc vec2-y 'description "Get y component")
(doc vec2->list 'type '(-> Vec2 (List Number)))
(doc vec2->list 'description "Convert to list of components")
(doc list->vec2 'type '(-> (List Number) Vec2))
(doc list->vec2 'description "Construct Vec2 from a two-element list")
(doc vec2-zero 'type '(-> Vec2))
(doc vec2-zero 'description "Zero vector (0, 0)")
(doc vec2-one 'type '(-> Vec2))
(doc vec2-one 'description "Ones vector (1, 1)")
(doc vec2-unit-x 'type '(-> Vec2))
(doc vec2-unit-x 'description "Unit vector along x axis")
(doc vec2-unit-y 'type '(-> Vec2))
(doc vec2-unit-y 'description "Unit vector along y axis")

(doc 'section 'arithmetic)
;;; @defines vec2-add vec2-sub vec2-neg vec2-mul vec2-div vec2-scale vec2-scale-inv
(generate-vec2-arithmetic
 vec2 vec2-x vec2-y
 vec2-add vec2-sub vec2-neg vec2-mul vec2-div vec2-scale vec2-scale-inv)

(doc vec2-add 'type '(-> Vec2 Vec2 Vec2))
(doc vec2-add 'description "Component-wise vector addition")
(doc vec2-sub 'type '(-> Vec2 Vec2 Vec2))
(doc vec2-sub 'description "Component-wise vector subtraction")
(doc vec2-neg 'type '(-> Vec2 Vec2))
(doc vec2-neg 'description "Negate both components")
(doc vec2-mul 'type '(-> Vec2 Vec2 Vec2))
(doc vec2-mul 'description "Component-wise (Hadamard) multiplication")
(doc vec2-div 'type '(-> Vec2 Vec2 Vec2))
(doc vec2-div 'description "Component-wise division")
(doc vec2-scale 'type '(-> Vec2 Number Vec2))
(doc vec2-scale 'description "Multiply vector by scalar")
(doc vec2-scale-inv 'type '(-> Vec2 Number Vec2))
(doc vec2-scale-inv 'description "Divide vector by scalar")

(doc 'section 'products-and-norms)
;;; @defines vec2-dot vec2-magnitude-sq vec2-magnitude vec2-length vec2-distance-sq vec2-distance vec2-normalize vec2-unit vec2-set-magnitude
(generate-vec2-products
 vec2 vec2-x vec2-y vec2-zero vec2-sub vec2-scale-inv
 vec2-dot vec2-magnitude-sq vec2-magnitude vec2-length
 vec2-distance-sq vec2-distance vec2-normalize vec2-unit vec2-set-magnitude)

(doc vec2-dot 'type '(-> Vec2 Vec2 Number))
(doc vec2-dot 'description "Dot product of two vectors")
(doc vec2-magnitude-sq 'type '(-> Vec2 Number))
(doc vec2-magnitude-sq 'description "Squared magnitude (avoids sqrt)")
(doc vec2-magnitude 'type '(-> Vec2 Number))
(doc vec2-magnitude 'description "Euclidean magnitude (length)")
(doc vec2-length 'type '(-> Vec2 Number))
(doc vec2-length 'description "Alias for vec2-magnitude")
(doc vec2-distance-sq 'type '(-> Vec2 Vec2 Number))
(doc vec2-distance-sq 'description "Squared distance between two points")
(doc vec2-distance 'type '(-> Vec2 Vec2 Number))
(doc vec2-distance 'description "Euclidean distance between two points")
(doc vec2-normalize 'type '(-> Vec2 Vec2))
(doc vec2-normalize 'description "Unit vector in same direction; returns zero for near-zero input")
(doc vec2-unit 'type '(-> Vec2 Vec2))
(doc vec2-unit 'description "Alias for vec2-normalize")
(doc vec2-set-magnitude 'type '(-> Vec2 Number Vec2))
(doc vec2-set-magnitude 'description "Return vector with given magnitude, preserving direction")

(doc 'section '2d-cross-product)

(define (vec2-cross a b)
  (doc 'type '(-> Vec2 Vec2 Number))
  (doc 'description "2D cross product (z-component of 3D cross product)")
  (doc 'note "Returns the signed area of the parallelogram formed by the vectors")
  (- (* (vec2-x a) (vec2-y b))
     (* (vec2-y a) (vec2-x b))))

(define vec2-perp-dot vec2-cross)
(doc vec2-perp-dot 'type '(-> Vec2 Vec2 Number))
(doc vec2-perp-dot 'description "Perpendicular dot product (same as cross)")

(doc 'section '2d-angles)

(define (vec2-angle v)
  (doc 'type '(-> Vec2 Number))
  (doc 'description "Angle of vector (radians, from positive x-axis)")
  (atan (vec2-y v) (vec2-x v)))

(define (vec2-angle-between a b)
  (doc 'type '(-> Vec2 Vec2 Number))
  (doc 'description "Angle between two vectors (radians, unsigned)")
  (let ([cos-theta (/ (vec2-dot a b)
                      (* (vec2-magnitude a) (vec2-magnitude b)))])
       (acos (max -1 (min 1 cos-theta)))))

(define (vec2-angle-to a b)
  (doc 'type '(-> Vec2 Vec2 Number))
  (doc 'description "Signed angle from a to b (radians)")
  (atan (vec2-cross a b) (vec2-dot a b)))

(define vec2-heading vec2-angle)
(doc vec2-heading 'type '(-> Vec2 Number))
(doc vec2-heading 'description "Alias for angle")


(doc 'section 'interpolation)
;;; @defines vec2-lerp vec2-slerp vec2-move-towards
(generate-vec2-interpolation
 vec2 vec2-x vec2-y
 vec2-add vec2-sub vec2-scale vec2-normalize vec2-magnitude vec2-angle-between
 vec2-lerp vec2-slerp vec2-move-towards)

(doc vec2-lerp 'type '(-> Vec2 Vec2 Number Vec2))
(doc vec2-lerp 'description "Linear interpolation between two vectors; t=0 gives a, t=1 gives b")
(doc vec2-slerp 'type '(-> Vec2 Vec2 Number Vec2))
(doc vec2-slerp 'description "Spherical linear interpolation; preserves angular velocity")
(doc vec2-move-towards 'type '(-> Vec2 Vec2 Number Vec2))
(doc vec2-move-towards 'description "Move from a towards b by at most max-distance")

(doc 'section 'projection)
;;; @defines vec2-project vec2-reject vec2-reflect
(generate-vec2-projection
 vec2 vec2-x vec2-y vec2-zero
 vec2-sub vec2-scale vec2-dot vec2-magnitude-sq
 vec2-project vec2-reject vec2-reflect)

(doc vec2-project 'type '(-> Vec2 Vec2 Vec2))
(doc vec2-project 'description "Project vector a onto vector b")
(doc vec2-reject 'type '(-> Vec2 Vec2 Vec2))
(doc vec2-reject 'description "Component of a perpendicular to b (rejection)")
(doc vec2-reflect 'type '(-> Vec2 Vec2 Vec2))
(doc vec2-reflect 'description "Reflect vector across a normal")

(doc 'section 'comparison)
;;; @defines vec2-equal? vec2-nearly-equal? vec2-zero?
(generate-vec2-comparison
 vec2-x vec2-y vec2-magnitude-sq
 vec2-equal? vec2-nearly-equal? vec2-zero?)

(doc vec2-equal? 'type '(-> Vec2 Vec2 Boolean))
(doc vec2-equal? 'description "Exact component-wise equality")
(doc vec2-nearly-equal? 'type '(-> Vec2 Vec2 Number Boolean))
(doc vec2-nearly-equal? 'description "Approximate equality within epsilon per component")
(doc vec2-zero? 'type '(-> Vec2 Boolean))
(doc vec2-zero? 'description "Test if vector is near-zero (magnitude < 1e-10)")

(doc 'section 'utilities)
;;; @defines vec2-min vec2-max vec2-clamp vec2-abs vec2-floor vec2-ceil vec2-round vec2-map vec2-fold vec2-sum vec2-product vec2-clamp-magnitude vec2-limit
(generate-vec2-utilities
 vec2 vec2-x vec2-y vec2-magnitude-sq vec2-set-magnitude
 vec2-min vec2-max vec2-clamp vec2-abs vec2-floor vec2-ceil vec2-round
 vec2-map vec2-fold vec2-sum vec2-product vec2-clamp-magnitude vec2-limit)

(doc vec2-min 'type '(-> Vec2 Vec2 Vec2))
(doc vec2-min 'description "Component-wise minimum")
(doc vec2-max 'type '(-> Vec2 Vec2 Vec2))
(doc vec2-max 'description "Component-wise maximum")
(doc vec2-clamp 'type '(-> Vec2 Vec2 Vec2 Vec2))
(doc vec2-clamp 'description "Clamp each component between min and max vectors")
(doc vec2-abs 'type '(-> Vec2 Vec2))
(doc vec2-abs 'description "Absolute value of each component")
(doc vec2-floor 'type '(-> Vec2 Vec2))
(doc vec2-floor 'description "Floor each component")
(doc vec2-ceil 'type '(-> Vec2 Vec2))
(doc vec2-ceil 'description "Ceiling each component")
(doc vec2-round 'type '(-> Vec2 Vec2))
(doc vec2-round 'description "Round each component to nearest integer")
(doc vec2-map 'type '(-> (-> Number Number) Vec2 Vec2))
(doc vec2-map 'description "Apply function to each component")
(doc vec2-fold 'type '(-> (-> a Number a) a Vec2 a))
(doc vec2-fold 'description "Left fold over components")
(doc vec2-sum 'type '(-> Vec2 Number))
(doc vec2-sum 'description "Sum of components")
(doc vec2-product 'type '(-> Vec2 Number))
(doc vec2-product 'description "Product of components")
(doc vec2-clamp-magnitude 'type '(-> Vec2 Number Number Vec2))
(doc vec2-clamp-magnitude 'description "Clamp vector magnitude between min and max")
(doc vec2-limit 'type '(-> Vec2 Number Vec2))
(doc vec2-limit 'description "Limit vector to maximum magnitude")

(doc 'section '2d-specific-construction)

(define (vec2-from-angle angle)
  (doc 'type '(-> Number Vec2))
  (doc 'description "Create unit vector from angle (radians)")
  (vec2 (cos angle) (sin angle)))

(define (vec2-from-polar r theta)
  (doc 'type '(-> Number Number Vec2))
  (doc 'description "Create vector from polar coordinates (r, theta)")
  (vec2 (* r (cos theta)) (* r (sin theta))))

(doc 'section '2d-rotation)

(define (vec2-rotate v angle)
  (doc 'type '(-> Vec2 Number Vec2))
  (doc 'description "Rotate vector by angle (radians)")
  (let ([c (cos angle)]
        [s (sin angle)]
        [x (vec2-x v)]
        [y (vec2-y v)])
       (vec2 (- (* x c) (* y s))
             (+ (* x s) (* y c)))))

(define (vec2-rotate-90 v)
  (doc 'type '(-> Vec2 Vec2))
  (doc 'description "Rotate 90 degrees counter-clockwise (perpendicular)")
  (vec2 (- (vec2-y v)) (vec2-x v)))

(define (vec2-rotate-neg-90 v)
  (doc 'type '(-> Vec2 Vec2))
  (doc 'description "Rotate 90 degrees clockwise")
  (vec2 (vec2-y v) (- (vec2-x v))))

(define vec2-perp vec2-rotate-90)
(doc vec2-perp 'type '(-> Vec2 Vec2))
(doc vec2-perp 'description "Perpendicular vector (rotate 90 CCW)")

(doc 'section 'printing)

(define (vec2->string v)
  (doc 'type '(-> Vec2 String))
  (format "(~a, ~a)" (vec2-x v) (vec2-y v)))
