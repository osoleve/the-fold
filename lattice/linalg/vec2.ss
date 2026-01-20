(load "core/base/prelude.ss")
(load "lattice/linalg/vec-common.ss")

(doc 'module 'vec2)
(doc 'description "Pure, functional 2D vector operations for physics and graphics")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "A vec2 is represented as: (vec2 x y)")
(doc 'note "All operations are pure and return new vectors")
(doc 'note "Uses vec-common.ss macros to generate shared operations")
(doc 'requires '(prelude vec-common))


(doc 'section 'core-type)
(generate-vec2-core
 vec2 vec2? vec2-x vec2-y vec2->list list->vec2
 vec2-zero vec2-one vec2-unit-x vec2-unit-y)

(doc 'section 'arithmetic)
(generate-vec2-arithmetic
 vec2 vec2-x vec2-y
 vec2-add vec2-sub vec2-neg vec2-mul vec2-div vec2-scale vec2-scale-inv)

(doc 'section 'products-and-norms)
(generate-vec2-products
 vec2 vec2-x vec2-y vec2-zero vec2-sub vec2-scale-inv
 vec2-dot vec2-magnitude-sq vec2-magnitude vec2-length
 vec2-distance-sq vec2-distance vec2-normalize vec2-unit vec2-set-magnitude)

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
(generate-vec2-interpolation
 vec2 vec2-x vec2-y
 vec2-add vec2-sub vec2-scale vec2-normalize vec2-magnitude vec2-angle-between
 vec2-lerp vec2-slerp vec2-move-towards)

(doc 'section 'projection)
(generate-vec2-projection
 vec2 vec2-x vec2-y vec2-zero
 vec2-sub vec2-scale vec2-dot vec2-magnitude-sq
 vec2-project vec2-reject vec2-reflect)

(doc 'section 'comparison)
(generate-vec2-comparison
 vec2-x vec2-y vec2-magnitude-sq
 vec2-equal? vec2-nearly-equal? vec2-zero?)

(doc 'section 'utilities)
(generate-vec2-utilities
 vec2 vec2-x vec2-y vec2-magnitude-sq vec2-set-magnitude
 vec2-min vec2-max vec2-clamp vec2-abs vec2-floor vec2-ceil vec2-round
 vec2-map vec2-fold vec2-sum vec2-product vec2-clamp-magnitude vec2-limit)

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
