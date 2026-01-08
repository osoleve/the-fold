;;; core/linalg/vec2.ss --- 2D Vector Math Library
;;;
;;; Pure, functional 2D vector operations for physics and graphics.
;;;
;;; A vec2 is represented as: (vec2 x y)
;;; All operations are pure and return new vectors.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; This module uses vec-common.ss macros to generate shared operations
;;; and defines 2D-specific operations (rotation, polar coords, etc.).
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec-common.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec-common.ss")


;;; ============================================================
;;; Generate Core Vec2 Type
;;; ============================================================

(generate-vec2-core
 vec2 vec2? vec2-x vec2-y vec2->list list->vec2
 vec2-zero vec2-one vec2-unit-x vec2-unit-y)


;;; ============================================================
;;; Generate Vec2 Arithmetic
;;; ============================================================

(generate-vec2-arithmetic
 vec2 vec2-x vec2-y
 vec2-add vec2-sub vec2-neg vec2-mul vec2-div vec2-scale vec2-scale-inv)


;;; ============================================================
;;; Generate Vec2 Products and Norms
;;; ============================================================

(generate-vec2-products
 vec2 vec2-x vec2-y vec2-zero vec2-sub vec2-scale-inv
 vec2-dot vec2-magnitude-sq vec2-magnitude vec2-length
 vec2-distance-sq vec2-distance vec2-normalize vec2-unit vec2-set-magnitude)


;;; ============================================================
;;; 2D Cross Product (returns scalar, not vector)
;;; ============================================================

;;; vec2-cross : Vec2 x Vec2 -> Number
;;; 2D cross product (z-component of 3D cross product).
;;; Returns the signed area of the parallelogram formed by the vectors.
(define (vec2-cross a b)
  (- (* (vec2-x a) (vec2-y b))
     (* (vec2-y a) (vec2-x b))))

;;; vec2-perp-dot : Vec2 x Vec2 -> Number
;;; Perpendicular dot product (same as cross).
(define vec2-perp-dot vec2-cross)


;;; ============================================================
;;; 2D Angles (needed before interpolation)
;;; ============================================================

;;; vec2-angle : Vec2 -> Number
;;; Angle of vector (radians, from positive x-axis).
(define (vec2-angle v)
  (atan (vec2-y v) (vec2-x v)))

;;; vec2-angle-between : Vec2 x Vec2 -> Number
;;; Angle between two vectors (radians, unsigned).
(define (vec2-angle-between a b)
  (let ([cos-theta (/ (vec2-dot a b)
                      (* (vec2-magnitude a) (vec2-magnitude b)))])
       ;; Clamp to avoid acos domain errors
       (acos (max -1 (min 1 cos-theta)))))

;;; vec2-angle-to : Vec2 x Vec2 -> Number
;;; Signed angle from a to b (radians).
(define (vec2-angle-to a b)
  (atan (vec2-cross a b) (vec2-dot a b)))

;;; vec2-heading : Vec2 -> Number
;;; Alias for angle.
(define vec2-heading vec2-angle)


;;; ============================================================
;;; Generate Vec2 Interpolation
;;; ============================================================

(generate-vec2-interpolation
 vec2 vec2-x vec2-y
 vec2-add vec2-sub vec2-scale vec2-normalize vec2-magnitude vec2-angle-between
 vec2-lerp vec2-slerp vec2-move-towards)


;;; ============================================================
;;; Generate Vec2 Projection
;;; ============================================================

(generate-vec2-projection
 vec2 vec2-x vec2-y vec2-zero
 vec2-sub vec2-scale vec2-dot vec2-magnitude-sq
 vec2-project vec2-reject vec2-reflect)


;;; ============================================================
;;; Generate Vec2 Comparison
;;; ============================================================

(generate-vec2-comparison
 vec2-x vec2-y vec2-magnitude-sq
 vec2-equal? vec2-nearly-equal? vec2-zero?)


;;; ============================================================
;;; Generate Vec2 Utilities
;;; ============================================================

(generate-vec2-utilities
 vec2 vec2-x vec2-y vec2-magnitude-sq vec2-set-magnitude
 vec2-min vec2-max vec2-clamp vec2-abs vec2-floor vec2-ceil vec2-round
 vec2-map vec2-fold vec2-sum vec2-product vec2-clamp-magnitude vec2-limit)


;;; ============================================================
;;; 2D-Specific Construction
;;; ============================================================

;;; vec2-from-angle : Number -> Vec2
;;; Create unit vector from angle (radians).
(define (vec2-from-angle angle)
  (vec2 (cos angle) (sin angle)))

;;; vec2-from-polar : Number x Number -> Vec2
;;; Create vector from polar coordinates (r, theta).
(define (vec2-from-polar r theta)
  (vec2 (* r (cos theta)) (* r (sin theta))))


;;; ============================================================
;;; 2D Rotation
;;; ============================================================

;;; vec2-rotate : Vec2 x Number -> Vec2
;;; Rotate vector by angle (radians).
(define (vec2-rotate v angle)
  (let ([c (cos angle)]
        [s (sin angle)]
        [x (vec2-x v)]
        [y (vec2-y v)])
       (vec2 (- (* x c) (* y s))
             (+ (* x s) (* y c)))))

;;; vec2-rotate-90 : Vec2 -> Vec2
;;; Rotate 90 degrees counter-clockwise (perpendicular).
(define (vec2-rotate-90 v)
  (vec2 (- (vec2-y v)) (vec2-x v)))

;;; vec2-rotate-neg-90 : Vec2 -> Vec2
;;; Rotate 90 degrees clockwise.
(define (vec2-rotate-neg-90 v)
  (vec2 (vec2-y v) (- (vec2-x v))))

;;; vec2-perp : Vec2 -> Vec2
;;; Perpendicular vector (rotate 90 CCW).
(define vec2-perp vec2-rotate-90)


;;; ============================================================
;;; Printing
;;; ============================================================

;;; vec2->string : Vec2 -> String
(define (vec2->string v)
  (format "(~a, ~a)" (vec2-x v) (vec2-y v)))
