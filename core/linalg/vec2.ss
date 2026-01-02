;;; fabric/stitches/vec2.ss — 2D Vector Math Library
;;;
;;; Pure, functional 2D vector operations for physics and graphics.
;;;
;;; A vec2 is represented as: (vec2 x y)
;;; All operations are pure and return new vectors.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/prelude.ss")

;;; ============================================================
;;; Vector Construction
;;; ============================================================

;;; vec2 : Number × Number → Vec2
;;; Create a 2D vector.
(define (vec2 x y)
  (list 'vec2 x y))

;;; vec2-zero : → Vec2
;;; The zero vector (0, 0).
(define (vec2-zero)
  (vec2 0 0))

;;; vec2-one : → Vec2
;;; The unit vector (1, 1).
(define (vec2-one)
  (vec2 1 1))

;;; vec2-unit-x : → Vec2
;;; Unit vector in x direction (1, 0).
(define (vec2-unit-x)
  (vec2 1 0))

;;; vec2-unit-y : → Vec2
;;; Unit vector in y direction (0, 1).
(define (vec2-unit-y)
  (vec2 0 1))

;;; vec2-from-angle : Number → Vec2
;;; Create unit vector from angle (radians).
(define (vec2-from-angle angle)
  (vec2 (cos angle) (sin angle)))

;;; vec2-from-polar : Number × Number → Vec2
;;; Create vector from polar coordinates (r, θ).
(define (vec2-from-polar r theta)
  (vec2 (* r (cos theta)) (* r (sin theta))))

;;; ============================================================
;;; Predicates
;;; ============================================================

;;; vec2? : Any → Boolean
(define (vec2? v)
  (and (pair? v) (eq? (car v) 'vec2)))

;;; vec2-zero? : Vec2 → Boolean
;;; Check if vector is (near) zero.
(define (vec2-zero? v)
  (and (< (abs (vec2-x v)) 1e-10)
       (< (abs (vec2-y v)) 1e-10)))

;;; ============================================================
;;; Accessors
;;; ============================================================

;;; vec2-x : Vec2 → Number
(define (vec2-x v) (cadr v))

;;; vec2-y : Vec2 → Number
(define (vec2-y v) (caddr v))

;;; vec2->list : Vec2 → (List Number Number)
(define (vec2->list v)
  (list (vec2-x v) (vec2-y v)))

;;; list->vec2 : (List Number Number) → Vec2
(define (list->vec2 lst)
  (vec2 (car lst) (cadr lst)))

;;; ============================================================
;;; Basic Arithmetic
;;; ============================================================

;;; vec2-add : Vec2 × Vec2 → Vec2
;;; Vector addition.
(define (vec2-add a b)
  (vec2 (+ (vec2-x a) (vec2-x b))
        (+ (vec2-y a) (vec2-y b))))

;;; vec2-sub : Vec2 × Vec2 → Vec2
;;; Vector subtraction.
(define (vec2-sub a b)
  (vec2 (- (vec2-x a) (vec2-x b))
        (- (vec2-y a) (vec2-y b))))

;;; vec2-neg : Vec2 → Vec2
;;; Negate a vector.
(define (vec2-neg v)
  (vec2 (- (vec2-x v)) (- (vec2-y v))))

;;; vec2-mul : Vec2 × Vec2 → Vec2
;;; Component-wise multiplication (Hadamard product).
(define (vec2-mul a b)
  (vec2 (* (vec2-x a) (vec2-x b))
        (* (vec2-y a) (vec2-y b))))

;;; vec2-div : Vec2 × Vec2 → Vec2
;;; Component-wise division.
(define (vec2-div a b)
  (vec2 (/ (vec2-x a) (vec2-x b))
        (/ (vec2-y a) (vec2-y b))))

;;; vec2-scale : Vec2 × Number → Vec2
;;; Scalar multiplication.
(define (vec2-scale v s)
  (vec2 (* (vec2-x v) s)
        (* (vec2-y v) s)))

;;; vec2-scale-inv : Vec2 × Number → Vec2
;;; Scalar division (v / s).
(define (vec2-scale-inv v s)
  (vec2 (/ (vec2-x v) s)
        (/ (vec2-y v) s)))

;;; ============================================================
;;; Products
;;; ============================================================

;;; vec2-dot : Vec2 × Vec2 → Number
;;; Dot product.
(define (vec2-dot a b)
  (+ (* (vec2-x a) (vec2-x b))
     (* (vec2-y a) (vec2-y b))))

;;; vec2-cross : Vec2 × Vec2 → Number
;;; 2D cross product (z-component of 3D cross product).
;;; Returns the signed area of the parallelogram formed by the vectors.
(define (vec2-cross a b)
  (- (* (vec2-x a) (vec2-y b))
     (* (vec2-y a) (vec2-x b))))

;;; vec2-perp-dot : Vec2 × Vec2 → Number
;;; Perpendicular dot product (same as cross).
(define vec2-perp-dot vec2-cross)

;;; ============================================================
;;; Length and Distance
;;; ============================================================

;;; vec2-magnitude-sq : Vec2 → Number
;;; Squared magnitude (avoids sqrt).
(define (vec2-magnitude-sq v)
  (+ (* (vec2-x v) (vec2-x v))
     (* (vec2-y v) (vec2-y v))))

;;; vec2-magnitude : Vec2 → Number
;;; Vector magnitude (length).
(define (vec2-magnitude v)
  (sqrt (vec2-magnitude-sq v)))

;;; vec2-length : Vec2 → Number
;;; Alias for magnitude.
(define vec2-length vec2-magnitude)

;;; vec2-distance-sq : Vec2 × Vec2 → Number
;;; Squared distance between two points.
(define (vec2-distance-sq a b)
  (vec2-magnitude-sq (vec2-sub a b)))

;;; vec2-distance : Vec2 × Vec2 → Number
;;; Distance between two points.
(define (vec2-distance a b)
  (vec2-magnitude (vec2-sub a b)))

;;; ============================================================
;;; Normalization and Direction
;;; ============================================================

;;; vec2-normalize : Vec2 → Vec2
;;; Return unit vector in same direction.
;;; Returns zero vector if input is zero.
(define (vec2-normalize v)
  (let ([mag (vec2-magnitude v)])
       (if (< mag 1e-10)
           (vec2-zero)
           (vec2-scale-inv v mag))))

;;; vec2-unit : Vec2 → Vec2
;;; Alias for normalize.
(define vec2-unit vec2-normalize)

;;; vec2-set-magnitude : Vec2 × Number → Vec2
;;; Scale vector to have given magnitude.
(define (vec2-set-magnitude v new-mag)
  (vec2-scale (vec2-normalize v) new-mag))

;;; vec2-limit : Vec2 × Number → Vec2
;;; Limit vector magnitude to max-mag.
(define (vec2-limit v max-mag)
  (let ([mag-sq (vec2-magnitude-sq v)])
       (if (> mag-sq (* max-mag max-mag))
           (vec2-set-magnitude v max-mag)
           v)))

;;; vec2-clamp-magnitude : Vec2 × Number × Number → Vec2
;;; Clamp vector magnitude between min and max.
(define (vec2-clamp-magnitude v min-mag max-mag)
  (let ([mag (vec2-magnitude v)])
       (cond
        [(< mag min-mag) (vec2-set-magnitude v min-mag)]
        [(> mag max-mag) (vec2-set-magnitude v max-mag)]
        [else v])))

;;; ============================================================
;;; Angles
;;; ============================================================

;;; vec2-angle : Vec2 → Number
;;; Angle of vector (radians, from positive x-axis).
(define (vec2-angle v)
  (atan (vec2-y v) (vec2-x v)))

;;; vec2-angle-between : Vec2 × Vec2 → Number
;;; Angle between two vectors (radians, unsigned).
(define (vec2-angle-between a b)
  (let ([cos-theta (/ (vec2-dot a b)
                      (* (vec2-magnitude a) (vec2-magnitude b)))])
       ;; Clamp to avoid acos domain errors
       (acos (max -1 (min 1 cos-theta)))))

;;; vec2-angle-to : Vec2 × Vec2 → Number
;;; Signed angle from a to b (radians).
(define (vec2-angle-to a b)
  (atan (vec2-cross a b) (vec2-dot a b)))

;;; vec2-heading : Vec2 → Number
;;; Alias for angle.
(define vec2-heading vec2-angle)

;;; ============================================================
;;; Rotation and Transformation
;;; ============================================================

;;; vec2-rotate : Vec2 × Number → Vec2
;;; Rotate vector by angle (radians).
(define (vec2-rotate v angle)
  (let ([c (cos angle)]
        [s (sin angle)]
        [x (vec2-x v)]
        [y (vec2-y v)])
       (vec2 (- (* x c) (* y s))
             (+ (* x s) (* y c)))))

;;; vec2-rotate-90 : Vec2 → Vec2
;;; Rotate 90 degrees counter-clockwise (perpendicular).
(define (vec2-rotate-90 v)
  (vec2 (- (vec2-y v)) (vec2-x v)))

;;; vec2-rotate-neg-90 : Vec2 → Vec2
;;; Rotate 90 degrees clockwise.
(define (vec2-rotate-neg-90 v)
  (vec2 (vec2-y v) (- (vec2-x v))))

;;; vec2-perp : Vec2 → Vec2
;;; Perpendicular vector (rotate 90 CCW).
(define vec2-perp vec2-rotate-90)

;;; vec2-reflect : Vec2 × Vec2 → Vec2
;;; Reflect vector across normal (normal should be unit vector).
(define (vec2-reflect v normal)
  (vec2-sub v (vec2-scale normal (* 2 (vec2-dot v normal)))))

;;; vec2-project : Vec2 × Vec2 → Vec2
;;; Project vector a onto vector b.
(define (vec2-project a b)
  (let ([b-mag-sq (vec2-magnitude-sq b)])
       (if (< b-mag-sq 1e-10)
           (vec2-zero)
           (vec2-scale b (/ (vec2-dot a b) b-mag-sq)))))

;;; vec2-reject : Vec2 × Vec2 → Vec2
;;; Component of a perpendicular to b.
(define (vec2-reject a b)
  (vec2-sub a (vec2-project a b)))

;;; ============================================================
;;; Interpolation
;;; ============================================================

;;; vec2-lerp : Vec2 × Vec2 × Number → Vec2
;;; Linear interpolation between a and b.
;;; t=0 returns a, t=1 returns b.
(define (vec2-lerp a b t)
  (vec2-add (vec2-scale a (- 1 t))
            (vec2-scale b t)))

;;; vec2-slerp : Vec2 × Vec2 × Number → Vec2
;;; Spherical linear interpolation (for unit vectors).
(define (vec2-slerp a b t)
  (let* ([theta (vec2-angle-between a b)]
         [sin-theta (sin theta)])
        (if (< (abs sin-theta) 1e-10)
            (vec2-lerp a b t)  ; Fall back to lerp for parallel vectors
            (vec2-add (vec2-scale a (/ (sin (* (- 1 t) theta)) sin-theta))
                      (vec2-scale b (/ (sin (* t theta)) sin-theta))))))

;;; vec2-move-towards : Vec2 × Vec2 × Number → Vec2
;;; Move from a towards b by at most max-distance.
(define (vec2-move-towards a b max-distance)
  (let* ([delta (vec2-sub b a)]
         [dist (vec2-magnitude delta)])
        (if (<= dist max-distance)
            b
            (vec2-add a (vec2-scale (vec2-normalize delta) max-distance)))))

;;; ============================================================
;;; Comparison
;;; ============================================================

;;; vec2-equal? : Vec2 × Vec2 → Boolean
;;; Exact equality.
(define (vec2-equal? a b)
  (and (= (vec2-x a) (vec2-x b))
       (= (vec2-y a) (vec2-y b))))

;;; vec2-nearly-equal? : Vec2 × Vec2 × Number → Boolean
;;; Approximate equality within epsilon.
(define (vec2-nearly-equal? a b epsilon)
  (and (< (abs (- (vec2-x a) (vec2-x b))) epsilon)
       (< (abs (- (vec2-y a) (vec2-y b))) epsilon)))

;;; vec2-min : Vec2 × Vec2 → Vec2
;;; Component-wise minimum.
(define (vec2-min a b)
  (vec2 (min (vec2-x a) (vec2-x b))
        (min (vec2-y a) (vec2-y b))))

;;; vec2-max : Vec2 × Vec2 → Vec2
;;; Component-wise maximum.
(define (vec2-max a b)
  (vec2 (max (vec2-x a) (vec2-x b))
        (max (vec2-y a) (vec2-y b))))

;;; vec2-clamp : Vec2 × Vec2 × Vec2 → Vec2
;;; Clamp vector components between min and max vectors.
(define (vec2-clamp v min-v max-v)
  (vec2-min (vec2-max v min-v) max-v))

;;; vec2-abs : Vec2 → Vec2
;;; Component-wise absolute value.
(define (vec2-abs v)
  (vec2 (abs (vec2-x v)) (abs (vec2-y v))))

;;; vec2-floor : Vec2 → Vec2
;;; Component-wise floor.
(define (vec2-floor v)
  (vec2 (floor (vec2-x v)) (floor (vec2-y v))))

;;; vec2-ceil : Vec2 → Vec2
;;; Component-wise ceiling.
(define (vec2-ceil v)
  (vec2 (ceiling (vec2-x v)) (ceiling (vec2-y v))))

;;; vec2-round : Vec2 → Vec2
;;; Component-wise rounding.
(define (vec2-round v)
  (vec2 (round (vec2-x v)) (round (vec2-y v))))

;;; ============================================================
;;; Utility
;;; ============================================================

;;; vec2-map : (Number → Number) × Vec2 → Vec2
;;; Apply function to each component.
(define (vec2-map f v)
  (vec2 (f (vec2-x v)) (f (vec2-y v))))

;;; vec2-fold : (a × Number → a) × a × Vec2 → a
;;; Fold over components.
(define (vec2-fold f init v)
  (f (f init (vec2-x v)) (vec2-y v)))

;;; vec2-sum : Vec2 → Number
;;; Sum of components.
(define (vec2-sum v)
  (+ (vec2-x v) (vec2-y v)))

;;; vec2-product : Vec2 → Number
;;; Product of components.
(define (vec2-product v)
  (* (vec2-x v) (vec2-y v)))

;;; ============================================================
;;; Printing
;;; ============================================================

;;; vec2-print : Vec2 → String
(define (vec2->string v)
  (format "(~a, ~a)" (vec2-x v) (vec2-y v)))
