;;; core/diff-physics/traced-vec2.ss --- Differentiable 2D Vector Operations
;;;
;;; Traced vec2 operations for automatic differentiation through physics.
;;; A traced-vec2 is a pair of traced scalar values, enabling gradient
;;; computation through vector arithmetic.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/vec2.ss
;;;   - autodiff/reverse-diff.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "lattice/autodiff/reverse-diff.ss")

;;; ============================================================
;;; Traced Vec2 Construction
;;; ============================================================

;;; traced-vec2 : TracedValue × TracedValue → TracedVec2
;;; Create a traced 2D vector from traced x and y components.
(define (traced-vec2 x y)
  (list 'traced-vec2 x y))

;;; traced-vec2? : Any → Boolean
(define (traced-vec2? v)
  (and (pair? v) (eq? (car v) 'traced-vec2)))

;;; traced-vec2-x : TracedVec2 → TracedValue
(define (traced-vec2-x v) (cadr v))

;;; traced-vec2-y : TracedVec2 → TracedValue
(define (traced-vec2-y v) (caddr v))

;;; traced-vec2-zero : Tape → TracedVec2
;;; Create traced zero vector.
(define (traced-vec2-zero tape)
  (traced-vec2 (make-traced-var 0 tape)
               (make-traced-var 0 tape)))

;;; ============================================================
;;; Conversion Between Vec2 and TracedVec2
;;; ============================================================

;;; lift-vec2 : Vec2 × Tape → TracedVec2
;;; Convert regular vec2 to traced vec2 (creates traced variables).
(define (lift-vec2 v tape)
  (traced-vec2 (make-traced-var (vec2-x v) tape)
               (make-traced-var (vec2-y v) tape)))

;;; lift-vec2-const : Vec2 → TracedVec2
;;; Convert regular vec2 to traced vec2 with constant components.
;;; Constants have zero gradient and don't need tape tracking.
(define (lift-vec2-const v)
  (traced-vec2 (vec2-x v) (vec2-y v)))

;;; unpack-traced-vec2 : TracedVec2 → Vec2
;;; Extract numeric values from traced vec2.
(define (unpack-traced-vec2 tv)
  (vec2 (traced-value (traced-vec2-x tv))
        (traced-value (traced-vec2-y tv))))

;;; traced-vec2->list : TracedVec2 → (List TracedValue)
;;; Extract components as list.
(define (traced-vec2->list tv)
  (list (traced-vec2-x tv) (traced-vec2-y tv)))

;;; ============================================================
;;; Basic Arithmetic
;;; ============================================================

;;; traced-vec2-add : TracedVec2 × TracedVec2 → TracedVec2
;;; Vector addition with gradient tracking.
(define (traced-vec2-add a b)
  (traced-vec2 (traced-add (traced-vec2-x a) (traced-vec2-x b))
               (traced-add (traced-vec2-y a) (traced-vec2-y b))))

;;; traced-vec2-sub : TracedVec2 × TracedVec2 → TracedVec2
;;; Vector subtraction.
(define (traced-vec2-sub a b)
  (traced-vec2 (traced-sub (traced-vec2-x a) (traced-vec2-x b))
               (traced-sub (traced-vec2-y a) (traced-vec2-y b))))

;;; traced-vec2-neg : TracedVec2 → TracedVec2
;;; Negate a vector.
(define (traced-vec2-neg v)
  (traced-vec2 (traced-neg (traced-vec2-x v))
               (traced-neg (traced-vec2-y v))))

;;; traced-vec2-mul : TracedVec2 × TracedVec2 → TracedVec2
;;; Component-wise multiplication (Hadamard product).
(define (traced-vec2-mul a b)
  (traced-vec2 (traced-mul (traced-vec2-x a) (traced-vec2-x b))
               (traced-mul (traced-vec2-y a) (traced-vec2-y b))))

;;; traced-vec2-div : TracedVec2 × TracedVec2 → TracedVec2
;;; Component-wise division.
(define (traced-vec2-div a b)
  (traced-vec2 (traced-div (traced-vec2-x a) (traced-vec2-x b))
               (traced-div (traced-vec2-y a) (traced-vec2-y b))))

;;; traced-vec2-scale : TracedVec2 × TracedValue → TracedVec2
;;; Scalar multiplication.
(define (traced-vec2-scale v s)
  (traced-vec2 (traced-mul (traced-vec2-x v) s)
               (traced-mul (traced-vec2-y v) s)))

;;; traced-vec2-scale-inv : TracedVec2 × TracedValue → TracedVec2
;;; Scalar division (v / s).
(define (traced-vec2-scale-inv v s)
  (traced-vec2 (traced-div (traced-vec2-x v) s)
               (traced-div (traced-vec2-y v) s)))

;;; ============================================================
;;; Products
;;; ============================================================

;;; traced-vec2-dot : TracedVec2 × TracedVec2 → TracedValue
;;; Dot product.
(define (traced-vec2-dot a b)
  (traced-add (traced-mul (traced-vec2-x a) (traced-vec2-x b))
              (traced-mul (traced-vec2-y a) (traced-vec2-y b))))

;;; traced-vec2-cross : TracedVec2 × TracedVec2 → TracedValue
;;; 2D cross product (z-component of 3D cross product).
;;; Returns ax*by - ay*bx (signed area of parallelogram).
(define (traced-vec2-cross a b)
  (traced-sub (traced-mul (traced-vec2-x a) (traced-vec2-y b))
              (traced-mul (traced-vec2-y a) (traced-vec2-x b))))

;;; ============================================================
;;; Length and Distance
;;; ============================================================

;;; traced-vec2-magnitude-sq : TracedVec2 → TracedValue
;;; Squared magnitude (avoids sqrt).
(define (traced-vec2-magnitude-sq v)
  (traced-add (traced-sq (traced-vec2-x v))
              (traced-sq (traced-vec2-y v))))

;;; traced-vec2-magnitude : TracedVec2 → TracedValue
;;; Vector magnitude (length).
;;; Note: gradient is undefined at |v|=0. Use smooth-magnitude for safety.
(define (traced-vec2-magnitude v)
  (traced-sqrt (traced-vec2-magnitude-sq v)))

;;; traced-vec2-smooth-magnitude : TracedVec2 × Number → TracedValue
;;; Smoothed magnitude with epsilon to avoid gradient singularity at zero.
;;; Returns sqrt(x² + y² + ε²) which is always > 0.
(define (traced-vec2-smooth-magnitude v epsilon)
  (traced-sqrt (traced-add (traced-vec2-magnitude-sq v)
                           (* epsilon epsilon))))

;;; traced-vec2-distance-sq : TracedVec2 × TracedVec2 → TracedValue
;;; Squared distance between two points.
(define (traced-vec2-distance-sq a b)
  (traced-vec2-magnitude-sq (traced-vec2-sub a b)))

;;; traced-vec2-distance : TracedVec2 × TracedVec2 → TracedValue
;;; Distance between two points.
(define (traced-vec2-distance a b)
  (traced-vec2-magnitude (traced-vec2-sub a b)))

;;; traced-vec2-smooth-distance : TracedVec2 × TracedVec2 × Number → TracedValue
;;; Smoothed distance with epsilon for gradient safety.
(define (traced-vec2-smooth-distance a b epsilon)
  (traced-vec2-smooth-magnitude (traced-vec2-sub a b) epsilon))

;;; ============================================================
;;; Normalization
;;; ============================================================

;;; traced-vec2-normalize : TracedVec2 → TracedVec2
;;; Return unit vector in same direction.
;;; WARNING: Gradient is undefined at |v|=0.
(define (traced-vec2-normalize v)
  (traced-vec2-scale-inv v (traced-vec2-magnitude v)))

;;; traced-vec2-smooth-normalize : TracedVec2 × Number → TracedVec2
;;; Normalized vector with smooth behavior near zero.
;;; Uses sqrt(x² + y² + ε²) in denominator for gradient stability.
(define (traced-vec2-smooth-normalize v epsilon)
  (traced-vec2-scale-inv v (traced-vec2-smooth-magnitude v epsilon)))

;;; traced-vec2-safe-normalize : TracedVec2 × Number → TracedVec2
;;; Safe normalization: returns zero vector if magnitude < epsilon.
;;; Not differentiable at the threshold - prefer smooth-normalize for AD.
(define (traced-vec2-safe-normalize v epsilon)
  (let ([mag (traced-value (traced-vec2-magnitude v))])
       (if (< mag epsilon)
           (lift-vec2-const (vec2-zero))
           (traced-vec2-normalize v))))

;;; ============================================================
;;; Rotation and Transformation
;;; ============================================================

;;; traced-vec2-rotate : TracedVec2 × TracedValue → TracedVec2
;;; Rotate vector by angle (radians).
;;; [x']   [cos θ  -sin θ] [x]
;;; [y'] = [sin θ   cos θ] [y]
(define (traced-vec2-rotate v angle)
  (let* ([c (traced-cos angle)]
         [s (traced-sin angle)]
         [x (traced-vec2-x v)]
         [y (traced-vec2-y v)])
        (traced-vec2 (traced-sub (traced-mul x c) (traced-mul y s))
                     (traced-add (traced-mul x s) (traced-mul y c)))))

;;; traced-vec2-rotate-90 : TracedVec2 → TracedVec2
;;; Rotate 90 degrees counter-clockwise (perpendicular).
(define (traced-vec2-rotate-90 v)
  (traced-vec2 (traced-neg (traced-vec2-y v))
               (traced-vec2-x v)))

;;; traced-vec2-rotate-neg-90 : TracedVec2 → TracedVec2
;;; Rotate 90 degrees clockwise.
(define (traced-vec2-rotate-neg-90 v)
  (traced-vec2 (traced-vec2-y v)
               (traced-neg (traced-vec2-x v))))

;;; traced-vec2-perp : TracedVec2 → TracedVec2
;;; Perpendicular vector (rotate 90 CCW).
(define traced-vec2-perp traced-vec2-rotate-90)

;;; traced-vec2-reflect : TracedVec2 × TracedVec2 → TracedVec2
;;; Reflect vector v across normal n.
;;; r = v - 2(v·n)n
(define (traced-vec2-reflect v normal)
  (let ([vn2 (traced-mul (traced-vec2-dot v normal) 2)])
       (traced-vec2-sub v (traced-vec2-scale normal vn2))))

;;; traced-vec2-project : TracedVec2 × TracedVec2 → TracedVec2
;;; Project vector a onto vector b.
;;; proj = (a·b / |b|²) * b
(define (traced-vec2-project a b)
  (let ([scale (traced-div (traced-vec2-dot a b)
                           (traced-vec2-magnitude-sq b))])
       (traced-vec2-scale b scale)))

;;; traced-vec2-reject : TracedVec2 × TracedVec2 → TracedVec2
;;; Component of a perpendicular to b.
(define (traced-vec2-reject a b)
  (traced-vec2-sub a (traced-vec2-project a b)))

;;; ============================================================
;;; Interpolation
;;; ============================================================

;;; traced-vec2-lerp : TracedVec2 × TracedVec2 × TracedValue → TracedVec2
;;; Linear interpolation between a and b.
;;; t=0 returns a, t=1 returns b.
(define (traced-vec2-lerp a b t)
  (traced-vec2-add (traced-vec2-scale a (traced-sub 1 t))
                   (traced-vec2-scale b t)))

;;; ============================================================
;;; Comparison (Non-differentiable - use carefully)
;;; ============================================================

;;; traced-vec2-min : TracedVec2 × TracedVec2 → TracedVec2
;;; Component-wise minimum.
;;; WARNING: Non-differentiable at equality. Use for bounds only.
(define (traced-vec2-min a b)
  (let ([ax (traced-value (traced-vec2-x a))]
        [ay (traced-value (traced-vec2-y a))]
        [bx (traced-value (traced-vec2-x b))]
        [by (traced-value (traced-vec2-y b))])
       (traced-vec2 (if (< ax bx) (traced-vec2-x a) (traced-vec2-x b))
                    (if (< ay by) (traced-vec2-y a) (traced-vec2-y b)))))

;;; traced-vec2-max : TracedVec2 × TracedVec2 → TracedVec2
;;; Component-wise maximum.
;;; WARNING: Non-differentiable at equality. Use for bounds only.
(define (traced-vec2-max a b)
  (let ([ax (traced-value (traced-vec2-x a))]
        [ay (traced-value (traced-vec2-y a))]
        [bx (traced-value (traced-vec2-x b))]
        [by (traced-value (traced-vec2-y b))])
       (traced-vec2 (if (> ax bx) (traced-vec2-x a) (traced-vec2-x b))
                    (if (> ay by) (traced-vec2-y a) (traced-vec2-y b)))))

;;; ============================================================
;;; Smooth Approximations for Non-differentiable Operations
;;; ============================================================

;;; traced-softplus : TracedValue × Number → TracedValue
;;; Smooth approximation of ReLU: log(1 + exp(α*x)) / α
;;; As α → ∞, approaches max(0, x).
(define (traced-softplus x alpha)
  (traced-div (traced-log (traced-add 1 (traced-exp (traced-mul x alpha))))
              alpha))

;;; traced-softmax2 : TracedValue × TracedValue × Number → TracedValue
;;; Smooth approximation of max(a, b).
;;; Uses log-sum-exp trick: (1/α) * log(exp(α*a) + exp(α*b))
(define (traced-softmax2 a b alpha)
  (let ([ea (traced-exp (traced-mul a alpha))]
        [eb (traced-exp (traced-mul b alpha))])
       (traced-div (traced-log (traced-add ea eb)) alpha)))

;;; traced-softmin2 : TracedValue × TracedValue × Number → TracedValue
;;; Smooth approximation of min(a, b).
(define (traced-softmin2 a b alpha)
  (traced-neg (traced-softmax2 (traced-neg a) (traced-neg b) alpha)))

;;; traced-smooth-abs : TracedValue × Number → TracedValue
;;; Smooth approximation of |x|: sqrt(x² + ε²)
(define (traced-smooth-abs x epsilon)
  (traced-sqrt (traced-add (traced-sq x) (* epsilon epsilon))))

;;; traced-smooth-clamp : TracedValue × Number × Number × Number → TracedValue
;;; Smooth clamp between lo and hi using softmin/softmax.
(define (traced-smooth-clamp x lo hi alpha)
  (traced-softmin2 (traced-softmax2 x lo alpha) hi alpha))

;;; ============================================================
;;; Gradient Utilities
;;; ============================================================

;;; vec2-gradient : ((TracedVec2) → TracedValue) × Vec2 → Vec2
;;; Compute gradient of scalar function f at point p.
;;; Returns (∂f/∂x, ∂f/∂y) as a regular vec2.
(define (vec2-gradient f p)
  (let* ([grads (gradient (lambda (x y)
                                  (f (traced-vec2 x y)))
                          (list (vec2-x p) (vec2-y p)))])
        (vec2 (car grads) (cadr grads))))

;;; vec2-gradient-2arg : ((TracedVec2 × TracedVec2) → TracedValue) × Vec2 × Vec2 → (Vec2 × Vec2)
;;; Compute gradients w.r.t. two vec2 arguments.
;;; Returns (∇_a f, ∇_b f).
(define (vec2-gradient-2arg f a b)
  (let* ([grads (gradient (lambda (ax ay bx by)
                                  (f (traced-vec2 ax ay) (traced-vec2 bx by)))
                          (list (vec2-x a) (vec2-y a) (vec2-x b) (vec2-y b)))])
        (values (vec2 (list-ref grads 0) (list-ref grads 1))
                (vec2 (list-ref grads 2) (list-ref grads 3)))))
