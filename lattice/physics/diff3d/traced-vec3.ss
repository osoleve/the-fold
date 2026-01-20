(load "core/base/prelude.ss")
(load "lattice/linalg/vec3.ss")
(load "core/autodiff/reverse-diff.ss")

(doc 'module 'traced-vec3)
(doc 'description "Differentiable 3D Vector Operations - Traced vec3 operations for automatic differentiation through 3D physics. A traced-vec3 is a triple of traced scalar values, enabling gradient computation through vector arithmetic.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "This is Core code: pure, total, assumes reasonable input.")

;;; ====
;;; Traced Vec3 Construction
;;; ====

;;; traced-vec3 : TracedValue × TracedValue × TracedValue → TracedVec3
;;; Create a traced 3D vector from traced x, y, and z components.
(define (traced-vec3 x y z)
  (doc 'export #t)
  (list 'traced-vec3 x y z))

;;; traced-vec3? : Any → Boolean
(define (traced-vec3? v)
  (and (pair? v) (eq? (car v) 'traced-vec3)))

;;; traced-vec3-x : TracedVec3 → TracedValue
(define (traced-vec3-x v) (list-ref v 1))

;;; traced-vec3-y : TracedVec3 → TracedValue
(define (traced-vec3-y v) (list-ref v 2))

;;; traced-vec3-z : TracedVec3 → TracedValue
(define (traced-vec3-z v) (list-ref v 3))

;;; traced-vec3-zero : Tape → TracedVec3
;;; Create traced zero vector.
(define (traced-vec3-zero tape)
  (traced-vec3 (make-traced-var 0 tape)
               (make-traced-var 0 tape)
               (make-traced-var 0 tape)))

;;; ====
;;; Conversion Between Vec3 and TracedVec3
;;; ====

;;; lift-vec3 : Vec3 × Tape → TracedVec3
;;; Convert regular vec3 to traced vec3 (creates traced variables).
(define (lift-vec3 v tape)
  (traced-vec3 (make-traced-var (vec3-x v) tape)
               (make-traced-var (vec3-y v) tape)
               (make-traced-var (vec3-z v) tape)))

;;; lift-vec3-const : Vec3 → TracedVec3
;;; Convert regular vec3 to traced vec3 with constant components.
;;; Constants have zero gradient and don't need tape tracking.
(define (lift-vec3-const v)
  (traced-vec3 (vec3-x v) (vec3-y v) (vec3-z v)))

;;; unpack-traced-vec3 : TracedVec3 → Vec3
;;; Extract numeric values from traced vec3.
(define (unpack-traced-vec3 tv)
  (vec3 (traced-value (traced-vec3-x tv))
        (traced-value (traced-vec3-y tv))
        (traced-value (traced-vec3-z tv))))

;;; traced-vec3->list : TracedVec3 → (List TracedValue)
;;; Extract components as list.
(define (traced-vec3->list tv)
  (list (traced-vec3-x tv) (traced-vec3-y tv) (traced-vec3-z tv)))

;;; ====
;;; Basic Arithmetic
;;; ====

;;; traced-vec3-add : TracedVec3 × TracedVec3 → TracedVec3
;;; Vector addition with gradient tracking.
(define (traced-vec3-add a b)
  (traced-vec3 (traced-add (traced-vec3-x a) (traced-vec3-x b))
               (traced-add (traced-vec3-y a) (traced-vec3-y b))
               (traced-add (traced-vec3-z a) (traced-vec3-z b))))

;;; traced-vec3-sub : TracedVec3 × TracedVec3 → TracedVec3
;;; Vector subtraction.
(define (traced-vec3-sub a b)
  (traced-vec3 (traced-sub (traced-vec3-x a) (traced-vec3-x b))
               (traced-sub (traced-vec3-y a) (traced-vec3-y b))
               (traced-sub (traced-vec3-z a) (traced-vec3-z b))))

;;; traced-vec3-neg : TracedVec3 → TracedVec3
;;; Negate a vector.
(define (traced-vec3-neg v)
  (traced-vec3 (traced-neg (traced-vec3-x v))
               (traced-neg (traced-vec3-y v))
               (traced-neg (traced-vec3-z v))))

;;; traced-vec3-mul : TracedVec3 × TracedVec3 → TracedVec3
;;; Component-wise multiplication (Hadamard product).
(define (traced-vec3-mul a b)
  (traced-vec3 (traced-mul (traced-vec3-x a) (traced-vec3-x b))
               (traced-mul (traced-vec3-y a) (traced-vec3-y b))
               (traced-mul (traced-vec3-z a) (traced-vec3-z b))))

;;; traced-vec3-div : TracedVec3 × TracedVec3 → TracedVec3
;;; Component-wise division.
(define (traced-vec3-div a b)
  (traced-vec3 (traced-div (traced-vec3-x a) (traced-vec3-x b))
               (traced-div (traced-vec3-y a) (traced-vec3-y b))
               (traced-div (traced-vec3-z a) (traced-vec3-z b))))

;;; traced-vec3-scale : TracedVec3 × TracedValue → TracedVec3
;;; Scalar multiplication.
(define (traced-vec3-scale v s)
  (traced-vec3 (traced-mul (traced-vec3-x v) s)
               (traced-mul (traced-vec3-y v) s)
               (traced-mul (traced-vec3-z v) s)))

;;; traced-vec3-scale-inv : TracedVec3 × TracedValue → TracedVec3
;;; Scalar division (v / s).
(define (traced-vec3-scale-inv v s)
  (traced-vec3 (traced-div (traced-vec3-x v) s)
               (traced-div (traced-vec3-y v) s)
               (traced-div (traced-vec3-z v) s)))

;;; ====
;;; Products
;;; ====

;;; traced-vec3-dot : TracedVec3 × TracedVec3 → TracedValue
;;; Dot product.
(define (traced-vec3-dot a b)
  (traced-add (traced-mul (traced-vec3-x a) (traced-vec3-x b))
              (traced-add (traced-mul (traced-vec3-y a) (traced-vec3-y b))
                          (traced-mul (traced-vec3-z a) (traced-vec3-z b)))))

;;; traced-vec3-cross : TracedVec3 × TracedVec3 → TracedVec3
;;; 3D cross product.
;;; Returns (ay*bz - az*by, az*bx - ax*bz, ax*by - ay*bx).
(define (traced-vec3-cross a b)
  (let ([ax (traced-vec3-x a)] [ay (traced-vec3-y a)] [az (traced-vec3-z a)]
        [bx (traced-vec3-x b)] [by (traced-vec3-y b)] [bz (traced-vec3-z b)])
       (traced-vec3 (traced-sub (traced-mul ay bz) (traced-mul az by))
                    (traced-sub (traced-mul az bx) (traced-mul ax bz))
                    (traced-sub (traced-mul ax by) (traced-mul ay bx)))))

;;; ====
;;; Length and Distance
;;; ====

;;; traced-vec3-magnitude-sq : TracedVec3 → TracedValue
;;; Squared magnitude (avoids sqrt).
(define (traced-vec3-magnitude-sq v)
  (traced-add (traced-sq (traced-vec3-x v))
              (traced-add (traced-sq (traced-vec3-y v))
                          (traced-sq (traced-vec3-z v)))))

;;; traced-vec3-magnitude : TracedVec3 → TracedValue
;;; Vector magnitude (length).
;;; Note: gradient is undefined at |v|=0. Use smooth-magnitude for safety.
(define (traced-vec3-magnitude v)
  (traced-sqrt (traced-vec3-magnitude-sq v)))

;;; traced-vec3-smooth-magnitude : TracedVec3 × Number → TracedValue
;;; Smoothed magnitude with epsilon to avoid gradient singularity at zero.
;;; Returns sqrt(x² + y² + z² + ε²) which is always > 0.
(define (traced-vec3-smooth-magnitude v epsilon)
  (traced-sqrt (traced-add (traced-vec3-magnitude-sq v)
                           (* epsilon epsilon))))

;;; traced-vec3-distance-sq : TracedVec3 × TracedVec3 → TracedValue
;;; Squared distance between two points.
(define (traced-vec3-distance-sq a b)
  (traced-vec3-magnitude-sq (traced-vec3-sub a b)))

;;; traced-vec3-distance : TracedVec3 × TracedVec3 → TracedValue
;;; Distance between two points.
(define (traced-vec3-distance a b)
  (traced-vec3-magnitude (traced-vec3-sub a b)))

;;; traced-vec3-smooth-distance : TracedVec3 × TracedVec3 × Number → TracedValue
;;; Smoothed distance with epsilon for gradient safety.
(define (traced-vec3-smooth-distance a b epsilon)
  (traced-vec3-smooth-magnitude (traced-vec3-sub a b) epsilon))

;;; ====
;;; Normalization
;;; ====

;;; traced-vec3-normalize : TracedVec3 → TracedVec3
;;; Return unit vector in same direction.
;;; WARNING: Gradient is undefined at |v|=0.
(define (traced-vec3-normalize v)
  (traced-vec3-scale-inv v (traced-vec3-magnitude v)))

;;; traced-vec3-smooth-normalize : TracedVec3 × Number → TracedVec3
;;; Normalized vector with smooth behavior near zero.
;;; Uses sqrt(x² + y² + z² + ε²) in denominator for gradient stability.
(define (traced-vec3-smooth-normalize v epsilon)
  (traced-vec3-scale-inv v (traced-vec3-smooth-magnitude v epsilon)))

;;; traced-vec3-safe-normalize : TracedVec3 × Number → TracedVec3
;;; Safe normalization: returns zero vector if magnitude < epsilon.
;;; Not differentiable at the threshold - prefer smooth-normalize for AD.
(define (traced-vec3-safe-normalize v epsilon)
  (let ([mag (traced-value (traced-vec3-magnitude v))])
       (if (< mag epsilon)
           (lift-vec3-const (vec3-zero))
           (traced-vec3-normalize v))))

;;; ====
;;; Projection and Reflection
;;; ====

;;; traced-vec3-reflect : TracedVec3 × TracedVec3 → TracedVec3
;;; Reflect vector v across normal n.
;;; r = v - 2(v·n)n
(define (traced-vec3-reflect v normal)
  (let ([vn2 (traced-mul (traced-vec3-dot v normal) 2)])
       (traced-vec3-sub v (traced-vec3-scale normal vn2))))

;;; traced-vec3-project : TracedVec3 × TracedVec3 → TracedVec3
;;; Project vector a onto vector b.
;;; proj = (a·b / |b|²) * b
(define (traced-vec3-project a b)
  (let ([scale (traced-div (traced-vec3-dot a b)
                           (traced-vec3-magnitude-sq b))])
       (traced-vec3-scale b scale)))

;;; traced-vec3-reject : TracedVec3 × TracedVec3 → TracedVec3
;;; Component of a perpendicular to b.
(define (traced-vec3-reject a b)
  (traced-vec3-sub a (traced-vec3-project a b)))

;;; ====
;;; Interpolation
;;; ====

;;; traced-vec3-lerp : TracedVec3 × TracedVec3 × TracedValue → TracedVec3
;;; Linear interpolation between a and b.
;;; t=0 returns a, t=1 returns b.
(define (traced-vec3-lerp a b t)
  (traced-vec3-add (traced-vec3-scale a (traced-sub 1 t))
                   (traced-vec3-scale b t)))

;;; ====
;;; Comparison (Non-differentiable - use carefully)
;;; ====

;;; traced-vec3-min : TracedVec3 × TracedVec3 → TracedVec3
;;; Component-wise minimum.
;;; WARNING: Non-differentiable at equality. Use for bounds only.
(define (traced-vec3-min a b)
  (let ([ax (traced-value (traced-vec3-x a))]
        [ay (traced-value (traced-vec3-y a))]
        [az (traced-value (traced-vec3-z a))]
        [bx (traced-value (traced-vec3-x b))]
        [by (traced-value (traced-vec3-y b))]
        [bz (traced-value (traced-vec3-z b))])
       (traced-vec3 (if (< ax bx) (traced-vec3-x a) (traced-vec3-x b))
                    (if (< ay by) (traced-vec3-y a) (traced-vec3-y b))
                    (if (< az bz) (traced-vec3-z a) (traced-vec3-z b)))))

;;; traced-vec3-max : TracedVec3 × TracedVec3 → TracedVec3
;;; Component-wise maximum.
;;; WARNING: Non-differentiable at equality. Use for bounds only.
(define (traced-vec3-max a b)
  (let ([ax (traced-value (traced-vec3-x a))]
        [ay (traced-value (traced-vec3-y a))]
        [az (traced-value (traced-vec3-z a))]
        [bx (traced-value (traced-vec3-x b))]
        [by (traced-value (traced-vec3-y b))]
        [bz (traced-value (traced-vec3-z b))])
       (traced-vec3 (if (> ax bx) (traced-vec3-x a) (traced-vec3-x b))
                    (if (> ay by) (traced-vec3-y a) (traced-vec3-y b))
                    (if (> az bz) (traced-vec3-z a) (traced-vec3-z b)))))

;;; traced-vec3-clamp : TracedVec3 × TracedVec3 × TracedVec3 → TracedVec3
;;; Component-wise clamp between min-bound and max-bound.
;;; WARNING: Non-differentiable at boundaries.
(define (traced-vec3-clamp v min-bound max-bound)
  (traced-vec3-min (traced-vec3-max v min-bound) max-bound))

;;; ====
;;; Smooth Approximations for Non-differentiable Operations
;;; ====

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

;;; traced-softmax3 : TracedValue × TracedValue × TracedValue × Number → TracedValue
;;; Smooth approximation of max(a, b, c).
(define (traced-softmax3 a b c alpha)
  (traced-softmax2 (traced-softmax2 a b alpha) c alpha))

;;; traced-softmin3 : TracedValue × TracedValue × TracedValue × Number → TracedValue
;;; Smooth approximation of min(a, b, c).
(define (traced-softmin3 a b c alpha)
  (traced-softmin2 (traced-softmin2 a b alpha) c alpha))

;;; traced-smooth-abs : TracedValue × Number → TracedValue
;;; Smooth approximation of |x|: sqrt(x² + ε²)
(define (traced-smooth-abs x epsilon)
  (traced-sqrt (traced-add (traced-sq x) (* epsilon epsilon))))

;;; traced-smooth-clamp : TracedValue × Number × Number × Number → TracedValue
;;; Smooth clamp between lo and hi using softmin/softmax.
(define (traced-smooth-clamp x lo hi alpha)
  (traced-softmin2 (traced-softmax2 x lo alpha) hi alpha))

;;; ====
;;; Gradient Utilities
;;; ====

;;; vec3-gradient : ((TracedVec3) → TracedValue) × Vec3 → Vec3
;;; Compute gradient of scalar function f at point p.
;;; Returns (∂f/∂x, ∂f/∂y, ∂f/∂z) as a regular vec3.
(define (vec3-gradient f p)
  (let* ([grads (gradient (lambda (x y z)
                                  (f (traced-vec3 x y z)))
                          (list (vec3-x p) (vec3-y p) (vec3-z p)))])
        (vec3 (list-ref grads 0) (list-ref grads 1) (list-ref grads 2))))

;;; vec3-gradient-2arg : ((TracedVec3 × TracedVec3) → TracedValue) × Vec3 × Vec3 → (Vec3 × Vec3)
;;; Compute gradients w.r.t. two vec3 arguments.
;;; Returns (∇_a f, ∇_b f).
(define (vec3-gradient-2arg f a b)
  (let* ([grads (gradient (lambda (ax ay az bx by bz)
                                  (f (traced-vec3 ax ay az) (traced-vec3 bx by bz)))
                          (list (vec3-x a) (vec3-y a) (vec3-z a)
                                (vec3-x b) (vec3-y b) (vec3-z b)))])
        (values (vec3 (list-ref grads 0) (list-ref grads 1) (list-ref grads 2))
                (vec3 (list-ref grads 3) (list-ref grads 4) (list-ref grads 5)))))
