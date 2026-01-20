(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix-solvers.ss")

(doc 'module 'lie-groups)
(doc 'description "Lie Groups and Algebras - Rotation and transformation groups with their Lie algebras")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "A Lie group G is a smooth manifold that is also a group")
(doc 'note "The tangent space at the identity element forms the Lie algebra 𝔤")
(doc 'note "The exponential map exp: 𝔤 → G converts infinitesimal motions into finite transformations")
(doc 'note "Implemented groups: SO(2), SO(3), SE(2), SE(3)")

(doc 'section 'constants)

(doc *lie-epsilon* 'description "Tolerance for numerical comparisons")
(define *lie-epsilon* 1e-10)

(doc *pi* 'description "Pi constant")
(define *pi* 3.141592653589793)

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

;;; identity-matrix : Nat → Matrix
;;; Create an n×n identity matrix.
(define (identity-matrix n)
  (let ([m (make-matrix n n 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) m)
      (matrix-set! m i i 1))))

;;; matrix-set! : Matrix × Nat × Nat × Num → Void
;;; Set element at (i,j) in a matrix (mutating).
(define (matrix-set! m i j val)
  (let ([cols (matrix-cols m)]
        [data (matrix-data m)])
    (vector-set! data (+ (* i cols) j) val)))

;;; matrix-trace : Matrix → Num
;;; Sum of diagonal elements.
(define (matrix-trace m)
  (let ([n (min (matrix-rows m) (matrix-cols m))])
    (let loop ([i 0] [sum 0])
      (if (= i n)
          sum
          (loop (+ i 1) (+ sum (matrix-ref m i i)))))))

;;; skew-symmetric? : Matrix × [Num] → Boolean
;;; Check if matrix is skew-symmetric (Aᵀ = -A).
(define (skew-symmetric? m . eps-arg)
  (let ([eps (if (null? eps-arg) *lie-epsilon* (car eps-arg))]
        [n (matrix-rows m)])
    (and (= n (matrix-cols m))  ; Must be square
         (let loop ([i 0])
           (or (= i n)
               (and (let inner ([j i])
                      (or (= j n)
                          (and (< (abs (+ (matrix-ref m i j)
                                          (matrix-ref m j i)))
                                  eps)
                               (inner (+ j 1)))))
                    (loop (+ i 1))))))))

(doc 'section 'so2-rotation-group)
(doc 'note "SO(2) is the group of 2×2 orthogonal matrices with determinant 1")
(doc 'note "Each element is a rotation by angle θ: R(θ) = [[cos θ, -sin θ], [sin θ, cos θ]]")
(doc 'note "The Lie algebra so(2) consists of 2×2 skew-symmetric matrices: ω̂ = [[0, -ω], [ω, 0]]")
(doc 'note "Exponential map: exp(θ · ĝ) = R(θ) where ĝ is the generator")

(doc so2? 'type '(-> Any Boolean))
(doc so2? 'description "Check if x is an SO(2) element (2×2 rotation matrix)")
(define (so2? x)
  (and (pair? x)
       (eq? (car x) 'so2)
       (= (length x) 2)
       (matrix? (cadr x))
       (= (matrix-rows (cadr x)) 2)
       (= (matrix-cols (cadr x)) 2)))

;;; so2-matrix : SO2 → Matrix
;;; Extract the rotation matrix.
(define (so2-matrix g)
  (cadr g))

;;; make-so2 : Num → SO2
;;; Create SO(2) element from rotation angle θ.
(define (make-so2 theta)
  (let ([c (cos theta)]
        [s (sin theta)]
        [m (make-matrix 2 2 0)])
    (matrix-set! m 0 0 c)
    (matrix-set! m 0 1 (- s))
    (matrix-set! m 1 0 s)
    (matrix-set! m 1 1 c)
    (list 'so2 m)))

;;; so2-identity : → SO2
;;; The identity element (zero rotation).
(define (so2-identity)
  (list 'so2 (identity-matrix 2)))

;;; so2-angle : SO2 → Num
;;; Extract the rotation angle from an SO(2) element.
(define (so2-angle g)
  (let ([m (so2-matrix g)])
    (atan (matrix-ref m 1 0) (matrix-ref m 0 0))))

;;; so2-compose : SO2 × SO2 → SO2
;;; Compose two rotations: g1 ∘ g2 (apply g2 first, then g1).
(define (so2-compose g1 g2)
  (list 'so2 (matrix-mul (so2-matrix g1) (so2-matrix g2))))

;;; so2-inverse : SO2 → SO2
;;; Inverse rotation (transpose, since orthogonal).
(define (so2-inverse g)
  (list 'so2 (matrix-transpose (so2-matrix g))))

;;; so2-act : SO2 × Vec2 → Vec2
;;; Apply rotation to a 2D vector.
(define (so2-act g v)
  (matrix-vec-mul (so2-matrix g) v))

;;; --- so(2) Lie Algebra ---

;;; so2-alg? : Any → Boolean
;;; Check if x is an so(2) algebra element.
(define (so2-alg? x)
  (and (pair? x)
       (eq? (car x) 'so2-alg)
       (= (length x) 2)
       (number? (cadr x))))

;;; so2-alg-omega : so2-alg → Num
;;; Extract the angular velocity.
(define (so2-alg-omega xi)
  (cadr xi))

;;; make-so2-alg : Num → so2-alg
;;; Create so(2) algebra element from angular velocity ω.
(define (make-so2-alg omega)
  (list 'so2-alg omega))

;;; so2-alg-to-matrix : so2-alg → Matrix
;;; Convert to skew-symmetric matrix form.
(define (so2-alg-to-matrix xi)
  (let ([omega (so2-alg-omega xi)]
        [m (make-matrix 2 2 0)])
    (matrix-set! m 0 1 (- omega))
    (matrix-set! m 1 0 omega)
    m))

;;; so2-matrix-to-alg : Matrix → so2-alg | Error
;;; Extract angular velocity from skew-symmetric matrix.
(define (so2-matrix-to-alg m)
  (if (and (= (matrix-rows m) 2) (= (matrix-cols m) 2))
      (make-so2-alg (matrix-ref m 1 0))
      `(error invalid-dimension)))

;;; --- SO(2) Exponential and Logarithm ---

;;; so2-exp : so2-alg → SO2
;;; Exponential map: so(2) → SO(2).
;;; exp(ω · dt) gives rotation by angle ω·dt.
(define (so2-exp xi)
  (make-so2 (so2-alg-omega xi)))

;;; so2-log : SO2 → so2-alg
;;; Logarithm map: SO(2) → so(2).
(define (so2-log g)
  (make-so2-alg (so2-angle g)))

;;; ============================================================================
;;; SO(3) — 3D Rotation Group
;;; ============================================================================
;;;
;;; SO(3) is the group of 3×3 orthogonal matrices with determinant 1.
;;; Parameterized by axis-angle: rotation by θ around unit axis n̂.
;;;
;;; The Lie algebra so(3) consists of 3×3 skew-symmetric matrices:
;;;
;;;   ω̂ = [ 0   -ω₃  ω₂]     corresponding to ω = (ω₁, ω₂, ω₃) ∈ ℝ³
;;;       [ ω₃   0  -ω₁]
;;;       [-ω₂  ω₁   0 ]
;;;
;;; The "hat" map ∧ : ℝ³ → so(3) and "vee" map ∨ : so(3) → ℝ³ convert between
;;; vector and matrix representations.
;;;
;;; Exponential map uses Rodrigues' formula:
;;;   exp(θn̂) = I + sin(θ)n̂ₓ + (1-cos(θ))n̂ₓ²
;;; where n̂ₓ is the skew-symmetric matrix of unit axis n̂.

;;; --- SO(3) Group Elements ---

;;; so3? : Any → Boolean
(define (so3? x)
  (and (pair? x)
       (eq? (car x) 'so3)
       (= (length x) 2)
       (matrix? (cadr x))
       (= (matrix-rows (cadr x)) 3)
       (= (matrix-cols (cadr x)) 3)))

;;; so3-matrix : SO3 → Matrix
(define (so3-matrix g)
  (cadr g))

;;; make-so3-from-matrix : Matrix → SO3 | Error
;;; Create SO(3) element from rotation matrix (validates orthogonality).
(define (make-so3-from-matrix m)
  (cond
    [(not (and (= (matrix-rows m) 3) (= (matrix-cols m) 3)))
     `(error invalid-dimension)]
    ;; Minimal validation: check orthogonality approximately
    [(let* ([mT (matrix-transpose m)]
            [prod (matrix-mul m mT)]
            [I (identity-matrix 3)])
       (not (matrix-approx-equal? prod I *lie-epsilon*)))
     `(error not-orthogonal)]
    [else
     (list 'so3 m)]))

;;; make-so3 : Vec3 × Num → SO3
;;; Create SO(3) element from axis (unit vector) and angle.
;;; Uses Rodrigues' formula.
(define (make-so3 axis theta)
  (so3-exp (make-so3-alg (vec-scale theta axis))))

;;; so3-identity : → SO3
(define (so3-identity)
  (list 'so3 (identity-matrix 3)))

;;; so3-compose : SO3 × SO3 → SO3
(define (so3-compose g1 g2)
  (list 'so3 (matrix-mul (so3-matrix g1) (so3-matrix g2))))

;;; so3-inverse : SO3 → SO3
(define (so3-inverse g)
  (list 'so3 (matrix-transpose (so3-matrix g))))

;;; so3-act : SO3 × Vec3 → Vec3
;;; Apply rotation to a 3D vector.
(define (so3-act g v)
  (matrix-vec-mul (so3-matrix g) v))

;;; --- so(3) Lie Algebra ---

;;; so3-alg? : Any → Boolean
(define (so3-alg? x)
  (and (pair? x)
       (eq? (car x) 'so3-alg)
       (= (length x) 2)
       (vector? (cadr x))
       (= (vector-length (cadr x)) 3)))

;;; so3-alg-vec : so3-alg → Vec3
;;; Extract the angular velocity vector.
(define (so3-alg-vec xi)
  (cadr xi))

;;; make-so3-alg : Vec3 → so3-alg
;;; Create so(3) algebra element from angular velocity vector ω.
(define (make-so3-alg omega)
  (list 'so3-alg omega))

;;; so3-hat : Vec3 → Matrix
;;; The "hat" map: convert vector to skew-symmetric matrix.
;;;   [ω₁]       [ 0   -ω₃  ω₂]
;;;   [ω₂]  →    [ ω₃   0  -ω₁]
;;;   [ω₃]       [-ω₂  ω₁   0 ]
(define (so3-hat v)
  (let ([m (make-matrix 3 3 0)]
        [w1 (vector-ref v 0)]
        [w2 (vector-ref v 1)]
        [w3 (vector-ref v 2)])
    (matrix-set! m 0 1 (- w3))
    (matrix-set! m 0 2 w2)
    (matrix-set! m 1 0 w3)
    (matrix-set! m 1 2 (- w1))
    (matrix-set! m 2 0 (- w2))
    (matrix-set! m 2 1 w1)
    m))

;;; so3-vee : Matrix → Vec3
;;; The "vee" map: extract vector from skew-symmetric matrix.
(define (so3-vee m)
  (vector (matrix-ref m 2 1)
          (matrix-ref m 0 2)
          (matrix-ref m 1 0)))

;;; so3-alg-to-matrix : so3-alg → Matrix
(define (so3-alg-to-matrix xi)
  (so3-hat (so3-alg-vec xi)))

;;; so3-matrix-to-alg : Matrix → so3-alg
(define (so3-matrix-to-alg m)
  (make-so3-alg (so3-vee m)))

;;; --- SO(3) Exponential Map (Rodrigues' Formula) ---

;;; so3-exp : so3-alg → SO3
;;; Exponential map: so(3) → SO(3).
;;; Uses Rodrigues' formula: exp(θn̂) = I + sin(θ)n̂ₓ + (1-cos(θ))n̂ₓ²
(define (so3-exp xi)
  (let* ([omega (so3-alg-vec xi)]
         [theta (vec-norm omega)])
    (if (< theta *lie-epsilon*)
        ;; Small angle: use Taylor expansion
        ;; exp(ω̂) ≈ I + ω̂ + ω̂²/2
        (let* ([omega-hat (so3-hat omega)]
               [omega-hat-sq (matrix-mul omega-hat omega-hat)]
               [I (identity-matrix 3)]
               [term1 (matrix-add I omega-hat)]
               [term2 (matrix-scale 0.5 omega-hat-sq)])
          (list 'so3 (matrix-add term1 term2)))
        ;; Normal case: Rodrigues' formula
        (let* ([axis (vec-scale (/ 1.0 theta) omega)]
               [axis-hat (so3-hat axis)]
               [axis-hat-sq (matrix-mul axis-hat axis-hat)]
               [I (identity-matrix 3)]
               [sin-term (matrix-scale (sin theta) axis-hat)]
               [cos-term (matrix-scale (- 1 (cos theta)) axis-hat-sq)])
          (list 'so3 (matrix-add (matrix-add I sin-term) cos-term))))))

;;; so3-log : SO3 → so3-alg
;;; Logarithm map: SO(3) → so(3).
;;; Inverse of Rodrigues' formula.
(define (so3-log g)
  (let* ([R (so3-matrix g)]
         [tr (matrix-trace R)]
         [cos-theta (/ (- tr 1) 2)])
    (cond
      ;; θ ≈ 0: Use linear approximation ω ≈ ½(R - Rᵀ)ᵛ
      ;; This preserves precision for small rotations instead of snapping to zero
      [(> cos-theta (- 1 *lie-epsilon*))
       (let* ([R-Rt (matrix-sub R (matrix-transpose R))]
              [omega-hat (matrix-scale 0.5 R-Rt)])
         (make-so3-alg (so3-vee omega-hat)))]
      ;; θ ≈ π: special handling needed
      [(< cos-theta (- -1 (- *lie-epsilon*)))
       ;; Find eigenvector corresponding to eigenvalue 1
       ;; For now, use approximation from R - Rᵀ
       (let* ([theta *pi*]
              [R-Rt (matrix-sub R (matrix-transpose R))]
              [omega-hat (matrix-scale (/ 1.0 2) R-Rt)]
              [omega (so3-vee omega-hat)]
              [n (vec-norm omega)])
         (if (< n *lie-epsilon*)
             ;; Diagonal case: extract from diagonal
             (let* ([d0 (+ 1 (matrix-ref R 0 0))]
                    [d1 (+ 1 (matrix-ref R 1 1))]
                    [d2 (+ 1 (matrix-ref R 2 2))]
                    [max-d (max d0 (max d1 d2))])
               (cond
                 [(= max-d d0)
                  (make-so3-alg (vec-scale (/ theta (sqrt (* 2 d0)))
                                           (vector d0
                                                   (matrix-ref R 0 1)
                                                   (matrix-ref R 0 2))))]
                 [(= max-d d1)
                  (make-so3-alg (vec-scale (/ theta (sqrt (* 2 d1)))
                                           (vector (matrix-ref R 1 0)
                                                   d1
                                                   (matrix-ref R 1 2))))]
                 [else
                  (make-so3-alg (vec-scale (/ theta (sqrt (* 2 d2)))
                                           (vector (matrix-ref R 2 0)
                                                   (matrix-ref R 2 1)
                                                   d2)))]))
             (make-so3-alg (vec-scale (/ theta n) omega))))]
      ;; Normal case
      [else
       (let* ([theta (acos cos-theta)]
              [R-Rt (matrix-sub R (matrix-transpose R))]
              [factor (/ theta (* 2 (sin theta)))]
              [omega-hat (matrix-scale factor R-Rt)])
         (make-so3-alg (so3-vee omega-hat)))])))

;;; so3-axis-angle : SO3 → (Vec3 × Num)
;;; Extract axis and angle from rotation.
(define (so3-axis-angle g)
  (let* ([xi (so3-log g)]
         [omega (so3-alg-vec xi)]
         [theta (vec-norm omega)])
    (if (< theta *lie-epsilon*)
        (cons (vector 1 0 0) 0)  ; Arbitrary axis for identity
        (cons (vec-scale (/ 1.0 theta) omega) theta))))

;;; ============================================================================
;;; SE(2) — 2D Rigid Transformation Group
;;; ============================================================================
;;;
;;; SE(2) = SO(2) ⋉ ℝ² is the group of 2D rigid transformations.
;;; Elements are (R, t) where R ∈ SO(2) and t ∈ ℝ² (translation).
;;;
;;; Represented as 3×3 homogeneous matrices:
;;;   [R  t]     [cos θ  -sin θ  tx]
;;;   [0  1]  =  [sin θ   cos θ  ty]
;;;             [  0       0     1 ]
;;;
;;; The Lie algebra se(2) has elements (ω, v) where ω ∈ so(2) and v ∈ ℝ².
;;; In matrix form:
;;;   [ 0  -ω  vx]
;;;   [ ω   0  vy]
;;;   [ 0   0   0]

;;; --- SE(2) Group Elements ---

;;; se2? : Any → Boolean
(define (se2? x)
  (and (pair? x)
       (eq? (car x) 'se2)
       (= (length x) 3)
       (so2? (cadr x))
       (vector? (caddr x))
       (= (vector-length (caddr x)) 2)))

;;; se2-rotation : SE2 → SO2
(define (se2-rotation g)
  (cadr g))

;;; se2-translation : SE2 → Vec2
(define (se2-translation g)
  (caddr g))

;;; make-se2 : SO2 × Vec2 → SE2
(define (make-se2 rot trans)
  (list 'se2 rot trans))

;;; make-se2-from-angle : Num × Vec2 → SE2
;;; Convenience constructor from angle and translation.
(define (make-se2-from-angle theta trans)
  (make-se2 (make-so2 theta) trans))

;;; se2-identity : → SE2
(define (se2-identity)
  (make-se2 (so2-identity) (vector 0 0)))

;;; se2-to-matrix : SE2 → Matrix (3×3)
(define (se2-to-matrix g)
  (let* ([R (so2-matrix (se2-rotation g))]
         [t (se2-translation g)]
         [m (make-matrix 3 3 0)])
    ;; Copy rotation block
    (matrix-set! m 0 0 (matrix-ref R 0 0))
    (matrix-set! m 0 1 (matrix-ref R 0 1))
    (matrix-set! m 1 0 (matrix-ref R 1 0))
    (matrix-set! m 1 1 (matrix-ref R 1 1))
    ;; Set translation
    (matrix-set! m 0 2 (vector-ref t 0))
    (matrix-set! m 1 2 (vector-ref t 1))
    ;; Set bottom row
    (matrix-set! m 2 2 1)
    m))

;;; matrix-to-se2 : Matrix → SE2 | Error
(define (matrix-to-se2 m)
  (if (not (and (= (matrix-rows m) 3) (= (matrix-cols m) 3)))
      `(error invalid-dimension)
      (let* ([R (make-matrix 2 2 0)]
             [t (vector (matrix-ref m 0 2) (matrix-ref m 1 2))])
        (matrix-set! R 0 0 (matrix-ref m 0 0))
        (matrix-set! R 0 1 (matrix-ref m 0 1))
        (matrix-set! R 1 0 (matrix-ref m 1 0))
        (matrix-set! R 1 1 (matrix-ref m 1 1))
        (make-se2 (list 'so2 R) t))))

;;; se2-compose : SE2 × SE2 → SE2
;;; (R1, t1) ∘ (R2, t2) = (R1·R2, R1·t2 + t1)
(define (se2-compose g1 g2)
  (let* ([R1 (se2-rotation g1)]
         [t1 (se2-translation g1)]
         [R2 (se2-rotation g2)]
         [t2 (se2-translation g2)]
         [R-new (so2-compose R1 R2)]
         [t-new (vec-add (so2-act R1 t2) t1)])
    (make-se2 R-new t-new)))

;;; se2-inverse : SE2 → SE2
;;; (R, t)⁻¹ = (R⁻¹, -R⁻¹·t)
(define (se2-inverse g)
  (let* ([R (se2-rotation g)]
         [t (se2-translation g)]
         [R-inv (so2-inverse R)]
         [t-inv (vec-negate (so2-act R-inv t))])
    (make-se2 R-inv t-inv)))

;;; se2-act : SE2 × Vec2 → Vec2
;;; Apply rigid transformation: x' = R·x + t
(define (se2-act g v)
  (vec-add (so2-act (se2-rotation g) v)
           (se2-translation g)))

;;; --- se(2) Lie Algebra ---

;;; se2-alg? : Any → Boolean
(define (se2-alg? x)
  (and (pair? x)
       (eq? (car x) 'se2-alg)
       (= (length x) 3)
       (number? (cadr x))
       (vector? (caddr x))
       (= (vector-length (caddr x)) 2)))

;;; se2-alg-omega : se2-alg → Num
(define (se2-alg-omega xi)
  (cadr xi))

;;; se2-alg-v : se2-alg → Vec2
(define (se2-alg-v xi)
  (caddr xi))

;;; make-se2-alg : Num × Vec2 → se2-alg
(define (make-se2-alg omega v)
  (list 'se2-alg omega v))

;;; se2-alg-to-matrix : se2-alg → Matrix (3×3)
(define (se2-alg-to-matrix xi)
  (let ([omega (se2-alg-omega xi)]
        [v (se2-alg-v xi)]
        [m (make-matrix 3 3 0)])
    (matrix-set! m 0 1 (- omega))
    (matrix-set! m 1 0 omega)
    (matrix-set! m 0 2 (vector-ref v 0))
    (matrix-set! m 1 2 (vector-ref v 1))
    m))

;;; --- SE(2) Exponential and Logarithm ---

;;; se2-exp : se2-alg → SE2
;;; Exponential map: se(2) → SE(2).
(define (se2-exp xi)
  (let* ([omega (se2-alg-omega xi)]
         [v (se2-alg-v xi)]
         [theta (abs omega)])
    (if (< theta *lie-epsilon*)
        ;; Small angle: exp(ξ) ≈ (I, v)
        (make-se2 (so2-identity) v)
        ;; Normal case: use closed-form solution
        (let* ([s (sin omega)]
               [c (cos omega)]
               [R (make-so2 omega)]
               ;; V matrix for translation
               ;; V = (1/θ)[sin θ    -(1-cos θ)]
               ;;          [1-cos θ   sin θ     ]
               [V00 (/ s omega)]
               [V01 (/ (- c 1) omega)]
               [V10 (/ (- 1 c) omega)]
               [V11 (/ s omega)]
               [tx (+ (* V00 (vector-ref v 0)) (* V01 (vector-ref v 1)))]
               [ty (+ (* V10 (vector-ref v 0)) (* V11 (vector-ref v 1)))])
          (make-se2 R (vector tx ty))))))

;;; se2-log : SE2 → se2-alg
;;; Logarithm map: SE(2) → se(2).
(define (se2-log g)
  (let* ([R (se2-rotation g)]
         [t (se2-translation g)]
         [theta (so2-angle R)])
    (if (< (abs theta) *lie-epsilon*)
        ;; Small angle: v = t
        (make-se2-alg 0 t)
        ;; Normal case: invert V matrix
        (let* ([half-theta (/ theta 2)]
               [cot-half (/ (cos half-theta) (sin half-theta))]
               ;; V⁻¹ = (θ/2)[cot(θ/2)  1]
               ;;             [-1        cot(θ/2)]
               [factor (/ theta 2)]
               [Vi00 (* factor cot-half)]
               [Vi01 factor]
               [Vi10 (- factor)]
               [Vi11 (* factor cot-half)]
               [vx (+ (* Vi00 (vector-ref t 0)) (* Vi01 (vector-ref t 1)))]
               [vy (+ (* Vi10 (vector-ref t 0)) (* Vi11 (vector-ref t 1)))])
          (make-se2-alg theta (vector vx vy))))))

;;; ============================================================================
;;; SE(3) — 3D Rigid Transformation Group
;;; ============================================================================
;;;
;;; SE(3) = SO(3) ⋉ ℝ³ is the group of 3D rigid transformations.
;;; Elements are (R, t) where R ∈ SO(3) and t ∈ ℝ³.
;;;
;;; Represented as 4×4 homogeneous matrices:
;;;   [R  t]
;;;   [0  1]
;;;
;;; The Lie algebra se(3) has elements (ω, v) where ω ∈ so(3) and v ∈ ℝ³.

;;; --- SE(3) Group Elements ---

;;; se3? : Any → Boolean
(define (se3? x)
  (and (pair? x)
       (eq? (car x) 'se3)
       (= (length x) 3)
       (so3? (cadr x))
       (vector? (caddr x))
       (= (vector-length (caddr x)) 3)))

;;; se3-rotation : SE3 → SO3
(define (se3-rotation g)
  (cadr g))

;;; se3-translation : SE3 → Vec3
(define (se3-translation g)
  (caddr g))

;;; make-se3 : SO3 × Vec3 → SE3
(define (make-se3 rot trans)
  (list 'se3 rot trans))

;;; se3-identity : → SE3
(define (se3-identity)
  (make-se3 (so3-identity) (vector 0 0 0)))

;;; se3-to-matrix : SE3 → Matrix (4×4)
(define (se3-to-matrix g)
  (let* ([R (so3-matrix (se3-rotation g))]
         [t (se3-translation g)]
         [m (make-matrix 4 4 0)])
    ;; Copy rotation block
    (do ([i 0 (+ i 1)])
        ((= i 3))
      (do ([j 0 (+ j 1)])
          ((= j 3))
        (matrix-set! m i j (matrix-ref R i j))))
    ;; Set translation
    (do ([i 0 (+ i 1)])
        ((= i 3))
      (matrix-set! m i 3 (vector-ref t i)))
    ;; Set bottom row
    (matrix-set! m 3 3 1)
    m))

;;; matrix-to-se3 : Matrix → SE3 | Error
(define (matrix-to-se3 m)
  (if (not (and (= (matrix-rows m) 4) (= (matrix-cols m) 4)))
      `(error invalid-dimension)
      (let* ([R (make-matrix 3 3 0)]
             [t (make-vector 3 0)])
        ;; Extract rotation
        (do ([i 0 (+ i 1)])
            ((= i 3))
          (do ([j 0 (+ j 1)])
              ((= j 3))
            (matrix-set! R i j (matrix-ref m i j))))
        ;; Extract translation
        (do ([i 0 (+ i 1)])
            ((= i 3))
          (vector-set! t i (matrix-ref m i 3)))
        (make-se3 (list 'so3 R) t))))

;;; se3-compose : SE3 × SE3 → SE3
(define (se3-compose g1 g2)
  (let* ([R1 (se3-rotation g1)]
         [t1 (se3-translation g1)]
         [R2 (se3-rotation g2)]
         [t2 (se3-translation g2)]
         [R-new (so3-compose R1 R2)]
         [t-new (vec-add (so3-act R1 t2) t1)])
    (make-se3 R-new t-new)))

;;; se3-inverse : SE3 → SE3
(define (se3-inverse g)
  (let* ([R (se3-rotation g)]
         [t (se3-translation g)]
         [R-inv (so3-inverse R)]
         [t-inv (vec-negate (so3-act R-inv t))])
    (make-se3 R-inv t-inv)))

;;; se3-act : SE3 × Vec3 → Vec3
(define (se3-act g v)
  (vec-add (so3-act (se3-rotation g) v)
           (se3-translation g)))

;;; --- se(3) Lie Algebra ---

;;; se3-alg? : Any → Boolean
(define (se3-alg? x)
  (and (pair? x)
       (eq? (car x) 'se3-alg)
       (= (length x) 3)
       (vector? (cadr x))
       (= (vector-length (cadr x)) 3)
       (vector? (caddr x))
       (= (vector-length (caddr x)) 3)))

;;; se3-alg-omega : se3-alg → Vec3
(define (se3-alg-omega xi)
  (cadr xi))

;;; se3-alg-v : se3-alg → Vec3
(define (se3-alg-v xi)
  (caddr xi))

;;; make-se3-alg : Vec3 × Vec3 → se3-alg
(define (make-se3-alg omega v)
  (list 'se3-alg omega v))

;;; se3-alg-to-matrix : se3-alg → Matrix (4×4)
(define (se3-alg-to-matrix xi)
  (let ([omega (se3-alg-omega xi)]
        [v (se3-alg-v xi)]
        [m (make-matrix 4 4 0)])
    ;; Set rotation block (skew-symmetric)
    (let ([omega-hat (so3-hat omega)])
      (do ([i 0 (+ i 1)])
          ((= i 3))
        (do ([j 0 (+ j 1)])
            ((= j 3))
          (matrix-set! m i j (matrix-ref omega-hat i j)))))
    ;; Set translation column
    (do ([i 0 (+ i 1)])
        ((= i 3))
      (matrix-set! m i 3 (vector-ref v i)))
    m))

;;; --- SE(3) Exponential and Logarithm ---

;;; se3-exp : se3-alg → SE3
;;; Exponential map: se(3) → SE(3).
(define (se3-exp xi)
  (let* ([omega (se3-alg-omega xi)]
         [v (se3-alg-v xi)]
         [theta (vec-norm omega)])
    (if (< theta *lie-epsilon*)
        ;; Small rotation: exp(ξ) ≈ (I, v)
        (make-se3 (so3-identity) v)
        ;; Normal case
        (let* ([axis (vec-scale (/ 1.0 theta) omega)]
               [R (so3-exp (make-so3-alg omega))]
               ;; V matrix: V = I + ((1-cos θ)/θ²)ω̂ + ((θ-sin θ)/θ³)ω̂²
               [omega-hat (so3-hat omega)]
               [omega-hat-sq (matrix-mul omega-hat omega-hat)]
               [I (identity-matrix 3)]
               [c1 (/ (- 1 (cos theta)) (* theta theta))]
               [c2 (/ (- theta (sin theta)) (* theta theta theta))]
               [V (matrix-add (matrix-add I
                                          (matrix-scale c1 omega-hat))
                              (matrix-scale c2 omega-hat-sq))]
               [t (matrix-vec-mul V v)])
          (make-se3 R t)))))

;;; se3-log : SE3 → se3-alg
;;; Logarithm map: SE(3) → se(3).
(define (se3-log g)
  (let* ([R (se3-rotation g)]
         [t (se3-translation g)]
         [omega-alg (so3-log R)]
         [omega (so3-alg-vec omega-alg)]
         [theta (vec-norm omega)])
    (if (< theta *lie-epsilon*)
        ;; Small rotation: v = t
        (make-se3-alg omega t)
        ;; Normal case: invert V matrix
        (let* ([omega-hat (so3-hat omega)]
               [omega-hat-sq (matrix-mul omega-hat omega-hat)]
               [I (identity-matrix 3)]
               ;; V⁻¹ = I - ω̂/2 + (1/θ² - (1+cos θ)/(2θ sin θ))ω̂²
               [c1 -0.5]
               [c2 (- (/ 1 (* theta theta))
                      (/ (+ 1 (cos theta))
                         (* 2 theta (sin theta))))]
               [V-inv (matrix-add (matrix-add I
                                              (matrix-scale c1 omega-hat))
                                  (matrix-scale c2 omega-hat-sq))]
               [v (matrix-vec-mul V-inv t)])
          (make-se3-alg omega v)))))

;;; ============================================================================
;;; Adjoint Representation
;;; ============================================================================
;;;
;;; The adjoint representation Ad_g : 𝔤 → 𝔤 describes how the group acts
;;; on its Lie algebra. For matrix Lie groups: Ad_g(ξ) = g·ξ·g⁻¹

;;; so3-adjoint : SO3 → (so3-alg → so3-alg)
;;; Adjoint representation for SO(3): Ad_R(ω) = R·ω
(define (so3-adjoint g)
  (lambda (xi)
    (make-so3-alg (so3-act g (so3-alg-vec xi)))))

;;; se3-adjoint : SE3 → (se3-alg → se3-alg)
;;; Adjoint representation for SE(3).
;;; Ad_(R,t)(ω, v) = (R·ω, R·v + t×(R·ω))
(define (se3-adjoint g)
  (lambda (xi)
    (let* ([R (se3-rotation g)]
           [t (se3-translation g)]
           [omega (se3-alg-omega xi)]
           [v (se3-alg-v xi)]
           [R-omega (so3-act R omega)]
           [R-v (so3-act R v)]
           [t-cross-R-omega (vec-cross t R-omega)])
      (make-se3-alg R-omega (vec-add R-v t-cross-R-omega)))))

;;; vec-cross : Vec3 × Vec3 → Vec3
;;; Cross product of 3D vectors.
(define (vec-cross a b)
  (vector (- (* (vector-ref a 1) (vector-ref b 2))
             (* (vector-ref a 2) (vector-ref b 1)))
          (- (* (vector-ref a 2) (vector-ref b 0))
             (* (vector-ref a 0) (vector-ref b 2)))
          (- (* (vector-ref a 0) (vector-ref b 1))
             (* (vector-ref a 1) (vector-ref b 0)))))

;;; ============================================================================
;;; Baker-Campbell-Hausdorff Formula
;;; ============================================================================
;;;
;;; BCH approximates log(exp(X)·exp(Y)) for Lie algebra elements.
;;; Full formula is an infinite series; we implement second-order approximation:
;;;   BCH(X, Y) ≈ X + Y + [X,Y]/2 + [X,[X,Y]]/12 + [Y,[Y,X]]/12 + ...

;;; so3-bracket : so3-alg × so3-alg → so3-alg
;;; Lie bracket [X, Y] = X×Y (cross product for so(3)).
(define (so3-bracket xi eta)
  (make-so3-alg (vec-cross (so3-alg-vec xi) (so3-alg-vec eta))))

;;; so3-bch-2 : so3-alg × so3-alg → so3-alg
;;; Second-order BCH approximation for so(3).
;;; BCH(X, Y) ≈ X + Y + [X,Y]/2
(define (so3-bch-2 xi eta)
  (let* ([bracket (so3-bracket xi eta)]
         [sum (vec-add (so3-alg-vec xi) (so3-alg-vec eta))]
         [bracket-half (vec-scale 0.5 (so3-alg-vec bracket))])
    (make-so3-alg (vec-add sum bracket-half))))

;;; so3-bch-4 : so3-alg × so3-alg → so3-alg
;;; Fourth-order BCH approximation for so(3).
;;; BCH ≈ X + Y + [X,Y]/2 + [X,[X,Y]]/12 + [Y,[Y,X]]/12
(define (so3-bch-4 xi eta)
  (let* ([bracket-xy (so3-bracket xi eta)]
         [bracket-yx (so3-bracket eta xi)]
         [bracket-xxy (so3-bracket xi bracket-xy)]
         [bracket-yyx (so3-bracket eta bracket-yx)]
         [sum (vec-add (so3-alg-vec xi) (so3-alg-vec eta))]
         [term2 (vec-scale 0.5 (so3-alg-vec bracket-xy))]
         [term3 (vec-scale (/ 1.0 12) (so3-alg-vec bracket-xxy))]
         [term4 (vec-scale (/ 1.0 12) (so3-alg-vec bracket-yyx))])
    (make-so3-alg (vec-add (vec-add sum term2)
                           (vec-add term3 term4)))))

;;; ============================================================================
;;; Interpolation
;;; ============================================================================

;;; so3-interpolate : SO3 × SO3 × Num → SO3
;;; Spherical linear interpolation (SLERP) between rotations.
;;; t ∈ [0, 1], returns rotation at parameter t.
(define (so3-interpolate g1 g2 t)
  (let* ([g1-inv (so3-inverse g1)]
         [delta (so3-compose g1-inv g2)]
         [delta-log (so3-log delta)]
         [scaled-log (make-so3-alg (vec-scale t (so3-alg-vec delta-log)))])
    (so3-compose g1 (so3-exp scaled-log))))

;;; se3-interpolate : SE3 × SE3 × Num → SE3
;;; Interpolation in SE(3) using exponential map.
(define (se3-interpolate g1 g2 t)
  (let* ([g1-inv (se3-inverse g1)]
         [delta (se3-compose g1-inv g2)]
         [delta-log (se3-log delta)]
         [scaled-omega (vec-scale t (se3-alg-omega delta-log))]
         [scaled-v (vec-scale t (se3-alg-v delta-log))]
         [scaled-log (make-se3-alg scaled-omega scaled-v)])
    (se3-compose g1 (se3-exp scaled-log))))

;;; ============================================================================
;;; REPL Interface
;;; ============================================================================

(printf "lie-groups.ss loaded — Lie Groups and Algebras\n")
(printf "  SO(2) - 2D Rotations:\n")
(printf "    (make-so2 theta)           - Create rotation by angle θ\n")
(printf "    (so2-exp xi)               - Exponential map: so(2) → SO(2)\n")
(printf "    (so2-log g)                - Logarithm map: SO(2) → so(2)\n")
(printf "  SO(3) - 3D Rotations:\n")
(printf "    (make-so3 axis theta)      - Create rotation (axis-angle)\n")
(printf "    (so3-exp xi)               - Rodrigues formula\n")
(printf "    (so3-log g)                - Inverse Rodrigues\n")
(printf "    (so3-hat v), (so3-vee m)   - Hat/vee isomorphisms\n")
(printf "  SE(2), SE(3) - Rigid Transformations:\n")
(printf "    (make-se2 R t), (make-se3 R t)\n")
(printf "    (se2-exp xi), (se3-exp xi)\n")
(printf "    (se2-log g), (se3-log g)\n")
(printf "  Adjoint:\n")
(printf "    (so3-adjoint g), (se3-adjoint g)\n")
(printf "  BCH (Baker-Campbell-Hausdorff):\n")
(printf "    (so3-bch-2 xi eta)         - Second-order approximation\n")
(printf "    (so3-bch-4 xi eta)         - Fourth-order approximation\n")
(printf "  Interpolation:\n")
(printf "    (so3-interpolate g1 g2 t)  - SLERP\n")
(printf "    (se3-interpolate g1 g2 t)\n")
