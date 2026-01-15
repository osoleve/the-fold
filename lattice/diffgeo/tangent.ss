;;; lattice/diffgeo/tangent.ss — Tangent and Cotangent Spaces
;;; @module tangent
;;; @requires prelude matrix vec charts
;;;
;;; Differential geometry structures for tangent vectors and differential forms.
;;;
;;; A tangent vector at a point p represents a "direction" on the manifold.
;;; In coordinates, it's represented by components (v¹, ..., vⁿ) that transform
;;; covariantly: v'ⁱ = Σⱼ (∂x'ⁱ/∂xʲ) vʲ = Jⁱⱼ vʲ
;;;
;;; A cotangent vector (1-form) at p is a linear functional on tangent vectors.
;;; Components (ω₁, ..., ωₙ) transform contravariantly: ω'ᵢ = Σⱼ (∂xʲ/∂x'ⁱ) ωⱼ
;;;
;;; This is Lattice code: pure, uses Core primitives.
;;;
;;; Dependencies:
;;;   core/base/prelude.ss
;;;   lattice/linalg/matrix.ss
;;;   lattice/linalg/vec.ss
;;;   lattice/diffgeo/charts.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/diffgeo/charts.ss")

;;; ============================================================================
;;; Tangent Vector Representation
;;; ============================================================================

;;; A tangent vector is: (tangent-vector point chart components)
;;; - point: The base point on the manifold
;;; - chart: The chart in which components are expressed
;;; - components: Vector of components (v¹, ..., vⁿ)
;;;
;;; The tangent vector represents ∂/∂xⁱ basis vectors:
;;;   v = Σᵢ vⁱ (∂/∂xⁱ)

;;; tangent-vector? : Any → Boolean
(define (tangent-vector? x)
  (and (pair? x)
       (eq? (car x) 'tangent-vector)
       (= (length x) 4)
       (chart? (tangent-vector-chart x))
       (vector? (tangent-vector-components x))))

;;; tangent-vector-point : TangentVector → Point
(define (tangent-vector-point tv)
  (list-ref tv 1))

;;; tangent-vector-chart : TangentVector → Chart
(define (tangent-vector-chart tv)
  (list-ref tv 2))

;;; tangent-vector-components : TangentVector → Vec
(define (tangent-vector-components tv)
  (list-ref tv 3))

;;; tangent-vector-dim : TangentVector → Nat
(define (tangent-vector-dim tv)
  (vector-length (tangent-vector-components tv)))

;;; ============================================================================
;;; Tangent Vector Construction
;;; ============================================================================

;;; make-tangent-vector : Point × Chart × Vec → TangentVector | Error
;;; Create a tangent vector at a point, expressed in chart coordinates.
(define (make-tangent-vector point chart components)
  (cond
   [(not (chart-contains? chart point))
    `(error point-not-in-chart ,(chart-name chart))]
   [(not (= (chart-dim chart) (vector-length components)))
    `(error dimension-mismatch ,(chart-dim chart) ,(vector-length components))]
   [else
    (list 'tangent-vector point chart components)]))

;;; tangent-zero : Point × Chart → TangentVector
;;; The zero tangent vector at a point.
(define (tangent-zero point chart)
  (make-tangent-vector point chart (vec-zeros (chart-dim chart))))

;;; tangent-basis-vector : Point × Chart × Nat → TangentVector
;;; The i-th basis vector ∂/∂xⁱ at a point.
(define (tangent-basis-vector point chart i)
  (make-tangent-vector point chart (vec-unit (chart-dim chart) i)))

;;; ============================================================================
;;; Tangent Vector Operations
;;; ============================================================================

;;; tangent-add : TangentVector × TangentVector → TangentVector | Error
;;; Add two tangent vectors (must be at the same point, same chart).
(define (tangent-add tv1 tv2)
  (let ([p1 (tangent-vector-point tv1)]
        [p2 (tangent-vector-point tv2)]
        [c1 (tangent-vector-chart tv1)]
        [c2 (tangent-vector-chart tv2)])
    (cond
     [(not (equal? p1 p2))
      `(error different-base-points)]
     [(not (eq? (chart-name c1) (chart-name c2)))
      `(error different-charts ,(chart-name c1) ,(chart-name c2))]
     [else
      (make-tangent-vector
       p1 c1
       (vec-add (tangent-vector-components tv1)
                (tangent-vector-components tv2)))])))

;;; tangent-sub : TangentVector × TangentVector → TangentVector | Error
;;; Subtract tangent vectors.
(define (tangent-sub tv1 tv2)
  (let ([p1 (tangent-vector-point tv1)]
        [p2 (tangent-vector-point tv2)]
        [c1 (tangent-vector-chart tv1)]
        [c2 (tangent-vector-chart tv2)])
    (cond
     [(not (equal? p1 p2))
      `(error different-base-points)]
     [(not (eq? (chart-name c1) (chart-name c2)))
      `(error different-charts ,(chart-name c1) ,(chart-name c2))]
     [else
      (make-tangent-vector
       p1 c1
       (vec-sub (tangent-vector-components tv1)
                (tangent-vector-components tv2)))])))

;;; tangent-scale : Num × TangentVector → TangentVector
;;; Scalar multiplication of a tangent vector.
(define (tangent-scale k tv)
  (make-tangent-vector
   (tangent-vector-point tv)
   (tangent-vector-chart tv)
   (vec-scale k (tangent-vector-components tv))))

;;; tangent-negate : TangentVector → TangentVector
(define (tangent-negate tv)
  (tangent-scale -1 tv))

;;; ============================================================================
;;; Chart Change for Tangent Vectors
;;; ============================================================================

;;; Tangent vectors transform covariantly under coordinate change:
;;;   v'ⁱ = Σⱼ Jⁱⱼ vʲ
;;; where J is the Jacobian of the transition function.

;;; tangent-change-chart : TangentVector × Chart → TangentVector | Error
;;; Express a tangent vector in a different chart.
(define (tangent-change-chart tv new-chart)
  (let* ([point (tangent-vector-point tv)]
         [old-chart (tangent-vector-chart tv)]
         [old-components (tangent-vector-components tv)])
    (cond
     [(not (chart-contains? new-chart point))
      `(error point-not-in-target-chart ,(chart-name new-chart))]
     [(eq? (chart-name old-chart) (chart-name new-chart))
      tv]  ; Same chart, no change needed
     [else
      ;; Compute Jacobian at the point's coordinates in old chart
      (let* ([old-coords (chart-apply old-chart point)]
             [J (transition-jacobian old-chart new-chart old-coords)]
             [new-components (matrix-vec-mul J old-components)])
        (make-tangent-vector point new-chart new-components))])))

;;; ============================================================================
;;; Cotangent Vector (1-Form) Representation
;;; ============================================================================

;;; A cotangent vector is: (cotangent-vector point chart components)
;;; - point: The base point on the manifold
;;; - chart: The chart in which components are expressed
;;; - components: Vector of components (ω₁, ..., ωₙ)
;;;
;;; In the coordinate basis, ω = Σᵢ ωᵢ dxⁱ
;;; where dxⁱ is the differential of the i-th coordinate function.

;;; cotangent-vector? : Any → Boolean
(define (cotangent-vector? x)
  (and (pair? x)
       (eq? (car x) 'cotangent-vector)
       (= (length x) 4)
       (chart? (cotangent-vector-chart x))
       (vector? (cotangent-vector-components x))))

;;; cotangent-vector-point : CotangentVector → Point
(define (cotangent-vector-point cv)
  (list-ref cv 1))

;;; cotangent-vector-chart : CotangentVector → Chart
(define (cotangent-vector-chart cv)
  (list-ref cv 2))

;;; cotangent-vector-components : CotangentVector → Vec
(define (cotangent-vector-components cv)
  (list-ref cv 3))

;;; cotangent-vector-dim : CotangentVector → Nat
(define (cotangent-vector-dim cv)
  (vector-length (cotangent-vector-components cv)))

;;; ============================================================================
;;; Cotangent Vector Construction
;;; ============================================================================

;;; make-cotangent-vector : Point × Chart × Vec → CotangentVector | Error
;;; Create a cotangent vector (1-form) at a point.
(define (make-cotangent-vector point chart components)
  (cond
   [(not (chart-contains? chart point))
    `(error point-not-in-chart ,(chart-name chart))]
   [(not (= (chart-dim chart) (vector-length components)))
    `(error dimension-mismatch ,(chart-dim chart) ,(vector-length components))]
   [else
    (list 'cotangent-vector point chart components)]))

;;; cotangent-zero : Point × Chart → CotangentVector
;;; The zero cotangent vector at a point.
(define (cotangent-zero point chart)
  (make-cotangent-vector point chart (vec-zeros (chart-dim chart))))

;;; cotangent-basis-form : Point × Chart × Nat → CotangentVector
;;; The i-th basis 1-form dxⁱ at a point.
(define (cotangent-basis-form point chart i)
  (make-cotangent-vector point chart (vec-unit (chart-dim chart) i)))

;;; ============================================================================
;;; Cotangent Vector Operations
;;; ============================================================================

;;; cotangent-add : CotangentVector × CotangentVector → CotangentVector | Error
(define (cotangent-add cv1 cv2)
  (let ([p1 (cotangent-vector-point cv1)]
        [p2 (cotangent-vector-point cv2)]
        [c1 (cotangent-vector-chart cv1)]
        [c2 (cotangent-vector-chart cv2)])
    (cond
     [(not (equal? p1 p2))
      `(error different-base-points)]
     [(not (eq? (chart-name c1) (chart-name c2)))
      `(error different-charts ,(chart-name c1) ,(chart-name c2))]
     [else
      (make-cotangent-vector
       p1 c1
       (vec-add (cotangent-vector-components cv1)
                (cotangent-vector-components cv2)))])))

;;; cotangent-sub : CotangentVector × CotangentVector → CotangentVector | Error
(define (cotangent-sub cv1 cv2)
  (let ([p1 (cotangent-vector-point cv1)]
        [p2 (cotangent-vector-point cv2)]
        [c1 (cotangent-vector-chart cv1)]
        [c2 (cotangent-vector-chart cv2)])
    (cond
     [(not (equal? p1 p2))
      `(error different-base-points)]
     [(not (eq? (chart-name c1) (chart-name c2)))
      `(error different-charts ,(chart-name c1) ,(chart-name c2))]
     [else
      (make-cotangent-vector
       p1 c1
       (vec-sub (cotangent-vector-components cv1)
                (cotangent-vector-components cv2)))])))

;;; cotangent-scale : Num × CotangentVector → CotangentVector
(define (cotangent-scale k cv)
  (make-cotangent-vector
   (cotangent-vector-point cv)
   (cotangent-vector-chart cv)
   (vec-scale k (cotangent-vector-components cv))))

;;; cotangent-negate : CotangentVector → CotangentVector
(define (cotangent-negate cv)
  (cotangent-scale -1 cv))

;;; ============================================================================
;;; Pairing: Cotangent × Tangent → Scalar
;;; ============================================================================

;;; The natural pairing ⟨ω, v⟩ = Σᵢ ωᵢ vⁱ is coordinate-independent.

;;; covector-apply : CotangentVector × TangentVector → Num | Error
;;; Apply a cotangent vector to a tangent vector: ⟨ω, v⟩.
(define (covector-apply cv tv)
  (let ([p1 (cotangent-vector-point cv)]
        [p2 (tangent-vector-point tv)]
        [c1 (cotangent-vector-chart cv)]
        [c2 (tangent-vector-chart tv)])
    (cond
     [(not (equal? p1 p2))
      `(error different-base-points)]
     [(not (eq? (chart-name c1) (chart-name c2)))
      ;; Transform tangent vector to same chart, then pair
      (let ([tv-transformed (tangent-change-chart tv c1)])
        (if (and (pair? tv-transformed) (eq? (car tv-transformed) 'error))
            tv-transformed
            (vec-dot (cotangent-vector-components cv)
                     (tangent-vector-components tv-transformed))))]
     [else
      (vec-dot (cotangent-vector-components cv)
               (tangent-vector-components tv))])))

;;; ============================================================================
;;; Chart Change for Cotangent Vectors
;;; ============================================================================

;;; Cotangent vectors transform contravariantly:
;;;   ω'ᵢ = Σⱼ (J⁻¹)ʲᵢ ωⱼ = Σⱼ (∂xʲ/∂x'ⁱ) ωⱼ
;;; where J⁻¹ is the inverse Jacobian.

;;; cotangent-change-chart : CotangentVector × Chart → CotangentVector | Error
;;; Express a cotangent vector in a different chart.
(define (cotangent-change-chart cv new-chart)
  (let* ([point (cotangent-vector-point cv)]
         [old-chart (cotangent-vector-chart cv)]
         [old-components (cotangent-vector-components cv)])
    (cond
     [(not (chart-contains? new-chart point))
      `(error point-not-in-target-chart ,(chart-name new-chart))]
     [(eq? (chart-name old-chart) (chart-name new-chart))
      cv]  ; Same chart
     [else
      ;; Need the inverse Jacobian: (J_{old→new})⁻¹ = J_{new→old}
      ;; ω transforms as: ω' = (J⁻¹)ᵀ ω = Jᵀ_{new→old} ω
      (let* ([new-coords (chart-apply new-chart point)]
             [J-inv (transition-jacobian new-chart old-chart new-coords)]
             ;; Cotangent transforms with transpose of inverse Jacobian
             [J-inv-T (matrix-transpose J-inv)]
             [new-components (matrix-vec-mul J-inv-T old-components)])
        (make-cotangent-vector point new-chart new-components))])))

;;; ============================================================================
;;; Pushforward (Differential)
;;; ============================================================================

;;; Given a smooth map f: M → N, the pushforward (differential) at p is:
;;;   df_p: T_p M → T_{f(p)} N
;;;
;;; In coordinates: (df)ⁱⱼ = ∂fⁱ/∂xʲ (the Jacobian of f)
;;;
;;; For a tangent vector v at p: df_p(v) is the tangent vector at f(p)
;;; with components (df_p(v))ⁱ = Σⱼ (∂fⁱ/∂xʲ) vʲ

;;; pushforward : (Vec → Vec) × TangentVector × Chart × Chart → TangentVector | Error
;;; Push a tangent vector forward through a map.
;;; f is the map in coordinate form, from source-chart to target-chart.
(define (pushforward f tv source-chart target-chart)
  (let* ([point (tangent-vector-point tv)]
         [tv-in-source (tangent-change-chart tv source-chart)])
    (if (and (pair? tv-in-source) (eq? (car tv-in-source) 'error))
        tv-in-source
        (let* ([coords (chart-apply source-chart point)]
               [new-coords (f coords)]
               [new-point (chart-apply-inverse target-chart new-coords)]
               [J (jacobian-numerical f coords)]
               [v-components (tangent-vector-components tv-in-source)]
               [new-components (matrix-vec-mul J v-components)])
          (make-tangent-vector new-point target-chart new-components)))))

;;; ============================================================================
;;; Pullback
;;; ============================================================================

;;; Given a smooth map f: M → N, the pullback at p is:
;;;   f*: T*_{f(p)} N → T*_p M
;;;
;;; For a cotangent vector ω at f(p): (f*ω)ᵢ = Σⱼ (∂fʲ/∂xⁱ) ωⱼ = Jᵀ ω
;;;
;;; Pullback is the dual of pushforward: ⟨f*ω, v⟩ = ⟨ω, df(v)⟩

;;; pullback-at : (Vec → Vec) × CotangentVector × Point × Chart × Chart → CotangentVector | Error
;;; Pull back a cotangent vector to a specific source point.
(define (pullback-at f cv source-point source-chart target-chart)
  (let ([cv-in-target (cotangent-change-chart cv target-chart)])
    (if (and (pair? cv-in-target) (eq? (car cv-in-target) 'error))
        cv-in-target
        (let* ([source-coords (chart-apply source-chart source-point)]
               [J (jacobian-numerical f source-coords)]
               [J-T (matrix-transpose J)]
               [omega-components (cotangent-vector-components cv-in-target)]
               [new-components (matrix-vec-mul J-T omega-components)])
          (make-cotangent-vector source-point source-chart new-components)))))

;;; ============================================================================
;;; Tangent Space
;;; ============================================================================

;;; The tangent space T_p M at a point p is the vector space of all tangent
;;; vectors at p. We represent it as a structure that can generate basis vectors.

;;; A tangent space is: (tangent-space point chart dim)
(define (tangent-space? x)
  (and (pair? x)
       (eq? (car x) 'tangent-space)
       (= (length x) 4)))

;;; tangent-space-point : TangentSpace → Point
(define (tangent-space-point ts)
  (list-ref ts 1))

;;; tangent-space-chart : TangentSpace → Chart
(define (tangent-space-chart ts)
  (list-ref ts 2))

;;; tangent-space-dim : TangentSpace → Nat
(define (tangent-space-dim ts)
  (list-ref ts 3))

;;; make-tangent-space : Point × Chart → TangentSpace | Error
;;; Create the tangent space at a point.
(define (make-tangent-space point chart)
  (if (chart-contains? chart point)
      (list 'tangent-space point chart (chart-dim chart))
      `(error point-not-in-chart ,(chart-name chart))))

;;; tangent-space-basis : TangentSpace → (List TangentVector)
;;; Get the coordinate basis vectors {∂/∂x¹, ..., ∂/∂xⁿ}.
(define (tangent-space-basis ts)
  (let ([point (tangent-space-point ts)]
        [chart (tangent-space-chart ts)]
        [n (tangent-space-dim ts)])
    (let loop ([i 0] [acc '()])
      (if (= i n)
          (reverse acc)
          (loop (+ i 1)
                (cons (tangent-basis-vector point chart i) acc))))))

;;; tangent-space-vector : TangentSpace × Vec → TangentVector
;;; Create a tangent vector in this tangent space from components.
(define (tangent-space-vector ts components)
  (make-tangent-vector
   (tangent-space-point ts)
   (tangent-space-chart ts)
   components))

;;; ============================================================================
;;; Cotangent Space
;;; ============================================================================

;;; The cotangent space T*_p M is the dual of T_p M.

;;; A cotangent space is: (cotangent-space point chart dim)
(define (cotangent-space? x)
  (and (pair? x)
       (eq? (car x) 'cotangent-space)
       (= (length x) 4)))

;;; cotangent-space-point : CotangentSpace → Point
(define (cotangent-space-point cs)
  (list-ref cs 1))

;;; cotangent-space-chart : CotangentSpace → Chart
(define (cotangent-space-chart cs)
  (list-ref cs 2))

;;; cotangent-space-dim : CotangentSpace → Nat
(define (cotangent-space-dim cs)
  (list-ref cs 3))

;;; make-cotangent-space : Point × Chart → CotangentSpace | Error
;;; Create the cotangent space at a point.
(define (make-cotangent-space point chart)
  (if (chart-contains? chart point)
      (list 'cotangent-space point chart (chart-dim chart))
      `(error point-not-in-chart ,(chart-name chart))))

;;; cotangent-space-basis : CotangentSpace → (List CotangentVector)
;;; Get the coordinate basis forms {dx¹, ..., dxⁿ}.
(define (cotangent-space-basis cs)
  (let ([point (cotangent-space-point cs)]
        [chart (cotangent-space-chart cs)]
        [n (cotangent-space-dim cs)])
    (let loop ([i 0] [acc '()])
      (if (= i n)
          (reverse acc)
          (loop (+ i 1)
                (cons (cotangent-basis-form point chart i) acc))))))

;;; cotangent-space-form : CotangentSpace × Vec → CotangentVector
;;; Create a cotangent vector in this space from components.
(define (cotangent-space-form cs components)
  (make-cotangent-vector
   (cotangent-space-point cs)
   (cotangent-space-chart cs)
   components))

;;; ============================================================================
;;; Tangent Bundle
;;; ============================================================================

;;; The tangent bundle TM is the disjoint union of all tangent spaces.
;;; A point in TM is a pair (p, v) where p ∈ M and v ∈ T_p M.
;;;
;;; We represent the tangent bundle as a structure that can produce
;;; tangent spaces at any point.

;;; A tangent bundle is: (tangent-bundle atlas)
(define (tangent-bundle? x)
  (and (pair? x)
       (eq? (car x) 'tangent-bundle)
       (= (length x) 2)
       (atlas? (tangent-bundle-atlas x))))

;;; tangent-bundle-atlas : TangentBundle → Atlas
(define (tangent-bundle-atlas tb)
  (list-ref tb 1))

;;; make-tangent-bundle : Atlas → TangentBundle
;;; Create the tangent bundle over a manifold given by an atlas.
(define (make-tangent-bundle atlas)
  (list 'tangent-bundle atlas))

;;; tangent-bundle-dim : TangentBundle → Nat | #f
;;; The dimension of the tangent bundle is 2n where n is the base dimension.
(define (tangent-bundle-dim tb)
  (let ([base-dim (atlas-dim (tangent-bundle-atlas tb))])
    (if base-dim (* 2 base-dim) #f)))

;;; tangent-bundle-fiber : TangentBundle × Point → TangentSpace | Error
;;; Get the fiber (tangent space) at a point.
(define (tangent-bundle-fiber tb point)
  (let* ([atlas (tangent-bundle-atlas tb)]
         [chart (atlas-find-chart atlas point)])
    (if chart
        (make-tangent-space point chart)
        `(error point-not-in-manifold))))

;;; tangent-bundle-section : TangentBundle × (Point × Chart → Vec) → (Point → TangentVector)
;;; Create a section (vector field) of the tangent bundle.
;;; component-fn takes (point, chart) and returns component vector.
(define (tangent-bundle-section tb component-fn)
  (lambda (point)
    (let* ([atlas (tangent-bundle-atlas tb)]
           [chart (atlas-find-chart atlas point)])
      (if chart
          (make-tangent-vector point chart (component-fn point chart))
          `(error point-not-in-manifold)))))

;;; ============================================================================
;;; Cotangent Bundle
;;; ============================================================================

;;; The cotangent bundle T*M is the disjoint union of all cotangent spaces.

;;; A cotangent bundle is: (cotangent-bundle atlas)
(define (cotangent-bundle? x)
  (and (pair? x)
       (eq? (car x) 'cotangent-bundle)
       (= (length x) 2)
       (atlas? (cotangent-bundle-atlas x))))

;;; cotangent-bundle-atlas : CotangentBundle → Atlas
(define (cotangent-bundle-atlas ctb)
  (list-ref ctb 1))

;;; make-cotangent-bundle : Atlas → CotangentBundle
(define (make-cotangent-bundle atlas)
  (list 'cotangent-bundle atlas))

;;; cotangent-bundle-dim : CotangentBundle → Nat | #f
(define (cotangent-bundle-dim ctb)
  (let ([base-dim (atlas-dim (cotangent-bundle-atlas ctb))])
    (if base-dim (* 2 base-dim) #f)))

;;; cotangent-bundle-fiber : CotangentBundle × Point → CotangentSpace | Error
(define (cotangent-bundle-fiber ctb point)
  (let* ([atlas (cotangent-bundle-atlas ctb)]
         [chart (atlas-find-chart atlas point)])
    (if chart
        (make-cotangent-space point chart)
        `(error point-not-in-manifold))))

;;; cotangent-bundle-section : CotangentBundle × (Point × Chart → Vec) → (Point → CotangentVector)
;;; Create a section (1-form field) of the cotangent bundle.
(define (cotangent-bundle-section ctb component-fn)
  (lambda (point)
    (let* ([atlas (cotangent-bundle-atlas ctb)]
           [chart (atlas-find-chart atlas point)])
      (if chart
          (make-cotangent-vector point chart (component-fn point chart))
          `(error point-not-in-manifold)))))

;;; ============================================================================
;;; Differential of a Scalar Function
;;; ============================================================================

;;; The differential df of a scalar function f: M → R is a 1-form.
;;; In coordinates: df = Σᵢ (∂f/∂xⁱ) dxⁱ

;;; differential : (Vec → Num) × Point × Chart × [Num] → CotangentVector
;;; Compute the differential of a scalar function at a point.
;;; f is the function in coordinate form.
(define (differential f point chart . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))]
         [coords (chart-apply chart point)]
         [n (chart-dim chart)]
         [grad (make-vector n 0)])
    ;; Compute gradient: (∂f/∂xⁱ)
    (do ([i 0 (+ i 1)])
        ((= i n))
        (let ([coords-plus (vec-copy coords)]
              [coords-minus (vec-copy coords)])
          (vector-set! coords-plus i (+ (vector-ref coords i) eps))
          (vector-set! coords-minus i (- (vector-ref coords i) eps))
          (let ([deriv (/ (- (f coords-plus) (f coords-minus)) (* 2 eps))])
            (vector-set! grad i deriv))))
    (make-cotangent-vector point chart grad)))

;;; ============================================================================
;;; Lie Bracket of Vector Fields
;;; ============================================================================

;;; The Lie bracket [X, Y] of two vector fields measures their non-commutativity.
;;; In coordinates: [X, Y]^k = X^i (∂Y^k/∂x^i) - Y^i (∂X^k/∂x^i)
;;;
;;; Geometrically, [X, Y] measures the failure of flows along X and Y to commute.
;;; If [X, Y] = 0, the flows commute (you get to the same place regardless of order).

;;; lie-bracket : (Point → TangentVector) × (Point → TangentVector) × Point × Chart × [Num]
;;;               → TangentVector
;;; Compute the Lie bracket [X, Y] at a point.
;;; X and Y are vector fields (functions from points to tangent vectors).
;;;
;;; Performance: O(N) field evaluations where N is the manifold dimension.
;;; We compute all partial derivatives first, then assemble the bracket algebraically.
(define (lie-bracket X Y point chart . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))]
         [coords (chart-apply chart point)]
         [n (chart-dim chart)]
         ;; Get X and Y components at the point (with chart compatibility check)
         [X-at-p (X point)]
         [Y-at-p (Y point)]
         [X-comps (tangent-vector-components
                   (if (eq? (chart-name (tangent-vector-chart X-at-p)) (chart-name chart))
                       X-at-p
                       (tangent-change-chart X-at-p chart)))]
         [Y-comps (tangent-vector-components
                   (if (eq? (chart-name (tangent-vector-chart Y-at-p)) (chart-name chart))
                       Y-at-p
                       (tangent-change-chart Y-at-p chart)))]
         ;; Pre-allocate derivative storage: dX[i] and dY[i] are n-vectors
         ;; dX[i][k] = ∂X^k/∂x^i
         [dX-matrix (make-vector n #f)]
         [dY-matrix (make-vector n #f)]
         ;; Scratch buffers for coordinate perturbation (reuse to reduce allocation)
         [coords-plus (vec-copy coords)]
         [coords-minus (vec-copy coords)]
         ;; Result components
         [result (make-vector n 0)])

    ;; Phase 1: Compute all partial derivatives (O(N) field evaluations)
    ;; For each direction i, perturb and evaluate X and Y once
    (do ([i 0 (+ i 1)])
        ((= i n))
        ;; Set up perturbed coordinates
        (let ([ci (vector-ref coords i)])
          (vector-set! coords-plus i (+ ci eps))
          (vector-set! coords-minus i (- ci eps)))
        ;; Get perturbed points
        (let* ([p-plus (chart-apply-inverse chart coords-plus)]
               [p-minus (chart-apply-inverse chart coords-minus)]
               ;; Evaluate fields at perturbed points
               [X-plus (tangent-vector-components (X p-plus))]
               [X-minus (tangent-vector-components (X p-minus))]
               [Y-plus (tangent-vector-components (Y p-plus))]
               [Y-minus (tangent-vector-components (Y p-minus))]
               ;; Compute derivative vectors: dX/dx^i and dY/dx^i
               [inv-2eps (/ 1.0 (* 2 eps))]
               [dXi (make-vector n 0)]
               [dYi (make-vector n 0)])
          ;; Compute each component of the derivative
          (do ([k 0 (+ k 1)])
              ((= k n))
              (vector-set! dXi k (* inv-2eps (- (vector-ref X-plus k) (vector-ref X-minus k))))
              (vector-set! dYi k (* inv-2eps (- (vector-ref Y-plus k) (vector-ref Y-minus k)))))
          ;; Store in matrices
          (vector-set! dX-matrix i dXi)
          (vector-set! dY-matrix i dYi))
        ;; Reset coords for next iteration
        (let ([ci (vector-ref coords i)])
          (vector-set! coords-plus i ci)
          (vector-set! coords-minus i ci)))

    ;; Phase 2: Compute bracket components (purely algebraic, O(N²) arithmetic ops)
    ;; [X, Y]^k = Σ_i (X^i * ∂Y^k/∂x^i - Y^i * ∂X^k/∂x^i)
    (do ([k 0 (+ k 1)])
        ((= k n))
        (let ([bracket-k 0])
          (do ([i 0 (+ i 1)])
              ((= i n))
              (let ([Xi (vector-ref X-comps i)]
                    [Yi (vector-ref Y-comps i)]
                    [dYk-dxi (vector-ref (vector-ref dY-matrix i) k)]
                    [dXk-dxi (vector-ref (vector-ref dX-matrix i) k)])
                (set! bracket-k (+ bracket-k (- (* Xi dYk-dxi) (* Yi dXk-dxi))))))
          (vector-set! result k bracket-k)))

    (make-tangent-vector point chart result)))

;;; lie-bracket-field : (Point → TangentVector) × (Point → TangentVector) × Chart × [Num]
;;;                     → (Point → TangentVector)
;;; Create a new vector field that is the Lie bracket of X and Y.
(define (lie-bracket-field X Y chart . epsilon-arg)
  (let ([eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))])
    (lambda (point)
      (lie-bracket X Y point chart eps))))

;;; ============================================================================
;;; REPL Interface
;;; ============================================================================

(printf "tangent.ss loaded — Tangent and Cotangent Spaces\n")
(printf "  Tangent Vectors:\n")
(printf "    (make-tangent-vector point chart components)\n")
(printf "    (tangent-add tv1 tv2), (tangent-scale k tv)\n")
(printf "    (tangent-change-chart tv new-chart)\n")
(printf "  Cotangent Vectors:\n")
(printf "    (make-cotangent-vector point chart components)\n")
(printf "    (covector-apply cv tv) - pairing ⟨ω, v⟩\n")
(printf "    (cotangent-change-chart cv new-chart)\n")
(printf "  Pushforward/Pullback:\n")
(printf "    (pushforward f tv src-chart tgt-chart)\n")
(printf "    (pullback-at f cv src-point src-chart tgt-chart)\n")
(printf "  Spaces and Bundles:\n")
(printf "    (make-tangent-space point chart)\n")
(printf "    (make-tangent-bundle atlas)\n")
(printf "    (differential f point chart) - df as 1-form\n")
(printf "  Lie Bracket:\n")
(printf "    (lie-bracket X Y point chart) - [X,Y] at point\n")
(printf "    (lie-bracket-field X Y chart) - [X,Y] as vector field\n")
