(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @module symbolic-metrics
;;; @requires curvature iteration
(require 'curvature)
(require 'iteration)

(doc 'module 'symbolic-metrics)
(doc 'description "Exact symbolic metric derivatives for standard coordinate systems")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Eliminates numerical differentiation errors for polar, spherical, cylindrical metrics")
(doc 'note "Provides exact ∂g_{ij}/∂x^l for Christoffel symbol computation")

(doc 'section 'metric-derivative-protocol)
(doc 'note "A metric with symbolic derivatives stores: (metric-symbolic chart metric-fn deriv-fn)")
(doc 'note "deriv-fn: (coords → (Vector Matrix)) returning exact ∂g/∂x^l")

;;; ----------------------------------------------------------------------------
;;; Symbolic Metric Construction
;;; ----------------------------------------------------------------------------

(define (metric-symbolic? m)
  (doc 'export #t)
  (doc 'type '(-> Metric Bool))
  (doc 'description "Check if metric has exact symbolic derivatives")
  (and (pair? m)
       (eq? (car m) 'metric-symbolic)))

(define (make-metric-symbolic chart metric-fn deriv-fn)
  (doc 'export #t)
  (doc 'type '(-> Chart (-> Vec Matrix) (-> Vec (Vector Matrix)) MetricSymbolic))
  (doc 'description "Create a metric with exact symbolic derivatives")
  (doc 'param 'deriv-fn "(coords → Vector of Matrices) where result[l] = ∂g/∂x^l")
  (list 'metric-symbolic chart metric-fn deriv-fn))

(define (metric-symbolic-deriv-fn m)
  (doc 'export #t)
  (doc 'type '(-> MetricSymbolic (-> Vec (Vector Matrix))))
  (doc 'description "Get the symbolic derivative function")
  (list-ref m 3))

;;; Override accessors to work with both metric and metric-symbolic
(define (metric-chart* m)
  (doc 'export #t)
  (if (metric-symbolic? m)
      (list-ref m 1)
      (metric-chart m)))

(define (metric-fn* m)
  (doc 'export #t)
  (if (metric-symbolic? m)
      (list-ref m 2)
      (metric-fn m)))

(define (metric-dim* m)
  (doc 'export #t)
  (chart-dim (metric-chart* m)))

(define (metric-at* m coords)
  (doc 'export #t)
  ((metric-fn* m) coords))

;;; ----------------------------------------------------------------------------
;;; Polar Metric: g = diag(1, r²)
;;;
;;; ∂g/∂r = | 0   0  |    ∂g/∂θ = | 0  0 |
;;;         | 0   2r |            | 0  0 |
;;; ----------------------------------------------------------------------------

(define (make-polar-metric-exact chart)
  (doc 'export #t)
  (doc 'type '(-> Chart MetricSymbolic))
  (doc 'description "Polar metric with exact derivatives")
  (let ([zero-2x2 (diagonal (vector 0 0))])  ; Pre-allocate constant zero matrix
    (make-metric-symbolic
     chart
     ;; metric-fn: (r, θ) → g = diag(1, r²)
     (lambda (coords)
       (let ([r (vector-ref coords 0)])
         (diagonal (vector 1 (* r r)))))
     ;; deriv-fn: (r, θ) → [∂g/∂r, ∂g/∂θ]
     (lambda (coords)
       (let ([r (vector-ref coords 0)])
         (vector (diagonal (vector 0 (* 2 r)))  ; ∂g/∂r = diag(0, 2r)
                 zero-2x2))))))                  ; ∂g/∂θ = 0

;;; ----------------------------------------------------------------------------
;;; Spherical Metric: g = diag(1, r², r²sin²θ)
;;;
;;; ∂g/∂r = diag(0, 2r, 2r·sin²θ)
;;; ∂g/∂θ = diag(0, 0, r²·2sinθ·cosθ) = diag(0, 0, r²·sin(2θ))
;;; ∂g/∂φ = 0
;;; ----------------------------------------------------------------------------

(define (make-spherical-metric-exact chart)
  (doc 'export #t)
  (doc 'type '(-> Chart MetricSymbolic))
  (doc 'description "Spherical metric with exact derivatives")
  (let ([zero-3x3 (diagonal (vector 0 0 0))])  ; Pre-allocate constant zero matrix
    (make-metric-symbolic
     chart
     ;; metric-fn: (r, θ, φ) → g = diag(1, r², r²sin²θ)
     (lambda (coords)
       (let* ([r (vector-ref coords 0)]
              [theta (vector-ref coords 1)]
              [r2 (* r r)]
              [sin-theta (sin theta)])
         (diagonal (vector 1 r2 (* r2 sin-theta sin-theta)))))
     ;; deriv-fn: (r, θ, φ) → [∂g/∂r, ∂g/∂θ, ∂g/∂φ]
     (lambda (coords)
       (let* ([r (vector-ref coords 0)]
              [theta (vector-ref coords 1)]
              [r2 (* r r)]
              [sin-theta (sin theta)]
              [cos-theta (cos theta)]
              [sin2-theta (* sin-theta sin-theta)])
         (vector (diagonal (vector 0 (* 2 r) (* 2 r sin2-theta)))        ; ∂g/∂r
                 (diagonal (vector 0 0 (* r2 2 sin-theta cos-theta)))    ; ∂g/∂θ
                 zero-3x3))))))                                           ; ∂g/∂φ = 0

;;; ----------------------------------------------------------------------------
;;; Cylindrical Metric: g = diag(1, ρ², 1)
;;;
;;; ∂g/∂ρ = diag(0, 2ρ, 0)
;;; ∂g/∂φ = 0
;;; ∂g/∂z = 0
;;; ----------------------------------------------------------------------------

(define (make-cylindrical-metric-exact chart)
  (doc 'export #t)
  (doc 'type '(-> Chart MetricSymbolic))
  (doc 'description "Cylindrical metric with exact derivatives")
  (let ([zero-3x3 (diagonal (vector 0 0 0))])  ; Pre-allocate constant zero matrix
    (make-metric-symbolic
     chart
     ;; metric-fn: (ρ, φ, z) → g = diag(1, ρ², 1)
     (lambda (coords)
       (let ([rho (vector-ref coords 0)])
         (diagonal (vector 1 (* rho rho) 1))))
     ;; deriv-fn: (ρ, φ, z) → [∂g/∂ρ, ∂g/∂φ, ∂g/∂z]
     (lambda (coords)
       (let ([rho (vector-ref coords 0)])
         (vector (diagonal (vector 0 (* 2 rho) 0))  ; ∂g/∂ρ
                 zero-3x3                            ; ∂g/∂φ = 0
                 zero-3x3))))))                      ; ∂g/∂z = 0

;;; ----------------------------------------------------------------------------
;;; Euclidean Metric: g = I (identity)
;;; All derivatives are zero
;;; ----------------------------------------------------------------------------

(define (make-euclidean-metric-exact chart)
  (doc 'export #t)
  (doc 'type '(-> Chart MetricSymbolic))
  (doc 'description "Euclidean metric with exact (zero) derivatives")
  (let* ([n (chart-dim chart)]
         ;; Pre-allocate the zero matrix (shared, immutable by convention)
         [zero-matrix (diagonal (make-vector n 0))])
    (make-metric-symbolic
     chart
     ;; metric-fn: identity matrix
     (lambda (coords)
       (matrix-identity n))
     ;; deriv-fn: return fresh vector pointing to shared zero matrix
     ;; (fresh vector protects against caller mutation of the vector itself)
     (lambda (coords)
       (make-vector n zero-matrix)))))

;;; ----------------------------------------------------------------------------
;;; Christoffel Symbols with Exact Derivatives
;;; ----------------------------------------------------------------------------

(define (christoffel-symbols-exact m coords)
  (doc 'export #t)
  (doc 'type '(-> MetricSymbolic Vec ChristoffelTensor))
  (doc 'description "Compute Christoffel symbols using exact symbolic derivatives")
  (doc 'note "No numerical error from finite differences")
  (let* ([n (metric-dim* m)]
         [g (metric-at* m coords)]
         [g-inv (matrix-inverse g)]
         ;; Get exact derivatives
         [dg ((metric-symbolic-deriv-fn m) coords)])

    ;; Compute each Γ^k_ij = ½ Σ_l g^{kl} (∂_i g_{jl} + ∂_j g_{il} - ∂_l g_{ij})
    (vec-tabulate n k
      (vec-tabulate n i
        (let ([dg-i (vector-ref dg i)])
          (vec-tabulate n j
            (let ([dg-j (vector-ref dg j)])
              (* 0.5
                 (range-fold sum 0 l 0 n
                   (+ sum (* (matrix-ref g-inv k l)
                             (- (+ (matrix-ref dg-i j l)
                                   (matrix-ref dg-j i l))
                                (matrix-ref (vector-ref dg l) i j)))))))))))))

;;; ----------------------------------------------------------------------------
;;; Unified Interface: Auto-detect symbolic vs numerical
;;; ----------------------------------------------------------------------------

(define (christoffel-symbols* m coords . epsilon-arg)
  (doc 'export #t)
  (doc 'type '(-> Metric Vec ChristoffelTensor))
  (doc 'description "Compute Christoffel symbols, using exact derivatives when available")
  (if (metric-symbolic? m)
      (christoffel-symbols-exact m coords)
      (apply christoffel-symbols (cons m (cons coords epsilon-arg)))))

;;; ----------------------------------------------------------------------------
;;; Known Christoffel Symbols (Pre-computed)
;;; ----------------------------------------------------------------------------
;;; For common metrics, we can hard-code the non-zero Christoffel symbols.
;;; This is even faster than computing from derivatives.

(define (polar-christoffel-exact coords)
  (doc 'export #t)
  (doc 'type '(-> Vec ChristoffelTensor))
  (doc 'description "Pre-computed Christoffel symbols for polar coordinates")
  (doc 'note "Non-zero: Γ^r_{θθ} = -r, Γ^θ_{rθ} = Γ^θ_{θr} = 1/r")
  (doc 'note "Returns +inf.0 at r=0 (coordinate singularity)")
  (let* ([r (vector-ref coords 0)]
         [inv-r (/ 1.0 r)])  ; Let IEEE 754 handle r=0 → +inf.0
    (vector
     ;; Γ^r (k=0)
     (vector (vector 0 0)            ; Γ^r_{r*}
             (vector 0 (- r)))       ; Γ^r_{θθ} = -r
     ;; Γ^θ (k=1) - note: 1/r diverges at r=0 (coordinate singularity)
     (vector (vector 0 inv-r)        ; Γ^θ_{rθ} = 1/r
             (vector inv-r 0)))))    ; Γ^θ_{θr} = 1/r

(define (spherical-christoffel-exact coords)
  (doc 'export #t)
  (doc 'type '(-> Vec ChristoffelTensor))
  (doc 'description "Pre-computed Christoffel symbols for spherical coordinates")
  (doc 'note "Non-zero: Γ^r_{θθ}=-r, Γ^r_{φφ}=-r·sin²θ, Γ^θ_{rθ}=1/r, Γ^θ_{φφ}=-sinθcosθ, Γ^φ_{rφ}=1/r, Γ^φ_{θφ}=cotθ")
  (doc 'note "Returns +inf.0 at r=0 and θ=0,π (coordinate singularities)")
  (let* ([r (vector-ref coords 0)]
         [theta (vector-ref coords 1)]
         [sin-t (sin theta)]
         [cos-t (cos theta)]
         [inv-r (/ 1.0 r)]            ; Let IEEE 754 handle r=0 → +inf.0
         [cot-t (/ cos-t sin-t)])     ; Let IEEE 754 handle sin=0 → ±inf.0
    (vector
     ;; Γ^r (k=0)
     (vector (vector 0 0 0)
             (vector 0 (- r) 0)                         ; Γ^r_{θθ} = -r
             (vector 0 0 (- (* r sin-t sin-t))))        ; Γ^r_{φφ} = -r·sin²θ
     ;; Γ^θ (k=1)
     (vector (vector 0 inv-r 0)                         ; Γ^θ_{rθ} = 1/r
             (vector inv-r 0 0)                         ; Γ^θ_{θr} = 1/r
             (vector 0 0 (- (* sin-t cos-t))))          ; Γ^θ_{φφ} = -sinθcosθ
     ;; Γ^φ (k=2)
     (vector (vector 0 0 inv-r)                         ; Γ^φ_{rφ} = 1/r
             (vector 0 0 cot-t)                         ; Γ^φ_{θφ} = cotθ
             (vector inv-r cot-t 0)))))                 ; Γ^φ_{φr} = 1/r, Γ^φ_{φθ} = cotθ

(define (cylindrical-christoffel-exact coords)
  (doc 'export #t)
  (doc 'type '(-> Vec ChristoffelTensor))
  (doc 'description "Pre-computed Christoffel symbols for cylindrical coordinates")
  (doc 'note "Non-zero: Γ^ρ_{φφ} = -ρ, Γ^φ_{ρφ} = Γ^φ_{φρ} = 1/ρ")
  (doc 'note "Returns +inf.0 at ρ=0 (coordinate singularity)")
  (let* ([rho (vector-ref coords 0)]
         [inv-rho (/ 1.0 rho)])       ; Let IEEE 754 handle ρ=0 → +inf.0
    (vector
     ;; Γ^ρ (k=0)
     (vector (vector 0 0 0)
             (vector 0 (- rho) 0)              ; Γ^ρ_{φφ} = -ρ
             (vector 0 0 0))
     ;; Γ^φ (k=1)
     (vector (vector 0 inv-rho 0)              ; Γ^φ_{ρφ} = 1/ρ
             (vector inv-rho 0 0)              ; Γ^φ_{φρ} = 1/ρ
             (vector 0 0 0))
     ;; Γ^z (k=2) - all zero
     (vector (vector 0 0 0)
             (vector 0 0 0)
             (vector 0 0 0)))))

;;; ----------------------------------------------------------------------------
;;; Help
;;; ----------------------------------------------------------------------------

(define (symbolic-metrics-help)
  (doc 'export #t)
  (display "Symbolic Metrics - Exact derivatives for standard coordinate systems\n\n")
  (display "Constructors:\n")
  (display "  (make-polar-metric-exact chart)       Polar with exact derivatives\n")
  (display "  (make-spherical-metric-exact chart)   Spherical with exact derivatives\n")
  (display "  (make-cylindrical-metric-exact chart) Cylindrical with exact derivatives\n")
  (display "  (make-euclidean-metric-exact chart)   Euclidean (flat) metric\n\n")
  (display "Christoffel symbols:\n")
  (display "  (christoffel-symbols* m coords)       Auto-detect symbolic vs numerical\n")
  (display "  (christoffel-symbols-exact m coords)  Use exact derivatives (requires symbolic metric)\n")
  (display "  (polar-christoffel-exact coords)      Pre-computed polar Christoffels\n")
  (display "  (spherical-christoffel-exact coords)  Pre-computed spherical Christoffels\n")
  (display "  (cylindrical-christoffel-exact coords) Pre-computed cylindrical Christoffels\n"))
