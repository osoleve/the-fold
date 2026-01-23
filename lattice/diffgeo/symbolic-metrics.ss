(load "lattice/diffgeo/curvature.ss")

(doc 'module 'symbolic-metrics)
(doc 'description "Exact symbolic metric derivatives for standard coordinate systems")
(doc 'layer 'lattice)
(doc 'purity 'total)
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
  (make-metric-symbolic
   chart
   ;; metric-fn: (r, θ) → g
   (lambda (coords)
     (let ([r (vector-ref coords 0)])
       (matrix-from-lists (list (list 1 0)
                                (list 0 (* r r))))))
   ;; deriv-fn: (r, θ) → [∂g/∂r, ∂g/∂θ]
   (lambda (coords)
     (let* ([r (vector-ref coords 0)]
            [dg (make-vector 2 #f)])
       ;; ∂g/∂r
       (vector-set! dg 0
                    (matrix-from-lists (list (list 0 0)
                                             (list 0 (* 2 r)))))
       ;; ∂g/∂θ = 0
       (vector-set! dg 1
                    (matrix-from-lists (list (list 0 0)
                                             (list 0 0))))
       dg))))

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
  (make-metric-symbolic
   chart
   ;; metric-fn: (r, θ, φ) → g
   (lambda (coords)
     (let* ([r (vector-ref coords 0)]
            [theta (vector-ref coords 1)]
            [r2 (* r r)]
            [sin-theta (sin theta)]
            [r2-sin2 (* r2 sin-theta sin-theta)])
       (matrix-from-lists (list (list 1 0 0)
                                (list 0 r2 0)
                                (list 0 0 r2-sin2)))))
   ;; deriv-fn: (r, θ, φ) → [∂g/∂r, ∂g/∂θ, ∂g/∂φ]
   (lambda (coords)
     (let* ([r (vector-ref coords 0)]
            [theta (vector-ref coords 1)]
            [r2 (* r r)]
            [sin-theta (sin theta)]
            [cos-theta (cos theta)]
            [sin2-theta (* sin-theta sin-theta)]
            [dg (make-vector 3 #f)])
       ;; ∂g/∂r
       (vector-set! dg 0
                    (matrix-from-lists
                     (list (list 0 0 0)
                           (list 0 (* 2 r) 0)
                           (list 0 0 (* 2 r sin2-theta)))))
       ;; ∂g/∂θ = diag(0, 0, r²·2sinθcosθ)
       (vector-set! dg 1
                    (matrix-from-lists
                     (list (list 0 0 0)
                           (list 0 0 0)
                           (list 0 0 (* r2 2 sin-theta cos-theta)))))
       ;; ∂g/∂φ = 0
       (vector-set! dg 2
                    (matrix-from-lists
                     (list (list 0 0 0)
                           (list 0 0 0)
                           (list 0 0 0))))
       dg))))

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
  (make-metric-symbolic
   chart
   ;; metric-fn: (ρ, φ, z) → g
   (lambda (coords)
     (let ([rho (vector-ref coords 0)])
       (matrix-from-lists (list (list 1 0 0)
                                (list 0 (* rho rho) 0)
                                (list 0 0 1)))))
   ;; deriv-fn: (ρ, φ, z) → [∂g/∂ρ, ∂g/∂φ, ∂g/∂z]
   (lambda (coords)
     (let* ([rho (vector-ref coords 0)]
            [dg (make-vector 3 #f)])
       ;; ∂g/∂ρ
       (vector-set! dg 0
                    (matrix-from-lists
                     (list (list 0 0 0)
                           (list 0 (* 2 rho) 0)
                           (list 0 0 0))))
       ;; ∂g/∂φ = 0
       (vector-set! dg 1
                    (matrix-from-lists
                     (list (list 0 0 0)
                           (list 0 0 0)
                           (list 0 0 0))))
       ;; ∂g/∂z = 0
       (vector-set! dg 2
                    (matrix-from-lists
                     (list (list 0 0 0)
                           (list 0 0 0)
                           (list 0 0 0))))
       dg))))

;;; ----------------------------------------------------------------------------
;;; Euclidean Metric: g = I (identity)
;;; All derivatives are zero
;;; ----------------------------------------------------------------------------

(define (make-euclidean-metric-exact chart)
  (doc 'export #t)
  (doc 'type '(-> Chart MetricSymbolic))
  (doc 'description "Euclidean metric with exact (zero) derivatives")
  (let ([n (chart-dim chart)])
    (make-metric-symbolic
     chart
     ;; metric-fn: identity matrix
     (lambda (coords)
       (make-identity-matrix n))
     ;; deriv-fn: all zeros
     (lambda (coords)
       (let* ([dg (make-vector n #f)]
              [zero-row (make-list n 0)]
              [zero-rows (make-list n zero-row)]
              [zero-matrix (matrix-from-lists zero-rows)])
         (do ([l 0 (+ l 1)])
             ((= l n) dg)
             (vector-set! dg l zero-matrix)))))))

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
         [dg ((metric-symbolic-deriv-fn m) coords)]
         ;; Allocate result: Γ[k][i][j]
         [gamma (make-vector n #f)])

    ;; Compute each Γ^k_ij = ½ Σ_l g^{kl} (∂_i g_{jl} + ∂_j g_{il} - ∂_l g_{ij})
    (do ([k 0 (+ k 1)])
        ((= k n) gamma)
        (let ([gamma-k (make-vector n #f)])
          (do ([i 0 (+ i 1)])
              ((= i n))
              (let ([gamma-ki (make-vector n 0)])
                (do ([j 0 (+ j 1)])
                    ((= j n))
                    (let ([sum 0])
                      (do ([l 0 (+ l 1)])
                          ((= l n))
                          (let* ([g-kl (matrix-ref g-inv k l)]
                                 [dg-i (vector-ref dg i)]
                                 [dg-j (vector-ref dg j)]
                                 [dg-l (vector-ref dg l)]
                                 [term1 (matrix-ref dg-i j l)]
                                 [term2 (matrix-ref dg-j i l)]
                                 [term3 (matrix-ref dg-l i j)])
                            (set! sum (+ sum (* g-kl (- (+ term1 term2) term3))))))
                      (vector-set! gamma-ki j (* 0.5 sum))))
                (vector-set! gamma-k i gamma-ki)))
          (vector-set! gamma k gamma-k)))))

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
         [gamma (make-vector 2 #f)])
    ;; Γ^r (k=0)
    (let ([gamma-r (make-vector 2 #f)])
      (vector-set! gamma-r 0 (vector 0 0))        ; Γ^r_{r*}
      (vector-set! gamma-r 1 (vector 0 (- r)))    ; Γ^r_{θθ} = -r
      (vector-set! gamma 0 gamma-r))
    ;; Γ^θ (k=1) - note: 1/r diverges at r=0 (coordinate singularity)
    (let ([gamma-theta (make-vector 2 #f)]
          [inv-r (/ 1.0 r)])  ; Let IEEE 754 handle r=0 → +inf.0
      (vector-set! gamma-theta 0 (vector 0 inv-r))    ; Γ^θ_{rθ} = 1/r
      (vector-set! gamma-theta 1 (vector inv-r 0))    ; Γ^θ_{θr} = 1/r
      (vector-set! gamma 1 gamma-theta))
    gamma))

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
         [cot-t (/ cos-t sin-t)]      ; Let IEEE 754 handle sin=0 → ±inf.0
         [gamma (make-vector 3 #f)])
    ;; Γ^r (k=0)
    (let ([gr (make-vector 3 #f)])
      (vector-set! gr 0 (vector 0 0 0))
      (vector-set! gr 1 (vector 0 (- r) 0))                           ; Γ^r_{θθ} = -r
      (vector-set! gr 2 (vector 0 0 (- (* r sin-t sin-t))))           ; Γ^r_{φφ} = -r·sin²θ
      (vector-set! gamma 0 gr))
    ;; Γ^θ (k=1)
    (let ([gt (make-vector 3 #f)])
      (vector-set! gt 0 (vector 0 inv-r 0))                           ; Γ^θ_{rθ} = 1/r
      (vector-set! gt 1 (vector inv-r 0 0))                           ; Γ^θ_{θr} = 1/r
      (vector-set! gt 2 (vector 0 0 (- (* sin-t cos-t))))             ; Γ^θ_{φφ} = -sinθcosθ
      (vector-set! gamma 1 gt))
    ;; Γ^φ (k=2)
    (let ([gp (make-vector 3 #f)])
      (vector-set! gp 0 (vector 0 0 inv-r))                           ; Γ^φ_{rφ} = 1/r
      (vector-set! gp 1 (vector 0 0 cot-t))                           ; Γ^φ_{θφ} = cotθ
      (vector-set! gp 2 (vector inv-r cot-t 0))                       ; Γ^φ_{φr} = 1/r, Γ^φ_{φθ} = cotθ
      (vector-set! gamma 2 gp))
    gamma))

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
  (display "  (spherical-christoffel-exact coords)  Pre-computed spherical Christoffels\n"))
