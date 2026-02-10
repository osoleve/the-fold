;;; lattice/diffgeo/test-curvature.ss — Tests for Curvature Computations
;;; Tests metric tensors, Christoffel symbols, Riemann tensor, and surface curvatures.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/diffgeo/curvature.ss")

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (approx-equal? a b tol)
  (< (abs (- a b)) tol))

(define (matrix-approx-equal? m1 m2 tol)
  (let ([rows (matrix-rows m1)]
        [cols (matrix-cols m1)])
    (and (= rows (matrix-rows m2))
         (= cols (matrix-cols m2))
         (let loop ([i 0])
           (if (= i rows)
               #t
               (let inner ([j 0])
                 (if (= j cols)
                     (loop (+ i 1))
                     (if (approx-equal? (matrix-ref m1 i j) (matrix-ref m2 i j) tol)
                         (inner (+ j 1))
                         #f))))))))

;;; ============================================================================
;;; Metric Tensor Tests
;;; ============================================================================

(test-group "Metric Tensors"

  (define-test "Euclidean metric is identity"
    (let* ([chart (make-identity-chart 'R3 3)]
           [m (make-euclidean-metric chart)]
           [g (metric-at m (vector 1 2 3))]
           [expected (matrix-identity 3)])
      (assert-true (matrix-approx-equal? g expected 1e-10))))

  (define-test "Polar metric at r=2"
    (let* ([chart (make-polar-chart)]
           [m (make-polar-metric chart)]
           [g (metric-at m (vector 2.0 1.0))])  ; r=2, theta=1
      (assert-true (approx-equal? (matrix-ref g 0 0) 1.0 1e-10))   ; g_rr = 1
      (assert-true (approx-equal? (matrix-ref g 0 1) 0.0 1e-10))   ; g_rθ = 0
      (assert-true (approx-equal? (matrix-ref g 1 0) 0.0 1e-10))   ; g_θr = 0
      (assert-true (approx-equal? (matrix-ref g 1 1) 4.0 1e-10)))) ; g_θθ = r² = 4

  (define-test "Spherical metric at r=2, theta=pi/4"
    (let* ([chart (make-spherical-chart)]
           [m (make-spherical-metric chart)]
           [g (metric-at m (vector 2.0 (/ 3.14159 4) 0.5))])
      (assert-true (approx-equal? (matrix-ref g 0 0) 1.0 1e-10))     ; g_rr = 1
      (assert-true (approx-equal? (matrix-ref g 1 1) 4.0 1e-10))     ; g_θθ = r² = 4
      ;; g_φφ = r² sin²θ = 4 * sin²(π/4) = 4 * 0.5 = 2
      (assert-true (approx-equal? (matrix-ref g 2 2) 2.0 0.01))))

  (define-test "Metric inner product"
    (let* ([chart (make-polar-chart)]
           [m (make-polar-metric chart)]
           [v1 (vector 1 0)]   ; radial direction
           [v2 (vector 0 1)]   ; angular direction
           [coords (vector 3.0 0.5)])  ; r=3
      ;; ⟨v1, v1⟩ = g_rr = 1
      (assert-true (approx-equal? (metric-inner-product m coords v1 v1) 1.0 1e-10))
      ;; ⟨v2, v2⟩ = g_θθ = r² = 9
      (assert-true (approx-equal? (metric-inner-product m coords v2 v2) 9.0 1e-10))
      ;; ⟨v1, v2⟩ = 0
      (assert-true (approx-equal? (metric-inner-product m coords v1 v2) 0.0 1e-10))))

  (define-test "Metric determinant (polar)"
    (let* ([chart (make-polar-chart)]
           [m (make-polar-metric chart)]
           [det (metric-determinant m (vector 3.0 1.0))])  ; r=3
      ;; det(g) = 1 * r² - 0 = r² = 9
      (assert-true (approx-equal? det 9.0 1e-10))))

) ; end Metric Tensors

;;; ============================================================================
;;; Christoffel Symbol Tests
;;; ============================================================================

(test-group "Christoffel Symbols"

  (define-test "Euclidean metric has zero Christoffel symbols"
    (let* ([chart (make-identity-chart 'R2 2)]
           [m (make-euclidean-metric chart)]
           [gamma (christoffel-symbols m (vector 1.0 2.0))])
      ;; All Christoffel symbols should be zero for flat metric
      (assert-true (approx-equal? (christoffel-ref gamma 0 0 0) 0.0 1e-8))
      (assert-true (approx-equal? (christoffel-ref gamma 0 0 1) 0.0 1e-8))
      (assert-true (approx-equal? (christoffel-ref gamma 0 1 0) 0.0 1e-8))
      (assert-true (approx-equal? (christoffel-ref gamma 0 1 1) 0.0 1e-8))
      (assert-true (approx-equal? (christoffel-ref gamma 1 0 0) 0.0 1e-8))
      (assert-true (approx-equal? (christoffel-ref gamma 1 0 1) 0.0 1e-8))
      (assert-true (approx-equal? (christoffel-ref gamma 1 1 0) 0.0 1e-8))
      (assert-true (approx-equal? (christoffel-ref gamma 1 1 1) 0.0 1e-8))))

  (define-test "Polar metric Christoffel symbols"
    ;; For polar coordinates (r, θ) with g_rr=1, g_θθ=r²:
    ;; Non-zero Christoffel symbols:
    ;;   Γ^r_θθ = -r
    ;;   Γ^θ_rθ = Γ^θ_θr = 1/r
    (let* ([chart (make-polar-chart)]
           [m (make-polar-metric chart)]
           [r 2.0]
           [gamma (christoffel-symbols m (vector r 0.5))])
      ;; Γ^r_θθ = -r = -2
      (assert-true (approx-equal? (christoffel-ref gamma 0 1 1) (- r) 0.01))
      ;; Γ^θ_rθ = 1/r = 0.5
      (assert-true (approx-equal? (christoffel-ref gamma 1 0 1) (/ 1.0 r) 0.01))
      ;; Γ^θ_θr = 1/r (symmetric in lower indices)
      (assert-true (approx-equal? (christoffel-ref gamma 1 1 0) (/ 1.0 r) 0.01))
      ;; All others should be zero
      (assert-true (approx-equal? (christoffel-ref gamma 0 0 0) 0.0 0.01))
      (assert-true (approx-equal? (christoffel-ref gamma 0 0 1) 0.0 0.01))
      (assert-true (approx-equal? (christoffel-ref gamma 1 1 1) 0.0 0.01))))

) ; end Christoffel Symbols

;;; ============================================================================
;;; Curvature Tensor Tests
;;; ============================================================================

(test-group "Curvature Tensors"

  (define-test "Euclidean space has zero Riemann tensor"
    (let* ([chart (make-identity-chart 'R2 2)]
           [m (make-euclidean-metric chart)]
           [R (riemann-tensor m (vector 1.0 2.0))])
      ;; All components should be zero
      (assert-true (approx-equal? (riemann-ref R 0 0 0 0) 0.0 1e-6))
      (assert-true (approx-equal? (riemann-ref R 0 0 0 1) 0.0 1e-6))
      (assert-true (approx-equal? (riemann-ref R 0 0 1 0) 0.0 1e-6))
      (assert-true (approx-equal? (riemann-ref R 0 0 1 1) 0.0 1e-6))
      (assert-true (approx-equal? (riemann-ref R 0 1 0 1) 0.0 1e-6))
      (assert-true (approx-equal? (riemann-ref R 1 0 1 0) 0.0 1e-6))))

  (define-test "Euclidean space has zero Ricci tensor"
    (let* ([chart (make-identity-chart 'R2 2)]
           [m (make-euclidean-metric chart)]
           [ric (ricci-tensor m (vector 1.0 2.0))])
      (assert-true (approx-equal? (matrix-ref ric 0 0) 0.0 1e-6))
      (assert-true (approx-equal? (matrix-ref ric 0 1) 0.0 1e-6))
      (assert-true (approx-equal? (matrix-ref ric 1 0) 0.0 1e-6))
      (assert-true (approx-equal? (matrix-ref ric 1 1) 0.0 1e-6))))

  (define-test "Euclidean space has zero scalar curvature"
    (let* ([chart (make-identity-chart 'R2 2)]
           [m (make-euclidean-metric chart)]
           [R (scalar-curvature m (vector 1.0 2.0))])
      (assert-true (approx-equal? R 0.0 1e-6))))

  (define-test "Polar coordinates (flat space) has zero scalar curvature"
    ;; Polar coordinates describe flat R² - curvature should still be zero
    (let* ([chart (make-polar-chart)]
           [m (make-polar-metric chart)]
           [R (scalar-curvature m (vector 2.0 1.0))])
      (assert-true (approx-equal? R 0.0 0.01))))

) ; end Curvature Tensors

;;; ============================================================================
;;; Surface Curvature Tests
;;; ============================================================================

(test-group "Surface Curvatures"

  (define-test "Sphere has constant positive Gaussian curvature"
    ;; Sphere of radius R has K = 1/R²
    (let* ([R 3.0]
           [sphere (make-sphere-surface R)]
           [K (gaussian-curvature sphere 1.0 0.5)])  ; θ=1, φ=0.5
      ;; K = 1/R² = 1/9
      (assert-true (approx-equal? K (/ 1.0 (* R R)) 0.01))))

  (define-test "Sphere has constant mean curvature"
    ;; Sphere of radius R has |H| = 1/R (sign depends on normal orientation)
    (let* ([R 2.0]
           [sphere (make-sphere-surface R)]
           [H (mean-curvature sphere 1.0 0.5)])
      ;; |H| = 1/R = 0.5
      (assert-true (approx-equal? (abs H) (/ 1.0 R) 0.01))))

  (define-test "Sphere principal curvatures are equal"
    ;; For a sphere, |κ₁| = |κ₂| = 1/R (sign depends on normal orientation)
    (let* ([R 2.0]
           [sphere (make-sphere-surface R)]
           [kappas (principal-curvatures sphere 1.0 0.5)]
           [k1 (car kappas)]
           [k2 (cadr kappas)])
      (assert-true (approx-equal? (abs k1) (/ 1.0 R) 0.01))
      (assert-true (approx-equal? (abs k2) (/ 1.0 R) 0.01))))

  (define-test "Saddle surface has negative Gaussian curvature"
    ;; Saddle z = x² - y² has K < 0 (hyperbolic)
    (let* ([saddle (make-saddle-surface)]
           [K (gaussian-curvature saddle 0.5 0.5)])
      (assert-true (< K 0))))

  (define-test "Saddle at origin has K = -4"
    ;; At (0,0), the saddle z = x² - y² has K = -4
    (let* ([saddle (make-saddle-surface)]
           [K (gaussian-curvature saddle 0.0 0.0)])
      (assert-true (approx-equal? K -4.0 0.1))))

  (define-test "Paraboloid has positive Gaussian curvature"
    ;; Paraboloid z = x² + y² has K > 0 (elliptic)
    (let* ([para (make-paraboloid-surface)]
           [K (gaussian-curvature para 0.5 0.5)])
      (assert-true (> K 0))))

  (define-test "Paraboloid at origin has K = 4"
    ;; At (0,0), paraboloid z = x² + y² has K = 4
    (let* ([para (make-paraboloid-surface)]
           [K (gaussian-curvature para 0.0 0.0)])
      (assert-true (approx-equal? K 4.0 0.1))))

  (define-test "Torus has variable Gaussian curvature"
    ;; Torus has positive K on outer edge, negative on inner edge
    (let* ([torus (make-torus-surface 3.0 1.0)]  ; R=3, r=1
           ;; v=0 is outer edge, v=π is inner edge
           [K-outer (gaussian-curvature torus 0.0 0.0)]
           [K-inner (gaussian-curvature torus 0.0 3.14159)])
      (assert-true (> K-outer 0))
      (assert-true (< K-inner 0))))

) ; end Surface Curvatures

;;; ============================================================================
;;; Surface Classification Tests
;;; ============================================================================

(test-group "Surface Classification"

  (define-test "Sphere is elliptic"
    (let* ([sphere (make-sphere-surface 1.0)]
           [class (surface-classify sphere 1.0 0.5)])
      (assert-equal 'elliptic class)))

  (define-test "Saddle is hyperbolic"
    (let* ([saddle (make-saddle-surface)]
           [class (surface-classify saddle 0.5 0.5)])
      (assert-equal 'hyperbolic class)))

  (define-test "Paraboloid is elliptic"
    (let* ([para (make-paraboloid-surface)]
           [class (surface-classify para 0.5 0.5)])
      (assert-equal 'elliptic class)))

) ; end Surface Classification

;;; ============================================================================
;;; First and Second Fundamental Form Tests
;;; ============================================================================

(test-group "Fundamental Forms"

  (define-test "Sphere first fundamental form"
    ;; For sphere r(θ,φ) = R(sin θ cos φ, sin θ sin φ, cos θ)
    ;; E = R², F = 0, G = R² sin²θ
    (let* ([R 2.0]
           [sphere (make-sphere-surface R)]
           [theta 1.0]
           [ff (first-fundamental-form sphere theta 0.5)]
           [E (car ff)]
           [F (cadr ff)]
           [G (caddr ff)]
           [sin-theta (sin theta)])
      (assert-true (approx-equal? E (* R R) 0.01))
      (assert-true (approx-equal? F 0.0 0.01))
      (assert-true (approx-equal? G (* R R sin-theta sin-theta) 0.01))))

  (define-test "Sphere second fundamental form"
    ;; For unit sphere: L = -1, M = 0, N = -sin²θ
    ;; (signs depend on normal orientation)
    (let* ([sphere (make-sphere-surface 1.0)]
           [theta 1.0]
           [ff (second-fundamental-form sphere theta 0.5)]
           [L (car ff)]
           [M (cadr ff)]
           [N (caddr ff)])
      ;; L and N should have same sign, M ≈ 0
      (assert-true (approx-equal? M 0.0 0.01))
      ;; L * N > 0 (both same sign)
      (assert-true (> (* L N) 0))))

) ; end Fundamental Forms

;;; ============================================================================
;;; Gauss-Bonnet Consistency
;;; ============================================================================

(test-group "Gauss-Bonnet"

  (define-test "Sphere total curvature is 4π"
    ;; ∫∫ K dA = 4π for a sphere (Gauss-Bonnet, χ=2)
    ;; We verify numerically by sampling K at several points
    (let* ([sphere (make-sphere-surface 1.0)]
           ;; Sample K at various (θ, φ) - should all be 1
           [K1 (gaussian-curvature sphere 0.5 0.5)]
           [K2 (gaussian-curvature sphere 1.0 1.0)]
           [K3 (gaussian-curvature sphere 1.5 2.0)]
           [K4 (gaussian-curvature sphere 2.0 0.0)])
      ;; All should be approximately 1/R² = 1 (use 0.05 tolerance for numerical derivatives)
      (assert-true (approx-equal? K1 1.0 0.05))
      (assert-true (approx-equal? K2 1.0 0.05))
      (assert-true (approx-equal? K3 1.0 0.05))
      (assert-true (approx-equal? K4 1.0 0.05))))

) ; end Gauss-Bonnet

;;; ============================================================================
;;; Sectional Curvature Tests
;;; ============================================================================

(test-group "Sectional Curvature"

  (define-test "Flat space has zero sectional curvature"
    ;; Euclidean metric in 2D: sectional curvature is 0
    (let* ([cart-chart (make-cartesian-chart)]
           [m (make-euclidean-metric cart-chart)]
           [coords '#(1.0 2.0)]  ; arbitrary point in 2D
           [X '#(1.0 0.0)]       ; unit vectors
           [Y '#(0.0 1.0)]
           [K (sectional-curvature m coords X Y)])
      (assert-true (approx-equal? K 0.0 1e-10))))

  (define-test "Flat space sectional curvature with arbitrary vectors"
    ;; Any plane in flat 2D space has K = 0
    (let* ([cart-chart (make-cartesian-chart)]
           [m (make-euclidean-metric cart-chart)]
           [coords '#(0.0 0.0)]
           [X '#(1.0 2.0)]
           [Y '#(-1.0 1.0)]
           [K (sectional-curvature m coords X Y)])
      (assert-true (approx-equal? K 0.0 1e-10))))

  (define-test "Sectional curvature is symmetric: K(X,Y) = K(Y,X)"
    ;; The plane spanned by X,Y is the same as Y,X
    (let* ([cart-chart (make-cartesian-chart)]
           [m (make-euclidean-metric cart-chart)]
           [coords '#(1.0 1.0)]
           [X '#(1.0 0.5)]
           [Y '#(-0.5 1.0)]
           [K-XY (sectional-curvature m coords X Y)]
           [K-YX (sectional-curvature m coords Y X)])
      (assert-true (approx-equal? K-XY K-YX 1e-12))))

  (define-test "Degenerate plane returns 0"
    ;; Parallel vectors give degenerate plane (denominator → 0)
    (let* ([cart-chart (make-cartesian-chart)]
           [m (make-euclidean-metric cart-chart)]
           [coords '#(0.0 0.0)]
           [X '#(1.0 0.0)]
           [Y '#(2.0 0.0)]  ; parallel to X
           [K (sectional-curvature m coords X Y)])
      (assert-equal K 0)))  ; degenerate case returns 0

  (define-test "Polar coordinates (flat space) has zero sectional curvature"
    ;; Polar coordinates in flat 2D have zero curvature
    ;; (it's just a coordinate change of Euclidean space)
    (let* ([polar-chart (make-polar-chart)]
           [m (make-polar-metric polar-chart)]
           [coords '#(2.0 0.5)]  ; (r=2, θ=0.5)
           [X '#(1.0 0.0)]       ; radial direction
           [Y '#(0.0 1.0)]       ; angular direction
           [K (sectional-curvature m coords X Y)])
      (assert-true (approx-equal? K 0.0 0.1))))

) ; end Sectional Curvature

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
