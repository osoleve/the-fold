;;; lattice/diffgeo/curvature.ss — Curvature Computations
;;; @module curvature
;;; @requires prelude matrix vec charts tangent
;;;
;;; Curvature measures for Riemannian manifolds and surfaces.
;;;
;;; This module provides:
;;;   - Metric tensors (Riemannian metrics)
;;;   - Christoffel symbols (connection coefficients)
;;;   - Riemann curvature tensor
;;;   - Ricci tensor and scalar curvature
;;;   - Surface curvatures (Gaussian, mean, principal)
;;;
;;; Mathematical Background:
;;;   A Riemannian metric g assigns an inner product to each tangent space.
;;;   In coordinates: g = Σ_ij g_ij dx^i ⊗ dx^j
;;;
;;;   The Levi-Civita connection is the unique torsion-free metric-compatible
;;;   connection, given by Christoffel symbols:
;;;     Γ^k_ij = ½ g^{kl} (∂_i g_{jl} + ∂_j g_{il} - ∂_l g_{ij})
;;;
;;;   The Riemann curvature tensor measures intrinsic curvature:
;;;     R^l_{ijk} = ∂_j Γ^l_{ik} - ∂_k Γ^l_{ij} + Γ^l_{jm} Γ^m_{ik} - Γ^l_{km} Γ^m_{ij}
;;;
;;; This is Lattice code: pure, uses Core primitives.

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/diffgeo/charts.ss")
(load "lattice/diffgeo/tangent.ss")

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(define *curvature-epsilon* 1e-7)  ; Step size for numerical derivatives

;;; ============================================================================
;;; Helper: Identity Matrix
;;; ============================================================================

;;; Note: prelude.ss defines (identity x) = x, which shadows matrix's identity.
;;; We define our own identity matrix helper here.

;;; make-identity-matrix : Nat → Matrix
;;; Create an n×n identity matrix.
(define (make-identity-matrix n)
  (let ([data (make-vector (* n n) 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) (list 'matrix n n data))
        (vector-set! data (+ (* i n) i) 1))))

;;; ============================================================================
;;; Metric Tensor Representation
;;; ============================================================================

;;; A metric tensor is: (metric chart metric-fn)
;;; - chart: The coordinate chart
;;; - metric-fn: Function (coords → Matrix) returning g_ij at coordinates
;;;
;;; The metric function returns an n×n symmetric positive-definite matrix
;;; where n is the chart dimension.

;;; metric? : Any → Boolean
(define (metric? x)
  (and (pair? x)
       (eq? (car x) 'metric)
       (= (length x) 3)
       (chart? (metric-chart x))
       (procedure? (metric-fn x))))

;;; metric-chart : Metric → Chart
(define (metric-chart m)
  (list-ref m 1))

;;; metric-fn : Metric → (Vec → Matrix)
(define (metric-fn m)
  (list-ref m 2))

;;; metric-dim : Metric → Nat
(define (metric-dim m)
  (chart-dim (metric-chart m)))

;;; ============================================================================
;;; Metric Construction
;;; ============================================================================

;;; make-metric : Chart × (Vec → Matrix) → Metric
;;; Create a metric tensor on a chart.
(define (make-metric chart metric-function)
  (list 'metric chart metric-function))

;;; metric-at : Metric × Vec → Matrix
;;; Evaluate the metric tensor at coordinates.
(define (metric-at m coords)
  ((metric-fn m) coords))

;;; metric-at-point : Metric × Point → Matrix
;;; Evaluate the metric tensor at a point.
(define (metric-at-point m point)
  (let* ([chart (metric-chart m)]
         [coords (chart-apply chart point)])
    (metric-at m coords)))

;;; ============================================================================
;;; Standard Metrics
;;; ============================================================================

;;; make-euclidean-metric : Chart → Metric
;;; The flat Euclidean metric: g_ij = δ_ij (identity matrix).
(define (make-euclidean-metric chart)
  (let ([n (chart-dim chart)])
    (make-metric chart
                 (lambda (coords)
                   (make-identity-matrix n)))))

;;; make-polar-metric : Chart → Metric
;;; The metric for polar coordinates (r, θ):
;;;   ds² = dr² + r² dθ²
;;;   g = | 1   0  |
;;;       | 0   r² |
(define (make-polar-metric chart)
  (make-metric chart
               (lambda (coords)
                 (let ([r (vector-ref coords 0)])
                   (matrix-from-lists (list (list 1 0)
                                            (list 0 (* r r))))))))

;;; make-spherical-metric : Chart → Metric
;;; The metric for spherical coordinates (r, θ, φ):
;;;   ds² = dr² + r² dθ² + r² sin²θ dφ²
;;;   g = | 1   0    0           |
;;;       | 0   r²   0           |
;;;       | 0   0    r²sin²θ     |
(define (make-spherical-metric chart)
  (make-metric chart
               (lambda (coords)
                 (let* ([r (vector-ref coords 0)]
                        [theta (vector-ref coords 1)]
                        [r2 (* r r)]
                        [sin-theta (sin theta)]
                        [r2-sin2 (* r2 sin-theta sin-theta)])
                   (matrix-from-lists (list (list 1 0 0)
                                            (list 0 r2 0)
                                            (list 0 0 r2-sin2)))))))

;;; make-cylindrical-metric : Chart → Metric
;;; The metric for cylindrical coordinates (ρ, φ, z):
;;;   ds² = dρ² + ρ² dφ² + dz²
(define (make-cylindrical-metric chart)
  (make-metric chart
               (lambda (coords)
                 (let ([rho (vector-ref coords 0)])
                   (matrix-from-lists (list (list 1 0 0)
                                            (list 0 (* rho rho) 0)
                                            (list 0 0 1)))))))

;;; ============================================================================
;;; Metric Operations
;;; ============================================================================

;;; metric-inverse : Metric × Vec → Matrix
;;; Compute the inverse metric g^{ij} at coordinates.
(define (metric-inverse m coords)
  (matrix-inverse (metric-at m coords)))

;;; metric-inner-product : Metric × Vec × Vec × Vec → Num
;;; Compute the inner product ⟨v, w⟩_g = g_ij v^i w^j at coordinates.
(define (metric-inner-product m coords v w)
  (let* ([g (metric-at m coords)]
         [gw (matrix-vec-mul g w)])
    (vec-dot v gw)))

;;; metric-norm : Metric × Vec × Vec → Num
;;; Compute the norm ||v||_g = sqrt(⟨v, v⟩_g) at coordinates.
(define (metric-norm m coords v)
  (sqrt (metric-inner-product m coords v v)))

;;; metric-determinant : Metric × Vec → Num
;;; Compute det(g) at coordinates.
(define (metric-determinant m coords)
  (matrix-det (metric-at m coords)))

;;; ============================================================================
;;; Christoffel Symbols
;;; ============================================================================

;;; Christoffel symbols of the second kind: Γ^k_ij
;;; These are the connection coefficients for the Levi-Civita connection.
;;;
;;; Formula: Γ^k_ij = ½ g^{kl} (∂_i g_{jl} + ∂_j g_{il} - ∂_l g_{ij})
;;;
;;; We store them as a 3D array indexed [k][i][j].

;;; christoffel-symbols : Metric × Vec × [Num] → (Vector (Vector (Vector Num)))
;;; Compute all Christoffel symbols at coordinates.
;;; Returns Γ[k][i][j] for all indices.
(define (christoffel-symbols m coords . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [n (metric-dim m)]
         [g (metric-at m coords)]
         [g-inv (matrix-inverse g)]
         ;; Compute partial derivatives of metric: ∂_l g_{ij}
         ;; dg[l] is the matrix of ∂g_{ij}/∂x^l
         [dg (compute-metric-derivatives m coords eps n)]
         ;; Allocate result: Γ[k][i][j]
         [gamma (make-vector n #f)])

    ;; Compute each Γ^k_ij
    (do ([k 0 (+ k 1)])
        ((= k n) gamma)
        (let ([gamma-k (make-vector n #f)])
          (do ([i 0 (+ i 1)])
              ((= i n))
              (let ([gamma-ki (make-vector n 0)])
                (do ([j 0 (+ j 1)])
                    ((= j n))
                    ;; Γ^k_ij = ½ Σ_l g^{kl} (∂_i g_{jl} + ∂_j g_{il} - ∂_l g_{ij})
                    (let ([sum 0])
                      (do ([l 0 (+ l 1)])
                          ((= l n))
                          (let* ([g-kl (matrix-ref g-inv k l)]
                                 [dg-i (vector-ref dg i)]  ; ∂g/∂x^i matrix
                                 [dg-j (vector-ref dg j)]
                                 [dg-l (vector-ref dg l)]
                                 [term1 (matrix-ref dg-i j l)]   ; ∂_i g_{jl}
                                 [term2 (matrix-ref dg-j i l)]   ; ∂_j g_{il}
                                 [term3 (matrix-ref dg-l i j)])  ; ∂_l g_{ij}
                            (set! sum (+ sum (* g-kl (- (+ term1 term2) term3))))))
                      (vector-set! gamma-ki j (* 0.5 sum))))
                (vector-set! gamma-k i gamma-ki)))
          (vector-set! gamma k gamma-k)))))

;;; compute-metric-derivatives : Metric × Vec × Num × Nat → (Vector Matrix)
;;; Helper: compute ∂g_{ij}/∂x^l for all l.
;;; Returns vector of matrices where dg[l] = ∂g/∂x^l.
(define (compute-metric-derivatives m coords eps n)
  (let ([dg (make-vector n #f)]
        [coords-plus (vec-copy coords)]
        [coords-minus (vec-copy coords)])
    (do ([l 0 (+ l 1)])
        ((= l n) dg)
        ;; Perturb coordinate l
        (let ([cl (vector-ref coords l)])
          (vector-set! coords-plus l (+ cl eps))
          (vector-set! coords-minus l (- cl eps))
          (let* ([g-plus (metric-at m coords-plus)]
                 [g-minus (metric-at m coords-minus)]
                 ;; Central difference
                 [dg-l (matrix-scale (/ 1.0 (* 2 eps))
                                     (matrix-sub g-plus g-minus))])
            (vector-set! dg l dg-l))
          ;; Reset
          (vector-set! coords-plus l cl)
          (vector-set! coords-minus l cl)))))

;;; christoffel-ref : Christoffel × Nat × Nat × Nat → Num
;;; Access Γ^k_ij from a Christoffel symbol array.
(define (christoffel-ref gamma k i j)
  (vector-ref (vector-ref (vector-ref gamma k) i) j))

;;; ============================================================================
;;; Riemann Curvature Tensor
;;; ============================================================================

;;; The Riemann tensor measures intrinsic curvature:
;;;   R^l_{ijk} = ∂_j Γ^l_{ik} - ∂_k Γ^l_{ij} + Γ^l_{jm} Γ^m_{ik} - Γ^l_{km} Γ^m_{ij}
;;;
;;; We store it as a 4D array indexed [l][i][j][k].
;;; Note: Different authors use different index conventions!

;;; riemann-tensor : Metric × Vec × [Num] → 4D-Array
;;; Compute the Riemann curvature tensor at coordinates.
;;; Returns R[l][i][j][k] for all indices.
(define (riemann-tensor m coords . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [n (metric-dim m)]
         ;; Compute Christoffel symbols and their derivatives
         [gamma (christoffel-symbols m coords eps)]
         [dgamma (compute-christoffel-derivatives m coords eps n)]
         ;; Allocate result: R[l][i][j][k]
         [R (make-4d-array n)])

    ;; Compute each R^l_{ijk}
    (do ([l 0 (+ l 1)])
        ((= l n) R)
        (do ([i 0 (+ i 1)])
            ((= i n))
            (do ([j 0 (+ j 1)])
                ((= j n))
                (do ([k 0 (+ k 1)])
                    ((= k n))
                    ;; R^l_{ijk} = ∂_j Γ^l_{ik} - ∂_k Γ^l_{ij}
                    ;;           + Σ_m (Γ^l_{jm} Γ^m_{ik} - Γ^l_{km} Γ^m_{ij})
                    (let ([term1 (dgamma-ref dgamma j l i k)]   ; ∂_j Γ^l_{ik}
                          [term2 (dgamma-ref dgamma k l i j)]   ; ∂_k Γ^l_{ij}
                          [sum 0])
                      ;; Product terms
                      (do ([mm 0 (+ mm 1)])
                          ((= mm n))
                          (set! sum (+ sum
                                       (- (* (christoffel-ref gamma l j mm)
                                             (christoffel-ref gamma mm i k))
                                          (* (christoffel-ref gamma l k mm)
                                             (christoffel-ref gamma mm i j))))))
                      (array-4d-set! R l i j k (+ (- term1 term2) sum)))))))))

;;; compute-christoffel-derivatives : Metric × Vec × Num × Nat → 4D-Array
;;; Helper: compute ∂Γ^l_{ij}/∂x^m for all indices.
;;; Returns dgamma[m][l][i][j] = ∂_m Γ^l_{ij}.
(define (compute-christoffel-derivatives m coords eps n)
  (let ([dgamma (make-4d-array n)]
        [coords-plus (vec-copy coords)]
        [coords-minus (vec-copy coords)])
    (do ([mm 0 (+ mm 1)])
        ((= mm n) dgamma)
        ;; Perturb coordinate m
        (let ([cm (vector-ref coords mm)])
          (vector-set! coords-plus mm (+ cm eps))
          (vector-set! coords-minus mm (- cm eps))
          (let ([gamma-plus (christoffel-symbols m coords-plus eps)]
                [gamma-minus (christoffel-symbols m coords-minus eps)])
            ;; Central difference for each component
            (do ([l 0 (+ l 1)])
                ((= l n))
                (do ([i 0 (+ i 1)])
                    ((= i n))
                    (do ([j 0 (+ j 1)])
                        ((= j n))
                        (let ([deriv (/ (- (christoffel-ref gamma-plus l i j)
                                           (christoffel-ref gamma-minus l i j))
                                        (* 2 eps))])
                          (array-4d-set! dgamma mm l i j deriv))))))
          ;; Reset
          (vector-set! coords-plus mm cm)
          (vector-set! coords-minus mm cm)))))

;;; 4D array helpers
(define (make-4d-array n)
  (let ([arr (make-vector n #f)])
    (do ([i 0 (+ i 1)])
        ((= i n) arr)
        (let ([arr-i (make-vector n #f)])
          (do ([j 0 (+ j 1)])
              ((= j n))
              (let ([arr-ij (make-vector n #f)])
                (do ([k 0 (+ k 1)])
                    ((= k n))
                    (vector-set! arr-ij k (make-vector n 0)))
                (vector-set! arr-i j arr-ij)))
          (vector-set! arr i arr-i)))))

(define (array-4d-ref arr i j k l)
  (vector-ref (vector-ref (vector-ref (vector-ref arr i) j) k) l))

(define (array-4d-set! arr i j k l val)
  (vector-set! (vector-ref (vector-ref (vector-ref arr i) j) k) l val))

(define (dgamma-ref dgamma m l i j)
  (array-4d-ref dgamma m l i j))

;;; riemann-ref : Riemann × Nat × Nat × Nat × Nat → Num
;;; Access R^l_{ijk} from a Riemann tensor.
(define (riemann-ref R l i j k)
  (array-4d-ref R l i j k))

;;; ============================================================================
;;; Ricci Tensor
;;; ============================================================================

;;; The Ricci tensor is the contraction of the Riemann tensor:
;;;   R_{ij} = R^k_{ikj} = Σ_k R^k_{ikj}
;;;
;;; It measures how volumes change under geodesic flow.

;;; ricci-tensor : Metric × Vec × [Num] → Matrix
;;; Compute the Ricci tensor at coordinates.
;;; Returns R_{ij} as an n×n matrix.
(define (ricci-tensor m coords . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [n (metric-dim m)]
         [R (riemann-tensor m coords eps)]
         [ric (make-matrix n n 0)])
    ;; R_{ij} = Σ_k R^k_{ikj}
    (do ([i 0 (+ i 1)])
        ((= i n) ric)
        (do ([j 0 (+ j 1)])
            ((= j n))
            (let ([sum 0])
              (do ([k 0 (+ k 1)])
                  ((= k n))
                  (set! sum (+ sum (riemann-ref R k i k j))))
              (matrix-set! ric i j sum))))))

;;; ============================================================================
;;; Scalar Curvature
;;; ============================================================================

;;; The scalar curvature (Ricci scalar) is:
;;;   R = g^{ij} R_{ij}
;;;
;;; It's the simplest curvature invariant.

;;; scalar-curvature : Metric × Vec × [Num] → Num
;;; Compute the scalar curvature at coordinates.
(define (scalar-curvature m coords . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [n (metric-dim m)]
         [ric (ricci-tensor m coords eps)]
         [g-inv (metric-inverse m coords)]
         [R 0])
    ;; R = Σ_{ij} g^{ij} R_{ij}
    (do ([i 0 (+ i 1)])
        ((= i n) R)
        (do ([j 0 (+ j 1)])
            ((= j n))
            (set! R (+ R (* (matrix-ref g-inv i j)
                            (matrix-ref ric i j))))))))

;;; ============================================================================
;;; Surface Curvatures (2D surfaces embedded in R³)
;;; ============================================================================

;;; For a 2D surface parametrized by (u, v) → (x(u,v), y(u,v), z(u,v)):
;;;
;;; First Fundamental Form (induced metric):
;;;   I = E du² + 2F du dv + G dv²
;;;   where E = r_u · r_u, F = r_u · r_v, G = r_v · r_v
;;;
;;; Second Fundamental Form:
;;;   II = L du² + 2M du dv + N dv²
;;;   where L = r_uu · n, M = r_uv · n, N = r_vv · n
;;;   and n is the unit normal
;;;
;;; Principal curvatures κ₁, κ₂ are eigenvalues of the shape operator.
;;; Gaussian curvature K = κ₁κ₂ = (LN - M²)/(EG - F²)
;;; Mean curvature H = (κ₁ + κ₂)/2 = (EN + GL - 2FM)/(2(EG - F²))

;;; A surface parametrization is: (surface param-fn)
;;; - param-fn: (u, v) → (x, y, z) as a vector

;;; surface? : Any → Boolean
(define (surface? x)
  (and (pair? x)
       (eq? (car x) 'surface)
       (= (length x) 2)
       (procedure? (surface-param x))))

;;; surface-param : Surface → (Vec → Vec)
(define (surface-param s)
  (list-ref s 1))

;;; make-surface : (Vec → Vec) → Surface
;;; Create a surface from a parametrization.
;;; The function takes (u, v) as a 2-vector and returns (x, y, z) as a 3-vector.
(define (make-surface param-fn)
  (list 'surface param-fn))

;;; surface-at : Surface × Num × Num → Vec
;;; Evaluate the surface at parameter values.
(define (surface-at surf u v)
  ((surface-param surf) (vector u v)))

;;; first-fundamental-form : Surface × Num × Num × [Num] → (Values Num Num Num)
;;; Compute the first fundamental form coefficients E, F, G.
;;; Returns a list (E F G).
(define (first-fundamental-form surf u v . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [r_u (surface-partial-u surf u v eps)]
         [r_v (surface-partial-v surf u v eps)]
         [E (vec-dot r_u r_u)]
         [F (vec-dot r_u r_v)]
         [G (vec-dot r_v r_v)])
    (list E F G)))

;;; second-fundamental-form : Surface × Num × Num × [Num] → (List Num Num Num)
;;; Compute the second fundamental form coefficients L, M, N.
;;; Returns a list (L M N).
(define (second-fundamental-form surf u v . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [n (surface-normal surf u v eps)]
         [r_uu (surface-partial-uu surf u v eps)]
         [r_uv (surface-partial-uv surf u v eps)]
         [r_vv (surface-partial-vv surf u v eps)]
         [L (vec-dot r_uu n)]
         [M (vec-dot r_uv n)]
         [N (vec-dot r_vv n)])
    (list L M N)))

;;; gaussian-curvature : Surface × Num × Num × [Num] → Num
;;; Compute the Gaussian curvature K = (LN - M²)/(EG - F²).
(define (gaussian-curvature surf u v . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [ff1 (first-fundamental-form surf u v eps)]
         [ff2 (second-fundamental-form surf u v eps)]
         [E (car ff1)] [F (cadr ff1)] [G (caddr ff1)]
         [L (car ff2)] [M (cadr ff2)] [N (caddr ff2)]
         [denom (- (* E G) (* F F))])
    (if (< (abs denom) 1e-15)
        +inf.0  ; Degenerate metric
        (/ (- (* L N) (* M M)) denom))))

;;; mean-curvature : Surface × Num × Num × [Num] → Num
;;; Compute the mean curvature H = (EN + GL - 2FM)/(2(EG - F²)).
(define (mean-curvature surf u v . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [ff1 (first-fundamental-form surf u v eps)]
         [ff2 (second-fundamental-form surf u v eps)]
         [E (car ff1)] [F (cadr ff1)] [G (caddr ff1)]
         [L (car ff2)] [M (cadr ff2)] [N (caddr ff2)]
         [denom (* 2 (- (* E G) (* F F)))])
    (if (< (abs denom) 1e-15)
        +inf.0
        (/ (+ (* E N) (* G L) (- (* 2 F M))) denom))))

;;; principal-curvatures : Surface × Num × Num × [Num] → (List Num Num)
;;; Compute the principal curvatures κ₁, κ₂.
;;; These are eigenvalues of the shape operator (Weingarten map).
;;; κ₁, κ₂ = H ± sqrt(H² - K)
(define (principal-curvatures surf u v . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [K (gaussian-curvature surf u v eps)]
         [H (mean-curvature surf u v eps)]
         [discriminant (- (* H H) K)])
    (if (< discriminant 0)
        ;; Numerical error - clamp to zero
        (list H H)
        (let ([sqrt-disc (sqrt discriminant)])
          (list (+ H sqrt-disc) (- H sqrt-disc))))))

;;; ============================================================================
;;; Surface Derivative Helpers
;;; ============================================================================

;;; surface-partial-u : Surface × Num × Num × Num → Vec
;;; Compute ∂r/∂u at (u, v).
(define (surface-partial-u surf u v eps)
  (let ([r-plus (surface-at surf (+ u eps) v)]
        [r-minus (surface-at surf (- u eps) v)])
    (vec-scale (/ 1.0 (* 2 eps)) (vec-sub r-plus r-minus))))

;;; surface-partial-v : Surface × Num × Num × Num → Vec
;;; Compute ∂r/∂v at (u, v).
(define (surface-partial-v surf u v eps)
  (let ([r-plus (surface-at surf u (+ v eps))]
        [r-minus (surface-at surf u (- v eps))])
    (vec-scale (/ 1.0 (* 2 eps)) (vec-sub r-plus r-minus))))

;;; surface-partial-uu : Surface × Num × Num × Num → Vec
;;; Compute ∂²r/∂u² at (u, v).
(define (surface-partial-uu surf u v eps)
  (let ([r-plus (surface-at surf (+ u eps) v)]
        [r-center (surface-at surf u v)]
        [r-minus (surface-at surf (- u eps) v)])
    (vec-scale (/ 1.0 (* eps eps))
               (vec-add (vec-sub r-plus r-center)
                        (vec-sub r-minus r-center)))))

;;; surface-partial-vv : Surface × Num × Num × Num → Vec
;;; Compute ∂²r/∂v² at (u, v).
(define (surface-partial-vv surf u v eps)
  (let ([r-plus (surface-at surf u (+ v eps))]
        [r-center (surface-at surf u v)]
        [r-minus (surface-at surf u (- v eps))])
    (vec-scale (/ 1.0 (* eps eps))
               (vec-add (vec-sub r-plus r-center)
                        (vec-sub r-minus r-center)))))

;;; surface-partial-uv : Surface × Num × Num × Num → Vec
;;; Compute ∂²r/∂u∂v at (u, v).
(define (surface-partial-uv surf u v eps)
  (let ([r-pp (surface-at surf (+ u eps) (+ v eps))]
        [r-pm (surface-at surf (+ u eps) (- v eps))]
        [r-mp (surface-at surf (- u eps) (+ v eps))]
        [r-mm (surface-at surf (- u eps) (- v eps))])
    (vec-scale (/ 1.0 (* 4 eps eps))
               (vec-sub (vec-sub r-pp r-pm)
                        (vec-sub r-mp r-mm)))))

;;; surface-normal : Surface × Num × Num × Num → Vec
;;; Compute the unit normal n = (r_u × r_v)/|r_u × r_v|.
(define (surface-normal surf u v eps)
  (let* ([r_u (surface-partial-u surf u v eps)]
         [r_v (surface-partial-v surf u v eps)]
         [cross (vec3-cross r_u r_v)]
         [norm (vec-norm cross)])
    (if (< norm 1e-15)
        (vector 0 0 1)  ; Degenerate - return arbitrary normal
        (vec-scale (/ 1.0 norm) cross))))

;;; vec3-cross : Vec × Vec → Vec
;;; 3D cross product.
(define (vec3-cross a b)
  (let ([ax (vector-ref a 0)] [ay (vector-ref a 1)] [az (vector-ref a 2)]
        [bx (vector-ref b 0)] [by (vector-ref b 1)] [bz (vector-ref b 2)])
    (vector (- (* ay bz) (* az by))
            (- (* az bx) (* ax bz))
            (- (* ax by) (* ay bx)))))

;;; ============================================================================
;;; Standard Surfaces
;;; ============================================================================

;;; make-sphere-surface : Num → Surface
;;; Create a sphere of radius R parametrized by (θ, φ).
;;; θ ∈ [0, π] (polar), φ ∈ [0, 2π) (azimuthal)
(define (make-sphere-surface R)
  (make-surface
   (lambda (params)
     (let ([theta (vector-ref params 0)]
           [phi (vector-ref params 1)])
       (vector (* R (sin theta) (cos phi))
               (* R (sin theta) (sin phi))
               (* R (cos theta)))))))

;;; make-torus-surface : Num × Num → Surface
;;; Create a torus with major radius R and minor radius r.
;;; Parameters: (u, v) both in [0, 2π).
(define (make-torus-surface R r)
  (make-surface
   (lambda (params)
     (let ([u (vector-ref params 0)]
           [v (vector-ref params 1)])
       (vector (* (+ R (* r (cos v))) (cos u))
               (* (+ R (* r (cos v))) (sin u))
               (* r (sin v)))))))

;;; make-paraboloid-surface : → Surface
;;; Create a paraboloid z = x² + y² parametrized by (u, v) = (x, y).
(define (make-paraboloid-surface)
  (make-surface
   (lambda (params)
     (let ([u (vector-ref params 0)]
           [v (vector-ref params 1)])
       (vector u v (+ (* u u) (* v v)))))))

;;; make-saddle-surface : → Surface
;;; Create a saddle (hyperbolic paraboloid) z = x² - y².
(define (make-saddle-surface)
  (make-surface
   (lambda (params)
     (let ([u (vector-ref params 0)]
           [v (vector-ref params 1)])
       (vector u v (- (* u u) (* v v)))))))

;;; ============================================================================
;;; Curvature Classification
;;; ============================================================================

;;; classify-point-curvature : Num × Num → Symbol
;;; Classify a point based on its Gaussian and mean curvatures.
(define (classify-point-curvature K H)
  (cond
   [(> K 0) 'elliptic]        ; Both principal curvatures same sign
   [(< K 0) 'hyperbolic]      ; Principal curvatures opposite signs
   [(and (= K 0) (not (= H 0))) 'parabolic]  ; One principal curvature zero
   [else 'flat]))             ; Both principal curvatures zero

;;; surface-classify : Surface × Num × Num × [Num] → Symbol
;;; Classify a surface point.
(define (surface-classify surf u v . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [K (gaussian-curvature surf u v eps)]
         [H (mean-curvature surf u v eps)])
    (classify-point-curvature K H)))

;;; ============================================================================
;;; Sectional Curvature
;;; ============================================================================

;;; Sectional curvature K(X, Y) for a 2-plane spanned by tangent vectors X, Y:
;;;   K(X,Y) = g(R(X,Y)Y, X) / (g(X,X)g(Y,Y) - g(X,Y)²)
;;; In components: R(X,Y)Y = R^l_{ijk} Y^i X^j Y^k, then contracted with X via metric.
;;; The numerator is R_{ijkl} Y^i X^j Y^k X^l where R_{ijkl} = g_{lm} R^m_{ijk}.

;;; sectional-curvature : Metric × Vec × Vec × Vec × [Num] → Num
;;; Compute the sectional curvature for the plane spanned by X and Y at coords.
(define (sectional-curvature m coords X Y . epsilon-arg)
  (let* ([eps (if (null? epsilon-arg) *curvature-epsilon* (car epsilon-arg))]
         [n (metric-dim m)]
         [g (metric-at m coords)]
         [R (riemann-tensor m coords eps)]
         ;; Compute R(X,Y,Y,X) = Σ_{ijkl} R_{ijkl} X^i Y^j Y^k X^l
         ;; where R_{ijkl} = g_{lm} R^m_{ijk}
         [numerator 0]
         [gXX (metric-inner-product m coords X X)]
         [gYY (metric-inner-product m coords Y Y)]
         [gXY (metric-inner-product m coords X Y)]
         [denominator (- (* gXX gYY) (* gXY gXY))])

    ;; Compute R(X,Y,Y,X)
    (do ([i 0 (+ i 1)])
        ((= i n))
        (do ([j 0 (+ j 1)])
            ((= j n))
            (do ([k 0 (+ k 1)])
                ((= k n))
                (do ([l 0 (+ l 1)])
                    ((= l n))
                    ;; R_{ijkl} = Σ_m g_{lm} R^m_{ijk}
                    (let ([R-ijkl 0])
                      (do ([mm 0 (+ mm 1)])
                          ((= mm n))
                          (set! R-ijkl (+ R-ijkl
                                          (* (matrix-ref g l mm)
                                             (riemann-ref R mm i j k)))))
                      ;; R(X,Y,Y,X) = R_{ijkl} Y^i X^j Y^k X^l
                      ;; Note: X^j Y^k (not Y^j Y^k) avoids symmetric/antisymmetric cancellation
                      (set! numerator (+ numerator
                                         (* R-ijkl
                                            (vector-ref Y i)
                                            (vector-ref X j)
                                            (vector-ref Y k)
                                            (vector-ref X l)))))))))

    (if (< (abs denominator) 1e-15)
        0  ; Degenerate plane
        (/ numerator denominator))))

;;; ============================================================================
;;; REPL Interface
;;; ============================================================================

(printf "curvature.ss loaded — Curvature Computations\n")
(printf "  Metric Tensors:\n")
(printf "    (make-metric chart metric-fn)        - Create metric\n")
(printf "    (make-euclidean-metric chart)        - Flat metric\n")
(printf "    (make-polar-metric chart)            - Polar coords metric\n")
(printf "    (make-spherical-metric chart)        - Spherical coords metric\n")
(printf "  Christoffel Symbols:\n")
(printf "    (christoffel-symbols metric coords)  - Connection coefficients\n")
(printf "  Curvature Tensors:\n")
(printf "    (riemann-tensor metric coords)       - Riemann tensor R^l_ijk\n")
(printf "    (ricci-tensor metric coords)         - Ricci tensor R_ij\n")
(printf "    (scalar-curvature metric coords)     - Scalar curvature R\n")
(printf "    (sectional-curvature metric coords X Y) - Sectional curvature\n")
(printf "  Surface Curvatures:\n")
(printf "    (make-surface param-fn)              - Parametric surface\n")
(printf "    (gaussian-curvature surf u v)        - Gaussian curvature K\n")
(printf "    (mean-curvature surf u v)            - Mean curvature H\n")
(printf "    (principal-curvatures surf u v)      - κ₁, κ₂\n")
(printf "    (surface-classify surf u v)          - elliptic/hyperbolic/etc\n")
(printf "  Standard Surfaces:\n")
(printf "    (make-sphere-surface R)              - Sphere of radius R\n")
(printf "    (make-torus-surface R r)             - Torus (R=major, r=minor)\n")
(printf "    (make-saddle-surface)                - Hyperbolic paraboloid\n")
