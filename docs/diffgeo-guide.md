# Differential Geometry Guide

A guide to The Fold's differential geometry module for smooth manifold operations.

## Overview

The `diffgeo` skill provides foundational structures for differential geometry:

- **Coordinate Charts** — Local parameterizations of manifolds
- **Atlases** — Collections of compatible charts covering a manifold
- **Tangent Vectors** — Directions at a point, transform covariantly
- **Cotangent Vectors** — Differential forms (1-forms), transform contravariantly
- **Pushforward/Pullback** — How vectors and forms transform under smooth maps
- **Bundles** — Tangent and cotangent bundles over manifolds

## Quick Start

```scheme
;; Load using the module system (preferred)
(require 'diffgeo/tangent)  ; Also loads charts dependency

;; Or load explicitly
;; (load "lattice/diffgeo/charts.ss")
;; (load "lattice/diffgeo/tangent.ss")

;; Create charts for R²
(define cart (make-cartesian-chart))
(define polar (make-polar-chart))

;; Create an atlas
(define R2-atlas (make-atlas 'R2 (list cart polar)))

;; Create a tangent vector at point (1, 1)
(define p (vector 1.0 1.0))
(define v (make-tangent-vector p cart (vector 2.0 3.0)))  ; 2∂/∂x + 3∂/∂y

;; Transform to polar coordinates
(define v-polar (tangent-change-chart v polar))
```

## Coordinate Charts

A **chart** provides local coordinates on a manifold. Each chart has:
- A domain (where it's valid)
- A coordinate map φ: M → Rⁿ
- An inverse map φ⁻¹: Rⁿ → M

### Standard Charts

```scheme
;; 2D Cartesian (identity on R²)
(define cart (make-cartesian-chart))

;; 2D Polar (r, θ)
;; Domain: R² \ {origin ∪ negative x-axis}
(define polar (make-polar-chart))

;; 3D Spherical (r, θ, φ)
;; Domain: R³ \ {z-axis ∪ negative x-axis half-plane}
(define spherical (make-spherical-chart))

;; 3D Cylindrical (ρ, φ, z)
(define cylindrical (make-cylindrical-chart))

;; Identity chart for Rⁿ
(define R3-chart (make-identity-chart 'R3 3))
```

### Custom Charts

```scheme
;; Create a custom chart
(define my-chart
  (make-chart
    'my-chart      ; name
    2              ; dimension
    (lambda (p)    ; domain predicate
      (> (vector-ref p 0) 0))
    (lambda (p)    ; coordinate map
      (vector (log (vector-ref p 0))
              (vector-ref p 1)))
    (lambda (c)    ; inverse map
      (vector (exp (vector-ref c 0))
              (vector-ref c 1)))))
```

### Chart Operations

```scheme
(chart-contains? polar (vector 1.0 1.0))  ; => #t
(chart-contains? polar (vector 0.0 0.0))  ; => #f (origin excluded)

(chart-apply polar (vector 1.0 1.0))
; => #(1.414... 0.785...)  ; (r, θ) = (√2, π/4)

(chart-apply-inverse polar (vector 2.0 0.0))
; => #(2.0 0.0)  ; (r=2, θ=0) → (x=2, y=0)
```

## Transition Functions

When two charts overlap, a **transition function** converts coordinates between them.

```scheme
;; Create transition: polar → cartesian
(define τ (make-transition polar cart))
(τ (vector 2.0 0.5))  ; Convert (r=2, θ=0.5) to (x, y)

;; Or use directly (with domain verification)
(transition-apply polar cart (vector 2.0 0.5))
```

### Domain Verification

`transition-apply` verifies that the intermediate point lies in the target chart's domain:

```scheme
;; This returns an error because (0,0) is not in polar's domain
(transition-apply cartesian polar (vector 0.0 0.0))
; => (error point-not-in-target-domain polar #(0.0 0.0))
```

Use `make-transition` for a raw (unchecked) transition function when performance matters.

### Jacobians

The **Jacobian** is the matrix of partial derivatives of a transition function:

```scheme
;; Jacobian of polar → cartesian at (r, θ) = (2, π/3)
(define J (transition-jacobian polar cart (vector 2.0 1.047)))
; J = [ cos θ   -r sin θ ]
;     [ sin θ    r cos θ ]

;; Determinant (useful for integration)
(jacobian-determinant J)  ; => r = 2
```

The `jacobian-determinant` function works for any dimension:
- 1×1, 2×2, 3×3: Uses explicit formulas (fast)
- n×n (n > 3): Uses LU decomposition via `matrix-det`

## Tangent Vectors

A **tangent vector** at point p represents a direction on the manifold. In coordinates, it's expressed as components (v¹, v², ..., vⁿ) in the basis {∂/∂x¹, ∂/∂x², ...}.

### Creating Tangent Vectors

```scheme
(define p (vector 1.0 2.0))
(define cart (make-cartesian-chart))

;; Create v = 3∂/∂x + 4∂/∂y at p
(define v (make-tangent-vector p cart (vector 3.0 4.0)))

;; Zero vector
(define zero (tangent-zero p cart))

;; Basis vectors
(define e1 (tangent-basis-vector p cart 0))  ; ∂/∂x
(define e2 (tangent-basis-vector p cart 1))  ; ∂/∂y
```

### Tangent Vector Operations

```scheme
;; Vector space operations (same point, same chart)
(tangent-add v1 v2)
(tangent-sub v1 v2)
(tangent-scale 2.0 v)
(tangent-negate v)
```

### Coordinate Transformation

Tangent vectors transform **covariantly** under coordinate change:
$$v'^i = \sum_j J^i_j v^j$$

```scheme
;; Transform v from cartesian to polar coordinates
(define v-polar (tangent-change-chart v polar))

;; The components change, but v represents the same geometric direction
```

## Cotangent Vectors (1-Forms)

A **cotangent vector** (or 1-form) is a linear functional on tangent vectors. In coordinates, it's expressed as components (ω₁, ω₂, ..., ωₙ) in the basis {dx¹, dx², ...}.

### Creating Cotangent Vectors

```scheme
;; Create ω = 2dx + 3dy at p
(define omega (make-cotangent-vector p cart (vector 2.0 3.0)))

;; Basis 1-forms
(define dx (cotangent-basis-form p cart 0))
(define dy (cotangent-basis-form p cart 1))
```

### The Natural Pairing

The **pairing** ⟨ω, v⟩ evaluates a cotangent vector on a tangent vector:
$$\langle\omega, v\rangle = \sum_i \omega_i v^i$$

This is coordinate-independent.

```scheme
(define omega (make-cotangent-vector p cart (vector 2.0 3.0)))
(define v (make-tangent-vector p cart (vector 4.0 5.0)))

(covector-apply omega v)  ; => 2*4 + 3*5 = 23
```

### Basis Duality

The coordinate bases are dual: ⟨dxⁱ, ∂/∂xʲ⟩ = δⁱⱼ

```scheme
(covector-apply dx e1)  ; => 1
(covector-apply dx e2)  ; => 0
(covector-apply dy e1)  ; => 0
(covector-apply dy e2)  ; => 1
```

### Coordinate Transformation

Cotangent vectors transform **contravariantly**:
$$\omega'_i = \sum_j (J^{-1})^j_i \omega_j$$

```scheme
(define omega-polar (cotangent-change-chart omega polar))
```

## Pushforward and Pullback

Given a smooth map f: M → N:

- **Pushforward** df_p: T_p M → T_{f(p)} N maps tangent vectors forward
- **Pullback** f*: T*_{f(p)} N → T*_p M maps cotangent vectors backward

These are dual: ⟨f*ω, v⟩ = ⟨ω, df(v)⟩

### Pushforward

```scheme
;; f(x, y) = (2x, 3y) — a scaling map
(define f (lambda (v)
            (vector (* 2 (vector-ref v 0))
                    (* 3 (vector-ref v 1)))))

;; Push a tangent vector forward
(define v (make-tangent-vector p cart (vector 1.0 1.0)))
(define df-v (pushforward f v cart cart))
; df maps ∂/∂x + ∂/∂y to 2∂/∂x + 3∂/∂y
```

### Pullback

```scheme
;; Pull a cotangent vector back through f
(define omega-target
  (make-cotangent-vector (f p) cart (vector 1.0 0.0)))

(define f-star-omega
  (pullback-at f omega-target p cart cart))
; f*(dx) = 2dx (Jacobian transpose)
```

## Tangent and Cotangent Spaces

The **tangent space** T_p M at a point p is the vector space of all tangent vectors at p.

```scheme
(define ts (make-tangent-space p cart))

(tangent-space-dim ts)     ; => 2
(tangent-space-basis ts)   ; => list of ∂/∂xⁱ vectors

;; Create a vector in this tangent space
(tangent-space-vector ts (vector 1.0 2.0))
```

Similarly for **cotangent spaces**:

```scheme
(define cs (make-cotangent-space p cart))

(cotangent-space-basis cs)  ; => list of dxⁱ forms
(cotangent-space-form cs (vector 1.0 2.0))
```

## Tangent and Cotangent Bundles

The **tangent bundle** TM is the union of all tangent spaces over a manifold.

```scheme
;; Create tangent bundle over R²
(define TM (make-tangent-bundle R2-atlas))

(tangent-bundle-dim TM)  ; => 4 (2n for n-dimensional base)

;; Get the fiber (tangent space) at a point
(tangent-bundle-fiber TM p)

;; Create a vector field (section of TM)
(define X (tangent-bundle-section TM
            (lambda (pt chart)
              (vector 1.0 0.0))))  ; Constant field ∂/∂x

((X) p)  ; => tangent vector at p
```

## Differential of a Scalar Function

The **differential** df of a scalar function f: M → R is a 1-form:
$$df = \sum_i \frac{\partial f}{\partial x^i} dx^i$$

```scheme
;; f(x, y) = x² + y²
(define f (lambda (v)
            (+ (* (vector-ref v 0) (vector-ref v 0))
               (* (vector-ref v 1) (vector-ref v 1)))))

;; Compute df at point (3, 4)
(define df (differential f (vector 3.0 4.0) cart))
; df = 6dx + 8dy (gradient components)

;; Apply to a tangent vector
(define v (make-tangent-vector (vector 3.0 4.0) cart (vector 1.0 0.0)))
(covector-apply df v)  ; => 6 (directional derivative in x direction)
```

## Lie Bracket

The **Lie bracket** [X, Y] of two vector fields measures their non-commutativity. Geometrically, it describes how the flows along X and Y fail to commute.

In coordinates:
$$[X, Y]^k = X^i \frac{\partial Y^k}{\partial x^i} - Y^i \frac{\partial X^k}{\partial x^i}$$

### Computing the Lie Bracket

```scheme
(define cart (make-cartesian-chart))

;; X = ∂/∂x (constant field)
(define X (lambda (p)
            (make-tangent-vector p cart (vector 1.0 0.0))))

;; Y = x∂/∂y (varies with x)
(define Y (lambda (p)
            (make-tangent-vector p cart
              (vector 0.0 (vector-ref p 0)))))

;; Compute [X, Y] at a point
(define bracket (lie-bracket X Y (vector 1.0 2.0) cart))
;; => ∂/∂y (since ∂x/∂x = 1)
```

### Properties

- **Antisymmetry**: [X, Y] = -[Y, X]
- **Self-bracket is zero**: [X, X] = 0
- **Constant fields commute**: If X and Y are constant, [X, Y] = 0
- **Jacobi identity**: [X, [Y, Z]] + [Y, [Z, X]] + [Z, [X, Y]] = 0

### Creating a Bracket Field

```scheme
;; Create [X, Y] as a vector field (function from points to tangent vectors)
(define bracket-field (lie-bracket-field X Y chart))

;; Evaluate at any point
(bracket-field (vector 3.0 4.0))
```

## Module Reference

### charts.ss

| Function | Description |
|----------|-------------|
| `make-chart` | Create a coordinate chart |
| `chart-apply` | Get coordinates of a point |
| `chart-apply-inverse` | Get point from coordinates |
| `make-transition` | Create transition function (unchecked) |
| `transition-apply` | Apply transition with domain check |
| `transition-jacobian` | Jacobian of transition |
| `jacobian-determinant` | Determinant of Jacobian (any dimension) |
| `make-atlas` | Create atlas from charts |
| `atlas-find-chart` | Find chart containing point |

### tangent.ss

| Function | Description |
|----------|-------------|
| `make-tangent-vector` | Create tangent vector |
| `tangent-change-chart` | Transform to new chart |
| `make-cotangent-vector` | Create cotangent vector (1-form) |
| `covector-apply` | Evaluate ⟨ω, v⟩ |
| `pushforward` | Push tangent vector through map |
| `pullback-at` | Pull cotangent vector back |
| `make-tangent-space` | Create tangent space at point |
| `make-tangent-bundle` | Create bundle over atlas |
| `differential` | Compute df for scalar f |
| `lie-bracket` | Compute [X, Y] at point |
| `lie-bracket-field` | Create [X, Y] as vector field |

## Testing

```bash
scheme --script lattice/diffgeo/test-charts.ss
scheme --script lattice/diffgeo/test-tangent.ss
```

## Riemannian Metrics and Curvature

A **Riemannian metric** assigns an inner product to each tangent space, allowing measurement of lengths and angles on the manifold.

### Metrics

```scheme
(require 'diffgeo/curvature)

;; Create a metric on a chart
(define cart (make-identity-chart 'R2 2))
(define g (make-euclidean-metric cart))

;; Evaluate metric at a point (returns matrix g_ij)
(metric-at g (vector 1.0 2.0))
; => identity matrix for Euclidean metric

;; Standard metrics for curvilinear coordinates
(define polar-chart (make-polar-chart))
(define g-polar (make-polar-metric polar-chart))
; ds² = dr² + r²dθ²

(define spherical-chart (make-spherical-chart))
(define g-spherical (make-spherical-metric spherical-chart))
; ds² = dr² + r²dθ² + r²sin²θ dφ²
```

### Metric Operations

```scheme
;; Inner product ⟨v, w⟩_g = g_ij v^i w^j
(metric-inner-product g coords v w)

;; Norm ||v||_g = √⟨v, v⟩
(metric-norm g coords v)

;; Inverse metric g^{ij}
(metric-inverse g coords)

;; Determinant det(g)
(metric-determinant g coords)
```

### Christoffel Symbols

The **Christoffel symbols** Γ^k_ij are the connection coefficients for the Levi-Civita connection:
$$\Gamma^k_{ij} = \frac{1}{2} g^{kl} \left( \partial_i g_{jl} + \partial_j g_{il} - \partial_l g_{ij} \right)$$

```scheme
;; Compute Christoffel symbols at coordinates
(define gamma (christoffel-symbols g-polar (vector 2.0 0.5)))

;; Access Γ^k_ij
(christoffel-ref gamma k i j)
```

### Curvature Tensors

```scheme
;; Riemann curvature tensor R^l_ijk
(define R (riemann-tensor g-spherical coords))
(riemann-ref R l i j k)

;; Ricci tensor R_ij (contraction of Riemann)
(define Ric (ricci-tensor g-spherical coords))

;; Scalar curvature R = g^{ij} R_ij
(scalar-curvature g-spherical coords)

;; Sectional curvature for plane spanned by X, Y
(sectional-curvature g coords X Y)
```

### Surface Curvatures

For 2D surfaces embedded in R³:

```scheme
;; Create a parametric surface
(define sphere (make-sphere-surface 1.0))  ; Radius 1

;; Evaluate at parameters (θ, φ)
(surface-at sphere 1.0 0.5)  ; => (x, y, z)

;; Gaussian curvature K = κ₁κ₂
(gaussian-curvature sphere 1.0 0.5)  ; => 1.0 for unit sphere

;; Mean curvature H = (κ₁ + κ₂)/2
(mean-curvature sphere 1.0 0.5)

;; Principal curvatures
(principal-curvatures sphere 1.0 0.5)  ; => (κ₁ κ₂)

;; Classification: elliptic, hyperbolic, parabolic, flat
(surface-classify sphere 1.0 0.5)  ; => elliptic
```

Standard surfaces:
- `(make-sphere-surface R)` — Sphere of radius R
- `(make-torus-surface R r)` — Torus (R = major radius, r = minor)
- `(make-paraboloid-surface)` — z = x² + y²
- `(make-saddle-surface)` — z = x² - y² (hyperbolic paraboloid)

## Geodesics

**Geodesics** are locally length-minimizing curves — the generalization of straight lines to curved spaces. They satisfy the geodesic equation:
$$\frac{d^2 x^k}{dt^2} + \Gamma^k_{ij} \frac{dx^i}{dt} \frac{dx^j}{dt} = 0$$

### Geodesic Tracing

```scheme
(require 'diffgeo/geodesics)

;; Trace a geodesic from point p with velocity v for time T
(define states (trace-geodesic metric p v T n-steps))

;; Each state contains position and velocity
(geodesic-state-coords (car states))
(geodesic-state-velocity (car states))

;; Get just the final state (more efficient)
(define final (trace-geodesic-final metric p v T n-steps))
```

### Exponential Map

The **exponential map** exp_p(v) follows the geodesic starting at p with initial velocity v for time 1:

```scheme
;; exp_p(v) — endpoint after unit time
(exp-map metric p v)            ; 100 integration steps
(exp-map metric p v 200)        ; custom step count

;; exp_p(tv) — endpoint at time t
(exp-map-t metric p v t)
```

This maps the tangent space T_p M to the manifold M. Small tangent vectors map to nearby points.

### Logarithm Map

The **logarithm map** log_p(q) is the inverse of exp_p — it finds the initial velocity v such that exp_p(v) = q:

```scheme
;; Find v such that exp_p(v) = q
(define result (log-map metric p q))

;; Returns (ok v) on success, (err message) on failure
(if (eq? (car result) 'ok)
    (let ([v (cadr result)])
      (printf "Initial velocity: ~a\n" v))
    (printf "Failed: ~a\n" (cadr result)))
```

The log map uses a Newton shooting method: iteratively adjust v until exp_p(v) hits the target q.

### Geodesic Distance

The **geodesic distance** is the length of the shortest geodesic connecting two points:

```scheme
;; Distance between p and q
(geodesic-distance metric p q)

;; Arc length of a traced geodesic path
(geodesic-length metric states)
```

For Euclidean space, this equals the straight-line distance. For curved spaces, geodesics curve with the space.

### Geodesic Interpolation

Smoothly interpolate between two points along the connecting geodesic:

```scheme
;; t=0 gives p, t=1 gives q
(geodesic-interpolate metric p q 0.0)   ; => p
(geodesic-interpolate metric p q 0.5)   ; => midpoint
(geodesic-interpolate metric p q 1.0)   ; => q
```

### Parallel Transport

**Parallel transport** moves a vector along a curve while keeping it "parallel" according to the connection. This preserves the metric norm.

```scheme
;; Transport vector V from p along geodesic with velocity v for time T
(parallel-transport metric p v V T)

;; Shorthand: transport to exp_p(v) (T=1)
(parallel-transport-along-geodesic metric p v V)
```

The parallel transport equation is:
$$\frac{dV^k}{dt} + \Gamma^k_{ij} \frac{dx^i}{dt} V^j = 0$$

Key property: the transported tangent vector of a geodesic equals itself (geodesics parallel-transport their own tangent).

### Example: Geodesics on a Sphere

```scheme
;; Create spherical metric
(define chart (make-identity-chart 'sphere 2))
(define metric (make-spherical-metric chart))

;; Start near north pole, shoot toward equator
(define p (vector 1.0 0.1 0.0))      ; r, θ, φ (near pole)
(define v (vector 0.0 1.0 0.0))      ; Pure θ-velocity

;; Trace geodesic (great circle)
(define endpoint (exp-map metric p v 200))
```

### Visualization

```scheme
;; Shoot rays in all directions from a point
;; Useful for visualizing the exponential map
(geodesic-spray metric p n-rays radius n-steps)
; Returns list of endpoints for evenly-spaced initial directions
```

## Module Reference

### charts.ss

| Function | Description |
|----------|-------------|
| `make-chart` | Create a coordinate chart |
| `chart-apply` | Get coordinates of a point |
| `chart-apply-inverse` | Get point from coordinates |
| `make-transition` | Create transition function (unchecked) |
| `transition-apply` | Apply transition with domain check |
| `transition-jacobian` | Jacobian of transition |
| `jacobian-determinant` | Determinant of Jacobian (any dimension) |
| `make-atlas` | Create atlas from charts |
| `atlas-find-chart` | Find chart containing point |

### tangent.ss

| Function | Description |
|----------|-------------|
| `make-tangent-vector` | Create tangent vector |
| `tangent-change-chart` | Transform to new chart |
| `make-cotangent-vector` | Create cotangent vector (1-form) |
| `covector-apply` | Evaluate ⟨ω, v⟩ |
| `pushforward` | Push tangent vector through map |
| `pullback-at` | Pull cotangent vector back |
| `make-tangent-space` | Create tangent space at point |
| `make-tangent-bundle` | Create bundle over atlas |
| `differential` | Compute df for scalar f |
| `lie-bracket` | Compute [X, Y] at point |
| `lie-bracket-field` | Create [X, Y] as vector field |

### curvature.ss

| Function | Description |
|----------|-------------|
| `make-metric` | Create Riemannian metric |
| `make-euclidean-metric` | Flat metric (identity) |
| `make-polar-metric` | ds² = dr² + r²dθ² |
| `make-spherical-metric` | Spherical coordinates metric |
| `metric-at` | Evaluate metric at coordinates |
| `metric-inner-product` | ⟨v, w⟩_g |
| `metric-norm` | \|\|v\|\|_g |
| `christoffel-symbols` | Connection coefficients Γ^k_ij |
| `riemann-tensor` | Riemann curvature R^l_ijk |
| `ricci-tensor` | Ricci tensor R_ij |
| `scalar-curvature` | Ricci scalar R |
| `gaussian-curvature` | Surface Gaussian curvature K |
| `mean-curvature` | Surface mean curvature H |
| `principal-curvatures` | Principal curvatures κ₁, κ₂ |

### geodesics.ss

| Function | Description |
|----------|-------------|
| `trace-geodesic` | Trace geodesic, return all states |
| `trace-geodesic-final` | Trace geodesic, return final state |
| `exp-map` | Exponential map exp_p(v) at t=1 |
| `exp-map-t` | Exponential map at time t |
| `log-map` | Logarithm map (inverse of exp) |
| `parallel-transport` | Transport vector along geodesic |
| `geodesic-distance` | Distance between two points |
| `geodesic-length` | Arc length of traced path |
| `geodesic-interpolate` | Interpolate along geodesic |
| `geodesic-spray` | Shoot rays for visualization |

## Testing

```bash
scheme --script lattice/diffgeo/test-charts.ss
scheme --script lattice/diffgeo/test-tangent.ss
scheme --script lattice/diffgeo/test-curvature.ss
scheme --script lattice/diffgeo/test-geodesics.ss
```

## Future Work

- Higher-order differential forms (k-forms)
- Exterior derivative and de Rham cohomology
- Covariant derivatives on general tensor fields
- Geodesic deviation and Jacobi fields
