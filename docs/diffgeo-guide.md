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

## Future Work

- Higher-order differential forms (k-forms)
- Exterior derivative
- Tensor fields and metric tensors
- Geodesics and connections
