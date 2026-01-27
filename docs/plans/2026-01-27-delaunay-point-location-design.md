# Delaunay Point Location Design

**Date:** 2026-01-27
**Issue:** fold-4az (partial)
**Status:** Draft

## Overview

Extend the Delaunay triangulation implementation with point location queries, enabling O(√n) lookup of which triangle contains a given point. This is foundational for interpolation, pathfinding, and future constrained Delaunay work.

## Breaking Change

We simplify the API by having `delaunay-triangulate` return a rich `triangulation` structure instead of a plain list. Adjacency is always computed (O(n), same complexity as triangulation itself).

## Data Structures

### Triangulation Record

```scheme
(triangulation
  points      ; original input points (list of point2)
  triangles   ; list of tri2
  adjacency   ; hash: tri2 → (n12 n23 n31)
              ;   where nXY is neighbor across edge pX→pY, or #f if boundary
  boundary)   ; list of boundary edges (precomputed)
```

### Query Result (Location Record)

Point location returns a `location` record with the containing triangle and barycentric coordinates:

```scheme
(location triangle #(u v w))
;; where: point = u*p1 + v*p2 + w*p3
;; Returns #f if point is outside convex hull
```

## Algorithms

### Building Adjacency (O(n))

```
1. Create edge→triangles hash
2. For each triangle T, for each edge E:
     - Canonicalize E (sort endpoints by coordinates)
     - Add T to edge-map[E]
3. For each triangle T, for each edge E (in order 12, 23, 31):
     - Look up edge-map[E]
     - Neighbor = other triangle sharing E, or #f if boundary
```

### Walking Algorithm (O(√n) expected)

```
locate-point(triangulation, point, hint=first-triangle):
  current = hint
  max-steps = 2 * ceil(sqrt(n)) + 10  ; safety bound
  steps = 0

  loop:
    if steps > max-steps: return #f  ; degenerate case, bail
    steps = steps + 1

    ; Use orient2d for robust edge crossing decisions
    (cross-12, cross-23, cross-31) = edge-orientations(point, current)

    if cross-12 ≥ 0 and cross-23 ≥ 0 and cross-31 ≥ 0:
      ; Point is inside — compute barycentric for final result
      (u, v, w) = barycentric(point, current)
      return (location current u v w)

    ; Cross the edge with negative orientation (point is beyond it)
    if cross-12 < 0:
      next = neighbor-12
    else if cross-23 < 0:
      next = neighbor-23
    else:
      next = neighbor-31

    if next = #f: return #f  ; outside convex hull
    current = next
```

**Key design decisions:**
- Use **orient2d** (robust geometric predicate) for walk decisions, not barycentric coordinates. This prevents numerical instability from causing infinite loops.
- Use **step limit** instead of visited set — O(1) per step vs O(log n) hash operations.
- Compute **barycentric coordinates only once** at the end, for the final result.

**Edge cases:**
- Point exactly on edge: returns one of the two adjacent triangles (non-deterministic).
- Point exactly on vertex: returns one of the incident triangles (non-deterministic).
- These are documented behaviors, not bugs.

## Public API

### Construction

```scheme
(delaunay-triangulate points) → triangulation
;; BREAKING: previously returned list of tri2
```

### Accessors

```scheme
(triangulation? x) → boolean
(triangulation-points tri) → vector of point2 (indexed by point-id)
(triangulation-triangles tri) → list of tri2
(triangulation-boundary tri) → list of edges (CCW winding)
(triangle-neighbors tri triangle) → (n12 n23 n31) or #f
```

### Point Location

```scheme
(locate-point tri point [hint]) → location or #f
;; Find triangle containing point, with barycentric coords
;; Optional hint: triangle to start walk from (for spatial locality)

;; Location record
(location? x) → boolean
(location-triangle loc) → tri2
(location-bary loc) → #(u v w) as vector
```

### Interpolation

```scheme
(interpolate-at tri point values) → number or #f
;; values: vector of numbers, indexed by point position in triangulation-points
;; Returns interpolated value using barycentric coords
```

## Migration

Existing code that uses `delaunay-triangulate`:

```scheme
;; Old
(define tris (delaunay-triangulate points))
(mesh-quality-report tris)

;; New
(define mesh (delaunay-triangulate points))
(mesh-quality-report (triangulation-triangles mesh))
```

## Internal Callers to Update

1. `voronoi.ss` — calls `delaunay-triangulate`; investigate if it can use adjacency directly (Voronoi is Delaunay dual, adjacency = Voronoi edges)
2. `refine-mesh` — operates on triangle list, needs unwrap
3. `mesh-quality-report` — accepts triangle list, needs unwrap
4. `render-mesh-2d` — accepts triangle list, needs unwrap
5. `triangles-to-3d` — accepts triangle list, needs unwrap

## Test Plan

1. **Unit tests for adjacency building**
   - Verify neighbor counts (interior edges have 2, boundary have 1)
   - Verify symmetry (if A neighbors B, then B neighbors A)

2. **Unit tests for point location**
   - Point inside known triangle
   - Point on triangle edge (should return one of the two)
   - Point on vertex (should return one of the incident triangles)
   - Point outside convex hull → #f
   - Hint parameter improves performance for clustered queries

3. **Interpolation tests**
   - Linear function should interpolate exactly
   - Values at vertices should return exact vertex values

4. **Regression tests for callers**
   - voronoi still works
   - refine-mesh still works
   - mesh-quality-report still works

## Future Work

This adjacency infrastructure enables:
- Constrained Delaunay (edge forcing via cavity re-triangulation)
- Mesh traversal algorithms
- More efficient Voronoi computation (walk edges instead of recompute)
