# Physics Engine Guide

A comprehensive guide to The Fold's physics simulation system.

## Overview

The physics engine provides production-grade rigid body simulation with:
- **2D and 3D** classical physics
- **Differentiable** physics for gradient-based optimization
- **Particle systems** for visual effects
- **Impulse-based** collision response
- **Constraint systems** for joints and connections

## Quick Start

```scheme
;; Load the 2D physics module
(load "lattice/physics/classical/world.ss")

;; Create a world with gravity pointing down
(define world (make-world (vec2 0 9.8)))

;; Add a bouncing ball
(world-add-entity! world
  (make-circle-entity 'ball (vec2 100 50) 20 1.0))

;; Add ground
(world-add-entity! world
  (make-ground 'floor (vec2 320 450) 600 20))

;; Simulation loop (60 FPS)
(let loop ()
  (world-step! world (/ 1 60))
  ;; ... render ...
  (loop))
```

## Core Concepts

### Bodies

A **body** represents a physical object with position, velocity, and mass.

**Body2D** (linear motion only):
```scheme
(define ball (make-body-2d
  (vec2 100 50)   ; position
  (vec2 0 0)      ; velocity
  1.0))           ; mass (0 = static)

(body-pos ball)       ; -> (vec2 100 50)
(body-vel ball)       ; -> (vec2 0 0)
(body-mass ball)      ; -> 1.0
(body-static? ball)   ; -> #f
```

**RigidBody2D** (with rotation):
```scheme
(define box (make-rigid-body
  (vec2 200 100)   ; position
  (vec2 0 0)       ; velocity
  0.0              ; angle (radians)
  0.0              ; angular velocity
  2.0              ; mass
  (rectangle-inertia 2.0 40 20)))  ; moment of inertia

(rigid-body-angle box)        ; -> 0.0
(rigid-body-angular-vel box)  ; -> 0.0
```

### Shapes

Shapes define collision geometry:

```scheme
;; Axis-Aligned Bounding Box
(define rect (make-aabb (vec2 0 0) (vec2 100 50)))

;; Circle
(define circle (make-circle (vec2 50 50) 25))

;; Polygon (convex)
(define triangle (make-polygon
  (list (vec2 0 0) (vec2 100 0) (vec2 50 86))))

;; Convenience constructors
(define box (make-box 100 50))  ; centered at origin
(define hex (make-regular-polygon 6 30))  ; hexagon
```

### Materials

Materials control collision response:

```scheme
(define bouncy (make-material
  0.9    ; restitution (bounciness, 0-1)
  0.3    ; static friction
  0.2))  ; dynamic friction

;; Predefined materials
rubber-material  ; high bounce
metal-material   ; low bounce, low friction
wood-material    ; medium
ice-material     ; very low friction
```

### Entities

An **entity** combines body + shape + material:

```scheme
(define player (make-entity
  'player              ; id
  player-body          ; physics body
  (make-circle 20)     ; collision shape
  rubber-material      ; material
  player-data))        ; user data (any)

;; Convenience constructors
(make-circle-entity 'ball pos radius mass)
(make-box-entity 'crate pos width height mass)
(make-static-circle 'obstacle pos radius)
(make-ground 'floor pos width height)
```

### World

The **world** manages all physics objects:

```scheme
(define world (make-world (vec2 0 9.8)))  ; gravity

;; Entity management
(world-add-entity! world entity)
(world-remove-entity! world 'entity-id)
(world-get-entity world 'entity-id)

;; Stepping
(world-step! world dt)  ; dt in seconds

;; Queries
(world-query-aabb world min-corner max-corner)
(world-query-point world point)
(world-raycast-closest world origin direction max-dist)
```

## Collision Detection

### Collision Flow

1. **Broad Phase**: Spatial hash culls distant pairs
2. **Narrow Phase**: Shape-specific collision tests
3. **Manifold Generation**: Contact point + normal + depth

### Shape Collision Tests

```scheme
;; Point-in-shape tests
(point-in-aabb? point aabb)
(point-in-circle? point circle)
(point-in-polygon? point polygon)

;; Shape-shape collision (returns manifold or #f)
(aabb-manifold aabb1 aabb2)
(circle-manifold circle1 circle2)
(circle-aabb-manifold circle aabb)
(polygon-manifold poly1 poly2)  ; uses SAT
```

### Manifold Structure

```scheme
(manifold-normal m)       ; collision normal (Vec2)
(manifold-penetration m)  ; overlap depth (Number)
(manifold-contact m)      ; contact point (Vec2)
```

## Collision Response

Impulse-based resolution:

```scheme
;; Resolve a collision
(resolve-collision manifold material-a material-b)

;; With rotation support
(resolve-with-rotation-and-correction manifold mat-a mat-b)

;; Manual impulse application
(apply-impulse body impulse)
(apply-impulse-at-point rigid-body impulse world-point)
```

## Constraints

### Joint Types

```scheme
;; Distance constraint (fixed length)
(make-distance-constraint
  'rope entity-a entity-b
  distance anchor-a anchor-b)

;; Revolute joint (rotation around point)
(make-revolute-constraint
  'hinge entity-a entity-b
  world-anchor)

;; Spring constraint
(make-spring-constraint
  'spring entity-a entity-b
  rest-length stiffness damping)

;; Weld constraint (fixed relative pose)
(make-weld-constraint
  'weld entity-a entity-b
  relative-angle)
```

### Using Constraints

```scheme
(world-add-constraint! world constraint)
(world-remove-constraint! world 'constraint-id)
```

## Numerical Integration

Available methods:

| Method | Order | Stability | Use Case |
|--------|-------|-----------|----------|
| Euler | 1st | Poor | Not recommended |
| Symplectic | 1st | Good | Fast games |
| Verlet | 2nd | Excellent | Default choice |
| RK4 | 4th | Good | High accuracy |

```scheme
;; Direct integration (rarely needed)
(integrate-body-euler body force-fn dt)
(integrate-body-symplectic body force-fn dt)
(integrate-body-verlet body force-fn dt)
(integrate-body-rk4 body force-fn t dt)
```

## Ray Casting

```scheme
(define ray (make-ray2 origin direction max-distance))

;; Against individual shapes
(ray-aabb-intersect ray aabb)
(ray-circle-intersect ray circle)
(ray-polygon-intersect ray polygon)

;; World query (returns closest hit)
(define hit (world-raycast-closest world origin dir dist))
(when hit
  (hit-entity hit)    ; entity that was hit
  (hit-point hit)     ; world position of hit
  (hit-normal hit)    ; surface normal at hit
  (hit-distance hit)) ; distance from origin
```

## Particle Systems

For visual effects (explosions, trails, weather):

```scheme
(load "lattice/physics/classical/particles.ss")

;; Create an emitter
(define fire (make-fire-emitter (vec2 100 400)))

;; Create force fields
(define forces (combine-fields
  (gravity-field (vec2 0 -50))     ; upward (fire rises)
  (turbulence-field 20 5 12345)))  ; noise

;; Create system
(define system (make-particle-system
  (list fire)    ; emitters
  '()            ; initial particles
  forces))       ; force field

;; Step the system
(let ([rng (make-pcg 12345 1)])
  (particle-system-step system dt rng))
```

### Force Fields

```scheme
(gravity-field (vec2 0 9.8))           ; constant gravity
(wind-field (vec2 1 0) 5.0)            ; directional wind
(drag-field 0.1)                       ; air resistance
(attractor-field center strength 2)    ; point attractor
(vortex-field center strength 1)       ; swirling force
(turbulence-field scale strength seed) ; noise-based
```

### Burst Effects

```scheme
;; Explosion burst
(explosion-burst (vec2 200 200) 50 rng)  ; 50 particles

;; Custom burst
(burst pos spread speed lifetime count color rng)
```

## Differentiable Physics

For optimization and learning:

```scheme
(load "lattice/physics/diff/optimize.ss")

;; Optimize initial velocity to hit target
(define result (optimize-initial-velocity
  (vec2 0 0)      ; start position
  (vec2 10 5)     ; target position
  (vec2 0 -9.8)   ; gravity
  (/ 1 60)        ; timestep
  100             ; simulation steps
  0.1             ; learning rate
  100))           ; optimization iterations
```

## 3D Physics

The 3D physics module mirrors the 2D API:

```scheme
(load "lattice/physics/classical3d/world3d.ss")

(define world (make-world3d (vec3 0 -9.8 0)))

(world3d-add-entity! world
  (make-sphere-entity 'ball (vec3 0 10 0) 1.0 1.0))

(world3d-step! world 0.016)
```

Key differences:
- Rotation uses quaternions
- Angular velocity is Vec3
- Inertia is 3x3 tensor (Mat3)
- GJK/EPA for convex collision

## Performance Tips

1. **Use spatial hashing** - Already enabled by default
2. **Prefer simple shapes** - Circles > Polygons
3. **Fixed timestep** - Use accumulator pattern
4. **Limit iterations** - Balance accuracy vs speed
5. **Sleep inactive bodies** - Reduce computation

## Testing

Run the physics test suite:

```bash
scheme --script lattice/physics/classical/test-physics.ss
```

Individual test files:
- `test-integrators.ss` - Numerical methods
- `test-collision-detection.ss` - Shape tests
- `test-collision-response.ss` - Impulse calculation
- `test-world.ss` - World stepping
- `test-constraints.ss` - Joint constraints
- `test-particles.ss` - Particle system

## API Reference

### Key Modules

| Module | Purpose |
|--------|---------|
| `integrators.ss` | Body types, numerical integration |
| `rigid-body.ss` | Rigid body with rotation |
| `collision-detection.ss` | Shapes, collision tests |
| `collision-response.ss` | Impulse resolution |
| `world.ss` | Physics world coordinator |
| `constraints.ss` | Joint types |
| `raycasting.ss` | Ray queries |
| `particles.ss` | Particle emitters |
