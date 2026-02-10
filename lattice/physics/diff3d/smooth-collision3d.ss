(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module smooth-collision3d
;;; @requires prelude vec3 reverse-diff traced-vec3
(require 'prelude)
(require 'vec3)
(require 'reverse-diff)
(require 'traced-vec3)

(doc 'module 'smooth-collision3d)
(doc 'description "Differentiable 3D Collision Primitives - Signed Distance Functions (SDFs) and soft contact models for differentiable 3D physics. SDFs provide smooth, differentiable representations of geometry.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Key insight: Standard collision detection returns discrete contact/no-contact. For differentiable physics, we use continuous penalty forces based on penetration depth, computed via SDFs.")

(doc 'section 'signed-distance-functions)

(doc 'note "An SDF returns: negative values (inside the shape), zero (on the boundary), positive values (outside the shape). The gradient of an SDF at any point gives the direction to the nearest surface point (i.e., the outward normal when normalized).")

;;; traced-sdf-sphere : TracedVec3 × TracedVec3 × Number → TracedValue
;;; SDF for a sphere centered at `center` with given `radius`.
;;; Returns signed distance: negative inside, positive outside.
(define (traced-sdf-sphere point center radius)
  (traced-sub (traced-vec3-smooth-distance point center 1e-8) radius))

;;; traced-sdf-point-3d : TracedVec3 × TracedVec3 → TracedValue
;;; SDF for a point (radius = 0 sphere).
(define (traced-sdf-point-3d query target)
  (traced-vec3-smooth-distance query target 1e-8))

;;; traced-sdf-box-3d : TracedVec3 × TracedVec3 × Vec3 → TracedValue
;;; SDF for an axis-aligned box.
;;;   center: box center
;;;   half-extents: (half-width, half-height, half-depth) - non-traced
;;; Uses smooth approximation for corners and edges.
(define (traced-sdf-box-3d point center half-extents)
  (let* ([local (traced-vec3-sub point center)]
         ;; Distance to each axis boundary
         [dx (traced-sub (traced-smooth-abs (traced-vec3-x local) 1e-8)
                         (vec3-x half-extents))]
         [dy (traced-sub (traced-smooth-abs (traced-vec3-y local) 1e-8)
                         (vec3-y half-extents))]
         [dz (traced-sub (traced-smooth-abs (traced-vec3-z local) 1e-8)
                         (vec3-z half-extents))]
         ;; Outside: sqrt(max(dx,0)² + max(dy,0)² + max(dz,0)²) + min(max(dx,dy,dz),0)
         [dx-pos (traced-softplus dx 20)]  ; smooth max(dx, 0)
         [dy-pos (traced-softplus dy 20)]  ; smooth max(dy, 0)
         [dz-pos (traced-softplus dz 20)]  ; smooth max(dz, 0)
         ;; Distance to corner (when outside)
         [outside-dist (traced-sqrt (traced-add (traced-sq dx-pos)
                                                (traced-add (traced-sq dy-pos)
                                                            (traced-add (traced-sq dz-pos) 1e-16))))]
         ;; Distance to interior (when inside)
         ;; smooth min(max(dx,dy,dz), 0)
         [max-d (traced-softmax2 dx (traced-softmax2 dy dz 20) 20)]
         [inside-dist (traced-softmin2 max-d 0 20)])
        ;; Combine: outside part + inside part
        (traced-add outside-dist inside-dist)))

;;; traced-sdf-capsule-3d : TracedVec3 × TracedVec3 × TracedVec3 × Number → TracedValue
;;; SDF for a 3D capsule (line segment with radius).
;;;   a, b: capsule endpoints
;;;   radius: capsule radius
(define (traced-sdf-capsule-3d point a b radius)
  (let* ([pa (traced-vec3-sub point a)]
         [ba (traced-vec3-sub b a)]
         ;; Project point onto line segment, clamped to [0, 1]
         [ba-len-sq (traced-vec3-magnitude-sq ba)]
         [t-raw (traced-div (traced-vec3-dot pa ba) ba-len-sq)]
         ;; Smooth clamp to [0, 1]
         [t (traced-smooth-clamp t-raw 0 1 20)]
         ;; Closest point on segment
         [closest (traced-vec3-add a (traced-vec3-scale ba t))]
         ;; Distance to closest point, minus radius
         [dist (traced-vec3-smooth-distance point closest 1e-8)])
        (traced-sub dist radius)))

;;; traced-sdf-line-segment-3d : TracedVec3 × TracedVec3 × TracedVec3 → TracedValue
;;; SDF for a 3D line segment (zero-radius capsule).
(define (traced-sdf-line-segment-3d point a b)
  (traced-sdf-capsule-3d point a b 0))

;;; traced-sdf-half-space : TracedVec3 × TracedVec3 × TracedVec3 → TracedValue
;;; SDF for a half-space (infinite plane).
;;;   point-on-plane: any point on the plane boundary
;;;   normal: outward normal (pointing toward positive distance)
(define (traced-sdf-half-space point point-on-plane normal)
  (traced-vec3-dot (traced-vec3-sub point point-on-plane) normal))

;;; ====
;;; Sphere-Sphere Collision (Most Common 3D Case)
;;; ====

;;; traced-sdf-sphere-sphere : TracedVec3 × Number × TracedVec3 × Number → TracedValue
;;; SDF for the Minkowski sum of two spheres (their intersection region).
;;; Returns negative when spheres overlap.
(define (traced-sdf-sphere-sphere pos-a radius-a pos-b radius-b)
  (traced-sub (traced-vec3-smooth-distance pos-a pos-b 1e-8)
              (+ radius-a radius-b)))

;;; traced-sphere-contact : TracedVec3 × Number × TracedVec3 × Number → (Values TracedVec3 TracedVec3 TracedValue)
;;; Compute contact information for two spheres.
;;; Returns: (contact-point, normal, penetration-depth)
;;;   normal: points from A to B
;;;   penetration: positive when overlapping
(define (traced-sphere-contact pos-a radius-a pos-b radius-b)
  (let* ([delta (traced-vec3-sub pos-b pos-a)]
         [dist (traced-vec3-smooth-magnitude delta 1e-8)]
         ;; Normal from A to B (smooth normalize)
         [normal (traced-vec3-smooth-normalize delta 1e-8)]
         ;; Penetration depth (positive when overlapping)
         [penetration (traced-sub (+ radius-a radius-b) dist)]
         ;; Contact point on surface of A (in world space)
         [contact (traced-vec3-add pos-a (traced-vec3-scale normal radius-a))])
        (values contact normal penetration)))

;;; ====
;;; Sphere-Plane Collision
;;; ====

;;; traced-sphere-plane-contact : TracedVec3 × Number × TracedVec3 × TracedVec3 → (Values TracedVec3 TracedVec3 TracedValue)
;;; Contact between sphere and infinite plane.
;;; plane-normal points into the valid region (away from solid).
(define (traced-sphere-plane-contact sphere-pos radius plane-point plane-normal)
  (let* (;; Signed distance from sphere center to plane
         [center-dist (traced-sdf-half-space sphere-pos plane-point plane-normal)]
         ;; Penetration is radius - center_dist (positive when penetrating)
         [penetration (traced-sub radius center-dist)]
         ;; Contact point on sphere surface (toward plane)
         [contact (traced-vec3-sub sphere-pos
                                   (traced-vec3-scale plane-normal radius))])
        ;; Normal points away from plane
        (values contact plane-normal penetration)))

;;; ====
;;; Soft Contact Force Model
;;; ====

;;; Instead of impulse-based collision response, soft contact uses
;;; penalty forces proportional to penetration depth.
;;; This is inherently smooth and differentiable.

;;; traced-soft-contact-force : TracedValue × Number × Number → TracedValue
;;; Compute contact force magnitude from penetration depth.
;;;   penetration: positive when overlapping
;;;   stiffness: contact spring constant (N/m)
;;;   alpha: softplus sharpness (higher = more like hard contact)
;;;
;;; Returns force magnitude (always >= 0).
(define (traced-soft-contact-force penetration stiffness alpha)
  (traced-mul stiffness (traced-softplus penetration alpha)))

;;; traced-soft-contact-force-damped : TracedValue × TracedValue × Number × Number × Number → TracedValue
;;; Contact force with velocity-dependent damping.
;;;   penetration: positive when overlapping
;;;   approach-vel: relative velocity along normal (positive = approaching)
;;;   stiffness: contact spring constant
;;;   damping: damping coefficient
;;;   alpha: softplus sharpness
(define (traced-soft-contact-force-damped penetration approach-vel stiffness damping alpha)
  (let* ([spring-force (traced-soft-contact-force penetration stiffness alpha)]
         ;; Only damp when in contact (penetration > 0)
         [damping-force (traced-mul (traced-mul damping approach-vel)
                                    (traced-softplus penetration (* alpha 2)))])
        ;; Total force = spring + damping
        (traced-add spring-force damping-force)))

;;; ====
;;; Complete Soft Contact Response
;;; ====

;;; traced-sphere-sphere-force : TracedVec3 × TracedVec3 × Number × TracedVec3 × TracedVec3 × Number × Number × Number × Number → TracedVec3
;;; Compute soft contact force between two spheres.
;;; Returns force on body A (negate for force on body B).
(define (traced-sphere-sphere-force pos-a vel-a radius-a pos-b vel-b radius-b stiffness damping alpha)
  (call-with-values
   (lambda () (traced-sphere-contact pos-a radius-a pos-b radius-b))
   (lambda (contact normal penetration)
           (let* ([rel-vel (traced-vec3-sub vel-a vel-b)]
                  [approach-vel (traced-neg (traced-vec3-dot rel-vel normal))]
                  [force-mag (traced-soft-contact-force-damped penetration approach-vel stiffness damping alpha)])
                 (traced-vec3-scale normal (traced-neg force-mag))))))

;;; traced-sphere-plane-force : TracedVec3 × TracedVec3 × Number × TracedVec3 × TracedVec3 × Number × Number × Number → TracedVec3
;;; Compute soft contact force between sphere and plane.
;;; Returns force on the sphere.
(define (traced-sphere-plane-force pos vel radius plane-point plane-normal stiffness damping alpha)
  (call-with-values
   (lambda () (traced-sphere-plane-contact pos radius plane-point plane-normal))
   (lambda (contact _ penetration)
           (let* ([approach-vel (traced-neg (traced-vec3-dot vel plane-normal))]
                  [force-mag (traced-soft-contact-force-damped penetration approach-vel stiffness damping alpha)])
                 (traced-vec3-scale plane-normal force-mag)))))

;;; ====
;;; Ground Plane (Common Special Case)
;;; ====

;;; traced-ground-plane-force-3d : TracedVec3 × TracedVec3 × Number × Number × Number × Number × Number → TracedVec3
;;; Force from horizontal ground plane at y = ground-y.
;;; Ground normal points up (+y).
(define (traced-ground-plane-force-3d pos vel radius ground-y stiffness damping alpha)
  (traced-sphere-plane-force pos vel radius
                             (lift-vec3-const (vec3 0 ground-y 0))
                             (lift-vec3-const (vec3 0 1 0))  ; up
                             stiffness damping alpha))

;;; ====
;;; Contact Normal and Point from SDF
;;; ====

;;; For SDFs, the gradient gives the normal direction.

;;; vec3-gradient : (TracedVec3 → TracedValue) × Vec3 → Vec3
;;; Compute gradient of scalar function f at point p using AD.
;;; The function f should take a TracedVec3 and return a TracedValue.
;;; This function wraps the scalar function to work with the gradient API.
(define (vec3-gradient f p)
  ;; The gradient function passes traced scalars, so we wrap them in a TracedVec3
  ;; and call f, which expects a TracedVec3.
  (gradient (lambda (x y z)
                    ;; x, y, z are already TracedValues from gradient
                    (let* ([pt (traced-vec3 x y z)]
                           [result (f pt)])
                          ;; Return the result (already traced)
                          result))
            (list (vec3-x p) (vec3-y p) (vec3-z p))))

;;; traced-sdf-normal-3d : (TracedVec3 → TracedValue) × Vec3 → Vec3
;;; Compute normal direction from SDF at a point.
;;; Uses automatic differentiation to get gradient.
(define (traced-sdf-normal-3d sdf-fn point)
  (let ([grads (vec3-gradient sdf-fn point)])
       (vec3-normalize (vec3 (car grads) (cadr grads) (caddr grads)))))

;;; traced-sdf-closest-point-3d : (TracedVec3 → TracedValue) × Vec3 → Vec3
;;; Approximate closest point on surface using SDF gradient.
;;; closest = point - sdf(point) * normal
(define (traced-sdf-closest-point-3d sdf-fn point)
  (let* ([tape (make-reverse-tape)]
         [traced-point (lift-vec3 point tape)]
         [sdf-val-traced (sdf-fn traced-point)]
         [sdf-val (traced-value sdf-val-traced)]
         [normal (traced-sdf-normal-3d sdf-fn point)])
        (vec3-sub point (vec3-scale normal sdf-val))))

;;; ====
;;; Soft Friction Model
;;; ====

;;; Coulomb friction is non-smooth (sign function). We use a smooth
;;; approximation based on hyperbolic tangent.

;;; traced-soft-friction-3d : TracedVec3 × TracedValue × Number × Number → TracedVec3
;;; Smooth 3D friction force.
;;;   tangent-vel: velocity component tangent to contact
;;;   normal-force: magnitude of normal force
;;;   mu: friction coefficient
;;;   smoothness: transition smoothness (higher = sharper)
;;;
;;; Uses tanh to smoothly interpolate between -μFn and +μFn.
(define (traced-soft-friction-3d tangent-vel normal-force mu smoothness)
  (let* ([vel-mag (traced-vec3-smooth-magnitude tangent-vel 1e-8)]
         ;; Smooth sign function: tanh(v * smoothness)
         [vel-sign (traced-tanh (traced-mul vel-mag smoothness))]
         ;; Friction magnitude: μ * Fn * sign(v)
         [friction-mag (traced-mul (traced-mul mu normal-force) vel-sign)]
         ;; Friction direction opposes velocity
         [friction-dir (traced-vec3-smooth-normalize (traced-vec3-neg tangent-vel) 1e-8)])
        (traced-vec3-scale friction-dir friction-mag)))

;;; traced-contact-friction-force-3d : TracedVec3 × TracedVec3 × TracedValue × Number × Number → TracedVec3
;;; Complete friction force at a 3D contact.
;;;   rel-vel: relative velocity at contact
;;;   normal: contact normal
;;;   normal-force: magnitude of normal force
;;;   mu: friction coefficient
(define (traced-contact-friction-force-3d rel-vel normal normal-force mu)
  (let* (;; Decompose velocity into normal and tangent
         [vel-normal (traced-vec3-dot rel-vel normal)]
         [vel-tangent (traced-vec3-sub rel-vel
                                       (traced-vec3-scale normal vel-normal))])
        (traced-soft-friction-3d vel-tangent normal-force mu 10)))

;;; ====
;;; Material Properties for Soft Contact
;;; ====

;;; Default stiffness and damping values.
;;; These should be tuned for your simulation's scale and timestep.

(define *default-contact-stiffness-3d* 10000)  ; N/m
(define *default-contact-damping-3d* 100)       ; Ns/m
(define *default-contact-alpha-3d* 100)         ; softplus sharpness
(define *default-friction-mu-3d* 0.5)           ; friction coefficient

;;; make-soft-material-3d : Number × Number × Number × Number → SoftMaterial3D
;;; Create a soft contact material for 3D.
(define (make-soft-material-3d stiffness damping alpha friction)
  (list 'soft-material-3d stiffness damping alpha friction))

;;; soft-material-3d-stiffness : SoftMaterial3D → Number
(define (soft-material-3d-stiffness m) (list-ref m 1))

;;; soft-material-3d-damping : SoftMaterial3D → Number
(define (soft-material-3d-damping m) (list-ref m 2))

;;; soft-material-3d-alpha : SoftMaterial3D → Number
(define (soft-material-3d-alpha m) (list-ref m 3))

;;; soft-material-3d-friction : SoftMaterial3D → Number
(define (soft-material-3d-friction m) (list-ref m 4))

;;; default-soft-material-3d : → SoftMaterial3D
(define (default-soft-material-3d)
  (make-soft-material-3d *default-contact-stiffness-3d*
                         *default-contact-damping-3d*
                         *default-contact-alpha-3d*
                         *default-friction-mu-3d*))

;;; ====
;;; Wall Boundaries (Common for 3D Simulations)
;;; ====

;;; traced-wall-forces-3d : TracedVec3 × TracedVec3 × Number × Vec3 × Vec3 × SoftMaterial3D → TracedVec3
;;; Compute forces from rectangular boundary walls (6 faces).
;;;   min-corner, max-corner: boundary box
(define (traced-wall-forces-3d pos vel radius min-corner max-corner mat)
  (let ([stiffness (soft-material-3d-stiffness mat)]
        [damping (soft-material-3d-damping mat)]
        [alpha (soft-material-3d-alpha mat)])
       ;; -X wall (x = min-x)
       (let* ([neg-x-force (traced-sphere-plane-force
                            pos vel radius
                            (lift-vec3-const min-corner)
                            (lift-vec3-const (vec3 1 0 0))  ; +x direction
                            stiffness damping alpha)]
              ;; +X wall (x = max-x)
              [pos-x-force (traced-sphere-plane-force
                            pos vel radius
                            (lift-vec3-const max-corner)
                            (lift-vec3-const (vec3 -1 0 0))  ; -x direction
                            stiffness damping alpha)]
              ;; -Y wall (y = min-y, ground)
              [neg-y-force (traced-sphere-plane-force
                            pos vel radius
                            (lift-vec3-const min-corner)
                            (lift-vec3-const (vec3 0 1 0))  ; +y (up)
                            stiffness damping alpha)]
              ;; +Y wall (y = max-y, ceiling)
              [pos-y-force (traced-sphere-plane-force
                            pos vel radius
                            (lift-vec3-const max-corner)
                            (lift-vec3-const (vec3 0 -1 0))  ; -y (down)
                            stiffness damping alpha)]
              ;; -Z wall (z = min-z)
              [neg-z-force (traced-sphere-plane-force
                            pos vel radius
                            (lift-vec3-const min-corner)
                            (lift-vec3-const (vec3 0 0 1))  ; +z direction
                            stiffness damping alpha)]
              ;; +Z wall (z = max-z)
              [pos-z-force (traced-sphere-plane-force
                            pos vel radius
                            (lift-vec3-const max-corner)
                            (lift-vec3-const (vec3 0 0 -1))  ; -z direction
                            stiffness damping alpha)])
             ;; Sum all wall forces
             (traced-vec3-add neg-x-force
                              (traced-vec3-add pos-x-force
                                               (traced-vec3-add neg-y-force
                                                                (traced-vec3-add pos-y-force
                                                                                 (traced-vec3-add neg-z-force pos-z-force))))))))

;;; ====
;;; Sphere-Box Contact (Soft)
;;; ====

;;; traced-sphere-box-force : TracedVec3 × TracedVec3 × Number × TracedVec3 × Vec3 × Number × Number × Number → TracedVec3
;;; Soft contact force between sphere and axis-aligned box.
;;; Uses SDF for smooth penetration calculation.
(define (traced-sphere-box-force sphere-pos sphere-vel radius box-center box-half-extents stiffness damping alpha)
  (let* (;; SDF gives penetration (negative when inside, but we want positive penetration)
         [sdf-val (traced-sdf-box-3d sphere-pos box-center box-half-extents)]
         ;; Penetration = radius - sdf (positive when overlapping)
         [penetration (traced-sub radius sdf-val)]
         ;; Normal from box surface (gradient direction)
         ;; Approximate using finite differences on traced values
         [eps 1e-6]
         [sx+ (traced-sdf-box-3d (traced-vec3-add sphere-pos (lift-vec3-const (vec3 eps 0 0)))
                                 box-center box-half-extents)]
         [sx- (traced-sdf-box-3d (traced-vec3-sub sphere-pos (lift-vec3-const (vec3 eps 0 0)))
                                 box-center box-half-extents)]
         [sy+ (traced-sdf-box-3d (traced-vec3-add sphere-pos (lift-vec3-const (vec3 0 eps 0)))
                                 box-center box-half-extents)]
         [sy- (traced-sdf-box-3d (traced-vec3-sub sphere-pos (lift-vec3-const (vec3 0 eps 0)))
                                 box-center box-half-extents)]
         [sz+ (traced-sdf-box-3d (traced-vec3-add sphere-pos (lift-vec3-const (vec3 0 0 eps)))
                                 box-center box-half-extents)]
         [sz- (traced-sdf-box-3d (traced-vec3-sub sphere-pos (lift-vec3-const (vec3 0 0 eps)))
                                 box-center box-half-extents)]
         [grad-x (traced-div (traced-sub sx+ sx-) (* 2 eps))]
         [grad-y (traced-div (traced-sub sy+ sy-) (* 2 eps))]
         [grad-z (traced-div (traced-sub sz+ sz-) (* 2 eps))]
         [normal-raw (traced-vec3 grad-x grad-y grad-z)]
         [normal (traced-vec3-smooth-normalize normal-raw 1e-8)]
         ;; Approach velocity
         [approach-vel (traced-neg (traced-vec3-dot sphere-vel normal))]
         ;; Force magnitude
         [force-mag (traced-soft-contact-force-damped penetration approach-vel stiffness damping alpha)])
        ;; Force direction opposes normal (pushes sphere out)
        (traced-vec3-scale normal (traced-neg force-mag))))
