;;; core/diff-physics/smooth-collision.ss --- Differentiable Collision Primitives
;;;
;;; Signed Distance Functions (SDFs) and soft contact models for differentiable
;;; physics. SDFs provide smooth, differentiable representations of geometry.
;;;
;;; Key insight: Standard collision detection returns discrete contact/no-contact.
;;; For differentiable physics, we use continuous penalty forces based on
;;; penetration depth, computed via SDFs.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/vec2.ss
;;;   - autodiff/reverse-diff.ss
;;;   - diff-physics/traced-vec2.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec2.ss")
(load "core/autodiff/reverse-diff.ss")
(load "core/diff-physics/traced-vec2.ss")

;;; ============================================================
;;; Signed Distance Functions (SDFs)
;;; ============================================================

;;; An SDF returns:
;;;   - negative values: inside the shape
;;;   - zero: on the boundary
;;;   - positive values: outside the shape
;;;
;;; The gradient of an SDF at any point gives the direction to the nearest
;;; surface point (i.e., the outward normal when normalized).

;;; traced-sdf-circle : TracedVec2 × TracedVec2 × Number → TracedValue
;;; SDF for a circle centered at `center` with given `radius`.
;;; Returns signed distance: negative inside, positive outside.
(define (traced-sdf-circle point center radius)
  (traced-sub (traced-vec2-smooth-distance point center 1e-8) radius))

;;; traced-sdf-point : TracedVec2 × TracedVec2 → TracedValue
;;; SDF for a point (radius = 0 circle).
(define (traced-sdf-point query target)
  (traced-vec2-smooth-distance query target 1e-8))

;;; traced-sdf-box : TracedVec2 × TracedVec2 × TracedVec2 → TracedValue
;;; SDF for an axis-aligned box.
;;;   center: box center
;;;   half-extents: (half-width, half-height)
;;; Uses smooth approximation for corners.
(define (traced-sdf-box point center half-extents)
  (let* ([local (traced-vec2-sub point center)]
         ;; Distance to each axis
         [dx (traced-sub (traced-smooth-abs (traced-vec2-x local) 1e-8)
                         (vec2-x half-extents))]
         [dy (traced-sub (traced-smooth-abs (traced-vec2-y local) 1e-8)
                         (vec2-y half-extents))]
         ;; Outside: sqrt(max(dx,0)² + max(dy,0)²) + min(max(dx,dy),0)
         [dx-pos (traced-softplus dx 20)]  ; smooth max(dx, 0)
         [dy-pos (traced-softplus dy 20)]  ; smooth max(dy, 0)
         ;; Distance to corner (when outside)
         [outside-dist (traced-sqrt (traced-add (traced-sq dx-pos)
                                                (traced-add (traced-sq dy-pos) 1e-16)))]
         ;; Distance to interior (when inside) - smooth min(max(dx,dy), 0)
         [inside-dist (traced-softmin2 (traced-softmax2 dx dy 20) 0 20)])
        ;; Combine: outside part + inside part
        (traced-add outside-dist inside-dist)))

;;; traced-sdf-capsule : TracedVec2 × TracedVec2 × TracedVec2 × Number → TracedValue
;;; SDF for a capsule (line segment with radius).
;;;   a, b: capsule endpoints
;;;   radius: capsule radius
(define (traced-sdf-capsule point a b radius)
  (let* ([pa (traced-vec2-sub point a)]
         [ba (traced-vec2-sub b a)]
         ;; Project point onto line segment, clamped to [0, 1]
         [ba-len-sq (traced-vec2-magnitude-sq ba)]
         [t-raw (traced-div (traced-vec2-dot pa ba) ba-len-sq)]
         ;; Smooth clamp to [0, 1]
         [t (traced-smooth-clamp t-raw 0 1 20)]
         ;; Closest point on segment
         [closest (traced-vec2-add a (traced-vec2-scale ba t))]
         ;; Distance to closest point, minus radius
         [dist (traced-vec2-smooth-distance point closest 1e-8)])
        (traced-sub dist radius)))

;;; traced-sdf-line-segment : TracedVec2 × TracedVec2 × TracedVec2 → TracedValue
;;; SDF for a line segment (zero-radius capsule).
(define (traced-sdf-line-segment point a b)
  (traced-sdf-capsule point a b 0))

;;; traced-sdf-half-plane : TracedVec2 × TracedVec2 × TracedVec2 → TracedValue
;;; SDF for a half-plane.
;;;   point-on-plane: any point on the plane boundary
;;;   normal: outward normal (pointing toward positive distance)
(define (traced-sdf-half-plane point point-on-plane normal)
  (traced-vec2-dot (traced-vec2-sub point point-on-plane) normal))

;;; ============================================================
;;; Circle-Circle Collision (Most Common Case)
;;; ============================================================

;;; traced-sdf-circle-circle : TracedVec2 × Number × TracedVec2 × Number → TracedValue
;;; SDF for the Minkowski sum of two circles (their intersection region).
;;; Returns negative when circles overlap.
(define (traced-sdf-circle-circle pos-a radius-a pos-b radius-b)
  (traced-sub (traced-vec2-smooth-distance pos-a pos-b 1e-8)
              (+ radius-a radius-b)))

;;; traced-circle-contact : TracedVec2 × Number × TracedVec2 × Number → (TracedVec2 × TracedVec2 × TracedValue)
;;; Compute contact information for two circles.
;;; Returns: (contact-point, normal, penetration-depth)
;;;   normal: points from A to B
;;;   penetration: positive when overlapping
(define (traced-circle-contact pos-a radius-a pos-b radius-b)
  (let* ([delta (traced-vec2-sub pos-b pos-a)]
         [dist (traced-vec2-smooth-magnitude delta 1e-8)]
         ;; Normal from A to B (smooth normalize)
         [normal (traced-vec2-smooth-normalize delta 1e-8)]
         ;; Penetration depth (positive when overlapping)
         [penetration (traced-sub (+ radius-a radius-b) dist)]
         ;; Contact point on surface of A (in world space)
         [contact (traced-vec2-add pos-a (traced-vec2-scale normal radius-a))])
        (values contact normal penetration)))

;;; ============================================================
;;; Circle-Plane Collision
;;; ============================================================

;;; traced-circle-plane-contact : TracedVec2 × Number × TracedVec2 × TracedVec2 → (TracedVec2 × TracedVec2 × TracedValue)
;;; Contact between circle and infinite plane.
;;; plane-normal points into the valid region (away from solid).
(define (traced-circle-plane-contact circle-pos radius plane-point plane-normal)
  (let* (;; Signed distance from circle center to plane
         [center-dist (traced-sdf-half-plane circle-pos plane-point plane-normal)]
         ;; Penetration is radius - center_dist (positive when penetrating)
         [penetration (traced-sub radius center-dist)]
         ;; Contact point on circle surface
         [contact (traced-vec2-sub circle-pos
                                   (traced-vec2-scale plane-normal radius))])
        ;; Normal points away from plane (opposite of plane-normal for reaction)
        (values contact plane-normal penetration)))

;;; ============================================================
;;; Soft Contact Force Model
;;; ============================================================

;;; Instead of impulse-based collision response, soft contact uses
;;; penalty forces proportional to penetration depth.
;;; This is inherently smooth and differentiable.

;;; traced-softplus : TracedValue × Number → TracedValue
;;; Smooth approximation of max(0, x).
;;; softplus(x, α) = log(1 + exp(αx)) / α
;;; As α → ∞, approaches max(0, x).
;;; (Re-exported from traced-vec2.ss for convenience)

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
         ;; Only damp when in contact (penetration > 0) and approaching
         [in-contact (traced-softplus penetration alpha)]  ; smooth indicator
         [damping-force (traced-mul (traced-mul damping approach-vel)
                                    (traced-softplus penetration (* alpha 2)))])
        ;; Total force = spring + damping (both only active when penetrating)
        (traced-add spring-force damping-force)))

;;; ============================================================
;;; Complete Soft Contact Response
;;; ============================================================

;;; traced-circle-circle-force : TracedVec2 × TracedVec2 × Number × TracedVec2 × TracedVec2 × Number × Number × Number × Number → TracedVec2
;;; Compute soft contact force between two circles.
;;; Returns force on body A (negate for force on body B).
(define (traced-circle-circle-force pos-a vel-a radius-a pos-b vel-b radius-b stiffness damping alpha)
  (call-with-values
   (lambda () (traced-circle-contact pos-a radius-a pos-b radius-b))
   (lambda (contact normal penetration)
           (let* ([rel-vel (traced-vec2-sub vel-a vel-b)]
                  [approach-vel (traced-neg (traced-vec2-dot rel-vel normal))]
                  [force-mag (traced-soft-contact-force-damped penetration approach-vel stiffness damping alpha)])
                 (traced-vec2-scale normal (traced-neg force-mag))))))

;;; traced-circle-plane-force : TracedVec2 × TracedVec2 × Number × TracedVec2 × TracedVec2 × Number × Number × Number → TracedVec2
;;; Compute soft contact force between circle and plane.
;;; Returns force on the circle.
(define (traced-circle-plane-force pos vel radius plane-point plane-normal stiffness damping alpha)
  (call-with-values
   (lambda () (traced-circle-plane-contact pos radius plane-point plane-normal))
   (lambda (contact _ penetration)
           (let* ([approach-vel (traced-neg (traced-vec2-dot vel plane-normal))]
                  [force-mag (traced-soft-contact-force-damped penetration approach-vel stiffness damping alpha)])
                 (traced-vec2-scale plane-normal force-mag)))))

;;; ============================================================
;;; Ground Plane (Common Special Case)
;;; ============================================================

;;; traced-ground-plane-force : TracedVec2 × TracedVec2 × Number × Number × Number × Number × Number → TracedVec2
;;; Force from horizontal ground plane at y = ground-y.
;;; Ground normal points up (+y).
(define (traced-ground-plane-force pos vel radius ground-y stiffness damping alpha)
  (traced-circle-plane-force pos vel radius
                             (lift-vec2-const (vec2 0 ground-y))
                             (lift-vec2-const (vec2 0 1))  ; up
                             stiffness damping alpha))

;;; ============================================================
;;; Contact Normal and Point from SDF
;;; ============================================================

;;; For SDFs, the gradient gives the normal direction.

;;; traced-sdf-normal : (TracedVec2 → TracedValue) × Vec2 → Vec2
;;; Compute normal direction from SDF at a point.
;;; Uses automatic differentiation to get gradient.
(define (traced-sdf-normal sdf-fn point)
  (vec2-normalize (vec2-gradient sdf-fn point)))

;;; traced-sdf-closest-point : (TracedVec2 → TracedValue) × Vec2 → Vec2
;;; Approximate closest point on surface using SDF gradient.
;;; closest = point - sdf(point) * normal
(define (traced-sdf-closest-point sdf-fn point)
  (let* ([tape (make-reverse-tape)]
         [traced-point (lift-vec2 point tape)]
         [sdf-val-traced (sdf-fn traced-point)]
         [sdf-val (traced-value sdf-val-traced)]
         [normal (traced-sdf-normal sdf-fn point)])
        (vec2-sub point (vec2-scale normal sdf-val))))

;;; ============================================================
;;; Soft Friction Model
;;; ============================================================

;;; Coulomb friction is non-smooth (sign function). We use a smooth
;;; approximation based on hyperbolic tangent.

;;; traced-soft-friction : TracedVec2 × TracedValue × Number × Number → TracedVec2
;;; Smooth friction force.
;;;   tangent-vel: velocity component tangent to contact
;;;   normal-force: magnitude of normal force
;;;   mu: friction coefficient
;;;   smoothness: transition smoothness (higher = sharper)
;;;
;;; Uses tanh to smoothly interpolate between -μFn and +μFn.
(define (traced-soft-friction tangent-vel normal-force mu smoothness)
  (let* ([vel-mag (traced-vec2-smooth-magnitude tangent-vel 1e-8)]
         ;; Smooth sign function: tanh(v * smoothness)
         [vel-sign (traced-tanh (traced-mul vel-mag smoothness))]
         ;; Friction magnitude: μ * Fn * sign(v)
         [friction-mag (traced-mul (traced-mul mu normal-force) vel-sign)]
         ;; Friction direction opposes velocity
         [friction-dir (traced-vec2-smooth-normalize (traced-vec2-neg tangent-vel) 1e-8)])
        (traced-vec2-scale friction-dir friction-mag)))

;;; traced-contact-friction-force : TracedVec2 × TracedVec2 × TracedValue × Number × Number → TracedVec2
;;; Complete friction force at a contact.
;;;   rel-vel: relative velocity at contact
;;;   normal: contact normal
;;;   normal-force: magnitude of normal force
;;;   mu: friction coefficient
(define (traced-contact-friction-force rel-vel normal normal-force mu)
  (let* (;; Decompose velocity into normal and tangent
         [vel-normal (traced-vec2-dot rel-vel normal)]
         [vel-tangent (traced-vec2-sub rel-vel
                                       (traced-vec2-scale normal vel-normal))])
        (traced-soft-friction vel-tangent normal-force mu 10)))

;;; ============================================================
;;; Material Properties for Soft Contact
;;; ============================================================

;;; Default stiffness and damping values.
;;; These should be tuned for your simulation's scale and timestep.

(define *default-contact-stiffness* 10000)  ; N/m
(define *default-contact-damping* 100)       ; Ns/m
(define *default-contact-alpha* 100)         ; softplus sharpness
(define *default-friction-mu* 0.5)           ; friction coefficient

;;; make-soft-material : Number × Number × Number × Number → SoftMaterial
;;; Create a soft contact material.
(define (make-soft-material stiffness damping alpha friction)
  (list 'soft-material stiffness damping alpha friction))

;;; soft-material-stiffness : SoftMaterial → Number
(define (soft-material-stiffness m) (list-ref m 1))

;;; soft-material-damping : SoftMaterial → Number
(define (soft-material-damping m) (list-ref m 2))

;;; soft-material-alpha : SoftMaterial → Number
(define (soft-material-alpha m) (list-ref m 3))

;;; soft-material-friction : SoftMaterial → Number
(define (soft-material-friction m) (list-ref m 4))

;;; default-soft-material : → SoftMaterial
(define (default-soft-material)
  (make-soft-material *default-contact-stiffness*
                      *default-contact-damping*
                      *default-contact-alpha*
                      *default-friction-mu*))

;;; ============================================================
;;; Wall Boundaries (Common for Simulations)
;;; ============================================================

;;; traced-wall-forces : TracedVec2 × TracedVec2 × Number × Vec2 × Vec2 × SoftMaterial → TracedVec2
;;; Compute forces from rectangular boundary walls.
;;;   min-corner, max-corner: boundary box
(define (traced-wall-forces pos vel radius min-corner max-corner mat)
  (let ([stiffness (soft-material-stiffness mat)]
        [damping (soft-material-damping mat)]
        [alpha (soft-material-alpha mat)])
       ;; Left wall (x = min-x)
       (let* ([left-force (traced-circle-plane-force
                           pos vel radius
                           (lift-vec2-const min-corner)
                           (lift-vec2-const (vec2 1 0))  ; right
                           stiffness damping alpha)]
              ;; Right wall (x = max-x)
              [right-force (traced-circle-plane-force
                            pos vel radius
                            (lift-vec2-const max-corner)
                            (lift-vec2-const (vec2 -1 0))  ; left
                            stiffness damping alpha)]
              ;; Bottom wall (y = min-y)
              [bottom-force (traced-circle-plane-force
                             pos vel radius
                             (lift-vec2-const min-corner)
                             (lift-vec2-const (vec2 0 1))  ; up
                             stiffness damping alpha)]
              ;; Top wall (y = max-y)
              [top-force (traced-circle-plane-force
                          pos vel radius
                          (lift-vec2-const max-corner)
                          (lift-vec2-const (vec2 0 -1))  ; down
                          stiffness damping alpha)])
             ;; Sum all wall forces
             (traced-vec2-add left-force
                              (traced-vec2-add right-force
                                               (traced-vec2-add bottom-force top-force))))))
