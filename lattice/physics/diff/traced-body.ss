(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module traced-body
;;; @requires prelude vec2 rigid-body traced-vec2
(require 'prelude)
(require 'vec2)
(require 'rigid-body)
(require 'traced-vec2)

(doc 'module 'traced-body)
(doc 'description "Differentiable Rigid Body State

Traced rigid body state for automatic differentiation through physics.
A traced body contains traced position, velocity, angle, and angular velocity.
Mass and inertia are treated as constants (non-differentiable) by default.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'traced-rigid-body-data-structure)

;;; A traced rigid body has:
;;;   - pos: position (TracedVec2)
;;;   - vel: velocity (TracedVec2)
;;;   - angle: rotation in radians (TracedValue)
;;;   - angular-vel: angular velocity (TracedValue)
;;;   - mass: mass (Number > 0, 0 for static) - constant
;;;   - inv-mass: inverse mass (0 for static) - constant
;;;   - inertia: moment of inertia (Number > 0) - constant
;;;   - inv-inertia: inverse inertia (0 for static) - constant

;;; make-traced-body : TracedVec2 × TracedVec2 × TracedValue × TracedValue × Number × Number → TracedBody
;;; Create a traced rigid body.
(define (make-traced-body pos vel angle angular-vel mass inertia)
  (list 'traced-body
        pos
        vel
        angle
        angular-vel
        mass
        (if (= mass 0) 0 (/ 1 mass))
        inertia
        (if (= inertia 0) 0 (/ 1 inertia))))

;;; traced-body? : Any → Boolean
(define (traced-body? b)
  (and (pair? b) (eq? (car b) 'traced-body)))

;;; ====
;;; Accessors
;;; ====

;;; traced-body-pos : TracedBody → TracedVec2
(define (traced-body-pos b) (list-ref b 1))

;;; traced-body-vel : TracedBody → TracedVec2
(define (traced-body-vel b) (list-ref b 2))

;;; traced-body-angle : TracedBody → TracedValue
(define (traced-body-angle b) (list-ref b 3))

;;; traced-body-angular-vel : TracedBody → TracedValue
(define (traced-body-angular-vel b) (list-ref b 4))

;;; traced-body-mass : TracedBody → Number
(define (traced-body-mass b) (list-ref b 5))

;;; traced-body-inv-mass : TracedBody → Number
(define (traced-body-inv-mass b) (list-ref b 6))

;;; traced-body-inertia : TracedBody → Number
(define (traced-body-inertia b) (list-ref b 7))

;;; traced-body-inv-inertia : TracedBody → Number
(define (traced-body-inv-inertia b) (list-ref b 8))

;;; traced-body-static? : TracedBody → Boolean
(define (traced-body-static? b)
  (= (traced-body-inv-mass b) 0))

;;; ====
;;; Updaters (Immutable)
;;; ====

;;; traced-body-with-pos : TracedBody × TracedVec2 → TracedBody
(define (traced-body-with-pos b new-pos)
  (make-traced-body new-pos
                    (traced-body-vel b)
                    (traced-body-angle b)
                    (traced-body-angular-vel b)
                    (traced-body-mass b)
                    (traced-body-inertia b)))

;;; traced-body-with-vel : TracedBody × TracedVec2 → TracedBody
(define (traced-body-with-vel b new-vel)
  (make-traced-body (traced-body-pos b)
                    new-vel
                    (traced-body-angle b)
                    (traced-body-angular-vel b)
                    (traced-body-mass b)
                    (traced-body-inertia b)))

;;; traced-body-with-angle : TracedBody × TracedValue → TracedBody
(define (traced-body-with-angle b new-angle)
  (make-traced-body (traced-body-pos b)
                    (traced-body-vel b)
                    new-angle
                    (traced-body-angular-vel b)
                    (traced-body-mass b)
                    (traced-body-inertia b)))

;;; traced-body-with-angular-vel : TracedBody × TracedValue → TracedBody
(define (traced-body-with-angular-vel b new-angular-vel)
  (make-traced-body (traced-body-pos b)
                    (traced-body-vel b)
                    (traced-body-angle b)
                    new-angular-vel
                    (traced-body-mass b)
                    (traced-body-inertia b)))

;;; traced-body-with-state : TracedBody × TracedVec2 × TracedVec2 × TracedValue × TracedValue → TracedBody
;;; Update all kinematic state at once.
(define (traced-body-with-state b new-pos new-vel new-angle new-angular-vel)
  (make-traced-body new-pos
                    new-vel
                    new-angle
                    new-angular-vel
                    (traced-body-mass b)
                    (traced-body-inertia b)))

;;; ====
;;; Conversion Between RigidBody2D and TracedBody
;;; ====

;;; trace-rigid-body : RigidBody2D × Tape → TracedBody
;;; Convert a regular rigid body to a traced body (creates traced variables).
(define (trace-rigid-body rb tape)
  (make-traced-body (lift-vec2 (rigid-body-pos rb) tape)
                    (lift-vec2 (rigid-body-vel rb) tape)
                    (make-traced-var (rigid-body-angle rb) tape)
                    (make-traced-var (rigid-body-angular-vel rb) tape)
                    (rigid-body-mass rb)
                    (rigid-body-inertia rb)))

;;; unpack-traced-body : TracedBody → RigidBody2D
;;; Extract numeric values from traced body into a regular rigid body.
(define (unpack-traced-body tb)
  (make-rigid-body (unpack-traced-vec2 (traced-body-pos tb))
                   (unpack-traced-vec2 (traced-body-vel tb))
                   (traced-value (traced-body-angle tb))
                   (traced-value (traced-body-angular-vel tb))
                   (traced-body-mass tb)
                   (traced-body-inertia tb)))

;;; ====
;;; Static Body Constructor
;;; ====

;;; make-traced-static-body : TracedVec2 × TracedValue → TracedBody
;;; Create a static (immovable) traced rigid body.
(define (make-traced-static-body pos angle)
  (let* ([tape (or (and (traced? (traced-vec2-x pos))
                        (traced-tape (traced-vec2-x pos)))
                   (and (traced? angle) (traced-tape angle)))])
        (make-traced-body pos
                          (if tape
                              (traced-vec2-zero tape)
                              (lift-vec2-const (vec2-zero)))
                          angle
                          (if tape 0 0)  ; angular velocity is constant 0
                          0    ; mass = 0 for static
                          0))) ; inertia = 0 for static

;;; ====
;;; Body State Flattening (for optimization)
;;; ====

;;; traced-body-state->list : TracedBody → (List TracedValue)
;;; Flatten body state to list of traced values: (px, py, vx, vy, angle, omega).
(define (traced-body-state->list tb)
  (list (traced-vec2-x (traced-body-pos tb))
        (traced-vec2-y (traced-body-pos tb))
        (traced-vec2-x (traced-body-vel tb))
        (traced-vec2-y (traced-body-vel tb))
        (traced-body-angle tb)
        (traced-body-angular-vel tb)))

;;; list->traced-body-state : (List TracedValue) × Number × Number → TracedBody
;;; Reconstruct body state from flattened list.
(define (list->traced-body-state vals mass inertia)
  (make-traced-body (traced-vec2 (list-ref vals 0) (list-ref vals 1))
                    (traced-vec2 (list-ref vals 2) (list-ref vals 3))
                    (list-ref vals 4)
                    (list-ref vals 5)
                    mass
                    inertia))

;;; body-state-dimension : → Number
;;; Dimension of body state vector (6 for 2D rigid body).
(define (body-state-dimension) 6)

;;; ====
;;; Physics Operations on Traced Bodies
;;; ====

;;; traced-body-velocity-at : TracedBody × TracedVec2 → TracedVec2
;;; Compute velocity at a world-space point on the body.
;;; v_point = v_cm + ω × r
;;; In 2D: v_point = v_cm + ω * perp(r)
(define (traced-body-velocity-at body world-point)
  (let* ([r (traced-vec2-sub world-point (traced-body-pos body))]
         [omega (traced-body-angular-vel body)]
         ;; ω × r in 2D: (-ω*ry, ω*rx)
         [rotational-vel (traced-vec2 (traced-neg (traced-mul omega (traced-vec2-y r)))
                                      (traced-mul omega (traced-vec2-x r)))])
        (traced-vec2-add (traced-body-vel body) rotational-vel)))

;;; traced-apply-impulse : TracedBody × TracedVec2 × TracedVec2 → TracedBody
;;; Apply an impulse J at world-space point contact.
;;; Changes linear velocity by J/m and angular velocity by (r × J)/I.
(define (traced-apply-impulse body impulse contact)
  (if (traced-body-static? body)
      body
      (let* ([r (traced-vec2-sub contact (traced-body-pos body))]
             [inv-mass (traced-body-inv-mass body)]
             [inv-inertia (traced-body-inv-inertia body)]
             ;; Δv = J / m
             [delta-vel (traced-vec2-scale impulse inv-mass)]
             ;; Δω = (r × J) / I = (rx*Jy - ry*Jx) / I
             [r-cross-J (traced-vec2-cross r impulse)]
             [delta-omega (traced-mul r-cross-J inv-inertia)]
             [new-vel (traced-vec2-add (traced-body-vel body) delta-vel)]
             [new-omega (traced-add (traced-body-angular-vel body) delta-omega)])
            (make-traced-body (traced-body-pos body)
                              new-vel
                              (traced-body-angle body)
                              new-omega
                              (traced-body-mass body)
                              (traced-body-inertia body)))))

;;; traced-apply-force : TracedBody × TracedVec2 × TracedVec2 × TracedValue → TracedBody
;;; Apply force F at world-space point for duration dt.
;;; This is equivalent to applying impulse F*dt.
(define (traced-apply-force body force point dt)
  (traced-apply-impulse body (traced-vec2-scale force dt) point))

;;; traced-apply-central-force : TracedBody × TracedVec2 × TracedValue → TracedBody
;;; Apply force at center of mass (no torque).
(define (traced-apply-central-force body force dt)
  (if (traced-body-static? body)
      body
      (let* ([impulse (traced-vec2-scale force dt)]
             [delta-vel (traced-vec2-scale impulse (traced-body-inv-mass body))]
             [new-vel (traced-vec2-add (traced-body-vel body) delta-vel)])
            (traced-body-with-vel body new-vel))))

;;; traced-apply-torque : TracedBody × TracedValue × TracedValue → TracedBody
;;; Apply torque τ for duration dt.
(define (traced-apply-torque body torque dt)
  (if (traced-body-static? body)
      body
      (let* ([delta-omega (traced-mul (traced-mul torque dt)
                                      (traced-body-inv-inertia body))]
             [new-omega (traced-add (traced-body-angular-vel body) delta-omega)])
            (traced-body-with-angular-vel body new-omega))))

;;; ====
;;; Moment of Inertia (same as rigid-body.ss, for convenience)
;;; ====

;;; These return regular numbers since they compute constants.

;;; traced-circle-inertia : Number × Number → Number
(define traced-circle-inertia circle-inertia)

;;; traced-rectangle-inertia : Number × Number × Number → Number
(define traced-rectangle-inertia rectangle-inertia)

;;; ====
;;; World-Space Transformations
;;; ====

;;; traced-local-to-world : TracedBody × TracedVec2 → TracedVec2
;;; Transform a point from body-local coordinates to world coordinates.
;;; world = pos + rotate(local, angle)
(define (traced-local-to-world body local-point)
  (traced-vec2-add (traced-body-pos body)
                   (traced-vec2-rotate local-point (traced-body-angle body))))

;;; traced-world-to-local : TracedBody × TracedVec2 → TracedVec2
;;; Transform a point from world coordinates to body-local coordinates.
;;; local = rotate(world - pos, -angle)
(define (traced-world-to-local body world-point)
  (traced-vec2-rotate (traced-vec2-sub world-point (traced-body-pos body))
                      (traced-neg (traced-body-angle body))))

;;; traced-local-dir-to-world : TracedBody × TracedVec2 → TracedVec2
;;; Transform a direction vector from body-local to world coordinates.
;;; (No translation, only rotation.)
(define (traced-local-dir-to-world body local-dir)
  (traced-vec2-rotate local-dir (traced-body-angle body)))

;;; traced-world-dir-to-local : TracedBody × TracedVec2 → TracedVec2
;;; Transform a direction vector from world to body-local coordinates.
(define (traced-world-dir-to-local body world-dir)
  (traced-vec2-rotate world-dir (traced-neg (traced-body-angle body))))

;;; ====
;;; Kinetic Energy (for debugging/validation)
;;; ====

;;; traced-body-kinetic-energy : TracedBody → TracedValue
;;; Compute total kinetic energy: (1/2)mv² + (1/2)Iω²
(define (traced-body-kinetic-energy body)
  (let* ([m (traced-body-mass body)]
         [I (traced-body-inertia body)]
         [v-sq (traced-vec2-magnitude-sq (traced-body-vel body))]
         [omega (traced-body-angular-vel body)]
         [omega-sq (traced-sq omega)])
        (traced-add (traced-mul (* 0.5 m) v-sq)
                    (traced-mul (* 0.5 I) omega-sq))))

;;; traced-body-momentum : TracedBody → TracedVec2
;;; Compute linear momentum: p = mv
(define (traced-body-momentum body)
  (traced-vec2-scale (traced-body-vel body) (traced-body-mass body)))

;;; traced-body-angular-momentum : TracedBody → TracedValue
;;; Compute angular momentum about center of mass: L = Iω
(define (traced-body-angular-momentum body)
  (traced-mul (traced-body-inertia body) (traced-body-angular-vel body)))
