;;; user/physics/rigid-body.ss — 2D Rigid Body with Rotation
;;;
;;; Extends Body2D with angular components for full rigid body dynamics:
;;;   - angle: rotation in radians
;;;   - angular-vel: angular velocity (rad/s)
;;;   - inertia: moment of inertia
;;;   - inv-inertia: inverse moment of inertia
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/linalg/vec2.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec2.ss")

;;; ============================================================
;;; RigidBody2D Data Structure
;;; ============================================================

;;; A rigid body in 2D has:
;;;   - pos: position (Vec2)
;;;   - vel: velocity (Vec2)
;;;   - angle: rotation in radians (Number)
;;;   - angular-vel: angular velocity (rad/s) (Number)
;;;   - mass: mass (Number > 0, 0 for static)
;;;   - inv-mass: inverse mass (0 for static)
;;;   - inertia: moment of inertia (Number > 0, 0 for static)
;;;   - inv-inertia: inverse inertia (0 for static)

;;; make-rigid-body : Vec2 x Vec2 x Number x Number x Number x Number -> RigidBody2D
;;; Create a 2D rigid body with rotation.
(define (make-rigid-body pos vel angle angular-vel mass inertia)
  (list 'rigid-body-2d
        pos
        vel
        angle
        angular-vel
        mass
        (if (= mass 0) 0 (/ 1 mass))
        inertia
        (if (= inertia 0) 0 (/ 1 inertia))))

;;; rigid-body? : Any -> Boolean
(define (rigid-body? b)
  (and (pair? b) (eq? (car b) 'rigid-body-2d)))

;;; Accessors

;;; rigid-body-pos : RigidBody2D -> Vec2
(define (rigid-body-pos b) (list-ref b 1))

;;; rigid-body-vel : RigidBody2D -> Vec2
(define (rigid-body-vel b) (list-ref b 2))

;;; rigid-body-angle : RigidBody2D -> Number
(define (rigid-body-angle b) (list-ref b 3))

;;; rigid-body-angular-vel : RigidBody2D -> Number
(define (rigid-body-angular-vel b) (list-ref b 4))

;;; rigid-body-mass : RigidBody2D -> Number
(define (rigid-body-mass b) (list-ref b 5))

;;; rigid-body-inv-mass : RigidBody2D -> Number
(define (rigid-body-inv-mass b) (list-ref b 6))

;;; rigid-body-inertia : RigidBody2D -> Number
(define (rigid-body-inertia b) (list-ref b 7))

;;; rigid-body-inv-inertia : RigidBody2D -> Number
(define (rigid-body-inv-inertia b) (list-ref b 8))

;;; rigid-body-static? : RigidBody2D -> Boolean
(define (rigid-body-static? b)
  (= (rigid-body-inv-mass b) 0))

;;; ============================================================
;;; Updaters (Immutable)
;;; ============================================================

;;; rigid-body-with-pos : RigidBody2D x Vec2 -> RigidBody2D
(define (rigid-body-with-pos b new-pos)
  (make-rigid-body new-pos
                   (rigid-body-vel b)
                   (rigid-body-angle b)
                   (rigid-body-angular-vel b)
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

;;; rigid-body-with-vel : RigidBody2D x Vec2 -> RigidBody2D
(define (rigid-body-with-vel b new-vel)
  (make-rigid-body (rigid-body-pos b)
                   new-vel
                   (rigid-body-angle b)
                   (rigid-body-angular-vel b)
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

;;; rigid-body-with-angle : RigidBody2D x Number -> RigidBody2D
(define (rigid-body-with-angle b new-angle)
  (make-rigid-body (rigid-body-pos b)
                   (rigid-body-vel b)
                   new-angle
                   (rigid-body-angular-vel b)
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

;;; rigid-body-with-angular-vel : RigidBody2D x Number -> RigidBody2D
(define (rigid-body-with-angular-vel b new-angular-vel)
  (make-rigid-body (rigid-body-pos b)
                   (rigid-body-vel b)
                   (rigid-body-angle b)
                   new-angular-vel
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

;;; rigid-body-with-state : RigidBody2D x Vec2 x Vec2 x Number x Number -> RigidBody2D
;;; Update all kinematic state at once.
(define (rigid-body-with-state b new-pos new-vel new-angle new-angular-vel)
  (make-rigid-body new-pos
                   new-vel
                   new-angle
                   new-angular-vel
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

;;; ============================================================
;;; Static Body Constructor
;;; ============================================================

;;; make-static-rigid-body : Vec2 x Number -> RigidBody2D
;;; Create a static (immovable) rigid body at given position and angle.
(define (make-static-rigid-body pos angle)
  (make-rigid-body pos (vec2 0 0) angle 0 0 0))

;;; ============================================================
;;; Moment of Inertia Calculations
;;; ============================================================

;;; circle-inertia : Number x Number -> Number
;;; Moment of inertia for a solid circle: I = 0.5 * m * r^2
(define (circle-inertia mass radius)
  (* 0.5 mass radius radius))

;;; rectangle-inertia : Number x Number x Number -> Number
;;; Moment of inertia for a rectangle: I = (1/12) * m * (w^2 + h^2)
(define (rectangle-inertia mass width height)
  (* (/ 1 12) mass (+ (* width width) (* height height))))

;;; rod-inertia : Number x Number -> Number
;;; Moment of inertia for a thin rod about center: I = (1/12) * m * L^2
(define (rod-inertia mass length)
  (* (/ 1 12) mass length length))

;;; point-mass-inertia : Number x Number -> Number
;;; Moment of inertia for a point mass at distance r: I = m * r^2
(define (point-mass-inertia mass radius)
  (* mass radius radius))

;;; ============================================================
;;; Coordinate Transformation
;;; ============================================================

;;; local-to-world : RigidBody2D x Vec2 -> Vec2
;;; Transform a point from local body space to world space.
;;; Applies rotation then translation.
(define (local-to-world body local-point)
  (let ([angle (rigid-body-angle body)]
        [pos (rigid-body-pos body)])
       (vec2-add pos (vec2-rotate local-point angle))))

;;; world-to-local : RigidBody2D x Vec2 -> Vec2
;;; Transform a point from world space to local body space.
;;; Applies inverse translation then inverse rotation.
(define (world-to-local body world-point)
  (let ([angle (rigid-body-angle body)]
        [pos (rigid-body-pos body)])
       (vec2-rotate (vec2-sub world-point pos) (- angle))))

;;; ============================================================
;;; Velocity at Point
;;; ============================================================

;;; rigid-body-velocity-at : RigidBody2D x Vec2 -> Vec2
;;; Get the velocity at a world-space point on the body.
;;; v_point = v_cm + omega x r
;;; In 2D, omega x r = omega * perp(r) where perp(x,y) = (-y, x)
(define (rigid-body-velocity-at body world-point)
  (let* ([pos (rigid-body-pos body)]
         [vel (rigid-body-vel body)]
         [omega (rigid-body-angular-vel body)]
         [r (vec2-sub world-point pos)]
         ;; omega x r in 2D: omega * (-ry, rx)
         [tangent-vel (vec2-scale (vec2-perp r) omega)])
        (vec2-add vel tangent-vel)))

;;; ============================================================
;;; Impulse Application
;;; ============================================================

;;; apply-linear-impulse : RigidBody2D x Vec2 -> RigidBody2D
;;; Apply a linear impulse at the center of mass.
;;; Delta-v = J / m
(define (apply-linear-impulse body impulse)
  (if (rigid-body-static? body)
      body
      (let* ([inv-mass (rigid-body-inv-mass body)]
             [new-vel (vec2-add (rigid-body-vel body)
                                (vec2-scale impulse inv-mass))])
            (rigid-body-with-vel body new-vel))))

;;; apply-angular-impulse : RigidBody2D x Number -> RigidBody2D
;;; Apply an angular impulse (torque * dt).
;;; Delta-omega = L / I
(define (apply-angular-impulse body angular-impulse)
  (if (rigid-body-static? body)
      body
      (let* ([inv-inertia (rigid-body-inv-inertia body)]
             [new-omega (+ (rigid-body-angular-vel body)
                           (* angular-impulse inv-inertia))])
            (rigid-body-with-angular-vel body new-omega))))

;;; apply-impulse-at-point : RigidBody2D x Vec2 x Vec2 -> RigidBody2D
;;; Apply an impulse at a world-space point.
;;; This produces both linear and angular effects.
;;; Delta-v = J / m
;;; Delta-omega = (r x J) / I
(define (apply-impulse-at-point body impulse world-point)
  (if (rigid-body-static? body)
      body
      (let* ([pos (rigid-body-pos body)]
             [r (vec2-sub world-point pos)]
             [inv-mass (rigid-body-inv-mass body)]
             [inv-inertia (rigid-body-inv-inertia body)]
             ;; Linear impulse
             [new-vel (vec2-add (rigid-body-vel body)
                                (vec2-scale impulse inv-mass))]
             ;; Angular impulse: r x J (2D cross product gives scalar)
             [torque-impulse (vec2-cross r impulse)]
             [new-omega (+ (rigid-body-angular-vel body)
                           (* torque-impulse inv-inertia))])
            (rigid-body-with-state body
                                   (rigid-body-pos body)
                                   new-vel
                                   (rigid-body-angle body)
                                   new-omega))))

;;; ============================================================
;;; Energy Calculations
;;; ============================================================

;;; rigid-body-kinetic-energy : RigidBody2D -> Number
;;; Total kinetic energy: KE = 0.5 * m * v^2 + 0.5 * I * omega^2
(define (rigid-body-kinetic-energy body)
  (let ([mass (rigid-body-mass body)]
        [vel (rigid-body-vel body)]
        [inertia (rigid-body-inertia body)]
        [omega (rigid-body-angular-vel body)])
       (+ (* 0.5 mass (vec2-magnitude-sq vel))
          (* 0.5 inertia omega omega))))

;;; rigid-body-linear-momentum : RigidBody2D -> Vec2
;;; Linear momentum: p = m * v
(define (rigid-body-linear-momentum body)
  (vec2-scale (rigid-body-vel body) (rigid-body-mass body)))

;;; rigid-body-angular-momentum : RigidBody2D -> Number
;;; Angular momentum about center: L = I * omega
(define (rigid-body-angular-momentum body)
  (* (rigid-body-inertia body) (rigid-body-angular-vel body)))

;;; ============================================================
;;; Integration Helpers
;;; ============================================================

;;; integrate-rigid-body : RigidBody2D x Vec2 x Number x Number -> RigidBody2D
;;; Integrate body forward by dt with given linear acceleration and torque.
;;; Uses semi-implicit Euler: update velocity first, then position.
(define (integrate-rigid-body body linear-accel angular-accel dt)
  (if (rigid-body-static? body)
      body
      (let* ([new-vel (vec2-add (rigid-body-vel body)
                                (vec2-scale linear-accel dt))]
             [new-omega (+ (rigid-body-angular-vel body)
                           (* angular-accel dt))]
             [new-pos (vec2-add (rigid-body-pos body)
                                (vec2-scale new-vel dt))]
             [new-angle (+ (rigid-body-angle body)
                           (* new-omega dt))])
            (rigid-body-with-state body new-pos new-vel new-angle new-omega))))

;;; ============================================================
;;; Conversion from Body2D
;;; ============================================================

;;; body-2d->rigid-body : Body2D x Number x Number -> RigidBody2D
;;; Convert a Body2D to RigidBody2D with given shape properties.
;;; Assumes Body2D has structure: (list 'body-2d pos vel mass inv-mass)
(define (body-2d->rigid-body body-2d inertia)
  (let ([pos (list-ref body-2d 1)]
        [vel (list-ref body-2d 2)]
        [mass (list-ref body-2d 3)])
       (make-rigid-body pos vel 0 0 mass inertia)))

;;; rigid-body->body-2d : RigidBody2D -> Body2D
;;; Convert back to Body2D (losing angular information).
(define (rigid-body->body-2d rigid-body)
  (let ([pos (rigid-body-pos rigid-body)]
        [vel (rigid-body-vel rigid-body)]
        [mass (rigid-body-mass rigid-body)])
       (list 'body-2d pos vel mass
             (if (= mass 0) 0 (/ 1 mass)))))
