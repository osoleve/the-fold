(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")

(doc 'module 'rigid-body)
(doc 'description "2D Rigid Body with rotation and moment of inertia")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'rigid-body-data-structure)

(doc "A rigid body in 2D has:")
(doc "- pos: position (Vec2)")
(doc "- vel: velocity (Vec2)")
(doc "- angle: rotation in radians (Number)")
(doc "- angular-vel: angular velocity (rad/s) (Number)")
(doc "- mass: mass (Number > 0, 0 for static)")
(doc "- inv-mass: inverse mass (0 for static)")
(doc "- inertia: moment of inertia (Number > 0, 0 for static)")
(doc "- inv-inertia: inverse inertia (0 for static)")

(define (make-rigid-body pos vel angle angular-vel mass inertia)
  (doc 'type '(-> Vec2 Vec2 Number Number Number Number RigidBody2D))
  (doc 'description "Create a 2D rigid body with rotation")
  (list 'rigid-body-2d
        pos
        vel
        angle
        angular-vel
        mass
        (if (= mass 0) 0 (/ 1 mass))
        inertia
        (if (= inertia 0) 0 (/ 1 inertia))))

(define (rigid-body? b)
  (doc 'type '(-> Any Boolean))
  (and (pair? b) (eq? (car b) 'rigid-body-2d)))

(doc 'section 'accessors)

(define (rigid-body-pos b)
  (doc 'type '(-> RigidBody2D Vec2))
  (list-ref b 1))

(define (rigid-body-vel b)
  (doc 'type '(-> RigidBody2D Vec2))
  (list-ref b 2))

(define (rigid-body-angle b)
  (doc 'type '(-> RigidBody2D Number))
  (list-ref b 3))

(define (rigid-body-angular-vel b)
  (doc 'type '(-> RigidBody2D Number))
  (list-ref b 4))

(define (rigid-body-mass b)
  (doc 'type '(-> RigidBody2D Number))
  (list-ref b 5))

(define (rigid-body-inv-mass b)
  (doc 'type '(-> RigidBody2D Number))
  (list-ref b 6))

(define (rigid-body-inertia b)
  (doc 'type '(-> RigidBody2D Number))
  (list-ref b 7))

(define (rigid-body-inv-inertia b)
  (doc 'type '(-> RigidBody2D Number))
  (list-ref b 8))

(define (rigid-body-static? b)
  (doc 'type '(-> RigidBody2D Boolean))
  (= (rigid-body-inv-mass b) 0))

(doc 'section 'updaters)

(define (rigid-body-with-pos b new-pos)
  (doc 'type '(-> RigidBody2D Vec2 RigidBody2D))
  (make-rigid-body new-pos
                   (rigid-body-vel b)
                   (rigid-body-angle b)
                   (rigid-body-angular-vel b)
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

(define (rigid-body-with-vel b new-vel)
  (doc 'type '(rigid-body-with-vel : RigidBody2D x Vec2 -> RigidBody2D))
  (make-rigid-body (rigid-body-pos b)
                   new-vel
                   (rigid-body-angle b)
                   (rigid-body-angular-vel b)
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

(define (rigid-body-with-angle b new-angle)
  (doc 'type '(rigid-body-with-angle : RigidBody2D x Number -> RigidBody2D))
  (make-rigid-body (rigid-body-pos b)
                   (rigid-body-vel b)
                   new-angle
                   (rigid-body-angular-vel b)
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

(define (rigid-body-with-angular-vel b new-angular-vel)
  (doc 'type '(rigid-body-with-angular-vel : RigidBody2D x Number -> RigidBody2D))
  (make-rigid-body (rigid-body-pos b)
                   (rigid-body-vel b)
                   (rigid-body-angle b)
                   new-angular-vel
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

(define (rigid-body-with-mass b new-mass)
  (doc 'type '(rigid-body-with-mass : RigidBody2D x Number -> RigidBody2D))
  (make-rigid-body (rigid-body-pos b)
                   (rigid-body-vel b)
                   (rigid-body-angle b)
                   (rigid-body-angular-vel b)
                   new-mass
                   (rigid-body-inertia b)))

(define (rigid-body-with-inertia b new-inertia)
  (doc 'type '(rigid-body-with-inertia : RigidBody2D x Number -> RigidBody2D))
  (make-rigid-body (rigid-body-pos b)
                   (rigid-body-vel b)
                   (rigid-body-angle b)
                   (rigid-body-angular-vel b)
                   (rigid-body-mass b)
                   new-inertia))

(doc 'rigid-body-with-state 'type 'RigidBody2D x Vec2 x Vec2 x Number x Number -> RigidBody2D)
(doc "Update all kinematic state at once.")
(define (rigid-body-with-state b new-pos new-vel new-angle new-angular-vel)
  (make-rigid-body new-pos
                   new-vel
                   new-angle
                   new-angular-vel
                   (rigid-body-mass b)
                   (rigid-body-inertia b)))

(doc 'section 'static)

(doc 'make-static-rigid-body 'type 'Vec2 x Number -> RigidBody2D)
(doc "Create a static (immovable) rigid body at given position and angle.")
(define (make-static-rigid-body pos angle)
  (make-rigid-body pos (vec2 0 0) angle 0 0 0))

(doc 'section 'moment)

(doc 'circle-inertia 'type 'Number x Number -> Number)
(doc "Moment of inertia for a solid circle: I = 0.5 * m * r^2")
(define (circle-inertia mass radius)
  (* 0.5 mass radius radius))

(doc 'rectangle-inertia 'type 'Number x Number x Number -> Number)
(doc "Moment of inertia for a rectangle: I = (1/12) * m * (w^2 + h^2)")
(define (rectangle-inertia mass width height)
  (* (/ 1 12) mass (+ (* width width) (* height height))))

(doc 'rod-inertia 'type 'Number x Number -> Number)
(doc "Moment of inertia for a thin rod about center: I = (1/12) * m * L^2")
(define (rod-inertia mass length)
  (* (/ 1 12) mass length length))

(doc 'point-mass-inertia 'type 'Number x Number -> Number)
(doc "Moment of inertia for a point mass at distance r: I = m * r^2")
(define (point-mass-inertia mass radius)
  (* mass radius radius))

(doc 'section 'coordinate)

(doc 'local-to-world 'type 'RigidBody2D x Vec2 -> Vec2)
(doc "Transform a point from local body space to world space.")
(doc "Applies rotation then translation.")
(define (local-to-world body local-point)
  (let ([angle (rigid-body-angle body)]
        [pos (rigid-body-pos body)])
       (vec2-add pos (vec2-rotate local-point angle))))

(doc 'world-to-local 'type 'RigidBody2D x Vec2 -> Vec2)
(doc "Transform a point from world space to local body space.")
(doc "Applies inverse translation then inverse rotation.")
(define (world-to-local body world-point)
  (let ([angle (rigid-body-angle body)]
        [pos (rigid-body-pos body)])
       (vec2-rotate (vec2-sub world-point pos) (- angle))))

(doc 'section 'velocity)

(doc 'rigid-body-velocity-at 'type 'RigidBody2D x Vec2 -> Vec2)
(doc "Get the velocity at a world-space point on the body.")
(doc "v_point = v_cm + omega x r")
(doc "In 2D, omega x r = omega * perp(r) where perp(x,y) = (-y, x)")
(define (rigid-body-velocity-at body world-point)
  (let* ([pos (rigid-body-pos body)]
         [vel (rigid-body-vel body)]
         [omega (rigid-body-angular-vel body)]
         [r (vec2-sub world-point pos)]
         ;; omega x r in 2D: omega * (-ry, rx)
         [tangent-vel (vec2-scale (vec2-perp r) omega)])
        (vec2-add vel tangent-vel)))

(doc 'section 'impulse)

(doc 'apply-linear-impulse 'type 'RigidBody2D x Vec2 -> RigidBody2D)
(doc "Apply a linear impulse at the center of mass.")
(doc "Delta-v = J / m")
(define (apply-linear-impulse body impulse)
  (if (rigid-body-static? body)
      body
      (let* ([inv-mass (rigid-body-inv-mass body)]
             [new-vel (vec2-add (rigid-body-vel body)
                                (vec2-scale impulse inv-mass))])
            (rigid-body-with-vel body new-vel))))

(doc 'apply-angular-impulse 'type 'RigidBody2D x Number -> RigidBody2D)
(doc "Apply an angular impulse (torque * dt).")
(doc "Delta-omega = L / I")
(define (apply-angular-impulse body angular-impulse)
  (if (rigid-body-static? body)
      body
      (let* ([inv-inertia (rigid-body-inv-inertia body)]
             [new-omega (+ (rigid-body-angular-vel body)
                           (* angular-impulse inv-inertia))])
            (rigid-body-with-angular-vel body new-omega))))

(doc 'apply-impulse-at-point 'type 'RigidBody2D x Vec2 x Vec2 -> RigidBody2D)
(doc "Apply an impulse at a world-space point.")
(doc "This produces both linear and angular effects.")
(doc "Delta-v = J / m")
(doc "Delta-omega = (r x J) / I")
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

(doc 'section 'energy)

(doc 'rigid-body-kinetic-energy 'type 'RigidBody2D -> Number)
(doc "Total kinetic energy: KE = 0.5 * m * v^2 + 0.5 * I * omega^2")
(define (rigid-body-kinetic-energy body)
  (let ([mass (rigid-body-mass body)]
        [vel (rigid-body-vel body)]
        [inertia (rigid-body-inertia body)]
        [omega (rigid-body-angular-vel body)])
       (+ (* 0.5 mass (vec2-magnitude-sq vel))
          (* 0.5 inertia omega omega))))

(doc 'rigid-body-linear-momentum 'type 'RigidBody2D -> Vec2)
(doc "Linear momentum: p = m * v")
(define (rigid-body-linear-momentum body)
  (vec2-scale (rigid-body-vel body) (rigid-body-mass body)))

(doc 'rigid-body-angular-momentum 'type 'RigidBody2D -> Number)
(doc "Angular momentum about center: L = I * omega")
(define (rigid-body-angular-momentum body)
  (* (rigid-body-inertia body) (rigid-body-angular-vel body)))

(doc 'section 'integration)

(doc 'integrate-rigid-body 'type 'RigidBody2D x Vec2 x Number x Number -> RigidBody2D)
(doc "Integrate body forward by dt with given linear acceleration and torque.")
(doc "Uses semi-implicit Euler: update velocity first, then position.")
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

(doc 'section 'conversion)

(doc "body-2d->rigid-body : Body2D x Number x Number -> RigidBody2D")
(doc "Convert a Body2D to RigidBody2D with given shape properties.")
(doc "Assumes Body2D has structure: (list 'body-2d pos vel mass inv-mass)")
(define (body-2d->rigid-body body-2d inertia)
  (let ([pos (list-ref body-2d 1)]
        [vel (list-ref body-2d 2)]
        [mass (list-ref body-2d 3)])
       (make-rigid-body pos vel 0 0 mass inertia)))

(doc "rigid-body->body-2d : RigidBody2D -> Body2D")
(doc "Convert back to Body2D (losing angular information).")
(define (rigid-body->body-2d rigid-body)
  (let ([pos (rigid-body-pos rigid-body)]
        [vel (rigid-body-vel rigid-body)]
        [mass (rigid-body-mass rigid-body)])
       (list 'body-2d pos vel mass
             (if (= mass 0) 0 (/ 1 mass)))))
