(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module lenses
;;; @requires templates protocol-bundle vec2 rigid-body particles
(require 'templates)
(require 'protocol-bundle)
(require 'vec2)
(require 'rigid-body)
(require 'particles)

(doc 'module 'lenses)
(doc 'description "Lens library for functional access to physics state. Enables elegant composition of getters and setters for nested physics structures.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Usage: (load \"lattice/physics/lenses.ss\")")
(doc 'note "View (get) through a lens: (view rigid-body-pos-lens body) => Vec2")
(doc 'note "Set through a lens: (set-lens rigid-body-vel-lens (vec2 1 0) body)")
(doc 'note "Modify through a lens: (over rigid-body-pos-lens (lambda (p) (vec2-add p gravity)) body)")
(doc 'note "Compose lenses for deep access: (view (lens-compose rigid-body-pos-lens vec2-x-lens) body)")
(doc 'note "Dot notation: (over (body. pos x) (lambda (x) (+ x 10)) body)")
(doc 'note "Open Protocol System: Generic lenses use lattice/fp/protocol.ss")
(doc 'note "New body types can register: (derive-bundle! body-ops 'my-body-type my-body-prefix)")
(doc 'note "With overrides: (derive-bundle! body-ops 'my-body-type my-body (\"mass\" custom-mass-getter custom-mass-setter))")

(doc 'section 'vec2-lenses)

(doc vec2-x-lens 'type '(Lens Vec2 Number))
(doc vec2-x-lens 'description "Focus on the x component of a Vec2")
(define vec2-x-lens
  (make-lens
   vec2-x
   (lambda (new-x v) (vec2 new-x (vec2-y v)))))

(doc vec2-y-lens 'type '(Lens Vec2 Number))
(doc vec2-y-lens 'description "Focus on the y component of a Vec2")
(define vec2-y-lens
  (make-lens
   vec2-y
   (lambda (new-y v) (vec2 (vec2-x v) new-y))))

(doc 'section 'rigid-body-lenses)

(doc rigid-body-pos-lens 'type '(Lens RigidBody2D Vec2))
(doc rigid-body-pos-lens 'description "Focus on position")
(define rigid-body-pos-lens
  (make-lens
   rigid-body-pos
   (lambda (new-pos b) (rigid-body-with-pos b new-pos))))

(doc rigid-body-vel-lens 'type '(Lens RigidBody2D Vec2))
(doc rigid-body-vel-lens 'description "Focus on velocity")
(define rigid-body-vel-lens
  (make-lens
   rigid-body-vel
   (lambda (new-vel b) (rigid-body-with-vel b new-vel))))

(doc rigid-body-angle-lens 'type '(Lens RigidBody2D Number))
(doc rigid-body-angle-lens 'description "Focus on rotation angle (radians)")
(define rigid-body-angle-lens
  (make-lens
   rigid-body-angle
   (lambda (new-angle b) (rigid-body-with-angle b new-angle))))

(doc rigid-body-angular-vel-lens 'type '(Lens RigidBody2D Number))
(doc rigid-body-angular-vel-lens 'description "Focus on angular velocity")
(define rigid-body-angular-vel-lens
  (make-lens
   rigid-body-angular-vel
   (lambda (new-omega b) (rigid-body-with-angular-vel b new-omega))))

(doc rigid-body-mass-lens 'type '(Lens RigidBody2D Number))
(doc rigid-body-mass-lens 'description "Focus on mass (read-only for static bodies)")
(doc rigid-body-mass-lens 'note "Setting mass requires recalculating inv-mass and potentially inertia")
(define rigid-body-mass-lens
  (make-lens
   rigid-body-mass
   (lambda (new-mass b)
     (make-rigid-body (rigid-body-pos b)
                      (rigid-body-vel b)
                      (rigid-body-angle b)
                      (rigid-body-angular-vel b)
                      new-mass
                      (rigid-body-inertia b)))))

(doc rigid-body-inertia-lens 'type '(Lens RigidBody2D Number))
(doc rigid-body-inertia-lens 'description "Focus on moment of inertia")
(define rigid-body-inertia-lens
  (make-lens
   rigid-body-inertia
   (lambda (new-inertia b)
     (make-rigid-body (rigid-body-pos b)
                      (rigid-body-vel b)
                      (rigid-body-angle b)
                      (rigid-body-angular-vel b)
                      (rigid-body-mass b)
                      new-inertia))))

(doc 'section 'rigid-body-composed-lenses)

(doc rigid-body-pos-x-lens 'type '(Lens RigidBody2D Number))
(doc rigid-body-pos-x-lens 'description "Focus on position's x component")
(define rigid-body-pos-x-lens
  (lens-compose rigid-body-pos-lens vec2-x-lens))

(doc rigid-body-pos-y-lens 'type '(Lens RigidBody2D Number))
(doc rigid-body-pos-y-lens 'description "Focus on position's y component")
(define rigid-body-pos-y-lens
  (lens-compose rigid-body-pos-lens vec2-y-lens))

(doc rigid-body-vel-x-lens 'type '(Lens RigidBody2D Number))
(doc rigid-body-vel-x-lens 'description "Focus on velocity's x component")
(define rigid-body-vel-x-lens
  (lens-compose rigid-body-vel-lens vec2-x-lens))

(doc rigid-body-vel-y-lens 'type '(Lens RigidBody2D Number))
(doc rigid-body-vel-y-lens 'description "Focus on velocity's y component")
(define rigid-body-vel-y-lens
  (lens-compose rigid-body-vel-lens vec2-y-lens))

(doc 'section 'particle-lenses)

(doc particle-pos-lens 'type '(Lens Particle Vec2))
(doc particle-pos-lens 'description "Focus on particle position")
(define particle-pos-lens
  (make-lens
   particle-pos
   (lambda (new-pos p) (particle-with-pos p new-pos))))

(doc particle-vel-lens 'type '(Lens Particle Vec2))
(doc particle-vel-lens 'description "Focus on particle velocity")
(define particle-vel-lens
  (make-lens
   particle-vel
   (lambda (new-vel p) (particle-with-vel p new-vel))))

(doc particle-lifetime-lens 'type '(Lens Particle Number))
(doc particle-lifetime-lens 'description "Focus on remaining lifetime")
(define particle-lifetime-lens
  (make-lens
   particle-lifetime
   (lambda (new-life p)
     (make-particle (particle-pos p)
                    (particle-vel p)
                    new-life
                    (particle-max-life p)
                    (particle-size p)
                    (particle-color p)
                    (particle-user-data p)))))

(doc particle-size-lens 'type '(Lens Particle Number))
(doc particle-size-lens 'description "Focus on particle size")
(define particle-size-lens
  (make-lens
   particle-size
   (lambda (new-size p)
     (make-particle (particle-pos p)
                    (particle-vel p)
                    (particle-lifetime p)
                    (particle-max-life p)
                    new-size
                    (particle-color p)
                    (particle-user-data p)))))

(doc particle-color-lens 'type '(Lens Particle Any))
(doc particle-color-lens 'description "Focus on particle color")
(define particle-color-lens
  (make-lens
   particle-color
   (lambda (new-color p)
     (make-particle (particle-pos p)
                    (particle-vel p)
                    (particle-lifetime p)
                    (particle-max-life p)
                    (particle-size p)
                    new-color
                    (particle-user-data p)))))

(doc 'section 'particle-composed-lenses)

(doc particle-pos-x-lens 'type '(Lens Particle Number))
(define particle-pos-x-lens
  (lens-compose particle-pos-lens vec2-x-lens))

(doc particle-pos-y-lens 'type '(Lens Particle Number))
(define particle-pos-y-lens
  (lens-compose particle-pos-lens vec2-y-lens))

(doc particle-vel-x-lens 'type '(Lens Particle Number))
(define particle-vel-x-lens
  (lens-compose particle-vel-lens vec2-x-lens))

(doc particle-vel-y-lens 'type '(Lens Particle Number))
(define particle-vel-y-lens
  (lens-compose particle-vel-lens vec2-y-lens))

(doc particle-mass-lens 'type '(Lens Particle Number))
(doc particle-mass-lens 'description "Virtual lens: particles have implicit unit mass (1.0)")
(doc particle-mass-lens 'note "Getter always returns 1.0, setter is a no-op (mass is conceptual)")
(doc particle-mass-lens 'note "This enables polymorphic use with apply-force-via-lens")
(define particle-mass-lens
  (make-lens
   (lambda (p) 1.0)
   (lambda (new-m p) p)))

(doc 'section 'open-protocol-definitions)
(doc 'note "These protocols enable extensible dispatch for generic body operations")
(doc 'note "New body types register implementations via implement-protocol!")

(doc body-pos 'type '(-> Body Vec2))
(doc body-pos 'description "Protocol for getting body position")
(define-protocol (body-pos b) "Get body position")

(doc body-set-pos 'type '(-> Body Vec2 Body))
(doc body-set-pos 'description "Protocol for setting body position")
(define-protocol (body-set-pos b p) "Set body position")

(doc body-vel 'type '(-> Body Vec2))
(doc body-vel 'description "Protocol for getting body velocity")
(define-protocol (body-vel b) "Get body velocity")

(doc body-set-vel 'type '(-> Body Vec2 Body))
(doc body-set-vel 'description "Protocol for setting body velocity")
(define-protocol (body-set-vel b v) "Set body velocity")

(doc body-mass 'type '(-> Body Number))
(doc body-mass 'description "Protocol for getting body mass")
(define-protocol (body-mass b) "Get body mass")

(doc body-set-mass 'type '(-> Body Number Body))
(doc body-set-mass 'description "Protocol for setting body mass")
(define-protocol (body-set-mass b m) "Set body mass")

(doc 'section 'rotational-protocols)
(doc 'note "Protocols for rotational dynamics - angle, angular velocity, inertia")
(doc 'note "Not all body types support rotation; check with implements-bundle?")

(doc body-angle 'type '(-> Body Number))
(doc body-angle 'description "Protocol for getting body rotation angle (radians)")
(define-protocol (body-angle b) "Get body rotation angle")

(doc body-set-angle 'type '(-> Body Number Body))
(doc body-set-angle 'description "Protocol for setting body rotation angle")
(define-protocol (body-set-angle b a) "Set body rotation angle")

(doc body-angular-vel 'type '(-> Body Number))
(doc body-angular-vel 'description "Protocol for getting angular velocity (rad/s)")
(define-protocol (body-angular-vel b) "Get body angular velocity")

(doc body-set-angular-vel 'type '(-> Body Number Body))
(doc body-set-angular-vel 'description "Protocol for setting angular velocity")
(define-protocol (body-set-angular-vel b omega) "Set body angular velocity")

(doc body-inertia 'type '(-> Body Number))
(doc body-inertia 'description "Protocol for getting moment of inertia")
(define-protocol (body-inertia b) "Get body moment of inertia")

(doc body-set-inertia 'type '(-> Body Number Body))
(doc body-set-inertia 'description "Protocol for setting moment of inertia")
(define-protocol (body-set-inertia b i) "Set body moment of inertia")

(doc 'section 'body-operations-bundle)
(doc 'note "Defines the standard body protocol pairs")
(doc 'note "New body types register implementations using derive-bundle! or implement-bundle!")

(define-protocol-bundle body-ops
  ((body-pos body-set-pos) "pos")
  ((body-vel body-set-vel) "vel")
  ((body-mass body-set-mass) "mass"))

(doc 'section 'rotational-body-operations-bundle)
(doc 'note "Protocol bundle for rotational dynamics")
(doc 'note "Separate from body-ops since not all bodies support rotation")

(define-protocol-bundle rotational-body-ops
  ((body-angle body-set-angle) "angle")
  ((body-angular-vel body-set-angular-vel) "angular-vel")
  ((body-inertia body-set-inertia) "inertia"))

(doc 'section 'protocol-implementations-rigid-body)
(doc 'note "Uses naming convention: rigid-body-<field>, rigid-body-with-<field>")
(doc 'note "All slots follow convention - no overrides needed")

(derive-bundle! body-ops 'rigid-body-2d rigid-body)
(derive-bundle! rotational-body-ops 'rigid-body-2d rigid-body)

(doc 'section 'protocol-implementations-particle)
(doc 'note "Particles have implicit unit mass (1.0), so we override the mass slot")
(doc 'note "Particles do NOT implement rotational-body-ops - they are point masses")
(doc 'note "Use (implements-bundle? 'particle rotational-body-ops) to check")

(derive-bundle! body-ops 'particle particle
  ("mass" (lambda (p) 1.0) (lambda (p m) p)))

;; NOTE: Particles intentionally do NOT implement rotational-body-ops.
;; Rationale: Particles are point masses with no spatial extent. Rotation
;; is physically meaningless for a point. Implementing a "silent no-op"
;; would violate lens laws (put-get) and hide bugs in physics code.
;; Code that needs rotation should check (implements-bundle? ...) explicitly.

(doc 'section 'generic-body-lenses)
(doc 'note "These lenses work with any body type that implements the protocols")
(doc 'note "New body types automatically work with these lenses after registering their protocol implementations")

(doc body-pos-lens 'type '(Lens Body Vec2))
(doc body-pos-lens 'description "Generic lens for body position (works with any body type)")
(define body-pos-lens
  (make-lens
   body-pos
   (lambda (new-pos b) (body-set-pos b new-pos))))

(doc body-vel-lens 'type '(Lens Body Vec2))
(doc body-vel-lens 'description "Generic lens for body velocity")
(define body-vel-lens
  (make-lens
   body-vel
   (lambda (new-vel b) (body-set-vel b new-vel))))

(doc body-mass-lens 'type '(Lens Body Number))
(doc body-mass-lens 'description "Generic lens for body mass")
(doc body-mass-lens 'note "Particles have implicit mass 1.0 (read-only)")
(define body-mass-lens
  (make-lens
   body-mass
   (lambda (new-mass b) (body-set-mass b new-mass))))

(doc 'section 'generic-rotational-lenses)
(doc 'note "Generic lenses for rotational dynamics")
(doc 'note "Only work with body types implementing rotational-body-ops")
(doc 'note "Check (implements-bundle? type-tag rotational-body-ops) before use")

(doc body-angle-lens 'type '(Lens RotationalBody Number))
(doc body-angle-lens 'description "Generic lens for body rotation angle (radians)")
(define body-angle-lens
  (make-lens
   body-angle
   (lambda (new-angle b) (body-set-angle b new-angle))))

(doc body-angular-vel-lens 'type '(Lens RotationalBody Number))
(doc body-angular-vel-lens 'description "Generic lens for angular velocity (rad/s)")
(define body-angular-vel-lens
  (make-lens
   body-angular-vel
   (lambda (new-omega b) (body-set-angular-vel b new-omega))))

(doc body-inertia-lens 'type '(Lens RotationalBody Number))
(doc body-inertia-lens 'description "Generic lens for moment of inertia")
(doc body-inertia-lens 'note "inertia=0 means static/infinite inertia (no rotation)")
(define body-inertia-lens
  (make-lens
   body-inertia
   (lambda (new-inertia b) (body-set-inertia b new-inertia))))

(doc 'section 'dot-notation-macro)

(doc body. 'type '(-> Symbol ... Lens))
(doc body. 'description "Convenience macro for composing physics lenses using dot notation")
(doc body. 'note "(body. pos x) => (lens-compose body-pos-lens vec2-x-lens)")
(doc body. 'note "Supported paths (generic - work with any body type):")
(doc body. 'note "(body. pos) -> body-pos-lens")
(doc body. 'note "(body. vel) -> body-vel-lens")
(doc body. 'note "(body. mass) -> body-mass-lens")
(doc body. 'note "(body. pos x) -> (lens-compose body-pos-lens vec2-x-lens)")
(doc body. 'note "(body. pos y) -> (lens-compose body-pos-lens vec2-y-lens)")
(doc body. 'note "(body. vel x) -> (lens-compose body-vel-lens vec2-x-lens)")
(doc body. 'note "(body. vel y) -> (lens-compose body-vel-lens vec2-y-lens)")
(doc body. 'note "Rigid-body specific: (body. angle), (body. angular-vel), (body. inertia)")
(doc body. 'note "Particle specific: (body. lifetime), (body. size), (body. color)")

(define-syntax body.
  (syntax-rules (pos vel x y angle angular-vel mass inertia lifetime size color)
    ;; Position paths
    [(body. pos) body-pos-lens]
    [(body. pos x) (lens-compose body-pos-lens vec2-x-lens)]
    [(body. pos y) (lens-compose body-pos-lens vec2-y-lens)]
    ;; Velocity paths
    [(body. vel) body-vel-lens]
    [(body. vel x) (lens-compose body-vel-lens vec2-x-lens)]
    [(body. vel y) (lens-compose body-vel-lens vec2-y-lens)]
    ;; Generic (works with any body type)
    [(body. mass) body-mass-lens]
    ;; Rotational dynamics (generic - works with any rotational body)
    [(body. angle) body-angle-lens]
    [(body. angular-vel) body-angular-vel-lens]
    [(body. inertia) body-inertia-lens]
    ;; Particle-specific
    [(body. lifetime) particle-lifetime-lens]
    [(body. size) particle-size-lens]
    [(body. color) particle-color-lens]))

(doc 'section 'lens-based-transformations)

(doc apply-force-via-lens 'type '(-> (Lens Body Vec2) (Lens Body Number) Vec2 Number Body Body))
(doc apply-force-via-lens 'description "Apply a force to a body using lens-based access")
(doc apply-force-via-lens 'note "force = mass * accel, so delta-v = force * dt / mass")
(define (apply-force-via-lens vel-lens mass-lens force dt body)
  (doc 'export #t)
  (let* ([mass (view mass-lens body)]
         [inv-mass (if (= mass 0) 0 (/ 1 mass))]
         [delta-v (vec2-scale force (* dt inv-mass))])
    (over vel-lens (lambda (v) (vec2-add v delta-v)) body)))

(doc integrate-position-via-lens 'type '(-> (Lens Body Vec2) (Lens Body Vec2) Number Body Body))
(doc integrate-position-via-lens 'description "Euler integration of position using lenses")
(define (integrate-position-via-lens pos-lens vel-lens dt body)
  (doc 'export #t)
  (let ([vel (view vel-lens body)])
    (over pos-lens (lambda (p) (vec2-add p (vec2-scale vel dt))) body)))

(doc 'section 'rotational-lens-transformations)

(doc apply-torque-via-lens 'type '(-> (Lens Body Number) (Lens Body Number) Number Number Body Body))
(doc apply-torque-via-lens 'description "Apply torque to a body using lens-based access")
(doc apply-torque-via-lens 'note "torque = inertia * angular-accel, so delta-omega = torque * dt / inertia")
(define (apply-torque-via-lens angular-vel-lens inertia-lens torque dt body)
  (doc 'export #t)
  (let* ([inertia (view inertia-lens body)]
         [inv-inertia (if (= inertia 0) 0 (/ 1 inertia))]
         [delta-omega (* torque dt inv-inertia)])
    (over angular-vel-lens (lambda (omega) (+ omega delta-omega)) body)))

(doc integrate-rotation-via-lens 'type '(-> (Lens Body Number) (Lens Body Number) Number Body Body))
(doc integrate-rotation-via-lens 'description "Euler integration of rotation using lenses")
(define (integrate-rotation-via-lens angle-lens angular-vel-lens dt body)
  (doc 'export #t)
  (let ([omega (view angular-vel-lens body)])
    (over angle-lens (lambda (a) (+ a (* omega dt))) body)))

(doc integrate-body-via-lens 'type '(-> (Lens Body Vec2) (Lens Body Vec2) (Lens Body Number) (Lens Body Number) Number Body Body))
(doc integrate-body-via-lens 'description "Full Euler integration of position and rotation")
(define (integrate-body-via-lens pos-lens vel-lens angle-lens angular-vel-lens dt body)
  (doc 'export #t)
  (let* ([body1 (integrate-position-via-lens pos-lens vel-lens dt body)]
         [body2 (integrate-rotation-via-lens angle-lens angular-vel-lens dt body1)])
    body2))

(doc apply-impulse-at-point-via-lens 'type '(-> (Lens Body Vec2) (Lens Body Vec2) (Lens Body Number) (Lens Body Number) (Lens Body Number) Vec2 Vec2 Body Body))
(doc apply-impulse-at-point-via-lens 'description "Apply impulse at a world point using lenses")
(doc apply-impulse-at-point-via-lens 'note "Produces both linear and angular effects: delta-v = J/m, delta-omega = (r x J)/I")
(doc apply-impulse-at-point-via-lens 'note "Only use on bodies implementing rotational-body-ops")
(define (apply-impulse-at-point-via-lens pos-lens vel-lens mass-lens angular-vel-lens inertia-lens impulse world-point body)
  (doc 'export #t)
  (let* ([pos (view pos-lens body)]
         [mass (view mass-lens body)]
         [inertia (view inertia-lens body)]
         [inv-mass (if (= mass 0) 0 (/ 1 mass))]
         [inv-inertia (if (= inertia 0) 0 (/ 1 inertia))]
         [r (vec2-sub world-point pos)]
         ;; Linear impulse
         [delta-v (vec2-scale impulse inv-mass)]
         ;; Angular impulse: r x J (2D cross product gives scalar)
         [torque-impulse (vec2-cross r impulse)]
         [delta-omega (* torque-impulse inv-inertia)]
         ;; Apply both
         [body1 (over vel-lens (lambda (v) (vec2-add v delta-v)) body)]
         [body2 (over angular-vel-lens (lambda (omega) (+ omega delta-omega)) body1)])
    body2))

(doc 'section 'generic-physics-helpers)
(doc 'note "Convenience functions using default generic lenses")
(doc 'note "Automatically check for rotation support via implements-bundle?")

(doc rotates? 'type '(-> Body Boolean))
(doc rotates? 'description "Check if a body supports rotational dynamics")
(define (rotates? body)
  (doc 'export #t)
  (implements-bundle? (get-type-tag body) rotational-body-ops))

(doc apply-torque 'type '(-> Number Number Body Body))
(doc apply-torque 'description "Apply torque to a body (no-op if body doesn't rotate)")
(define (apply-torque torque dt body)
  (doc 'export #t)
  (if (rotates? body)
      (apply-torque-via-lens body-angular-vel-lens body-inertia-lens torque dt body)
      body))

(doc integrate-body 'type '(-> Number Body Body))
(doc integrate-body 'description "Integrate position and rotation (if supported)")
(define (integrate-body dt body)
  (doc 'export #t)
  (let ([body1 (integrate-position-via-lens body-pos-lens body-vel-lens dt body)])
    (if (rotates? body1)
        (integrate-rotation-via-lens body-angle-lens body-angular-vel-lens dt body1)
        body1)))

(doc apply-impulse-at-point 'type '(-> Vec2 Vec2 Body Body))
(doc apply-impulse-at-point 'description "Apply impulse at world point (linear only if body doesn't rotate)")
(define (apply-impulse-at-point impulse world-point body)
  (doc 'export #t)
  (let* ([pos (view body-pos-lens body)]
         [mass (view body-mass-lens body)]
         [inv-mass (if (= mass 0) 0 (/ 1 mass))]
         [delta-v (vec2-scale impulse inv-mass)]
         [body1 (over body-vel-lens (lambda (v) (vec2-add v delta-v)) body)])
    (if (rotates? body1)
        (let* ([r (vec2-sub world-point pos)]
               [inertia (view body-inertia-lens body1)]
               [inv-inertia (if (= inertia 0) 0 (/ 1 inertia))]
               [torque-impulse (vec2-cross r impulse)]
               [delta-omega (* torque-impulse inv-inertia)])
          (over body-angular-vel-lens (lambda (omega) (+ omega delta-omega)) body1))
        body1)))

(doc 'section 'aliases)
(doc 'note "Backward compatibility aliases")

(define position-lens body-pos-lens)
(define velocity-lens body-vel-lens)
(define mass-lens body-mass-lens)
(define rotation-lens body-angle-lens)
(define angular-velocity-lens body-angular-vel-lens)
(define moment-of-inertia-lens body-inertia-lens)

(doc 'section 'print-help)

(display "  Vec2:        vec2-x-lens, vec2-y-lens\n")
(display "  RigidBody:   rigid-body-pos-lens, rigid-body-vel-lens\n")
(display "               rigid-body-angle-lens, rigid-body-angular-vel-lens\n")
(display "               rigid-body-mass-lens, rigid-body-inertia-lens\n")
(display "  Particle:    particle-pos-lens, particle-vel-lens, particle-mass-lens\n")
(display "               particle-lifetime-lens, particle-size-lens\n")
(display "               (NOTE: particles do NOT support rotation)\n")
(display "  Generic:     body-pos-lens, body-vel-lens, body-mass-lens\n")
(display "  Rotational:  body-angle-lens, body-angular-vel-lens, body-inertia-lens\n")
(display "  Helpers:     (rotates? body), (apply-torque torque dt body)\n")
(display "               (integrate-body dt body), (apply-impulse-at-point J pt body)\n")
(display "  Bundles:     body-ops, rotational-body-ops\n")
(display "  Check:       (implements-bundle? 'particle rotational-body-ops) => #f\n")
(display "  Extend:      (derive-bundle! body-ops 'my-type my-prefix)\n")
(display "               (derive-bundle! rotational-body-ops 'my-type my-prefix)\n")
