;;; lattice/physics/lenses.ss — Optics for Physics Entities
;;;
;;; Lens library for functional access to physics state. Enables elegant
;;; composition of getters and setters for nested physics structures.
;;;
;;; Usage:
;;;   (load "lattice/physics/lenses.ss")
;;;
;;;   ;; View (get) through a lens
;;;   (view rigid-body-pos-lens body)           ; => Vec2
;;;   (view rigid-body-pos-x-lens body)         ; => Number
;;;
;;;   ;; Set through a lens
;;;   (set-lens rigid-body-vel-lens (vec2 1 0) body)
;;;
;;;   ;; Modify through a lens
;;;   (over rigid-body-pos-lens (lambda (p) (vec2-add p gravity)) body)
;;;   (over particle-lifetime-lens (lambda (t) (- t dt)) particle)
;;;
;;;   ;; Compose lenses for deep access
;;;   (view (lens-compose rigid-body-pos-lens vec2-x-lens) body)
;;;   (over (body. pos x) (lambda (x) (+ x 10)) body)  ; Dot notation
;;;
;;; This is Lattice code: pure, functional.
;;;
;;; Open Protocol System:
;;;   Generic lenses use the open protocol system from lattice/fp/protocol.ss.
;;;   New body types can register implementations without modifying this file:
;;;
;;;   (implement-protocol! 'body-pos 'my-body-type my-pos-getter)
;;;   (implement-protocol! 'body-set-pos 'my-body-type my-pos-setter)
;;;
;;; Dependencies:
;;;   - lattice/fp/templates.ss (lens infrastructure)
;;;   - lattice/fp/protocol.ss (open dispatch)
;;;   - lattice/linalg/vec2.ss
;;;   - lattice/physics/classical/rigid-body.ss
;;;   - lattice/physics/classical/particles.ss

(load "lattice/fp/templates.ss")
(load "lattice/fp/protocol.ss")
(load "lattice/linalg/vec2.ss")
(load "lattice/physics/classical/rigid-body.ss")
(load "lattice/physics/classical/particles.ss")

;;; ====
;;; Vec2 Component Lenses
;;; ====

;;; vec2-x-lens : Lens Vec2 Number
;;; Focus on the x component of a Vec2.
(define vec2-x-lens
  (make-lens
   vec2-x
   (lambda (new-x v) (vec2 new-x (vec2-y v)))))

;;; vec2-y-lens : Lens Vec2 Number
;;; Focus on the y component of a Vec2.
(define vec2-y-lens
  (make-lens
   vec2-y
   (lambda (new-y v) (vec2 (vec2-x v) new-y))))

;;; ====
;;; RigidBody2D Lenses
;;; ====

;;; rigid-body-pos-lens : Lens RigidBody2D Vec2
;;; Focus on position.
(define rigid-body-pos-lens
  (make-lens
   rigid-body-pos
   (lambda (new-pos b) (rigid-body-with-pos b new-pos))))

;;; rigid-body-vel-lens : Lens RigidBody2D Vec2
;;; Focus on velocity.
(define rigid-body-vel-lens
  (make-lens
   rigid-body-vel
   (lambda (new-vel b) (rigid-body-with-vel b new-vel))))

;;; rigid-body-angle-lens : Lens RigidBody2D Number
;;; Focus on rotation angle (radians).
(define rigid-body-angle-lens
  (make-lens
   rigid-body-angle
   (lambda (new-angle b) (rigid-body-with-angle b new-angle))))

;;; rigid-body-angular-vel-lens : Lens RigidBody2D Number
;;; Focus on angular velocity.
(define rigid-body-angular-vel-lens
  (make-lens
   rigid-body-angular-vel
   (lambda (new-omega b) (rigid-body-with-angular-vel b new-omega))))

;;; rigid-body-mass-lens : Lens RigidBody2D Number
;;; Focus on mass (read-only for static bodies).
;;; Note: Setting mass requires recalculating inv-mass and potentially inertia.
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

;;; rigid-body-inertia-lens : Lens RigidBody2D Number
;;; Focus on moment of inertia.
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

;;; ====
;;; RigidBody2D Composed Lenses (Convenience)
;;; ====

;;; rigid-body-pos-x-lens : Lens RigidBody2D Number
;;; Focus on position's x component.
(define rigid-body-pos-x-lens
  (lens-compose rigid-body-pos-lens vec2-x-lens))

;;; rigid-body-pos-y-lens : Lens RigidBody2D Number
;;; Focus on position's y component.
(define rigid-body-pos-y-lens
  (lens-compose rigid-body-pos-lens vec2-y-lens))

;;; rigid-body-vel-x-lens : Lens RigidBody2D Number
;;; Focus on velocity's x component.
(define rigid-body-vel-x-lens
  (lens-compose rigid-body-vel-lens vec2-x-lens))

;;; rigid-body-vel-y-lens : Lens RigidBody2D Number
;;; Focus on velocity's y component.
(define rigid-body-vel-y-lens
  (lens-compose rigid-body-vel-lens vec2-y-lens))

;;; ====
;;; Particle Lenses
;;; ====

;;; particle-pos-lens : Lens Particle Vec2
;;; Focus on particle position.
(define particle-pos-lens
  (make-lens
   particle-pos
   (lambda (new-pos p) (particle-with-pos p new-pos))))

;;; particle-vel-lens : Lens Particle Vec2
;;; Focus on particle velocity.
(define particle-vel-lens
  (make-lens
   particle-vel
   (lambda (new-vel p) (particle-with-vel p new-vel))))

;;; particle-lifetime-lens : Lens Particle Number
;;; Focus on remaining lifetime.
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

;;; particle-size-lens : Lens Particle Number
;;; Focus on particle size.
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

;;; particle-color-lens : Lens Particle Any
;;; Focus on particle color.
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

;;; ====
;;; Particle Composed Lenses
;;; ====

;;; particle-pos-x-lens : Lens Particle Number
(define particle-pos-x-lens
  (lens-compose particle-pos-lens vec2-x-lens))

;;; particle-pos-y-lens : Lens Particle Number
(define particle-pos-y-lens
  (lens-compose particle-pos-lens vec2-y-lens))

;;; particle-vel-x-lens : Lens Particle Number
(define particle-vel-x-lens
  (lens-compose particle-vel-lens vec2-x-lens))

;;; particle-vel-y-lens : Lens Particle Number
(define particle-vel-y-lens
  (lens-compose particle-vel-lens vec2-y-lens))

;;; particle-mass-lens : Lens Particle Number
;;; Virtual lens: particles have implicit unit mass (1.0).
;;; Getter always returns 1.0, setter is a no-op (mass is conceptual).
;;; This enables polymorphic use with apply-force-via-lens.
(define particle-mass-lens
  (make-lens
   (lambda (p) 1.0)            ; Particles have implicit mass of 1.0
   (lambda (new-m p) p)))      ; No-op: cannot change implicit mass

;;; ====
;;; Open Protocol Definitions
;;; ====
;;;
;;; These protocols enable extensible dispatch for generic body operations.
;;; New body types register implementations via implement-protocol!

;;; body-pos : Body → Vec2
;;; Protocol for getting body position.
(define-protocol (body-pos b) "Get body position")

;;; body-set-pos : Body × Vec2 → Body
;;; Protocol for setting body position.
(define-protocol (body-set-pos b p) "Set body position")

;;; body-vel : Body → Vec2
;;; Protocol for getting body velocity.
(define-protocol (body-vel b) "Get body velocity")

;;; body-set-vel : Body × Vec2 → Body
;;; Protocol for setting body velocity.
(define-protocol (body-set-vel b v) "Set body velocity")

;;; body-mass : Body → Number
;;; Protocol for getting body mass.
(define-protocol (body-mass b) "Get body mass")

;;; body-set-mass : Body × Number → Body
;;; Protocol for setting body mass.
(define-protocol (body-set-mass b m) "Set body mass")

;;; ====
;;; Protocol Implementations: RigidBody2D
;;; ====

(implement-protocol! 'body-pos 'rigid-body-2d
  rigid-body-pos)

(implement-protocol! 'body-set-pos 'rigid-body-2d
  (lambda (b p) (rigid-body-with-pos b p)))

(implement-protocol! 'body-vel 'rigid-body-2d
  rigid-body-vel)

(implement-protocol! 'body-set-vel 'rigid-body-2d
  (lambda (b v) (rigid-body-with-vel b v)))

(implement-protocol! 'body-mass 'rigid-body-2d
  rigid-body-mass)

(implement-protocol! 'body-set-mass 'rigid-body-2d
  (lambda (b m)
    (make-rigid-body (rigid-body-pos b)
                     (rigid-body-vel b)
                     (rigid-body-angle b)
                     (rigid-body-angular-vel b)
                     m
                     (rigid-body-inertia b))))

;;; ====
;;; Protocol Implementations: Particle
;;; ====

(implement-protocol! 'body-pos 'particle
  particle-pos)

(implement-protocol! 'body-set-pos 'particle
  (lambda (p pos) (particle-with-pos p pos)))

(implement-protocol! 'body-vel 'particle
  particle-vel)

(implement-protocol! 'body-set-vel 'particle
  (lambda (p vel) (particle-with-vel p vel)))

(implement-protocol! 'body-mass 'particle
  (lambda (p) 1.0))  ; Particles have implicit unit mass

(implement-protocol! 'body-set-mass 'particle
  (lambda (p m) p))  ; No-op: cannot change implicit mass

;;; ====
;;; Generic Body Lenses (Protocol-based)
;;; ====
;;;
;;; These lenses work with any body type that implements the protocols.
;;; New body types automatically work with these lenses after registering
;;; their protocol implementations.

;;; body-pos-lens : Lens Body Vec2
;;; Generic lens for body position (works with any body type).
(define body-pos-lens
  (make-lens
   body-pos
   (lambda (new-pos b) (body-set-pos b new-pos))))

;;; body-vel-lens : Lens Body Vec2
;;; Generic lens for body velocity.
(define body-vel-lens
  (make-lens
   body-vel
   (lambda (new-vel b) (body-set-vel b new-vel))))

;;; body-mass-lens : Lens Body Number
;;; Generic lens for body mass. Particles have implicit mass 1.0 (read-only).
(define body-mass-lens
  (make-lens
   body-mass
   (lambda (new-mass b) (body-set-mass b new-mass))))

;;; ====
;;; Dot Notation Macro
;;; ====

;;; body. : Symbol ... -> Lens
;;; Convenience macro for composing physics lenses using dot notation.
;;; (body. pos x) => (lens-compose body-pos-lens vec2-x-lens)
;;;
;;; Supported paths (generic - work with any body type):
;;;   (body. pos)       -> body-pos-lens
;;;   (body. vel)       -> body-vel-lens
;;;   (body. mass)      -> body-mass-lens
;;;   (body. pos x)     -> (lens-compose body-pos-lens vec2-x-lens)
;;;   (body. pos y)     -> (lens-compose body-pos-lens vec2-y-lens)
;;;   (body. vel x)     -> (lens-compose body-vel-lens vec2-x-lens)
;;;   (body. vel y)     -> (lens-compose body-vel-lens vec2-y-lens)
;;;
;;; Rigid-body specific:
;;;   (body. angle)     -> rigid-body-angle-lens
;;;   (body. angular-vel) -> rigid-body-angular-vel-lens
;;;   (body. inertia)   -> rigid-body-inertia-lens
;;;
;;; Particle specific:
;;;   (body. lifetime)  -> particle-lifetime-lens
;;;   (body. size)      -> particle-size-lens
;;;   (body. color)     -> particle-color-lens

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
    ;; RigidBody-specific
    [(body. angle) rigid-body-angle-lens]
    [(body. angular-vel) rigid-body-angular-vel-lens]
    [(body. inertia) rigid-body-inertia-lens]
    ;; Particle-specific
    [(body. lifetime) particle-lifetime-lens]
    [(body. size) particle-size-lens]
    [(body. color) particle-color-lens]))

;;; ====
;;; Utility: Lens-based Transformations
;;; ====

;;; apply-force-lens : Lens Body Vec2 -> Vec2 -> Number -> Number -> Body -> Body
;;; Apply a force to a body using lens-based access.
;;; force = mass * accel, so delta-v = force * dt / mass
(define (apply-force-via-lens vel-lens mass-lens force dt body)
  (let* ([mass (view mass-lens body)]
         [inv-mass (if (= mass 0) 0 (/ 1 mass))]
         [delta-v (vec2-scale force (* dt inv-mass))])
    (over vel-lens (lambda (v) (vec2-add v delta-v)) body)))

;;; integrate-position-lens : Lens Body Vec2 -> Lens Body Vec2 -> Number -> Body -> Body
;;; Euler integration of position using lenses.
(define (integrate-position-via-lens pos-lens vel-lens dt body)
  (let ([vel (view vel-lens body)])
    (over pos-lens (lambda (p) (vec2-add p (vec2-scale vel dt))) body)))

;;; ====
;;; Aliases (Backward Compatibility)
;;; ====

(define position-lens body-pos-lens)
(define velocity-lens body-vel-lens)
(define mass-lens body-mass-lens)
(define rotation-lens rigid-body-angle-lens)

;;; ====
;;; Print Help
;;; ====

(display "lenses.ss loaded (with open protocol dispatch).\n")
(display "  Vec2:        vec2-x-lens, vec2-y-lens\n")
(display "  RigidBody:   rigid-body-pos-lens, rigid-body-vel-lens\n")
(display "               rigid-body-angle-lens, rigid-body-angular-vel-lens\n")
(display "               rigid-body-mass-lens, rigid-body-inertia-lens\n")
(display "  Particle:    particle-pos-lens, particle-vel-lens, particle-mass-lens\n")
(display "               particle-lifetime-lens, particle-size-lens\n")
(display "  Generic:     body-pos-lens, body-vel-lens, body-mass-lens\n")
(display "  Aliases:     position-lens, velocity-lens, mass-lens, rotation-lens\n")
(display "  Dot syntax:  (body. pos x), (body. vel y), (body. mass), etc.\n")
(display "  Extend:      (implement-protocol! 'body-pos 'my-type my-getter)\n")
