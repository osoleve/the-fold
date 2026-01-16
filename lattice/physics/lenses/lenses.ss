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
;;; Dependencies:
;;;   - lattice/fp/templates.ss (lens infrastructure)
;;;   - lattice/linalg/vec2.ss
;;;   - lattice/physics/classical/rigid-body.ss
;;;   - lattice/physics/classical/particles.ss

(load "lattice/fp/templates.ss")
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

;;; ====
;;; Generic Body Lenses (Work with any body type)
;;; ====

;;; These use dispatch to work with rigid-body, particle, or traced-body.

;;; body-pos : Any -> Vec2
;;; Generic position accessor.
(define (body-pos b)
  (cond
   [(rigid-body? b) (rigid-body-pos b)]
   [(particle? b) (particle-pos b)]
   [else (error 'body-pos "Unknown body type" b)]))

;;; body-pos-lens : Lens Body Vec2
;;; Generic lens for body position (works with rigid-body and particle).
(define body-pos-lens
  (make-lens
   body-pos
   (lambda (new-pos b)
     (cond
      [(rigid-body? b) (rigid-body-with-pos b new-pos)]
      [(particle? b) (particle-with-pos b new-pos)]
      [else (error 'body-pos-lens "Unknown body type" b)]))))

;;; body-vel : Any -> Vec2
;;; Generic velocity accessor.
(define (body-vel b)
  (cond
   [(rigid-body? b) (rigid-body-vel b)]
   [(particle? b) (particle-vel b)]
   [else (error 'body-vel "Unknown body type" b)]))

;;; body-vel-lens : Lens Body Vec2
;;; Generic lens for body velocity.
(define body-vel-lens
  (make-lens
   body-vel
   (lambda (new-vel b)
     (cond
      [(rigid-body? b) (rigid-body-with-vel b new-vel)]
      [(particle? b) (particle-with-vel b new-vel)]
      [else (error 'body-vel-lens "Unknown body type" b)]))))

;;; ====
;;; Dot Notation Macro
;;; ====

;;; body. : Symbol ... -> Lens
;;; Convenience macro for composing physics lenses using dot notation.
;;; (body. pos x) => (lens-compose rigid-body-pos-lens vec2-x-lens)
;;;
;;; Supported paths:
;;;   (body. pos)       -> body-pos-lens
;;;   (body. vel)       -> body-vel-lens
;;;   (body. pos x)     -> (lens-compose body-pos-lens vec2-x-lens)
;;;   (body. pos y)     -> (lens-compose body-pos-lens vec2-y-lens)
;;;   (body. vel x)     -> (lens-compose body-vel-lens vec2-x-lens)
;;;   (body. vel y)     -> (lens-compose body-vel-lens vec2-y-lens)
;;;   (body. angle)     -> rigid-body-angle-lens
;;;   (body. angular-vel) -> rigid-body-angular-vel-lens
;;;   (body. mass)      -> rigid-body-mass-lens
;;;   (body. inertia)   -> rigid-body-inertia-lens

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
    ;; RigidBody-specific
    [(body. angle) rigid-body-angle-lens]
    [(body. angular-vel) rigid-body-angular-vel-lens]
    [(body. mass) rigid-body-mass-lens]
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
;;; Aliases (Backward Compatibility with Issue Description)
;;; ====

;;; The issue mentioned: position-lens, velocity-lens, mass-lens, rotation-lens
(define position-lens body-pos-lens)
(define velocity-lens body-vel-lens)
(define mass-lens rigid-body-mass-lens)
(define rotation-lens rigid-body-angle-lens)

;;; ====
;;; Print Help
;;; ====

(display "lenses.ss loaded.\n")
(display "  Vec2:        vec2-x-lens, vec2-y-lens\n")
(display "  RigidBody:   rigid-body-pos-lens, rigid-body-vel-lens\n")
(display "               rigid-body-angle-lens, rigid-body-angular-vel-lens\n")
(display "               rigid-body-mass-lens, rigid-body-inertia-lens\n")
(display "  Particle:    particle-pos-lens, particle-vel-lens\n")
(display "               particle-lifetime-lens, particle-size-lens\n")
(display "  Generic:     body-pos-lens, body-vel-lens\n")
(display "  Aliases:     position-lens, velocity-lens, mass-lens, rotation-lens\n")
(display "  Dot syntax:  (body. pos x), (body. vel y), etc.\n")
