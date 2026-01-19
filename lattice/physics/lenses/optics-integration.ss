;;; lattice/physics/lenses/optics-integration.ss — Physics Lenses with Optics Tower
;;;
;;; Integrates the physics lens library with the optics tower, providing:
;;;   - Compatibility with optics operators (^., ^?, %~, .~, &)
;;;   - Traversals for collections of bodies (bodies-each, particles-each)
;;;   - Affines for optional physics properties
;;;   - World-level lens composition
;;;   - The >>> composition operator for chaining
;;;
;;; Usage:
;;;   (load "lattice/physics/lenses/optics-integration.ss")
;;;
;;;   ;; Use optics operators
;;;   (^. body (body. pos))                    ; View position
;;;   (& body (%~ (body. pos x) (lambda (x) (+ x 10))))  ; Modify pos.x
;;;
;;;   ;; Traverse collections
;;;   (traversal-over bodies-each             ; Update all bodies
;;;     (lambda (b) (& b (%~ (body. vel y) (lambda (vy) (+ vy -9.8)))))
;;;     world-bodies)
;;;
;;;   ;; World-level composition with >>>
;;;   (^. world (>>> (world-body 'player) (body. vel x)))
;;;
;;; This is Lattice code: pure, functional.
;;;
;;; Dependencies:
;;;   - lattice/fp/optics/optics.ss (optics tower)
;;;   - lattice/physics/lenses/lenses.ss (physics lenses)

(load "lattice/fp/optics/optics.ss")
(load "lattice/physics/lenses/lenses.ss")

;;; ============================================================
;;; Part 1: Composition Operator
;;; ============================================================
;;;
;;; The >>> operator provides left-to-right optic composition,
;;; which reads more naturally for "drilling into" data structures.

;;; >>> : Optic a b x Optic b c -> Optic a c
;;; Left-to-right optic composition (like Haskell's >>> or lens's .)
;;; (>>> outer inner) = (optic-compose outer inner)
(define (>>> outer inner)
  (optic-compose outer inner))

;;; ============================================================
;;; Part 2: Lens-to-Optic Adapters
;;; ============================================================
;;;
;;; The existing physics lenses are already compatible with the
;;; optics tower because they use make-lens from templates.ss.
;;; However, we provide explicit conversion utilities for clarity.

;;; physics-lens->optic : Lens -> Optic
;;; Explicit conversion (identity since both use same representation).
(define (physics-lens->optic lens)
  lens)  ; Already compatible

;;; ============================================================
;;; Part 3: Traversals for Physics Collections
;;; ============================================================

;;; bodies-each : Traversal (List Body) Body
;;; Traverse each body in a collection.
(define bodies-each
  (make-traversal
   (lambda (f bodies) (map f bodies))
   identity))

;;; particles-each : Traversal (List Particle) Particle
;;; Traverse each particle in a collection.
(define particles-each
  (make-traversal
   (lambda (f particles) (map f particles))
   identity))

;;; bodies-alive : Traversal (List Particle) Particle
;;; Traverse only alive particles.
(define particles-alive
  (make-traversal
   (lambda (f particles)
     (map (lambda (p)
            (if (particle-alive? p) (f p) p))
          particles))
   (lambda (particles)
     (filter particle-alive? particles))))

;;; bodies-filtered : (Body -> Boolean) -> Traversal (List Body) Body
;;; Traverse bodies matching predicate.
(define (bodies-filtered pred)
  (make-traversal
   (lambda (f bodies)
     (map (lambda (b) (if (pred b) (f b) b)) bodies))
   (lambda (bodies) (filter pred bodies))))

;;; rigid-bodies-only : Traversal (List Body) RigidBody2D
;;; Traverse only rigid bodies in a mixed collection.
(define rigid-bodies-only
  (bodies-filtered rigid-body?))

;;; particles-only : Traversal (List Body) Particle
;;; Traverse only particles in a mixed collection.
(define particles-only
  (bodies-filtered particle?))

;;; ============================================================
;;; Part 4: Affines for Optional Properties
;;; ============================================================
;;;
;;; Affines handle properties that may not exist on all body types.

;;; affine-collision-info : Affine Body CollisionInfo
;;; Access collision info if present (assumes bodies may have optional collision data).
;;; Returns nothing if no collision info exists.
(define (make-body-affine getter-fn setter-fn type-pred)
  (make-affine
   (lambda (b)
     (if (type-pred b)
         (just (getter-fn b))
         nothing))
   (lambda (v b)
     (if (type-pred b)
         (setter-fn b v)
         b))))

;;; affine-rigid-angle : Affine Body Number
;;; Access angle if body is a rigid body.
(define affine-rigid-angle
  (make-body-affine
   rigid-body-angle
   (lambda (b v) (rigid-body-with-angle b v))
   rigid-body?))

;;; affine-rigid-angular-vel : Affine Body Number
;;; Access angular velocity if body is a rigid body.
(define affine-rigid-angular-vel
  (make-body-affine
   rigid-body-angular-vel
   (lambda (b v) (rigid-body-with-angular-vel b v))
   rigid-body?))

;;; affine-rigid-inertia : Affine Body Number
;;; Access inertia if body is a rigid body.
(define affine-rigid-inertia
  (make-body-affine
   rigid-body-inertia
   (lambda (b v)
     (make-rigid-body (rigid-body-pos b)
                      (rigid-body-vel b)
                      (rigid-body-angle b)
                      (rigid-body-angular-vel b)
                      (rigid-body-mass b)
                      v))
   rigid-body?))

;;; affine-particle-lifetime : Affine Body Number
;;; Access lifetime if body is a particle.
(define affine-particle-lifetime
  (make-body-affine
   particle-lifetime
   (lambda (p v)
     (make-particle (particle-pos p)
                    (particle-vel p)
                    v
                    (particle-max-life p)
                    (particle-size p)
                    (particle-color p)
                    (particle-user-data p)))
   particle?))

;;; affine-particle-size : Affine Body Number
;;; Access size if body is a particle.
(define affine-particle-size
  (make-body-affine
   particle-size
   (lambda (p v)
     (make-particle (particle-pos p)
                    (particle-vel p)
                    (particle-lifetime p)
                    (particle-max-life p)
                    v
                    (particle-color p)
                    (particle-user-data p)))
   particle?))

;;; affine-particle-color : Affine Body Any
;;; Access color if body is a particle.
(define affine-particle-color
  (make-body-affine
   particle-color
   (lambda (p v)
     (make-particle (particle-pos p)
                    (particle-vel p)
                    (particle-lifetime p)
                    (particle-max-life p)
                    (particle-size p)
                    v
                    (particle-user-data p)))
   particle?))

;;; ============================================================
;;; Part 5: World-Level Optics
;;; ============================================================
;;;
;;; For games/simulations with a "world" containing named entities.

;;; make-world : (List (Symbol . Body)) -> World
;;; Create a world as an association list of named bodies.
(define (make-world bodies)
  (list 'world bodies))

;;; world? : Any -> Boolean
(define (world? w)
  (and (pair? w) (eq? (car w) 'world)))

;;; world-bodies : World -> (List (Symbol . Body))
(define (world-bodies w)
  (cadr w))

;;; world-with-bodies : World x (List (Symbol . Body)) -> World
(define (world-with-bodies w new-bodies)
  (make-world new-bodies))

;;; world-body : Symbol -> Affine World Body
;;; Focus on a named body in the world (may not exist).
(define (world-body name)
  (make-affine
   ;; getter: World -> Maybe Body
   (lambda (w)
     (let ([entry (assq name (world-bodies w))])
       (if entry
           (just (cdr entry))
           nothing)))
   ;; setter: Body -> World -> World
   (lambda (b w)
     (let* ([bodies (world-bodies w)]
            [entry (assq name bodies)])
       (if entry
           (world-with-bodies w
             (map (lambda (e)
                    (if (eq? (car e) name)
                        (cons name b)
                        e))
                  bodies))
           w)))))

;;; world-bodies-lens : Lens World (List (Symbol . Body))
;;; Direct access to the bodies list.
(define world-bodies-lens
  (make-lens
   world-bodies
   (lambda (bodies w) (world-with-bodies w bodies))))

;;; world-all-bodies : Traversal World Body
;;; Traverse all bodies in the world.
(define world-all-bodies
  (make-traversal
   (lambda (f w)
     (world-with-bodies w
       (map (lambda (entry)
              (cons (car entry) (f (cdr entry))))
            (world-bodies w))))
   (lambda (w)
     (map cdr (world-bodies w)))))

;;; ============================================================
;;; Part 6: Convenience Functions
;;; ============================================================

;;; body-view : Body x Lens -> a
;;; View using optics operator.
(define (body-view body lens)
  (^. body lens))

;;; body-modify : Body x Lens x (a -> a) -> Body
;;; Modify using optics operators.
(define (body-modify body lens f)
  (& body (%~ lens f)))

;;; body-set : Body x Lens x a -> Body
;;; Set using optics operators.
(define (body-set body lens val)
  (& body (.~ lens val)))

;;; body-preview : Body x Affine -> Maybe a
;;; Preview through affine (safe access).
(define (body-preview body affine)
  (^? body affine))

;;; apply-gravity : Number -> Body -> Body
;;; Apply gravity to a body's velocity using optics.
(define (apply-gravity g)
  (lambda (body)
    (& body (%~ (body. vel y) (lambda (vy) (+ vy g))))))

;;; apply-impulse : Vec2 -> Body -> Body
;;; Apply velocity impulse using optics.
(define (apply-impulse impulse)
  (lambda (body)
    (& body (%~ (body. vel) (lambda (v) (vec2-add v impulse))))))

;;; move-by : Vec2 -> Body -> Body
;;; Move body by offset using optics.
(define (move-by offset)
  (lambda (body)
    (& body (%~ (body. pos) (lambda (p) (vec2-add p offset))))))

;;; ============================================================
;;; Part 7: Fold Operations for Physics
;;; ============================================================

;;; fold-bodies : Fold (List Body) Body
;;; Read-only fold over body list.
(define fold-bodies
  (make-fold identity))

;;; total-mass : (List Body) -> Number
;;; Sum masses of all bodies using fold.
(define (total-mass bodies)
  (fold-sum (fold-compose fold-bodies (lens->fold body-mass-lens)) bodies))

;;; center-of-mass : (List Body) -> Vec2
;;; Compute center of mass using folds.
(define (center-of-mass bodies)
  (let* ([total-m (total-mass bodies)]
         [weighted-sum
          (fold-left
           (lambda (acc b)
             (let ([m (view body-mass-lens b)]
                   [p (view body-pos-lens b)])
               (vec2-add acc (vec2-scale p m))))
           (vec2 0 0)
           bodies)])
    (if (= total-m 0)
        (vec2 0 0)
        (vec2-scale weighted-sum (/ 1 total-m)))))

;;; bodies-in-region : Vec2 x Number -> Fold (List Body) Body
;;; Fold over bodies within radius of point.
(define (bodies-in-region center radius)
  (make-fold
   (lambda (bodies)
     (filter
      (lambda (b)
        (<= (vec2-distance (view body-pos-lens b) center) radius))
      bodies))))

;;; ============================================================
;;; Part 8: Composite Optics Examples
;;; ============================================================
;;;
;;; Demonstrates power of composition for complex operations.

;;; player-pos-x : Affine World Number
;;; Drill into world -> player body -> position -> x component.
;;; Returns nothing if player doesn't exist.
(define player-pos-x
  (>>> (world-body 'player) (>>> (lens->affine body-pos-lens) (lens->affine vec2-x-lens))))

;;; player-vel : Affine World Vec2
;;; Access player's velocity in world.
(define player-vel
  (>>> (world-body 'player) (lens->affine body-vel-lens)))

;;; all-body-speeds : Fold World Number
;;; Get speeds (velocity magnitudes) of all bodies.
(define (body-speed-getter b)
  (vec2-length (view body-vel-lens b)))

;;; ============================================================
;;; Part 9: Physics Step with Optics
;;; ============================================================

;;; step-body : Number -> Body -> Body
;;; Single Euler integration step using optics.
(define (step-body dt)
  (lambda (body)
    (let ([vel (^. body body-vel-lens)])
      (& body (%~ body-pos-lens (lambda (p) (vec2-add p (vec2-scale vel dt))))))))

;;; step-bodies : Number -> (List Body) -> (List Body)
;;; Step all bodies using traversal.
(define (step-bodies dt)
  (lambda (bodies)
    (traversal-over bodies-each (step-body dt) bodies)))

;;; step-world : Number -> World -> World
;;; Step all bodies in world using traversal.
(define (step-world dt)
  (lambda (w)
    (traversal-over world-all-bodies (step-body dt) w)))

;;; apply-forces-to-world : (List (Symbol . Vec2)) -> World -> World
;;; Apply named forces to bodies.
(define (apply-forces-to-world forces)
  (lambda (w)
    (fold-left
     (lambda (world force-pair)
       (let ([name (car force-pair)]
             [force (cdr force-pair)])
         (let ([maybe-body (^? world (world-body name))])
           (if (nothing? maybe-body)
               world
               (& world (.~ (world-body name)
                           (apply-impulse force (from-just maybe-body))))))))
     w
     forces)))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Composition:
;;;   >>> (left-to-right composition)
;;;
;;; Traversals:
;;;   bodies-each, particles-each, particles-alive
;;;   bodies-filtered, rigid-bodies-only, particles-only
;;;
;;; Affines:
;;;   affine-rigid-angle, affine-rigid-angular-vel, affine-rigid-inertia
;;;   affine-particle-lifetime, affine-particle-size, affine-particle-color
;;;
;;; World:
;;;   make-world, world?, world-bodies, world-with-bodies
;;;   world-body, world-bodies-lens, world-all-bodies
;;;
;;; Convenience:
;;;   body-view, body-modify, body-set, body-preview
;;;   apply-gravity, apply-impulse, move-by
;;;
;;; Folds:
;;;   fold-bodies, total-mass, center-of-mass, bodies-in-region
;;;
;;; Composite Examples:
;;;   player-pos-x, player-vel
;;;
;;; Physics Steps:
;;;   step-body, step-bodies, step-world, apply-forces-to-world
;;;
;;; Re-exports from lenses.ss:
;;;   All physics lenses (body., rigid-body-*, particle-*, etc.)
;;;
;;; Re-exports from optics.ss:
;;;   ^., ^?, ^.., .~, %~, & and all optic types

(display "optics-integration.ss loaded.\n")
(display "  Composition: >>>\n")
(display "  Traversals: bodies-each, particles-each, particles-alive\n")
(display "  Affines: affine-rigid-angle, affine-particle-lifetime, etc.\n")
(display "  World: make-world, world-body, world-all-bodies\n")
(display "  Operators: ^., ^?, %~, .~, & (from optics.ss)\n")
