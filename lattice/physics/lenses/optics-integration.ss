(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module optics-integration
;;; @requires prelude optics lenses
(require 'prelude)
(require 'optics)
(require 'lenses)

(doc 'module 'optics-integration)
(doc 'description "Integrates the physics lens library with the optics tower")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Provides compatibility with optics operators (^., ^?, %~, .~, &)")
(doc 'note "Provides traversals for collections of bodies (bodies-each, particles-each)")
(doc 'note "Provides affines for optional physics properties")
(doc 'note "Provides world-level lens composition")
(doc 'note "Provides the >>> composition operator for chaining")
(doc 'note "Usage: (load \"lattice/physics/lenses/optics-integration.ss\")")
(doc 'note "Example: (^. body (body. pos)) ; View position")
(doc 'note "Example: (& body (%~ (body. pos x) (lambda (x) (+ x 10)))) ; Modify pos.x")
(doc 'note "Example: (traversal-over bodies-each update-fn world-bodies)")
(doc 'note "Example: (^. world (>>> (world-body 'player) (body. vel x)))")

(doc 'section 'composition-operator)
(doc 'note "The >>> operator provides left-to-right optic composition")
(doc 'note "This reads more naturally for 'drilling into' data structures")

(doc >>> 'type '(-> (Optic a b) (Optic b c) (Optic a c)))
(doc >>> 'description "Left-to-right optic composition (like Haskell's >>> or lens's .)")
(doc >>> 'note "(>>> outer inner) = (optic-compose outer inner)")
(define (>>> outer inner)
  (optic-compose outer inner))

(doc 'section 'lens-to-optic-adapters)
(doc 'note "The existing physics lenses are already compatible with the optics tower")
(doc 'note "They use make-lens from templates.ss, so this is for clarity only")

(doc physics-lens->optic 'type '(-> Lens Optic))
(doc physics-lens->optic 'description "Explicit conversion (identity since both use same representation)")
(define (physics-lens->optic lens)
  lens)

(doc 'section 'traversals-for-physics-collections)

(doc bodies-each 'type '(Traversal (List Body) Body))
(doc bodies-each 'description "Traverse each body in a collection")
(define bodies-each
  (make-traversal
   (lambda (f bodies) (map f bodies))
   identity))

(doc particles-each 'type '(Traversal (List Particle) Particle))
(doc particles-each 'description "Traverse each particle in a collection")
(define particles-each
  (make-traversal
   (lambda (f particles) (map f particles))
   identity))

(doc particles-alive 'type '(Traversal (List Particle) Particle))
(doc particles-alive 'description "Traverse only alive particles")
(define particles-alive
  (make-traversal
   (lambda (f particles)
     (map (lambda (p)
            (if (particle-alive? p) (f p) p))
          particles))
   (lambda (particles)
     (filter particle-alive? particles))))

(doc bodies-filtered 'type '(-> (-> Body Boolean) (Traversal (List Body) Body)))
(doc bodies-filtered 'description "Traverse bodies matching predicate")
(define (bodies-filtered pred)
  (make-traversal
   (lambda (f bodies)
     (map (lambda (b) (if (pred b) (f b) b)) bodies))
   (lambda (bodies) (filter pred bodies))))

(doc rigid-bodies-only 'type '(Traversal (List Body) RigidBody2D))
(doc rigid-bodies-only 'description "Traverse only rigid bodies in a mixed collection")
(define rigid-bodies-only
  (bodies-filtered rigid-body?))

(doc particles-only 'type '(Traversal (List Body) Particle))
(doc particles-only 'description "Traverse only particles in a mixed collection")
(define particles-only
  (bodies-filtered particle?))

(doc 'section 'affines-for-optional-properties)
(doc 'note "Affines handle properties that may not exist on all body types")

(doc make-body-affine 'type '(-> (-> Body a) (-> Body a Body) (-> Body Boolean) (Affine Body a)))
(doc make-body-affine 'description "Create an affine for optional body properties")
(doc make-body-affine 'note "Returns nothing if body doesn't match type predicate")
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

(doc affine-rigid-angle 'type '(Affine Body Number))
(doc affine-rigid-angle 'description "Access angle if body is a rigid body")
(define affine-rigid-angle
  (make-body-affine
   rigid-body-angle
   (lambda (b v) (rigid-body-with-angle b v))
   rigid-body?))

(doc affine-rigid-angular-vel 'type '(Affine Body Number))
(doc affine-rigid-angular-vel 'description "Access angular velocity if body is a rigid body")
(define affine-rigid-angular-vel
  (make-body-affine
   rigid-body-angular-vel
   (lambda (b v) (rigid-body-with-angular-vel b v))
   rigid-body?))

(doc affine-rigid-inertia 'type '(Affine Body Number))
(doc affine-rigid-inertia 'description "Access inertia if body is a rigid body")
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

(doc affine-particle-lifetime 'type '(Affine Body Number))
(doc affine-particle-lifetime 'description "Access lifetime if body is a particle")
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

(doc affine-particle-size 'type '(Affine Body Number))
(doc affine-particle-size 'description "Access size if body is a particle")
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

(doc affine-particle-color 'type '(Affine Body Any))
(doc affine-particle-color 'description "Access color if body is a particle")
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

(doc 'section 'world-level-optics)
(doc 'note "For games/simulations with a 'world' containing named entities")

(doc make-world 'type '(-> (List (Pair Symbol Body)) World))
(doc make-world 'description "Create a world as an association list of named bodies")
(define (make-world bodies)
  (list 'world bodies))

(doc world? 'type '(-> Any Boolean))
(define (world? w)
  (and (pair? w) (eq? (car w) 'world)))

(doc world-bodies 'type '(-> World (List (Pair Symbol Body))))
(define (world-bodies w)
  (cadr w))

(doc world-with-bodies 'type '(-> World (List (Pair Symbol Body)) World))
(define (world-with-bodies w new-bodies)
  (make-world new-bodies))

(doc world-body 'type '(-> Symbol (Affine World Body)))
(doc world-body 'description "Focus on a named body in the world (may not exist)")
(define (world-body name)
  (make-affine
   (lambda (w)
     (let ([entry (assq name (world-bodies w))])
       (if entry
           (just (cdr entry))
           nothing)))
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

(doc world-bodies-lens 'type '(Lens World (List (Pair Symbol Body))))
(doc world-bodies-lens 'description "Direct access to the bodies list")
(define world-bodies-lens
  (make-lens
   world-bodies
   (lambda (bodies w) (world-with-bodies w bodies))))

(doc world-all-bodies 'type '(Traversal World Body))
(doc world-all-bodies 'description "Traverse all bodies in the world")
(define world-all-bodies
  (make-traversal
   (lambda (f w)
     (world-with-bodies w
       (map (lambda (entry)
              (cons (car entry) (f (cdr entry))))
            (world-bodies w))))
   (lambda (w)
     (map cdr (world-bodies w)))))

(doc 'section 'convenience-functions)

(doc body-view 'type '(-> Body Lens a))
(doc body-view 'description "View using optics operator")
(define (body-view body lens)
  (^. body lens))

(doc body-modify 'type '(-> Body Lens (-> a a) Body))
(doc body-modify 'description "Modify using optics operators")
(define (body-modify body lens f)
  (& body (%~ lens f)))

(doc body-set 'type '(-> Body Lens a Body))
(doc body-set 'description "Set using optics operators")
(define (body-set body lens val)
  (& body (.~ lens val)))

(doc body-preview 'type '(-> Body Affine (Maybe a)))
(doc body-preview 'description "Preview through affine (safe access)")
(define (body-preview body affine)
  (^? body affine))

(doc apply-gravity 'type '(-> Number (-> Body Body)))
(doc apply-gravity 'description "Apply gravity to a body's velocity using optics")
(define (apply-gravity g)
  (lambda (body)
    (& body (%~ (body. vel y) (lambda (vy) (+ vy g))))))

(doc apply-impulse 'type '(-> Vec2 (-> Body Body)))
(doc apply-impulse 'description "Apply velocity impulse using optics")
(define (apply-impulse impulse)
  (lambda (body)
    (& body (%~ (body. vel) (lambda (v) (vec2-add v impulse))))))

(doc move-by 'type '(-> Vec2 (-> Body Body)))
(doc move-by 'description "Move body by offset using optics")
(define (move-by offset)
  (lambda (body)
    (& body (%~ (body. pos) (lambda (p) (vec2-add p offset))))))

(doc 'section 'fold-operations-for-physics)

(doc fold-bodies 'type '(Fold (List Body) Body))
(doc fold-bodies 'description "Read-only fold over body list")
(define fold-bodies
  (make-fold identity))

(doc total-mass 'type '(-> (List Body) Number))
(doc total-mass 'description "Sum masses of all bodies using fold")
(define (total-mass bodies)
  (fold-sum (fold-compose fold-bodies (lens->fold body-mass-lens)) bodies))

(doc center-of-mass 'type '(-> (List Body) Vec2))
(doc center-of-mass 'description "Compute center of mass using folds")
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

(doc bodies-in-region 'type '(-> Vec2 Number (Fold (List Body) Body)))
(doc bodies-in-region 'description "Fold over bodies within radius of point")
(define (bodies-in-region center radius)
  (make-fold
   (lambda (bodies)
     (filter
      (lambda (b)
        (<= (vec2-distance (view body-pos-lens b) center) radius))
      bodies))))

(doc 'section 'composite-optics-examples)
(doc 'note "Demonstrates power of composition for complex operations")

(doc player-pos-x 'type '(Affine World Number))
(doc player-pos-x 'description "Drill into world -> player body -> position -> x component")
(doc player-pos-x 'note "Returns nothing if player doesn't exist")
(define player-pos-x
  (>>> (world-body 'player) (>>> (lens->affine body-pos-lens) (lens->affine vec2-x-lens))))

(doc player-vel 'type '(Affine World Vec2))
(doc player-vel 'description "Access player's velocity in world")
(define player-vel
  (>>> (world-body 'player) (lens->affine body-vel-lens)))

(doc body-speed-getter 'type '(-> Body Number))
(doc body-speed-getter 'description "Get speed (velocity magnitude) of a body")
(define (body-speed-getter b)
  (vec2-length (view body-vel-lens b)))

(doc 'section 'physics-step-with-optics)

(doc step-body 'type '(-> Number (-> Body Body)))
(doc step-body 'description "Single Euler integration step using optics")
(define (step-body dt)
  (lambda (body)
    (let ([vel (^. body body-vel-lens)])
      (& body (%~ body-pos-lens (lambda (p) (vec2-add p (vec2-scale vel dt))))))))

(doc step-bodies 'type '(-> Number (-> (List Body) (List Body))))
(doc step-bodies 'description "Step all bodies using traversal")
(define (step-bodies dt)
  (lambda (bodies)
    (traversal-over bodies-each (step-body dt) bodies)))

(doc step-world 'type '(-> Number (-> World World)))
(doc step-world 'description "Step all bodies in world using traversal")
(define (step-world dt)
  (lambda (w)
    (traversal-over world-all-bodies (step-body dt) w)))

(doc apply-forces-to-world 'type '(-> (List (Pair Symbol Vec2)) (-> World World)))
(doc apply-forces-to-world 'description "Apply named forces to bodies")
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

(doc 'section 'exports)
(doc 'note "Composition: >>> (left-to-right composition)")
(doc 'note "Traversals: bodies-each, particles-each, particles-alive, bodies-filtered, rigid-bodies-only, particles-only")
(doc 'note "Affines: affine-rigid-angle, affine-rigid-angular-vel, affine-rigid-inertia, affine-particle-lifetime, affine-particle-size, affine-particle-color")
(doc 'note "World: make-world, world?, world-bodies, world-with-bodies, world-body, world-bodies-lens, world-all-bodies")
(doc 'note "Convenience: body-view, body-modify, body-set, body-preview, apply-gravity, apply-impulse, move-by")
(doc 'note "Folds: fold-bodies, total-mass, center-of-mass, bodies-in-region")
(doc 'note "Composite Examples: player-pos-x, player-vel")
(doc 'note "Physics Steps: step-body, step-bodies, step-world, apply-forces-to-world")
(doc 'note "Re-exports from lenses.ss: All physics lenses (body., rigid-body-*, particle-*, etc.)")
(doc 'note "Re-exports from optics.ss: ^., ^?, ^.., .~, %~, & and all optic types")

(display "  Composition: >>>\n")
(display "  Traversals: bodies-each, particles-each, particles-alive\n")
(display "  Affines: affine-rigid-angle, affine-particle-lifetime, etc.\n")
(display "  World: make-world, world-body, world-all-bodies\n")
(display "  Operators: ^., ^?, %~, .~, & (from optics.ss)\n")
