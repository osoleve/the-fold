(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module particles
;;; @requires prelude sort vec2 prng
(require 'prelude)
(require 'sort)
(require 'vec2)
(require 'prng)

(doc 'module 'particles)
(doc 'description "2D particle system with emitters and force fields")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'section 'particle)

(doc "Particle : lightweight physics object")
(doc "  - pos       : Vec2 - current position")
(doc "  - vel       : Vec2 - current velocity")
(doc "  - lifetime  : Number - frames remaining (or seconds if using dt)")
(doc "  - max-life  : Number - initial lifetime (for age calculation)")
(doc "  - size      : Number - particle size (for rendering/collision)")
(doc "  - color     : Any - particle color (user-defined representation)")
(doc "  - user-data : Any - arbitrary user data")

(define (make-particle pos vel lifetime max-life size color user-data)
  (doc 'export #t)
  (doc 'type '(make-particle : Vec2 × Vec2 × Number × Number × Number × Any × Any → Particle))
  (list 'particle pos vel lifetime max-life size color user-data))

(doc "particle? : Any → Boolean")
(define (particle? p)
  (doc 'export #t)
  (and (pair? p) (eq? (car p) 'particle)))

(doc "Accessors")
(define (particle-pos p) (list-ref p 1))
(define (particle-vel p) (list-ref p 2))
(define (particle-lifetime p) (list-ref p 3))
(define (particle-max-life p) (list-ref p 4))
(define (particle-size p) (list-ref p 5))
(define (particle-color p) (list-ref p 6))
(define (particle-user-data p) (list-ref p 7))

(doc "particle-alive? : Particle → Boolean")
(doc "Check if particle is still alive.")
(define (particle-alive? p)
  (doc 'export #t)
  (> (particle-lifetime p) 0))

(doc 'particle-age 'type 'Particle → Real[0,1])
(doc "Get normalized age (0=just born, 1=about to die).")
(define (particle-age p)
  (doc 'export #t)
  (let ([lifetime (particle-lifetime p)]
        [max-life (particle-max-life p)])
       (if (= max-life 0)
           1.0
           (- 1.0 (/ lifetime max-life)))))

(define (particle-with-pos p new-pos)
  (doc 'export #t)
  (doc 'type '(particle-with-pos : Particle × Vec2 → Particle))
  (make-particle new-pos
                 (particle-vel p)
                 (particle-lifetime p)
                 (particle-max-life p)
                 (particle-size p)
                 (particle-color p)
                 (particle-user-data p)))

(define (particle-with-vel p new-vel)
  (doc 'export #t)
  (doc 'type '(particle-with-vel : Particle × Vec2 → Particle))
  (make-particle (particle-pos p)
                 new-vel
                 (particle-lifetime p)
                 (particle-max-life p)
                 (particle-size p)
                 (particle-color p)
                 (particle-user-data p)))

(define (particle-with-state p new-pos new-vel new-lifetime)
  (doc 'export #t)
  (doc 'type '(particle-with-state : Particle × Vec2 × Vec2 × Number → Particle))
  (make-particle new-pos
                 new-vel
                 new-lifetime
                 (particle-max-life p)
                 (particle-size p)
                 (particle-color p)
                 (particle-user-data p)))

(doc 'section 'force)

(doc "A force field is a function: Particle → Vec2")
(doc "The force is applied as acceleration (assumes unit mass particles).")

(doc 'gravity-field 'type 'Vec2 → ForceField)
(doc "Constant gravitational acceleration.")
(define (gravity-field g)
  (doc 'export #t)
  (lambda (p) g))

(doc 'wind-field 'type 'Vec2 × Number → ForceField)
(doc "Wind force proportional to strength.")
(define (wind-field direction strength)
  (doc 'export #t)
  (let ([force (vec2-scale (vec2-normalize direction) strength)])
       (lambda (p) force)))

(doc 'drag-field 'type 'Number → ForceField)
(doc "Velocity-proportional drag (slows particles).")
(define (drag-field coefficient)
  (doc 'export #t)
  (lambda (p)
          (vec2-scale (particle-vel p) (- coefficient))))

(doc 'attractor-field 'type 'Vec2 × Number × Number → ForceField)
(doc "Point attractor/repeller.")
(doc "  center: attraction point")
(doc "  strength: positive = attract, negative = repel")
(doc "  falloff: 1=linear, 2=inverse-square (realistic)")
(define (attractor-field center strength falloff)
  (doc 'export #t)
  (lambda (p)
          (let* ([to-center (vec2-sub center (particle-pos p))]
                 [dist (vec2-magnitude to-center)])
                (if (< dist 0.001)
                    (vec2 0 0)  ; avoid singularity
                    (let ([magnitude (/ strength (expt dist falloff))])
                         (vec2-scale (vec2-normalize to-center) magnitude))))))

(doc 'vortex-field 'type 'Vec2 × Number × Number → ForceField)
(doc "Swirling vortex around a center point.")
(define (vortex-field center strength falloff)
  (doc 'export #t)
  (lambda (p)
          (let* ([to-center (vec2-sub center (particle-pos p))]
                 [dist (vec2-magnitude to-center)])
                (if (< dist 0.001)
                    (vec2 0 0)
                    (let* ([tangent (vec2-perp (vec2-normalize to-center))]
                           [magnitude (/ strength (expt dist falloff))])
                          (vec2-scale tangent magnitude))))))

(doc 'turbulence-field 'type 'Number × Number × Number → ForceField)
(doc "Pseudo-random noise-based turbulence.")
(doc "Uses simple hash-based noise for determinism.")
(define (turbulence-field scale strength seed)
  (doc 'export #t)
  (lambda (p)
          (let* ([pos (particle-pos p)]
                 [x (vec2-x pos)]
                 [y (vec2-y pos)]
                 ;; Simple spatial hash for noise
                 [ix (inexact->exact (floor (/ x scale)))]
                 [iy (inexact->exact (floor (/ y scale)))]
                 [hash (modulo (+ (* ix 73856093) (* iy 19349663) seed) 1000000)]
                 ;; Convert to angle
                 [angle (* (/ hash 1000000.0) 2 3.141592653589793)]
                 [force (vec2-from-angle angle)])
                (vec2-scale force strength))))

(doc 'combine-fields 'type '(List ForceField) → ForceField)
(doc "Sum multiple force fields.")
(define (combine-fields fields)
  (doc 'export #t)
  (lambda (p)
          (fold-left (lambda (total field)
                             (vec2-add total (field p)))
                     (vec2 0 0)
                     fields)))

(doc 'section 'particle)

(doc 'integrate-particle 'type 'Particle × ForceField × Number → Particle)
(doc "Update particle using symplectic Euler integration.")
(define (integrate-particle p field dt)
  (doc 'export #t)
  (let* ([accel (field p)]
         [new-vel (vec2-add (particle-vel p) (vec2-scale accel dt))]
         [new-pos (vec2-add (particle-pos p) (vec2-scale new-vel dt))]
         [new-life (- (particle-lifetime p) dt)])
        (particle-with-state p new-pos new-vel new-life)))

(doc 'update-particle 'type 'Particle × ForceField × Number → Particle)
(doc "Alias for integrate-particle.")
(define update-particle integrate-particle)

(doc 'update-particles 'type '(List Particle) × ForceField × Number → (List Particle))
(doc "Update all particles and remove dead ones.")
(define (update-particles particles field dt)
  (doc 'export #t)
  (filter particle-alive?
          (map (lambda (p) (integrate-particle p field dt))
               particles)))

(doc 'section 'emitter)

(doc "Emitter: particle source")
(doc "  - pos           : Vec2 - emitter position")
(doc "  - rate          : Number - particles per second")
(doc "  - spread        : Number - half-angle spread (radians)")
(doc "  - direction     : Vec2 - emission direction (unit vector)")
(doc "  - speed-min     : Number - minimum initial speed")
(doc "  - speed-max     : Number - maximum initial speed")
(doc "  - lifetime-min  : Number - minimum particle lifetime")
(doc "  - lifetime-max  : Number - maximum particle lifetime")
(doc "  - size-min      : Number - minimum particle size")
(doc "  - size-max      : Number - maximum particle size")
(doc "  - color         : Any - particle color")
(doc "  - user-data     : Any - passed to spawned particles")
(doc "  - accumulator   : Number - fractional particle accumulator")

(define (make-emitter pos rate spread direction
                      speed-min speed-max
                      lifetime-min lifetime-max
                      size-min size-max
                      color user-data)
  (doc 'type '(make-emitter : ... → Emitter))
  (list 'emitter
        pos rate spread (vec2-normalize direction)
        speed-min speed-max
        lifetime-min lifetime-max
        size-min size-max
        color user-data
        0))  ; accumulator
(doc "emitter? : Any → Boolean")
(define (emitter? e)
  (doc 'export #t)
  (and (pair? e) (eq? (car e) 'emitter)))

(doc "Emitter accessors")
(define (emitter-pos e) (list-ref e 1))
(define (emitter-rate e) (list-ref e 2))
(define (emitter-spread e) (list-ref e 3))
(define (emitter-direction e) (list-ref e 4))
(define (emitter-speed-min e) (list-ref e 5))
(define (emitter-speed-max e) (list-ref e 6))
(define (emitter-lifetime-min e) (list-ref e 7))
(define (emitter-lifetime-max e) (list-ref e 8))
(define (emitter-size-min e) (list-ref e 9))
(define (emitter-size-max e) (list-ref e 10))
(define (emitter-color e) (list-ref e 11))
(define (emitter-user-data e) (list-ref e 12))
(define (emitter-accumulator e) (list-ref e 13))

(define (emitter-with-pos e new-pos)
  (doc 'export #t)
  (doc 'type '(emitter-with-pos : Emitter × Vec2 → Emitter))
  (list 'emitter
        new-pos
        (emitter-rate e)
        (emitter-spread e)
        (emitter-direction e)
        (emitter-speed-min e)
        (emitter-speed-max e)
        (emitter-lifetime-min e)
        (emitter-lifetime-max e)
        (emitter-size-min e)
        (emitter-size-max e)
        (emitter-color e)
        (emitter-user-data e)
        (emitter-accumulator e)))

(define (emitter-with-direction e new-dir)
  (doc 'export #t)
  (doc 'type '(emitter-with-direction : Emitter × Vec2 → Emitter))
  (list 'emitter
        (emitter-pos e)
        (emitter-rate e)
        (emitter-spread e)
        (vec2-normalize new-dir)
        (emitter-speed-min e)
        (emitter-speed-max e)
        (emitter-lifetime-min e)
        (emitter-lifetime-max e)
        (emitter-size-min e)
        (emitter-size-max e)
        (emitter-color e)
        (emitter-user-data e)
        (emitter-accumulator e)))

(define (emitter-with-accumulator e acc)
  (doc 'export #t)
  (doc 'type '(emitter-with-accumulator : Emitter × Number → Emitter))
  (list 'emitter
        (emitter-pos e)
        (emitter-rate e)
        (emitter-spread e)
        (emitter-direction e)
        (emitter-speed-min e)
        (emitter-speed-max e)
        (emitter-lifetime-min e)
        (emitter-lifetime-max e)
        (emitter-size-min e)
        (emitter-size-max e)
        (emitter-color e)
        (emitter-user-data e)
        acc))

(doc 'section 'emitter)

(doc 'spawn-particle 'type 'Emitter × RNG → (Particle × RNG))
(doc "Spawn a single particle from the emitter.")
(define (spawn-particle e rng)
  (doc 'export #t)
  (let* (;; Random angle within spread
         [r1 (run-state (random-float-range (- (emitter-spread e))
                                            (emitter-spread e))
                        rng)]
         [angle-offset (car r1)]
         [rng1 (cdr r1)]
         ;; Random speed
         [r2 (run-state (random-float-range (emitter-speed-min e)
                                            (emitter-speed-max e))
                        rng1)]
         [speed (car r2)]
         [rng2 (cdr r2)]
         ;; Random lifetime
         [r3 (run-state (random-float-range (emitter-lifetime-min e)
                                            (emitter-lifetime-max e))
                        rng2)]
         [lifetime (car r3)]
         [rng3 (cdr r3)]
         ;; Random size
         [r4 (run-state (random-float-range (emitter-size-min e)
                                            (emitter-size-max e))
                        rng3)]
         [size (car r4)]
         [rng4 (cdr r4)]
         ;; Compute velocity
         [dir (vec2-rotate (emitter-direction e) angle-offset)]
         [vel (vec2-scale dir speed)]
         ;; Create particle
         [particle (make-particle (emitter-pos e)
                                  vel
                                  lifetime
                                  lifetime
                                  size
                                  (emitter-color e)
                                  (emitter-user-data e))])
        (cons particle rng4)))

(doc 'emitter-emit 'type 'Emitter × Number × RNG → (List Particle) × Emitter × RNG)
(doc "Emit particles based on rate and dt.")
(doc "Returns (particles new-emitter new-rng).")
(define (emitter-emit e dt rng)
  (doc 'export #t)
  (let* ([to-emit (+ (emitter-accumulator e) (* (emitter-rate e) dt))]
         [count (inexact->exact (floor to-emit))]
         [remainder (- to-emit count)]
         [new-emitter (emitter-with-accumulator e remainder)])
        (let loop ([n count] [particles '()] [r rng])
             (if (<= n 0)
                 (list particles new-emitter r)
                 (let* ([result (spawn-particle e r)]
                        [p (car result)]
                        [r-next (cdr result)])
                       (loop (- n 1) (cons p particles) r-next))))))

(doc 'section 'particle)

(doc "ParticleSystem: manages emitters, particles, and forces")
(doc "  - emitters  : (List Emitter)")
(doc "  - particles : (List Particle)")
(doc "  - forces    : ForceField (combined)")

(define (make-particle-system emitters particles forces)
  (doc 'export #t)
  (doc 'type '(make-particle-system : (List Emitter) × (List Particle) × ForceField → ParticleSystem))
  (list 'particle-system emitters particles forces))

(doc "particle-system? : Any → Boolean")
(define (particle-system? ps)
  (doc 'export #t)
  (and (pair? ps) (eq? (car ps) 'particle-system)))

(doc "Accessors")
(define (particle-system-emitters ps) (list-ref ps 1))
(define (particle-system-particles ps) (list-ref ps 2))
(define (particle-system-forces ps) (list-ref ps 3))

(doc 'particle-system-count 'type 'ParticleSystem → Nat)
(doc "Count active particles.")
(define (particle-system-count ps)
  (doc 'export #t)
  (length (particle-system-particles ps)))

(define (particle-system-add-emitter ps emitter)
  (doc 'export #t)
  (doc 'type '(particle-system-add-emitter : ParticleSystem × Emitter → ParticleSystem))
  (make-particle-system (cons emitter (particle-system-emitters ps))
                        (particle-system-particles ps)
                        (particle-system-forces ps)))

(define (particle-system-add-force ps field)
  (doc 'export #t)
  (doc 'type '(particle-system-add-force : ParticleSystem × ForceField → ParticleSystem))
  (make-particle-system (particle-system-emitters ps)
                        (particle-system-particles ps)
                        (combine-fields (list (particle-system-forces ps) field))))

(define (particle-system-add-particles ps new-particles)
  (doc 'export #t)
  (doc 'type '(particle-system-add-particles : ParticleSystem × (List Particle) → ParticleSystem))
  (make-particle-system (particle-system-emitters ps)
                        (append new-particles (particle-system-particles ps))
                        (particle-system-forces ps)))

(doc 'section 'particle)

(doc 'particle-system-step 'type 'ParticleSystem × Number × RNG → (ParticleSystem × RNG))
(doc "Step the particle system forward by dt.")
(define (particle-system-step ps dt rng)
  (doc 'export #t)
  (let* ([forces (particle-system-forces ps)]
         ;; Update existing particles
         [updated-particles (update-particles (particle-system-particles ps) forces dt)]
         ;; Emit new particles from each emitter
         [emit-result (fold-left
                       (lambda (acc e)
                               (let* ([particles-so-far (car acc)]
                                      [emitters-so-far (cadr acc)]
                                      [current-rng (caddr acc)]
                                      [result (emitter-emit e dt current-rng)]
                                      [new-particles (car result)]
                                      [new-emitter (cadr result)]
                                      [new-rng (caddr result)])
                                     (list (append new-particles particles-so-far)
                                           (cons new-emitter emitters-so-far)
                                           new-rng)))
                       (list '() '() rng)
                       (particle-system-emitters ps))]
         [emitted (car emit-result)]
         [new-emitters (reverse (cadr emit-result))]
         [new-rng (caddr emit-result)]
         [all-particles (append emitted updated-particles)]
         [new-system (make-particle-system new-emitters all-particles forces)])
        (cons new-system new-rng)))

(doc 'section 'convenience)

(doc 'make-empty-system 'type '→ ParticleSystem)
(doc "Create an empty particle system with no forces.")
(define (make-empty-system)
  (doc 'export #t)
  (make-particle-system '() '() (lambda (p) (vec2 0 0))))

(doc 'make-simple-emitter 'type 'Vec2 × Number → Emitter)
(doc "Create a simple upward emitter with defaults.")
(define (make-simple-emitter pos rate)
  (doc 'export #t)
  (make-emitter pos
                rate
                0.3          ; spread (radians)
                (vec2 0 -1)  ; upward
                50 100       ; speed range
                1.0 2.0      ; lifetime range
                2 5          ; size range
                'white       ; color
                #f))         ; no user data

(doc 'make-explosion-emitter 'type 'Vec2 × Number → Emitter)
(doc "Create an omnidirectional burst emitter.")
(define (make-explosion-emitter pos particle-count)
  (doc 'export #t)
  (make-emitter pos
                (* particle-count 60)  ; all in one frame (at 60fps)
                3.14159                ; full spread (pi = half circle each side)
                (vec2 1 0)             ; base direction (spread covers all)
                100 200                ; fast speed
                0.5 1.5                ; short lifetime
                3 8                    ; varied sizes
                'orange                ; color
                #f))

(doc 'make-fountain-emitter 'type 'Vec2 → Emitter)
(doc "Create a fountain-style upward emitter.")
(define (make-fountain-emitter pos)
  (doc 'export #t)
  (make-emitter pos
                50           ; 50 particles/second
                0.2          ; narrow spread
                (vec2 0 -1)  ; upward
                150 250      ; high speed
                2.0 4.0      ; medium lifetime
                2 4          ; small particles
                'cyan        ; color
                #f))

(doc 'make-fire-emitter 'type 'Vec2 → Emitter)
(doc "Create a fire-style emitter.")
(define (make-fire-emitter pos)
  (doc 'export #t)
  (make-emitter pos
                100          ; dense
                0.4          ; moderate spread
                (vec2 0 -1)  ; upward
                30 80        ; slow-medium speed
                0.3 1.0      ; short lifetime
                4 10         ; varied sizes
                'red         ; color
                #f))

(doc 'section 'burst)

(doc 'burst 'type 'Vec2 × Number × Number × Number × Any × RNG → (List Particle) × RNG)
(doc "Create a burst of particles at a position.")
(doc "spread: angle spread (radians), speed: initial speed, count: particle count")
(define (burst pos spread speed lifetime count color rng)
  (doc 'export #t)
  (let loop ([n count] [particles '()] [r rng])
       (if (<= n 0)
           (cons particles r)
           (let* ([r1 (run-state (random-float-range (- spread) spread) r)]
                  [angle (+ (car r1) (* -0.5 3.14159))]  ; centered upward
                  [rng1 (cdr r1)]
                  [r2 (run-state (random-float-range 0.8 1.2) rng1)]
                  [speed-var (* speed (car r2))]
                  [rng2 (cdr r2)]
                  [vel (vec2-scale (vec2-from-angle angle) speed-var)]
                  [p (make-particle pos vel lifetime lifetime 3 color #f)])
                 (loop (- n 1) (cons p particles) rng2)))))

(doc 'explosion-burst 'type 'Vec2 × Number × RNG → (List Particle) × RNG)
(doc "Create an explosion burst.")
(define (explosion-burst pos count rng)
  (doc 'export #t)
  (let loop ([n count] [particles '()] [r rng])
       (if (<= n 0)
           (cons particles r)
           (let* ([r1 (run-state (random-float-range 0 (* 2 3.14159)) r)]
                  [angle (car r1)]
                  [rng1 (cdr r1)]
                  [r2 (run-state (random-float-range 50 200) rng1)]
                  [speed (car r2)]
                  [rng2 (cdr r2)]
                  [r3 (run-state (random-float-range 0.5 1.5) rng2)]
                  [lifetime (car r3)]
                  [rng3 (cdr r3)]
                  [vel (vec2-scale (vec2-from-angle angle) speed)]
                  [p (make-particle pos vel lifetime lifetime 4 'orange #f)])
                 (loop (- n 1) (cons p particles) rng3)))))

(doc 'section 'particle)

(doc 'particles-in-radius 'type '(List Particle) × Vec2 × Number → (List Particle))
(doc "Find all particles within radius of a point.")
(define (particles-in-radius particles center radius)
  (doc 'export #t)
  (let ([r-sq (* radius radius)])
       (filter (lambda (p)
                       (<= (vec2-distance-sq (particle-pos p) center) r-sq))
               particles)))

(doc 'particles-by-age 'type '(List Particle) × Number × Number → (List Particle))
(doc "Find particles with age in [min-age, max-age].")
(define (particles-by-age particles min-age max-age)
  (doc 'export #t)
  (filter (lambda (p)
                  (let ([age (particle-age p)])
                       (and (>= age min-age) (<= age max-age))))
          particles))

(doc 'oldest-particles 'type '(List Particle) × Number → (List Particle))
(doc "Get the n oldest particles.")
(define (oldest-particles particles n)
  (doc 'export #t)
  (take n (sort-by (lambda (a b)
                          (> (particle-age a) (particle-age b)))
                  particles)))

(doc 'section 'collision)

(doc 'particle-bounce 'type 'Particle × Vec2 × Number → Particle)
(doc "Bounce particle off a surface with given normal and restitution.")
(define (particle-bounce p normal restitution)
  (doc 'export #t)
  (let* ([vel (particle-vel p)]
         [dot (vec2-dot vel normal)]
         [reflected (vec2-sub vel (vec2-scale normal (* 2 dot)))]
         [new-vel (vec2-scale reflected restitution)])
        (particle-with-vel p new-vel)))

(doc "particle-in-bounds? : Particle × Vec2 × Vec2 → Boolean")
(doc "Check if particle is within rectangular bounds.")
(define (particle-in-bounds? p min-corner max-corner)
  (doc 'export #t)
  (let ([pos (particle-pos p)])
       (and (>= (vec2-x pos) (vec2-x min-corner))
            (<= (vec2-x pos) (vec2-x max-corner))
            (>= (vec2-y pos) (vec2-y min-corner))
            (<= (vec2-y pos) (vec2-y max-corner)))))

(doc 'filter-bounds 'type '(List Particle) × Vec2 × Vec2 → (List Particle))
(doc "Remove particles outside bounds.")
(define (filter-bounds particles min-corner max-corner)
  (doc 'export #t)
  (filter (lambda (p) (particle-in-bounds? p min-corner max-corner))
          particles))

(doc 'section 'statistics)

(doc 'particle-system-stats 'type 'ParticleSystem → Alist)
(doc "Get statistics about the particle system.")
(define (particle-system-stats ps)
  (doc 'export #t)
  (let* ([particles (particle-system-particles ps)]
         [count (length particles)]
         [emitter-count (length (particle-system-emitters ps))]
         [avg-age (if (= count 0)
                      0
                      (/ (fold-left + 0 (map particle-age particles)) count))]
         [avg-speed (if (= count 0)
                        0
                        (/ (fold-left + 0 (map (lambda (p)
                                                       (vec2-magnitude (particle-vel p)))
                                               particles))
                           count))])
        `((particle-count . ,count)
          (emitter-count . ,emitter-count)
          (average-age . ,avg-age)
          (average-speed . ,avg-speed))))

(doc 'section 'module)

(display "  Create: (make-particle-system emitters particles forces)\n")
(display "  Emitters: (make-simple-emitter pos rate), (make-fire-emitter pos)\n")
(display "  Forces: (gravity-field g), (drag-field c), (attractor-field center s f)\n")
(display "  Step: (particle-system-step system dt rng) -> (system . rng)\n")
