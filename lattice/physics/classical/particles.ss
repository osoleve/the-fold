;;; lattice/physics/classical/particles.ss — 2D Particle System
;;;
;;; Physics-integrated particle simulation for effects like:
;;;   - Explosions, sparks, smoke, fire
;;;   - Trail effects
;;;   - Environmental effects (rain, snow, debris)
;;;
;;; This is Lattice code: pure, functional, uses vec2 and PRNG.
;;;
;;; Features:
;;;   - Particle emitters with configurable rate, spread, velocity
;;;   - Particle lifetime and age-based effects
;;;   - Force fields: gravity, wind, attractors, drag, turbulence
;;;   - Optional collision with world geometry
;;;   - Efficient list-based management (no mutation)
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/linalg/vec2.ss
;;;   - lattice/random/prng.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "lattice/random/prng.ss")

;;; ====
;;; Particle Type
;;; ====

;;; Particle : lightweight physics object
;;;   - pos       : Vec2 - current position
;;;   - vel       : Vec2 - current velocity
;;;   - lifetime  : Number - frames remaining (or seconds if using dt)
;;;   - max-life  : Number - initial lifetime (for age calculation)
;;;   - size      : Number - particle size (for rendering/collision)
;;;   - color     : Any - particle color (user-defined representation)
;;;   - user-data : Any - arbitrary user data

;;; make-particle : Vec2 × Vec2 × Number × Number × Number × Any × Any → Particle
(define (make-particle pos vel lifetime max-life size color user-data)
  (list 'particle pos vel lifetime max-life size color user-data))

;;; particle? : Any → Boolean
(define (particle? p)
  (and (pair? p) (eq? (car p) 'particle)))

;;; Accessors
(define (particle-pos p) (list-ref p 1))
(define (particle-vel p) (list-ref p 2))
(define (particle-lifetime p) (list-ref p 3))
(define (particle-max-life p) (list-ref p 4))
(define (particle-size p) (list-ref p 5))
(define (particle-color p) (list-ref p 6))
(define (particle-user-data p) (list-ref p 7))

;;; particle-alive? : Particle → Boolean
;;; Check if particle is still alive.
(define (particle-alive? p)
  (> (particle-lifetime p) 0))

;;; particle-age : Particle → Real[0,1]
;;; Get normalized age (0=just born, 1=about to die).
(define (particle-age p)
  (let ([lifetime (particle-lifetime p)]
        [max-life (particle-max-life p)])
       (if (= max-life 0)
           1.0
           (- 1.0 (/ lifetime max-life)))))

;;; particle-with-pos : Particle × Vec2 → Particle
(define (particle-with-pos p new-pos)
  (make-particle new-pos
                 (particle-vel p)
                 (particle-lifetime p)
                 (particle-max-life p)
                 (particle-size p)
                 (particle-color p)
                 (particle-user-data p)))

;;; particle-with-vel : Particle × Vec2 → Particle
(define (particle-with-vel p new-vel)
  (make-particle (particle-pos p)
                 new-vel
                 (particle-lifetime p)
                 (particle-max-life p)
                 (particle-size p)
                 (particle-color p)
                 (particle-user-data p)))

;;; particle-with-state : Particle × Vec2 × Vec2 × Number → Particle
(define (particle-with-state p new-pos new-vel new-lifetime)
  (make-particle new-pos
                 new-vel
                 new-lifetime
                 (particle-max-life p)
                 (particle-size p)
                 (particle-color p)
                 (particle-user-data p)))

;;; ====
;;; Force Fields
;;; ====

;;; A force field is a function: Particle → Vec2
;;; The force is applied as acceleration (assumes unit mass particles).

;;; gravity-field : Vec2 → ForceField
;;; Constant gravitational acceleration.
(define (gravity-field g)
  (lambda (p) g))

;;; wind-field : Vec2 × Number → ForceField
;;; Wind force proportional to strength.
(define (wind-field direction strength)
  (let ([force (vec2-scale (vec2-normalize direction) strength)])
       (lambda (p) force)))

;;; drag-field : Number → ForceField
;;; Velocity-proportional drag (slows particles).
(define (drag-field coefficient)
  (lambda (p)
          (vec2-scale (particle-vel p) (- coefficient))))

;;; attractor-field : Vec2 × Number × Number → ForceField
;;; Point attractor/repeller.
;;;   center: attraction point
;;;   strength: positive = attract, negative = repel
;;;   falloff: 1=linear, 2=inverse-square (realistic)
(define (attractor-field center strength falloff)
  (lambda (p)
          (let* ([to-center (vec2-sub center (particle-pos p))]
                 [dist (vec2-magnitude to-center)])
                (if (< dist 0.001)
                    (vec2 0 0)  ; avoid singularity
                    (let ([magnitude (/ strength (expt dist falloff))])
                         (vec2-scale (vec2-normalize to-center) magnitude))))))

;;; vortex-field : Vec2 × Number × Number → ForceField
;;; Swirling vortex around a center point.
(define (vortex-field center strength falloff)
  (lambda (p)
          (let* ([to-center (vec2-sub center (particle-pos p))]
                 [dist (vec2-magnitude to-center)])
                (if (< dist 0.001)
                    (vec2 0 0)
                    (let* ([tangent (vec2-perp (vec2-normalize to-center))]
                           [magnitude (/ strength (expt dist falloff))])
                          (vec2-scale tangent magnitude))))))

;;; turbulence-field : Number × Number × Number → ForceField
;;; Pseudo-random noise-based turbulence.
;;; Uses simple hash-based noise for determinism.
(define (turbulence-field scale strength seed)
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

;;; combine-fields : (List ForceField) → ForceField
;;; Sum multiple force fields.
(define (combine-fields fields)
  (lambda (p)
          (fold-left (lambda (total field)
                             (vec2-add total (field p)))
                     (vec2 0 0)
                     fields)))

;;; ====
;;; Particle Update
;;; ====

;;; integrate-particle : Particle × ForceField × Number → Particle
;;; Update particle using symplectic Euler integration.
(define (integrate-particle p field dt)
  (let* ([accel (field p)]
         [new-vel (vec2-add (particle-vel p) (vec2-scale accel dt))]
         [new-pos (vec2-add (particle-pos p) (vec2-scale new-vel dt))]
         [new-life (- (particle-lifetime p) dt)])
        (particle-with-state p new-pos new-vel new-life)))

;;; update-particle : Particle × ForceField × Number → Particle
;;; Alias for integrate-particle.
(define update-particle integrate-particle)

;;; update-particles : (List Particle) × ForceField × Number → (List Particle)
;;; Update all particles and remove dead ones.
(define (update-particles particles field dt)
  (filter particle-alive?
          (map (lambda (p) (integrate-particle p field dt))
               particles)))

;;; ====
;;; Emitter Type
;;; ====

;;; Emitter: particle source
;;;   - pos           : Vec2 - emitter position
;;;   - rate          : Number - particles per second
;;;   - spread        : Number - half-angle spread (radians)
;;;   - direction     : Vec2 - emission direction (unit vector)
;;;   - speed-min     : Number - minimum initial speed
;;;   - speed-max     : Number - maximum initial speed
;;;   - lifetime-min  : Number - minimum particle lifetime
;;;   - lifetime-max  : Number - maximum particle lifetime
;;;   - size-min      : Number - minimum particle size
;;;   - size-max      : Number - maximum particle size
;;;   - color         : Any - particle color
;;;   - user-data     : Any - passed to spawned particles
;;;   - accumulator   : Number - fractional particle accumulator

;;; make-emitter : ... → Emitter
(define (make-emitter pos rate spread direction
                      speed-min speed-max
                      lifetime-min lifetime-max
                      size-min size-max
                      color user-data)
  (list 'emitter
        pos rate spread (vec2-normalize direction)
        speed-min speed-max
        lifetime-min lifetime-max
        size-min size-max
        color user-data
        0))  ; accumulator

;;; emitter? : Any → Boolean
(define (emitter? e)
  (and (pair? e) (eq? (car e) 'emitter)))

;;; Emitter accessors
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

;;; emitter-with-pos : Emitter × Vec2 → Emitter
(define (emitter-with-pos e new-pos)
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

;;; emitter-with-direction : Emitter × Vec2 → Emitter
(define (emitter-with-direction e new-dir)
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

;;; emitter-with-accumulator : Emitter × Number → Emitter
(define (emitter-with-accumulator e acc)
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

;;; ====
;;; Emitter Emission
;;; ====

;;; spawn-particle : Emitter × RNG → (Particle × RNG)
;;; Spawn a single particle from the emitter.
(define (spawn-particle e rng)
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

;;; emitter-emit : Emitter × Number × RNG → (List Particle) × Emitter × RNG
;;; Emit particles based on rate and dt.
;;; Returns (particles new-emitter new-rng).
(define (emitter-emit e dt rng)
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

;;; ====
;;; Particle System
;;; ====

;;; ParticleSystem: manages emitters, particles, and forces
;;;   - emitters  : (List Emitter)
;;;   - particles : (List Particle)
;;;   - forces    : ForceField (combined)

;;; make-particle-system : (List Emitter) × (List Particle) × ForceField → ParticleSystem
(define (make-particle-system emitters particles forces)
  (list 'particle-system emitters particles forces))

;;; particle-system? : Any → Boolean
(define (particle-system? ps)
  (and (pair? ps) (eq? (car ps) 'particle-system)))

;;; Accessors
(define (particle-system-emitters ps) (list-ref ps 1))
(define (particle-system-particles ps) (list-ref ps 2))
(define (particle-system-forces ps) (list-ref ps 3))

;;; particle-system-count : ParticleSystem → Nat
;;; Count active particles.
(define (particle-system-count ps)
  (length (particle-system-particles ps)))

;;; particle-system-add-emitter : ParticleSystem × Emitter → ParticleSystem
(define (particle-system-add-emitter ps emitter)
  (make-particle-system (cons emitter (particle-system-emitters ps))
                        (particle-system-particles ps)
                        (particle-system-forces ps)))

;;; particle-system-add-force : ParticleSystem × ForceField → ParticleSystem
(define (particle-system-add-force ps field)
  (make-particle-system (particle-system-emitters ps)
                        (particle-system-particles ps)
                        (combine-fields (list (particle-system-forces ps) field))))

;;; particle-system-add-particles : ParticleSystem × (List Particle) → ParticleSystem
(define (particle-system-add-particles ps new-particles)
  (make-particle-system (particle-system-emitters ps)
                        (append new-particles (particle-system-particles ps))
                        (particle-system-forces ps)))

;;; ====
;;; Particle System Update
;;; ====

;;; particle-system-step : ParticleSystem × Number × RNG → (ParticleSystem × RNG)
;;; Step the particle system forward by dt.
(define (particle-system-step ps dt rng)
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

;;; ====
;;; Convenience Constructors
;;; ====

;;; make-empty-system : → ParticleSystem
;;; Create an empty particle system with no forces.
(define (make-empty-system)
  (make-particle-system '() '() (lambda (p) (vec2 0 0))))

;;; make-simple-emitter : Vec2 × Number → Emitter
;;; Create a simple upward emitter with defaults.
(define (make-simple-emitter pos rate)
  (make-emitter pos
                rate
                0.3          ; spread (radians)
                (vec2 0 -1)  ; upward
                50 100       ; speed range
                1.0 2.0      ; lifetime range
                2 5          ; size range
                'white       ; color
                #f))         ; no user data

;;; make-explosion-emitter : Vec2 × Number → Emitter
;;; Create an omnidirectional burst emitter.
(define (make-explosion-emitter pos particle-count)
  (make-emitter pos
                (* particle-count 60)  ; all in one frame (at 60fps)
                3.14159                ; full spread (pi = half circle each side)
                (vec2 1 0)             ; base direction (spread covers all)
                100 200                ; fast speed
                0.5 1.5                ; short lifetime
                3 8                    ; varied sizes
                'orange                ; color
                #f))

;;; make-fountain-emitter : Vec2 → Emitter
;;; Create a fountain-style upward emitter.
(define (make-fountain-emitter pos)
  (make-emitter pos
                50           ; 50 particles/second
                0.2          ; narrow spread
                (vec2 0 -1)  ; upward
                150 250      ; high speed
                2.0 4.0      ; medium lifetime
                2 4          ; small particles
                'cyan        ; color
                #f))

;;; make-fire-emitter : Vec2 → Emitter
;;; Create a fire-style emitter.
(define (make-fire-emitter pos)
  (make-emitter pos
                100          ; dense
                0.4          ; moderate spread
                (vec2 0 -1)  ; upward
                30 80        ; slow-medium speed
                0.3 1.0      ; short lifetime
                4 10         ; varied sizes
                'red         ; color
                #f))

;;; ====
;;; Burst Emission (One-Shot)
;;; ====

;;; burst : Vec2 × Number × Number × Number × Any × RNG → (List Particle) × RNG
;;; Create a burst of particles at a position.
;;; spread: angle spread (radians), speed: initial speed, count: particle count
(define (burst pos spread speed lifetime count color rng)
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

;;; explosion-burst : Vec2 × Number × RNG → (List Particle) × RNG
;;; Create an explosion burst.
(define (explosion-burst pos count rng)
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

;;; ====
;;; Particle Queries
;;; ====

;;; particles-in-radius : (List Particle) × Vec2 × Number → (List Particle)
;;; Find all particles within radius of a point.
(define (particles-in-radius particles center radius)
  (let ([r-sq (* radius radius)])
       (filter (lambda (p)
                       (<= (vec2-distance-sq (particle-pos p) center) r-sq))
               particles)))

;;; particles-by-age : (List Particle) × Number × Number → (List Particle)
;;; Find particles with age in [min-age, max-age].
(define (particles-by-age particles min-age max-age)
  (filter (lambda (p)
                  (let ([age (particle-age p)])
                       (and (>= age min-age) (<= age max-age))))
          particles))

;;; oldest-particles : (List Particle) × Number → (List Particle)
;;; Get the n oldest particles.
(define (oldest-particles particles n)
  (take n (sort (lambda (a b)
                        (> (particle-age a) (particle-age b)))
                particles)))

;;; ====
;;; Collision Support (Optional)
;;; ====

;;; particle-bounce : Particle × Vec2 × Number → Particle
;;; Bounce particle off a surface with given normal and restitution.
(define (particle-bounce p normal restitution)
  (let* ([vel (particle-vel p)]
         [dot (vec2-dot vel normal)]
         [reflected (vec2-sub vel (vec2-scale normal (* 2 dot)))]
         [new-vel (vec2-scale reflected restitution)])
        (particle-with-vel p new-vel)))

;;; particle-in-bounds? : Particle × Vec2 × Vec2 → Boolean
;;; Check if particle is within rectangular bounds.
(define (particle-in-bounds? p min-corner max-corner)
  (let ([pos (particle-pos p)])
       (and (>= (vec2-x pos) (vec2-x min-corner))
            (<= (vec2-x pos) (vec2-x max-corner))
            (>= (vec2-y pos) (vec2-y min-corner))
            (<= (vec2-y pos) (vec2-y max-corner)))))

;;; filter-bounds : (List Particle) × Vec2 × Vec2 → (List Particle)
;;; Remove particles outside bounds.
(define (filter-bounds particles min-corner max-corner)
  (filter (lambda (p) (particle-in-bounds? p min-corner max-corner))
          particles))

;;; ====
;;; Statistics
;;; ====

;;; particle-system-stats : ParticleSystem → Alist
;;; Get statistics about the particle system.
(define (particle-system-stats ps)
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

;;; ====
;;; Module Load Message
;;; ====

(display "Particle System loaded.\n")
(display "  Create: (make-particle-system emitters particles forces)\n")
(display "  Emitters: (make-simple-emitter pos rate), (make-fire-emitter pos)\n")
(display "  Forces: (gravity-field g), (drag-field c), (attractor-field center s f)\n")
(display "  Step: (particle-system-step system dt rng) -> (system . rng)\n")
