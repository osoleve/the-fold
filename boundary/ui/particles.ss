(load "boundary/ui/color.ss")
(load "boundary/ui/layout-color.ss")
(load "boundary/ui/animation.ss")

(doc 'module 'particles)
(doc 'description "Particle Effects System - Provides particle emitters and effects for visual feedback: Hearts floating up when petted, Sparkles when happy, Ripples in water, Dust/motion trails. Particles are transient visual elements that add life to the scene.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'particle-type)

(doc 'note "Particle : (× Point Point Char Color Nat Nat)")
(doc 'note "A single particle with:")
(doc 'note "  - position   : Current (x, y) position")
(doc 'note "  - velocity   : (dx, dy) velocity per frame")
(doc 'note "  - char       : Character to display")
(doc 'note "  - color      : Particle color")
(doc 'note "  - lifetime   : Frames remaining before particle dies")
(doc 'note "  - max-life   : Total frames particle can live")

(define (make-particle pos vel char color lifetime)
  (doc 'type (-> Point Point Char Color Nat Particle))
  (list pos vel char color lifetime lifetime))

(define (particle-position p)    (list-ref p 0))
(define (particle-velocity p)    (list-ref p 1))
(define (particle-char p)        (list-ref p 2))
(define (particle-color p)       (list-ref p 3))
(define (particle-lifetime p)    (list-ref p 4))
(define (particle-max-life p)    (list-ref p 5))

(define (particle-alive? p)
  (doc 'type (-> Particle Bool))
  (doc 'description "Check if particle is still alive.")
  (> (particle-lifetime p) 0))

(define (particle-age p)
  (doc 'type (-> Particle Real))
  (doc 'description "Get normalized age (0=just born, 1=about to die).")
  (let ([lifetime (particle-lifetime p)]
        [max-life (particle-max-life p)])
       (- 1.0 (/ (exact->inexact lifetime) (exact->inexact max-life)))))

(doc 'section 'particle-update)

(define (update-particle p)
  (doc 'type (-> Particle Particle))
  (doc 'description "Update particle for one frame: Move by velocity, Decrease lifetime")
  (let* ([pos (particle-position p)]
         [vel (particle-velocity p)]
         [new-x (+ (point-x pos) (point-x vel))]
         [new-y (+ (point-y pos) (point-y vel))]
         [new-pos (point new-x new-y)]
         [new-lifetime (- (particle-lifetime p) 1)])
        (list new-pos
              vel
              (particle-char p)
              (particle-color p)
              new-lifetime
              (particle-max-life p))))

(define (update-particles particles)
  (doc 'type (-> (List Particle) (List Particle)))
  (doc 'description "Update all particles and remove dead ones.")
  (filter particle-alive?
          (map update-particle particles)))

(doc 'section 'particle-rendering)

(define (render-particle canvas p)
  (doc 'type (-> Canvas Particle Canvas))
  (doc 'description "Render a single particle to the canvas. Particles fade out as they age.")
  (let* ([pos (particle-position p)]
         [x (inexact->exact (round (point-x pos)))]
         [y (inexact->exact (round (point-y pos)))]
         [char (particle-char p)]
         [color (particle-color p)]
         [age (particle-age p)]
         ;; Fade color as particle ages
         [faded-color (if (> age 0.7)
                          ;; Fade to darker color near death
                          (darken color 0.5)
                          color)])
        ;; Only render if within canvas bounds
        (if (and (>= x 0) (< x (canvas-width canvas))
                 (>= y 0) (< y (canvas-height canvas)))
            (draw-char-colored canvas pos char faded-color color-default)
            canvas)))

(define (render-particles canvas particles)
  (doc 'type (-> Canvas (List Particle) Canvas))
  (doc 'description "Render all particles to the canvas.")
  (if (null? particles)
      canvas
      (render-particles (render-particle canvas (car particles))
                        (cdr particles))))

(doc 'section 'particle-emitters-hearts)

(define (emit-hearts origin)
  (doc 'type (-> Point (List Particle)))
  (doc 'description "Emit heart particles that float upward. Used for affection/petting interactions.")
  (let ([x (point-x origin)]
        [y (point-y origin)])
       (list
        (make-particle (point (+ x 0) y) (point 0.0 -0.5) #\♥ color-pink 30)
        (make-particle (point (+ x 2) y) (point 0.2 -0.6) #\♡ color-red 25)
        (make-particle (point (+ x -1) y) (point -0.1 -0.4) #\♥ color-pink 28))))

(doc 'section 'particle-emitters-sparkles)

(define (emit-sparkles origin)
  (doc 'type (-> Point (List Particle)))
  (doc 'description "Emit sparkle particles that radiate outward. Used for happiness/excitement.")
  (let ([x (point-x origin)]
        [y (point-y origin)])
       (list
        (make-particle (point x y) (point 0.5 -0.3) #\* color-yellow 20)
        (make-particle (point x y) (point -0.5 -0.3) #\✨ color-gold 20)
        (make-particle (point x y) (point 0.3 0.3) #\* color-yellow 15)
        (make-particle (point x y) (point -0.3 0.3) #\✦ color-gold 15)
        (make-particle (point x y) (point 0.0 -0.6) #\✨ color-white 25))))

(doc 'section 'particle-emitters-bubbles)

(define (emit-bubbles origin)
  (doc 'type (-> Point (List Particle)))
  (doc 'description "Emit bubble particles that float upward. Used for water/pond interactions.")
  (let ([x (point-x origin)]
        [y (point-y origin)])
       (list
        (make-particle (point x y) (point 0.1 -0.4) #\○ color-cyan 35)
        (make-particle (point (+ x 2) y) (point -0.1 -0.5) #\◦ color-blue 30)
        (make-particle (point (+ x 1) (+ y 2)) (point 0.0 -0.3) #\○ color-cyan 40))))

(doc 'section 'particle-emitters-ripples)

(define (emit-ripple origin)
  (doc 'type (-> Point (List Particle)))
  (doc 'description "Emit ripple particles that expand outward. Used for splashes and impacts.")
  (let ([x (point-x origin)]
        [y (point-y origin)])
       (list
        (make-particle (point x y) (point 0.0 0.0) #\○ color-blue 10)
        (make-particle (point x y) (point 0.8 0.0) #\~ color-cyan 12)
        (make-particle (point x y) (point -0.8 0.0) #\~ color-cyan 12)
        (make-particle (point x y) (point 0.0 0.5) #\~ color-blue 11)
        (make-particle (point x y) (point 0.0 -0.5) #\~ color-blue 11))))

(doc 'section 'particle-emitters-stars)

(define (emit-stars origin)
  (doc 'type (-> Point (List Particle)))
  (doc 'description "Emit twinkling star particles. Used for curious/wonder moments.")
  (let ([x (point-x origin)]
        [y (point-y origin)])
       (list
        (make-particle (point (+ x 1) (- y 2)) (point 0.0 0.0) #\★ color-yellow 25)
        (make-particle (point (+ x -2) (- y 1)) (point 0.0 0.0) #\☆ color-gold 20)
        (make-particle (point (+ x 3) (- y 3)) (point 0.0 0.0) #\✦ color-white 22))))

(doc 'section 'particle-emitters-zzz)

(define (emit-zzz origin)
  (doc 'type (-> Point (List Particle)))
  (doc 'description "Emit \"Z\" particles that drift upward. Used for sleep/drowsiness.")
  (let ([x (point-x origin)]
        [y (point-y origin)])
       (list
        (make-particle (point (+ x 2) (- y 1)) (point 0.2 -0.3) #\Z color-purple 30)
        (make-particle (point (+ x 3) (- y 2)) (point 0.1 -0.2) #\z color-purple 25))))

(doc 'section 'particle-emitters-notes)

(define (emit-notes origin)
  (doc 'type (-> Point (List Particle)))
  (doc 'description "Emit musical note particles. Used for playful/energetic moments.")
  (let ([x (point-x origin)]
        [y (point-y origin)])
       (list
        (make-particle (point (+ x 1) y) (point 0.3 -0.4) #\♪ color-pink 28)
        (make-particle (point (+ x 3) (- y 1)) (point -0.2 -0.5) #\♫ color-magenta 25)
        (make-particle (point (+ x 2) (+ y 1)) (point 0.1 -0.3) #\♬ color-pink 30))))

(doc 'section 'particle-emitters-exclamation)

(define (emit-exclamation origin)
  (doc 'type (-> Point (List Particle)))
  (doc 'description "Emit exclamation/surprise particles. Used for startled/surprised reactions.")
  (let ([x (point-x origin)]
        [y (point-y origin)])
       (list
        (make-particle (point (+ x 1) (- y 2)) (point 0.0 -0.2) #\! color-red 20)
        (make-particle (point (+ x 2) (- y 2)) (point 0.0 -0.2) #\! color-yellow 20))))

(doc 'section 'particle-system-state)

(doc 'note "ParticleSystem : (List Particle)")
(doc 'note "Collection of active particles.")

(define (make-particle-system)
  (doc 'type (-> ParticleSystem))
  '())

(define (add-particles system new-particles)
  (doc 'type (-> ParticleSystem (List Particle) ParticleSystem))
  (doc 'description "Add new particles to the system.")
  (append system new-particles))

(define (update-particle-system system)
  (doc 'type (-> ParticleSystem ParticleSystem))
  (doc 'description "Update all particles in the system (movement + lifetime).")
  (update-particles system))

(define (render-particle-system canvas system)
  (doc 'type (-> Canvas ParticleSystem Canvas))
  (doc 'description "Render all particles in the system to a canvas.")
  (render-particles canvas system))

(doc 'section 'convenience-functions)

(define (emit-by-mood origin mood)
  (doc 'type (-> Point Mood (List Particle)))
  (doc 'description "Emit particles appropriate for a given mood.")
  (case mood
        [(happy)    (emit-sparkles origin)]
        [(curious)  (emit-stars origin)]
        [(sleepy)   (emit-zzz origin)]
        [(content)  (emit-bubbles origin)]
        [(lonely)   '()]  ; No particles when lonely
        [(playful)  (emit-notes origin)]
        [else       '()]))

(define (emit-by-interaction origin interaction)
  (doc 'type (-> Point Symbol (List Particle)))
  (doc 'description "Emit particles for specific interactions.")
  (case interaction
        [(pet)    (emit-hearts origin)]
        [(feed)   (emit-sparkles origin)]
        [(play)   (emit-notes origin)]
        [(talk)   (emit-bubbles origin)]
        [(splash) (emit-ripple origin)]
        [else     '()]))
