;;; thimble/particles.ss — Particle Effects System
;;;
;;; Provides particle emitters and effects for visual feedback:
;;;   - Hearts floating up when petted
;;;   - Sparkles when happy
;;;   - Ripples in water
;;;   - Dust/motion trails
;;;
;;; This is Shell code: handles particle physics and rendering.
;;; Particles are transient visual elements that add life to the scene.

;;; ============================================================
;;; Dependencies
;;; ============================================================

(load "thimble/color.ss")
(load "thimble/layout-color.ss")
(load "thimble/animation.ss")

;;; ============================================================
;;; Particle Type
;;; ============================================================

;;; Particle : (× Point Point Char Color Nat Nat)
;;;
;;; A single particle with:
;;;   - position   : Current (x, y) position
;;;   - velocity   : (dx, dy) velocity per frame
;;;   - char       : Character to display
;;;   - color      : Particle color
;;;   - lifetime   : Frames remaining before particle dies
;;;   - max-life   : Total frames particle can live

(define (make-particle pos vel char color lifetime)
  (list pos vel char color lifetime lifetime))

(define (particle-position p)    (list-ref p 0))
(define (particle-velocity p)    (list-ref p 1))
(define (particle-char p)        (list-ref p 2))
(define (particle-color p)       (list-ref p 3))
(define (particle-lifetime p)    (list-ref p 4))
(define (particle-max-life p)    (list-ref p 5))

;;; particle-alive? : Particle → Bool
;;;
;;; Check if particle is still alive.
(define (particle-alive? p)
  (> (particle-lifetime p) 0))

;;; particle-age : Particle → Real[0,1]
;;;
;;; Get normalized age (0=just born, 1=about to die).
(define (particle-age p)
  (let ([lifetime (particle-lifetime p)]
        [max-life (particle-max-life p)])
    (- 1.0 (/ (exact->inexact lifetime) (exact->inexact max-life)))))

;;; ============================================================
;;; Particle Update
;;; ============================================================

;;; update-particle : Particle → Particle
;;;
;;; Update particle for one frame:
;;;   - Move by velocity
;;;   - Decrease lifetime
(define (update-particle p)
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

;;; update-particles : (List Particle) → (List Particle)
;;;
;;; Update all particles and remove dead ones.
(define (update-particles particles)
  (filter particle-alive?
          (map update-particle particles)))

;;; ============================================================
;;; Particle Rendering
;;; ============================================================

;;; render-particle : Canvas × Particle → Canvas
;;;
;;; Render a single particle to the canvas.
;;; Particles fade out as they age.
(define (render-particle canvas p)
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

;;; render-particles : Canvas × (List Particle) → Canvas
;;;
;;; Render all particles to the canvas.
(define (render-particles canvas particles)
  (if (null? particles)
      canvas
      (render-particles (render-particle canvas (car particles))
                       (cdr particles))))

;;; ============================================================
;;; Particle Emitters — Hearts
;;; ============================================================

;;; emit-hearts : Point → (List Particle)
;;;
;;; Emit heart particles that float upward.
;;; Used for affection/petting interactions.
(define (emit-hearts origin)
  (let ([x (point-x origin)]
        [y (point-y origin)])
    (list
      (make-particle (point (+ x 0) y) (point 0.0 -0.5) #\♥ color-pink 30)
      (make-particle (point (+ x 2) y) (point 0.2 -0.6) #\♡ color-red 25)
      (make-particle (point (+ x -1) y) (point -0.1 -0.4) #\♥ color-pink 28))))

;;; ============================================================
;;; Particle Emitters — Sparkles
;;; ============================================================

;;; emit-sparkles : Point → (List Particle)
;;;
;;; Emit sparkle particles that radiate outward.
;;; Used for happiness/excitement.
(define (emit-sparkles origin)
  (let ([x (point-x origin)]
        [y (point-y origin)])
    (list
      (make-particle (point x y) (point 0.5 -0.3) #\* color-yellow 20)
      (make-particle (point x y) (point -0.5 -0.3) #\✨ color-gold 20)
      (make-particle (point x y) (point 0.3 0.3) #\* color-yellow 15)
      (make-particle (point x y) (point -0.3 0.3) #\✦ color-gold 15)
      (make-particle (point x y) (point 0.0 -0.6) #\✨ color-white 25))))

;;; ============================================================
;;; Particle Emitters — Bubbles
;;; ============================================================

;;; emit-bubbles : Point → (List Particle)
;;;
;;; Emit bubble particles that float upward.
;;; Used for water/pond interactions.
(define (emit-bubbles origin)
  (let ([x (point-x origin)]
        [y (point-y origin)])
    (list
      (make-particle (point x y) (point 0.1 -0.4) #\○ color-cyan 35)
      (make-particle (point (+ x 2) y) (point -0.1 -0.5) #\◦ color-blue 30)
      (make-particle (point (+ x 1) (+ y 2)) (point 0.0 -0.3) #\○ color-cyan 40))))

;;; ============================================================
;;; Particle Emitters — Ripples
;;; ============================================================

;;; emit-ripple : Point → (List Particle)
;;;
;;; Emit ripple particles that expand outward.
;;; Used for splashes and impacts.
(define (emit-ripple origin)
  (let ([x (point-x origin)]
        [y (point-y origin)])
    (list
      (make-particle (point x y) (point 0.0 0.0) #\○ color-blue 10)
      (make-particle (point x y) (point 0.8 0.0) #\~ color-cyan 12)
      (make-particle (point x y) (point -0.8 0.0) #\~ color-cyan 12)
      (make-particle (point x y) (point 0.0 0.5) #\~ color-blue 11)
      (make-particle (point x y) (point 0.0 -0.5) #\~ color-blue 11))))

;;; ============================================================
;;; Particle Emitters — Stars
;;; ============================================================

;;; emit-stars : Point → (List Particle)
;;;
;;; Emit twinkling star particles.
;;; Used for curious/wonder moments.
(define (emit-stars origin)
  (let ([x (point-x origin)]
        [y (point-y origin)])
    (list
      (make-particle (point (+ x 1) (- y 2)) (point 0.0 0.0) #\★ color-yellow 25)
      (make-particle (point (+ x -2) (- y 1)) (point 0.0 0.0) #\☆ color-gold 20)
      (make-particle (point (+ x 3) (- y 3)) (point 0.0 0.0) #\✦ color-white 22))))

;;; ============================================================
;;; Particle Emitters — Sleepy Z's
;;; ============================================================

;;; emit-zzz : Point → (List Particle)
;;;
;;; Emit "Z" particles that drift upward.
;;; Used for sleep/drowsiness.
(define (emit-zzz origin)
  (let ([x (point-x origin)]
        [y (point-y origin)])
    (list
      (make-particle (point (+ x 2) (- y 1)) (point 0.2 -0.3) #\Z color-purple 30)
      (make-particle (point (+ x 3) (- y 2)) (point 0.1 -0.2) #\z color-purple 25))))

;;; ============================================================
;;; Particle Emitters — Music Notes
;;; ============================================================

;;; emit-notes : Point → (List Particle)
;;;
;;; Emit musical note particles.
;;; Used for playful/energetic moments.
(define (emit-notes origin)
  (let ([x (point-x origin)]
        [y (point-y origin)])
    (list
      (make-particle (point (+ x 1) y) (point 0.3 -0.4) #\♪ color-pink 28)
      (make-particle (point (+ x 3) (- y 1)) (point -0.2 -0.5) #\♫ color-magenta 25)
      (make-particle (point (+ x 2) (+ y 1)) (point 0.1 -0.3) #\♬ color-pink 30))))

;;; ============================================================
;;; Particle Emitters — Exclamation
;;; ============================================================

;;; emit-exclamation : Point → (List Particle)
;;;
;;; Emit exclamation/surprise particles.
;;; Used for startled/surprised reactions.
(define (emit-exclamation origin)
  (let ([x (point-x origin)]
        [y (point-y origin)])
    (list
      (make-particle (point (+ x 1) (- y 2)) (point 0.0 -0.2) #\! color-red 20)
      (make-particle (point (+ x 2) (- y 2)) (point 0.0 -0.2) #\! color-yellow 20))))

;;; ============================================================
;;; Particle System State
;;; ============================================================

;;; ParticleSystem : (List Particle)
;;;
;;; Collection of active particles.

(define (make-particle-system)
  '())

;;; add-particles : ParticleSystem × (List Particle) → ParticleSystem
;;;
;;; Add new particles to the system.
(define (add-particles system new-particles)
  (append system new-particles))

;;; update-particle-system : ParticleSystem → ParticleSystem
;;;
;;; Update all particles in the system (movement + lifetime).
(define (update-particle-system system)
  (update-particles system))

;;; render-particle-system : Canvas × ParticleSystem → Canvas
;;;
;;; Render all particles in the system to a canvas.
(define (render-particle-system canvas system)
  (render-particles canvas system))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; emit-by-mood : Point × Mood → (List Particle)
;;;
;;; Emit particles appropriate for a given mood.
(define (emit-by-mood origin mood)
  (case mood
    [(happy)    (emit-sparkles origin)]
    [(curious)  (emit-stars origin)]
    [(sleepy)   (emit-zzz origin)]
    [(content)  (emit-bubbles origin)]
    [(lonely)   '()]  ; No particles when lonely
    [(playful)  (emit-notes origin)]
    [else       '()]))

;;; emit-by-interaction : Point × Symbol → (List Particle)
;;;
;;; Emit particles for specific interactions.
(define (emit-by-interaction origin interaction)
  (case interaction
    [(pet)    (emit-hearts origin)]
    [(feed)   (emit-sparkles origin)]
    [(play)   (emit-notes origin)]
    [(talk)   (emit-bubbles origin)]
    [(splash) (emit-ripple origin)]
    [else     '()]))
