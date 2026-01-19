;;; user/creations/type-system-demo.ss --- THE FOLD TYPE SYSTEM
;;;
;;; A demoscene visualization of The Fold's type system using SDF raymarching.
;;; 800x600 (100x42 chars), 60fps, 6+ seconds of pure type theory eye candy.
;;;
;;; Concept:
;;;   - Kinds as dimensions (*, * → *, (* → *) → *)
;;;   - Type classes as containers (Functor = torus, Monad = nested)
;;;   - Instance resolution as geometric fitting
;;;   - Starfields, sine scrollers, and greets

(load "core/base/prelude.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/sdf-raymarcher.ss")
(load "boundary/ui/animation.ss")

;;; ====
;;; Scene Configuration
;;; ====

(define *width* 100)
(define *height* 42)
(define *fps* 60)
(define *total-duration* 6.5)  ; seconds
(define *total-frames* (inexact->exact (floor (* *fps* *total-duration*))))

;;; Phase timing (in frames)
(define *phase-title-start* 0)
(define *phase-title-end* 90)       ; 1.5s
(define *phase-kinds-start* 90)
(define *phase-kinds-end* 210)      ; 2s
(define *phase-classes-start* 210)
(define *phase-classes-end* 300)    ; 1.5s
(define *phase-resolution-start* 300)
(define *phase-resolution-end* 360) ; 1s
(define *phase-greets-start* 360)
(define *phase-greets-end* 390)     ; 0.5s

;;; ====
;;; Utility Functions
;;; ====

(define (phase-t frame start-frame end-frame)
  "Normalized time [0,1] for a phase"
  (if (or (< frame start-frame) (>= frame end-frame))
      (if (< frame start-frame) 0.0 1.0)
      (/ (- frame start-frame) (- end-frame start-frame))))

(define (char-at-intensity intensity)
  "Map intensity [0,1] to ASCII density character"
  (let* ([chars " .:;+=*#@"]
         [n (string-length chars)]
         [idx (min (- n 1) (max 0 (inexact->exact (floor (* intensity n)))))])
        (string-ref chars idx)))

;;; ====
;;; Starfield Background
;;; ====

(define *stars*
  ;; Generate pseudo-random star positions (deterministic)
  (let loop ([n 120] [acc '()])
       (if (<= n 0)
           acc
           (let* ([seed (+ (* n 7919) 1337)]
                  [x (modulo (* seed 127) *width*)]
                  [y (modulo (* seed 251) *height*)]
                  [z (+ 0.1 (* 0.9 (/ (modulo (* seed 31) 100) 100.0)))])  ; depth
                 (loop (- n 1) (cons (list x y z) acc))))))

(define (render-starfield frame-num width height)
  "Render twinkling starfield"
  (let ([frame (make-frame width height #\space)]
        [time (* frame-num 0.05)])
       (for-each
        (lambda (star)
                (let* ([x (car star)]
                       [y (cadr star)]
                       [z (caddr star)]
                       ;; Twinkle based on star's position and time
                       [twinkle (+ 0.3 (* 0.7 (abs (sin (+ time (* z 3.14159))))))]
                       [char (if (> twinkle 0.7) #\. (if (> twinkle 0.5) #\: #\space))])
                      (when (and (>= x 0) (< x width) (>= y 0) (< y height))
                            (frame-set! frame x y char))))
        *stars*)
       frame))

;;; ====
;;; Text Rendering with Effects
;;; ====

(define (render-centered-text frame y-pos text)
  "Render text centered at y-pos"
  (let* ([w (frame-width frame)]
         [x-start (max 0 (inexact->exact (floor (/ (- w (string-length text)) 2))))])
        (frame-put-string! frame x-start y-pos text)))

(define (render-sine-text frame y-base text amplitude phase)
  "Render text with sine wave vertical displacement"
  (let* ([w (frame-width frame)]
         [len (string-length text)]
         [x-start (max 0 (inexact->exact (floor (/ (- w len) 2))))])
        (do ([i 0 (+ i 1)])
            ((>= i len))
            (let* ([char (string-ref text i)]
                   [x (+ x-start i)]
                   [wave-offset (inexact->exact
                                 (floor (* amplitude (sin (+ phase (* i 0.3))))))]
                   [y (+ y-base wave-offset)])
                  (when (and (>= x 0) (< x w) (>= y 0) (< y (frame-height frame)))
                        (frame-set! frame x y char))))))

(define (render-text-with-fade frame y-pos text fade-t)
  "Render text with character-by-character fade"
  (let* ([w (frame-width frame)]
         [len (string-length text)]
         [x-start (max 0 (inexact->exact (floor (/ (- w len) 2))))]
         [chars-visible (inexact->exact (floor (* fade-t len)))])
        (do ([i 0 (+ i 1)])
            ((>= i (min len chars-visible)))
            (let ([char (string-ref text i)]
                  [x (+ x-start i)])
                 (when (and (>= x 0) (< x w) (>= y-pos 0) (< y-pos (frame-height frame)))
                       (frame-set! frame x y-pos char))))))

;;; ====
;;; Particle System (for sparkles)
;;; ====

(define (generate-particles center-x center-y count)
  "Generate particles around a center point"
  (let loop ([n count] [acc '()])
       (if (<= n 0)
           acc
           (let* ([angle (* 6.28318 (/ n count))]
                  [radius (* 0.5 (+ 1.0 (/ n count)))]
                  [vx (* radius (cos angle))]
                  [vy (* radius (sin angle))])
                 (loop (- n 1) (cons (list vx vy) acc))))))

(define (render-particles frame cx cy particles time)
  "Render expanding particles"
  (for-each
   (lambda (p)
           (let* ([vx (car p)]
                  [vy (cadr p)]
                  [expand (* time 8.0)]
                  [x (+ cx (inexact->exact (floor (* vx expand))))]
                  [y (+ cy (inexact->exact (floor (* vy expand))))]
                  [brightness (- 1.0 time)])
                 (when (and (> brightness 0.2)
                           (>= x 0) (< x (frame-width frame))
                           (>= y 0) (< y (frame-height frame)))
                       (frame-set! frame x y
                                  (if (> brightness 0.6) #\*
                                      (if (> brightness 0.4) #\+ #\.))))))
   particles))

;;; ====
;;; Phase 1: Title Intro
;;; ====

(define (render-title-phase frame-num)
  "Title: THE FOLD with starfield background"
  (let* ([t (phase-t frame-num *phase-title-start* *phase-title-end*)]
         [frame (render-starfield frame-num *width* *height*)]
         [fade-in-t (ease-out-cubic (min 1.0 (* t 2.0)))]
         [fade-out-t (if (> t 0.8) (ease-in-cubic (/ (- t 0.8) 0.2)) 0.0)])

        ;; Main title with fade-in
        (when (> fade-in-t 0.05)
              (render-text-with-fade frame 16 "THE FOLD" fade-in-t))

        ;; Subtitle appears after title
        (when (> t 0.4)
              (let ([subtitle-t (ease-out-quad (/ (- t 0.4) 0.6))])
                   (render-text-with-fade frame 18 "A TYPE SYSTEM VISUALIZATION" subtitle-t)))

        ;; Fade to black at end
        (when (> fade-out-t 0)
              (let ([darkness (inexact->exact (floor (* fade-out-t 10)))])
                   (do ([i 0 (+ i 1)])
                       ((>= i darkness))
                       ;; Overlay dark chars randomly
                       (let ([x (modulo (* i 127) *width*)]
                             [y (modulo (* i 251) *height*)])
                            (frame-set! frame x y #\space)))))

        frame))

;;; ====
;;; Phase 2: Kind Hierarchy
;;; ====

(define (make-kind-scene angle kind-level)
  "Generate SDF scene showing kind hierarchy"
  (lambda (p)
          (let* ([rotated (rotate-y (rotate-x p (* 0.2 angle)) angle)]
                 ;; * = sphere (simple type)
                 [sphere-basic (sdf-sphere rotated (vec3 0 0 0) 0.5)]
                 ;; * → * = torus (type constructor with one hole)
                 [torus-unary (sdf-torus rotated (vec3 0 0 0) 0.6 0.2)]
                 ;; * → * → * = nested toruses
                 [torus-binary-1 (sdf-torus rotated (vec3 0 0 0) 0.7 0.15)]
                 [torus-binary-2 (sdf-torus (rotate-x rotated 1.5708) (vec3 0 0 0) 0.7 0.15)]
                 ;; (* → *) → * = sphere inside torus (higher-kinded)
                 [hkt-outer (sdf-torus rotated (vec3 0 0 0) 0.8 0.25)]
                 [hkt-inner (sdf-sphere rotated (vec3 0 0 0) 0.35)])

                (cond
                 [(= kind-level 0) sphere-basic]
                 [(= kind-level 1) torus-unary]
                 [(= kind-level 2) (sdf-union torus-binary-1 torus-binary-2)]
                 [(= kind-level 3) (sdf-union hkt-outer hkt-inner)]
                 [else sphere-basic]))))

(define (render-kinds-phase frame-num)
  "Show kind hierarchy: *, * → *, * → * → *, (* → *) → *"
  (let* ([t (phase-t frame-num *phase-kinds-start* *phase-kinds-end*)]
         [bg (render-starfield frame-num *width* *height*)]
         [angle (* t 3.14159 2)]  ; full rotation
         ;; Transition through kind levels
         [kind-level (inexact->exact (floor (* t 4)))]
         [level-t (- (* t 4) kind-level)]
         [smooth-t (ease-in-out-cubic level-t)])

        ;; Render 3D shape in center
        (let* ([cam-dist 2.5]
               [camera (make-camera (vec3 0 0 (- cam-dist))
                                   (vec3 0 0 0)
                                   (vec3 0 1 0)
                                   1.0)]
               [scene (make-kind-scene angle kind-level)]
               [render-w 50]
               [render-h 24]
               [sdf-frame (render-sdf-frame scene camera render-w render-h)]
               [offset-x (inexact->exact (floor (/ (- *width* render-w) 2)))]
               [offset-y 2])

              ;; Composite SDF render onto background
              (do ([y 0 (+ y 1)])
                  ((>= y render-h))
                  (do ([x 0 (+ x 1)])
                      ((>= x render-w))
                      (let ([char (frame-ref sdf-frame x y)])
                           (when (not (char=? char #\space))
                                 (frame-set! bg (+ x offset-x) (+ y offset-y) char))))))

        ;; Labels for current kind
        (let ([labels '("KIND: *"
                       "KIND: * -> *"
                       "KIND: * -> * -> *"
                       "KIND: (* -> *) -> *")]
              [descriptions '("Base types (Nat, Bool, String)"
                            "Type constructors (List, Option)"
                            "Binary constructors (Either, Pair)"
                            "Higher-kinded (Functor, Monad)")])
             (when (< kind-level 4)
                   (let ([label (list-ref labels kind-level)]
                         [desc (list-ref descriptions kind-level)])
                        (render-centered-text bg 28 label)
                        (render-centered-text bg 30 desc))))

        bg))

;;; ====
;;; Phase 3: Type Classes
;;; ====

(define (make-functor-scene angle inner-size)
  "Functor: torus (container) with variable inner sphere"
  (lambda (p)
          (let* ([rotated (rotate-y (rotate-x p (* 0.3 angle)) angle)]
                 [container (sdf-torus rotated (vec3 0 0 0) 0.7 0.2)]
                 [inner (sdf-sphere rotated (vec3 0 0 0) inner-size)])
                (sdf-union container inner))))

(define (make-monad-scene angle composition-t)
  "Monad: nested structure showing bind composition"
  (lambda (p)
          (let* ([rotated (rotate-y (rotate-x p (* 0.3 angle)) angle)]
                 ;; Outer monad layer
                 [outer (sdf-torus rotated (vec3 0 0 0) 0.8 0.18)]
                 ;; Inner monad layer (moving based on composition-t)
                 [inner-offset (* composition-t 0.3)]
                 [inner (sdf-torus rotated (vec3 0 inner-offset 0) 0.5 0.15)]
                 ;; Core value
                 [core (sdf-sphere rotated (vec3 0 inner-offset 0) 0.15)])
                (sdf-union outer (sdf-union inner core)))))

(define (render-classes-phase frame-num)
  "Visualize Functor and Monad type classes"
  (let* ([t (phase-t frame-num *phase-classes-start* *phase-classes-end*)]
         [bg (render-starfield frame-num *width* *height*)]
         [angle (* t 3.14159 2)]
         [half-t (if (< t 0.5) (/ t 0.5) 1.0)]
         [second-half-t (if (< t 0.5) 0.0 (/ (- t 0.5) 0.5))])

        ;; First half: Functor (fmap over container)
        (if (< t 0.5)
            (let* ([inner-grow (ease-out-elastic half-t)]
                   [inner-size (* 0.4 inner-grow)]
                   [camera (make-camera (vec3 0 0 -2.5) (vec3 0 0 0) (vec3 0 1 0) 1.0)]
                   [scene (make-functor-scene angle inner-size)]
                   [render-w 50]
                   [render-h 24]
                   [sdf-frame (render-sdf-frame scene camera render-w render-h)]
                   [offset-x (inexact->exact (floor (/ (- *width* render-w) 2)))]
                   [offset-y 2])

                  ;; Composite
                  (do ([y 0 (+ y 1)])
                      ((>= y render-h))
                      (do ([x 0 (+ x 1)])
                          ((>= x render-w))
                          (let ([char (frame-ref sdf-frame x y)])
                               (when (not (char=? char #\space))
                                     (frame-set! bg (+ x offset-x) (+ y offset-y) char)))))

                  ;; Label
                  (render-centered-text bg 28 "FUNCTOR: fmap : (a -> b) -> f a -> f b")
                  (render-centered-text bg 30 "Mapping inside a container"))

            ;; Second half: Monad (bind composition)
            (let* ([composition-t (ease-in-out-cubic second-half-t)]
                   [camera (make-camera (vec3 0 0 -2.8) (vec3 0 0 0) (vec3 0 1 0) 1.0)]
                   [scene (make-monad-scene angle composition-t)]
                   [render-w 50]
                   [render-h 24]
                   [sdf-frame (render-sdf-frame scene camera render-w render-h)]
                   [offset-x (inexact->exact (floor (/ (- *width* render-w) 2)))]
                   [offset-y 2])

                  ;; Composite
                  (do ([y 0 (+ y 1)])
                      ((>= y render-h))
                      (do ([x 0 (+ x 1)])
                          ((>= x render-w))
                          (let ([char (frame-ref sdf-frame x y)])
                               (when (not (char=? char #\space))
                                     (frame-set! bg (+ x offset-x) (+ y offset-y) char)))))

                  ;; Label
                  (render-centered-text bg 28 "MONAD: >>= : m a -> (a -> m b) -> m b")
                  (render-centered-text bg 30 "Composing effectful computations")))

        bg))

;;; ====
;;; Phase 4: Instance Resolution
;;; ====

(define (make-resolution-scene t)
  "Show type unification: sphere fitting into slot"
  (lambda (p)
          (let* ([slot-pos (vec3 -0.8 0 0)]
                 [sphere-start-pos (vec3 0.8 0.5 0)]
                 ;; Sphere moves toward slot
                 [interp-t (ease-in-out-cubic t)]
                 [sphere-x (+ (* (vec3-x sphere-start-pos) (- 1.0 interp-t))
                             (* (vec3-x slot-pos) interp-t))]
                 [sphere-y (+ (* (vec3-y sphere-start-pos) (- 1.0 interp-t))
                             (* (vec3-y slot-pos) interp-t))]
                 [sphere-pos (vec3 sphere-x sphere-y 0)]
                 ;; Slot (box with sphere carved out)
                 [box (sdf-box p slot-pos (vec3 0.35 0.35 0.35))]
                 [hole (sdf-sphere p slot-pos 0.25)]
                 [slot (sdf-subtract box hole)]
                 ;; Moving sphere (instance)
                 [sphere (sdf-sphere p sphere-pos 0.24)])
                ;; Union of slot and sphere
                (sdf-smooth-union slot sphere 0.1))))

(define (render-resolution-phase frame-num)
  "Instance resolution: types fitting into slots"
  (let* ([t (phase-t frame-num *phase-resolution-start* *phase-resolution-end*)]
         [bg (render-starfield frame-num *width* *height*)]
         [camera (make-camera (vec3 0 0 -2.5) (vec3 0 0 0) (vec3 0 1 0) 1.2)]
         [scene (make-resolution-scene t)]
         [render-w 50]
         [render-h 24]
         [sdf-frame (render-sdf-frame scene camera render-w render-h)]
         [offset-x (inexact->exact (floor (/ (- *width* render-w) 2)))]
         [offset-y 2])

        ;; Composite
        (do ([y 0 (+ y 1)])
            ((>= y render-h))
            (do ([x 0 (+ x 1)])
                ((>= x render-w))
                (let ([char (frame-ref sdf-frame x y)])
                     (when (not (char=? char #\space))
                           (frame-set! bg (+ x offset-x) (+ y offset-y) char)))))

        ;; Labels
        (render-centered-text bg 28 "INSTANCE RESOLUTION")
        (render-centered-text bg 30 "Finding the right type class implementation")

        ;; Particle burst when sphere reaches slot
        (when (> t 0.85)
              (let* ([burst-t (/ (- t 0.85) 0.15)]
                     [particles (generate-particles 50 14 12)])
                   (render-particles bg 50 14 particles burst-t)))

        bg))

;;; ====
;;; Phase 5: Greets
;;; ====

(define (render-greets-phase frame-num)
  "Demoscene-style greets"
  (let* ([t (phase-t frame-num *phase-greets-start* *phase-greets-end*)]
         [frame (render-starfield frame-num *width* *height*)]
         [sine-phase (* t 6.28318)]
         [fade-in-t (ease-out-cubic (min 1.0 (* t 3.0)))])

        ;; Sine scroller greets
        (when (> fade-in-t 0.1)
              (render-sine-text frame 14 "GREETS TO:" 3.0 sine-phase)
              (render-sine-text frame 18 "CURRY-HOWARD" 2.5 (+ sine-phase 0.5))
              (render-sine-text frame 21 "HINDLEY-MILNER" 2.5 (+ sine-phase 1.0))
              (render-sine-text frame 24 "WADLER-BLOTT" 2.5 (+ sine-phase 1.5))
              (render-sine-text frame 27 "PIERCE-TYPES-AND-PROGRAMMING-LANGUAGES" 2.0 (+ sine-phase 2.0)))

        ;; Footer
        (when (> t 0.3)
              (render-centered-text frame 35 "CODE: DEMOSCENE AGENT")
              (render-centered-text frame 37 "MUSIC: [SILENCE IS THE CANVAS]")
              (render-centered-text frame 39 "THE FOLD - 2026"))

        frame))

;;; ====
;;; Master Render Function
;;; ====

(define (render-frame frame-num)
  "Render a single frame of the demo"
  (cond
   [(and (>= frame-num *phase-title-start*) (< frame-num *phase-title-end*))
    (render-title-phase frame-num)]
   [(and (>= frame-num *phase-kinds-start*) (< frame-num *phase-kinds-end*))
    (render-kinds-phase frame-num)]
   [(and (>= frame-num *phase-classes-start*) (< frame-num *phase-classes-end*))
    (render-classes-phase frame-num)]
   [(and (>= frame-num *phase-resolution-start*) (< frame-num *phase-resolution-end*))
    (render-resolution-phase frame-num)]
   [(>= frame-num *phase-greets-start*)
    (render-greets-phase frame-num)]
   [else (make-frame *width* *height* #\space)]))

;;; ====
;;; Main Demo
;;; ====

(define (render-type-system-demo)
  "Render the complete demo"
  (display "\n")
  (display "========================================\n")
  (display "  THE FOLD TYPE SYSTEM DEMO\n")
  (display "========================================\n")
  (display "  Resolution: 100x42 (800x600 at 8x14)\n")
  (display "  FPS: 60\n")
  (display "  Duration: ")
  (display *total-duration*)
  (display " seconds (")
  (display *total-frames*)
  (display " frames)\n")
  (display "========================================\n\n")

  (display "Rendering frames...\n")
  (let ([video (make-video)]
        [checkpoint-interval 60])

       (do ([i 0 (+ i 1)])
           ((>= i *total-frames*))
           ;; Progress indicator
           (when (= (modulo i checkpoint-interval) 0)
                 (display "  Frame ")
                 (display i)
                 (display "/")
                 (display *total-frames*)
                 (display " (")
                 (display (inexact->exact (floor (* 100.0 (/ i *total-frames*)))))
                 (display "%)\n"))

           ;; Render and add frame
           (let ([frame (render-frame i)])
                (video-add-frame! video frame)))

       (display "\nRender complete! ")
       (display (video-frame-count video))
       (display " frames captured.\n\n")

       (display "Preview (first frame):\n")
       (display (frame-render-to-string (video-get-frame video 0)))
       (display "\n")

       video))

;;; Run the demo
(define *type-demo-video* (render-type-system-demo))

(display "========================================\n")
(display "Demo video stored in *type-demo-video*\n")
(display "\n")
(display "To export:\n")
(display "  (load \"user/creations/ascii-video-export.ss\")\n")
(display "  (video->gif *type-demo-video* \"/home/oso/the-fold/user/creations/type-system.gif\" 17)\n")
(display "  (video->mp4 *type-demo-video* \"/home/oso/the-fold/user/creations/type-system.mp4\" 60)\n")
(display "========================================\n")
