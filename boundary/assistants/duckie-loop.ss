;;; Summoned by Opus, implemented by Sonnet.
;;; Christmas Day, 2024 — The moment DUCKIE begins to breathe.

;;; From playpen/duckie.ss — The soul
(load "user/duckie.ss")

;;; From playpen/environment.ss — The world
(load "user/environment.ss")

;;; From boundary/ui/color.ss — The palette
(load "boundary/ui/color.ss")

;;; From boundary/ui/layout-color.ss — The colored canvas
(load "boundary/ui/layout-color.ss")

;;; From boundary/ui/layers.ss — The depth system
(load "boundary/ui/layers.ss")

;;; From boundary/ui/animation.ss — The motion
(load "boundary/ui/animation.ss")

;;; From boundary/ui/particles.ss — The sparkle
(load "boundary/ui/particles.ss")

;;; From core/parse.ss — The ears
(load "core/lang/parse.ss")

;;; From boundary/assistants/duckie-persist.ss — The memory
(load "boundary/assistants/duckie-persist.ss")

;;; From core/prelude.ss — The string utilities
;;; NOTE: string-trim provided by core/prelude.ss
(load "core/base/prelude.ss")

(doc 'module 'boundary/assistants/duckie-loop)
(doc 'description "DUCKIE Heartbeat - The main loop giving DUCKIE life: INPUT → PARSE → UPDATE → RENDER → DISPLAY → WAIT → REPEAT")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Shell code handling IO, timing, and game loop - the heartbeat that makes DUCKIE present")

(doc 'section 'state-definition)

(define (make-loop-state duckie canvas frame running prev-mood transition-start particles)
  (doc 'type (-> Duckie Canvas Nat Bool Mood Nat ParticleSystem LoopState))
  (doc 'description "Create complete loop state with DUCKIE, canvas, animation frame, running flag, mood transitions, and particles")
  (list duckie canvas frame running prev-mood transition-start particles))

(doc 'section 'accessors)

(define (loop-state-duckie state)           (list-ref state 0))
(define (loop-state-canvas state)           (list-ref state 1))
(define (loop-state-frame state)            (list-ref state 2))
(define (loop-state-running state)          (list-ref state 3))
(define (loop-state-prev-mood state)        (list-ref state 4))
(define (loop-state-transition-start state) (list-ref state 5))
(define (loop-state-particles state)        (list-ref state 6))

(doc 'section 'updaters-functional)
(define (loop-state-set-duckie state duckie)
  (make-loop-state duckie
                   (loop-state-canvas state)
                   (loop-state-frame state)
                   (loop-state-running state)
                   (loop-state-prev-mood state)
                   (loop-state-transition-start state)
                   (loop-state-particles state)))

(define (loop-state-set-canvas state canvas)
  (make-loop-state (loop-state-duckie state)
                   canvas
                   (loop-state-frame state)
                   (loop-state-running state)
                   (loop-state-prev-mood state)
                   (loop-state-transition-start state)
                   (loop-state-particles state)))

(define (loop-state-set-frame state frame)
  (make-loop-state (loop-state-duckie state)
                   (loop-state-canvas state)
                   frame
                   (loop-state-running state)
                   (loop-state-prev-mood state)
                   (loop-state-transition-start state)
                   (loop-state-particles state)))

(define (loop-state-set-running state running)
  (make-loop-state (loop-state-duckie state)
                   (loop-state-canvas state)
                   (loop-state-frame state)
                   running
                   (loop-state-prev-mood state)
                   (loop-state-transition-start state)
                   (loop-state-particles state)))

(define (loop-state-set-particles state particles)
  (make-loop-state (loop-state-duckie state)
                   (loop-state-canvas state)
                   (loop-state-frame state)
                   (loop-state-running state)
                   (loop-state-prev-mood state)
                   (loop-state-transition-start state)
                   particles))

(define (emit-particles-at-duckie state interaction)
  (doc 'type (-> LoopState Symbol LoopState))
  (doc 'description "Emit particles at DUCKIE's location based on interaction type")
  (let* ([duckie (loop-state-duckie state)]
         [pos (duckie-location duckie)]
         [new-particles (emit-by-interaction pos interaction)]
         [particles (loop-state-particles state)]
         [updated-particles (add-particles particles new-particles)])
        (loop-state-set-particles state updated-particles)))

(define (begin-mood-transition state new-mood)
  (doc 'type (-> LoopState Mood LoopState))
  (doc 'description "Start mood transition by recording previous mood and transition start frame")
  (let ([old-mood (duckie-mood (loop-state-duckie state))]
        [frame (loop-state-frame state)])
       (if (eq? old-mood new-mood)
           state  ; No transition needed
           (make-loop-state (loop-state-duckie state)
                            (loop-state-canvas state)
                            frame
                            (loop-state-running state)
                            old-mood      ; Store previous mood
                            frame
                            (loop-state-particles state)))))     ; Record transition start

(doc 'section 'command-type)

(define (make-command type . args)
  (cons type args))

(define (command-type cmd) (car cmd))
(define (command-args cmd) (cdr cmd))

(doc 'section 'input-handling)

(define (read-input)
  (doc 'type (-> (Maybe String)))
  (doc 'description "Read a line of input from user - returns #f on EOF or empty input")
  (doc 'note "Blocking IO - true non-blocking would require char-ready? and complex buffering")
  (display "> ")
  (flush-output-port (current-output-port))
  (let ([line (get-line (current-input-port))])
       (if (eof-object? line)
           #f
           (let ([trimmed (string-trim line)])
                (if (string=? trimmed "")
                    #f
                    trimmed)))))

(doc 'section 'command-parsing)
(define text-rest
  (lambda (input)
          (let ([trimmed (string-trim input)])
               (success trimmed ""))))

;;; command-parser : Parser Command
(define command-parser
  (choice
   (list
    ;; pet
    (map-p (lambda (_) (make-command 'pet))
           (string-p "pet"))
    ;; feed
    (map-p (lambda (_) (make-command 'feed))
           (string-p "feed"))
    ;; play
    (map-p (lambda (_) (make-command 'play))
           (string-p "play"))
    ;; sleep
    (map-p (lambda (_) (make-command 'sleep))
           (string-p "sleep"))
    ;; wake
    (map-p (lambda (_) (make-command 'wake))
           (string-p "wake"))
    ;; quit
    (map-p (lambda (_) (make-command 'quit))
           (string-p "quit"))
    ;; talk [text]
    (bind (string-p "talk") (lambda (_)
                                    (bind (optional spaces1) (lambda (_)
                                                                     (bind text-rest (lambda (text)
                                                                                             (pure (make-command 'talk text)))))))))))

(define (parse-command input)
  (let ([result (run-parser (lexeme command-parser) input)])
       (if (parse-ok? result)
           (result-value result)
           #f)))

(doc 'section 'time-and-mood-the-tick)

(define (tick state)
  (doc 'type (-> LoopState LoopState))
  (doc 'description "Advance time by one frame - drains energy, drifts mood, updates particles")
  (doc 'note "Gives DUCKIE a sense of time passing")
  (let* ([frame (loop-state-frame state)]
         [new-frame (+ frame 1)]
         [duckie (loop-state-duckie state)]
         ;; Drain energy every 10 frames (slow metabolism)
         [duckie (if (zero? (modulo new-frame 10))
                     (duckie-drain-energy duckie 1)
                     duckie)]
         ;; Mood drift every 100 frames (loneliness creeps in)
         [result (if (zero? (modulo new-frame 100))
                     (let ([new-mood (mood-after-interaction
                                      (duckie-mood duckie)
                                      'idle)])
                          (if (not (eq? new-mood (duckie-mood duckie)))
                              ;; Mood changed — begin transition
                              (let* ([state (begin-mood-transition state new-mood)]
                                     [duckie (duckie-set-mood duckie new-mood)])
                                    (list state duckie))
                              ;; No mood change
                              (list state duckie)))
                     ;; No mood drift this frame
                     (list state duckie))]
         [state (car result)]
         [duckie (cadr result)]
         ;; Update particle system
         [particles (update-particle-system (loop-state-particles state))]
         [state (loop-state-set-particles state particles)])
        (loop-state-set-frame
         (loop-state-set-duckie state duckie)
         new-frame)))

(doc 'section 'command-handling)

(define (handle-command state cmd)
  (doc 'type (-> LoopState Command LoopState))
  (doc 'description "Apply command effects to DUCKIE state - updates mood, energy, age, and running flag")
  (let ([cmd-type (command-type cmd)]
        [duckie (loop-state-duckie state)])
       (case cmd-type
             [(pet)
              ;; Pet the DUCKIE — gentle interaction
              (let* ([new-mood (mood-after-interaction (duckie-mood duckie) 'pet)]
                     [state (begin-mood-transition state new-mood)]
                     [state (emit-particles-at-duckie state 'pet)]  ; Hearts!
                     [duckie (duckie-set-mood duckie new-mood)]
                     [duckie (duckie-age-once duckie)])
                    (begin
                     (display "You pet DUCKIE gently. ")
                     (display-mood-message new-mood)
                     (newline)
                     (loop-state-set-duckie state duckie)))]
             
             [(feed)
              ;; Feed the DUCKIE — restores energy
              (let* ([state (emit-particles-at-duckie state 'feed)]  ; Sparkles!
                     [duckie (duckie-restore-energy duckie 20)]
                     [duckie (duckie-age-once duckie)])
                    (begin
                     (display "You feed DUCKIE. Energy restored!")
                     (newline)
                     (loop-state-set-duckie state duckie)))]
             
             [(play)
              ;; Play with DUCKIE — makes it playful
              (let* ([new-mood (mood-after-interaction (duckie-mood duckie) 'play)]
                     [state (begin-mood-transition state new-mood)]
                     [state (emit-particles-at-duckie state 'play)]  ; Music notes!
                     [duckie (duckie-set-mood duckie new-mood)]
                     [duckie (duckie-drain-energy duckie 5)]  ; Playing is tiring!
                     [duckie (duckie-age-once duckie)])
                    (begin
                     (display "You play with DUCKIE! ")
                     (display-mood-message new-mood)
                     (newline)
                     (loop-state-set-duckie state duckie)))]
             
             [(talk)
              ;; Talk to DUCKIE — curious interaction
              (let* ([text (if (null? (command-args cmd))
                               ""
                               (car (command-args cmd)))]
                     [new-mood (mood-after-interaction (duckie-mood duckie) 'talk)]
                     [state (begin-mood-transition state new-mood)]
                     [state (emit-particles-at-duckie state 'talk)]  ; Bubbles!
                     [duckie (duckie-set-mood duckie new-mood)]
                     [duckie (duckie-age-once duckie)])
                    (begin
                     (display "You say: \"")
                     (display text)
                     (display "\". ")
                     (display-mood-message new-mood)
                     (newline)
                     (loop-state-set-duckie state duckie)))]
             
             [(sleep)
              ;; DUCKIE goes to sleep — becomes sleepy, restores energy
              (let* ([state (begin-mood-transition state 'sleepy)]
                     [duckie (duckie-set-mood duckie 'sleepy)]
                     [duckie (duckie-restore-energy duckie 30)])
                    (begin
                     (display "DUCKIE curls up and falls asleep...")
                     (newline)
                     (loop-state-set-duckie state duckie)))]
             
             [(wake)
              ;; Wake DUCKIE — becomes curious
              (let* ([state (begin-mood-transition state 'curious)]
                     [duckie (duckie-set-mood duckie 'curious)])
                    (begin
                     (display "DUCKIE wakes up and looks around curiously.")
                     (newline)
                     (loop-state-set-duckie state duckie)))]
             
             [(quit)
              ;; Exit the loop
              (begin
               (display "Goodbye! DUCKIE will miss you.")
               (newline)
               (loop-state-set-running state #f))]
             
             [else
              ;; Unknown command — no change
              (begin
               (display "DUCKIE tilts its head, confused.")
               (newline)
               state)])))

;;; display-mood-message : Mood → ()
;;; Print a flavor message based on mood.
(define (display-mood-message mood)
  (case mood
        [(happy)    (display "DUCKIE bounces happily!")]
        [(curious)  (display "DUCKIE watches you with interest.")]
        [(sleepy)   (display "DUCKIE's eyes are drooping...")]
        [(content)  (display "DUCKIE seems peaceful.")]
        [(lonely)   (display "DUCKIE looks around, waiting...")]
        [(playful)  (display "DUCKIE is full of energy!")]
        [else       (display "DUCKIE exists.")]))

(doc 'section 'rendering-drawing-duckie)

(define (get-current-mood-color state)
  (doc 'type (-> LoopState Color))
  (doc 'description "Get current mood color with smooth 30-frame transitions using easing")
  (let* ([current-mood (duckie-mood (loop-state-duckie state))]
         [prev-mood (loop-state-prev-mood state)]
         [transition-start (loop-state-transition-start state)]
         [current-frame (loop-state-frame state)]
         [transition-duration 30]  ; 30 frames = smooth transition
         [frames-since-transition (- current-frame transition-start)])
        (if (or (>= frames-since-transition transition-duration)
                (eq? prev-mood current-mood))
            ;; Transition complete or no transition — use current mood color
            (mood->color current-mood)
            ;; In transition — interpolate between colors
            (let* ([t (frame->t frames-since-transition transition-duration)]
                   [prev-color (mood->color prev-mood)]
                   [current-color (mood->color current-mood)])
                  (animate-color smooth t prev-color current-color)))))

;;; draw-energy-bar : Canvas × Nat × Color → Canvas
;;;
;;; Draw a visual energy bar showing DUCKIE's energy level (0-100).
;;; The bar uses color to show energy state (gradient from dark to bright).
(define (draw-energy-bar canvas energy energy-color)
  (let* ([bar-width 30]
         [bar-x 2]
         [bar-y 1]
         [filled-width (quotient (* energy bar-width) 100)]
         ;; Draw bar label
         [canvas (draw-string-colored canvas
                                      (point bar-x bar-y)
                                      "Energy: ["
                                      color-white
                                      color-default)]
         ;; Draw filled portion
         [canvas (let loop ([i 0] [c canvas])
                      (if (>= i filled-width)
                          c
                          (loop (+ i 1)
                                (draw-char-colored c
                                                   (point (+ bar-x 9 i) bar-y)
                                                   #\=
                                                   energy-color
                                                   color-default))))]
         ;; Draw empty portion
         [canvas (let loop ([i filled-width] [c canvas])
                      (if (>= i bar-width)
                          c
                          (loop (+ i 1)
                                (draw-char-colored c
                                                   (point (+ bar-x 9 i) bar-y)
                                                   #\-
                                                   color-gray
                                                   color-default))))]
         ;; Draw closing bracket
         [canvas (draw-string-colored canvas
                                      (point (+ bar-x 9 bar-width) bar-y)
                                      "]"
                                      color-white
                                      color-default)])
        canvas))

(define (render-duckie state)
  (doc 'type (-> LoopState Canvas))
  (doc 'description "Draw DUCKIE using layered rendering - background, sprite, particles, UI - color expresses emotional state")
  (let* ([duckie (loop-state-duckie state)]
         [canvas (loop-state-canvas state)]
         [frame (loop-state-frame state)]
         [loc (duckie-location duckie)]
         [x (point-x loc)]
         [y (point-y loc)]
         [mood (duckie-mood duckie)]
         [energy (duckie-energy duckie)]
         [name (duckie-name duckie)]
         [width (canvas-width canvas)]
         [height (canvas-height canvas)]
         ;; Get mood color with smooth transitions
         [mood-color (get-current-mood-color state)]
         [energy-color (energy->color energy)])
        
        ;; Layer 0: Background (pond environment with day/night cycle)
        (let* ([bg-layer (make-background-layer width height)]
               [env-canvas (render-animated-environment width height frame)]
               [bg-layer (layer-set-canvas bg-layer env-canvas)])
              
              ;; Layer 50: DUCKIE sprite
              (let* ([sprite-layer (make-sprite-layer 'duckie 10 6 loc)]
                     [bob (if (< (modulo frame 20) 10) 0 1)]
                     [duck-y bob]
                     ;; Draw DUCKIE sprite on sprite layer canvas
                     [sprite-canvas (make-transparent-canvas 10 6)]
                     [sprite-canvas (draw-string-colored sprite-canvas (point 0 duck-y)
                                                         "   __(o)>"
                                                         mood-color
                                                         color-default)]
                     [sprite-canvas (draw-string-colored sprite-canvas (point 0 (+ duck-y 1))
                                                         "  \\___)  "
                                                         mood-color
                                                         color-default)]
                     [sprite-canvas (draw-string-colored sprite-canvas (point 0 (+ duck-y 3))
                                                         (string-append "~ " name " ~")
                                                         mood-color
                                                         color-default)]
                     [sprite-layer (layer-set-canvas sprite-layer sprite-canvas)])
                    
                    ;; Layer 75: Particles (above sprites, below UI)
                    (let* ([particle-layer (layer 'particles 75 #t (point 0 0)
                                                  (make-transparent-canvas width height))]
                           [particles (loop-state-particles state)]
                           [particle-canvas (render-particle-system
                                             (make-transparent-canvas width height)
                                             particles)]
                           [particle-layer (layer-set-canvas particle-layer particle-canvas)])
                          
                          ;; Layer 100: UI (borders, energy bar, status)
                          (let* ([ui-layer (make-ui-layer width height)]
                                 [ui-canvas (make-transparent-canvas width height)]
                                 ;; Draw mood-colored box border
                                 [ui-canvas (draw-box-colored ui-canvas
                                                              (make-rect (point 0 0) width height)
                                                              'light
                                                              mood-color
                                                              color-default)]
                                 ;; Draw energy bar
                                 [ui-canvas (draw-energy-bar ui-canvas energy energy-color)]
                                 ;; Draw status line
                                 [status (string-append
                                          "Mood: " (mood->string mood)
                                          " | Energy: " (number->string energy))]
                                 [ui-canvas (draw-string-colored ui-canvas
                                                                 (point 2 (- height 2))
                                                                 status
                                                                 color-white
                                                                 color-default)]
                                 [ui-layer (layer-set-canvas ui-layer ui-canvas)])
                                
                                ;; Compose all layers into final canvas
                                (let* ([stack (make-layer-stack (list bg-layer sprite-layer particle-layer ui-layer))]
                                       [final-canvas (flatten-layers stack width height)])
                                      final-canvas)))))))

(doc 'section 'display-output-to-terminal)

(define (clear-screen)
  (doc 'type (-> Void))
  (doc 'description "Clear terminal screen using ANSI escape sequence")
  (display "\x1b;[2J\x1b;[H")
  (flush-output-port (current-output-port)))

;;; display-canvas : Canvas → ()
;;;
;;; Render canvas to terminal.
(define (display-canvas canvas)
  (display (canvas->string canvas))
  (newline)
  (flush-output-port (current-output-port)))

(doc 'section 'main-loop)

(define (loop-iteration state)
  (doc 'type (-> LoopState LoopState))
  (doc 'description "One iteration of main loop: INPUT → PARSE → UPDATE → RENDER → DISPLAY")
  ;; INPUT
  (let ([input (read-input)])
       ;; PARSE
       (let ([cmd (if input (parse-command input) #f)])
            ;; UPDATE
            (let* ([state (if cmd
                              (handle-command state cmd)
                              state)]
                   [state (tick state)])
                  ;; RENDER
                  (let ([canvas (render-duckie state)])
                       (let ([state (loop-state-set-canvas state canvas)])
                            ;; DISPLAY
                            (begin
                             (clear-screen)
                             (display-canvas canvas)
                             state)))))))

(define (run-loop state)
  (doc 'type (-> LoopState Void))
  (doc 'description "The heartbeat itself - infinite loop continuing until running flag becomes #f")
  (if (loop-state-running state)
      (let ([new-state (loop-iteration state)])
           (run-loop new-state))
      (begin
       (display "DUCKIE's heart stops beating.")
       (newline))))

(doc 'section 'start-and-stop)

(define (duckie-start name)
  (doc 'type (-> String Void))
  (doc 'description "Initialize DUCKIE with a name and enter the main loop - the moment DUCKIE comes to life")
  (let* ([duckie (make-duckie name)]
         [canvas (make-canvas 60 20)]  ; 60x20 character canvas
         [initial-mood (duckie-mood duckie)]
         [particles (make-particle-system)]  ; Start with no particles
         [state (make-loop-state duckie canvas 0 #t initial-mood 0 particles)])
        (begin
         (clear-screen)
         (display "====")
         (newline)
         (display "  DUCKIE Heartbeat — The Digital Companion")
         (newline)
         (display "====")
         (newline)
         (newline)
         (display "Welcome! Your DUCKIE is named: ")
         (display name)
         (newline)
         (newline)
         (display "Commands: pet, feed, play, talk [text], sleep, wake, quit")
         (newline)
         (display "Press Enter to begin...")
         (newline)
         (read-line)
         (run-loop state))))

(define (duckie-stop state)
  (doc 'type (-> LoopState Void))
  (doc 'description "Clean shutdown - save DUCKIE state to disk")
  (doc 'todo "Use boundary/io/fs.ss to persist DUCKIE to CAS")
  (let ([duckie (loop-state-duckie state)])
       (begin
        (display "Saving DUCKIE's soul...")
        (newline)
        ;; Persist DUCKIE to content-addressed store
        (let ([hash (save-duckie! duckie)])
             (display "Saved as: ")
             (display (bytevector->hex-string hash))
             (newline))
        (display "Done. Until next time.")
        (newline))))

(doc 'section 'entry-point)
