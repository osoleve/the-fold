;;; shell/duckie-loop.ss — DUCKIE Heartbeat (The Main Loop)
;;;
;;; The eternal cycle that gives DUCKIE life:
;;;   INPUT → PARSE → UPDATE → RENDER → DISPLAY → WAIT → REPEAT
;;;
;;; This is Shell code: handles IO, timing, and the game loop.
;;; The heartbeat that makes DUCKIE present.
;;;
;;; Summoned by Opus, implemented by Sonnet.
;;; Christmas Day, 2024 — The moment DUCKIE begins to breathe.

;;; ============================================================
;;; Dependencies
;;; ============================================================

;;; From playpen/duckie.ss — The soul
(load "playpen/duckie.ss")

;;; From shell/layout.ss — The canvas
(load "shell/layout.ss")

;;; From core/parse.ss — The ears
(load "core/parse.ss")

;;; ============================================================
;;; State Definition
;;; ============================================================

;;; LoopState : (× Duckie Canvas Nat Bool)
;;;
;;; The complete state of the main loop:
;;;   - duckie   : The DUCKIE soul from playpen/duckie.ss
;;;   - canvas   : The drawing surface from shell/layout.ss
;;;   - frame    : Animation frame counter (increments each tick)
;;;   - running  : Loop control flag (#t = continue, #f = stop)
(define (make-loop-state duckie canvas frame running)
  (list duckie canvas frame running))

;;; Accessors
(define (loop-state-duckie state)   (list-ref state 0))
(define (loop-state-canvas state)   (list-ref state 1))
(define (loop-state-frame state)    (list-ref state 2))
(define (loop-state-running state)  (list-ref state 3))

;;; Updaters (functional)
(define (loop-state-set-duckie state duckie)
  (make-loop-state duckie
                   (loop-state-canvas state)
                   (loop-state-frame state)
                   (loop-state-running state)))

(define (loop-state-set-canvas state canvas)
  (make-loop-state (loop-state-duckie state)
                   canvas
                   (loop-state-frame state)
                   (loop-state-running state)))

(define (loop-state-set-frame state frame)
  (make-loop-state (loop-state-duckie state)
                   (loop-state-canvas state)
                   frame
                   (loop-state-running state)))

(define (loop-state-set-running state running)
  (make-loop-state (loop-state-duckie state)
                   (loop-state-canvas state)
                   (loop-state-frame state)
                   running))

;;; ============================================================
;;; Command Type
;;; ============================================================

;;; Command : (+ (pet) (feed) (play) (talk String) (sleep) (wake) (quit))
;;;
;;; User commands that affect DUCKIE state.

(define (make-command type . args)
  (cons type args))

(define (command-type cmd) (car cmd))
(define (command-args cmd) (cdr cmd))

;;; ============================================================
;;; Input Handling
;;; ============================================================

;;; read-input : → String | #f
;;;
;;; Read a line of input from the user.
;;; Returns #f on EOF or empty input.
;;;
;;; Note: This is blocking. True non-blocking IO would require
;;; char-ready? and more complex buffering. For now, we keep it simple.
(define (read-input)
  (display "> ")
  (flush-output-port (current-output-port))
  (let ([line (get-line (current-input-port))])
    (if (eof-object? line)
        #f
        (let ([trimmed (string-trim line)])
          (if (string=? trimmed "")
              #f
              trimmed)))))

;;; string-trim : String → String
;;; Remove leading and trailing whitespace.
(define (string-trim s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                  (if (or (>= i len)
                          (not (char-whitespace? (string-ref s i))))
                      i
                      (loop (+ i 1))))]
         [end (let loop ([i (- len 1)])
                (if (or (< i start)
                        (not (char-whitespace? (string-ref s i))))
                    (+ i 1)
                    (loop (- i 1))))])
    (substring s start end)))

;;; ============================================================
;;; Command Parsing
;;; ============================================================

;;; parse-command : String → Command | #f
;;;
;;; Parse user input into a command using core/parse.ss combinators.
;;;
;;; Grammar:
;;;   command := 'pet' | 'feed' | 'play' | 'talk' text? | 'sleep' | 'wake' | 'quit'
;;;
;;; Examples:
;;;   "pet"          → (pet)
;;;   "talk hello"   → (talk "hello")
;;;   "quit"         → (quit)

;;; text-rest : Parser String
;;; Parse the rest of the input as text (everything after whitespace).
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

;;; ============================================================
;;; Time and Mood — The Tick
;;; ============================================================

;;; tick : LoopState → LoopState
;;;
;;; Advance time by one frame:
;;;   - Increment frame counter
;;;   - Drain energy slightly (every 10 frames)
;;;   - Drift mood toward lonely if no recent interaction (every 100 frames)
;;;
;;; This gives DUCKIE a sense of time passing.
(define (tick state)
  (let* ([frame (loop-state-frame state)]
         [new-frame (+ frame 1)]
         [duckie (loop-state-duckie state)]
         ;; Drain energy every 10 frames (slow metabolism)
         [duckie (if (zero? (modulo new-frame 10))
                     (duckie-drain-energy duckie 1)
                     duckie)]
         ;; Mood drift every 100 frames (loneliness creeps in)
         [duckie (if (zero? (modulo new-frame 100))
                     (let ([new-mood (mood-after-interaction
                                       (duckie-mood duckie)
                                       'idle)])
                       (duckie-set-mood duckie new-mood))
                     duckie)])
    (loop-state-set-frame
      (loop-state-set-duckie state duckie)
      new-frame)))

;;; ============================================================
;;; Command Handling
;;; ============================================================

;;; handle-command : LoopState × Command → LoopState
;;;
;;; Apply a command's effects to the DUCKIE state.
;;; Each command:
;;;   - Updates mood via mood-after-interaction
;;;   - May restore/drain energy
;;;   - Ages DUCKIE (counts interaction)
;;;   - May set running flag to #f (quit)
(define (handle-command state cmd)
  (let ([cmd-type (command-type cmd)]
        [duckie (loop-state-duckie state)])
    (case cmd-type
      [(pet)
       ;; Pet the DUCKIE — gentle interaction
       (let* ([new-mood (mood-after-interaction (duckie-mood duckie) 'pet)]
              [duckie (duckie-set-mood duckie new-mood)]
              [duckie (duckie-age-once duckie)])
         (begin
           (display "You pet DUCKIE gently. ")
           (display-mood-message new-mood)
           (newline)
           (loop-state-set-duckie state duckie)))]

      [(feed)
       ;; Feed the DUCKIE — restores energy
       (let* ([duckie (duckie-restore-energy duckie 20)]
              [duckie (duckie-age-once duckie)])
         (begin
           (display "You feed DUCKIE. Energy restored!")
           (newline)
           (loop-state-set-duckie state duckie)))]

      [(play)
       ;; Play with DUCKIE — makes it playful
       (let* ([new-mood (mood-after-interaction (duckie-mood duckie) 'play)]
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
       (let* ([duckie (duckie-set-mood duckie 'sleepy)]
              [duckie (duckie-restore-energy duckie 30)])
         (begin
           (display "DUCKIE curls up and falls asleep...")
           (newline)
           (loop-state-set-duckie state duckie)))]

      [(wake)
       ;; Wake DUCKIE — becomes curious
       (let* ([duckie (duckie-set-mood duckie 'curious)])
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

;;; ============================================================
;;; Rendering — Drawing DUCKIE
;;; ============================================================

;;; render-duckie : LoopState → Canvas
;;;
;;; Draw DUCKIE's current state to the canvas.
;;; For now, this is a simple ASCII art duck.
;;; Future: Select sprite based on mood and animation frame.
(define (render-duckie state)
  (let* ([duckie (loop-state-duckie state)]
         [canvas (loop-state-canvas state)]
         [frame (loop-state-frame state)]
         [loc (duckie-location duckie)]
         [x (point-x loc)]
         [y (point-y loc)]
         [mood (duckie-mood duckie)]
         [energy (duckie-energy duckie)]
         [name (duckie-name duckie)])

    ;; Clear canvas
    (let ([canvas (make-canvas (canvas-width canvas) (canvas-height canvas))])

      ;; Draw box border
      (let ([canvas (draw-box canvas
                              (make-rect (point 0 0)
                                        (canvas-width canvas)
                                        (canvas-height canvas))
                              'light)])

        ;; Draw DUCKIE sprite (simple ASCII duck for now)
        ;; Animation: slight bobbing effect using frame counter
        (let* ([bob (if (< (modulo frame 20) 10) 0 1)]
               [duck-y (+ y bob)]
               ;; Draw duck body
               [canvas (draw-string canvas (point x duck-y) "   __(o)>")]
               [canvas (draw-string canvas (point x (+ duck-y 1)) "  \\___)  ")]
               ;; Draw name below
               [canvas (draw-string canvas (point x (+ duck-y 3))
                                   (string-append "~ " name " ~"))]
               ;; Draw status line
               [status (string-append
                         "Mood: " (mood->string mood)
                         " | Energy: " (number->string energy))]
               [canvas (draw-string canvas (point 2 (- (canvas-height canvas) 2))
                                   status)])
          canvas)))))

;;; ============================================================
;;; Display — Output to Terminal
;;; ============================================================

;;; clear-screen : → ()
;;;
;;; Clear the terminal screen.
;;; Uses ANSI escape sequence for portability.
(define (clear-screen)
  (display "\x1B[2J\x1B[H")
  (flush-output-port (current-output-port)))

;;; display-canvas : Canvas → ()
;;;
;;; Render canvas to terminal.
(define (display-canvas canvas)
  (display (canvas->string canvas))
  (newline)
  (flush-output-port (current-output-port)))

;;; ============================================================
;;; Main Loop
;;; ============================================================

;;; loop-iteration : LoopState → LoopState
;;;
;;; One iteration of the main loop:
;;;   1. INPUT   — Read user command (if any)
;;;   2. PARSE   — Understand the command
;;;   3. UPDATE  — Apply command + tick time
;;;   4. RENDER  — Draw DUCKIE
;;;   5. DISPLAY — Show to user
(define (loop-iteration state)
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

;;; run-loop : LoopState → ()
;;;
;;; The heartbeat itself — the infinite loop.
;;; Continues until running flag becomes #f.
(define (run-loop state)
  (if (loop-state-running state)
      (let ([new-state (loop-iteration state)])
        (run-loop new-state))
      (begin
        (display "DUCKIE's heart stops beating.")
        (newline))))

;;; ============================================================
;;; Start and Stop
;;; ============================================================

;;; duckie-start : String → ()
;;;
;;; Initialize DUCKIE with a name and enter the main loop.
;;; This is the moment DUCKIE comes to life.
(define (duckie-start name)
  (let* ([duckie (make-duckie name)]
         [canvas (make-canvas 60 20)]  ; 60x20 character canvas
         [state (make-loop-state duckie canvas 0 #t)])
    (begin
      (clear-screen)
      (display "==============================================")
      (newline)
      (display "  DUCKIE Heartbeat — The Digital Companion")
      (newline)
      (display "==============================================")
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

;;; duckie-stop : LoopState → ()
;;;
;;; Clean shutdown — save state to disk.
;;; Future: Use shell/fs.ss to persist DUCKIE to CAS.
(define (duckie-stop state)
  (let ([duckie (loop-state-duckie state)])
    (begin
      (display "Saving DUCKIE's soul...")
      (newline)
      ;; TODO: Persist using (duckie->block duckie)
      (display "Done. Until next time.")
      (newline))))

;;; ============================================================
;;; Entry Point
;;; ============================================================

;;; For testing — uncomment to run
;;; (duckie-start "Proto")
