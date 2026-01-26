;;; user/demos/orbital-dance.ss --- N-body gravitational simulation in ASCII
;;;
;;; Watch celestial bodies dance through gravitational attraction.
;;; Trails show the history of their orbits, revealing chaotic beauty.
;;;
;;; Run with: scheme --script user/demos/orbital-dance.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")

;;; ============================================================
;;; Section: Physics Constants and Bodies
;;; ============================================================

(define *G* 50.0)           ; Gravitational constant (tweaked for visuals)
(define *dt* 0.02)          ; Time step
(define *softening* 2.0)    ; Prevent singularity at close approach
(define *trail-length* 80)  ; How many positions to remember

(define (make-body mass x y vx vy)
  (list 'body mass
        (vector x y)
        (vector vx vy)
        '()))  ; trail (list of positions)

(define (body-mass b) (list-ref b 1))
(define (body-pos b) (list-ref b 2))
(define (body-vel b) (list-ref b 3))
(define (body-trail b) (list-ref b 4))
(define (body-x b) (vector-ref (body-pos b) 0))
(define (body-y b) (vector-ref (body-pos b) 1))

(define (body-update b new-pos new-vel)
  (let* ([old-trail (body-trail b)]
         [new-trail (cons (body-pos b)
                          (if (> (length old-trail) *trail-length*)
                              (take *trail-length* old-trail)
                              old-trail))])
    (list 'body (body-mass b) new-pos new-vel new-trail)))

;; take provided by prelude

;;; ============================================================
;;; Section: Gravitational Physics
;;; ============================================================

;;; Calculate gravitational acceleration on body1 due to body2
(define (gravity-accel b1 b2)
  (let* ([pos1 (body-pos b1)]
         [pos2 (body-pos b2)]
         [delta (vec-sub pos2 pos1)]
         [dist-sq (+ (vec-dot delta delta) (* *softening* *softening*))]
         [dist (sqrt dist-sq)]
         [force-mag (/ (* *G* (body-mass b2)) dist-sq)]
         [unit-vec (vec-scale (/ 1.0 dist) delta)])
    (vec-scale force-mag unit-vec)))

;;; Total acceleration on a body from all others
(define (total-accel body bodies)
  (fold-left
   (lambda (acc other)
     (if (eq? body other)
         acc
         (vec-add acc (gravity-accel body other))))
   (vector 0.0 0.0)
   bodies))

;;; Velocity Verlet integration step
(define (integrate-bodies bodies)
  ;; Calculate accelerations at current positions
  (let* ([accels (map (lambda (b) (total-accel b bodies)) bodies)]
         ;; Update positions: x' = x + v*dt + 0.5*a*dt^2
         [new-positions
          (map (lambda (b a)
                 (vec-add (body-pos b)
                          (vec-add (vec-scale *dt* (body-vel b))
                                   (vec-scale (* 0.5 *dt* *dt*) a))))
               bodies accels)]
         ;; Create temporary bodies at new positions to get new accelerations
         [temp-bodies
          (map (lambda (b np)
                 (list 'body (body-mass b) np (body-vel b) '()))
               bodies new-positions)]
         [new-accels (map (lambda (tb) (total-accel tb temp-bodies)) temp-bodies)]
         ;; Update velocities: v' = v + 0.5*(a + a')*dt
         [new-velocities
          (map (lambda (b a na)
                 (vec-add (body-vel b)
                          (vec-scale (* 0.5 *dt*) (vec-add a na))))
               bodies accels new-accels)])
    ;; Build updated bodies with trails
    (map body-update bodies new-positions new-velocities)))

;;; ============================================================
;;; Section: ASCII Rendering
;;; ============================================================

(define *width* 100)
(define *height* 45)
(define *view-scale* 3.0)  ; World units per character

(define (world-to-screen x y)
  (let ([cx (/ *width* 2)]
        [cy (/ *height* 2)])
    (values (+ cx (/ x *view-scale*))
            (+ cy (/ y *view-scale*)))))

(define (make-frame-buffer)
  (make-vector (* *width* *height*) #\space))

(define (fb-set! fb x y char)
  (let ([xi (inexact->exact (round x))]
        [yi (inexact->exact (round y))])
    (when (and (>= xi 0) (< xi *width*)
               (>= yi 0) (< yi *height*))
      (vector-set! fb (+ xi (* yi *width*)) char))))

(define (fb-get fb x y)
  (let ([xi (inexact->exact (round x))]
        [yi (inexact->exact (round y))])
    (if (and (>= xi 0) (< xi *width*)
             (>= yi 0) (< yi *height*))
        (vector-ref fb (+ xi (* yi *width*)))
        #\space)))

;;; Draw a line for trails
(define (fb-line! fb x1 y1 x2 y2 char)
  (let* ([dx (- x2 x1)]
         [dy (- y2 y1)]
         [steps (max 1 (inexact->exact (ceiling (max (abs dx) (abs dy)))))])
    (do ([i 0 (+ i 1)])
        ((> i steps))
      (let ([t (/ i steps)])
        (fb-set! fb (+ x1 (* t dx)) (+ y1 (* t dy)) char)))))

;;; Trail characters: fade from bright to dim
(define *trail-chars* '#(#\* #\o #\. #\. #\. #\. #\. #\space))

(define (trail-char-at age)
  (let ([idx (min (- (vector-length *trail-chars*) 1)
                  (inexact->exact (floor (/ age 12))))])
    (vector-ref *trail-chars* idx)))

;;; Body display based on mass
(define (body-char mass)
  (cond
    [(> mass 100) #\@]   ; Star/massive
    [(> mass 20) #\O]    ; Large planet
    [(> mass 5) #\o]     ; Medium
    [else #\*]))         ; Small

;;; ANSI colors
(define (ansi-fg code) (string-append "\x1b;[38;5;" (number->string code) "m"))
(define (ansi-reset) "\x1b;[0m")

(define *body-colors* '#(226 208 51 46 201 196 226 51)) ; yellow, orange, cyan, green...

(define (render-frame bodies frame-num)
  (let ([fb (make-frame-buffer)]
        [colors (make-vector (* *width* *height*) 0)])
    ;; Draw trails first (oldest = faintest)
    (let body-loop ([bs bodies] [bi 0])
      (when (pair? bs)
        (let* ([b (car bs)]
               [trail (body-trail b)]
               [color (vector-ref *body-colors* (mod bi 8))])
          (let trail-loop ([t trail] [age 0] [prev #f])
            (when (pair? t)
              (let-values ([(sx sy) (world-to-screen
                                     (vector-ref (car t) 0)
                                     (vector-ref (car t) 1))])
                (when prev
                  (fb-line! fb (car prev) (cdr prev) sx sy (trail-char-at age)))
                (let ([xi (inexact->exact (round sx))]
                      [yi (inexact->exact (round sy))])
                  (when (and (>= xi 0) (< xi *width*) (>= yi 0) (< yi *height*))
                    (let ([idx (+ xi (* yi *width*))])
                      (when (< age 40)
                        (vector-set! colors idx color)))))
                (trail-loop (cdr t) (+ age 1) (cons sx sy))))))
        (body-loop (cdr bs) (+ bi 1))))
    ;; Draw bodies on top
    (let body-loop ([bs bodies] [bi 0])
      (when (pair? bs)
        (let* ([b (car bs)]
               [color (vector-ref *body-colors* (mod bi 8))])
          (let-values ([(sx sy) (world-to-screen (body-x b) (body-y b))])
            (fb-set! fb sx sy (body-char (body-mass b)))
            (let ([xi (inexact->exact (round sx))]
                  [yi (inexact->exact (round sy))])
              (when (and (>= xi 0) (< xi *width*) (>= yi 0) (< yi *height*))
                (vector-set! colors (+ xi (* yi *width*)) color)))))
        (body-loop (cdr bs) (+ bi 1))))
    ;; Build colored string
    (let ([result '()]
          [last-color -1])
      (do ([y 0 (+ y 1)])
          ((>= y *height*))
        (do ([x 0 (+ x 1)])
            ((>= x *width*))
          (let* ([idx (+ x (* y *width*))]
                 [ch (vector-ref fb idx)]
                 [col (vector-ref colors idx)])
            (cond
              [(and (> col 0) (not (= col last-color)))
               (set! result (cons (ansi-fg col) result))
               (set! last-color col)]
              [(and (= col 0) (not (= last-color -1)) (not (eq? ch #\space)))
               (set! result (cons (ansi-fg 240) result))
               (set! last-color -1)])
            (set! result (cons (string ch) result))))
        (set! result (cons "\n" result)))
      (string-append (apply string-append (reverse result)) (ansi-reset)))))

;;; ============================================================
;;; Section: Interesting Initial Conditions
;;; ============================================================

;;; Figure-8 three-body (approximately stable)
(define (figure-8-system)
  (let ([v 0.93240737])
    (list
     (make-body 10 -0.97000436 0.24308753 (* v 0.4662036850) (* v 0.4323657300))
     (make-body 10 0.0 0.0 (* v -0.93240737) (* v -0.86473146))
     (make-body 10 0.97000436 -0.24308753 (* v 0.4662036850) (* v 0.4323657300)))))

;;; Solar system-ish: central star with orbiting planets
(define (solar-system)
  (list
   (make-body 200.0 0.0 0.0 0.0 0.0)          ; Sun
   (make-body 1.0 30.0 0.0 0.0 8.0)           ; Inner planet
   (make-body 3.0 50.0 0.0 0.0 6.0)           ; Middle planet
   (make-body 2.0 70.0 0.0 0.0 5.0)           ; Outer planet
   (make-body 0.5 90.0 0.0 0.0 4.2)))         ; Far planet

;;; Binary star with planet
(define (binary-system)
  (list
   (make-body 100.0 -20.0 0.0 0.0 3.0)        ; Star A
   (make-body 100.0 20.0 0.0 0.0 -3.0)        ; Star B
   (make-body 1.0 0.0 60.0 -4.5 0.0)))        ; Planet

;;; Chaotic 4-body
(define (chaotic-quartet)
  (list
   (make-body 30.0 -40.0 0.0 0.0 3.0)
   (make-body 30.0 40.0 0.0 0.0 -3.0)
   (make-body 30.0 0.0 -40.0 3.0 0.0)
   (make-body 30.0 0.0 40.0 -3.0 0.0)))

;;; Galaxy collision (two clumps approaching)
(define (galaxy-collision)
  (append
   ;; Cluster 1
   (list
    (make-body 80.0 -60.0 0.0 1.5 0.5)
    (make-body 5.0 -75.0 10.0 1.5 2.5)
    (make-body 5.0 -75.0 -10.0 1.5 -1.5)
    (make-body 5.0 -50.0 15.0 1.5 1.0))
   ;; Cluster 2
   (list
    (make-body 80.0 60.0 0.0 -1.5 -0.5)
    (make-body 5.0 75.0 -10.0 -1.5 -2.5)
    (make-body 5.0 75.0 10.0 -1.5 1.5)
    (make-body 5.0 50.0 -15.0 -1.5 -1.0))))

;;; ============================================================
;;; Section: Animation Loop
;;; ============================================================

(define (simulate-frames bodies n-frames skip-frames)
  (let loop ([bs bodies] [frame 0] [skip 0] [frames '()])
    (cond
      [(>= (length frames) n-frames) (reverse frames)]
      [(> skip 0)
       (loop (integrate-bodies bs) frame (- skip 1) frames)]
      [else
       (let ([fr (render-frame bs frame)])
         (loop (integrate-bodies bs) (+ frame 1) skip-frames (cons fr frames)))])))

(define (play-animation frames delay-ms loops)
  (define (delay-busy ms)
    ;; Busy wait using real-time clock
    (let* ([start (current-time 'time-monotonic)]
           [start-ns (+ (* (time-second start) 1000000000)
                        (time-nanosecond start))]
           [end-ns (+ start-ns (* ms 1000000))])
      (let spin ()
        (let* ([now (current-time 'time-monotonic)]
               [now-ns (+ (* (time-second now) 1000000000)
                          (time-nanosecond now))])
          (when (< now-ns end-ns) (spin))))))
  (do ([i 0 (+ i 1)])
      ((>= i loops))
    (for-each
     (lambda (frame)
       (display "\x1b;[2J\x1b;[H")  ; Clear screen
       (display frame)
       (flush-output-port (current-output-port))
       (delay-busy delay-ms))
     frames)))

;;; ============================================================
;;; Section: Frame Export (for GIF generation)
;;; ============================================================

(define (export-frames bodies n-frames skip-frames output-dir)
  (display (format "Generating ~a frames...\n" n-frames))
  (flush-output-port (current-output-port))
  (let loop ([bs bodies] [frame 0] [skip 0] [exported 0])
    (cond
      [(>= exported n-frames)
       (display (format "Exported ~a frames to ~a\n" exported output-dir))]
      [(> skip 0)
       (loop (integrate-bodies bs) frame (- skip 1) exported)]
      [else
       (let* ([fr (render-frame bs frame)]
              [filename (format "~a/frame-~a.txt" output-dir
                               (let ([s (number->string exported)])
                                 (string-append (make-string (max 0 (- 3 (string-length s))) #\0) s)))])
         (call-with-output-file filename
           (lambda (port)
             (display fr port)))
         (when (= (mod exported 10) 0)
           (display (format "  Frame ~a...\n" exported))
           (flush-output-port (current-output-port)))
         (loop (integrate-bodies bs) (+ frame 1) skip-frames (+ exported 1)))])))

;;; ============================================================
;;; Section: Main
;;; ============================================================

(define (run-interactive system-fn name)
  (display "\x1b;[2J\x1b;[H")
  (display "========================================\n")
  (display "      ORBITAL DANCE\n")
  (display "   N-Body Gravitational Simulation\n")
  (display "========================================\n\n")
  (display (format "Generating ~a simulation...\n" name))
  (display "(60 frames, this takes a moment)\n\n")
  (flush-output-port (current-output-port))

  (let* ([bodies (system-fn)]
         [frames (simulate-frames bodies 60 3)])
    (display "Playing animation (Ctrl+C to stop)...\n")
    (display "Press Enter to start: ")
    (flush-output-port (current-output-port))
    (read-char)
    (play-animation frames 80 10)
    (display "\x1b;[2J\x1b;[H")
    (display "Animation complete!\n")))

(define (run-export system-fn name output-dir n-frames)
  (display (format "Exporting ~a simulation (~a frames)...\n" name n-frames))
  (export-frames (system-fn) n-frames 3 output-dir))

(define (main)
  (display "\x1b;[2J\x1b;[H")
  (display "========================================\n")
  (display "      ORBITAL DANCE\n")
  (display "   N-Body Gravitational Simulation\n")
  (display "========================================\n\n")
  (display "Systems available:\n")
  (display "  1. Solar System (star + planets)\n")
  (display "  2. Binary Stars (two suns + planet)\n")
  (display "  3. Chaotic Quartet (4-body chaos)\n")
  (display "  4. Galaxy Collision (two clusters)\n")
  (display "  5. Figure-8 (stable 3-body)\n")
  (display "\nGenerating Solar System simulation...\n")
  (display "(60 frames, this takes a moment)\n\n")
  (flush-output-port (current-output-port))

  (let* ([bodies (solar-system)]
         [frames (simulate-frames bodies 60 3)])
    (display "Playing animation (Ctrl+C to stop)...\n")
    (display "Press Enter to start: ")
    (flush-output-port (current-output-port))
    (read-char)
    (play-animation frames 80 10)
    (display "\x1b;[2J\x1b;[H")
    (display "Animation complete!\n")))

;; To run interactively: (main)
;; To export frames: (export-frames (chaotic-quartet) 80 2 "/tmp/frames")
;;
;; Uncomment to run directly:
;; (main)
