;;; gyroscope-render.ss --- High-res 60fps 5-second looping animation
;;;
;;; An armillary sphere / gyroscope with nested rotating rings.
;;; 300 frames at 60fps = 5 second perfect loop.

(load "user/creations/ascii-video-export.ss")
(load "user/creations/sdf-raymarcher.ss")

(display "\n")
(display "╔══════════════════════════════════════════════════════════════════╗\n")
(display "║     GYROSCOPE: Ultra High-Resolution 60fps Animation             ║\n")
(display "╠══════════════════════════════════════════════════════════════════╣\n")
(display "║  Resolution: 200x88 chars (1600x1232 pixels)                     ║\n")
(display "║  Frames: 300 (5 seconds at 60fps)                                ║\n")
(display "║  Scene: 4 nested rotating tori (armillary sphere)                ║\n")
(display "╚══════════════════════════════════════════════════════════════════╝\n\n")

;;; ============================================================
;;; Gyroscope Scene: Nested Rotating Rings
;;; ============================================================

;;; Rotate a point around an arbitrary axis (Rodrigues' rotation)
(define (rotate-axis p axis angle)
  "Rotate point p around normalized axis by angle radians"
  (let* ([c (cos angle)]
         [s (sin angle)]
         [k axis]
         [kx (vec3-x k)] [ky (vec3-y k)] [kz (vec3-z k)]
         [px (vec3-x p)] [py (vec3-y p)] [pz (vec3-z p)]
         ;; k × p (cross product)
         [cross-x (- (* ky pz) (* kz py))]
         [cross-y (- (* kz px) (* kx pz))]
         [cross-z (- (* kx py) (* ky px))]
         ;; k · p (dot product)
         [dot (+ (* kx px) (* ky py) (* kz pz))]
         ;; Rodrigues formula: p*cos(θ) + (k×p)*sin(θ) + k*(k·p)*(1-cos(θ))
         [rx (+ (* px c) (* cross-x s) (* kx dot (- 1 c)))]
         [ry (+ (* py c) (* cross-y s) (* ky dot (- 1 c)))]
         [rz (+ (* pz c) (* cross-z s) (* kz dot (- 1 c)))])
        (vec3 rx ry rz)))

(define (make-gyroscope-scene t)
  "Armillary sphere with 4 nested rings rotating on different axes"
  (let* (;; Ring parameters - INTEGER rates for perfect looping!
         ;; Each rate = number of full rotations per loop
         ;; Outermost ring: 2 rotations (slow, majestic)
         [r0-R 2.5] [r0-r 0.10]
         [r0-axis (vec3-normalize (vec3 0.2 1 0.3))]  ; Slight tilt
         [r0-angle (* t 2)]
         ;; Outer ring: 3 rotations (tilted axis so it visibly moves!)
         [r1-R 2.0] [r1-r 0.09]
         [r1-axis (vec3-normalize (vec3 0.5 1 0.2))]
         [r1-angle (* t 3)]
         ;; Middle ring: 5 rotations
         [r2-R 1.5] [r2-r 0.08]
         [r2-axis (vec3-normalize (vec3 1 0.5 0))]
         [r2-angle (* t 5)]
         ;; Inner ring: 7 rotations
         [r3-R 1.0] [r3-r 0.07]
         [r3-axis (vec3-normalize (vec3 0.3 1 0.5))]
         [r3-angle (* t 7)]
         ;; Central sphere: 4 pulse cycles
         [sphere-r (+ 0.22 (* 0.04 (sin (* t 4))))])
        
        (lambda (p)
                ;; Transform point into each ring's local space
                (let* (;; Outermost ring (tilted, slow rotation)
                       [p0 (rotate-axis p r0-axis (- r0-angle))]
                       [d0 (sdf-torus p0 (vec3 0 0 0) r0-R r0-r)]
                       
                       ;; Outer ring (tilted axis rotation)
                       [p1 (rotate-axis p r1-axis (- r1-angle))]
                       [d1 (sdf-torus p1 (vec3 0 0 0) r1-R r1-r)]
                       
                       ;; Middle ring (tilted axis)
                       [p2 (rotate-axis p r2-axis (- r2-angle))]
                       [d2 (sdf-torus p2 (vec3 0 0 0) r2-R r2-r)]
                       
                       ;; Inner ring (another tilted axis)
                       [p3 (rotate-axis p r3-axis (- r3-angle))]
                       [d3 (sdf-torus p3 (vec3 0 0 0) r3-R r3-r)]
                       
                       ;; Central sphere
                       [d4 (sdf-sphere p (vec3 0 0 0) sphere-r)])
                      
                      ;; Union of all 5 elements
                      (min d0 (min d1 (min d2 (min d3 d4))))))))

;;; ============================================================
;;; Render Parameters
;;; ============================================================

(define *width* 200)
(define *height* 88)
(define *num-frames* 300)
(define *fps* 60)
(define *frame-delay-ms* (inexact->exact (round (/ 1000.0 *fps*))))

;;; Camera pulled back to see the whole gyroscope (4 rings now)
(define *gyro-camera*
  (make-camera (vec3 0 1.8 -6.5)      ; position (further back for 4th ring)
               (vec3 0 0 0)            ; look-at center
               (vec3 0 1 0)            ; up
               1.0))                   ; fov

;;; ============================================================
;;; Progress Display
;;; ============================================================

(define (display-progress current total start-time)
  (let* ([elapsed (/ (- (cpu-time) start-time) 1000000.0)]
         [rate (if (> current 0) (/ current elapsed) 0)]
         [eta (if (> rate 0) (/ (- total current) rate) 0)]
         [pct (inexact->exact (round (* 100 (/ current total))))])
        (display (string-append
                  "\r  Frame " (number->string current) "/" (number->string total)
                  " (" (number->string pct) "%)"
                  " | " (number->string (inexact->exact (round rate))) " fps"
                  " | ETA: " (number->string (inexact->exact (round eta))) "s   "))
        (flush-output-port)))

;;; ============================================================
;;; Main Render Loop
;;; ============================================================

(display "Starting render...\n")
(display (string-append "  " (number->string *width*) "x" (number->string *height*)
                        " = " (number->string (* *width* 8)) "x" (number->string (* *height* 14)) " pixels\n"))
(display (string-append "  " (number->string *num-frames*) " frames at " (number->string *fps*) " fps\n"))
(display (string-append "  Duration: " (number->string (/ *num-frames* *fps*)) " seconds\n\n"))

(define gyro-video (make-video))
(define render-start (cpu-time))

(do ([i 0 (+ i 1)])
    ((>= i *num-frames*))
    ;; t goes from 0 to 2π for perfect loop
    (let* ([t (* i (/ 6.28318530718 *num-frames*))]
           [scene (make-gyroscope-scene t)]
           [frame (render-sdf-frame scene *gyro-camera* *width* *height*)])
          (video-add-frame! gyro-video frame)
          ;; Progress every 10 frames
          (when (= (modulo i 10) 0)
                (display-progress i *num-frames* render-start))))

(display-progress *num-frames* *num-frames* render-start)
(newline)

(define render-end (cpu-time))
(define total-seconds (/ (- render-end render-start) 1000000.0))

(display "\n")
(display "╔══════════════════════════════════════════════════════════════════╗\n")
(display "║  Render Complete!                                                ║\n")
(display "╠══════════════════════════════════════════════════════════════════╣\n")
(display (string-append "║  Total time: " (number->string (inexact->exact (round total-seconds))) " seconds"))
(display (make-string (- 52 (string-length (number->string (inexact->exact (round total-seconds))))) #\space))
(display "║\n")
(display (string-append "║  Average: " (number->string (round (/ *num-frames* total-seconds))) " frames/sec"))
(display (make-string (- 55 (string-length (number->string (round (/ *num-frames* total-seconds))))) #\space))
(display "║\n")
(display "╚══════════════════════════════════════════════════════════════════╝\n\n")

;;; ============================================================
;;; Export to GIF
;;; ============================================================

(display "Exporting to GIF (this may take a moment)...\n")
(video->gif gyro-video "user/creations/gyroscope-60fps.gif" *frame-delay-ms*)

(display "\nChecking output...\n")
(system "ls -lh user/creations/gyroscope-60fps.gif")
(system "file user/creations/gyroscope-60fps.gif")

;;; ============================================================
;;; Post to Discord
;;; ============================================================

(display "\nPosting to Discord #art...\n")
(load "shell/discord/post-with-files.ss")

(discord-post-files 'art
                    "Gyroscope: 60fps 5-Second Loop"
                    "100x44 ASCII (800x616px), 300 frames at 60fps. Three nested tori rotating on different axes with a pulsing central sphere. Pure Scheme ray marching."
                    '("user/creations/gyroscope-60fps.gif"))

(display "\n")
(display "╔══════════════════════════════════════════════════════════════════╗\n")
(display "║  All done! Check Discord #art for the animation.                 ║\n")
(display "╚══════════════════════════════════════════════════════════════════╝\n")
