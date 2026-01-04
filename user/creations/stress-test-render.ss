;;; stress-test-render.ss --- High-res, long animation stress test
;;;
;;; Renders a longer, higher-resolution animation to stress test
;;; the ASCII-to-video export pipeline.

(load "user/creations/ascii-video-export.ss")
(load "user/creations/sdf-raymarcher.ss")

(display "\n")
(display "============================================================\n")
(display "  STRESS TEST: High-Resolution Ray Marcher Animation\n")
(display "============================================================\n")
(display "  Resolution: 80x35 chars (640x490 pixels)\n")
(display "  Frames: 60 (smooth 2-second loop at 30fps)\n")
(display "  Scene: Morphing Metaballs + Orbiting Atom\n")
(display "============================================================\n\n")

;;; ============================================================
;;; Combined Scene: Metaballs with orbiting spheres
;;; ============================================================

(define (make-stress-scene t)
  "Complex scene with morphing blobs and orbiting satellites"
  (let* ([pulse (+ 0.8 (* 0.4 (sin (* t 2))))]  ; Pulsing effect
         [orbit-angle (* t 1.5)]
         ;; Central morphing blob
         [blob1-x (* 0.6 (cos t))]
         [blob1-z (* 0.6 (sin t))]
         [blob2-x (* 0.6 (cos (+ t 2.1)))]
         [blob2-z (* 0.6 (sin (+ t 2.1)))]
         [blob3-x (* 0.6 (cos (+ t 4.2)))]
         [blob3-z (* 0.6 (sin (+ t 4.2)))]
         ;; Orbiting satellites
         [sat1-x (* 1.8 (cos orbit-angle))]
         [sat1-z (* 1.8 (sin orbit-angle))]
         [sat2-x (* 1.8 (cos (+ orbit-angle 3.14)))]
         [sat2-z (* 1.8 (sin (+ orbit-angle 3.14)))])
        (lambda (p)
                (let* ([rot-p (rotate-y (rotate-x p (* 0.3 t)) t)]
                       ;; Three morphing central blobs (smooth union)
                       [d1 (sdf-sphere rot-p (vec3 blob1-x 0 blob1-z) (* 0.4 pulse))]
                       [d2 (sdf-sphere rot-p (vec3 blob2-x 0 blob2-z) (* 0.35 pulse))]
                       [d3 (sdf-sphere rot-p (vec3 blob3-x 0 blob3-z) (* 0.3 pulse))]
                       [k 0.5]  ; Smoothness
                       [blob-d (sdf-smooth-union (sdf-smooth-union d1 d2 k) d3 k)]
                       ;; Two orbiting satellites
                       [sat1-d (sdf-sphere rot-p (vec3 sat1-x 0 sat1-z) 0.2)]
                       [sat2-d (sdf-sphere rot-p (vec3 sat2-x 0 sat2-z) 0.2)]
                       ;; Small ring torus around the blob
                       [ring-d (sdf-torus rot-p (vec3 0 0 0) 1.2 0.08)])
                      ;; Combine all: blobs, satellites, ring
                      (min blob-d (min sat1-d (min sat2-d ring-d)))))))

;;; ============================================================
;;; Render Parameters
;;; ============================================================

(define *hires-width* 80)
(define *hires-height* 35)
(define *num-frames* 60)

;;; Wide camera for the scene
(define *wide-camera*
  (make-camera (vec3 0 0 -4.5)      ; position (further back for wide view)
               (vec3 0 0 0)          ; look-at
               (vec3 0 1 0)          ; up
               1.4))                 ; fov (radians, wider)

;;; ============================================================
;;; Render Animation
;;; ============================================================

(display "Starting render...\n")
(display (string-append "  " (number->string *hires-width*) "x"
                        (number->string *hires-height*) " chars = "
                        (number->string (* *hires-width* 8)) "x"
                        (number->string (* *hires-height* 14)) " pixels\n"))
(display (string-append "  " (number->string *num-frames*) " frames\n\n"))

(define stress-video (make-video))

(define start-time (cpu-time))

(do ([i 0 (+ i 1)])
    ((>= i *num-frames*))
    (let* ([t (* i (/ 6.28318 *num-frames*))]
           [scene (make-stress-scene t)]
           [frame (render-sdf-frame scene *wide-camera* *hires-width* *hires-height*)])
          (video-add-frame! stress-video frame)
          ;; Progress indicator
          (when (= (modulo i 10) 0)
                (display (string-append "  Frame " (number->string i) "/"
                                        (number->string *num-frames*) "...\n"))
                (flush-output-port))))

(define end-time (cpu-time))
(define render-seconds (/ (- end-time start-time) 1000000.0))

(display "\nRender complete!\n")
(display (string-append "  Time: " (number->string (round render-seconds)) " seconds\n"))
(display (string-append "  FPS: " (number->string (round (/ *num-frames* render-seconds))) " frames/sec\n\n"))

;;; ============================================================
;;; Export
;;; ============================================================

(display "Exporting to GIF...\n")
(video->gif stress-video "user/creations/stress-test-hires.gif" 33)  ; ~30fps

(display "\nDone! Checking file size...\n")
(system "ls -lh user/creations/stress-test-hires.gif")

;;; ============================================================
;;; Post to Discord
;;; ============================================================

(display "\nPosting to Discord...\n")
(load "shell/discord/post-with-files.ss")

(discord-post-files 'art
                    "Stress Test: High-Res Ray Marcher"
                    "80x35 ASCII (640x490px), 60 frames at 30fps. Morphing metaballs with orbiting satellites and a torus ring. Pure Scheme ray marching → FFmpeg GIF."
                    '("user/creations/stress-test-hires.gif"))

(display "\nAll done! Check Discord #art.\n")
