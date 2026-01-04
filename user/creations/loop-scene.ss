;;; loop-scene.ss --- DSL for guaranteed-looping ASCII raymarched animations
;;;
;;; A declarative toolkit for creating perfectly looping animations.
;;; The key insight: INTEGER cycle counts guarantee seamless loops.
;;;
;;; Example:
;;;   (define gyro
;;;     (make-loop-scene
;;;       (scene-config :duration 5.0 :fps 60 :resolution '(200 88)
;;;                     :camera (look-at '(0 1.8 -6.5) '(0 0 0)))
;;;       (ring :R 2.5 :r 0.10 :axis '(0.2 1 0.3) :cycles 2)
;;;       (ring :R 2.0 :r 0.09 :axis '(0.5 1 0.2) :cycles 3)
;;;       (ring :R 1.5 :r 0.08 :axis '(1 0.5 0) :cycles 5)
;;;       (ring :R 1.0 :r 0.07 :axis '(0.3 1 0.5) :cycles 7)
;;;       (core :r 0.22 :pulse 0.04 :cycles 4)))
;;;
;;;   (render-loop! gyro "gyroscope.gif")

(load "core/base/prelude.ss")
(load "user/creations/sdf-raymarcher.ss")
(load "user/creations/ascii-video-export.ss")

(display "\n")
(display "╔══════════════════════════════════════════════════════════════════╗\n")
(display "║  LOOP-SCENE: Guaranteed Looping ASCII Animations                 ║\n")
(display "╠══════════════════════════════════════════════════════════════════╣\n")
(display "║  Integer cycles = Perfect loops. Always.                         ║\n")
(display "╚══════════════════════════════════════════════════════════════════╝\n\n")

;;; ============================================================
;;; Core Math: Rodrigues Rotation (from gyroscope-render.ss)
;;; ============================================================

(define (vec3-cross a b)
  "Cross product of two vec3s"
  (vec3 (- (* (vec3-y a) (vec3-z b)) (* (vec3-z a) (vec3-y b)))
        (- (* (vec3-z a) (vec3-x b)) (* (vec3-x a) (vec3-z b)))
        (- (* (vec3-x a) (vec3-y b)) (* (vec3-y a) (vec3-x b)))))

(define (rotate-axis p axis angle)
  "Rotate point p around normalized axis by angle radians (Rodrigues' formula)"
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

;;; ============================================================
;;; Animation Primitives
;;; ============================================================

;;; These are "time functions" - they take normalized time t ∈ [0, 2π)
;;; and return animated values.

(define (make-rotator axis cycles)
  "Create a rotation function that completes 'cycles' full rotations per loop.
   REQUIRES: cycles is a positive integer for perfect looping."
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-rotator
                 (string-append "cycles must be a positive integer for perfect loops, got: "
                                (if (number? cycles) (number->string cycles) "non-number"))))
  (let ([norm-axis (vec3-normalize axis)])
       (lambda (t)
               ;; At t=2π, angle = cycles * 2π = full rotations
               (lambda (p)
                       (rotate-axis p norm-axis (* t cycles))))))

(define (make-pulser base amplitude cycles)
  "Create a pulsing value: base + amplitude * sin(t * cycles).
   REQUIRES: cycles is a positive integer for perfect looping."
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-pulser
                 (string-append "cycles must be a positive integer for perfect loops, got: "
                                (if (number? cycles) (number->string cycles) "non-number"))))
  (lambda (t)
          (+ base (* amplitude (sin (* t cycles))))))

(define (make-orbiter radius cycles)
  "Create an orbital path in XZ plane completing 'cycles' orbits per loop.
   REQUIRES: cycles is a positive integer for perfect looping."
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-orbiter
                 (string-append "cycles must be a positive integer for perfect loops, got: "
                                (if (number? cycles) (number->string cycles) "non-number"))))
  (lambda (t)
          (vec3 (* radius (cos (* t cycles)))
                0
                (* radius (sin (* t cycles))))))

(define (make-orbiter-3d radius axis cycles)
  "Create an orbital path around arbitrary axis.
   REQUIRES: cycles is a positive integer for perfect looping."
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-orbiter-3d
                 (string-append "cycles must be a positive integer for perfect loops, got: "
                                (if (number? cycles) (number->string cycles) "non-number"))))
  (let ([norm-axis (vec3-normalize axis)])
       (lambda (t)
               ;; Start with point on XZ plane, rotate around axis
               (let ([base-point (vec3 radius 0 0)])
                    (rotate-axis base-point norm-axis (* t cycles))))))

;;; ============================================================
;;; Scene Element Constructors
;;; ============================================================

;;; Each element returns a function: t → (p → distance)

(define (make-rotating-torus R r axis cycles)
  "A torus that rotates around an axis.
   R = major radius, r = minor radius
   axis = rotation axis (will be normalized)
   cycles = integer number of rotations per loop"
  (let ([rotator (make-rotator axis cycles)])
       (lambda (t)
               (let ([rot-fn (rotator t)])
                    (lambda (p)
                            (sdf-torus (rot-fn p) (vec3 0 0 0) R r))))))

(define (make-pulsing-sphere base-r pulse-amplitude cycles)
  "A sphere that pulses in size.
   base-r = base radius
   pulse-amplitude = how much it grows/shrinks
   cycles = integer number of pulses per loop"
  (let ([pulser (make-pulser base-r pulse-amplitude cycles)])
       (lambda (t)
               (let ([r (pulser t)])
                    (lambda (p)
                            (sdf-sphere p (vec3 0 0 0) r))))))

(define (make-orbiting-sphere r orbit-radius cycles)
  "A sphere that orbits in the XZ plane.
   r = sphere radius
   orbit-radius = distance from center
   cycles = integer number of orbits per loop"
  (let ([orbiter (make-orbiter orbit-radius cycles)])
       (lambda (t)
               (let ([pos (orbiter t)])
                    (lambda (p)
                            (sdf-sphere p pos r))))))

(define (make-static-sphere r position)
  "A non-animated sphere at a fixed position."
  (lambda (t)
          (lambda (p)
                  (sdf-sphere p position r))))

(define (make-static-torus R r position)
  "A non-animated torus at a fixed position."
  (lambda (t)
          (lambda (p)
                  (sdf-torus p position R r))))

(define (make-rotating-box size axis cycles)
  "A box that rotates around an axis."
  (let ([rotator (make-rotator axis cycles)])
       (lambda (t)
               (let ([rot-fn (rotator t)])
                    (lambda (p)
                            (sdf-box (rot-fn p) (vec3 0 0 0) size))))))

;;; ============================================================
;;; Scene Composition
;;; ============================================================

(define (combine-elements elements)
  "Combine multiple scene elements into one SDF (union).
   elements = list of (t → (p → distance)) functions"
  (lambda (t)
          (let ([sdfs (map (lambda (elem) (elem t)) elements)])
               (lambda (p)
                       (apply min (map (lambda (sdf) (sdf p)) sdfs))))))

(define (combine-smooth elements k)
  "Combine elements with smooth union (k = smoothness factor)."
  (lambda (t)
          (let ([sdfs (map (lambda (elem) (elem t)) elements)])
               (lambda (p)
                       (fold-left (lambda (acc sdf)
                                          (sdf-smooth-union acc (sdf p) k))
                                  (car sdfs) p)
                       ;; Actually need to call each sdf on p
                       (let loop ([remaining sdfs] [result +inf.0])
                            (if (null? remaining)
                                result
                                (loop (cdr remaining)
                                      (if (= result +inf.0)
                                          ((car remaining) p)
                                          (sdf-smooth-union result ((car remaining) p) k)))))))))

;;; ============================================================
;;; Camera Helpers
;;; ============================================================

(define (look-at from to)
  "Create a camera looking from 'from' position toward 'to' position.
   from, to = lists of 3 numbers (x y z)"
  (let ([pos (apply vec3 from)]
        [target (apply vec3 to)])
       (make-camera pos target (vec3 0 1 0) 1.0)))

(define (look-at* from to fov)
  "Create a camera with custom field of view (in radians)."
  (let ([pos (apply vec3 from)]
        [target (apply vec3 to)])
       (make-camera pos target (vec3 0 1 0) fov)))

;;; ============================================================
;;; Scene Configuration
;;; ============================================================

(define-record-type loop-scene
  (fields duration    ; seconds
          fps         ; frames per second
          width       ; ASCII columns
          height      ; ASCII rows
          camera      ; camera object
          scene-fn))  ; t → (p → distance)

(define (make-loop-scene* duration fps width height camera elements)
  "Internal constructor for loop scenes."
  (let ([scene-fn (combine-elements elements)]
        [num-frames (inexact->exact (round (* duration fps)))])
       (make-loop-scene duration fps width height camera scene-fn)))

;;; ============================================================
;;; High-Level Scene Builder
;;; ============================================================

;;; Macro-free builder using a simple alist for options

(define (build-loop-scene config . elements)
  "Build a loop scene from config alist and element list.

   config keys:
     'duration - loop duration in seconds (default: 5.0)
     'fps - frames per second (default: 30)
     'width - ASCII columns (default: 80)
     'height - ASCII rows (default: 35)
     'camera - camera object (required)

   Example:
     (build-loop-scene
       '((duration . 5.0) (fps . 60) (width . 200) (height . 88)
         (camera . ,(look-at '(0 1.8 -6.5) '(0 0 0))))
       (ring :R 2.5 :r 0.10 :axis '(0.2 1 0.3) :cycles 2)
       ...)"
  (let* ([get (lambda (key default)
                      (let ([pair (assq key config)])
                           (if pair (cdr pair) default)))]
         [duration (get 'duration 5.0)]
         [fps (get 'fps 30)]
         [width (get 'width 80)]
         [height (get 'height 35)]
         [camera (get 'camera #f)])
        (unless camera
                (error 'build-loop-scene "camera is required in config"))
        (make-loop-scene* duration fps width height camera elements)))

;;; ============================================================
;;; Shorthand Element Constructors (for clean DSL syntax)
;;; ============================================================

(define (ring . args)
  "Create a rotating torus ring.
   Keyword args: :R (major radius), :r (minor radius), :axis, :cycles
   Example: (ring :R 2.0 :r 0.1 :axis '(1 0 0) :cycles 3)"
  (let ([R (get-keyword-arg args ':R 1.0)]
        [r (get-keyword-arg args ':r 0.1)]
        [axis (get-keyword-arg args ':axis '(0 1 0))]
        [cycles (get-keyword-arg args ':cycles 1)])
       (make-rotating-torus R r (apply vec3 axis) cycles)))

(define (core . args)
  "Create a pulsing central sphere.
   Keyword args: :r (base radius), :pulse (amplitude), :cycles
   Example: (core :r 0.25 :pulse 0.05 :cycles 4)"
  (let ([r (get-keyword-arg args ':r 0.25)]
        [pulse (get-keyword-arg args ':pulse 0.05)]
        [cycles (get-keyword-arg args ':cycles 1)])
       (make-pulsing-sphere r pulse cycles)))

(define (orb . args)
  "Create an orbiting sphere.
   Keyword args: :r (radius), :orbit (orbit radius), :cycles
   Example: (orb :r 0.2 :orbit 1.5 :cycles 2)"
  (let ([r (get-keyword-arg args ':r 0.2)]
        [orbit (get-keyword-arg args ':orbit 1.0)]
        [cycles (get-keyword-arg args ':cycles 1)])
       (make-orbiting-sphere r orbit cycles)))

(define (ball . args)
  "Create a static sphere.
   Keyword args: :r (radius), :at (position as list)
   Example: (ball :r 0.3 :at '(0 0 0))"
  (let ([r (get-keyword-arg args ':r 0.3)]
        [at (get-keyword-arg args ':at '(0 0 0))])
       (make-static-sphere r (apply vec3 at))))

(define (box . args)
  "Create a rotating box.
   Keyword args: :size (as list), :axis, :cycles
   Example: (box :size '(0.5 0.5 0.5) :axis '(1 1 1) :cycles 2)"
  (let ([size (get-keyword-arg args ':size '(0.5 0.5 0.5))]
        [axis (get-keyword-arg args ':axis '(0 1 0))]
        [cycles (get-keyword-arg args ':cycles 1)])
       (make-rotating-box (apply vec3 size) (apply vec3 axis) cycles)))

;;; Keyword argument helper
(define (get-keyword-arg args key default)
  "Extract keyword argument from args list."
  (let loop ([remaining args])
       (cond
        [(null? remaining) default]
        [(null? (cdr remaining)) default]
        [(eq? (car remaining) key) (cadr remaining)]
        [else (loop (cddr remaining))])))

;;; Define keyword symbols for cleaner DSL syntax
;;; These are self-quoting when used with the define-syntax-rule trick
(define :R ':R)
(define :r ':r)
(define :axis ':axis)
(define :cycles ':cycles)
(define :at ':at)
(define :size ':size)
(define :pulse ':pulse)
(define :orbit ':orbit)
(define :a ':a)
(define :b ':b)
(define :delta ':delta)
(define :scale ':scale)
(define :amplitude ':amplitude)
(define :frequency ':frequency)
(define :rate ':rate)
(define :positions ':positions)
(define :k-base ':k-base)
(define :k-range ':k-range)
(define :element ':element)
(define :spacing ':spacing)
(define :count ':count)
(define :path ':path)
(define :radius ':radius)
(define :height ':height)
(define :turns ':turns)

;;; ============================================================
;;; Validation
;;; ============================================================

(define (validate-scene scene)
  "Validate a loop scene before rendering.
   Returns #t if valid, raises error otherwise."
  (let* ([duration (loop-scene-duration scene)]
         [fps (loop-scene-fps scene)]
         [num-frames (* duration fps)])
        ;; Check that num-frames is reasonable
        (unless (and (positive? duration) (positive? fps))
                (error 'validate-scene "duration and fps must be positive"))
        (unless (integer? num-frames)
                (display "⚠️ Warning: duration × fps is not an integer. ")
                (display "Consider adjusting for clean frame count.\n"))
        #t))

;;; ============================================================
;;; Rendering
;;; ============================================================

(define (render-loop! scene output-path)
  "Render a loop scene to a GIF file."
  (validate-scene scene)
  (let* ([duration (loop-scene-duration scene)]
         [fps (loop-scene-fps scene)]
         [width (loop-scene-width scene)]
         [height (loop-scene-height scene)]
         [camera (loop-scene-camera scene)]
         [scene-fn (loop-scene-scene-fn scene)]
         [num-frames (inexact->exact (round (* duration fps)))]
         [frame-delay-ms (inexact->exact (round (/ 1000.0 fps)))]
         [video (make-video)]
         [start-time (cpu-time)])
        
        (display "\n")
        (display "╔══════════════════════════════════════════════════════════════════╗\n")
        (display "║  Rendering Loop Scene                                            ║\n")
        (display "╠══════════════════════════════════════════════════════════════════╣\n")
        (display (string-append "║  Resolution: " (number->string width) "×"
                                (number->string height) " chars ("
                                (number->string (* width 8)) "×"
                                (number->string (* height 14)) " pixels)\n"))
        (display (string-append "║  Frames: " (number->string num-frames)
                                " at " (number->string fps) " fps\n"))
        (display (string-append "║  Duration: " (number->string duration) " seconds\n"))
        (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
        
        ;; Render all frames
        (do ([i 0 (+ i 1)])
            ((>= i num-frames))
            (let* ([t (* i (/ 6.28318530718 num-frames))]
                   [sdf (scene-fn t)]
                   [frame (render-sdf-frame sdf camera width height)])
                  (video-add-frame! video frame)
                  ;; Progress every 10 frames
                  (when (= (modulo i 10) 0)
                        (let* ([elapsed (/ (- (cpu-time) start-time) 1000000.0)]
                               [rate (if (> i 0) (/ i elapsed) 0)]
                               [eta (if (> rate 0) (/ (- num-frames i) rate) 0)])
                              (display (string-append
                                        "\r  Frame " (number->string i) "/"
                                        (number->string num-frames)
                                        " | " (number->string (inexact->exact (round rate))) " fps"
                                        " | ETA: " (number->string (inexact->exact (round eta))) "s   "))
                              (flush-output-port)))))
        
        ;; Final progress
        (display (string-append "\r  Frame " (number->string num-frames) "/"
                                (number->string num-frames) " complete!          \n"))
        
        ;; Calculate stats
        (let* ([end-time (cpu-time)]
               [total-seconds (/ (- end-time start-time) 1000000.0)])
              (display "\n")
              (display "╔══════════════════════════════════════════════════════════════════╗\n")
              (display "║  Render Complete!                                                ║\n")
              (display "╠══════════════════════════════════════════════════════════════════╣\n")
              (display (string-append "║  Time: " (number->string (inexact->exact (round total-seconds)))
                                      " seconds\n"))
              (display (string-append "║  Speed: "
                                      (number->string (inexact->exact (round (/ num-frames total-seconds))))
                                      " frames/sec\n"))
              (display "╚══════════════════════════════════════════════════════════════════╝\n\n"))
        
        ;; Export to GIF
        (display "Exporting to GIF...\n")
        (video->gif video output-path frame-delay-ms)
        
        ;; Check file size
        (display "\nOutput:\n")
        (system (string-append "ls -lh " output-path))
        
        output-path))

;;; ============================================================
;;; Preview Utilities
;;; ============================================================

(define (preview-frame scene t)
  "Render a single frame at normalized time t ∈ [0, 2π) and display it."
  (let* ([width (loop-scene-width scene)]
         [height (loop-scene-height scene)]
         [camera (loop-scene-camera scene)]
         [scene-fn (loop-scene-scene-fn scene)]
         [sdf (scene-fn t)]
         [frame (render-sdf-frame sdf camera width height)])
        (display (frame-render-to-string frame))
        (newline)))

(define (preview-scene scene)
  "Preview a scene at t=0 (start of loop)."
  (preview-frame scene 0))

(define (preview-quarter scene)
  "Preview at quarter points: 0, π/2, π, 3π/2"
  (display "t = 0:\n")
  (preview-frame scene 0)
  (display "\nt = π/2:\n")
  (preview-frame scene 1.5708)
  (display "\nt = π:\n")
  (preview-frame scene 3.1416)
  (display "\nt = 3π/2:\n")
  (preview-frame scene 4.7124))

(define (estimate-render-time scene)
  "Estimate render time based on resolution and frame count.
   Returns estimated seconds (rough approximation)."
  (let* ([width (loop-scene-width scene)]
         [height (loop-scene-height scene)]
         [fps (loop-scene-fps scene)]
         [duration (loop-scene-duration scene)]
         [num-frames (* fps duration)]
         [pixels (* width height)]
         ;; Rough estimate: ~500 pixels/second on average hardware
         [pixels-per-second 500]
         [total-pixels (* pixels num-frames)]
         [estimated-seconds (/ total-pixels pixels-per-second)])
        (display (string-append "Estimated render time: ~"
                                (number->string (inexact->exact (round estimated-seconds)))
                                " seconds\n"))
        (display (string-append "  " (number->string num-frames) " frames × "
                                (number->string width) "×" (number->string height)
                                " = " (number->string total-pixels) " total ray marches\n"))
        estimated-seconds))

;;; ============================================================
;;; Rate Suggestions
;;; ============================================================

;;; Pre-computed sets of coprime integers for visual interest.
;;; Coprime rates ensure patterns don't repeat until full loop.
(define *coprime-sets*
  '((2 3)           ; 2 elements
    (2 3 5)         ; 3 elements
    (2 3 5 7)       ; 4 elements
    (2 3 5 7 11)    ; 5 elements
    (2 3 5 7 11 13) ; 6 elements
    ;; Slower variants
    (1 2 3)
    (1 2 3 5)
    (1 2 3 5 7)))

(define (suggest-rates n)
  "Suggest coprime rates for n animated elements.
   Returns a list of integers that are pairwise coprime."
  (display (string-append "Suggested coprime rates for " (number->string n) " elements:\n"))
  (let loop ([sets *coprime-sets*])
       (cond
        [(null? sets)
         (display "  No pre-computed set available. Use prime numbers!\n")
         '()]
        [(>= (length (car sets)) n)
         (let ([rates (take (car sets) n)])
              (display (string-append "  " (format-list rates) "\n"))
              (display "  (These are coprime, maximizing visual complexity)\n")
              rates)]
        [else (loop (cdr sets))])))

(define (take lst n)
  "Take first n elements of list."
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (format-list lst)
  "Format a list for display."
  (apply string-append
         (cons "("
               (append (let loop ([items lst] [first #t])
                            (if (null? items)
                                '()
                                (cons (string-append (if first "" " ")
                                                     (number->string (car items)))
                                      (loop (cdr items) #f))))
                       '(")")))))

;;; ============================================================
;;; Preset Scenes
;;; ============================================================

(define (gyroscope-scene)
  "The classic 4-ring armillary sphere."
  (build-loop-scene
   `((duration . 5.0) (fps . 60) (width . 200) (height . 88)
     (camera . ,(look-at '(0 1.8 -6.5) '(0 0 0))))
   (ring :R 2.5 :r 0.10 :axis '(0.2 1 0.3) :cycles 2)
   (ring :R 2.0 :r 0.09 :axis '(0.5 1 0.2) :cycles 3)
   (ring :R 1.5 :r 0.08 :axis '(1 0.5 0) :cycles 5)
   (ring :R 1.0 :r 0.07 :axis '(0.3 1 0.5) :cycles 7)
   (core :r 0.22 :pulse 0.04 :cycles 4)))

(define (atom-scene)
  "An atom with orbiting electrons."
  (build-loop-scene
   `((duration . 3.0) (fps . 30) (width . 80) (height . 35)
     (camera . ,(look-at '(0 0 -4) '(0 0 0))))
   (ball :r 0.4 :at '(0 0 0))
   (orb :r 0.15 :orbit 1.2 :cycles 2)
   (orb :r 0.15 :orbit 1.2 :cycles 3)
   (orb :r 0.15 :orbit 1.2 :cycles 5)))

(define (spinning-cube-scene)
  "A cube tumbling on multiple axes."
  (build-loop-scene
   `((duration . 4.0) (fps . 30) (width . 60) (height . 30)
     (camera . ,(look-at '(0 0 -4) '(0 0 0))))
   (box :size '(0.8 0.8 0.8) :axis '(1 1 0.5) :cycles 3)))

;;; ============================================================
;;; Help / Documentation
;;; ============================================================

(define (loop-scene-help)
  "Display usage documentation."
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════════╗\n")
  (display "║  LOOP-SCENE DSL                                                  ║\n")
  (display "║  Create perfectly looping ASCII raymarched animations            ║\n")
  (display "╠══════════════════════════════════════════════════════════════════╣\n")
  (display "║                                                                  ║\n")
  (display "║  THE KEY INSIGHT:                                                ║\n")
  (display "║  Integer cycle counts = perfect loops. Always.                   ║\n")
  (display "║                                                                  ║\n")
  (display "║  If element completes N rotations in time T,                     ║\n")
  (display "║  it returns to start position at T. Simple!                      ║\n")
  (display "║                                                                  ║\n")
  (display "╠══════════════════════════════════════════════════════════════════╣\n")
  (display "║  SCENE ELEMENTS:                                                 ║\n")
  (display "║                                                                  ║\n")
  (display "║  (ring :R 2.0 :r 0.1 :axis '(1 0 0) :cycles 3)                   ║\n")
  (display "║    → Torus rotating around axis, 3 full rotations per loop       ║\n")
  (display "║                                                                  ║\n")
  (display "║  (core :r 0.3 :pulse 0.05 :cycles 4)                             ║\n")
  (display "║    → Pulsing sphere, 4 pulse cycles per loop                     ║\n")
  (display "║                                                                  ║\n")
  (display "║  (orb :r 0.2 :orbit 1.5 :cycles 2)                               ║\n")
  (display "║    → Orbiting sphere, 2 orbits per loop                          ║\n")
  (display "║                                                                  ║\n")
  (display "║  (ball :r 0.3 :at '(0 0 0))                                      ║\n")
  (display "║    → Static sphere                                               ║\n")
  (display "║                                                                  ║\n")
  (display "║  (box :size '(0.5 0.5 0.5) :axis '(1 1 1) :cycles 2)             ║\n")
  (display "║    → Rotating box                                                ║\n")
  (display "║                                                                  ║\n")
  (display "╠══════════════════════════════════════════════════════════════════╣\n")
  (display "║  BUILDING A SCENE:                                               ║\n")
  (display "║                                                                  ║\n")
  (display "║  (define my-scene                                                ║\n")
  (display "║    (build-loop-scene                                             ║\n")
  (display "║      `((duration . 5.0)                                          ║\n")
  (display "║        (fps . 60)                                                ║\n")
  (display "║        (width . 100)                                             ║\n")
  (display "║        (height . 44)                                             ║\n")
  (display "║        (camera . ,(look-at '(0 1 -5) '(0 0 0))))                 ║\n")
  (display "║      (ring :R 1.5 :r 0.1 :axis '(1 0 0) :cycles 2)               ║\n")
  (display "║      (core :r 0.3 :pulse 0.05 :cycles 3)))                       ║\n")
  (display "║                                                                  ║\n")
  (display "╠══════════════════════════════════════════════════════════════════╣\n")
  (display "║  RENDERING:                                                      ║\n")
  (display "║                                                                  ║\n")
  (display "║  (preview-scene my-scene)      ; Quick preview at t=0            ║\n")
  (display "║  (preview-quarter my-scene)    ; Preview at 4 time points        ║\n")
  (display "║  (estimate-render-time my-scene)                                 ║\n")
  (display "║  (render-loop! my-scene \"output.gif\")                            ║\n")
  (display "║                                                                  ║\n")
  (display "╠══════════════════════════════════════════════════════════════════╣\n")
  (display "║  UTILITIES:                                                      ║\n")
  (display "║                                                                  ║\n")
  (display "║  (suggest-rates 4)             ; Coprime rates for 4 elements    ║\n")
  (display "║                                                                  ║\n")
  (display "╠══════════════════════════════════════════════════════════════════╣\n")
  (display "║  PRESETS:                                                        ║\n")
  (display "║                                                                  ║\n")
  (display "║  (gyroscope-scene)             ; 4-ring armillary sphere         ║\n")
  (display "║  (atom-scene)                  ; Orbiting electrons              ║\n")
  (display "║  (spinning-cube-scene)         ; Tumbling cube                   ║\n")
  (display "║                                                                  ║\n")
  (display "╚══════════════════════════════════════════════════════════════════╝\n\n"))

;;; ============================================================
;;; ADVANCED ANIMATIONS
;;; ============================================================

;;; ------------------------------------------------------------
;;; Morphing Between Shapes
;;; ------------------------------------------------------------

(define (make-morph shape1 shape2 cycles)
  "Smoothly morph between two SDF shapes.
   At t=0: 100% shape1, at t=π: 100% shape2, back to shape1 at t=2π.
   cycles = number of full morph cycles per loop."
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-morph "cycles must be a positive integer"))
  (lambda (t)
          (let* ([blend (/ (+ 1 (cos (* t cycles))) 2.0)]  ; 1→0→1
                 [sdf1 (shape1 t)]
                 [sdf2 (shape2 t)])
                (lambda (p)
                        (+ (* blend (sdf1 p))
                           (* (- 1 blend) (sdf2 p)))))))

(define (morph elem1 elem2 . args)
  "Morph between two scene elements.
   Example: (morph (ball :r 0.5 :at '(0 0 0))
                   (box :size '(0.4 0.4 0.4) :axis '(0 1 0) :cycles 1)
                   :cycles 2)"
  (let ([cycles (get-keyword-arg args ':cycles 1)])
       (make-morph elem1 elem2 cycles)))

;;; ------------------------------------------------------------
;;; Path Animations (beyond simple orbits)
;;; ------------------------------------------------------------

(define (make-lissajous-path a b delta cycles)
  "Lissajous curve path: x = sin(a*t + delta), y = 0, z = sin(b*t)
   Creates interesting figure-8 and pretzel patterns.
   a, b should be coprime integers for non-repeating patterns."
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-lissajous-path "cycles must be a positive integer"))
  (lambda (t)
          (vec3 (sin (+ (* a t cycles) delta))
                0
                (sin (* b t cycles)))))

(define (make-spiral-path radius height turns cycles)
  "Helical spiral path.
   radius = radius of helix
   height = total height (centered at 0)
   turns = number of turns in the helix
   cycles = rotations per loop"
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-spiral-path "cycles must be a positive integer"))
  (lambda (t)
          (let ([angle (* t turns cycles)]
                [y-offset (* (- (/ t 6.28318) 0.5) height)])
               (vec3 (* radius (cos angle))
                     y-offset
                     (* radius (sin angle))))))

(define (make-figure8-path scale cycles)
  "Figure-8 (lemniscate) path in XZ plane."
  (make-lissajous-path 1 2 0 cycles))

(define (follow-path path-fn r)
  "Create a sphere that follows a parametric path.
   path-fn = (t → vec3 position)
   r = sphere radius"
  (lambda (t)
          (let ([pos (path-fn t)])
               (lambda (p)
                       (sdf-sphere p pos r)))))

(define (tracer . args)
  "A sphere following a Lissajous path.
   Keyword args: :r, :a, :b, :delta, :scale, :cycles
   Example: (tracer :r 0.2 :a 2 :b 3 :scale 1.5 :cycles 1)"
  (let* ([r (get-keyword-arg args ':r 0.2)]
         [a (get-keyword-arg args ':a 2)]
         [b (get-keyword-arg args ':b 3)]
         [delta (get-keyword-arg args ':delta 0)]
         [scale (get-keyword-arg args ':scale 1.0)]
         [cycles (get-keyword-arg args ':cycles 1)]
         [path (make-lissajous-path a b delta cycles)])
        (lambda (t)
                (let ([pos (vec3-scale (path t) scale)])
                     (lambda (p)
                             (sdf-sphere p pos r))))))

(define (helix-tracer . args)
  "A sphere following a helical path.
   Keyword args: :r, :radius, :height, :turns, :cycles"
  (let* ([r (get-keyword-arg args ':r 0.15)]
         [radius (get-keyword-arg args ':radius 1.0)]
         [height (get-keyword-arg args ':height 2.0)]
         [turns (get-keyword-arg args ':turns 3)]
         [cycles (get-keyword-arg args ':cycles 1)]
         [path (make-spiral-path radius height turns cycles)])
        (follow-path path r)))

;;; ------------------------------------------------------------
;;; Wave / Deformation Effects
;;; ------------------------------------------------------------

(define (make-wave-deformer amplitude frequency cycles)
  "Create a wave deformation: displaces surface based on position.
   Applies sin wave along Y axis."
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-wave-deformer "cycles must be a positive integer"))
  (lambda (t)
          (let ([phase (* t cycles)])
               (lambda (sdf)
                       (lambda (p)
                               (let* ([wave (* amplitude (sin (+ (* frequency (vec3-x p))
                                                                 phase)))]
                                      [displaced-y (+ (vec3-y p) wave)]
                                      [p* (vec3 (vec3-x p) displaced-y (vec3-z p))])
                                     (sdf p*)))))))

(define (make-twist-deformer twist-rate cycles)
  "Twist the shape around Y axis.
   twist-rate = radians of twist per unit of Y."
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-twist-deformer "cycles must be a positive integer"))
  (lambda (t)
          (let ([twist (* twist-rate (+ 1 (sin (* t cycles))))])
               (lambda (sdf)
                       (lambda (p)
                               (let* ([angle (* twist (vec3-y p))]
                                      [c (cos angle)]
                                      [s (sin angle)]
                                      [x (vec3-x p)]
                                      [z (vec3-z p)]
                                      [p* (vec3 (- (* c x) (* s z))
                                                (vec3-y p)
                                                (+ (* s x) (* c z)))])
                                     (sdf p*)))))))

(define (with-wave base-elem . args)
  "Apply wave deformation to an element.
   Example: (with-wave (ball :r 1.0 :at '(0 0 0))
                       :amplitude 0.2 :frequency 3 :cycles 2)"
  (let* ([amplitude (get-keyword-arg args ':amplitude 0.1)]
         [frequency (get-keyword-arg args ':frequency 2)]
         [cycles (get-keyword-arg args ':cycles 1)]
         [deformer (make-wave-deformer amplitude frequency cycles)])
        (lambda (t)
                ((deformer t) (base-elem t)))))

(define (with-twist base-elem . args)
  "Apply twist deformation to an element.
   Example: (with-twist (box :size '(0.5 2 0.5) :axis '(0 1 0) :cycles 1)
                        :rate 1.5 :cycles 2)"
  (let* ([rate (get-keyword-arg args ':rate 1.0)]
         [cycles (get-keyword-arg args ':cycles 1)]
         [deformer (make-twist-deformer rate cycles)])
        (lambda (t)
                ((deformer t) (base-elem t)))))

;;; ------------------------------------------------------------
;;; Animated Smooth Union
;;; ------------------------------------------------------------

(define (make-breathing-union k-base k-range cycles)
  "Smooth union with animated smoothness factor.
   Creates a 'breathing' blob effect."
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-breathing-union "cycles must be a positive integer"))
  (lambda (t)
          (+ k-base (* k-range (sin (* t cycles))))))

(define (blob . args)
  "Multiple spheres combined with animated smooth union.
   Creates a breathing/morphing blob effect.
   Keyword args: :positions (list of (x y z)), :r, :k-base, :k-range, :cycles
   Example: (blob :positions '((0.5 0 0) (-0.5 0 0) (0 0.5 0))
                  :r 0.4 :k-base 0.3 :k-range 0.2 :cycles 2)"
  (let* ([positions (get-keyword-arg args ':positions '((0.5 0 0) (-0.5 0 0)))]
         [r (get-keyword-arg args ':r 0.4)]
         [k-base (get-keyword-arg args ':k-base 0.3)]
         [k-range (get-keyword-arg args ':k-range 0.1)]
         [cycles (get-keyword-arg args ':cycles 1)]
         [k-fn (make-breathing-union k-base k-range cycles)]
         [centers (map (lambda (pos) (apply vec3 pos)) positions)])
        (lambda (t)
                (let ([k (k-fn t)])
                     (lambda (p)
                             (let loop ([remaining centers] [result +inf.0])
                                  (if (null? remaining)
                                      result
                                      (let ([d (sdf-sphere p (car remaining) r)])
                                           (loop (cdr remaining)
                                                 (if (= result +inf.0)
                                                     d
                                                     (sdf-smooth-union result d k)))))))))))

;;; ------------------------------------------------------------
;;; Camera Animation
;;; ------------------------------------------------------------

(define (make-orbiting-camera distance height look-at-pos cycles)
  "Create a camera that orbits around a point.
   Returns a function: t → camera"
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-orbiting-camera "cycles must be a positive integer"))
  (let ([target (apply vec3 look-at-pos)])
       (lambda (t)
               (let* ([angle (* t cycles)]
                      [x (* distance (sin angle))]
                      [z (- (* distance (cos angle)))]
                      [pos (vec3 x height z)])
                     (make-camera pos target (vec3 0 1 0) 1.0)))))

(define (build-loop-scene-animated-camera config camera-fn . elements)
  "Build a loop scene with an animated camera.
   camera-fn = (t → camera)

   Example:
     (build-loop-scene-animated-camera
       '((duration . 5.0) (fps . 30) (width . 80) (height . 35))
       (make-orbiting-camera 5 2 '(0 0 0) 1)
       (ball :r 1.0 :at '(0 0 0)))"
  (let* ([get (lambda (key default)
                      (let ([pair (assq key config)])
                           (if pair (cdr pair) default)))]
         [duration (get 'duration 5.0)]
         [fps (get 'fps 30)]
         [width (get 'width 80)]
         [height (get 'height 35)]
         [scene-fn (combine-elements elements)]
         [num-frames (inexact->exact (round (* duration fps)))])
        ;; Store camera-fn in the scene for animated rendering
        (vector 'animated-camera-scene
                duration fps width height camera-fn scene-fn)))

;;; Override render for animated camera scenes
(define (render-animated-camera-loop! scene output-path)
  "Render a scene with animated camera."
  (let* ([duration (vector-ref scene 1)]
         [fps (vector-ref scene 2)]
         [width (vector-ref scene 3)]
         [height (vector-ref scene 4)]
         [camera-fn (vector-ref scene 5)]
         [scene-fn (vector-ref scene 6)]
         [num-frames (inexact->exact (round (* duration fps)))]
         [frame-delay-ms (inexact->exact (round (/ 1000.0 fps)))]
         [video (make-video)]
         [start-time (cpu-time)])
        
        (display "\n")
        (display "╔══════════════════════════════════════════════════════════════════╗\n")
        (display "║  Rendering Animated Camera Scene                                 ║\n")
        (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
        
        ;; Render with animated camera
        (do ([i 0 (+ i 1)])
            ((>= i num-frames))
            (let* ([t (* i (/ 6.28318530718 num-frames))]
                   [camera (camera-fn t)]
                   [sdf (scene-fn t)]
                   [frame (render-sdf-frame sdf camera width height)])
                  (video-add-frame! video frame)
                  (when (= (modulo i 10) 0)
                        (display (string-append "\r  Frame " (number->string i) "/" (number->string num-frames)))
                        (flush-output-port))))
        
        (display (string-append "\r  Frame " (number->string num-frames) "/" (number->string num-frames) " complete!\n"))
        
        ;; Export
        (display "Exporting to GIF...\n")
        (video->gif video output-path frame-delay-ms)
        output-path))

;;; ------------------------------------------------------------
;;; Animated Lighting
;;; ------------------------------------------------------------

(define *animated-light-dir* (make-parameter #f))

(define (make-rotating-light cycles)
  "Light source that rotates around the scene.
   Returns: t → light-direction"
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-rotating-light "cycles must be a positive integer"))
  (lambda (t)
          (let ([angle (* t cycles)])
               (vec3-normalize (vec3 (cos angle) 0.8 (sin angle))))))

;;; ------------------------------------------------------------
;;; Repetition / Instancing
;;; ------------------------------------------------------------

(define (make-grid-repeat elem spacing count)
  "Repeat an element in a grid pattern.
   Creates count × count × count instances."
  (lambda (t)
          (let ([sdf (elem t)]
                [half (* spacing (/ (- count 1) 2.0))])
               (lambda (p)
                       (let ([px (vec3-x p)] [py (vec3-y p)] [pz (vec3-z p)])
                            ;; Use modulo to repeat space
                            (let* ([qx (- (mod (+ px half) spacing) (/ spacing 2.0))]
                                   [qy (- (mod (+ py half) spacing) (/ spacing 2.0))]
                                   [qz (- (mod (+ pz half) spacing) (/ spacing 2.0))]
                                   [q (vec3 qx qy qz)])
                                  (sdf q)))))))

(define (mod a b)
  "Modulo that works for negative numbers."
  (let ([r (remainder a b)])
       (if (negative? r) (+ r b) r)))

(define (grid . args)
  "Repeat an element in a grid.
   Keyword args: :element, :spacing, :count
   Example: (grid :element (ball :r 0.1 :at '(0 0 0))
                  :spacing 0.5 :count 5)"
  (let* ([elem (get-keyword-arg args ':element (ball :r 0.1 :at '(0 0 0)))]
         [spacing (get-keyword-arg args ':spacing 1.0)]
         [count (get-keyword-arg args ':count 3)])
        (make-grid-repeat elem spacing count)))

;;; ============================================================
;;; 3D Asset Loading (OBJ Format)
;;; ============================================================
;;;
;;; OBJ is the simplest widely-used 3D format - plain text, easy to parse.
;;; We load vertices and faces, then compute signed distance to the mesh.

;;; ------------------------------------------------------------
;;; OBJ Parsing
;;; ------------------------------------------------------------

(define (parse-obj-line line)
  "Parse a single OBJ line. Returns (type . data) or #f."
  (let ([trimmed (string-trim line)])
       (cond
        [(or (string=? trimmed "") (char=? (string-ref trimmed 0) #\#))
         #f]  ; Comment or empty
        [(string-prefix? "v " trimmed)
         (cons 'vertex (parse-vertex trimmed))]
        [(string-prefix? "f " trimmed)
         (cons 'face (parse-face trimmed))]
        [else #f])))

(define (string-prefix? prefix str)
  "Check if string starts with prefix."
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

(define (string-trim s)
  "Trim whitespace from both ends."
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                     (if (and (< i len) (char-whitespace? (string-ref s i)))
                         (loop (+ i 1))
                         i))]
         [end (let loop ([i len])
                   (if (and (> i start) (char-whitespace? (string-ref s (- i 1))))
                       (loop (- i 1))
                       i))])
        (substring s start end)))

(define (split-string str delim)
  "Split string by delimiter character."
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      result
                      (cons (list->string (reverse current)) result)))]
        [(char=? (car chars) delim)
         (loop (cdr chars)
               '()
               (if (null? current)
                   result
                   (cons (list->string (reverse current)) result)))]
        [else
         (loop (cdr chars)
               (cons (car chars) current)
               result)])))

(define (parse-vertex line)
  "Parse 'v x y z' line."
  (let* ([parts (split-string line #\space)]
         [nums (filter (lambda (s) (not (string=? s ""))) (cdr parts))])
        (if (>= (length nums) 3)
            (vec3 (string->number (car nums))
                  (string->number (cadr nums))
                  (string->number (caddr nums)))
            (vec3 0 0 0))))

(define (parse-face line)
  "Parse 'f v1 v2 v3 ...' line. Handles 'v', 'v/t', 'v/t/n', 'v//n' formats."
  (let* ([parts (split-string line #\space)]
         [indices (filter (lambda (s) (not (string=? s ""))) (cdr parts))])
        ;; Extract vertex index (before first /)
        (map (lambda (idx-str)
                     (let ([slash-pos (string-index idx-str #\/)])
                          (if slash-pos
                              (- (string->number (substring idx-str 0 slash-pos)) 1)
                              (- (string->number idx-str) 1))))
             indices)))

(define (string-index str char)
  "Find first index of char in string, or #f."
  (let loop ([i 0])
       (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) char) i]
        [else (loop (+ i 1))])))

(define (filter pred lst)
  "Filter list by predicate."
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
   [else (filter pred (cdr lst))]))

(define (load-obj path)
  "Load an OBJ file. Returns (vertices . faces) where:
   - vertices is a vector of vec3
   - faces is a list of index lists (triangles/quads)"
  (display (string-append "Loading OBJ: " path "\n"))
  (let ([vertices '()]
        [faces '()])
       (call-with-input-file path
                             (lambda (port)
                                     (let loop ()
                                          (let ([line (get-line port)])
                                               (unless (eof-object? line)
                                                       (let ([parsed (parse-obj-line line)])
                                                            (when parsed
                                                                  (case (car parsed)
                                                                        [(vertex) (set! vertices (cons (cdr parsed) vertices))]
                                                                        [(face) (set! faces (cons (cdr parsed) faces))])))
                                                       (loop))))))
       ;; Convert to vectors for O(1) access
       (let ([vert-vec (list->vector (reverse vertices))])
            (display (string-append "  " (number->string (vector-length vert-vec)) " vertices, "
                                    (number->string (length faces)) " faces\n"))
            (cons vert-vec (reverse faces)))))

;;; ------------------------------------------------------------
;;; Mesh SDF Computation
;;; ------------------------------------------------------------

(define (triangle-sdf p v0 v1 v2)
  "Compute unsigned distance from point p to triangle (v0, v1, v2).
   Uses the closest point on triangle method."
  (let* ([e0 (vec3-sub v1 v0)]   ; Edge 0-1
         [e1 (vec3-sub v2 v0)]   ; Edge 0-2
         [e2 (vec3-sub v2 v1)]   ; Edge 1-2
         [v (vec3-sub p v0)]
         
         ;; Compute barycentric coordinates
         [d00 (vec3-dot e0 e0)]
         [d01 (vec3-dot e0 e1)]
         [d11 (vec3-dot e1 e1)]
         [d20 (vec3-dot v e0)]
         [d21 (vec3-dot v e1)]
         [denom (- (* d00 d11) (* d01 d01))]
         
         [s (/ (- (* d11 d20) (* d01 d21)) denom)]
         [t (/ (- (* d00 d21) (* d01 d20)) denom)])
        
        ;; Check if inside triangle
        (if (and (>= s 0) (>= t 0) (<= (+ s t) 1))
            ;; Inside triangle - project to plane
            (let* ([n (vec3-cross e0 e1)]
                   [n-len (vec3-length n)])
                  (if (< n-len 0.0001)
                      +inf.0  ; Degenerate triangle
                      (abs (/ (vec3-dot v n) n-len))))
            ;; Outside triangle - distance to closest edge/vertex
            (min (point-segment-distance p v0 v1)
                 (min (point-segment-distance p v1 v2)
                      (point-segment-distance p v2 v0))))))

(define (point-segment-distance p a b)
  "Distance from point p to line segment a-b."
  (let* ([ab (vec3-sub b a)]
         [ap (vec3-sub p a)]
         [t (/ (vec3-dot ap ab) (vec3-dot ab ab))]
         [t-clamped (max 0 (min 1 t))]
         [closest (vec3-add a (vec3-scale ab t-clamped))])
        (vec3-length (vec3-sub p closest))))

(define (mesh-sdf vertices faces)
  "Create an SDF function from mesh vertices and faces.
   This is O(n) per point - fine for simple meshes."
  (lambda (p)
          (let loop ([remaining faces] [min-dist +inf.0])
               (if (null? remaining)
                   min-dist
                   (let* ([face (car remaining)]
                          [v0 (vector-ref vertices (car face))]
                          [v1 (vector-ref vertices (cadr face))]
                          [v2 (vector-ref vertices (caddr face))]
                          [d (triangle-sdf p v0 v1 v2)])
                         (loop (cdr remaining) (min min-dist d)))))))

;;; ------------------------------------------------------------
;;; OBJ Scene Element
;;; ------------------------------------------------------------

(define (make-mesh-element mesh scale offset axis cycles)
  "Create a rotating mesh element.
   mesh = (vertices . faces) from load-obj
   scale = uniform scale factor
   offset = position offset (vec3)
   axis = rotation axis
   cycles = rotations per loop"
  (let ([vertices (car mesh)]
        [faces (cdr mesh)]
        [rotator (if (> cycles 0)
                     (make-rotator axis cycles)
                     #f)])
       ;; Pre-scale vertices
       (let ([scaled-verts (make-vector (vector-length vertices))])
            (do ([i 0 (+ i 1)])
                ((>= i (vector-length vertices)))
                (let ([v (vector-ref vertices i)])
                     (vector-set! scaled-verts i
                                  (vec3-add offset
                                            (vec3-scale v scale)))))
            (let ([base-sdf (mesh-sdf scaled-verts faces)])
                 (if rotator
                     (lambda (t)
                             (let ([rot-fn (rotator t)])
                                  (lambda (p)
                                          (base-sdf (rot-fn p)))))
                     (lambda (t)
                             base-sdf))))))

(define (mesh . args)
  "Load and use an OBJ mesh in the scene.
   Keyword args: :path, :scale, :at, :axis, :cycles
   Example: (mesh :path \"models/teapot.obj\" :scale 0.5 :axis '(0 1 0) :cycles 2)"
  (let* ([path (get-keyword-arg args ':path #f)]
         [scale (get-keyword-arg args ':scale 1.0)]
         [at (get-keyword-arg args ':at '(0 0 0))]
         [axis (get-keyword-arg args ':axis '(0 1 0))]
         [cycles (get-keyword-arg args ':cycles 0)])
        (unless path
                (error 'mesh "path is required"))
        (let ([loaded (load-obj path)])
             (make-mesh-element loaded scale (apply vec3 at) (apply vec3 axis) cycles))))

;;; ------------------------------------------------------------
;;; Simple Mesh Generators (for testing without files)
;;; ------------------------------------------------------------

(define (make-icosahedron)
  "Generate an icosahedron mesh (20 faces, 12 vertices).
   Returns (vertices . faces)."
  (let* ([phi (/ (+ 1 (sqrt 5)) 2)]  ; Golden ratio
         [a 1.0]
         [b phi]
         [vertices (vector
                    (vec3 0 a b) (vec3 0 (- a) b) (vec3 0 a (- b)) (vec3 0 (- a) (- b))
                    (vec3 a b 0) (vec3 (- a) b 0) (vec3 a (- b) 0) (vec3 (- a) (- b) 0)
                    (vec3 b 0 a) (vec3 (- b) 0 a) (vec3 b 0 (- a)) (vec3 (- b) 0 (- a)))]
         [faces '((0 1 8) (0 8 4) (0 4 5) (0 5 9) (0 9 1)
                  (1 9 7) (1 7 6) (1 6 8) (8 6 10) (8 10 4)
                  (4 10 2) (4 2 5) (5 2 11) (5 11 9) (9 11 7)
                  (3 2 10) (3 10 6) (3 6 7) (3 7 11) (3 11 2))])
        (cons vertices faces)))

(define (make-cube-mesh)
  "Generate a cube mesh (12 faces = 6 quads → 12 triangles)."
  (let* ([s 0.5]
         [vertices (vector
                    (vec3 (- s) (- s) (- s)) (vec3 s (- s) (- s))
                    (vec3 s s (- s)) (vec3 (- s) s (- s))
                    (vec3 (- s) (- s) s) (vec3 s (- s) s)
                    (vec3 s s s) (vec3 (- s) s s))]
         [faces '((0 2 1) (0 3 2)   ; Back
                  (4 5 6) (4 6 7)   ; Front
                  (0 1 5) (0 5 4)   ; Bottom
                  (2 3 7) (2 7 6)   ; Top
                  (0 4 7) (0 7 3)   ; Left
                  (1 2 6) (1 6 5))] ; Right
         )
        (cons vertices faces)))

(define (icosahedron . args)
  "A rotating icosahedron (Platonic solid with 20 faces).
   Keyword args: :scale, :at, :axis, :cycles"
  (let* ([scale (get-keyword-arg args ':scale 1.0)]
         [at (get-keyword-arg args ':at '(0 0 0))]
         [axis (get-keyword-arg args ':axis '(0 1 0))]
         [cycles (get-keyword-arg args ':cycles 1)]
         [mesh (make-icosahedron)])
        (make-mesh-element mesh scale (apply vec3 at) (apply vec3 axis) cycles)))

(define (cube-mesh . args)
  "A rotating cube mesh (as triangles, not SDF box).
   Keyword args: :scale, :at, :axis, :cycles"
  (let* ([scale (get-keyword-arg args ':scale 1.0)]
         [at (get-keyword-arg args ':at '(0 0 0))]
         [axis (get-keyword-arg args ':axis '(0 1 0))]
         [cycles (get-keyword-arg args ':cycles 1)]
         [mesh (make-cube-mesh)])
        (make-mesh-element mesh scale (apply vec3 at) (apply vec3 axis) cycles)))

;;; ============================================================
;;; More Preset Scenes
;;; ============================================================

(define (lissajous-scene)
  "Sphere tracing a Lissajous curve."
  (build-loop-scene
   `((duration . 4.0) (fps . 30) (width . 80) (height . 35)
     (camera . ,(look-at '(0 0 -4) '(0 0 0))))
   (tracer :r 0.25 :a 3 :b 2 :scale 1.2 :cycles 1)))

(define (morph-demo-scene)
  "Sphere morphing to box and back."
  (build-loop-scene
   `((duration . 4.0) (fps . 30) (width . 80) (height . 35)
     (camera . ,(look-at '(2 1.5 -3) '(0 0 0))))
   (morph (ball :r 0.8 :at '(0 0 0))
          (box :size '(0.6 0.6 0.6) :axis '(1 1 1) :cycles 2)
          :cycles 1)))

(define (blob-scene)
  "Breathing metaballs."
  (build-loop-scene
   `((duration . 3.0) (fps . 30) (width . 80) (height . 35)
     (camera . ,(look-at '(0 0 -4) '(0 0 0))))
   (blob :positions '((0.6 0 0) (-0.6 0 0) (0 0.5 0.3) (0 -0.5 0.3))
         :r 0.35 :k-base 0.4 :k-range 0.2 :cycles 2)))

(define (orbiting-camera-scene)
  "Static scene with orbiting camera."
  (build-loop-scene-animated-camera
   '((duration . 5.0) (fps . 30) (width . 80) (height . 35))
   (make-orbiting-camera 4 1.5 '(0 0 0) 1)
   (ring :R 1.0 :r 0.15 :axis '(0 1 0) :cycles 2)
   (ball :r 0.4 :at '(0 0 0))))

(define (dna-helix-scene)
  "Two helical tracers like DNA strands."
  (build-loop-scene
   `((duration . 4.0) (fps . 30) (width . 80) (height . 40)
     (camera . ,(look-at* '(3 0 -3) '(0 0 0) 1.2)))
   (helix-tracer :r 0.12 :radius 0.8 :height 3.0 :turns 2 :cycles 1)
   ;; Second strand offset by π
   (lambda (t)
           (let* ([path (make-spiral-path 0.8 3.0 2 1)]
                  [pos (path (+ t 3.14159))])  ; Offset by π for double helix
                 (lambda (p)
                         (sdf-sphere p pos 0.12))))))

(define (icosahedron-scene)
  "Rotating icosahedron (20-sided Platonic solid)."
  (build-loop-scene
   `((duration . 4.0) (fps . 30) (width . 80) (height . 35)
     (camera . ,(look-at '(0 0 -4) '(0 0 0))))
   (icosahedron :scale 0.8 :axis '(1 1 0.5) :cycles 2)))

;;; Show help on load
(loop-scene-help)

(display "Type (loop-scene-help) to see this again.\n\n")

(display "Additional scene presets:\n")
(display "  (lissajous-scene)        ; Sphere on figure-8 path\n")
(display "  (morph-demo-scene)       ; Sphere ↔ box morph\n")
(display "  (blob-scene)             ; Breathing metaballs\n")
(display "  (orbiting-camera-scene)  ; Camera orbits static scene\n")
(display "  (dna-helix-scene)        ; Double helix tracers\n")
(display "  (icosahedron-scene)      ; Rotating 20-sided Platonic solid\n\n")

(display "Mesh elements (built-in or load OBJ files):\n")
(display "  (icosahedron :scale 0.8 :axis '(1 1 0) :cycles 2)\n")
(display "  (cube-mesh :scale 1.0 :axis '(0 1 0) :cycles 3)\n")
(display "  (mesh :path \"model.obj\" :scale 0.5 :cycles 2)\n\n")
