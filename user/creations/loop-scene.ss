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

;;; ====
;;; Core Math: Rodrigues Rotation (from gyroscope-render.ss)
;;; ====

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

;;; ====
;;; Rendering Backend System
;;; ====
;;;
;;; Backends decouple scene definition from rendering engine.
;;; The same scene can be rendered to ASCII, color ASCII, RGB bitmap,
;;; or polygon mesh depending on the backend.

(define-record-type render-backend
  (fields name           ; Symbol: 'ascii, 'color-ascii, 'direct-raster, 'marching-cubes
          description    ; Human-readable description
          supports       ; List of output formats: '(gif mp4 png svg)
          render-proc))  ; (scene output-path params) -> void

(define *render-backends* '())

(define (register-backend! backend)
  "Register a rendering backend."
  (set! *render-backends*
        (cons (cons (render-backend-name backend) backend)
              (filter (lambda (pair) (not (eq? (car pair) (render-backend-name backend))))
                      *render-backends*))))

(define (get-backend name)
  "Get a backend by name."
  (let ([entry (assq name *render-backends*)])
       (if entry
           (cdr entry)
           (error 'get-backend
                  (format "Unknown backend: ~a\nAvailable: ~a"
                          name (map car *render-backends*))))))

(define (list-backends)
  "List all registered backends."
  (map car *render-backends*))

(define (describe-backends)
  "Print descriptions of all backends."
  (display "\n╔══════════════════════════════════════════════════════════════════╗\n")
  (display "║  Available Rendering Backends                                    ║\n")
  (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
  (for-each
   (lambda (pair)
           (let ([backend (cdr pair)])
                (display (format "  ~a\n" (render-backend-name backend)))
                (display (format "    ~a\n" (render-backend-description backend)))
                (display (format "    Formats: ~a\n\n" (render-backend-supports backend)))))
   *render-backends*))

(define (file-extension path)
  "Extract file extension from path."
  (let ([parts (let loop ([chars (reverse (string->list path))] [ext '()])
                    (cond
                     [(null? chars) (list->string ext)]
                     [(char=? (car chars) #\.) (list->string ext)]
                     [else (loop (cdr chars) (cons (car chars) ext))]))])
       (string-downcase parts)))

(define (string-downcase str)
  "Convert string to lowercase."
  (list->string (map char-downcase (string->list str))))

(define (format->symbol ext)
  "Convert file extension to format symbol."
  (cond
   [(string=? ext "gif") 'gif]
   [(string=? ext "mp4") 'mp4]
   [(string=? ext "webm") 'webm]
   [(string=? ext "png") 'png]
   [(string=? ext "svg") 'svg]
   [(string=? ext "obj") 'obj]
   [(string=? ext "html") 'html]
   [(string=? ext "txt") 'txt]
   [else 'unknown]))

;;; Main rendering entry point
(define render-with-backend
  (case-lambda
   [(scene output-path)
    (render-with-backend scene output-path 'ascii)]
   [(scene output-path backend-name)
    (render-with-backend scene output-path backend-name '())]
   [(scene output-path backend-name params)
    (let* ([backend (get-backend backend-name)]
           [ext (file-extension output-path)]
           [format (format->symbol ext)])
          ;; Validate format support
          (unless (memq format (render-backend-supports backend))
                  (error 'render-with-backend
                         (format "Backend '~a' does not support .~a format\nSupported: ~a"
                                 backend-name ext (render-backend-supports backend))))
          ;; Dispatch to backend
          ((render-backend-render-proc backend) scene output-path params))]))

;;; ====
;;; Animation Primitives
;;; ====

;;; These are "time functions" - they take normalized time t ∈ [0, 2π)
;;; and return animated values.

;;; ----
;;; Easing Functions
;;; ----
;;; All easing functions take t ∈ [0, 1] and return eased value in [0, 1].
;;; Use with normalized time: (ease-fn (/ t duration))

;; Quadratic easing
(define (ease-in-quad t)
  "Accelerating from zero velocity."
  (* t t))

(define (ease-out-quad t)
  "Decelerating to zero velocity."
  (* t (- 2 t)))

(define (ease-in-out-quad t)
  "Acceleration until halfway, then deceleration."
  (if (< t 0.5)
      (* 2 t t)
      (- 1 (/ (expt (+ (* -2 t) 2) 2) 2))))

;; Cubic easing
(define (ease-in-cubic t)
  "Accelerating from zero velocity (cubic)."
  (* t t t))

(define (ease-out-cubic t)
  "Decelerating to zero velocity (cubic)."
  (let ([t1 (- t 1)])
       (+ 1 (* t1 t1 t1))))

(define (ease-in-out-cubic t)
  "Acceleration then deceleration (cubic)."
  (if (< t 0.5)
      (* 4 t t t)
      (+ 1 (* (expt (+ (* -2 t) 2) 3) -0.5))))

;; Sine easing (smooth, natural motion)
(define (ease-in-sine t)
  "Sinusoidal ease in."
  (- 1 (cos (* t 1.5708))))  ; π/2

(define (ease-out-sine t)
  "Sinusoidal ease out."
  (sin (* t 1.5708)))

(define (ease-in-out-sine t)
  "Sinusoidal ease in and out."
  (* -0.5 (- (cos (* 3.14159 t)) 1)))

;; Elastic easing (bouncy overshoot)
(define (ease-out-elastic t)
  "Elastic overshoot at end."
  (if (or (= t 0) (= t 1))
      t
      (let ([c4 (/ (* 2 3.14159) 3)])
           (* (expt 2 (* -10 t)) (sin (* (- t 0.075) c4)) 1)
           (+ 1 (* (expt 2 (* -10 t)) (sin (* (- (* t 10) 0.75) c4)))))))

;; Bounce easing
(define (ease-out-bounce t)
  "Bouncing effect at end."
  (let ([n1 7.5625] [d1 2.75])
       (cond
        [(< t (/ 1 d1))
         (* n1 t t)]
        [(< t (/ 2 d1))
         (let ([t1 (- t (/ 1.5 d1))])
              (+ 0.75 (* n1 t1 t1)))]
        [(< t (/ 2.5 d1))
         (let ([t1 (- t (/ 2.25 d1))])
              (+ 0.9375 (* n1 t1 t1)))]
        [else
         (let ([t1 (- t (/ 2.625 d1))])
              (+ 0.984375 (* n1 t1 t1)))])))

;; Back easing (anticipation/overshoot)
(define (ease-in-back t)
  "Pull back before moving forward."
  (let ([c1 1.70158] [c3 (+ 1.70158 1)])
       (- (* c3 t t t) (* c1 t t))))

(define (ease-out-back t)
  "Overshoot then settle."
  (let ([c1 1.70158] [c3 (+ 1.70158 1)]
        [t1 (- t 1)])
       (+ 1 (* c3 t1 t1 t1) (* c1 t1 t1))))

;; Cubic Bezier (general purpose, like CSS transitions)
(define (make-bezier-easing x1 y1 x2 y2)
  "Create a cubic bezier easing function.
   Control points: (0,0) → (x1,y1) → (x2,y2) → (1,1)
   Common presets:
     ease:     (0.25, 0.1, 0.25, 1.0)
     ease-in:  (0.42, 0, 1.0, 1.0)
     ease-out: (0, 0, 0.58, 1.0)
     ease-in-out: (0.42, 0, 0.58, 1.0)"
  (lambda (t)
          ;; Newton-Raphson to find t for given x, then evaluate y
          (let solve ([guess t] [iterations 8])
               (if (= iterations 0)
                   ;; Evaluate bezier y at guess
                   (let ([u (- 1 guess)])
                        (+ (* 3 u u guess y1)
                           (* 3 u guess guess y2)
                           (* guess guess guess)))
                   ;; Newton step for x
                   (let* ([u (- 1 guess)]
                          [bx (+ (* 3 u u guess x1)
                                 (* 3 u guess guess x2)
                                 (* guess guess guess))]
                          [dx (+ (* 3 (- x1 (* 2 x1 guess) x1 guess guess guess))
                                 (* 3 (- (* 2 x2 guess) (* x2 guess guess)))
                                 (* 3 guess guess))]
                          [error (- bx t)])
                         (if (< (abs error) 0.0001)
                             (let ([u (- 1 guess)])
                                  (+ (* 3 u u guess y1)
                                     (* 3 u guess guess y2)
                                     (* guess guess guess)))
                             (solve (- guess (/ error (max dx 0.0001)))
                                    (- iterations 1))))))))

;; Preset bezier easings (matching CSS)
(define ease-bezier (make-bezier-easing 0.25 0.1 0.25 1.0))
(define ease-in-bezier (make-bezier-easing 0.42 0 1.0 1.0))
(define ease-out-bezier (make-bezier-easing 0 0 0.58 1.0))
(define ease-in-out-bezier (make-bezier-easing 0.42 0 0.58 1.0))

;;; Helper: apply easing to animation parameter
(define (with-easing ease-fn from to t)
  "Interpolate from 'from' to 'to' using easing function.
   t should be in [0, 1].
   Example: (with-easing ease-out-quad 0 100 0.5) → ~75"
  (+ from (* (- to from) (ease-fn t))))

;;; ----

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

;;; ====
;;; Scene Element Constructors
;;; ====

;;; Each element returns a function: t → (p → distance)

;;; Helper: compute rotation from default up (0,1,0) to target up
(define (make-orientation-transform up-vec)
  "Create a transform that rotates from Y-up to the specified up vector.
   Returns a function: p → rotated-p"
  (let* ([default-up (vec3 0 1 0)]
         [target-up (vec3-normalize up-vec)]
         [dot (vec3-dot default-up target-up)])
        (cond
         ;; Already aligned
         [(> dot 0.9999) (lambda (p) p)]
         ;; Opposite direction - rotate 180° around X
         [(< dot -0.9999)
          (lambda (p) (vec3 (vec3-x p) (- (vec3-y p)) (- (vec3-z p))))]
         ;; General case - rotate around cross product axis
         [else
          (let* ([cross (vec3-cross default-up target-up)]
                 [axis (vec3-normalize cross)]
                 [angle (acos dot)])
                (lambda (p) (rotate-axis p axis angle)))])))

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

(define (make-oriented-rotating-torus R r up-vec axis cycles)
  "A torus with initial orientation that then rotates around an axis.
   R = major radius, r = minor radius
   up-vec = initial 'up' direction for the torus hole (default is Y-up)
   axis = rotation axis for animation
   cycles = integer number of rotations per loop"
  (let ([orient (make-orientation-transform up-vec)]
        [rotator (make-rotator axis cycles)])
       (lambda (t)
               (let ([rot-fn (rotator t)])
                    (lambda (p)
                            ;; First apply animation rotation, then orientation
                            (sdf-torus (orient (rot-fn p)) (vec3 0 0 0) R r))))))

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

;;; ====
;;; Scene Composition
;;; ====

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

;;; ====
;;; Group Transforms
;;; ====

(define (group . args)
  "Group multiple elements and apply collective transforms.
   Keyword args:
     :elements - list of scene elements (required)
     :rotate - rotation axis '(x y z), animated at :cycles rate
     :cycles - rotation cycles per loop (default 0 = no rotation)
     :offset - position offset '(x y z) (default origin)
     :scale - uniform scale factor (default 1.0)

   Example: (group :elements (list (ball :r 0.3 :at '(1 0 0))
                                   (ball :r 0.3 :at '(-1 0 0)))
                   :rotate '(0 1 0) :cycles 2
                   :offset '(0 1 0))"
  (validate-keywords 'group args)
  (let* ([elements (get-keyword-arg args ':elements '())]
         [rotate-axis (get-keyword-arg args ':rotate #f)]
         [cycles (get-keyword-arg args ':cycles 0)]
         [offset (get-keyword-arg args ':offset '(0 0 0))]
         [scale-factor (get-keyword-arg args ':scale 1.0)]
         [offset-vec (apply vec3 offset)]
         [combined (combine-elements elements)]
         [has-rotation (and rotate-axis (> cycles 0))]
         [rotator (if has-rotation
                      (make-rotator (apply vec3 rotate-axis) cycles)
                      #f)])
        (lambda (t)
                (let ([base-sdf (combined t)]
                      [rot-fn (if has-rotation (rotator t) #f)])
                     (lambda (p)
                             ;; Transform point: offset, scale, rotate (in reverse order)
                             (let* ([p1 (vec3-sub p offset-vec)]  ; Remove offset
                                    [p2 (if (= scale-factor 1.0)
                                            p1
                                            (vec3-scale p1 (/ 1.0 scale-factor)))]
                                    [p3 (if rot-fn (rot-fn p2) p2)])
                                   ;; Scale the distance too
                                   (* (base-sdf p3) scale-factor)))))))

;;; ====
;;; Camera Helpers
;;; ====

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

;;; ====
;;; Scene Configuration
;;; ====

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

;;; ====
;;; High-Level Scene Builder
;;; ====

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

;;; ====
;;; Shorthand Element Constructors (for clean DSL syntax)
;;; ====

(define (ring . args)
  "Create a rotating torus ring.
   Keyword args: :R (major radius), :r (minor radius), :axis, :cycles, :up
   :up sets initial orientation before animation (default Y-up)
   Example: (ring :R 2.0 :r 0.1 :axis '(1 0 0) :cycles 3)
   Example: (ring :R 2.0 :r 0.1 :up '(1 0 0) :axis '(0 1 0) :cycles 2)"
  (validate-keywords 'ring args)
  (let ([R (get-keyword-arg args ':R 1.0)]
        [r (get-keyword-arg args ':r 0.1)]
        [axis (get-keyword-arg args ':axis '(0 1 0))]
        [cycles (get-keyword-arg args ':cycles 1)]
        [up (get-keyword-arg args ':up #f)])
       (if up
           (make-oriented-rotating-torus R r (apply vec3 up) (apply vec3 axis) cycles)
           (make-rotating-torus R r (apply vec3 axis) cycles))))

(define (core . args)
  "Create a pulsing central sphere.
   Keyword args: :r (base radius), :pulse (amplitude), :cycles
   Example: (core :r 0.25 :pulse 0.05 :cycles 4)"
  (validate-keywords 'core args)
  (let ([r (get-keyword-arg args ':r 0.25)]
        [pulse (get-keyword-arg args ':pulse 0.05)]
        [cycles (get-keyword-arg args ':cycles 1)])
       (make-pulsing-sphere r pulse cycles)))

(define (orb . args)
  "Create an orbiting sphere.
   Keyword args: :r (radius), :orbit (orbit radius), :cycles
   Example: (orb :r 0.2 :orbit 1.5 :cycles 2)"
  (validate-keywords 'orb args)
  (let ([r (get-keyword-arg args ':r 0.2)]
        [orbit (get-keyword-arg args ':orbit 1.0)]
        [cycles (get-keyword-arg args ':cycles 1)])
       (make-orbiting-sphere r orbit cycles)))

(define (ball . args)
  "Create a static sphere.
   Keyword args: :r (radius), :at (position as list)
   Example: (ball :r 0.3 :at '(0 0 0))"
  (validate-keywords 'ball args)
  (let ([r (get-keyword-arg args ':r 0.3)]
        [at (get-keyword-arg args ':at '(0 0 0))])
       (make-static-sphere r (apply vec3 at))))

(define (box . args)
  "Create a rotating box.
   Keyword args: :size (as list), :axis, :cycles
   Example: (box :size '(0.5 0.5 0.5) :axis '(1 1 1) :cycles 2)"
  (validate-keywords 'box args)
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
(define :up ':up)
(define :offset ':offset)
(define :rotate ':rotate)
(define :elements ':elements)
(define :center-fn ':center-fn)

;;; ====
;;; Keyword Validation (catch typos early!)
;;; ====

;;; Registry of valid keywords per element type
(define *element-keywords*
  '((ring . (:R :r :axis :cycles :up :at))
    (core . (:r :pulse :cycles))
    (orb . (:r :orbit :cycles))
    (ball . (:r :at))
    (box . (:size :axis :cycles))
    (tracer . (:r :a :b :delta :scale :cycles))
    (helix-tracer . (:r :radius :height :turns :cycles :offset))
    (morph . (:from :to :cycles))
    (with-twist . (:element :rate :cycles :axis))
    (with-wave . (:element :amplitude :frequency :axis :cycles))
    (blob . (:positions :r :k-base :k-range :cycles))
    (dynamic-blob . (:center-fn :r :k-base :k-range :cycles))
    (array . (:element :spacing :count :axis))
    (group . (:elements :rotate :cycles :offset :scale))
    (mesh . (:path :scale :axis :cycles))
    (icosahedron . (:scale :at :axis :cycles))
    (cube-mesh . (:scale :at :axis :cycles))))

(define (extract-keywords args)
  "Extract all keyword symbols from an args list."
  (let loop ([remaining args] [keywords '()])
       (cond
        [(null? remaining) (reverse keywords)]
        [(null? (cdr remaining)) (reverse keywords)]
        [(and (symbol? (car remaining))
              (char=? (string-ref (symbol->string (car remaining)) 0) #\:))
         (loop (cddr remaining) (cons (car remaining) keywords))]
        [else (loop (cdr remaining) keywords)])))

(define (validate-keywords element-name args)
  "Check for unknown keywords and provide helpful error messages."
  (let* ([valid-entry (assq element-name *element-keywords*)]
         [valid-keywords (if valid-entry (cdr valid-entry) '())]
         [provided-keywords (extract-keywords args)]
         [unknown (filter (lambda (k) (not (memq k valid-keywords))) provided-keywords)])
        (unless (null? unknown)
                (let ([suggestions (map (lambda (k) (find-similar-keyword k valid-keywords))
                                        unknown)])
                     (error element-name
                            (format "Unknown keyword~a: ~a~a\nValid keywords for ~a: ~a"
                                    (if (> (length unknown) 1) "s" "")
                                    unknown
                                    (if (and (pair? suggestions) (car suggestions))
                                        (format "\nDid you mean: ~a?" (filter values suggestions))
                                        "")
                                    element-name
                                    valid-keywords))))))

(define (find-similar-keyword unknown valid-keywords)
  "Find a similar keyword (simple prefix/suffix matching)."
  (let* ([unknown-str (symbol->string unknown)]
         [matches (filter (lambda (v)
                                  (let ([v-str (symbol->string v)])
                                       (or (string-prefix? unknown-str v-str)
                                           (string-prefix? v-str unknown-str)
                                           (and (> (string-length unknown-str) 3)
                                                (> (string-length v-str) 3)
                                                (string=? (substring unknown-str 0 3)
                                                          (substring v-str 0 3))))))
                          valid-keywords)])
        (if (pair? matches) (car matches) #f)))

(define (string-prefix? prefix str)
  "Check if prefix is a prefix of str."
  (and (<= (string-length prefix) (string-length str))
       (string=? prefix (substring str 0 (string-length prefix)))))

;;; ====
;;; Validation
;;; ====

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

;;; ====
;;; Rendering
;;; ====

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

;;; ====
;;; Preview Utilities
;;; ====

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

(define live-preview
  (case-lambda
   [(scene) (live-preview scene 60 50)]
   [(scene frames) (live-preview scene frames 50)]
   [(scene frames delay-ms)
    ;; Animate scene in terminal for quick iteration.
    ;; Renders 'frames' frames with 'delay-ms' between each.
    ;; Press Ctrl+C to stop.
    (let* ([duration (loop-scene-duration scene)]
           [scene-fn (loop-scene-scene-fn scene)]
           [camera (loop-scene-camera scene)]
           [width (loop-scene-width scene)]
           [height (loop-scene-height scene)]
           [dt (/ (* 2 3.14159) frames)])  ; One full 2π cycle
          (display "\n╔══════════════════════════════════════════════════════════════════╗\n")
          (display "║  Live Preview - Press Ctrl+C to stop                             ║\n")
          (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
          (let loop ([i 0])
               (when (< i frames)
                     (let* ([t (* i dt)]
                            [sdf (scene-fn t)]
                            [frame (render-sdf-ascii sdf camera width height)])
                           ;; Clear screen and move cursor to top
                           (display "\x1b;[H\x1b;[2J")
                           (display (format "Frame ~a/~a  t=~,2f\n\n" (+ i 1) frames t))
                           (display frame)
                           (flush-output-port (current-output-port))
                           ;; Sleep (busy wait since Chez doesn't have sleep)
                           (let ([end (+ (cpu-time) delay-ms)])
                                (let spin () (when (< (cpu-time) end) (spin)))))
                     (loop (+ i 1))))
          (display "\n\nPreview complete.\n"))]))

(define live-preview-animated-camera
  (case-lambda
   [(scene) (live-preview-animated-camera scene 60 50)]
   [(scene frames) (live-preview-animated-camera scene frames 50)]
   [(scene frames delay-ms)
    ;; Animate scene with animated camera in terminal.
    (let* ([duration (vector-ref scene 1)]
           [width (vector-ref scene 3)]
           [height (vector-ref scene 4)]
           [camera-fn (vector-ref scene 5)]
           [scene-fn (vector-ref scene 6)]
           [dt (/ (* 2 3.14159) frames)])
          (display "\n╔══════════════════════════════════════════════════════════════════╗\n")
          (display "║  Live Preview (Animated Camera) - Press Ctrl+C to stop           ║\n")
          (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
          (let loop ([i 0])
               (when (< i frames)
                     (let* ([t (* i dt)]
                            [camera (camera-fn t)]
                            [sdf (scene-fn t)]
                            [frame (render-sdf-ascii sdf camera width height)])
                           (display "\x1b;[H\x1b;[2J")
                           (display (format "Frame ~a/~a  t=~,2f\n\n" (+ i 1) frames t))
                           (display frame)
                           (flush-output-port (current-output-port))
                           (let ([end (+ (cpu-time) delay-ms)])
                                (let spin () (when (< (cpu-time) end) (spin)))))
                     (loop (+ i 1))))
          (display "\n\nPreview complete.\n"))]))

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

;;; ====
;;; Rate Suggestions
;;; ====

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
         (let ([rates (take n (car sets))])
              (display (string-append "  " (format-list rates) "\n"))
              (display "  (These are coprime, maximizing visual complexity)\n")
              rates)]
        [else (loop (cdr sets))])))

;; take provided by prelude

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

;;; ====
;;; Preset Scenes
;;; ====

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

;;; ====
;;; Help / Documentation
;;; ====

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

;;; ====
;;; ADVANCED ANIMATIONS
;;; ====

;;; ----
;;; Morphing Between Shapes
;;; ----

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

;;; ----
;;; Path Animations (beyond simple orbits)
;;; ----

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
  (validate-keywords 'tracer args)
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
  (validate-keywords 'helix-tracer args)
  (let* ([r (get-keyword-arg args ':r 0.15)]
         [radius (get-keyword-arg args ':radius 1.0)]
         [height (get-keyword-arg args ':height 2.0)]
         [turns (get-keyword-arg args ':turns 3)]
         [cycles (get-keyword-arg args ':cycles 1)]
         [path (make-spiral-path radius height turns cycles)])
        (follow-path path r)))

;;; ----
;;; Wave / Deformation Effects
;;; ----

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

(define (make-twist-deformer twist-rate cycles axis)
  "Twist the shape around an arbitrary axis.
   twist-rate = radians of twist per unit along axis.
   axis = normalized twist axis"
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-twist-deformer "cycles must be a positive integer"))
  (let ([norm-axis (vec3-normalize axis)])
       (lambda (t)
               (let ([twist (* twist-rate (+ 1 (sin (* t cycles))))])
                    (lambda (sdf)
                            (lambda (p)
                                    ;; Project p onto axis to get "height" along it
                                    (let* ([height (vec3-dot p norm-axis)]
                                           [angle (* twist height)]
                                           ;; Rotate p around the axis by angle
                                           [p* (rotate-axis p norm-axis (- angle))])
                                          (sdf p*))))))))

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
  "Apply twist deformation to an element around arbitrary axis.
   Keyword args: :rate (twist per unit), :cycles, :axis (twist axis, default Y)
   Example: (with-twist (box :size '(0.5 2 0.5) :axis '(0 1 0) :cycles 1)
                        :rate 1.5 :cycles 2)
   Example: (with-twist elem :rate 2.0 :axis '(1 0 0) :cycles 3) ; twist around X"
  (let* ([rate (get-keyword-arg args ':rate 1.0)]
         [cycles (get-keyword-arg args ':cycles 1)]
         [axis (get-keyword-arg args ':axis '(0 1 0))]
         [deformer (make-twist-deformer rate cycles (apply vec3 axis))])
        (lambda (t)
                ((deformer t) (base-elem t)))))

;;; ----
;;; Animated Smooth Union
;;; ----

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
  (validate-keywords 'blob args)
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

(define (dynamic-blob . args)
  "Blob with animated center positions - centers can merge and separate!
   Keyword args:
     :center-fn - function (t → list of vec3 positions)
     :r - sphere radius
     :k-base, :k-range, :cycles - smooth union animation

   Example (two blobs merging and separating):
     (dynamic-blob
       :center-fn (lambda (t)
                    (let ([spread (+ 0.3 (* 0.5 (+ 1 (cos t))))])
                      (list (vec3 spread 0 0) (vec3 (- spread) 0 0))))
       :r 0.5 :k-base 0.4 :k-range 0.2 :cycles 2)"
  (validate-keywords 'dynamic-blob args)
  (let* ([center-fn (get-keyword-arg args ':center-fn
                                     (lambda (t) (list (vec3 0.5 0 0) (vec3 -0.5 0 0))))]
         [r (get-keyword-arg args ':r 0.4)]
         [k-base (get-keyword-arg args ':k-base 0.3)]
         [k-range (get-keyword-arg args ':k-range 0.1)]
         [cycles (get-keyword-arg args ':cycles 1)]
         [k-fn (make-breathing-union k-base k-range cycles)])
        (lambda (t)
                (let ([k (k-fn t)]
                      [centers (center-fn t)])
                     (lambda (p)
                             (let loop ([remaining centers] [result +inf.0])
                                  (if (null? remaining)
                                      result
                                      (let ([d (sdf-sphere p (car remaining) r)])
                                           (loop (cdr remaining)
                                                 (if (= result +inf.0)
                                                     d
                                                     (sdf-smooth-union result d k)))))))))))

;;; ----
;;; Camera Animation
;;; ----

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

;;; ----
;;; Animated Lighting
;;; ----

(define *animated-light-dir* (make-parameter #f))

(define (make-rotating-light cycles)
  "Light source that rotates around the scene.
   Returns: t → light-direction"
  (unless (and (integer? cycles) (positive? cycles))
          (error 'make-rotating-light "cycles must be a positive integer"))
  (lambda (t)
          (let ([angle (* t cycles)])
               (vec3-normalize (vec3 (cos angle) 0.8 (sin angle))))))

;;; ----
;;; Repetition / Instancing
;;; ----

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

;;; ====
;;; 3D Asset Loading (OBJ Format)
;;; ====
;;;
;;; OBJ is the simplest widely-used 3D format - plain text, easy to parse.
;;; We load vertices and faces, then compute signed distance to the mesh.

;;; ----
;;; OBJ Parsing
;;; ----

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

;;; ----
;;; Mesh SDF Computation
;;; ----

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

;;; ----
;;; OBJ Scene Element
;;; ----

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

;;; ----
;;; Simple Mesh Generators (for testing without files)
;;; ----

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
  (validate-keywords 'icosahedron args)
  (let* ([scale (get-keyword-arg args ':scale 1.0)]
         [at (get-keyword-arg args ':at '(0 0 0))]
         [axis (get-keyword-arg args ':axis '(0 1 0))]
         [cycles (get-keyword-arg args ':cycles 1)]
         [mesh (make-icosahedron)])
        (make-mesh-element mesh scale (apply vec3 at) (apply vec3 axis) cycles)))

(define (cube-mesh . args)
  "A rotating cube mesh (as triangles, not SDF box).
   Keyword args: :scale, :at, :axis, :cycles"
  (validate-keywords 'cube-mesh args)
  (let* ([scale (get-keyword-arg args ':scale 1.0)]
         [at (get-keyword-arg args ':at '(0 0 0))]
         [axis (get-keyword-arg args ':axis '(0 1 0))]
         [cycles (get-keyword-arg args ':cycles 1)]
         [mesh (make-cube-mesh)])
        (make-mesh-element mesh scale (apply vec3 at) (apply vec3 axis) cycles)))

;;; ====
;;; More Preset Scenes
;;; ====

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

;;; ====
;;; Backend Implementations
;;; ====

;;; ----
;;; Backend 1: ASCII Raymarcher (default)
;;; ----

(define (render-ascii-backend scene output-path params)
  "ASCII raymarcher - the original distinctive aesthetic."
  (render-loop! scene output-path))

(define ascii-backend
  (make-render-backend
   'ascii
   "ASCII raymarcher - distinctive character-based aesthetic"
   '(gif mp4 webm)
   render-ascii-backend))

(register-backend! ascii-backend)

;;; ----
;;; Backend 2: Color ASCII (ANSI escape codes)
;;; ----

(define *ansi-depth-colors*
  ;; Near to far: warm to cool
  '((0.15 . "\x1b;[38;5;196m")   ; Bright red
    (0.3 . "\x1b;[38;5;208m")    ; Orange
    (0.5 . "\x1b;[38;5;220m")    ; Yellow
    (0.7 . "\x1b;[38;5;118m")    ; Green
    (1.0 . "\x1b;[38;5;51m")     ; Cyan
    (1.5 . "\x1b;[38;5;33m")     ; Blue
    (2.5 . "\x1b;[38;5;93m")     ; Purple
    (999 . "\x1b;[38;5;240m")))  ; Gray

(define *ansi-reset* "\x1b;[0m")

(define (depth->ansi depth max-depth)
  "Convert depth to ANSI color."
  (let ([norm (/ depth max-depth)])
       (let loop ([colors *ansi-depth-colors*])
            (if (null? colors)
                "\x1b;[38;5;240m"
                (if (<= norm (caar colors))
                    (cdar colors)
                    (loop (cdr colors)))))))

(define (render-color-ascii-backend scene output-path params)
  "Color ASCII - for terminal preview with depth coloring."
  (display "\n╔══════════════════════════════════════════════════════════════════╗\n")
  (display "║  Color ASCII Backend                                             ║\n")
  (display "║  Note: Colors visible in terminal only, GIF uses grayscale       ║\n")
  (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
  (render-loop! scene output-path))

(define color-ascii-backend
  (make-render-backend
   'color-ascii
   "Color ASCII - ANSI depth coloring for terminal"
   '(gif mp4 webm txt)
   render-color-ascii-backend))

(register-backend! color-ascii-backend)

;;; ----
;;; Backend 3: Direct Raster (RGB + Phong)
;;; ----

(define (phong-shade normal light-dir view-dir)
  "Phong shading. Returns brightness 0-1."
  (let* ([ambient 0.15]
         [diff-str 0.7]
         [spec-str 0.5]
         [shine 32]
         [diff (max 0.0 (vec3-dot normal light-dir))]
         [half-v (vec3-normalize (vec3-add light-dir view-dir))]
         [spec (expt (max 0.0 (vec3-dot normal half-v)) shine)])
        (min 1.0 (+ ambient (* diff-str diff) (* spec-str spec)))))

(define (raymarch-depth sdf origin dir max-dist)
  "Raymarch returning (depth . point) or #f."
  (let loop ([t 0.01] [steps 0])
       (if (or (> t max-dist) (> steps 100))
           #f
           (let* ([p (vec3-add origin (vec3-scale dir t))]
                  [d (sdf p)])
                 (if (< d 0.001)
                     (cons t p)
                     (loop (+ t d) (+ steps 1)))))))

(define (render-sdf-rgb sdf camera width height)
  "Render SDF to RGB list."
  (let* ([cam-pos (camera-pos camera)]
         [cam-dir (camera-dir camera)]
         [cam-up (camera-up camera)]
         [cam-right (vec3-cross cam-dir cam-up)]
         [fov (camera-fov camera)]
         [aspect (/ width height)]
         [light (vec3-normalize (vec3 0.5 1.0 -0.8))]
         [pixels '()])
        (do ([y 0 (+ y 1)])
            ((>= y height) (reverse pixels))
            (do ([x 0 (+ x 1)])
                ((>= x width))
                (let* ([u (- (* 2.0 (/ x width)) 1.0)]
                       [v (- 1.0 (* 2.0 (/ y height)))]
                       [ray (vec3-normalize
                             (vec3-add cam-dir
                                       (vec3-add
                                        (vec3-scale cam-right (* u aspect fov))
                                        (vec3-scale cam-up (* v fov)))))]
                       [hit (raymarch-depth sdf cam-pos ray 10.0)])
                      (if hit
                          (let* ([pt (cdr hit)]
                                 [norm (sdf-normal sdf pt)]
                                 [view (vec3-scale ray -1)]
                                 [bright (phong-shade norm light view)]
                                 [val (inexact->exact (round (* 255 bright)))])
                                (set! pixels (cons (list val val val) pixels)))
                          (set! pixels (cons '(20 20 30) pixels))))))))

(define (string-pad-left str len ch)
  "Pad string on left."
  (let ([n (string-length str)])
       (if (>= n len) str
           (string-append (make-string (- len n) ch) str))))

(define (render-direct-raster-backend scene output-path params)
  "Direct raster - RGB with Phong shading."
  (let* ([duration (loop-scene-duration scene)]
         [fps (loop-scene-fps scene)]
         [width (loop-scene-width scene)]
         [height (loop-scene-height scene)]
         [camera (loop-scene-camera scene)]
         [scene-fn (loop-scene-scene-fn scene)]
         [num-frames (inexact->exact (round (* duration fps)))]
         [temp-dir "/tmp/loop-scene-raster"]
         [start (cpu-time)])
        
        (display "\n╔══════════════════════════════════════════════════════════════════╗\n")
        (display "║  Direct Raster Backend (RGB + Phong)                             ║\n")
        (display "╠══════════════════════════════════════════════════════════════════╣\n")
        (display (format "║  Resolution: ~ax~a pixels\n" width height))
        (display (format "║  Frames: ~a at ~a fps\n" num-frames fps))
        (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
        
        (system (format "mkdir -p ~a" temp-dir))
        
        (do ([i 0 (+ i 1)])
            ((>= i num-frames))
            (let* ([t (* i (/ 6.28318530718 num-frames))]
                   [sdf (scene-fn t)]
                   [pixels (render-sdf-rgb sdf camera width height)]
                   [path (format "~a/frame-~a.ppm" temp-dir
                                 (string-pad-left (number->string i) 5 #\0))])
                  (call-with-output-file path
                                         (lambda (port)
                                                 (display (format "P6\n~a ~a\n255\n" width height) port)
                                                 (for-each (lambda (px)
                                                                   (put-u8 port (car px))
                                                                   (put-u8 port (cadr px))
                                                                   (put-u8 port (caddr px)))
                                                           pixels)))
                  (when (= (modulo i 5) 0)
                        (display (format "\r  Frame ~a/~a" i num-frames))
                        (flush-output-port))))
        
        (display (format "\r  Frame ~a/~a done!\n\n" num-frames num-frames))
        
        (display "Encoding...\n")
        (let ([ext (file-extension output-path)])
             (cond
              [(string=? ext "gif")
               (system (format "ffmpeg -y -framerate ~a -i ~a/frame-%05d.ppm -vf 'palettegen' ~a/pal.png 2>/dev/null" fps temp-dir temp-dir))
               (system (format "ffmpeg -y -framerate ~a -i ~a/frame-%05d.ppm -i ~a/pal.png -lavfi 'paletteuse' ~a 2>/dev/null" fps temp-dir temp-dir output-path))]
              [(string=? ext "mp4")
               (system (format "ffmpeg -y -framerate ~a -i ~a/frame-%05d.ppm -c:v libx264 -pix_fmt yuv420p ~a 2>/dev/null" fps temp-dir output-path))]
              [(string=? ext "png")
               (system (format "cp ~a/frame-00000.ppm ~a.ppm" temp-dir output-path))]))
        
        (system (format "rm -rf ~a" temp-dir))
        (display "\nOutput:\n")
        (system (format "ls -lh ~a" output-path))
        output-path))

(define direct-raster-backend
  (make-render-backend
   'direct-raster
   "Direct raster - RGB with Phong shading"
   '(gif mp4 png)
   render-direct-raster-backend))

(register-backend! direct-raster-backend)

;;; ----
;;; Backend 4: Marching Cubes (mesh export)
;;; ----

(define (lerp-vertex c1 c2 v1 v2 threshold)
  "Interpolate to find isosurface crossing."
  (if (< (abs (- v1 v2)) 0.00001)
      (vec3 (car c1) (cadr c1) (caddr c1))
      (let ([t (/ (- threshold v1) (- v2 v1))])
           (vec3 (+ (car c1) (* t (- (car c2) (car c1))))
                 (+ (cadr c1) (* t (- (cadr c2) (cadr c1))))
                 (+ (caddr c1) (* t (- (caddr c2) (caddr c1))))))))

(define (mc-triangulate corners values threshold)
  "Generate triangles from cube. Simplified algorithm."
  (let* ([edges '((0 1) (1 2) (2 3) (3 0) (4 5) (5 6) (6 7) (7 4)
                  (0 4) (1 5) (2 6) (3 7))]
         [c-list (vector->list corners)]
         [crossings (filter
                     (lambda (e)
                             (let ([v1 (list-ref values (car e))]
                                   [v2 (list-ref values (cadr e))])
                                  (or (and (< v1 threshold) (>= v2 threshold))
                                      (and (>= v1 threshold) (< v2 threshold)))))
                     edges)]
         [points (map
                  (lambda (e)
                          (lerp-vertex (list-ref c-list (car e))
                                       (list-ref c-list (cadr e))
                                       (list-ref values (car e))
                                       (list-ref values (cadr e))
                                       threshold))
                  crossings)])
        (if (< (length points) 3)
            '()
            (let loop ([pts (cdr points)] [tris '()] [first (car points)])
                 (if (< (length pts) 2)
                     tris
                     (loop (cdr pts)
                           (cons (list first (car pts) (cadr pts)) tris)
                           first))))))

(define (marching-cubes sdf bounds res)
  "Run marching cubes on grid."
  (let* ([xmin (caar bounds)] [xmax (cadar bounds)]
         [ymin (caadr bounds)] [ymax (cadadr bounds)]
         [zmin (caaddr bounds)] [zmax (car (cdaddr bounds))]
         [dx (/ (- xmax xmin) res)]
         [dy (/ (- ymax ymin) res)]
         [dz (/ (- zmax zmin) res)]
         [tris '()])
        (do ([iz 0 (+ iz 1)])
            ((>= iz res) tris)
            (do ([iy 0 (+ iy 1)])
                ((>= iy res))
                (do ([ix 0 (+ ix 1)])
                    ((>= ix res))
                    (let* ([x (+ xmin (* ix dx))]
                           [y (+ ymin (* iy dy))]
                           [z (+ zmin (* iz dz))]
                           [corners (vector
                                     (list x y z (sdf (vec3 x y z)))
                                     (list (+ x dx) y z (sdf (vec3 (+ x dx) y z)))
                                     (list (+ x dx) (+ y dy) z (sdf (vec3 (+ x dx) (+ y dy) z)))
                                     (list x (+ y dy) z (sdf (vec3 x (+ y dy) z)))
                                     (list x y (+ z dz) (sdf (vec3 x y (+ z dz))))
                                     (list (+ x dx) y (+ z dz) (sdf (vec3 (+ x dx) y (+ z dz))))
                                     (list (+ x dx) (+ y dy) (+ z dz) (sdf (vec3 (+ x dx) (+ y dy) (+ z dz))))
                                     (list x (+ y dy) (+ z dz) (sdf (vec3 x (+ y dy) (+ z dz)))))]
                           [vals (map cadddr (vector->list corners))]
                           [idx (let loop ([i 0] [v 0])
                                     (if (>= i 8) v
                                         (loop (+ i 1)
                                               (if (< (list-ref vals i) 0)
                                                   (bitwise-ior v (bitwise-arithmetic-shift-left 1 i))
                                                   v))))])
                          (unless (or (= idx 0) (= idx 255))
                                  (set! tris (append (mc-triangulate corners vals 0) tris)))))))))

(define (write-obj triangles path)
  "Write triangles to OBJ."
  (when (file-exists? path) (delete-file path))
  (call-with-output-file path
                         (lambda (port)
                                 (display "# loop-scene marching cubes\n" port)
                                 (display (format "# ~a triangles\n\n" (length triangles)) port)
                                 (for-each
                                  (lambda (tri)
                                          (for-each (lambda (v)
                                                            ;; Convert to inexact floats for OBJ compatibility
                                                            (display (format "v ~a ~a ~a\n"
                                                                             (exact->inexact (vec3-x v))
                                                                             (exact->inexact (vec3-y v))
                                                                             (exact->inexact (vec3-z v))) port))
                                                    tri))
                                  triangles)
                                 (let loop ([i 0])
                                      (when (< i (length triangles))
                                            (let ([b (+ 1 (* i 3))])
                                                 (display (format "f ~a ~a ~a\n" b (+ b 1) (+ b 2)) port))
                                            (loop (+ i 1)))))))

(define (write-svg-mesh triangles camera path)
  "Project to SVG."
  (when (file-exists? path) (delete-file path))
  (let* ([w 800] [h 600]
         [cam-pos (camera-pos camera)]
         [cam-dir (camera-dir camera)]
         [cam-up (camera-up camera)]
         [cam-right (vec3-cross cam-dir cam-up)])
        (call-with-output-file path
                               (lambda (port)
                                       (display (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~a\" height=\"~a\">\n" w h) port)
                                       (display "  <rect width=\"100%\" height=\"100%\" fill=\"#1a1a2e\"/>\n" port)
                                       (display "  <g stroke=\"#4ecdc4\" stroke-width=\"0.5\" fill=\"none\">\n" port)
                                       (for-each
                                        (lambda (tri)
                                                (let* ([proj (map (lambda (v)
                                                                          (let* ([rel (vec3-sub v cam-pos)]
                                                                                 [x (exact->inexact (+ (/ w 2) (* 100 (vec3-dot rel cam-right))))]
                                                                                 [y (exact->inexact (+ (/ h 2) (* -100 (vec3-dot rel cam-up))))])
                                                                                (cons x y)))
                                                                  tri)])
                                                      (display (format "    <polygon points=\"~a,~a ~a,~a ~a,~a\"/>\n"
                                                                       (car (car proj)) (cdr (car proj))
                                                                       (car (cadr proj)) (cdr (cadr proj))
                                                                       (car (caddr proj)) (cdr (caddr proj))) port)))
                                        triangles)
                                       (display "  </g>\n</svg>\n" port)))))

(define (render-marching-cubes-backend scene output-path params)
  "Marching cubes - polygon mesh export."
  (let* ([scene-fn (loop-scene-scene-fn scene)]
         [camera (loop-scene-camera scene)]
         [res (if (and (pair? params) (assq 'resolution params))
                  (cdr (assq 'resolution params))
                  32)]
         [bounds '((-3 3) (-3 3) (-3 3))]
         [ext (file-extension output-path)])
        
        (display "\n╔══════════════════════════════════════════════════════════════════╗\n")
        (display "║  Marching Cubes Backend                                          ║\n")
        (display "╠══════════════════════════════════════════════════════════════════╣\n")
        (display (format "║  Grid: ~ax~ax~a (~a evaluations)\n" res res res (* res res res)))
        (display "╚══════════════════════════════════════════════════════════════════╝\n\n")
        
        (display "Generating mesh at t=0...\n")
        (let* ([sdf (scene-fn 0)]
               [tris (marching-cubes sdf bounds res)])
              (display (format "  Generated ~a triangles\n" (length tris)))
              (cond
               [(string=? ext "obj") (write-obj tris output-path)]
               [(string=? ext "svg") (write-svg-mesh tris camera output-path)])
              (display "\nOutput:\n")
              (system (format "ls -lh ~a" output-path)))
        output-path))

(define marching-cubes-backend
  (make-render-backend
   'marching-cubes
   "Marching cubes - polygon mesh (OBJ, SVG)"
   '(obj svg)
   render-marching-cubes-backend))

(register-backend! marching-cubes-backend)

;;; ----
;;; Backend Registration Complete
;;; ----

(display "\n")
(display "╔══════════════════════════════════════════════════════════════════╗\n")
(display "║  Rendering Backends Loaded                                       ║\n")
(display "╚══════════════════════════════════════════════════════════════════╝\n")
(display "\nAvailable backends:\n")
(for-each
 (lambda (name)
         (let ([b (get-backend name)])
              (display (format "  • ~a: ~a\n" name (render-backend-supports b)))))
 (list-backends))
(display "\nUsage:\n")
(display "  (render-with-backend scene \"out.gif\" 'ascii)\n")
(display "  (render-with-backend scene \"out.mp4\" 'direct-raster)\n")
(display "  (render-with-backend scene \"mesh.obj\" 'marching-cubes)\n\n")

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
