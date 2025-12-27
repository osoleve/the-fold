;;; shell/animation.ss — Animation and Easing Functions
;;;
;;; Provides easing functions for smooth motion and transitions.
;;; All functions are pure: (Easing : Real[0,1] → Real)
;;;
;;; This is Shell code: provides animation utilities for rendering.
;;; Easing functions transform linear time into non-linear motion.

;;; ============================================================
;;; Core Easing Type
;;; ============================================================

;;; Easing : Real[0,1] → Real
;;;
;;; An easing function takes a normalized time value t ∈ [0,1]
;;; and returns an eased value (typically ∈ [0,1], but may overshoot).
;;;
;;; Input:  t = 0.0 → start of animation
;;;         t = 0.5 → halfway through
;;;         t = 1.0 → end of animation
;;;
;;; Output: Transformed value showing animation progress

;;; ============================================================
;;; Linear Easing (No Easing)
;;; ============================================================

;;; linear : Easing
;;; No easing — constant velocity.
(define (linear t) t)

;;; ============================================================
;;; Quadratic Easing (Gentle)
;;; ============================================================

;;; ease-in-quad : Easing
;;; Accelerating from zero velocity (t²).
(define (ease-in-quad t)
  (* t t))

;;; ease-out-quad : Easing
;;; Decelerating to zero velocity.
(define (ease-out-quad t)
  (- (* t (- t 2))))

;;; ease-in-out-quad : Easing
;;; Acceleration until halfway, then deceleration.
(define (ease-in-out-quad t)
  (if (< t 0.5)
      (* 2 t t)
      (+ (* -2 t t) (* 4 t) -1)))

;;; ============================================================
;;; Cubic Easing (Medium)
;;; ============================================================

;;; ease-in-cubic : Easing
;;; Accelerating from zero velocity (t³).
(define (ease-in-cubic t)
  (* t t t))

;;; ease-out-cubic : Easing
;;; Decelerating to zero velocity.
(define (ease-out-cubic t)
  (let ([t1 (- t 1)])
    (+ (* t1 t1 t1) 1)))

;;; ease-in-out-cubic : Easing
;;; Acceleration until halfway, then deceleration.
(define (ease-in-out-cubic t)
  (if (< t 0.5)
      (* 4 t t t)
      (let ([t1 (- (* 2 t) 2)])
        (+ (* 0.5 t1 t1 t1) 1))))

;;; ============================================================
;;; Quartic Easing (Strong)
;;; ============================================================

;;; ease-in-quart : Easing
;;; Accelerating from zero velocity (t⁴).
(define (ease-in-quart t)
  (* t t t t))

;;; ease-out-quart : Easing
;;; Decelerating to zero velocity.
(define (ease-out-quart t)
  (let ([t1 (- t 1)])
    (- 1 (* t1 t1 t1 t1))))

;;; ease-in-out-quart : Easing
;;; Acceleration until halfway, then deceleration.
(define (ease-in-out-quart t)
  (if (< t 0.5)
      (* 8 t t t t)
      (let ([t1 (- (* 2 t) 2)])
        (- 1 (* 0.5 t1 t1 t1 t1)))))

;;; ============================================================
;;; Exponential Easing (Very Strong)
;;; ============================================================

;;; ease-in-expo : Easing
;;; Exponential accelerating from zero velocity.
(define (ease-in-expo t)
  (if (= t 0)
      0
      (expt 2 (* 10 (- t 1)))))

;;; ease-out-expo : Easing
;;; Exponential decelerating to zero velocity.
(define (ease-out-expo t)
  (if (= t 1)
      1
      (- 1 (expt 2 (* -10 t)))))

;;; ease-in-out-expo : Easing
;;; Exponential acceleration/deceleration.
(define (ease-in-out-expo t)
  (cond
    [(= t 0) 0]
    [(= t 1) 1]
    [(< t 0.5) (* 0.5 (expt 2 (* 20 t -10)))]
    [else (- 1 (* 0.5 (expt 2 (+ (* -20 t) 10))))]))

;;; ============================================================
;;; Sine Easing (Smooth)
;;; ============================================================

;;; ease-in-sine : Easing
;;; Sinusoidal accelerating from zero velocity.
(define (ease-in-sine t)
  (- 1 (cos (* t 1.5707963267948966))))  ; π/2

;;; ease-out-sine : Easing
;;; Sinusoidal decelerating to zero velocity.
(define (ease-out-sine t)
  (sin (* t 1.5707963267948966)))  ; π/2

;;; ease-in-out-sine : Easing
;;; Sinusoidal acceleration/deceleration.
(define (ease-in-out-sine t)
  (* -0.5 (- (cos (* 3.141592653589793 t)) 1)))  ; π

;;; ============================================================
;;; Bounce Easing (Playful)
;;; ============================================================

;;; ease-out-bounce : Easing
;;; Bounce effect at the end.
(define (ease-out-bounce t)
  (cond
    [(< t (/ 1 2.75))
     (* 7.5625 t t)]
    [(< t (/ 2 2.75))
     (let ([t2 (- t (/ 1.5 2.75))])
       (+ (* 7.5625 t2 t2) 0.75))]
    [(< t (/ 2.5 2.75))
     (let ([t2 (- t (/ 2.25 2.75))])
       (+ (* 7.5625 t2 t2) 0.9375))]
    [else
     (let ([t2 (- t (/ 2.625 2.75))])
       (+ (* 7.5625 t2 t2) 0.984375))]))

;;; ease-in-bounce : Easing
;;; Bounce effect at the start.
(define (ease-in-bounce t)
  (- 1 (ease-out-bounce (- 1 t))))

;;; ease-in-out-bounce : Easing
;;; Bounce at both ends.
(define (ease-in-out-bounce t)
  (if (< t 0.5)
      (* 0.5 (ease-in-bounce (* t 2)))
      (+ (* 0.5 (ease-out-bounce (- (* t 2) 1))) 0.5)))

;;; ============================================================
;;; Elastic Easing (Springy)
;;; ============================================================

;;; ease-out-elastic : Easing
;;; Elastic/spring effect at the end.
(define (ease-out-elastic t)
  (if (or (= t 0) (= t 1))
      t
      (* (expt 2 (* -10 t))
         (sin (* (- (* t 10) 0.75) (/ (* 2 3.141592653589793) 3)))
         0.5
         2)))

;;; ease-in-elastic : Easing
;;; Elastic/spring effect at the start.
(define (ease-in-elastic t)
  (- 1 (ease-out-elastic (- 1 t))))

;;; ease-in-out-elastic : Easing
;;; Elastic at both ends.
(define (ease-in-out-elastic t)
  (cond
    [(= t 0) 0]
    [(= t 1) 1]
    [(< t 0.5)
     (* -0.5
        (expt 2 (- (* 20 t) 10))
        (sin (* (- (* 20 t) 11.125) (/ (* 2 3.141592653589793) 4.5))))]
    [else
     (+ (* 0.5
           (expt 2 (+ (* -20 t) 10))
           (sin (* (- (* 20 t) 11.125) (/ (* 2 3.141592653589793) 4.5))))
        1)]))

;;; ============================================================
;;; Back Easing (Anticipation)
;;; ============================================================

;;; ease-in-back : Easing
;;; Back up slightly before moving forward.
(define (ease-in-back t)
  (let ([c1 1.70158]
        [c3 (+ 1.70158 1)])
    (- (* c3 t t t) (* c1 t t))))

;;; ease-out-back : Easing
;;; Overshoot target, then settle.
(define (ease-out-back t)
  (let ([c1 1.70158]
        [c3 (+ 1.70158 1)]
        [t1 (- t 1)])
    (+ 1 (* c3 t1 t1 t1) (* c1 t1 t1))))

;;; ease-in-out-back : Easing
;;; Anticipation at both ends.
(define (ease-in-out-back t)
  (let ([c1 1.70158]
        [c2 (* 1.70158 1.525)])
    (if (< t 0.5)
        (let ([t2 (* 2 t)])
          (* 0.5 (* t2 t2 (- (* (+ c2 1) t2) c2))))
        (let ([t2 (- (* 2 t) 2)])
          (+ 1 (* 0.5 (* t2 t2 (+ (* (+ c2 1) t2) c2))))))))

;;; ============================================================
;;; Animation Utilities
;;; ============================================================

;;; animate : Easing × Real × Real × Real → Real
;;;
;;; Apply an easing function to interpolate between start and end values.
;;;
;;; Parameters:
;;;   easing : The easing function to use
;;;   t      : Time value ∈ [0,1]
;;;   start  : Starting value
;;;   end    : Ending value
;;;
;;; Returns:
;;;   Interpolated value between start and end
(define (animate easing t start end)
  (let ([eased-t (easing t)])
    (+ start (* (- end start) eased-t))))

;;; animate-clamped : Easing × Real × Real × Real → Real
;;;
;;; Like animate, but clamps t to [0,1] first.
(define (animate-clamped easing t start end)
  (animate easing (max 0 (min 1 t)) start end))

;;; frame->t : Nat × Nat → Real
;;;
;;; Convert frame counter to normalized time t ∈ [0,1].
;;;
;;; Parameters:
;;;   frame     : Current frame number
;;;   duration  : Total frames for animation
;;;
;;; Returns:
;;;   Normalized time value
(define (frame->t frame duration)
  (if (= duration 0)
      1.0
      (min 1.0 (/ (exact->inexact frame) (exact->inexact duration)))))

;;; loop-t : Real → Real
;;;
;;; Loop time value: 0→1→0→1...
;;; Useful for continuous animations.
(define (loop-t t)
  (- 1 (abs (- (* 2 (- t (floor t))) 1))))

;;; ping-pong : Nat × Nat → Real
;;;
;;; Ping-pong between 0 and 1 over duration frames.
(define (ping-pong frame duration)
  (loop-t (frame->t frame duration)))

;;; ============================================================
;;; Color Animation
;;; ============================================================

;;; animate-color : Easing × Real × Color × Color → Color
;;;
;;; Interpolate between two colors using an easing function.
;;; Requires color.ss for lerp-color.
(define (animate-color easing t color-start color-end)
  (let ([eased-t (easing t)])
    (lerp-color color-start color-end eased-t)))

;;; ============================================================
;;; Point Animation
;;; ============================================================

;;; animate-point : Easing × Real × Point × Point → Point
;;;
;;; Interpolate between two points using an easing function.
(define (animate-point easing t p-start p-end)
  (let ([eased-t (easing t)]
        [x-start (point-x p-start)]
        [y-start (point-y p-start)]
        [x-end (point-x p-end)]
        [y-end (point-y p-end)])
    (point (inexact->exact (round (+ x-start (* (- x-end x-start) eased-t))))
           (inexact->exact (round (+ y-start (* (- y-end y-start) eased-t)))))))

;;; ============================================================
;;; Presets
;;; ============================================================

;;; Common animation presets for convenience

;;; smooth : Easing
;;; Smooth, gentle animation (ease-in-out-sine)
(define smooth ease-in-out-sine)

;;; snappy : Easing
;;; Quick, responsive animation (ease-out-cubic)
(define snappy ease-out-cubic)

;;; playful : Easing
;;; Bouncy, fun animation (ease-out-bounce)
(define playful ease-out-bounce)

;;; springy : Easing
;;; Elastic, spring-like animation (ease-out-elastic)
(define springy ease-out-elastic)
