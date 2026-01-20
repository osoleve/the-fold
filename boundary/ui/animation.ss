(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'animation)
(doc 'description "Animation and Easing Functions — Provides easing functions for smooth motion and transitions.")
(doc 'layer 'boundary)
(doc 'purity 'total)

(doc 'section 'core-easing-type)

(doc 'note "Easing : Real[0,1] → Real — An easing function takes a normalized time value t ∈ [0,1] and returns an eased value (typically ∈ [0,1], but may overshoot). Input: t = 0.0 → start of animation, t = 0.5 → halfway through, t = 1.0 → end of animation. Output: Transformed value showing animation progress.")

(doc 'section 'linear-easing)

(doc linear 'type (-> Real Real))
(doc linear 'description "No easing — constant velocity.")
(define (linear t) t)

(doc 'section 'quadratic-easing)

(doc ease-in-quad 'type (-> Real Real))
(doc ease-in-quad 'description "Accelerating from zero velocity (t²).")
(define (ease-in-quad t)
  (* t t))

(doc ease-out-quad 'type (-> Real Real))
(doc ease-out-quad 'description "Decelerating to zero velocity.")
(define (ease-out-quad t)
  (- (* t (- t 2))))

(doc ease-in-out-quad 'type (-> Real Real))
(doc ease-in-out-quad 'description "Acceleration until halfway, then deceleration.")
(define (ease-in-out-quad t)
  (if (< t 0.5)
      (* 2 t t)
      (+ (* -2 t t) (* 4 t) -1)))

(doc 'section 'cubic-easing)

(doc ease-in-cubic 'type (-> Real Real))
(doc ease-in-cubic 'description "Accelerating from zero velocity (t³).")
(define (ease-in-cubic t)
  (* t t t))

(doc ease-out-cubic 'type (-> Real Real))
(doc ease-out-cubic 'description "Decelerating to zero velocity.")
(define (ease-out-cubic t)
  (let ([t1 (- t 1)])
       (+ (* t1 t1 t1) 1)))

(doc ease-in-out-cubic 'type (-> Real Real))
(doc ease-in-out-cubic 'description "Acceleration until halfway, then deceleration.")
(define (ease-in-out-cubic t)
  (if (< t 0.5)
      (* 4 t t t)
      (let ([t1 (- (* 2 t) 2)])
           (+ (* 0.5 t1 t1 t1) 1))))

(doc 'section 'quartic-easing)

(doc ease-in-quart 'type (-> Real Real))
(doc ease-in-quart 'description "Accelerating from zero velocity (t⁴).")
(define (ease-in-quart t)
  (* t t t t))

(doc ease-out-quart 'type (-> Real Real))
(doc ease-out-quart 'description "Decelerating to zero velocity.")
(define (ease-out-quart t)
  (let ([t1 (- t 1)])
       (- 1 (* t1 t1 t1 t1))))

(doc ease-in-out-quart 'type (-> Real Real))
(doc ease-in-out-quart 'description "Acceleration until halfway, then deceleration.")
(define (ease-in-out-quart t)
  (if (< t 0.5)
      (* 8 t t t t)
      (let ([t1 (- (* 2 t) 2)])
           (- 1 (* 0.5 t1 t1 t1 t1)))))

(doc 'section 'exponential-easing)

(doc ease-in-expo 'type (-> Real Real))
(doc ease-in-expo 'description "Exponential accelerating from zero velocity.")
(define (ease-in-expo t)
  (if (= t 0)
      0
      (expt 2 (* 10 (- t 1)))))

(doc ease-out-expo 'type (-> Real Real))
(doc ease-out-expo 'description "Exponential decelerating to zero velocity.")
(define (ease-out-expo t)
  (if (= t 1)
      1
      (- 1 (expt 2 (* -10 t)))))

(doc ease-in-out-expo 'type (-> Real Real))
(doc ease-in-out-expo 'description "Exponential acceleration/deceleration.")
(define (ease-in-out-expo t)
  (cond
   [(= t 0) 0]
   [(= t 1) 1]
   [(< t 0.5) (* 0.5 (expt 2 (* 20 t -10)))]
   [else (- 1 (* 0.5 (expt 2 (+ (* -20 t) 10))))]))

(doc 'section 'sine-easing)

(doc ease-in-sine 'type (-> Real Real))
(doc ease-in-sine 'description "Sinusoidal accelerating from zero velocity.")
(define (ease-in-sine t)
  (- 1 (cos (* t 1.5707963267948966))))  ; π/2

(doc ease-out-sine 'type (-> Real Real))
(doc ease-out-sine 'description "Sinusoidal decelerating to zero velocity.")
(define (ease-out-sine t)
  (sin (* t 1.5707963267948966)))  ; π/2

(doc ease-in-out-sine 'type (-> Real Real))
(doc ease-in-out-sine 'description "Sinusoidal acceleration/deceleration.")
(define (ease-in-out-sine t)
  (* -0.5 (- (cos (* 3.141592653589793 t)) 1)))  ; π

(doc 'section 'bounce-easing)

(doc ease-out-bounce 'type (-> Real Real))
(doc ease-out-bounce 'description "Bounce effect at the end.")
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

(doc ease-in-bounce 'type (-> Real Real))
(doc ease-in-bounce 'description "Bounce effect at the start.")
(define (ease-in-bounce t)
  (- 1 (ease-out-bounce (- 1 t))))

(doc ease-in-out-bounce 'type (-> Real Real))
(doc ease-in-out-bounce 'description "Bounce at both ends.")
(define (ease-in-out-bounce t)
  (if (< t 0.5)
      (* 0.5 (ease-in-bounce (* t 2)))
      (+ (* 0.5 (ease-out-bounce (- (* t 2) 1))) 0.5)))

(doc 'section 'elastic-easing)

(doc ease-out-elastic 'type (-> Real Real))
(doc ease-out-elastic 'description "Elastic/spring effect at the end.")
(define (ease-out-elastic t)
  (if (or (= t 0) (= t 1))
      t
      (* (expt 2 (* -10 t))
         (sin (* (- (* t 10) 0.75) (/ (* 2 3.141592653589793) 3)))
         0.5
         2)))

(doc ease-in-elastic 'type (-> Real Real))
(doc ease-in-elastic 'description "Elastic/spring effect at the start.")
(define (ease-in-elastic t)
  (- 1 (ease-out-elastic (- 1 t))))

(doc ease-in-out-elastic 'type (-> Real Real))
(doc ease-in-out-elastic 'description "Elastic at both ends.")
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

(doc 'section 'back-easing)

(doc ease-in-back 'type (-> Real Real))
(doc ease-in-back 'description "Back up slightly before moving forward.")
(define (ease-in-back t)
  (let ([c1 1.70158]
        [c3 (+ 1.70158 1)])
       (- (* c3 t t t) (* c1 t t))))

(doc ease-out-back 'type (-> Real Real))
(doc ease-out-back 'description "Overshoot target, then settle.")
(define (ease-out-back t)
  (let ([c1 1.70158]
        [c3 (+ 1.70158 1)]
        [t1 (- t 1)])
       (+ 1 (* c3 t1 t1 t1) (* c1 t1 t1))))

(doc ease-in-out-back 'type (-> Real Real))
(doc ease-in-out-back 'description "Anticipation at both ends.")
(define (ease-in-out-back t)
  (let ([c1 1.70158]
        [c2 (* 1.70158 1.525)])
       (if (< t 0.5)
           (let ([t2 (* 2 t)])
                (* 0.5 (* t2 t2 (- (* (+ c2 1) t2) c2))))
           (let ([t2 (- (* 2 t) 2)])
                (+ 1 (* 0.5 (* t2 t2 (+ (* (+ c2 1) t2) c2))))))))

(doc 'section 'animation-utilities)

(doc animate 'type (-> (-> Real Real) Real Real Real Real))
(doc animate 'description "Apply an easing function to interpolate between start and end values.")
(doc animate 'param 'easing "The easing function to use")
(doc animate 'param 't "Time value ∈ [0,1]")
(doc animate 'param 'start "Starting value")
(doc animate 'param 'end "Ending value")
(doc animate 'returns "Interpolated value between start and end")
(define (animate easing t start end)
  (let ([eased-t (easing t)])
       (+ start (* (- end start) eased-t))))

(doc animate-clamped 'type (-> (-> Real Real) Real Real Real Real))
(doc animate-clamped 'description "Like animate, but clamps t to [0,1] first.")
(define (animate-clamped easing t start end)
  (animate easing (max 0 (min 1 t)) start end))

(doc frame->t 'type (-> Nat Nat Real))
(doc frame->t 'description "Convert frame counter to normalized time t ∈ [0,1].")
(doc frame->t 'param 'frame "Current frame number")
(doc frame->t 'param 'duration "Total frames for animation")
(doc frame->t 'returns "Normalized time value")
(define (frame->t frame duration)
  (if (= duration 0)
      1.0
      (min 1.0 (/ (exact->inexact frame) (exact->inexact duration)))))

(doc loop-t 'type (-> Real Real))
(doc loop-t 'description "Loop time value: 0→1→0→1... Useful for continuous animations.")
(define (loop-t t)
  (- 1 (abs (- (* 2 (- t (floor t))) 1))))

(doc ping-pong 'type (-> Nat Nat Real))
(doc ping-pong 'description "Ping-pong between 0 and 1 over duration frames.")
(define (ping-pong frame duration)
  (loop-t (frame->t frame duration)))

(doc 'section 'color-animation)

(doc animate-color 'type (-> (-> Real Real) Real Color Color Color))
(doc animate-color 'description "Interpolate between two colors using an easing function. Requires color.ss for lerp-color.")
(define (animate-color easing t color-start color-end)
  (let ([eased-t (easing t)])
       (lerp-color color-start color-end eased-t)))

(doc 'section 'point-animation)

(doc animate-point 'type (-> (-> Real Real) Real Point Point Point))
(doc animate-point 'description "Interpolate between two points using an easing function.")
(define (animate-point easing t p-start p-end)
  (let ([eased-t (easing t)]
        [x-start (point-x p-start)]
        [y-start (point-y p-start)]
        [x-end (point-x p-end)]
        [y-end (point-y p-end)])
       (point (inexact->exact (round (+ x-start (* (- x-end x-start) eased-t))))
              (inexact->exact (round (+ y-start (* (- y-end y-start) eased-t)))))))

(doc 'section 'presets)
(doc 'note "Common animation presets for convenience")

(doc smooth 'type (-> Real Real))
(doc smooth 'description "Smooth, gentle animation (ease-in-out-sine)")
(define smooth ease-in-out-sine)

(doc snappy 'type (-> Real Real))
(doc snappy 'description "Quick, responsive animation (ease-out-cubic)")
(define snappy ease-out-cubic)

(doc playful 'type (-> Real Real))
(doc playful 'description "Bouncy, fun animation (ease-out-bounce)")
(define playful ease-out-bounce)

(doc springy 'type (-> Real Real))
(doc springy 'description "Elastic, spring-like animation (ease-out-elastic)")
(define springy ease-out-elastic)
