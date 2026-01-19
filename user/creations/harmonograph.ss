;;; harmonograph.ss — Simulated harmonograph drawing
;;;
;;; A harmonograph uses pendulums to create beautiful interference patterns.
;;; This simulates the effect using parametric equations with damping.

;;; Load turtle graphics
(load "boundary/ui/turtle-color.ss")
(load "boundary/ui/turtle-path.ss")
(load "boundary/ui/turtle.ss")
(load "boundary/ui/turtle-svg.ss")

;;; Harmonograph equations with damping
;;; x(t) = A1*sin(f1*t + p1)*exp(-d1*t) + A2*sin(f2*t + p2)*exp(-d2*t)
;;; y(t) = A3*sin(f3*t + p3)*exp(-d3*t) + A4*sin(f4*t + p4)*exp(-d4*t)

(define (harmonograph-point t a1 f1 p1 d1 a2 f2 p2 d2)
  (* a1 (sin (+ (* f1 t) p1)) (exp (* (- d1) t))))

(define (draw-harmonograph t n dt params)
  (if (<= n 0)
      t
      (let* ([time (* n dt)]
             [damping (exp (* -0.002 time))]
             ;; X coordinate
             [x1 (harmonograph-point time 80 2.01 0.0 0.002 0 0 0 0)]
             [x2 (harmonograph-point time 80 3.0 1.5 0.002 0 0 0 0)]
             [x (+ 320 x1 x2)]
             ;; Y coordinate
             [y1 (harmonograph-point time 80 3.0 0.0 0.002 0 0 0 0)]
             [y2 (harmonograph-point time 80 2.0 0.5 0.002 0 0 0 0)]
             [y (+ 240 y1 y2)]
             ;; Color based on time and damping
             [col-val (mod (inexact->exact (floor (* time 100))) 4096)]
             [alpha (inexact->exact (floor (* damping 15)))]
             [col (color12-from-int col-val)]
             [t (setpc t col)]
             [t (if (= n 3000) (pu t) t)]  ; Start with pen up
             [t (setxy t x y)]
             [t (if (< n 2999) (pd t) t)])  ; Put pen down after first move
            (draw-harmonograph t (- n 1) dt params))))

;;; Create harmonograph drawing
(define harmono-turtle
  (let* ([t (make-turtle)]
         [bg (color12-from-int #x000)]  ; Black background
         [t (setbg t bg)]
         [t (setpw t 1)]
         [t (setxy t 320 240)])
        (draw-harmonograph t 3000 0.02 '())))

;;; Export
(save-svg (turtle->drawing harmono-turtle) "user/creations/harmonograph.svg")
(display "✓ Created harmonograph.svg - mathematical pendulum art!\n")
