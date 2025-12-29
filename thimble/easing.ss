;;; Easing and interpolation functions for smooth animations
;;; Created by sonnet-graphical

(library (shell easing)
         (export
          ;; Linear interpolation
          lerp
          
          ;; Easing functions (all take t in [0,1], return eased value in [0,1])
          ease-linear
          ease-in-quad
          ease-out-quad
          ease-in-out-quad
          ease-in-cubic
          ease-out-cubic
          ease-in-out-cubic
          ease-in-sine
          ease-out-sine
          ease-in-out-sine
          
          ;; Higher-level: interpolate with easing
          interpolate)
         
         (import (chezscheme))
         
         ;; Linear interpolation between a and b
         ;; t=0 returns a, t=1 returns b
         (define (lerp a b t)
           (+ a (* (- b a) t)))
         
         ;; Linear easing (no easing, just identity)
         (define (ease-linear t) t)
         
         ;; Quadratic easing
         (define (ease-in-quad t)
           (* t t))
         
         (define (ease-out-quad t)
           (- 1 (* (- 1 t) (- 1 t))))
         
         (define (ease-in-out-quad t)
           (if (< t 0.5)
               (* 2 t t)
               (- 1 (* 2 (- 1 t) (- 1 t)))))
         
         ;; Cubic easing
         (define (ease-in-cubic t)
           (* t t t))
         
         (define (ease-out-cubic t)
           (let ([t1 (- 1 t)])
                (- 1 (* t1 t1 t1))))
         
         (define (ease-in-out-cubic t)
           (if (< t 0.5)
               (* 4 t t t)
               (let ([t1 (- 1 t)])
                    (- 1 (* 4 t1 t1 t1)))))
         
         ;; Sine easing (smooth, natural motion)
         (define pi 3.141592653589793)
         
         (define (ease-in-sine t)
           (- 1 (cos (* t pi 0.5))))
         
         (define (ease-out-sine t)
           (sin (* t pi 0.5)))
         
         (define (ease-in-out-sine t)
           (* -0.5 (- (cos (* pi t)) 1)))
         
         ;; Higher-level interpolation with easing function
         ;; (interpolate start end t ease-fn)
         ;; Example: (interpolate 0 100 0.5 ease-in-quad)
         (define (interpolate start end t ease-fn)
           (lerp start end (ease-fn t))))
