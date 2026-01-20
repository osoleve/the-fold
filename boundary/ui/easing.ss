(library (shell easing)
         (export
          lerp
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
          interpolate)

         (import (chezscheme))

         (define-syntax doc
           (syntax-rules ()
             [(_ args ...) (void)]))

         (doc 'module 'easing)
         (doc 'description "Easing and interpolation functions for smooth animations")
         (doc 'layer 'boundary)
         (doc 'purity 'total)
         (doc 'note "Created by sonnet-graphical")

         (doc 'section 'linear-interpolation)

         (define (lerp a b t)
           (doc 'type (-> Real Real Real Real))
           (doc 'description "Linear interpolation between a and b. t=0 returns a, t=1 returns b")
           (+ a (* (- b a) t)))

         (doc 'section 'easing-functions)

         (define (ease-linear t)
           (doc 'type (-> Real Real))
           (doc 'description "Linear easing (no easing, just identity)")
           t)

         (doc 'note "Quadratic easing")
         (define (ease-in-quad t)
           (doc 'type (-> Real Real))
           (* t t))

         (define (ease-out-quad t)
           (doc 'type (-> Real Real))
           (- 1 (* (- 1 t) (- 1 t))))

         (define (ease-in-out-quad t)
           (doc 'type (-> Real Real))
           (if (< t 0.5)
               (* 2 t t)
               (- 1 (* 2 (- 1 t) (- 1 t)))))

         (doc 'note "Cubic easing")
         (define (ease-in-cubic t)
           (doc 'type (-> Real Real))
           (* t t t))

         (define (ease-out-cubic t)
           (doc 'type (-> Real Real))
           (let ([t1 (- 1 t)])
                (- 1 (* t1 t1 t1))))

         (define (ease-in-out-cubic t)
           (doc 'type (-> Real Real))
           (if (< t 0.5)
               (* 4 t t t)
               (let ([t1 (- 1 t)])
                    (- 1 (* 4 t1 t1 t1)))))

         (doc 'note "Sine easing (smooth, natural motion)")
         (define pi 3.141592653589793)

         (define (ease-in-sine t)
           (doc 'type (-> Real Real))
           (- 1 (cos (* t pi 0.5))))

         (define (ease-out-sine t)
           (doc 'type (-> Real Real))
           (sin (* t pi 0.5)))

         (define (ease-in-out-sine t)
           (doc 'type (-> Real Real))
           (* -0.5 (- (cos (* pi t)) 1)))

         (doc 'section 'higher-level-interpolation)

         (define (interpolate start end t ease-fn)
           (doc 'type (-> Real Real Real (-> Real Real) Real))
           (doc 'description "Higher-level interpolation with easing function")
           (doc 'note "Example: (interpolate 0 100 0.5 ease-in-quad)")
           (lerp start end (ease-fn t))))
