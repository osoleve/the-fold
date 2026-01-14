;;; gradient-flow-video.ss --- Gradient Flow Animation with Proper Export
;;;
;;; Uses the ascii-video + ascii-video-export system for proper GIF output.
;;; Showcases symbolic differentiation + autodiff integration.
;;;
;;; Usage:
;;;   (load "user/creations/gradient-flow-video.ss")
;;;   (render-gradient-flow!)  ; Creates gradient-flow.gif

(load "core/base/prelude.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "lattice/fp/symbolic/integrate-autodiff.ss")

;;; ====
;;; Configuration
;;; ====

(define *width* 60)
(define *height* 20)
(define *num-particles* 10)
(define *num-frames* 20)

;;; Coordinate transforms
(define (canvas->math-x x) (- (* (/ x *width*) 6.0) 3.0))
(define (canvas->math-y y) (- (* (/ y *height*) 4.0) 2.0))
(define (math->canvas-x mx) (inexact->exact (floor (* (/ (+ mx 3.0) 6.0) *width*))))
(define (math->canvas-y my) (inexact->exact (floor (* (/ (+ my 2.0) 4.0) *height*))))

;;; ====
;;; ASCII Character Ramps
;;; ====

(define density-ramp " .'`^\":;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$")
(define density-len (string-length density-ramp))

(define (angle->arrow angle)
  (let* ([normalized (mod (+ angle 360.0) 360.0)]
         [octant (inexact->exact (round (/ normalized 45.0)))])
        (case (modulo octant 8)
              [(0) #\>] [(1) #\\] [(2) #\v] [(3) #\/]
              [(4) #\<] [(5) #\/] [(6) #\^] [(7) #\\]
              [else #\.])))

;;; ====
;;; Symbolic Potential Field
;;; ====

(define (sym-gaussian x-expr y-expr cx cy amplitude sigma)
  (let* ([dx (difference x-expr (num cx))]
         [dy (difference y-expr (num cy))]
         [r2 (sum (power dx (num 2)) (power dy (num 2)))]
         [exp-arg (product (num (- (/ 1.0 (* 2 sigma sigma)))) r2)])
        (product (num amplitude) (sym-exp exp-arg))))

(define (build-potential t)
  ;; Simpler: single moving Gaussian for faster compilation
  (let* ([x (var 'x)]
         [y (var 'y)]
         ;; Center orbits in a circle
         [cx (* 1.5 (cos (* t 2 3.14159)))]
         [cy (* 1.0 (sin (* t 2 3.14159)))]
         [g1 (sym-gaussian x y cx cy 1.0 0.7)]
         [potential (make-neg g1)])
        (values potential x y)))

;;; ====
;;; Gradient Computation (Symbolic -> Numerical)
;;; ====

(define (compile-field expr vars)
  (let ([traced-fn (expr-to-traced expr vars)])
       (lambda (x y)
               (let ([result (traced-fn x y)])
                    (if (traced? result)
                        (traced-value result)
                        result)))))

(define (compile-gradient expr vars)
  (let* ([grad-exprs (map simplify (sym-gradient expr vars))]
         [dx-fn (expr-to-traced (car grad-exprs) vars)]
         [dy-fn (expr-to-traced (cadr grad-exprs) vars)])
        (lambda (x y)
                (let ([dx (let ([r (dx-fn x y)]) (if (traced? r) (traced-value r) r))]
                      [dy (let ([r (dy-fn x y)]) (if (traced? r) (traced-value r) r))])
                     (values dx dy)))))

;;; ====
;;; Particle System
;;; ====

(define (make-particle x y) (list x y 0))
(define (particle-x p) (car p))
(define (particle-y p) (cadr p))
(define (particle-age p) (caddr p))

(define (init-particles n)
  (let loop ([i 0] [particles '()])
       (if (>= i n)
           particles
           (loop (+ i 1)
                 (cons (make-particle (- (* (random 100) 0.06) 3.0)
                                      (- (* (random 100) 0.04) 2.0))
                       particles)))))

(define (update-particle p grad-fn dt)
  (let* ([x (particle-x p)]
         [y (particle-y p)]
         [age (particle-age p)])
        (call-with-values
         (lambda () (grad-fn x y))
         (lambda (dx dy)
                 (let* ([mag (sqrt (+ (* dx dx) (* dy dy)))]
                        [scale (if (> mag 0.001) (/ dt mag) 0.0)]
                        [nx (- x (* dx scale))]
                        [ny (- y (* dy scale))]
                        [nx (max -2.9 (min 2.9 nx))]
                        [ny (max -1.9 (min 1.9 ny))])
                       (list nx ny (+ age 1)))))))

(define (respawn-if-old p max-age)
  (if (> (particle-age p) max-age)
      (make-particle (- (* (random 100) 0.06) 3.0)
                     (- (* (random 100) 0.04) 2.0))
      p))

;;; ====
;;; Frame Rendering
;;; ====

(define (render-gradient-frame potential-fn grad-fn particles frame-num t)
  (let ([frame (make-frame *width* *height* #\space)])
       
       ;; Pass 1: Potential field as density
       (do ([cy 0 (+ cy 1)])
           ((>= cy *height*))
           (do ([cx 0 (+ cx 1)])
               ((>= cx *width*))
               (let* ([mx (canvas->math-x cx)]
                      [my (canvas->math-y cy)]
                      [val (potential-fn mx my)]
                      [normalized (max 0.0 (min 1.0 (+ 0.5 (* (- val) 0.5))))]
                      [idx (inexact->exact (floor (* normalized (- density-len 1))))]
                      [ch (string-ref density-ramp idx)])
                     (frame-set! frame cx cy ch))))
       
       ;; Pass 2: Gradient arrows
       (do ([cy 2 (+ cy 4)])
           ((>= cy (- *height* 2)))
           (do ([cx 4 (+ cx 6)])
               ((>= cx (- *width* 4)))
               (let* ([mx (canvas->math-x cx)]
                      [my (canvas->math-y cy)])
                     (call-with-values
                      (lambda () (grad-fn mx my))
                      (lambda (dx dy)
                              (let ([mag (sqrt (+ (* dx dx) (* dy dy)))])
                                   (when (> mag 0.1)
                                         (let* ([angle (* (atan dy dx) (/ 180.0 3.14159))]
                                                [arrow (angle->arrow (- 90 angle))])
                                               (frame-set! frame cx cy arrow)))))))))
       
       ;; Pass 3: Particles
       (for-each
        (lambda (p)
                (let* ([px (math->canvas-x (particle-x p))]
                       [py (math->canvas-y (particle-y p))])
                      (when (and (>= px 0) (< px *width*)
                                 (>= py 0) (< py *height*))
                            (let ([ch (case (modulo (inexact->exact (floor (particle-age p))) 4)
                                            [(0) #\@] [(1) #\*] [(2) #\o] [(3) #\.])])
                                 (frame-set! frame px py ch)))))
        particles)
       
       ;; Title bar
       (frame-put-string! frame 2 0 "SYMBOLIC GRADIENT FLOW")
       (frame-put-string! frame 2 1 (string-append "t=" (number->string (/ (round (* t 100)) 100))))
       
       frame))

;;; ====
;;; Animation Generation
;;; ====

(define (generate-gradient-flow-video)
  (display "\nGenerating Gradient Flow Symphony...\n")
  (display "  Using symbolic differentiation + autodiff\n\n")
  
  (let ([video (make-video)]
        [particles (init-particles *num-particles*)])
       
       (let loop ([frame-num 0] [particles particles])
            (when (< frame-num *num-frames*)
                  (let ([t (/ frame-num *num-frames*)])
                       (call-with-values
                        (lambda () (build-potential t))
                        (lambda (potential x y)
                                (let* ([potential-fn (compile-field potential '(x y))]
                                       [grad-fn (compile-gradient potential '(x y))]
                                       [new-particles
                                        (map (lambda (p)
                                                     (respawn-if-old (update-particle p grad-fn 0.15) 50))
                                             particles)]
                                       [frame (render-gradient-frame potential-fn grad-fn new-particles frame-num t)])
                                      
                                      (video-add-frame! video frame)
                                      
                                      (when (zero? (modulo frame-num 10))
                                            (display "  Frame ")
                                            (display frame-num)
                                            (display "/")
                                            (display *num-frames*)
                                            (display "\n"))
                                      
                                      (loop (+ frame-num 1) new-particles)))))))
       
       (display "  Generated ")
       (display (video-frame-count video))
       (display " frames\n")
       video))

;;; ====
;;; Export
;;; ====

(define (render-gradient-flow!)
  (display "\n")
  (display "====\n")
  (display "  GRADIENT FLOW SYMPHONY\n")
  (display "  Symbolic Differentiation + Autodiff Demo\n")
  (display "====\n")
  
  (let ([video (generate-gradient-flow-video)])
       (display "\nExporting to GIF...\n")
       (video->gif video "user/creations/gradient-flow.gif" 80)
       (display "\nDone! Check user/creations/gradient-flow.gif\n")))

;;; ====
;;; Demo Entry
;;; ====

(display "\n")
(display "====\n")
(display "  GRADIENT FLOW VIDEO\n")
(display "  Proper Scheme-based animation export\n")
(display "====\n")
(display "\n")
(display "  (render-gradient-flow!)  - Generate gradient-flow.gif\n")
(display "\n")
