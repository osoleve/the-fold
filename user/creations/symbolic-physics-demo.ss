;;; user/creations/symbolic-physics-demo.ss --- Symbolic Autodiff Physics Demo
;;;
;;; Visual demonstration of symbolic differentiation applied to physics:
;;; 1. Define physics equations symbolically
;;; 2. Compute symbolic derivatives (gradients)
;;; 3. Simplify the symbolic expressions
;;; 4. Use them for optimization (e.g., optimal projectile angle)

(load "core/fp/symbolic/integrate-autodiff.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

(define WIDTH 70)
(define HEIGHT 35)

;;; ============================================================
;;; Frame Drawing
;;; ============================================================

(define (draw-title! frame title y-pos)
  (let ([start-x (inexact->exact (floor (- (/ WIDTH 2) (/ (string-length title) 2))))])
       (do ([i 0 (+ i 1)])
           ((>= i (string-length title)))
           (frame-set! frame (+ start-x i) y-pos (string-ref title i)))))

(define (draw-text! frame text x y)
  (do ([i 0 (+ i 1)])
      ((>= i (min (string-length text) (- WIDTH x))))
      (frame-set! frame (+ x i) y (string-ref text i))))

(define (draw-box! frame x y w h)
  ;; Top edge
  (draw-text! frame (make-string w #\-) x y)
  ;; Bottom edge
  (draw-text! frame (make-string w #\-) x (+ y h -1))
  ;; Left/right edges
  (do ([row 1 (+ row 1)])
      ((>= row (- h 1)))
      (frame-set! frame x (+ y row) #\|)
      (frame-set! frame (+ x w -1) (+ y row) #\|)))

;;; ============================================================
;;; Physics Expressions
;;; ============================================================

;; Projectile range: R = (v^2 * sin(2*theta)) / g
;; We'll optimize theta to maximize range

(define v (var 'v))      ; initial velocity
(define theta (var 'theta)) ; launch angle
(define g (var 'g))      ; gravity

;; sin(2*theta) = 2*sin(theta)*cos(theta)
(define range-expr
  (quotient
   (product (power v (num 2))
            (product (num 2)
                     (product (sym-sin theta) (sym-cos theta))))
   g))

;; Derivative of range with respect to theta
(define d-range-d-theta (deriv range-expr 'theta))
(define d-range-simplified (simplify d-range-d-theta))

;; Kinetic energy: KE = (1/2) * m * v^2
(define m (var 'm))
(define ke-expr
  (product (quotient (num 1) (num 2))
           (product m (power v (num 2)))))

;; Derivative of KE with respect to v
(define d-ke-d-v (deriv ke-expr 'v))
(define d-ke-simplified (simplify d-ke-d-v))

;; Gravitational potential: U = m * g * h
(define h (var 'h))
(define pe-expr (product m (product g h)))
(define d-pe-d-h (deriv pe-expr 'h))
(define d-pe-simplified (simplify d-pe-d-h))

;; Pendulum period (small angle): T = 2*pi*sqrt(L/g)
(define L (var 'L))
(define pi-const (var 'pi))
(define period-expr
  (product (num 2)
           (product pi-const
                    (sym-sqrt (quotient L g)))))
(define d-period-d-L (deriv period-expr 'L))
(define d-period-simplified (simplify d-period-d-L))

;;; ============================================================
;;; Numerical Optimization Demo
;;; ============================================================

;; Find optimal angle for maximum range using symbolic gradient
(define (find-optimal-angle)
  (let* ([v0 20]   ; m/s
         [g0 9.8]  ; m/s^2
         [lr 0.01]
         [iterations 50])
        ;; Compile symbolic derivative to traced function
        (let ([range-fn (expr-to-traced range-expr '(v theta g))]
              [grad-fn (expr-to-traced d-range-simplified '(v theta g))])
             ;; Gradient ascent (maximize range)
             (let loop ([theta0 0.3] [iter 0])  ; start at ~17 degrees
                  (if (>= iter iterations)
                      theta0
                      (let* ([grad-val (grad-fn v0 theta0 g0)]
                             [grad-num (if (traced? grad-val) (traced-value grad-val) grad-val)])
                            (loop (+ theta0 (* lr grad-num)) (+ iter 1))))))))

;;; ============================================================
;;; Animation Frames
;;; ============================================================

(define (make-title-frames video n-frames)
  (do ([i 0 (+ i 1)])
      ((>= i n-frames))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame "SYMBOLIC AUTODIFF" 8)
           (draw-title! frame "Physics Edition" 10)
           (draw-title! frame "Exact Derivatives by Computer Algebra" 14)
           (video-add-frame! video frame))))

(define (make-equation-frames video title eq-str deriv-str simp-str n-hold)
  ;; Phase 1: Show equation
  (do ([i 0 (+ i 1)])
      ((>= i 15))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame title 2)
           (draw-box! frame 4 5 62 5)
           (draw-text! frame "f(x) =" 6 7)
           (draw-text! frame eq-str 14 7)
           (video-add-frame! video frame)))
  
  ;; Phase 2: Show derivative appearing (typing effect)
  (let ([deriv-label "d/dx ="])
       (do ([chars 0 (+ chars 2)])
           ((> chars (string-length deriv-str)))
           (let ([frame (make-frame WIDTH HEIGHT #\space)])
                (draw-title! frame title 2)
                (draw-box! frame 4 5 62 5)
                (draw-text! frame "f(x) =" 6 7)
                (draw-text! frame eq-str 14 7)
                (draw-box! frame 4 11 62 5)
                (draw-text! frame deriv-label 6 13)
                (draw-text! frame (substring deriv-str 0 (min chars (string-length deriv-str))) 14 13)
                (video-add-frame! video frame))))
  
  ;; Phase 3: Show simplified form
  (do ([i 0 (+ i 1)])
      ((>= i 10))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame title 2)
           (draw-box! frame 4 5 62 5)
           (draw-text! frame "f(x) =" 6 7)
           (draw-text! frame eq-str 14 7)
           (draw-box! frame 4 11 62 5)
           (draw-text! frame "d/dx =" 6 13)
           (draw-text! frame deriv-str 14 13)
           (draw-text! frame "SIMPLIFYING..." (- (/ WIDTH 2) 6) 18)
           (video-add-frame! video frame)))
  
  ;; Phase 4: Show simplified result
  (do ([i 0 (+ i 1)])
      ((>= i n-hold))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame title 2)
           (draw-box! frame 4 5 62 5)
           (draw-text! frame "f(x) =" 6 7)
           (draw-text! frame eq-str 14 7)
           (draw-box! frame 4 11 62 5)
           (draw-text! frame "d/dx =" 6 13)
           (draw-text! frame simp-str 14 13)
           (draw-text! frame "[Simplified]" 50 13)
           (video-add-frame! video frame))))

(define (make-optimization-frames video)
  ;; Compute optimal angle
  (let* ([optimal-theta (find-optimal-angle)]
         [optimal-deg (* optimal-theta (/ 180 3.14159))])
        
        ;; Show optimization process
        (do ([iter 0 (+ iter 1)])
            ((>= iter 30))
            (let* ([progress (/ iter 30.0)]
                   [current-theta (+ 0.3 (* progress (- optimal-theta 0.3)))]
                   [current-deg (* current-theta (/ 180 3.14159))]
                   [frame (make-frame WIDTH HEIGHT #\space)])
                  (draw-title! frame "OPTIMIZATION: Maximum Projectile Range" 2)
                  
                  (draw-text! frame "Using symbolic gradient: dR/dtheta" 5 5)
                  (draw-text! frame (string-append "Iteration: " (number->string iter)) 5 7)
                  (draw-text! frame (string-append "theta = "
                                                   (number->string (inexact->exact (round current-deg)))
                                                   " degrees") 5 9)
                  
                  ;; Draw a simple trajectory visualization
                  (let ([base-y 28]
                        [base-x 10])
                       ;; Ground line
                       (do ([x 0 (+ x 1)])
                           ((>= x 50))
                           (frame-set! frame (+ base-x x) base-y #\-))
                       
                       ;; Draw trajectory arc
                       (let ([scale 0.4])
                            (do ([t 0 (+ t 0.1)])
                                ((> t 2.5))
                                (let* ([vx (* 20 (cos current-theta))]
                                       [vy (* 20 (sin current-theta))]
                                       [px (* t vx scale)]
                                       [py (- (* t vy scale) (* 0.5 9.8 t t scale scale))]
                                       [sx (+ base-x (inexact->exact (round px)))]
                                       [sy (- base-y (inexact->exact (round (* py 8))))])
                                      (when (and (>= sx 0) (< sx WIDTH)
                                                 (>= sy 0) (< sy HEIGHT)
                                                 (>= sy (- base-y 15)))
                                            (frame-set! frame sx sy #\*))))))
                  
                  (video-add-frame! video frame)))
        
        ;; Final result
        (do ([i 0 (+ i 1)])
            ((>= i 30))
            (let ([frame (make-frame WIDTH HEIGHT #\space)])
                 (draw-title! frame "OPTIMAL ANGLE FOUND!" 2)
                 (draw-text! frame (string-append "theta* = "
                                                  (number->string (inexact->exact (round optimal-deg)))
                                                  " degrees (45 degrees is optimal)") 5 6)
                 (draw-text! frame "Computed using symbolic derivative:" 5 9)
                 (draw-text! frame "dR/dtheta = (2*v^2/g)*(cos^2(theta) - sin^2(theta))" 5 11)
                 (draw-text! frame "Setting dR/dtheta = 0 => cos(2*theta) = 0" 5 13)
                 (draw-text! frame "=> theta = pi/4 = 45 degrees" 5 15)
                 
                 ;; Draw optimal trajectory
                 (let ([base-y 30]
                       [base-x 10]
                       [theta-opt (/ 3.14159 4)])
                      (do ([x 0 (+ x 1)])
                          ((>= x 50))
                          (frame-set! frame (+ base-x x) base-y #\-))
                      (let ([scale 0.4])
                           (do ([t 0 (+ t 0.1)])
                               ((> t 2.9))
                               (let* ([vx (* 20 (cos theta-opt))]
                                      [vy (* 20 (sin theta-opt))]
                                      [px (* t vx scale)]
                                      [py (- (* t vy scale) (* 0.5 9.8 t t scale scale))]
                                      [sx (+ base-x (inexact->exact (round px)))]
                                      [sy (- base-y (inexact->exact (round (* py 8))))])
                                     (when (and (>= sx 0) (< sx WIDTH)
                                                (>= sy 0) (< sy HEIGHT)
                                                (>= sy (- base-y 20)))
                                           (frame-set! frame sx sy #\@))))))
                 
                 (video-add-frame! video frame)))))

;;; ============================================================
;;; Main
;;; ============================================================

(define (run-symbolic-physics-demo)
  (display "Building symbolic physics demo...\n")
  
  ;; Pre-compute all the string representations
  (display "Computing symbolic derivatives...\n")
  
  (let ([ke-str "(1/2)*m*v^2"]
        [ke-deriv-str (expr->infix d-ke-d-v)]
        [ke-simp-str (expr->infix d-ke-simplified)]
        [pe-str "m*g*h"]
        [pe-deriv-str (expr->infix d-pe-d-h)]
        [pe-simp-str (expr->infix d-pe-simplified)])
       
       (display "KE derivative: ") (display ke-deriv-str) (newline)
       (display "KE simplified: ") (display ke-simp-str) (newline)
       (display "PE derivative: ") (display pe-deriv-str) (newline)
       (display "PE simplified: ") (display pe-simp-str) (newline)
       
       (let ([video (make-video)])
            
            ;; Title
            (make-title-frames video 25)
            
            ;; Kinetic Energy
            (make-equation-frames video
                                  "KINETIC ENERGY"
                                  ke-str
                                  ke-deriv-str
                                  ke-simp-str
                                  20)
            
            ;; Potential Energy
            (make-equation-frames video
                                  "GRAVITATIONAL POTENTIAL"
                                  pe-str
                                  pe-deriv-str
                                  pe-simp-str
                                  20)
            
            ;; Optimization
            (make-optimization-frames video)
            
            (display "Total frames: ")
            (display (video-frame-count video))
            (newline)
            
            ;; Export
            (display "Exporting to GIF...\n")
            (video->gif video "user/creations/symbolic-physics-demo.gif" 80)
            (display "Done! Saved to user/creations/symbolic-physics-demo.gif\n")
            
            video)))

;; Run it
(run-symbolic-physics-demo)
