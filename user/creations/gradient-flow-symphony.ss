;;; user/creations/gradient-flow-symphony.ss — Symbolic Gradient Flow Visualization
;;;
;;; A mesmerizing ASCII demonstration of symbolic differentiation + autodiff:
;;;   - Symbolic potential fields rendered as ASCII density
;;;   - Symbolic gradients displayed as mathematical expressions
;;;   - Gradient vectors shown as Unicode arrows
;;;   - Particles flowing along gradient descent paths
;;;   - Morphing between potential landscapes
;;;
;;; This is the flex. The absurd demo. The "yes we actually did this" moment.

(load "boundary/ui/layout.ss")
(load "lattice/autodiff/symbolic-diff.ss")

;;; ====
;;; Constants
;;; ====

(define *width* 72)
(define *height* 24)
(define *num-particles* 12)
(define *num-frames* 60)

;;; Coordinate transform: canvas (0,0)-(w,h) → math coords (-3,3)×(-2,2)
(define (canvas->math-x x) (- (* (/ x *width*) 6.0) 3.0))
(define (canvas->math-y y) (- (* (/ y *height*) 4.0) 2.0))
(define (math->canvas-x mx) (inexact->exact (floor (* (/ (+ mx 3.0) 6.0) *width*))))
(define (math->canvas-y my) (inexact->exact (floor (* (/ (+ my 2.0) 4.0) *height*))))

;;; ====
;;; ASCII Character Ramps
;;; ====

;;; Density ramp for potential field (dark = high, light = low)
(define density-ramp " .'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$")
(define density-len (string-length density-ramp))

;;; Arrow characters for gradient directions
(define (angle->arrow angle)
  (let* ([normalized (mod (+ angle 360.0) 360.0)]
         [octant (inexact->exact (round (/ normalized 45.0)))])
        (case (modulo octant 8)
              [(0) #\>] [(1) #\\] [(2) #\v] [(3) #\/]
              [(4) #\<] [(5) #\/] [(6) #\^] [(7) #\\]
              [else #\.])))

;;; ====
;;; Symbolic Potential Field Builder
;;; ====

;;; gaussian : Expr × Expr × Number × Number × Number → Expr
;;; Creates a symbolic Gaussian: A * exp(-((x-cx)² + (y-cy)²) / (2*σ²))
(define (sym-gaussian x-expr y-expr cx cy amplitude sigma)
  (let* ([dx (difference x-expr (num cx))]
         [dy (difference y-expr (num cy))]
         [r2 (sum (power dx (num 2)) (power dy (num 2)))]
         [exp-arg (product (num (- (/ 1.0 (* 2 sigma sigma)))) r2)])
        (product (num amplitude) (sym-exp exp-arg))))

;;; build-potential : Number → (Expr × Expr × Expr)
;;; Build a potential field that morphs based on time parameter t ∈ [0,1]
;;; Returns (potential-expr, x-var, y-var)
(define (build-potential t)
  (let* ([x (var 'x)]
         [y (var 'y)]
         ;; Central well (always present)
         [g1 (sym-gaussian x y 0.0 0.0 1.0 0.8)]
         ;; Two side wells that fade in/out
         [left-amp (* 0.7 (sin (* t 3.14159)))]
         [right-amp (* 0.7 (sin (* (+ t 0.5) 3.14159)))]
         [g2 (sym-gaussian x y -1.8 0.5 left-amp 0.6)]
         [g3 (sym-gaussian x y 1.8 -0.3 right-amp 0.6)]
         ;; Combine into potential (negative for "wells")
         [potential (make-neg (sum g1 (sum g2 g3)))])
        (values potential x y)))

;;; ====
;;; Symbolic Expression Pretty Printer
;;; ====

;;; expr->math-string : Expr → String
;;; Convert symbolic expression to readable math notation
(define (expr->math-string expr)
  (define (format-num n)
    (if (integer? n)
        (number->string (inexact->exact n))
        (let ([s (number->string n)])
             (if (> (string-length s) 6)
                 (substring s 0 6)
                 s))))
  (cond
   [(num? expr) (format-num (num-val expr))]
   [(var? expr) (symbol->string (var-name expr))]
   [(sum? expr)
    (let ([terms (sum-terms expr)])
         (string-append "(" (string-join (map expr->math-string terms) " + ") ")"))]
   [(product? expr)
    (let ([factors (product-factors expr)])
         (string-append (string-join (map expr->math-string factors) "·")))]
   [(difference? expr)
    (if (diff-right expr)
        (string-append "(" (expr->math-string (diff-left expr))
                       " - " (expr->math-string (diff-right expr)) ")")
        (string-append "-" (expr->math-string (diff-left expr))))]
   [(power? expr)
    (string-append (expr->math-string (pow-base expr))
                   "^" (expr->math-string (pow-exp expr)))]
   [(app? expr)
    (string-append (symbol->string (app-fn expr))
                   "(" (expr->math-string (app-arg expr)) ")")]
   [else "?"]))

;;; truncate-string : String × Nat → String
(define (truncate-string s max-len)
  (if (<= (string-length s) max-len)
      s
      (string-append (substring s 0 (- max-len 3)) "...")))

;;; ====
;;; Gradient Field Computation
;;; ====

;;; compile-field : Expr × (List Symbol) → (Number × Number → Number)
(define (compile-field expr vars)
  (let ([traced-fn (expr-to-traced expr vars)])
       (lambda (x y)
               (let ([result (traced-fn x y)])
                    (if (traced? result)
                        (traced-value result)
                        result)))))

;;; compile-gradient : Expr × (List Symbol) → (Number × Number → (Number × Number))
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

(define (make-particle x y)
  (list x y 0))  ; x, y, age

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
                 ;; Move in negative gradient direction (descent)
                 (let* ([mag (sqrt (+ (* dx dx) (* dy dy)))]
                        [scale (if (> mag 0.001) (/ dt mag) 0.0)]
                        [nx (- x (* dx scale))]
                        [ny (- y (* dy scale))]
                        ;; Clamp to bounds
                        [nx (max -2.9 (min 2.9 nx))]
                        [ny (max -1.9 (min 1.9 ny))])
                       (list nx ny (+ age 1)))))))

(define (respawn-particle-if-old p max-age)
  (if (> (particle-age p) max-age)
      (make-particle (- (* (random 100) 0.06) 3.0)
                     (- (* (random 100) 0.04) 2.0))
      p))

;;; ====
;;; Rendering
;;; ====

(define (render-frame potential-fn grad-fn particles frame-num t)
  (let ([canvas (make-canvas *width* *height*)])
       ;; Pass 1: Render potential field as density
       (let y-loop ([cy 0])
            (when (< cy *height*)
                  (let x-loop ([cx 0])
                       (when (< cx *width*)
                             (let* ([mx (canvas->math-x cx)]
                                    [my (canvas->math-y cy)]
                                    [val (potential-fn mx my)]
                                    ;; Map value to density (potential is negative, so negate)
                                    [normalized (max 0.0 (min 1.0 (+ 0.5 (* (- val) 0.5))))]
                                    [idx (inexact->exact (floor (* normalized (- density-len 1))))]
                                    [ch (string-ref density-ramp idx)])
                                   (canvas-set! canvas cx cy ch))
                             (x-loop (+ cx 1))))
                  (y-loop (+ cy 1))))
       
       ;; Pass 2: Overlay gradient arrows at grid points
       (let y-loop ([cy 2])
            (when (< cy (- *height* 2))
                  (let x-loop ([cx 4])
                       (when (< cx (- *width* 4))
                             (let* ([mx (canvas->math-x cx)]
                                    [my (canvas->math-y cy)])
                                   (call-with-values
                                    (lambda () (grad-fn mx my))
                                    (lambda (dx dy)
                                            (let* ([mag (sqrt (+ (* dx dx) (* dy dy)))])
                                                  (when (> mag 0.1)
                                                        (let* ([angle (* (atan dy dx) (/ 180.0 3.14159))]
                                                               [arrow (angle->arrow (- 90 angle))])  ; Adjust for screen coords
                                                              (canvas-set! canvas cx cy arrow)))))))
                             (x-loop (+ cx 6))))
                  (y-loop (+ cy 4))))
       
       ;; Pass 3: Draw particles
       (for-each
        (lambda (p)
                (let* ([px (math->canvas-x (particle-x p))]
                       [py (math->canvas-y (particle-y p))])
                      (when (and (>= px 0) (< px *width*)
                                 (>= py 0) (< py *height*))
                            (let ([ch (case (modulo (inexact->exact (floor (particle-age p))) 4)
                                            [(0) #\@] [(1) #\*] [(2) #\o] [(3) #\.])])
                                 (canvas-set! canvas px py ch)))))
        particles)
       
       canvas))

;;; ====
;;; Frame Output
;;; ====

(define (clear-screen)
  (display "\x1b;[2J\x1b;[H"))

(define (output-frame canvas expr t frame-num)
  (clear-screen)
  ;; Header
  (display "╔══════════════════════════════════════════════════════════════════════╗\n")
  (display "║  ")
  (display "\x1b;[1;36mSYMBOLIC GRADIENT FLOW\x1b;[0m")
  (display "                                               ║\n")
  (display "║                                                                      ║\n")
  ;; Expression display
  (display "║  \x1b;[33mf(x,y)\x1b;[0m = ")
  (let ([expr-str (truncate-string (expr->math-string expr) 58)])
       (display expr-str)
       (display (make-string (- 60 (string-length expr-str)) #\space)))
  (display "║\n")
  (display "║                                                                      ║\n")
  (display "║  \x1b;[32m∇f\x1b;[0m computed symbolically, simplified, compiled to traced functions  ║\n")
  (display "╠══════════════════════════════════════════════════════════════════════╣\n")
  ;; Canvas
  (let ([lines (string-split (canvas->string canvas) #\newline)])
       (for-each
        (lambda (line)
                (display "║")
                (display line)
                (let ([pad (- *width* (string-length line))])
                     (when (> pad 0)
                           (display (make-string pad #\space))))
                (display "║\n"))
        lines))
  ;; Footer
  (display "╠══════════════════════════════════════════════════════════════════════╣\n")
  (display "║  ")
  (display "\x1b;[35mParticles\x1b;[0m: @*o· following -∇f (gradient descent)  ")
  (display "\x1b;[34mArrows\x1b;[0m: ∇f dir  ║\n")
  (display "║  Frame: ")
  (display (number->string frame-num))
  (display "/")
  (display (number->string *num-frames*))
  (display "  t=")
  (display (number->string (/ (round (* t 100)) 100)))
  (display "                                              ║\n")
  (display "╚══════════════════════════════════════════════════════════════════════╝\n")
  (display "\x1b;[90mThe Fold • core/fp/symbolic + core/autodiff\x1b;[0m\n"))

;;; ====
;;; Animation Loop
;;; ====

(define (run-animation)
  (display "\n\x1b;[1;33mGradient Flow Symphony\x1b;[0m\n")
  (display "Building symbolic potential field...\n")
  
  (let loop ([frame 0]
             [particles (init-particles *num-particles*)])
       (when (< frame *num-frames*)
             (let ([t (/ frame *num-frames*)])
                  (call-with-values
                   (lambda () (build-potential t))
                   (lambda (potential x y)
                           ;; Compile to numerical functions
                           (let* ([potential-fn (compile-field potential '(x y))]
                                  [grad-fn (compile-gradient potential '(x y))]
                                  ;; Update particles
                                  [new-particles
                                   (map (lambda (p)
                                                (respawn-particle-if-old
                                                 (update-particle p grad-fn 0.15)
                                                 50))
                                        particles)]
                                  ;; Render
                                  [canvas (render-frame potential-fn grad-fn new-particles frame t)])
                                 (output-frame canvas potential t frame)
                                 ;; Delay for animation
                                 (let delay ([i 0])
                                      (when (< i 3000000) (delay (+ i 1))))
                                 ;; Continue
                                 (loop (+ frame 1) new-particles)))))))
  (display "\n\x1b;[1;32mAnimation complete!\x1b;[0m\n"))

;;; ====
;;; Single Frame Demo (for testing)
;;; ====

(define (demo-single-frame)
  (display "\n\x1b;[1;33mGradient Flow Symphony - Single Frame\x1b;[0m\n\n")
  (let-values ([(potential x y) (build-potential 0.3)])
              (display "Symbolic potential:\n  ")
              (display (truncate-string (expr->math-string potential) 70))
              (display "\n\n")
              
              (display "Computing symbolic gradient...\n")
              (let* ([grad (sym-gradient potential '(x y))]
                     [dx (simplify (car grad))]
                     [dy (simplify (cadr grad))])
                    (display "  ∂f/∂x = ")
                    (display (truncate-string (expr->math-string dx) 60))
                    (display "\n  ∂f/∂y = ")
                    (display (truncate-string (expr->math-string dy) 60))
                    (display "\n\n")
                    
                    (display "Compiling to traced functions...\n")
                    (let* ([potential-fn (compile-field potential '(x y))]
                           [grad-fn (compile-gradient potential '(x y))]
                           [particles (init-particles *num-particles*)]
                           [canvas (render-frame potential-fn grad-fn particles 0 0.3)])
                          (display "\n")
                          (output-frame canvas potential 0.3 1)))))

;;; ====
;;; Entry Points
;;; ====

(display "\n")
(display "╔═══════════════════════════════════════════════════════════════╗\n")
(display "║         GRADIENT FLOW SYMPHONY                                ║\n")
(display "║         Symbolic Differentiation + Autodiff Demo              ║\n")
(display "╠═══════════════════════════════════════════════════════════════╣\n")
(display "║  (demo-single-frame)  - Show one frame with explanations      ║\n")
(display "║  (run-animation)      - Run full animated demo                ║\n")
(display "╚═══════════════════════════════════════════════════════════════╝\n")
(display "\n")

;; Auto-run single frame demo
(demo-single-frame)
