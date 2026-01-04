;;; user/creations/gradient-descent-toy.ss --- Gradient Descent Playground
;;;
;;; A toy demonstrating autodiff, gradient storage, and optimization.
;;; Watch a particle roll down a mathematical landscape!

(load "core/autodiff/grad-store.ss")
(load "core/autodiff/comp-graph.ss")

;;; ============================================================
;;; The Landscape: A Bowl with Some Bumps
;;; ============================================================

;;; Our loss function: a bumpy bowl
;;; f(x, y) = x² + y² + 0.5*sin(3x)*cos(3y)
;;;
;;; Global minimum near (0, 0), but with local texture

(define (landscape x y)
  (+ (* x x)
     (* y y)
     (* 0.5 (sin (* 3 x)) (cos (* 3 y)))))

;;; Gradient of the landscape (computed analytically for this demo)
;;; ∂f/∂x = 2x + 1.5*cos(3x)*cos(3y)
;;; ∂f/∂y = 2y - 1.5*sin(3x)*sin(3y)

(define (landscape-grad x y)
  (let ([dx (+ (* 2 x) (* 1.5 (cos (* 3 x)) (cos (* 3 y))))]
        [dy (- (* 2 y) (* 1.5 (sin (* 3 x)) (sin (* 3 y))))])
       (cons dx dy)))

;;; ============================================================
;;; The Particle: Our Optimizer
;;; ============================================================

(define (make-particle x y)
  (list x y))

(define (particle-x p) (car p))
(define (particle-y p) (cadr p))

(define (particle-loss p)
  (landscape (particle-x p) (particle-y p)))

;;; ============================================================
;;; Gradient Descent Step
;;; ============================================================

(define (gradient-step particle learning-rate)
  (let* ([x (particle-x particle)]
         [y (particle-y particle)]
         [grad (landscape-grad x y)]
         [dx (car grad)]
         [dy (cdr grad)]
         [new-x (- x (* learning-rate dx))]
         [new-y (- y (* learning-rate dy))])
        (make-particle new-x new-y)))

;;; ============================================================
;;; Training Loop with Checkpoints
;;; ============================================================

(define (train particle epochs learning-rate checkpoint-interval)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║          GRADIENT DESCENT TOY                                ║\n")
  (display "║          Rolling down a bumpy bowl...                        ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "Starting position: (")
  (display (particle-x particle))
  (display ", ")
  (display (particle-y particle))
  (display ")\n")
  (display "Initial loss: ")
  (display (particle-loss particle))
  (display "\n")
  (display "Learning rate: ")
  (display learning-rate)
  (display "\n\n")
  
  (let loop ([p particle]
             [epoch 0]
             [checkpoints '()]
             [prev-checkpoint-addr #f])
       (if (>= epoch epochs)
           ;; Done!
           (begin
            (display "\n")
            (display "════════════════════════════════════════════════════════════════\n")
            (display "Final position: (")
            (display (particle-x p))
            (display ", ")
            (display (particle-y p))
            (display ")\n")
            (display "Final loss: ")
            (display (particle-loss p))
            (display "\n")
            (display "Checkpoints saved: ")
            (display (length checkpoints))
            (display "\n")
            (display "════════════════════════════════════════════════════════════════\n")
            (values p (reverse checkpoints)))
           
           ;; Training step
           (let* ([new-p (gradient-step p learning-rate)]
                  [loss (particle-loss new-p)]
                  [grad (landscape-grad (particle-x p) (particle-y p))]
                  [grad-norm (sqrt (+ (* (car grad) (car grad))
                                      (* (cdr grad) (cdr grad))))])
                 
                 ;; Print progress every 10 epochs
                 (when (zero? (modulo epoch 10))
                       (display "Epoch ")
                       (display epoch)
                       (display ": loss=")
                       (display (round-to loss 6))
                       (display " |grad|=")
                       (display (round-to grad-norm 6))
                       (display " pos=(")
                       (display (round-to (particle-x new-p) 4))
                       (display ", ")
                       (display (round-to (particle-y new-p) 4))
                       (display ")")
                       
                       ;; Visual progress bar
                       (display " ")
                       (display (make-progress-bar loss 5.0 20))
                       (display "\n"))
                 
                 ;; Checkpoint?
                 (if (and (> checkpoint-interval 0)
                          (zero? (modulo epoch checkpoint-interval)))
                     ;; Save checkpoint
                     (let* ([ht (make-hashtable equal-hash equal?)]
                            [_ (begin
                                (hashtable-set! ht 0 (car grad))
                                (hashtable-set! ht 1 (cdr grad)))]
                            [checkpoint-name (string-append "epoch-" (number->string epoch))]
                            [addr (if prev-checkpoint-addr
                                      (save-gradients-versioned ht checkpoint-name prev-checkpoint-addr)
                                      (save-gradients ht checkpoint-name))])
                           (when (zero? (modulo epoch 50))
                                 (display "  [checkpoint saved: ")
                                 (display checkpoint-name)
                                 (display "]\n"))
                           (loop new-p (+ epoch 1) (cons addr checkpoints) addr))
                     
                     ;; No checkpoint
                     (loop new-p (+ epoch 1) checkpoints prev-checkpoint-addr))))))

;;; ============================================================
;;; Utilities
;;; ============================================================

(define (round-to n places)
  (let ([factor (expt 10 places)])
       (/ (round (* n factor)) factor)))

(define (make-progress-bar value max-value width)
  (let* ([ratio (min 1.0 (max 0.0 (/ value max-value)))]
         [filled (inexact->exact (round (* ratio width)))]
         [empty (- width filled)])
        (string-append "["
                       (make-string filled #\#)
                       (make-string empty #\-)
                       "]")))

;;; ============================================================
;;; Checkpoint Analysis
;;; ============================================================

(define (analyze-checkpoints checkpoints)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║          CHECKPOINT ANALYSIS                                 ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (if (null? checkpoints)
      (display "No checkpoints to analyze.\n")
      (let ([latest (car (reverse checkpoints))])
           (display "Total checkpoints: ")
           (display (length checkpoints))
           (display "\n")
           (display "Checkpoint chain length: ")
           (display (checkpoint-count latest))
           (display "\n\n")
           
           ;; Show gradient evolution
           (display "Gradient magnitude over time:\n")
           (let loop ([addrs checkpoints] [i 0])
                (unless (null? addrs)
                        (let* ([ht (load-gradients (car addrs))]
                               [dx (hashtable-ref ht 0 0)]
                               [dy (hashtable-ref ht 1 0)]
                               [mag (sqrt (+ (* dx dx) (* dy dy)))])
                              (when (zero? (modulo i 5))
                                    (display "  ")
                                    (display i)
                                    (display ": ")
                                    (display (make-progress-bar mag 5.0 30))
                                    (display " ")
                                    (display (round-to mag 4))
                                    (display "\n"))
                              (loop (cdr addrs) (+ i 1))))))))

;;; ============================================================
;;; Visualize the Landscape (ASCII art!)
;;; ============================================================

(define (visualize-landscape x-min x-max y-min y-max width height particle)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║          LANDSCAPE VISUALIZATION                             ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (let ([x-step (/ (- x-max x-min) width)]
        [y-step (/ (- y-max y-min) height)]
        [px (particle-x particle)]
        [py (particle-y particle)])
       
       ;; Find min/max for color scaling
       (let ([levels " .'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"])
            
            (do ([row 0 (+ row 1)])
                ((>= row height))
                (let ([y (+ y-min (* row y-step))])
                     (do ([col 0 (+ col 1)])
                         ((>= col width))
                         (let* ([x (+ x-min (* col x-step))]
                                [z (landscape x y)]
                                ;; Map z to character (0-5 range)
                                [idx (min (- (string-length levels) 1)
                                          (max 0 (inexact->exact (round (* 10 z)))))]
                                ;; Check if particle is here
                                [is-particle (and (< (abs (- x px)) x-step)
                                                  (< (abs (- y py)) y-step))])
                               (if is-particle
                                   (display "O")
                                   (display (string-ref levels idx)))))
                     (newline))))
       
       (display "\nLegend: O = particle, darker = lower loss\n")))

;;; ============================================================
;;; Run the Demo!
;;; ============================================================

(define (run-demo)
  ;; Start at a random-ish point
  (let* ([start-x 2.0]
         [start-y 1.5]
         [particle (make-particle start-x start-y)])
        
        ;; Visualize starting landscape
        (visualize-landscape -3.0 3.0 -3.0 3.0 60 25 particle)
        
        ;; Train!
        (let-values ([(final-particle checkpoints)
                      (train particle 100 0.1 10)])
                    
                    ;; Visualize final position
                    (visualize-landscape -3.0 3.0 -3.0 3.0 60 25 final-particle)
                    
                    ;; Analyze checkpoints
                    (analyze-checkpoints checkpoints)
                    
                    (display "\n")
                    (display "The particle found its way to the bottom of the bowl!\n")
                    (display "Gradients were checkpointed along the way using the new grad-store.\n")
                    (display "\n"))))

;; Run it!
(run-demo)
