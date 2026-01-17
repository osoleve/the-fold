;;; Preview a specific frame from the-dualities animation
(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "user/creations/ascii-video.ss")
(load "shell/ui/animation.ss")

;; Load the main demo code (but don't run it)
(define WIDTH 80)
(define HEIGHT 34)
(define L1 8.0)
(define L2 6.0)
(define M1 2.0)
(define M2 1.5)
(define G 9.8)
(define THETA1-0 0.5)
(define THETA2-0 -0.3)
(define OMEGA1-0 0.0)
(define OMEGA2-0 0.0)
(define DT 0.004)
(define SUB-STEPS 4)
(define PENDULUM-CENTER-X 25)
(define PENDULUM-CENTER-Y 8)
(define PENDULUM-SCALE 2.0)

;; Load helper functions from the-dualities.ss
(load "user/creations/the-dualities.ss")

;; Simulate to a specific frame
(define (preview-frame frame-num)
  (display "Simulating to frame ") (display frame-num) (display "...\n")

  (let ([state (let loop ([i 0] [s (list THETA1-0 THETA2-0 OMEGA1-0 OMEGA2-0)])
                    (if (>= i frame-num)
                        s
                        (let ([new-state (let sub-loop ([j 0] [st s])
                                              (if (>= j SUB-STEPS)
                                                  st
                                                  (sub-loop (+ j 1) (step-double-pendulum st DT))))])
                             (loop (+ i 1) new-state))))])

       (let* ([theta1 (list-ref state 0)]
               [theta2 (list-ref state 1)]
               [omega1 (list-ref state 2)]
               [omega2 (list-ref state 3)]
               [H (compute-hamiltonian theta1 theta2 omega1 omega2)]
               [L-val (compute-lagrangian theta1 theta2 omega1 omega2)]
               [momenta (compute-momenta theta1 theta2 omega1 omega2)])

             (display "\n")
             (display "Frame ") (display frame-num) (display ":\n")
             (display "  θ1 = ") (display theta1) (newline)
             (display "  θ2 = ") (display theta2) (newline)
             (display "  ω1 = ") (display omega1) (newline)
             (display "  ω2 = ") (display omega2) (newline)
             (display "  p1 = ") (display (car momenta)) (newline)
             (display "  p2 = ") (display (cdr momenta)) (newline)
             (display "  L  = ") (display L-val) (newline)
             (display "  H  = ") (display H) (newline))))

;; Preview frame 100 (middle of duality phase)
(preview-frame 100)

;; Preview frame 300 (chaos phase)
(preview-frame 300)
