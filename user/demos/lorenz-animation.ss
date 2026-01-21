#!/usr/bin/env scheme-script
;; Spinning Lorenz Attractor Animation
;; Run with: scheme --script user/demos/lorenz-animation.ss

(load "lattice/sim/dynamics/attractor-render.ss")

(display "Generating Lorenz attractor trajectory (10000 points)...\n")
(flush-output-port (current-output-port))

(define lorenz-trajectory
  (generate-attractor lorenz-classic
                      (vector 1.0 1.0 1.0)
                      0.005 10000 1000))

(display "Rendering 36 animation frames...\n")
(flush-output-port (current-output-port))

(define frames
  (render-spinning-attractor-colored lorenz-trajectory 80 40 14 36))

(display "Playing animation (Ctrl+C to stop)...\n\n")
(flush-output-port (current-output-port))

;; Play the animation in a loop
(loop-animation frames 100 5)

(display "\x1b;[2J\x1b;[H")
(display "Animation complete!\n")
