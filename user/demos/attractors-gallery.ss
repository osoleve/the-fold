#!/usr/bin/env scheme-script
;; Strange Attractor Gallery
;; Displays various chaotic attractors with Lyapunov exponent analysis

(load "lattice/sim/dynamics/attractor-render.ss")

(define (display-attractor name sys initial dt n-points n-transient width height scale)
  (display "\x1b;[2J\x1b;[H")
  (display "============================================================\n")
  (display name)
  (display "\n============================================================\n\n")

  (display "Generating trajectory...\n")
  (flush-output-port (current-output-port))

  (let* ([traj (generate-attractor sys initial dt n-points n-transient)]
         [frame (render-attractor-colored traj width height scale 0.3 0.5 0)])
        (display frame)
        (display "\n")

        ;; Compute largest Lyapunov exponent
        (display "Computing Lyapunov exponent...\n")
        (flush-output-port (current-output-port))

        (let ([lyap (largest-lyapunov-exponent sys initial dt 2000 500)])
             (display "Largest Lyapunov exponent: ")
             (display (if (> lyap 0)
                         (string-append (number->string (inexact lyap)) " (CHAOTIC)")
                         (string-append (number->string (inexact lyap)) " (non-chaotic)")))
             (display "\n")))

  (display "\nPress Enter for next attractor...")
  (flush-output-port (current-output-port))
  (read-char))

;; Gallery of attractors
(display-attractor
 "LORENZ ATTRACTOR (The Butterfly)"
 lorenz-classic
 (vector 1.0 1.0 1.0)
 0.005 8000 1000
 80 35 13)

(display-attractor
 "ROSSLER ATTRACTOR (Spiral Chaos)"
 rossler-classic
 (vector 1.0 1.0 1.0)
 0.02 6000 500
 80 35 12)

(display-attractor
 "THOMAS ATTRACTOR (Symmetric Chaos)"
 thomas-classic
 (vector 1.0 0.0 0.0)
 0.05 5000 200
 80 35 10)

(display-attractor
 "CHEN ATTRACTOR (Double Scroll)"
 chen-classic
 (vector 1.0 1.0 1.0)
 0.002 8000 1000
 80 35 10)

(display-attractor
 "AIZAWA ATTRACTOR (Torus Chaos)"
 aizawa-classic
 (vector 0.1 0.0 0.0)
 0.01 8000 500
 80 35 15)

(display "\x1b;[2J\x1b;[H")
(display "Gallery complete!\n")
(display "All displayed systems are chaotic (positive Lyapunov exponent).\n")
