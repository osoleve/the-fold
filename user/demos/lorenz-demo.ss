(load "lattice/sim/dynamics/attractor-render.ss")

;; Generate Lorenz attractor trajectory
(display "Generating Lorenz attractor trajectory...\n")
(flush-output-port (current-output-port))

(define lorenz-trajectory
  (generate-attractor lorenz-classic
                      (vector 1.0 1.0 1.0)
                      0.005 8000 1000))

(display "Rendering...\n")
(flush-output-port (current-output-port))

;; Render a single frame
(define frame (render-attractor-colored lorenz-trajectory 80 35 13 0.3 0.6 0))

;; Display it
(display "\x1b;[2J\x1b;[H")  ; clear screen
(display frame)
(display "\n")
(display "Lorenz Attractor - The Butterfly Effect\n")
(display "Points: 8000 | dt: 0.005 | sigma=10, rho=28, beta=8/3\n")
