(load "lattice/sim/dynamics/attractor-render.ss")

;; Generate Rössler attractor
(display "ROSSLER ATTRACTOR\n")
(display "=================\n\n")

(let* ([traj (generate-attractor rossler-classic
                                 (vector 1.0 1.0 1.0)
                                 0.02 6000 500)]
       [frame (render-attractor-colored traj 70 25 10 0.3 0.7 0)])
      (display frame))

(display "\nComputing Lyapunov exponent...\n")
(let ([lyap (largest-lyapunov-exponent rossler-classic (vector 1.0 1.0 1.0) 0.02 2000 500)])
     (display "Largest Lyapunov exponent: ")
     (display lyap)
     (display (if (> lyap 0) " (CHAOTIC)\n" " (non-chaotic)\n")))
