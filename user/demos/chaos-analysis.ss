(load "lattice/sim/dynamics/chaos.ss")

;; Chaos Analysis Demo
(display "CHAOS ANALYSIS - Lyapunov Exponents\n")
(display "===================================\n\n")

;; Test Lorenz system
(display "Lorenz System (sigma=10, rho=28, beta=8/3)\n")
(display "------------------------------------------\n")
(let ([lyap (largest-lyapunov-exponent lorenz-classic (vector 1.0 1.0 1.0) 0.01 3000 500)])
     (display "  Largest Lyapunov exponent: ")
     (display lyap)
     (display "\n  Status: ")
     (display (if (> lyap 0) "CHAOTIC" "Non-chaotic"))
     (display "\n  Expected: ~0.9 (chaotic)\n\n"))

;; Test Rossler system
(display "Rossler System (a=0.2, b=0.2, c=5.7)\n")
(display "------------------------------------\n")
(let ([lyap (largest-lyapunov-exponent rossler-classic (vector 1.0 1.0 1.0) 0.02 2000 500)])
     (display "  Largest Lyapunov exponent: ")
     (display lyap)
     (display "\n  Status: ")
     (display (if (> lyap 0) "CHAOTIC" "Non-chaotic"))
     (display "\n  Expected: ~0.07 (chaotic)\n\n"))

;; Test harmonic oscillator (should be non-chaotic)
(display "Harmonic Oscillator (omega=1)\n")
(display "-----------------------------\n")
(let ([lyap (largest-lyapunov-exponent (harmonic-oscillator 1.0) (vector 1.0 0.0) 0.01 2000 500)])
     (display "  Largest Lyapunov exponent: ")
     (display lyap)
     (display "\n  Status: ")
     (display (if (> lyap 0.01) "CHAOTIC" "Non-chaotic (or marginally stable)"))
     (display "\n  Expected: ~0 (periodic, not chaotic)\n\n"))

;; Kaplan-Yorke dimension for Lorenz
(display "Kaplan-Yorke Dimension\n")
(display "----------------------\n")
(display "  Known Lorenz exponents: (0.9, 0, -14.6)\n")
(let ([ky (kaplan-yorke-dimension '(0.9 0.0 -14.6))])
     (display "  Kaplan-Yorke dimension: ")
     (display ky)
     (display "\n  Expected: ~2.06 (fractal dimension between 2 and 3)\n"))
