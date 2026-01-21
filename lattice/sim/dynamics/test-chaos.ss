(load "core/testing/test-framework.ss")
(load "lattice/sim/dynamics/chaos.ss")

(test-group "chaos-module"

  (define-test "rk4-step advances state correctly"
    (let* ([sys (harmonic-oscillator 1.0)]
           [state0 (vector 1.0 0.0)]  ; x=1, v=0
           [state1 (rk4-step sys 0 state0 0.01)])
          (assert-true (> (abs (vector-ref state1 0)) 0.99))  ; position changed slightly
          (assert-true (< (vector-ref state1 1) 0))))        ; velocity became negative

  (define-test "integrate-rk4 produces trajectory"
    (let* ([sys (harmonic-oscillator 1.0)]
           [traj (integrate-rk4 sys 0 (vector 1.0 0.0) 0.01 100)])
          (assert-equal 101 (length traj))  ; 100 steps + initial
          (assert-true (vector? (car traj)))))

  (define-test "lorenz-system has correct dimension"
    (assert-equal 3 (ode-dimension lorenz-classic)))

  (define-test "rossler-system has correct dimension"
    (assert-equal 3 (ode-dimension rossler-classic)))

  (define-test "generate-attractor removes transient"
    (let* ([traj (generate-attractor lorenz-classic
                                     (vector 1.0 1.0 1.0)
                                     0.01 100 50)])
          (assert-equal 100 (length traj))))

  (define-test "attractor-bounds computes bounding box"
    (let* ([traj (list (vector 0.0 1.0 2.0)
                       (vector 1.0 0.0 3.0)
                       (vector -1.0 2.0 1.0))]
           [bounds (attractor-bounds traj)])
          (assert-equal 3 (length bounds))
          (assert-equal -1.0 (caar bounds))   ; x min
          (assert-equal 1.0 (cdar bounds))    ; x max
          (assert-equal 0.0 (caadr bounds))   ; y min
          (assert-equal 2.0 (cdadr bounds)))) ; y max

  (define-test "normalize-trajectory centers and scales"
    (let* ([traj (list (vector 0.0 0.0 0.0)
                       (vector 2.0 2.0 2.0))]
           [bounds (attractor-bounds traj)]
           [norm (normalize-trajectory traj bounds 1.0)])
          (assert-equal 2 (length norm))
          ;; First point should be at -1,-1,-1 after centering
          (assert-true (< (abs (+ 1.0 (vector-ref (car norm) 0))) 0.01))))

  (define-test "gram-schmidt orthonormalizes vectors"
    (let* ([v1 (vector 1.0 0.0 0.0)]
           [v2 (vector 1.0 1.0 0.0)]
           [v3 (vector 1.0 1.0 1.0)]
           [result (gram-schmidt-orthonormalize (list v1 v2 v3))]
           [ortho (car result)]
           [norms (cdr result)])
          (assert-equal 3 (length ortho))
          (assert-equal 3 (length norms))
          ;; First vector should be unchanged (already unit)
          (assert-true (< (abs (- 1.0 (vector-ref (car ortho) 0))) 0.01))
          ;; Vectors should be orthogonal
          (assert-true (< (abs (vec-dot (car ortho) (cadr ortho))) 0.01))))

  (define-test "kaplan-yorke-dimension for Lorenz is around 2.06"
    ;; Known Lyapunov exponents for Lorenz: ~0.9, 0, ~-14.6
    (let* ([lyaps '(0.9 0.0 -14.6)]
           [ky-dim (kaplan-yorke-dimension lyaps)])
          ;; Should be around 2 + 0.9/14.6 ≈ 2.06
          (assert-true (> ky-dim 2.0))
          (assert-true (< ky-dim 2.2))))

  (define-test "poincare-section-2d projects correctly"
    (let* ([points (list (vector 1.0 2.0 3.0)
                         (vector 4.0 5.0 6.0))]
           [proj (poincare-section-2d points '(0 2))])
          (assert-equal 2 (length proj))
          (assert-equal 1.0 (caar proj))
          (assert-equal 3.0 (cdar proj))))

)

(run-all-tests)
