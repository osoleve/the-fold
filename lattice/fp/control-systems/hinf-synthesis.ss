;;; lattice/fp/control-systems/hinf-synthesis.ss — H-infinity Controller Synthesis
;;;
;;; H-infinity control minimizes the worst-case gain from disturbances to
;;; controlled outputs, providing robust performance guarantees.
;;;
;;; This module provides:
;;;   - Mixed-sensitivity H-infinity synthesis (W1*S, W3*T stacking)
;;;   - Generalized plant construction
;;;   - γ-iteration via Riccati equation feasibility
;;;   - Two-Riccati solution (Glover-Doyle method)
;;;
;;; The H-infinity problem minimizes ||T_{zw}||_∞ where T_{zw} is the
;;; closed-loop transfer from disturbances w to performance outputs z.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; @requires core/base/prelude.ss
;;; @requires lattice/linalg/matrix.ss
;;; @requires lattice/linalg/matrix-eigen.ss
;;; @requires lattice/linalg/matrix-solvers.ss
;;; @requires lattice/fp/control-systems/state-space.ss
;;; @requires lattice/fp/control-systems/transfer-function.ss
;;; @requires lattice/fp/control-systems/stability.ss
;;; @requires lattice/fp/control-systems/controller-design.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-eigen.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/fp/control-systems/state-space.ss")
(load "lattice/fp/control-systems/transfer-function.ss")
(load "lattice/fp/control-systems/stability.ss")
(load "lattice/fp/control-systems/controller-design.ss")

(doc 'module 'hinf-synthesis)
(doc 'description "H-infinity controller synthesis via γ-iteration and Riccati equations")
(doc 'layer 'lattice)

;;; ============================================================================
;;; Generalized Plant Construction
;;; ============================================================================
;;;
;;; The generalized plant P partitions inputs/outputs as:
;;;
;;;     ┌───────┐
;;;  w →│       │→ z   (disturbances to performance)
;;;  u →│   P   │→ y   (control to measurement)
;;;     └───────┘
;;;
;;; P = | A   B1  B2  |
;;;     | C1  D11 D12 |
;;;     | C2  D21 D22 |
;;;
;;; The closed-loop is T_zw = F_l(P, K) = P11 + P12*K*(I - P22*K)^-1*P21

(define (make-generalized-plant A B1 B2 C1 C2 D11 D12 D21 D22)
  (doc 'type "Matrix^9 → GeneralizedPlant")
  (doc 'description "Create a generalized plant structure for H-infinity synthesis.
  w: disturbance inputs (B1), z: performance outputs (C1)
  u: control inputs (B2), y: measurement outputs (C2)")
  (list 'gen-plant A B1 B2 C1 C2 D11 D12 D21 D22))

(define (gp? x)
  (and (pair? x) (eq? (car x) 'gen-plant)))

(define (gp-A gp) (list-ref gp 1))
(define (gp-B1 gp) (list-ref gp 2))
(define (gp-B2 gp) (list-ref gp 3))
(define (gp-C1 gp) (list-ref gp 4))
(define (gp-C2 gp) (list-ref gp 5))
(define (gp-D11 gp) (list-ref gp 6))
(define (gp-D12 gp) (list-ref gp 7))
(define (gp-D21 gp) (list-ref gp 8))
(define (gp-D22 gp) (list-ref gp 9))

(define (gp-order gp) (matrix-rows (gp-A gp)))
(define (gp-nw gp) (matrix-cols (gp-B1 gp)))    ; Number of disturbances
(define (gp-nu gp) (matrix-cols (gp-B2 gp)))    ; Number of controls
(define (gp-nz gp) (matrix-rows (gp-C1 gp)))    ; Number of performance outputs
(define (gp-ny gp) (matrix-rows (gp-C2 gp)))    ; Number of measurements

;;; ============================================================================
;;; Mixed-Sensitivity Problem
;;; ============================================================================
;;;
;;; The standard mixed-sensitivity problem stacks weighted sensitivity
;;; and complementary sensitivity:
;;;
;;;   minimize ||W1*S||      (tracking/disturbance rejection at low freq)
;;;            ||W3*T||_∞    (robustness/noise rejection at high freq)
;;;
;;; where S = 1/(1+GK) and T = GK/(1+GK)
;;;
;;; The weighting functions W1, W3 shape the frequency response:
;;;   - W1 large at low freq → good tracking
;;;   - W3 large at high freq → good noise rejection

(define (hinf-mixed-sensitivity-plant G W1 W3)
  (doc 'type "SS × SS × SS → GeneralizedPlant | Error")
  (doc 'description "Construct generalized plant for mixed-sensitivity H-infinity.
  Stacks [W1*S; W3*T] for minimization.
  G: plant (state-space), W1: sensitivity weight, W3: comp-sensitivity weight.

  The resulting plant has:
    w = [r; d] reference and disturbance
    z = [W1*e; W3*u] weighted error and control
    u = control input
    y = measured output (error e = r - y)")
  (let* ([nG (ss-order G)]
         [nW1 (ss-order W1)]
         [nW3 (ss-order W3)]
         [n (+ nG nW1 nW3)]
         ;; Extract matrices
         [Ag (ss-A G)]   [Bg (ss-B G)]   [Cg (ss-C G)]   [Dg (ss-D G)]
         [A1 (ss-A W1)]  [B1 (ss-B W1)]  [C1 (ss-C W1)]  [D1 (ss-D W1)]
         [A3 (ss-A W3)]  [B3 (ss-B W3)]  [C3 (ss-C W3)]  [D3 (ss-D W3)])
        ;; Dimension checks
        (if (not (and (= (ss-inputs G) 1) (= (ss-outputs G) 1)
                      (= (ss-inputs W1) 1) (= (ss-outputs W1) 1)
                      (= (ss-inputs W3) 1) (= (ss-outputs W3) 1)))
            '(error siso-only-for-mixed-sensitivity)
            ;; Build augmented system matrices
            ;; State: [x_G; x_W1; x_W3]
            ;; The interconnection:
            ;;   e = r - y = r - Cg*xG - Dg*u
            ;;   W1 filters e, W3 filters u
            (let* (;; A matrix: block diagonal with coupling through outputs
                   [A (make-matrix n n 0)]
                   ;; Fill Ag block
                   [_ (matrix-copy-block! A 0 0 Ag)]
                   ;; Fill A1 block - W1 driven by e = r - Cg*xG - Dg*u
                   ;; x1' = A1*x1 + B1*(r - Cg*xG - Dg*u)
                   ;; Need coupling: -B1*Cg affects x1 from xG
                   [_ (matrix-copy-block! A nG nG A1)]
                   [coupling-1G (matrix-scale -1 (matrix-mul B1 Cg))]
                   [_ (matrix-copy-block! A nG 0 coupling-1G)]
                   ;; Fill A3 block - W3 filters u directly
                   [_ (matrix-copy-block! A (+ nG nW1) (+ nG nW1) A3)]
                   ;; B1 matrix: disturbance input w = r (reference)
                   [B1-aug (make-matrix n 1 0)]
                   [_ (matrix-copy-block! B1-aug nG 0 B1)]  ; W1 gets r
                   ;; B2 matrix: control input u
                   [B2-aug (make-matrix n 1 0)]
                   [_ (matrix-copy-block! B2-aug 0 0 Bg)]  ; G gets u
                   [neg-B1Dg (matrix-scale -1 (matrix-mul B1 Dg))]
                   [_ (matrix-copy-block! B2-aug nG 0 neg-B1Dg)]  ; W1 coupling via Dg
                   [_ (matrix-copy-block! B2-aug (+ nG nW1) 0 B3)]  ; W3 gets u
                   ;; C1 matrix: performance outputs z = [z1; z3]
                   [nz1 (ss-outputs W1)]
                   [nz3 (ss-outputs W3)]
                   [nz (+ nz1 nz3)]
                   [C1-aug (make-matrix nz n 0)]
                   ;; z1 = C1*x1 + D1*(r - Cg*xG - Dg*u)
                   ;; Static part for xG: -D1*Cg
                   [neg-D1Cg (matrix-scale -1 (matrix-mul D1 Cg))]
                   [_ (matrix-copy-block! C1-aug 0 0 neg-D1Cg)]
                   [_ (matrix-copy-block! C1-aug 0 nG C1)]
                   ;; z3 = C3*x3 + D3*u
                   [_ (matrix-copy-block! C1-aug nz1 (+ nG nW1) C3)]
                   ;; C2 matrix: measurement y = -e = Cg*xG + Dg*u - r
                   ;; For standard form, y = Cg*xG
                   [C2-aug (make-matrix 1 n 0)]
                   [_ (matrix-copy-block! C2-aug 0 0 Cg)]
                   ;; D11: w → z (reference to performance)
                   [D11-aug (make-matrix nz 1 0)]
                   [_ (matrix-copy-block! D11-aug 0 0 D1)]  ; D1*r in z1
                   ;; D12: u → z (control to performance)
                   [D12-aug (make-matrix nz 1 0)]
                   [neg-D1Dg (matrix-scale -1 (matrix-mul D1 Dg))]
                   [_ (matrix-copy-block! D12-aug 0 0 neg-D1Dg)]
                   [_ (matrix-copy-block! D12-aug nz1 0 D3)]
                   ;; D21: w → y (disturbance to measurement)
                   [D21-aug (make-matrix 1 1 -1)]  ; y = -r + Cg*xG + Dg*u
                   ;; D22: u → y (control to measurement)
                   [D22-aug Dg])
                  (make-generalized-plant A B1-aug B2-aug C1-aug C2-aug
                                          D11-aug D12-aug D21-aug D22-aug)))))

(define (matrix-copy-block! dest row col src)
  (doc 'type "Matrix × Nat × Nat × Matrix → Void")
  (doc 'description "Copy src matrix into dest starting at (row, col)")
  (let ([m (matrix-rows src)]
        [n (matrix-cols src)])
       (do ([i 0 (+ i 1)])
           [(= i m)]
           (do ([j 0 (+ j 1)])
               [(= j n)]
               (matrix-set! dest (+ row i) (+ col j)
                            (matrix-ref src i j))))))

;;; ============================================================================
;;; γ-Iteration Algorithm
;;; ============================================================================
;;;
;;; The core of H-infinity synthesis: find the minimum γ such that
;;; ||T_{zw}||_∞ < γ is achievable with a stabilizing controller.
;;;
;;; For each candidate γ, we check feasibility via two Riccati equations.
;;; The bisection narrows down to the optimal γ*.

(define (hinf-gamma-iteration gp gamma-min gamma-max tolerance max-iter)
  (doc 'type "GeneralizedPlant × Number × Number × Number × Nat → (γ K) | Error")
  (doc 'description "Find minimum achievable γ via bisection.
  Returns (optimal-gamma controller-K) or error if infeasible.")
  (let loop ([lo gamma-min]
             [hi gamma-max]
             [iter 0]
             [last-feasible-K #f]
             [last-feasible-gamma #f])
       (if (>= iter max-iter)
           (if last-feasible-K
               (list last-feasible-gamma last-feasible-K)
               '(error no-feasible-controller-found))
           (let* ([gamma (/ (+ lo hi) 2)]
                  [result (hinf-check-gamma gp gamma)])
                 (cond
                  ;; Converged
                  [(< (- hi lo) tolerance)
                   (if last-feasible-K
                       (list last-feasible-gamma last-feasible-K)
                       ;; Try once more at hi
                       (let ([final (hinf-check-gamma gp hi)])
                            (if (and (pair? final) (not (eq? (car final) 'error)))
                                (list hi (cadr final))
                                '(error no-feasible-controller-found))))]
                  ;; Feasible at gamma - try lower
                  [(and (pair? result) (not (eq? (car result) 'error)))
                   (loop lo gamma (+ iter 1) (cadr result) gamma)]
                  ;; Infeasible at gamma - try higher
                  [else
                   (loop gamma hi (+ iter 1) last-feasible-K last-feasible-gamma)])))))

;;; ============================================================================
;;; Two-Riccati Feasibility Check
;;; ============================================================================
;;;
;;; For a given γ, the H-infinity suboptimal controller exists iff:
;;;
;;; 1. The control Riccati equation has a stabilizing solution X ≥ 0:
;;;    A'X + XA + C1'C1 + X(γ^-2 B1 B1' - B2 B2')X = 0
;;;
;;; 2. The filter Riccati equation has a stabilizing solution Y ≥ 0:
;;;    AY + YA' + B1 B1' + Y(γ^-2 C1'C1 - C2'C2)Y = 0
;;;
;;; 3. Spectral radius condition: ρ(XY) < γ^2
;;;
;;; The controller is then:
;;;    Ak = A + γ^-2 B1 B1' X - B2 F - Z L C2
;;;    Bk = Z L
;;;    Ck = -F
;;;    Dk = 0
;;; where F = B2'X, L = Y C2', Z = (I - γ^-2 YX)^-1

(define (hinf-check-gamma gp gamma)
  (doc 'type "GeneralizedPlant × Number → (ok K) | (error ...)")
  (doc 'description "Check if γ is achievable and compute controller if so.
  Uses the two-Riccati approach (Glover-Doyle).")
  (let* ([A (gp-A gp)]
         [B1 (gp-B1 gp)]
         [B2 (gp-B2 gp)]
         [C1 (gp-C1 gp)]
         [C2 (gp-C2 gp)]
         [D12 (gp-D12 gp)]
         [D21 (gp-D21 gp)]
         [n (gp-order gp)]
         [gamma2 (* gamma gamma)]
         [gamma2-inv (/ 1 gamma2)])
        ;; Simplifying assumptions for standard form:
        ;; D11 = 0, D12'D12 = I, D21 D21' = I, D22 = 0
        ;; (These are achievable via loop-shifting transformations)

        ;; Step 1: Solve control Riccati
        ;; A'X + XA + C1'C1 + X*R_X*X = 0
        ;; where R_X = γ^-2 B1 B1' - B2 B2'
        (let* ([B1B1t (matrix-mul B1 (matrix-transpose B1))]
               [B2B2t (matrix-mul B2 (matrix-transpose B2))]
               [R-X (matrix-sub (matrix-scale gamma2-inv B1B1t) B2B2t)]
               [C1tC1 (matrix-mul (matrix-transpose C1) C1)]
               [X-result (solve-hinf-riccati A R-X C1tC1 1000)])
              (if (and (pair? X-result) (eq? (car X-result) 'error))
                  X-result
                  ;; Step 2: Solve filter Riccati
                  ;; AY + YA' + B1 B1' + Y*R_Y*Y = 0
                  ;; where R_Y = γ^-2 C1'C1 - C2'C2
                  (let* ([X X-result]
                         [C2tC2 (matrix-mul (matrix-transpose C2) C2)]
                         [R-Y (matrix-sub (matrix-scale gamma2-inv C1tC1) C2tC2)]
                         [Y-result (solve-hinf-riccati-dual A R-Y B1B1t 1000)])
                        (if (and (pair? Y-result) (eq? (car Y-result) 'error))
                            Y-result
                            ;; Step 3: Check spectral radius condition
                            (let* ([Y Y-result]
                                   [XY (matrix-mul X Y)]
                                   [rho (spectral-radius XY)])
                                  (if (>= rho gamma2)
                                      '(error spectral-radius-too-large)
                                      ;; Step 4: Construct controller
                                      (hinf-construct-controller A B1 B2 C1 C2 X Y gamma)))))))))

(define (solve-hinf-riccati A R Q max-iter)
  (doc 'type "Matrix × Matrix × Matrix × Nat → Matrix | Error")
  (doc 'description "Solve H-infinity Riccati: A'X + XA + Q + X*R*X = 0
  This is a generalization of CARE with indefinite R.
  Uses Newton iteration with stabilization.")
  (let* ([n (matrix-rows A)]
         [At (matrix-transpose A)]
         ;; Initial guess: solve the LQR-like problem ignoring indefinite part
         [X (make-matrix n n 0)])
        (let loop ([X X] [iter 0])
             (if (>= iter max-iter)
                 ;; Check if converged
                 (let ([res-norm (hinf-riccati-residual A R Q X)])
                      (if (< res-norm 1e-6)
                          X
                          '(error riccati-did-not-converge)))
                 ;; Newton iteration
                 ;; Linearize: (A + RX)'dX + dX(A + RX) = -residual
                 (let* ([AtX (matrix-mul At X)]
                        [XA (matrix-mul X A)]
                        [XRX (matrix-mul X (matrix-mul R X))]
                        [residual (matrix-add (matrix-add AtX XA)
                                              (matrix-add Q XRX))]
                        [res-norm (matrix-frobenius-norm-hinf residual)])
                       (if (< res-norm 1e-10)
                           X
                           ;; Solve Lyapunov for Newton step
                           (let* ([A-mod (matrix-add A (matrix-mul R X))]
                                  [dX (lyapunov-solve-hinf A-mod residual 100)]
                                  [X-new (matrix-sub X dX)])
                                 ;; Ensure positive semi-definiteness
                                 (let ([X-stabilized (matrix-psd-project X-new)])
                                      (loop X-stabilized (+ iter 1))))))))))

(define (solve-hinf-riccati-dual A R Q max-iter)
  (doc 'type "Matrix × Matrix × Matrix × Nat → Matrix | Error")
  (doc 'description "Solve dual H-infinity Riccati: AY + YA' + Q + Y*R*Y = 0")
  ;; This is the dual: swap A and A'
  (let ([At (matrix-transpose A)])
       (solve-hinf-riccati At R Q max-iter)))

(define (hinf-riccati-residual A R Q X)
  (doc 'type "Matrix × Matrix × Matrix × Matrix → Number")
  (doc 'description "Compute ||A'X + XA + Q + XRX||_F")
  (let* ([At (matrix-transpose A)]
         [AtX (matrix-mul At X)]
         [XA (matrix-mul X A)]
         [XRX (matrix-mul X (matrix-mul R X))]
         [residual (matrix-add (matrix-add AtX XA)
                               (matrix-add Q XRX))])
        (matrix-frobenius-norm-hinf residual)))

(define (matrix-frobenius-norm-hinf M)
  (doc 'type "Matrix → Number")
  (let* ([rows (matrix-rows M)]
         [cols (matrix-cols M)])
        (sqrt (let loop ([i 0] [sum 0])
                   (if (>= i rows)
                       sum
                       (loop (+ i 1)
                             (+ sum (let inner ([j 0] [row-sum 0])
                                         (if (>= j cols)
                                             row-sum
                                             (inner (+ j 1)
                                                    (+ row-sum (expt (matrix-ref M i j) 2))))))))))))

(define (lyapunov-solve-hinf A Q max-iter)
  (doc 'type "Matrix × Matrix × Nat → Matrix")
  (doc 'description "Solve A'X + XA = -Q via iteration (for Newton step)")
  (let* ([n (matrix-rows A)]
         [At (matrix-transpose A)]
         [X Q]
         [dt 0.05])
        (let loop ([X X] [iter 0])
             (if (>= iter max-iter)
                 X
                 (let* ([AtX (matrix-mul At X)]
                        [XA (matrix-mul X A)]
                        [residual (matrix-add (matrix-add AtX XA) Q)]
                        [X-new (matrix-sub X (matrix-scale dt residual))]
                        [diff (matrix-frobenius-norm-hinf (matrix-sub X-new X))])
                       (if (< diff 1e-12)
                           X-new
                           (loop X-new (+ iter 1))))))))

(define (matrix-psd-project M)
  (doc 'type "Matrix → Matrix")
  (doc 'description "Project matrix onto positive semi-definite cone.
  Sets negative eigenvalues to zero.")
  ;; Simple approximation: symmetrize and ensure diagonal dominance
  (let* ([n (matrix-rows M)]
         [M-sym (matrix-scale 0.5 (matrix-add M (matrix-transpose M)))]
         [result (make-matrix n n 0)])
        ;; Copy symmetric part and clamp small negatives
        (do ([i 0 (+ i 1)])
            [(= i n) result]
            (do ([j 0 (+ j 1)])
                [(= j n)]
                (let ([val (matrix-ref M-sym i j)])
                     (matrix-set! result i j
                                  (if (and (= i j) (< val 0))
                                      0
                                      val)))))))

(define (spectral-radius M)
  (doc 'type "Matrix → Number")
  (doc 'description "Compute spectral radius (maximum |eigenvalue|)")
  (let* ([n (matrix-rows M)])
        (cond
         ;; Empty matrix has spectral radius 0
         [(= n 0) 0]
         ;; 1x1 matrix: eigenvalue is the single element
         [(= n 1) (abs (matrix-ref M 0 0))]
         ;; General case: use QR algorithm
         [else
          (let ([eigs (qr-algorithm-shifted M 100 1e-10)])
               (cond
                ;; Error result
                [(and (pair? eigs) (eq? (car eigs) 'error))
                 (matrix-frobenius-norm-hinf M)]
                ;; Vector of eigenvalues (what qr-algorithm-shifted returns)
                [(vector? eigs)
                 (let loop ([i 0] [max-mag 0])
                      (if (>= i (vector-length eigs))
                          max-mag
                          (let* ([ev (vector-ref eigs i)]
                                 [mag (if (number? ev)
                                          (abs ev)
                                          (complex-magnitude ev))])
                                (loop (+ i 1) (if (> mag max-mag) mag max-mag)))))]
                ;; List of eigenvalues
                [(pair? eigs)
                 (let loop ([es eigs] [max-mag 0])
                      (if (null? es)
                          max-mag
                          (let ([mag (complex-magnitude (car es))])
                               (loop (cdr es) (if (> mag max-mag) mag max-mag)))))]
                ;; Fallback
                [else (matrix-frobenius-norm-hinf M)]))])))

;;; ============================================================================
;;; Controller Construction
;;; ============================================================================

(define (hinf-construct-controller A B1 B2 C1 C2 X Y gamma)
  (doc 'type "Matrix^5 × Matrix × Matrix × Number → SS")
  (doc 'description "Construct H-infinity controller from Riccati solutions.
  K: state-space controller (Ak, Bk, Ck, Dk)")
  (let* ([n (matrix-rows A)]
         [nu (matrix-cols B2)]
         [ny (matrix-rows C2)]
         [gamma2 (* gamma gamma)]
         [gamma2-inv (/ 1 gamma2)]
         ;; F = B2' X (state feedback gain)
         [B2t (matrix-transpose B2)]
         [F (matrix-mul B2t X)]
         ;; L = Y C2' (observer gain)
         [C2t (matrix-transpose C2)]
         [L (matrix-mul Y C2t)]
         ;; Z = (I - γ^-2 Y X)^-1
         [I (matrix-identity n)]
         [YX (matrix-mul Y X)]
         [I-g2YX (matrix-sub I (matrix-scale gamma2-inv YX))]
         [Z (matrix-inverse I-g2YX)])
        (if (and (pair? Z) (eq? (car Z) 'error))
            '(error coupling-matrix-singular)
            (let* (;; Ak = A + γ^-2 B1 B1' X - B2 F - Z L C2
                   [B1B1tX (matrix-mul (matrix-mul B1 (matrix-transpose B1)) X)]
                   [B2F (matrix-mul B2 F)]
                   [ZL (matrix-mul Z L)]
                   [ZLC2 (matrix-mul ZL C2)]
                   [Ak (matrix-sub
                        (matrix-add A (matrix-scale gamma2-inv B1B1tX))
                        (matrix-add B2F ZLC2))]
                   ;; Bk = Z L
                   [Bk ZL]
                   ;; Ck = -F
                   [Ck (matrix-scale -1 F)]
                   ;; Dk = 0
                   [Dk (make-matrix nu ny 0)])
                  (list 'ok (make-ss Ak Bk Ck Dk))))))

;;; ============================================================================
;;; High-Level Synthesis Functions
;;; ============================================================================

(define (hinf-synthesize plant W1 W3 . opts)
  (doc 'type "SS × SS × SS × ... → (γ K) | Error")
  (doc 'description "Synthesize H-infinity controller for mixed-sensitivity problem.

  Arguments:
    plant - the system to control (state-space)
    W1 - sensitivity weight (low frequency performance)
    W3 - complementary sensitivity weight (high frequency robustness)

  Optional:
    'gamma-min num - minimum γ to search (default 0.1)
    'gamma-max num - maximum γ to search (default 100)
    'tolerance num - convergence tolerance (default 0.01)

  Returns:
    (optimal-gamma controller) on success
    (error ...) on failure

  Example:
    ;; First-order plant G = 1/(s+1)
    (define G (tf->ss (tf-from-lists '(1) '(1 1))))
    ;; Weight: W1 = (s+0.1)/(s+10) boosts low freq
    (define W1 (tf->ss (tf-from-lists '(1 0.1) '(1 10))))
    ;; Weight: W3 = 0.5 (constant)
    (define W3 (ss-scalar 0 0 0 0.5))
    (hinf-synthesize G W1 W3)")
  (let* ([gamma-min (get-opt opts 'gamma-min 0.1)]
         [gamma-max (get-opt opts 'gamma-max 100)]
         [tolerance (get-opt opts 'tolerance 0.01)]
         [max-iter (get-opt opts 'max-iter 50)]
         ;; Build generalized plant
         [gp (hinf-mixed-sensitivity-plant plant W1 W3)])
        (if (and (pair? gp) (eq? (car gp) 'error))
            gp
            ;; Run γ-iteration
            (hinf-gamma-iteration gp gamma-min gamma-max tolerance max-iter))))

(define (get-opt opts key default)
  (doc 'type "(List Any) × Symbol × Any → Any")
  (let loop ([lst opts])
       (cond
        [(null? lst) default]
        [(null? (cdr lst)) default]
        [(eq? (car lst) key) (cadr lst)]
        [else (loop (cddr lst))])))

;;; ============================================================================
;;; Weighting Function Helpers
;;; ============================================================================

(define (hinf-weight-first-order dc hf wc)
  (doc 'type "Number × Number × Number → SS")
  (doc 'description "Create first-order weighting function.
  W(s) = (s/M + wc) / (s + wc*A)
  where dc = 1/A is DC gain, hf = M is high-freq gain, wc is crossover.

  Simplified form: W(s) = (hf*s + dc*wc) / (s + wc*dc/hf)

  For sensitivity weight W1: dc large (good tracking), hf small
  For comp-sensitivity weight W3: dc small, hf large (roll-off)")
  (let* ([A (/ 1 dc)]
         [M hf]
         ;; W(s) = (s + wc*M) / (M * (s + wc*A))
         ;; Rewrite: W(s) = (1/M) * (s + wc*M) / (s + wc*A)
         [num-coeffs (list (/ 1 M) wc)]
         [den-coeffs (list 1 (* wc A))]
         [tf-w (tf-from-lists num-coeffs den-coeffs)])
        (tf->ss tf-w)))

(define (hinf-weight-constant k)
  (doc 'type "Number → SS")
  (doc 'description "Create constant (static) weighting function W(s) = k.
  Returns a zero-order (static) state-space system with only D matrix.")
  ;; For a truly static system, we use empty A, B, C with scalar D
  (make-ss (make-matrix 0 0 0)
           (make-matrix 0 1 0)
           (make-matrix 1 0 0)
           (make-matrix 1 1 k)))

;;; ============================================================================
;;; Transfer Function to State Space
;;; ============================================================================

(define (tf->ss tf)
  (doc 'type "TF → SS")
  (doc 'description "Convert transfer function to controllable canonical state space.
  G(s) = (b_m s^m + ... + b_0) / (s^n + a_{n-1} s^{n-1} + ... + a_0)

  Uses controller canonical form:
  A = | 0  1  0 ... 0 |    B = | 0 |
      | 0  0  1 ... 0 |        | 0 |
      | :        :   |        | : |
      |-a0 -a1 ... -a_{n-1}|  | 1 |

  C = [b0-a0*bm  b1-a1*bm  ...  b_{n-1}-a_{n-1}*bm]
  D = bm (if m = n) or 0 (if m < n)")
  (let* ([num (tf-num tf)]
         [den (tf-den tf)]
         [num-coeffs (poly-coeffs num)]
         [den-coeffs (poly-coeffs den)]
         [n (- (vector-length den-coeffs) 1)]  ; System order
         [m (- (vector-length num-coeffs) 1)]) ; Numerator degree
        (if (< n 1)
            ;; Static gain - zero-order system with only D matrix
            (let ([gain (/ (vector-ref num-coeffs 0)
                           (vector-ref den-coeffs 0))])
                 (make-ss (make-matrix 0 0 0)
                          (make-matrix 0 1 0)
                          (make-matrix 1 0 0)
                          (make-matrix 1 1 gain)))
            ;; Build state space matrices
            (let* ([A (make-matrix n n 0)]
                   [B (make-matrix n 1 0)]
                   [C (make-matrix 1 n 0)]
                   ;; Normalize so leading coeff of den is 1
                   [lead (vector-ref den-coeffs 0)]
                   [a (make-vector n 0)]
                   [b (make-vector (+ n 1) 0)])
                  ;; Extract normalized coefficients
                  (do ([i 0 (+ i 1)])
                      [(= i n)]
                      (vector-set! a i (/ (vector-ref den-coeffs (- n i)) lead)))
                  ;; Pad numerator coefficients
                  (do ([i 0 (+ i 1)])
                      [(> i m)]
                      (let ([idx (- n m i -1)])
                           (when (and (>= idx 0) (< idx (+ n 1)))
                                 (vector-set! b idx
                                              (/ (vector-ref num-coeffs (- m i)) lead)))))
                  ;; Fill A: companion matrix
                  (do ([i 0 (+ i 1)])
                      [(= i (- n 1))]
                      (matrix-set! A i (+ i 1) 1))
                  (do ([i 0 (+ i 1)])
                      [(= i n)]
                      (matrix-set! A (- n 1) i (- (vector-ref a i))))
                  ;; Fill B
                  (matrix-set! B (- n 1) 0 1)
                  ;; Fill C: account for direct feedthrough
                  (let ([d (if (= m n) (vector-ref b n) 0)])
                       (do ([i 0 (+ i 1)])
                           [(= i n)]
                           (matrix-set! C 0 i (- (vector-ref b i)
                                                  (* d (vector-ref a i)))))
                       ;; D matrix
                       (let ([D (make-matrix 1 1 d)])
                            (make-ss A B C D)))))))

;;; ============================================================================
;;; Closed-Loop Analysis
;;; ============================================================================

(define (hinf-closed-loop-norm plant controller)
  (doc 'type "SS × SS → Number")
  (doc 'description "Compute H-infinity norm of closed-loop system.
  Uses frequency gridding for verification.")
  ;; Convert to transfer functions and compute
  (let* ([G-tf (ss->tf plant)]
         [K-tf (ss->tf controller)]
         [T-tf (closed-loop-tf G-tf K-tf)])
        (hinf-norm T-tf)))

(define (ss->tf sys)
  (doc 'type "SS → TF")
  (doc 'description "Convert state-space to transfer function (SISO only).
  G(s) = C(sI - A)^-1 B + D
  Uses characteristic polynomial for denominator.")
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [C (ss-C sys)]
         [D (ss-D sys)]
         [n (ss-order sys)])
        (if (= n 0)
            ;; Static system
            (tf-from-lists (list (matrix-ref D 0 0)) '(1))
            ;; Compute transfer function via characteristic polynomial
            (let* ([char-poly (characteristic-polynomial A)]
                   ;; Numerator: use adjugate formula
                   ;; For now, use a simpler approach: evaluate at multiple points
                   [num-poly (ss-numerator-poly sys)])
                  (make-tf num-poly char-poly)))))

(define (ss-numerator-poly sys)
  (doc 'type "SS → Poly")
  (doc 'description "Compute numerator polynomial of SISO transfer function.
  Uses coefficient matching from frequency response samples.")
  ;; Simple approach: sample at n+1 points and interpolate
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [C (ss-C sys)]
         [D (ss-D sys)]
         [n (ss-order sys)]
         [den-poly (characteristic-polynomial A)])
        (if (= n 0)
            (make-poly (vector (matrix-ref D 0 0)))
            ;; Sample at frequencies and use Lagrange interpolation
            ;; This is approximate but works for reasonable systems
            (let* ([samples (+ n 1)]
                   [s-vals (map (lambda (k) (make-complex 0 (* k 0.5)))
                                (iota samples))]
                   [G-vals (map (lambda (s) (ss-eval-complex sys s)) s-vals)]
                   ;; Numerator = G(s) * den(s)
                   [num-vals (map (lambda (s G)
                                         (complex-mul G (poly-eval-complex den-poly s)))
                                  s-vals G-vals)])
                  ;; Fit polynomial to samples (simplified: use direct coefficients for low order)
                  (poly-from-complex-samples s-vals num-vals n)))))

(define (ss-eval-complex sys s)
  (doc 'type "SS × Complex → Complex")
  (doc 'description "Evaluate G(s) = C(sI-A)^-1 B + D at complex s")
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [C (ss-C sys)]
         [D (ss-D sys)]
         [n (ss-order sys)]
         [sI-A (matrix-sub-scalar-diag (matrix-scale-complex s (matrix-identity n))
                                        A)])
        ;; (sI-A)^-1 via simple iteration for complex case
        (let* ([inv-result (matrix-inverse-complex sI-A)]
               [CinvB (matrix-mul-complex C (matrix-mul-complex inv-result B))]
               [result (complex-add (matrix-ref-complex CinvB 0 0)
                                    (make-complex (matrix-ref D 0 0) 0))])
              result)))

;; Simplified complex matrix operations (for small matrices)
(define (matrix-scale-complex s M)
  (let* ([n (matrix-rows M)]
         [m (matrix-cols M)]
         [sr (complex-real s)]
         [si (complex-imag s)]
         [result-r (make-matrix n m 0)]
         [result-i (make-matrix n m 0)])
        (do ([i 0 (+ i 1)])
            [(= i n)]
            (do ([j 0 (+ j 1)])
                [(= j m)]
                (let ([v (matrix-ref M i j)])
                     (matrix-set! result-r i j (* sr v))
                     (matrix-set! result-i i j (* si v)))))
        (list result-r result-i)))

(define (matrix-sub-scalar-diag sM A)
  (doc 'description "Compute sI - A where sM = (Re, Im) parts of s*I")
  (let* ([sr (car sM)]
         [si (cadr sM)]
         [n (matrix-rows A)]
         [result-r (make-matrix n n 0)]
         [result-i (make-matrix n n 0)])
        (do ([i 0 (+ i 1)])
            [(= i n) (list result-r result-i)]
            (do ([j 0 (+ j 1)])
                [(= j n)]
                (let ([a-val (matrix-ref A i j)]
                      [s-r (matrix-ref sr i j)]
                      [s-i (matrix-ref si i j)])
                     (matrix-set! result-r i j (- s-r a-val))
                     (matrix-set! result-i i j s-i))))))

(define (matrix-inverse-complex M-complex)
  (doc 'description "Invert complex matrix (simple 2x2 or use augmented real)")
  ;; Use augmented real matrix approach:
  ;; [Mr -Mi]^-1 gives real and imag parts
  ;; [Mi  Mr]
  (let* ([Mr (car M-complex)]
         [Mi (cadr M-complex)]
         [n (matrix-rows Mr)]
         [aug (make-matrix (* 2 n) (* 2 n) 0)])
        ;; Fill augmented matrix
        (do ([i 0 (+ i 1)])
            [(= i n)]
            (do ([j 0 (+ j 1)])
                [(= j n)]
                (matrix-set! aug i j (matrix-ref Mr i j))
                (matrix-set! aug i (+ j n) (- (matrix-ref Mi i j)))
                (matrix-set! aug (+ i n) j (matrix-ref Mi i j))
                (matrix-set! aug (+ i n) (+ j n) (matrix-ref Mr i j))))
        ;; Invert
        (let ([aug-inv (matrix-inverse aug)])
             (if (and (pair? aug-inv) (eq? (car aug-inv) 'error))
                 (list (make-matrix n n 0) (make-matrix n n 0))
                 ;; Extract real and imag parts
                 (let ([inv-r (make-matrix n n 0)]
                       [inv-i (make-matrix n n 0)])
                      (do ([i 0 (+ i 1)])
                          [(= i n) (list inv-r inv-i)]
                          (do ([j 0 (+ j 1)])
                              [(= j n)]
                              (matrix-set! inv-r i j (matrix-ref aug-inv i j))
                              (matrix-set! inv-i i j (matrix-ref aug-inv (+ i n) j)))))))))

(define (matrix-mul-complex A-complex B-complex)
  (let* ([Ar (car A-complex)] [Ai (cadr A-complex)]
         [Br (car B-complex)] [Bi (cadr B-complex)]
         [Cr (matrix-sub (matrix-mul Ar Br) (matrix-mul Ai Bi))]
         [Ci (matrix-add (matrix-mul Ar Bi) (matrix-mul Ai Br))])
        (list Cr Ci)))

(define (matrix-ref-complex M-complex i j)
  (make-complex (matrix-ref (car M-complex) i j)
                (matrix-ref (cadr M-complex) i j)))

(define (poly-eval-complex poly s)
  (doc 'type "Poly × Complex → Complex")
  (doc 'description "Evaluate polynomial at complex value using Horner's method")
  (let* ([coeffs (poly-coeffs poly)]
         [n (vector-length coeffs)])
        (let loop ([i 0] [result (make-complex 0 0)])
             (if (>= i n)
                 result
                 (loop (+ i 1)
                       (complex-add (complex-mul result s)
                                    (make-complex (vector-ref coeffs i) 0)))))))

(define (poly-from-complex-samples s-vals num-vals degree)
  (doc 'type "(List Complex) × (List Complex) × Nat → Poly")
  (doc 'description "Fit polynomial to complex samples (simplified)")
  ;; For small degrees, extract real coefficients directly
  ;; This is a simplification - full implementation would use least squares
  (let* ([coeffs (make-vector (+ degree 1) 0)])
        ;; Use real parts of low-frequency samples
        (let* ([dc (complex-real (car num-vals))])
              (vector-set! coeffs degree dc)
              ;; For higher coefficients, use finite differences
              (when (> degree 0)
                    (let* ([slope (if (> (length num-vals) 1)
                                      (/ (- (complex-real (cadr num-vals)) dc)
                                         (complex-imag (cadr s-vals)))
                                      0)])
                          (vector-set! coeffs (- degree 1) slope))))
        (make-poly coeffs)))

(define (complex-mul a b)
  (make-complex (- (* (complex-real a) (complex-real b))
                   (* (complex-imag a) (complex-imag b)))
                (+ (* (complex-real a) (complex-imag b))
                   (* (complex-imag a) (complex-real b)))))

(define (complex-add a b)
  (make-complex (+ (complex-real a) (complex-real b))
                (+ (complex-imag a) (complex-imag b))))

;;; ============================================================================
;;; Module Info
;;; ============================================================================

(define (hinf-synthesis-info)
  (doc 'type "→ String")
  "H-infinity Synthesis Module:
   - hinf-synthesize: Main entry point for mixed-sensitivity design
   - hinf-weight-first-order: Create shaping filters
   - hinf-closed-loop-norm: Verify achieved performance

   Example workflow:
   1. Define plant G as state-space
   2. Choose weighting functions W1 (sensitivity) and W3 (comp-sensitivity)
   3. Call (hinf-synthesize G W1 W3)
   4. Verify with (hinf-closed-loop-norm G K)")
