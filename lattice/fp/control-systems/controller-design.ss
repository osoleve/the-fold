;;; lattice/fp/control-systems/controller-design.ss — Controller Design
;;;
;;; Design and synthesis of controllers for linear time-invariant systems.
;;;
;;; This module provides:
;;;   - PID controller design (tuning methods)
;;;   - Pole placement / state feedback
;;;   - Observer design (full-order and reduced-order)
;;;   - LQR (Linear Quadratic Regulator)
;;;   - LQG (LQR with Kalman filter)
;;;   - Basic H-infinity concepts
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/linalg/matrix.ss
;;;   - lattice/linalg/matrix-eigen.ss
;;;   - lattice/linalg/matrix-solvers.ss
;;;   - lattice/fp/control-systems/state-space.ss
;;;   - lattice/fp/control-systems/transfer-function.ss
;;;   - lattice/fp/control-systems/stability.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-eigen.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/fp/control-systems/state-space.ss")
(load "lattice/fp/control-systems/transfer-function.ss")
(load "lattice/fp/control-systems/stability.ss")

;;; ============================================================
;;; PID Controller Design
;;; ============================================================

;;; PID transfer function: C(s) = Kp + Ki/s + Kd*s
;;;                            = (Kd*s^2 + Kp*s + Ki) / s
;;;
;;; Various tuning methods are provided.

;;; pid-tf : Number × Number × Number → TF
;;; Create PID controller as transfer function.
;;; Parameters: Kp (proportional), Ki (integral), Kd (derivative)
(define (pid-tf Kp Ki Kd)
  (if (zero? Kd)
      (if (zero? Ki)
          ;; P only: C(s) = Kp
          (tf-from-lists (list Kp) '(1))
          ;; PI: C(s) = (Kp*s + Ki) / s
          (tf-from-lists (list Kp Ki) '(1 0)))
      ;; PID: C(s) = (Kd*s^2 + Kp*s + Ki) / s
      (tf-from-lists (list Kd Kp Ki) '(1 0))))

;;; pid-design-zn-open-loop : Number × Number × Number → (Kp Ki Kd)
;;; Ziegler-Nichols open-loop tuning from step response.
;;; Parameters:
;;;   K  = process gain (steady-state gain)
;;;   L  = dead time (delay)
;;;   T  = time constant
(define (pid-design-zn-open-loop K L T)
  (let* ([Kp (* (/ 1.2 K) (/ T L))]
         [Ti (* 2 L)]
         [Td (* 0.5 L)]
         [Ki (/ Kp Ti)]
         [Kd (* Kp Td)])
        (list Kp Ki Kd)))

;;; pid-design-zn-closed-loop : Number × Number → (Kp Ki Kd)
;;; Ziegler-Nichols closed-loop (ultimate gain) tuning.
;;; Parameters:
;;;   Ku = ultimate gain (at oscillation)
;;;   Tu = oscillation period
(define (pid-design-zn-closed-loop Ku Tu)
  (let* ([Kp (* 0.6 Ku)]
         [Ti (* 0.5 Tu)]
         [Td (* 0.125 Tu)]
         [Ki (/ Kp Ti)]
         [Kd (* Kp Td)])
        (list Kp Ki Kd)))

;;; pid-design-imc : TF × Number → (Kp Ki Kd)
;;; Internal Model Control (IMC) based PID tuning.
;;; Works for first-order plus dead time (FOPDT) models.
;;; Parameters:
;;;   plant = first-order plant transfer function K/(Ts+1)
;;;   lambda = desired closed-loop time constant
(define (pid-design-imc plant lambda)
  (let* ([poles (tf-poles plant)]
         [zeros (tf-zeros plant)]
         ;; Extract FOPDT parameters
         [K (tf-dc-gain plant)]
         [tau (if (null? poles)
                  1.0  ; Default
                  (- (/ 1 (complex-real (car poles)))))])  ; T = -1/pole
        (if (eq? K 'infinite)
            '(error integrating-process)
            (let* ([Kp (/ tau (* K lambda))]
                   [Ki (/ Kp tau)]
                   [Kd 0])  ; No derivative for basic IMC
                  (list Kp Ki Kd)))))

;;; pid-design-lambda : Number × Number × Number × Number → (Kp Ki Kd)
;;; Lambda tuning method (IMC variant).
;;; Parameters:
;;;   K = process gain
;;;   tau = process time constant
;;;   theta = dead time
;;;   lambda = desired closed-loop time constant (typically = theta)
(define (pid-design-lambda K tau theta lambda)
  (let* ([Kp (/ tau (* K (+ lambda theta)))]
         [Ti tau]
         [Ki (/ Kp Ti)])
        (list Kp Ki 0)))

;;; ============================================================
;;; State Feedback / Pole Placement
;;; ============================================================

;;; State feedback: u = -K*x where K is the feedback gain matrix.
;;; Closed-loop system: x' = (A - B*K)*x
;;; Goal: place eigenvalues of (A-BK) at desired locations.

;;; pole-placement-ackermann : SS × (List Complex) → Matrix | Error
;;; Compute state feedback gain K using Ackermann's formula.
;;; Places closed-loop poles at specified locations.
;;; Only works for SISO systems (single input).
;;;
;;; Formula: K = [0 0 ... 0 1] * inv(C_n) * p(A)
;;; where C_n is controllability matrix and p(s) = desired char poly
(define (pole-placement-ackermann sys desired-poles)
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [n (ss-order sys)]
         [m (matrix-cols B)])
        (if (not (= m 1))
            '(error ackermann-siso-only)
            ;; Check controllability
            (let ([C-mat (controllability-matrix sys)])
                 (if (not (= (stability-matrix-rank C-mat) n))
                     '(error system-not-controllable)
                     ;; Compute desired characteristic polynomial
                     (let* ([char-poly (poly-from-roots desired-poles 1)]
                            ;; Evaluate p(A)
                            [pA (poly-eval-matrix char-poly A)]
                            ;; Compute C_n inverse
                            [C-inv (matrix-inverse C-mat)]
                            ;; K = e_n' * C_inv * p(A)
                            [e-n (make-row-vector n (- n 1))]
                            [temp (matrix-mul e-n C-inv)]
                            [K (matrix-mul temp pA)])
                           K))))))

;;; make-row-vector : Nat × Nat → Matrix
;;; Create row vector with 1 at position idx, 0 elsewhere.
(define (make-row-vector n idx)
  (let ([result (make-matrix 1 n 0)])
       (matrix-set! result 0 idx 1)
       result))

;;; poly-eval-matrix : Poly × Matrix → Matrix
;;; Evaluate polynomial at matrix argument.
;;; p(A) = a_n*A^n + ... + a_1*A + a_0*I
(define (poly-eval-matrix poly A)
  (let* ([coeffs (poly-coeffs poly)]
         [n (matrix-rows A)]
         [I (identity n)]
         [deg (- (vector-length coeffs) 1)])
        (if (= deg 0)
            (matrix-scale (vector-ref coeffs 0) I)
            ;; Horner's method for matrix polynomial
            (let loop ([i 0] [result (matrix-scale (vector-ref coeffs 0) I)])
                 (if (>= i deg)
                     result
                     (loop (+ i 1)
                           (matrix-add (matrix-mul A result)
                                       (matrix-scale (vector-ref coeffs (+ i 1)) I))))))))

;;; pole-placement-bass-gura : SS × (List Complex) → Matrix | Error
;;; Compute state feedback gain using Bass-Gura formula.
;;; More numerically stable than Ackermann for higher-order systems.
(define (pole-placement-bass-gura sys desired-poles)
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [n (ss-order sys)]
         [m (matrix-cols B)])
        (if (not (= m 1))
            '(error bass-gura-siso-only)
            ;; Get open-loop and desired characteristic polynomials
            (let* ([open-char-poly (characteristic-polynomial A)]
                   [des-char-poly (poly-from-roots desired-poles 1)]
                   ;; Compute difference in coefficients
                   [alpha-open (poly-coeffs-vector open-char-poly n)]
                   [alpha-des (poly-coeffs-vector des-char-poly n)]
                   ;; Delta = desired - open coefficients
                   [delta (vector-sub alpha-des alpha-open)]
                   ;; Controllability matrix
                   [C-mat (controllability-matrix sys)]
                   [C-inv (matrix-inverse C-mat)]
                   ;; Build transformation matrix W
                   [W (bass-gura-W alpha-open n)]
                   ;; K = delta' * W * C_inv
                   [WC-inv (matrix-mul W C-inv)]
                   [K (matrix-mul (vector->row-matrix delta) WC-inv)])
                  K))))

;;; characteristic-polynomial : Matrix → Poly
;;; Compute characteristic polynomial det(sI - A).
;;; Uses Faddeev-LeVerrier method.
(define (characteristic-polynomial A)
  (let* ([n (matrix-rows A)]
         [coeffs (make-vector (+ n 1) 0)]
         [M (identity n)])
        (vector-set! coeffs 0 1)  ; Leading coefficient
        (let loop ([k 1] [M M])
             (if (> k n)
                 (make-poly coeffs)
                 (let* ([AM (matrix-mul A M)]
                        [trace-AM (matrix-trace AM)]
                        [c-k (/ (- trace-AM) k)]
                        [M-new (if (= k n)
                                   M
                                   (matrix-add AM (matrix-scale c-k (identity n))))])
                       (vector-set! coeffs k c-k)
                       (loop (+ k 1) M-new))))))

;;; matrix-trace : Matrix → Number
(define (matrix-trace M)
  (let ([n (min (matrix-rows M) (matrix-cols M))])
       (let loop ([i 0] [sum 0])
            (if (>= i n)
                sum
                (loop (+ i 1) (+ sum (matrix-ref M i i)))))))

;;; poly-coeffs-vector : Poly × Nat → Vector
;;; Extract coefficient vector of length n (padded with zeros).
(define (poly-coeffs-vector poly n)
  (let* ([coeffs (poly-coeffs poly)]
         [len (vector-length coeffs)]
         [result (make-vector n 0)])
        ;; Copy from end (constant term) to beginning
        (do ([i 0 (+ i 1)])
            ((or (>= i n) (>= i len)))
            (let ([src-idx (- len 1 i)]
                  [dst-idx (- n 1 i)])
                 (when (and (>= src-idx 0) (>= dst-idx 0))
                       (vector-set! result dst-idx (vector-ref coeffs src-idx)))))
        result))

;;; vector-sub : Vector × Vector → Vector
(define (vector-sub v1 v2)
  (let* ([n (vector-length v1)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (- (vector-ref v1 i) (vector-ref v2 i))))))

;;; vector->row-matrix : Vector → Matrix
(define (vector->row-matrix v)
  (let* ([n (vector-length v)]
         [result (make-matrix 1 n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (matrix-set! result 0 i (vector-ref v i)))))

;;; bass-gura-W : Vector × Nat → Matrix
;;; Build the W transformation matrix for Bass-Gura.
(define (bass-gura-W alpha n)
  (let ([W (make-matrix n n 0)])
       ;; W is upper triangular with specific pattern
       (do ([i 0 (+ i 1)])
           ((= i n) W)
           (matrix-set! W i i 1)
           (do ([j (+ i 1) (+ j 1)])
               ((= j n))
               (matrix-set! W i j (vector-ref alpha (- j i)))))))

;;; ============================================================
;;; Observer Design
;;; ============================================================

;;; Observer (state estimator): x_hat' = A*x_hat + B*u + L*(y - C*x_hat)
;;;                                    = (A - L*C)*x_hat + B*u + L*y
;;; Goal: place eigenvalues of (A-LC) for fast estimation.
;;; By duality: observer gain L is computed like feedback gain K
;;; for the dual system (A', C', B').

;;; observer-design-ackermann : SS × (List Complex) → Matrix | Error
;;; Design observer gain L using Ackermann's formula on dual system.
;;; desired-poles are for the observer (typically 2-10x faster than controller).
(define (observer-design-ackermann sys desired-poles)
  (let* ([A (ss-A sys)]
         [C (ss-C sys)]
         [n (ss-order sys)]
         [p (matrix-rows C)])
        (if (not (= p 1))
            '(error ackermann-siso-only)
            ;; Build dual system: (A', C') where
            ;; A_dual = A^T (n x n)
            ;; B_dual = C^T (n x 1 for SISO)
            (let* ([At (matrix-transpose A)]
                   [Ct (matrix-transpose C)]
                   ;; For the dual system, C_dual and D_dual just need valid dimensions
                   ;; C_dual: 1 x n, D_dual: 1 x 1
                   [C-dual (make-matrix 1 n 0)]
                   [D-dual (make-matrix 1 1 0)]
                   [dual-sys (make-ss At Ct C-dual D-dual)]
                   ;; Design feedback for dual system
                   [Kt-result (pole-placement-ackermann dual-sys desired-poles)])
                  (if (and (pair? Kt-result) (eq? (car Kt-result) 'error))
                      Kt-result
                      ;; L = K^T
                      (matrix-transpose Kt-result))))))

;;; observer-design-place : SS × (List Complex) → Matrix | Error
;;; Design observer gain by direct pole placement.
(define (observer-design-place sys desired-poles)
  (observer-design-ackermann sys desired-poles))

;;; ============================================================
;;; LQR - Linear Quadratic Regulator
;;; ============================================================

;;; LQR minimizes: J = integral(x'Qx + u'Ru) dt
;;; Solution: u = -K*x where K = R^{-1}*B'*P
;;; P satisfies the continuous algebraic Riccati equation (CARE):
;;;   A'P + PA - PBR^{-1}B'P + Q = 0

;;; lqr : SS × Matrix × Matrix → (K P eigenvalues) | Error
;;; Compute LQR controller gain.
;;; Parameters:
;;;   sys = state space system
;;;   Q   = state weighting matrix (n×n, positive semi-definite)
;;;   R   = control weighting matrix (m×m, positive definite)
;;; Returns: K (gain), P (Riccati solution), closed-loop eigenvalues
(define (lqr sys Q R)
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [n (ss-order sys)]
         [m (matrix-cols B)]
         ;; Solve CARE using iterative method
         [P (solve-care A B Q R 1000)])
        (if (and (pair? P) (eq? (car P) 'error))
            P
            (let* ([R-inv (matrix-inverse R)]
                   [Bt (matrix-transpose B)]
                   [K (matrix-mul R-inv (matrix-mul Bt P))]
                   ;; Closed-loop eigenvalues
                   [Acl (matrix-sub A (matrix-mul B K))]
                   [cl-eigs (if (= n 1)
                                (list (make-complex (matrix-ref Acl 0 0) 0))
                                (qr-algorithm-shifted Acl 100 1e-10))])
                  (list K P cl-eigs)))))

;;; solve-care : Matrix × Matrix × Matrix × Matrix × Nat → Matrix | Error
;;; Solve continuous algebraic Riccati equation using Newton iteration.
;;; A'P + PA - PBR^{-1}B'P + Q = 0
;;; Uses direct Lyapunov solver for small systems (n < 15), iterative for larger.
(define (solve-care A B Q R max-iter)
  (let* ([n (matrix-rows A)]
         [At (matrix-transpose A)]
         [Bt (matrix-transpose B)]
         [R-inv (matrix-inverse R)]
         [BRinvBt (matrix-mul B (matrix-mul R-inv Bt))]
         ;; Initial guess: P = Q
         [P Q]
         ;; Use direct Lyapunov solver for small systems
         [use-direct? (< n 15)])
        ;; Newton iteration
        (let loop ([P P] [iter 0])
             (if (>= iter max-iter)
                 P
                 ;; Compute residual: AtP + PA - PBR^{-1}B'P + Q
                 (let* ([AtP (matrix-mul At P)]
                        [PA (matrix-mul P A)]
                        [PBRP (matrix-mul P (matrix-mul BRinvBt P))]
                        [residual (matrix-add (matrix-add AtP PA)
                                              (matrix-sub Q PBRP))]
                        [norm (matrix-frobenius-norm-local residual)])
                       (if (< norm 1e-10)
                           P
                           ;; Solve Lyapunov equation for Newton step
                           ;; (A - BR^{-1}B'P)'*dP + dP*(A - BR^{-1}B'P) = -residual
                           (let* ([A-mod (matrix-sub A (matrix-mul BRinvBt P))]
                                  ;; Use direct solver for stability on small systems
                                  [dP (if use-direct?
                                          (lyapunov-solve A-mod residual)
                                          (lyapunov-solve-iterative A-mod residual 100))]
                                  [P-new (matrix-add P dP)])
                                 (loop P-new (+ iter 1)))))))))

;;; ============================================================
;;; Direct Lyapunov Equation Solver
;;; ============================================================
;;;
;;; Solves A'X + XA = -Q using vectorization:
;;; (I ⊗ A' + A' ⊗ I) vec(X) = -vec(Q)
;;;
;;; This is O(n^6) but numerically stable, suitable for n < 20.

;;; kronecker-product : Matrix × Matrix → Matrix
;;; Compute Kronecker product A ⊗ B
(define (kronecker-product A B)
  (let* ([pa (matrix-rows A)]
         [qa (matrix-cols A)]
         [pb (matrix-rows B)]
         [qb (matrix-cols B)]
         [result (make-matrix (* pa pb) (* qa qb) 0)])
        (do ([i 0 (+ i 1)])
            [(= i pa) result]
            (do ([j 0 (+ j 1)])
                [(= j qa)]
                (let ([a-ij (matrix-ref A i j)])
                     (do ([k 0 (+ k 1)])
                         [(= k pb)]
                         (do ([l 0 (+ l 1)])
                             [(= l qb)]
                             (matrix-set! result
                                          (+ (* i pb) k)
                                          (+ (* j qb) l)
                                          (* a-ij (matrix-ref B k l))))))))))

;;; matrix-vec : Matrix → Vector
;;; Vectorize matrix column-major: vec([a1 a2 ... an]) = [a1; a2; ...; an]
(define (matrix-vec M)
  (let* ([rows (matrix-rows M)]
         [cols (matrix-cols M)]
         [result (make-vector (* rows cols) 0)])
        (do ([j 0 (+ j 1)])
            [(= j cols) result]
            (do ([i 0 (+ i 1)])
                [(= i rows)]
                (vector-set! result (+ (* j rows) i)
                             (matrix-ref M i j))))))

;;; matrix-unvec : Vector × Nat × Nat → Matrix
;;; Inverse of vec: reshape vector to m×n matrix (column-major)
(define (matrix-unvec v m n)
  (let ([result (make-matrix m n 0)])
       (do ([j 0 (+ j 1)])
           [(= j n) result]
           (do ([i 0 (+ i 1)])
               [(= i m)]
               (matrix-set! result i j
                            (vector-ref v (+ (* j m) i)))))))

;;; lyapunov-solve : Matrix × Matrix → Matrix
;;; Solve continuous Lyapunov equation: A'X + XA = -Q
;;; Uses direct vectorization method for stability.
(define (lyapunov-solve A Q)
  (let* ([n (matrix-rows A)]
         [At (matrix-transpose A)]
         [I (identity n)]
         ;; Build coefficient matrix: (I ⊗ A') + (A' ⊗ I)
         [kron1 (kronecker-product I At)]
         [kron2 (kronecker-product At I)]
         [coeff (matrix-add kron1 kron2)]
         ;; Right-hand side: -vec(Q)
         [rhs-vec (matrix-vec Q)]
         [rhs (make-vector (* n n) 0)])
        ;; Negate RHS
        (do ([i 0 (+ i 1)])
            [(= i (* n n))]
            (vector-set! rhs i (- (vector-ref rhs-vec i))))
        ;; Convert to matrix for solving
        (let* ([rhs-mat (make-matrix (* n n) 1 0)])
              (do ([i 0 (+ i 1)])
                  [(= i (* n n))]
                  (matrix-set! rhs-mat i 0 (vector-ref rhs i)))
              ;; Solve linear system
              (let ([x-mat (matrix-solve-lu coeff rhs-mat)])
                   (if (and (pair? x-mat) (eq? (car x-mat) 'error))
                       ;; Fall back to iterative if direct fails
                       (lyapunov-solve-iterative A Q 1000)
                       ;; Reshape solution to n×n matrix
                       (let ([x-vec (make-vector (* n n) 0)])
                            (do ([i 0 (+ i 1)])
                                [(= i (* n n))]
                                (vector-set! x-vec i (matrix-ref x-mat i 0)))
                            (matrix-unvec x-vec n n)))))))

;;; matrix-solve-lu : Matrix × Matrix → Matrix | Error
;;; Solve A*X = B using LU decomposition
(define (matrix-solve-lu A B)
  (guard (e [else `(error lu-solve-failed)])
         (let ([lu-result (matrix-lu A)])
              (if (and (pair? lu-result) (eq? (car lu-result) 'error))
                  lu-result
                  (let* ([L (car lu-result)]
                         [U (cadr lu-result)]
                         [P (caddr lu-result)]
                         [Pb (matrix-mul P B)]
                         [Y (forward-sub L Pb)]
                         [X (back-sub U Y)])
                        X)))))

;;; forward-sub : Matrix × Matrix → Matrix
;;; Solve L*Y = B for lower triangular L
(define (forward-sub L B)
  (let* ([n (matrix-rows L)]
         [m (matrix-cols B)]
         [Y (make-matrix n m 0)])
        (do ([j 0 (+ j 1)])
            [(= j m) Y]
            (do ([i 0 (+ i 1)])
                [(= i n)]
                (let* ([sum (let loop ([k 0] [acc 0])
                                 (if (>= k i)
                                     acc
                                     (loop (+ k 1)
                                           (+ acc (* (matrix-ref L i k)
                                                     (matrix-ref Y k j))))))]
                       [diag (matrix-ref L i i)]
                       [val (/ (- (matrix-ref B i j) sum)
                               (if (zero? diag) 1e-10 diag))])
                      (matrix-set! Y i j val))))))

;;; back-sub : Matrix × Matrix → Matrix
;;; Solve U*X = Y for upper triangular U
(define (back-sub U Y)
  (let* ([n (matrix-rows U)]
         [m (matrix-cols Y)]
         [X (make-matrix n m 0)])
        (do ([j 0 (+ j 1)])
            [(= j m) X]
            (do ([i (- n 1) (- i 1)])
                [(< i 0)]
                (let* ([sum (let loop ([k (+ i 1)] [acc 0])
                                 (if (>= k n)
                                     acc
                                     (loop (+ k 1)
                                           (+ acc (* (matrix-ref U i k)
                                                     (matrix-ref X k j))))))]
                       [diag (matrix-ref U i i)]
                       [val (/ (- (matrix-ref Y i j) sum)
                               (if (zero? diag) 1e-10 diag))])
                      (matrix-set! X i j val))))))

;;; lyapunov-solve-iterative : Matrix × Matrix × Nat → Matrix
;;; Fallback iterative Lyapunov solver: A'X + XA = -Q
;;; Uses Smith iteration for improved convergence.
(define (lyapunov-solve-iterative A Q max-iter)
  (let* ([n (matrix-rows A)]
         [At (matrix-transpose A)]
         ;; Initial guess using stationary iteration
         [X Q])
        (let loop ([X X] [iter 0])
             (if (>= iter max-iter)
                 X
                 (let* ([AtX (matrix-mul At X)]
                        [XA (matrix-mul X A)]
                        [residual (matrix-add (matrix-add AtX XA) Q)]
                        [norm (matrix-frobenius-norm-local residual)])
                       (if (< norm 1e-10)
                           X
                           ;; Use residual as correction (scaled gradient descent)
                           (let* ([alpha 0.1]  ; Step size
                                  [X-new (matrix-sub X (matrix-scale alpha residual))])
                                 (loop X-new (+ iter 1)))))))))

;;; lyapunov-solve-simple : Matrix × Matrix × Nat → Matrix
;;; DEPRECATED: Use lyapunov-solve instead for better numerical properties.
;;; Simple iterative Lyapunov solver: A'X + XA = -Q
(define (lyapunov-solve-simple A Q max-iter)
  (lyapunov-solve-iterative A Q max-iter))

;;; matrix-frobenius-norm-local : Matrix → Number
(define (matrix-frobenius-norm-local M)
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

;;; ============================================================
;;; Discrete LQR
;;; ============================================================

;;; Discrete LQR minimizes: J = sum(x'Qx + u'Ru)
;;; Solution: u[k] = -K*x[k] where K = (R + B'PB)^{-1}*B'PA
;;; P satisfies discrete algebraic Riccati equation (DARE):
;;;   P = A'PA - A'PB(R + B'PB)^{-1}B'PA + Q

;;; dlqr : SS × Matrix × Matrix → (K P eigenvalues) | Error
;;; Compute discrete LQR controller gain.
(define (dlqr sys Q R)
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [n (ss-order sys)]
         [m (matrix-cols B)]
         ;; Solve DARE using iteration
         [P (solve-dare A B Q R 1000)])
        (if (and (pair? P) (eq? (car P) 'error))
            P
            (let* ([Bt (matrix-transpose B)]
                   [BtPB (matrix-mul Bt (matrix-mul P B))]
                   [temp-inv (matrix-inverse (matrix-add R BtPB))]
                   [BtPA (matrix-mul Bt (matrix-mul P A))]
                   [K (matrix-mul temp-inv BtPA)]
                   ;; Closed-loop eigenvalues
                   [Acl (matrix-sub A (matrix-mul B K))]
                   [cl-eigs (if (= n 1)
                                (list (make-complex (matrix-ref Acl 0 0) 0))
                                (qr-algorithm-shifted Acl 100 1e-10))])
                  (list K P cl-eigs)))))

;;; solve-dare : Matrix × Matrix × Matrix × Matrix × Nat → Matrix | Error
;;; Solve discrete algebraic Riccati equation using iteration.
(define (solve-dare A B Q R max-iter)
  (let* ([At (matrix-transpose A)]
         [Bt (matrix-transpose B)]
         [P Q])
        (let loop ([P P] [iter 0])
             (if (>= iter max-iter)
                 P
                 (let* ([BtPB (matrix-mul Bt (matrix-mul P B))]
                        [temp-inv (matrix-inverse (matrix-add R BtPB))]
                        [BtPA (matrix-mul Bt (matrix-mul P A))]
                        [K-temp (matrix-mul temp-inv BtPA)]
                        [AtPA (matrix-mul At (matrix-mul P A))]
                        [AtPB-K (matrix-mul (matrix-mul At (matrix-mul P B)) K-temp)]
                        [P-new (matrix-add Q (matrix-sub AtPA AtPB-K))]
                        [diff (matrix-frobenius-norm-local (matrix-sub P-new P))])
                       (if (< diff 1e-10)
                           P-new
                           (loop P-new (+ iter 1))))))))

;;; ============================================================
;;; LQG - Linear Quadratic Gaussian
;;; ============================================================

;;; LQG combines LQR state feedback with matrix Kalman filter state estimation.
;;; This is the full MATRIX Kalman filter for state-space control systems.
;;; (For scalar Kalman filtering, see kalman.ss)
;;;
;;; Controller: u = -K*x_hat
;;; Estimator: x_hat' = A*x_hat + B*u + L*(y - C*x_hat)
;;;
;;; Design:
;;; 1. Design LQR gain K using Q, R
;;; 2. Design Kalman filter gain L using Qn (process noise), Rn (measurement noise)

;;; lqg : SS × Matrix × Matrix × Matrix × Matrix → (K L) | Error
;;; Design LQG controller with optimal state estimation.
;;; Parameters:
;;;   sys = state space system
;;;   Q, R = LQR weights (state and control)
;;;   Qn, Rn = Noise covariances (process and measurement)
(define (lqg sys Q R Qn Rn)
  (let* (;; Design LQR controller
         [lqr-result (lqr sys Q R)])
        (if (and (pair? lqr-result) (eq? (car lqr-result) 'error))
            lqr-result
            (let ([K (car lqr-result)])
                 ;; Design Kalman filter (dual LQR problem)
                 ;; For Kalman filter: A' plays role of A, C' plays role of B
                 (let* ([A (ss-A sys)]
                        [C (ss-C sys)]
                        [n (matrix-rows A)]
                        [At (matrix-transpose A)]
                        [Ct (matrix-transpose C)]
                        ;; Solve filter Riccati: A*P*A' - A*P*C'*(C*P*C' + Rn)^{-1}*C*P*A' + Qn = P
                        ;; Or equivalently solve dual CARE for L
                        [P-filter (solve-care At Ct Qn Rn 1000)])
                       (if (and (pair? P-filter) (eq? (car P-filter) 'error))
                           P-filter
                           (let* ([Rn-inv (matrix-inverse Rn)]
                                  [L (matrix-mul P-filter (matrix-mul Ct Rn-inv))])
                                 (list K L))))))))

;;; ============================================================
;;; H-infinity Control Basics
;;; ============================================================

;;; H-infinity control minimizes the H-infinity norm of a closed-loop
;;; transfer function, providing robust performance guarantees.
;;;
;;; The H-infinity norm is: ||G||_inf = sup_w |G(jw)|
;;; For a state-space system, this requires solving a pair of Riccati equations.

;;; hinf-norm : TF → Number | Error
;;; Compute H-infinity norm of a transfer function.
;;; Uses bisection on frequency response.
(define (hinf-norm tf)
  (let* ([w-test (logspace -4 4 1000)]
         [mags (tf-magnitude tf w-test)]
         [max-mag (vector-max mags)])
        max-mag))

;;; vector-max : Vector → Number
(define (vector-max v)
  (let ([n (vector-length v)])
       (let loop ([i 1] [max-val (vector-ref v 0)])
            (if (>= i n)
                max-val
                (let ([val (vector-ref v i)])
                     (loop (+ i 1) (if (> val max-val) val max-val)))))))

;;; hinf-bounded? : TF × Number → Boolean
;;; Check if H-infinity norm is bounded by gamma.
(define (hinf-bounded? tf gamma)
  (< (hinf-norm tf) gamma))

;;; ============================================================
;;; Compensator Synthesis
;;; ============================================================

;;; closed-loop-tf : TF × TF → TF
;;; Compute closed-loop transfer function for unity negative feedback.
;;; T(s) = G(s)C(s) / (1 + G(s)C(s))
(define (closed-loop-tf plant controller)
  (let* ([GC (tf-series plant controller)]
         [num-GC (tf-num GC)]
         [den-GC (tf-den GC)]
         ;; T = GC / (1 + GC) = num_GC / (den_GC + num_GC)
         [new-den (poly-add den-GC num-GC)])
        (make-tf num-GC new-den)))

;;; sensitivity-tf : TF × TF → TF
;;; Compute sensitivity transfer function S(s) = 1/(1 + G(s)C(s)).
(define (sensitivity-tf plant controller)
  (let* ([GC (tf-series plant controller)]
         [num-GC (tf-num GC)]
         [den-GC (tf-den GC)]
         ;; S = 1 / (1 + GC) = den_GC / (den_GC + num_GC)
         [new-den (poly-add den-GC num-GC)])
        (make-tf den-GC new-den)))

;;; complementary-sensitivity-tf : TF × TF → TF
;;; Compute complementary sensitivity T(s) = G(s)C(s)/(1 + G(s)C(s)).
;;; Note: S + T = 1
(define (complementary-sensitivity-tf plant controller)
  (closed-loop-tf plant controller))

;;; ============================================================
;;; Lead/Lag Compensator Design
;;; ============================================================

;;; lead-compensator : Number × Number × Number → TF
;;; Create a lead compensator: C(s) = Kc * (s + z)/(s + p) where z < p.
;;; Lead compensators add phase lead at crossover frequency.
;;; Parameters: Kc = gain, zero = magnitude of zero (zero at s = -zero), pole = magnitude of pole
(define (lead-compensator Kc zero pole)
  ;; C(s) = Kc * (s + zero) / (s + pole)
  ;; Numerator: Kc*s + Kc*zero → coeffs (Kc, Kc*zero)
  ;; Denominator: s + pole → coeffs (1, pole)
  (tf-from-lists (list Kc (* Kc zero))
                 (list 1 pole)))

;;; lag-compensator : Number × Number × Number → TF
;;; Create a lag compensator: C(s) = Kc * (s + z)/(s + p) where z > p.
;;; Lag compensators increase low-frequency gain.
;;; Parameters: Kc = gain, zero = magnitude of zero, pole = magnitude of pole
(define (lag-compensator Kc zero pole)
  ;; C(s) = Kc * (s + zero) / (s + pole)
  (tf-from-lists (list Kc (* Kc zero))
                 (list 1 pole)))

;;; lead-lag-compensator : Number × Number × Number × Number × Number → TF
;;; Create lead-lag compensator (product of lead and lag sections).
(define (lead-lag-compensator Kc z-lead p-lead z-lag p-lag)
  (tf-series (lead-compensator 1 z-lead p-lead)
             (lag-compensator Kc z-lag p-lag)))

;;; ============================================================
;;; Utility
;;; ============================================================

;;; controller-info : String
(define (controller-info)
  "Controller Design Module: PID design, pole placement, LQR/LQG, observer design")
