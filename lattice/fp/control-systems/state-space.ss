;;; core/fp/control-systems/state-space.ss — State Space Models
;;;
;;; State space representation of linear time-invariant (LTI) systems.
;;;
;;; A continuous-time state space model is:
;;;   x'(t) = A*x(t) + B*u(t)   (state equation)
;;;   y(t)  = C*x(t) + D*u(t)   (output equation)
;;;
;;; where:
;;;   x(t) is the n×1 state vector
;;;   u(t) is the m×1 input vector
;;;   y(t) is the p×1 output vector
;;;   A is the n×n state (system) matrix
;;;   B is the n×m input matrix
;;;   C is the p×n output matrix
;;;   D is the p×m feedthrough matrix
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/prelude.ss
;;;   - core/matrix.ss
;;;   - core/matrix-decomp.ss (for some operations)

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-eigen.ss")  ; Required for SVD
(load "lattice/linalg/svd.ss")            ; SVD for robust rank computation

;;; ============================================================
;;; State Space Representation
;;; ============================================================

;;; A state space system is: (ss A B C D)
;;; where A, B, C, D are matrices with compatible dimensions.

;;; ss? : Any → Boolean
(define (ss? sys)
  (and (pair? sys)
       (eq? (car sys) 'ss)
       (= (length sys) 5)
       (matrix? (cadr sys))      ; A
       (matrix? (caddr sys))     ; B
       (matrix? (cadddr sys))    ; C
       (matrix? (car (cddddr sys))))) ; D

;;; Accessors

;;; ss-A : SS → Matrix
(define (ss-A sys) (cadr sys))

;;; ss-B : SS → Matrix
(define (ss-B sys) (caddr sys))

;;; ss-C : SS → Matrix
(define (ss-C sys) (cadddr sys))

;;; ss-D : SS → Matrix
(define (ss-D sys) (car (cddddr sys)))

;;; ss-order : SS → Nat
;;; Get the order (number of states) of the system.
(define (ss-order sys)
  (matrix-rows (ss-A sys)))

;;; ss-inputs : SS → Nat
;;; Get the number of inputs.
(define (ss-inputs sys)
  (matrix-cols (ss-B sys)))

;;; ss-outputs : SS → Nat
;;; Get the number of outputs.
(define (ss-outputs sys)
  (matrix-rows (ss-C sys)))

;;; ============================================================
;;; State Space Construction
;;; ============================================================

;;; make-ss : Matrix × Matrix × Matrix × Matrix → SS | Error
;;; Create a state space system, validating dimensions.
(define (make-ss A B C D)
  (let ([n (matrix-rows A)])
       (cond
        ;; Check A is square
        [(not (= n (matrix-cols A)))
         `(error A-not-square ,(matrix-shape A))]
        ;; Check B has n rows
        [(not (= n (matrix-rows B)))
         `(error B-rows-mismatch ,(matrix-rows B) ,n)]
        ;; Check C has n columns
        [(not (= n (matrix-cols C)))
         `(error C-cols-mismatch ,(matrix-cols C) ,n)]
        ;; Check D dimensions match B cols and C rows
        [(not (= (matrix-rows D) (matrix-rows C)))
         `(error D-rows-mismatch ,(matrix-rows D) ,(matrix-rows C))]
        [(not (= (matrix-cols D) (matrix-cols B)))
         `(error D-cols-mismatch ,(matrix-cols D) ,(matrix-cols B))]
        [else
         (list 'ss A B C D)])))

;;; ss-from-lists : (List (List Number)) × (List (List Number)) × (List (List Number)) × (List (List Number)) → SS
;;; Create state space from nested lists.
(define (ss-from-lists A-lists B-lists C-lists D-lists)
  (make-ss (matrix-from-lists A-lists)
           (matrix-from-lists B-lists)
           (matrix-from-lists C-lists)
           (matrix-from-lists D-lists)))

;;; ============================================================
;;; Basic State Space Operations
;;; ============================================================

;;; ss-state-equation : SS × Vec × Vec → Vec
;;; Compute x' = A*x + B*u
(define (ss-state-equation sys x u)
  (vec-add (matrix-vec-mul (ss-A sys) x)
           (matrix-vec-mul (ss-B sys) u)))

;;; ss-output-equation : SS × Vec × Vec → Vec
;;; Compute y = C*x + D*u
(define (ss-output-equation sys x u)
  (vec-add (matrix-vec-mul (ss-C sys) x)
           (matrix-vec-mul (ss-D sys) u)))

;;; ============================================================
;;; State Transition Matrix
;;; ============================================================

;;; ============================================================
;;; Matrix Exponential - Scaling and Squaring with Padé
;;; ============================================================
;;;
;;; The Scaling and Squaring algorithm computes e^A as follows:
;;; 1. Find s such that ||A/2^s|| < 1 (scaling)
;;; 2. Compute e^(A/2^s) using Padé approximant
;;; 3. Square the result s times: e^A = (e^(A/2^s))^(2^s)
;;;
;;; This is much more numerically stable than Taylor series.

;;; *pade-coeffs-6* : Coefficients for [6,6] diagonal Padé approximant
;;; N(X) = sum_{k=0}^6 b_k * X^k, D(X) = sum_{k=0}^6 b_k * (-X)^k
(define *pade-coeffs-6*
  ;; Coefficients: b_k = (2n-k)! * n! / ((2n)! * (n-k)! * k!) for n=6
  ;; Precomputed for efficiency
  (list 1                     ; b_0 = 1
        1/2                   ; b_1 = 1/2
        1/10                  ; b_2 = 1/10
        1/120                 ; b_3 = 1/120
        1/1680                ; b_4 = 1/1680
        1/30240               ; b_5 = 1/30240
        1/665280))            ; b_6 = 1/665280

;;; matrix-1norm : Matrix → Real
;;; Compute 1-norm (max column sum of absolute values)
(define (matrix-1norm m)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)])
       (let col-loop ([j 0] [max-sum 0])
            (if (>= j cols)
                max-sum
                (let row-loop ([i 0] [col-sum 0])
                     (if (>= i rows)
                         (col-loop (+ j 1) (max max-sum col-sum))
                         (row-loop (+ i 1)
                                   (+ col-sum (abs (matrix-ref m i j))))))))))

;;; matrix-pade-6 : Matrix → (Matrix . Matrix)
;;; Compute [6,6] Padé numerator N and denominator D matrices.
;;; Returns (N . D) where e^A ≈ D^(-1) * N
(define (matrix-pade-6 A)
  (let* ([n (matrix-rows A)]
         [I (identity n)]
         [A2 (matrix-mul A A)]
         [A4 (matrix-mul A2 A2)]
         [A6 (matrix-mul A4 A2)]
         [b (list-ref *pade-coeffs-6* 0)]  ; 1
         [b1 (list-ref *pade-coeffs-6* 1)] ; 1/2
         [b2 (list-ref *pade-coeffs-6* 2)] ; 1/10
         [b3 (list-ref *pade-coeffs-6* 3)] ; 1/120
         [b4 (list-ref *pade-coeffs-6* 4)] ; 1/1680
         [b5 (list-ref *pade-coeffs-6* 5)] ; 1/30240
         [b6 (list-ref *pade-coeffs-6* 6)] ; 1/665280
         ;; U = A * (b1*I + b3*A² + b5*A⁴) + b6*A⁶ ... wait, let me restructure
         ;; N = I + b1*A + b2*A² + b3*A³ + b4*A⁴ + b5*A⁵ + b6*A⁶
         ;; D = I - b1*A + b2*A² - b3*A³ + b4*A⁴ - b5*A⁵ + b6*A⁶
         ;; Efficient: compute even and odd parts separately
         [A3 (matrix-mul A2 A)]
         [A5 (matrix-mul A4 A)]
         ;; Even terms: I + b2*A² + b4*A⁴ + b6*A⁶
         [even-terms (matrix-add I
                                 (matrix-add (matrix-scale b2 A2)
                                             (matrix-add (matrix-scale b4 A4)
                                                         (matrix-scale b6 A6))))]
         ;; Odd terms: b1*A + b3*A³ + b5*A⁵
         [odd-terms (matrix-add (matrix-scale b1 A)
                                (matrix-add (matrix-scale b3 A3)
                                            (matrix-scale b5 A5)))]
         ;; N = even + odd, D = even - odd
         [N (matrix-add even-terms odd-terms)]
         [D (matrix-sub even-terms odd-terms)])
        (cons N D)))

;;; matrix-exp : Matrix → Matrix
;;; Compute matrix exponential using Scaling and Squaring with [6,6] Padé.
;;; Numerically robust for a wide range of matrices.
(define (matrix-exp A)
  (let* ([n (matrix-rows A)]
         [norm-A (matrix-1norm A)]
         ;; Scale so that ||A/2^s|| <= 1/2 (ensures Padé convergence)
         ;; s = max(0, ceil(log2(2*norm-A)))
         [s (if (<= norm-A 0.5)
                0
                (+ 1 (exact (floor (log (inexact norm-A) 2)))))]
         [scale-factor (expt 2 s)]
         [A-scaled (matrix-scale (/ 1 scale-factor) A)]
         ;; Compute Padé approximant for scaled matrix
         [pade-result (matrix-pade-6 A-scaled)]
         [N (car pade-result)]
         [D (cdr pade-result)]
         ;; Solve D * X = N for X (X = D^(-1) * N)
         [exp-scaled (matrix-solve-system D N)])
        ;; Square s times: e^A = (e^(A/2^s))^(2^s)
        (let square ([k 0] [result exp-scaled])
             (if (>= k s)
                 result
                 (square (+ k 1) (matrix-mul result result))))))

;;; matrix-solve-system : Matrix × Matrix → Matrix
;;; Solve A * X = B for X using LU decomposition.
;;; Returns X = A^(-1) * B
(define (matrix-solve-system A B)
  (let* ([n (matrix-rows A)]
         [m (matrix-cols B)]
         ;; Use LU decomposition to solve
         [lu-result (matrix-lu A)])
        (if (and (pair? lu-result) (eq? (car lu-result) 'error))
            ;; LU failed, fall back to direct inverse
            (matrix-mul (matrix-inverse A) B)
            ;; lu-result is (L U P) - solve using forward/back substitution
            (let* ([L (car lu-result)]
                   [U (cadr lu-result)]
                   [P (caddr lu-result)]
                   [Pb (matrix-mul P B)])
                  ;; Solve L * Y = P*B, then U * X = Y
                  (let ([Y (forward-substitute L Pb)])
                       (back-substitute U Y))))))

;;; forward-substitute : Matrix × Matrix → Matrix
;;; Solve L * Y = B where L is lower triangular
(define (forward-substitute L B)
  (let* ([n (matrix-rows L)]
         [m (matrix-cols B)]
         [Y (make-matrix n m 0)])
        (do ([j 0 (+ j 1)])
            [(= j m) Y]
            (do ([i 0 (+ i 1)])
                [(= i n)]
                (let ([sum (let loop ([k 0] [acc 0])
                                (if (>= k i)
                                    acc
                                    (loop (+ k 1)
                                          (+ acc (* (matrix-ref L i k)
                                                    (matrix-ref Y k j))))))])
                     (matrix-set! Y i j
                                  (/ (- (matrix-ref B i j) sum)
                                     (matrix-ref L i i))))))))

;;; back-substitute : Matrix × Matrix → Matrix
;;; Solve U * X = Y where U is upper triangular
(define (back-substitute U Y)
  (let* ([n (matrix-rows U)]
         [m (matrix-cols Y)]
         [X (make-matrix n m 0)])
        (do ([j 0 (+ j 1)])
            [(= j m) X]
            (do ([i (- n 1) (- i 1)])
                [(< i 0)]
                (let ([sum (let loop ([k (+ i 1)] [acc 0])
                                (if (>= k n)
                                    acc
                                    (loop (+ k 1)
                                          (+ acc (* (matrix-ref U i k)
                                                    (matrix-ref X k j))))))])
                     (matrix-set! X i j
                                  (/ (- (matrix-ref Y i j) sum)
                                     (matrix-ref U i i))))))))

;;; matrix-exp-taylor : Matrix × Nat → Matrix
;;; DEPRECATED: Use matrix-exp instead for numerical stability.
;;; Compute matrix exponential using Taylor series.
;;; e^A = I + A + A²/2! + A³/3! + ...
;;; fuel limits number of terms.
(define (matrix-exp-taylor A fuel)
  (let* ([n (matrix-rows A)]
         [I (identity n)]
         [result I]
         [term I]
         [factorial 1])
        (let loop ([k 1] [term term] [result result] [factorial 1])
             (if (>= k fuel)
                 result
                 (let* ([new-term (matrix-scale (/ 1 (* factorial k)) (matrix-mul term A))]
                        [new-factorial (* factorial k)])
                       (loop (+ k 1)
                             (matrix-mul term A)
                             (matrix-add result new-term)
                             new-factorial))))))

;;; ss-transition-matrix : SS × Num → Matrix
;;; Compute the state transition matrix Φ(t) = e^(A*t)
;;; using numerically robust Scaling and Squaring with Padé.
(define (ss-transition-matrix sys t)
  (matrix-exp (matrix-scale t (ss-A sys))))

;;; ss-transition-matrix-taylor : SS × Num × Nat → Matrix
;;; DEPRECATED: Use ss-transition-matrix instead.
;;; Compute the state transition matrix using Taylor series.
(define (ss-transition-matrix-taylor sys t terms)
  (matrix-exp-taylor (matrix-scale t (ss-A sys)) terms))

;;; ============================================================
;;; Controllability
;;; ============================================================

;;; ss-controllability-matrix : SS → Matrix
;;; Compute the controllability matrix C = [B AB A²B ... A^(n-1)B]
(define (ss-controllability-matrix sys)
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [n (ss-order sys)]
         [m (ss-inputs sys)])
        ;; Build controllability matrix column-by-column
        (let build ([k 0] [A-power (identity n)] [cols '()])
             (if (>= k n)
                 ;; Combine all columns into result matrix
                 (let* ([total-cols (* n m)]
                        [result (make-matrix n total-cols 0)])
                       (let fill-cols ([col-list cols] [col-idx 0])
                            (if (null? col-list)
                                result
                                (begin
                                 (do ([i 0 (+ i 1)])
                                     [(= i n)]
                                     (matrix-set! result i col-idx
                                                  (matrix-ref (car col-list) i 0)))
                                 (fill-cols (cdr col-list) (+ col-idx 1))))))
                 ;; A^k * B, extract each column
                 (let* ([AkB (matrix-mul A-power B)]
                        [new-cols (let extract-cols ([j 0] [acc '()])
                                       (if (>= j m)
                                           (reverse acc)
                                           (extract-cols (+ j 1)
                                                         (cons (matrix-column-as-matrix AkB j) acc))))])
                       (build (+ k 1)
                              (matrix-mul A-power A)
                              (append cols new-cols)))))))

;;; matrix-column-as-matrix : Matrix × Nat → Matrix (n×1)
;;; Extract column j as an n×1 matrix.
(define (matrix-column-as-matrix m j)
  (let* ([n (matrix-rows m)]
         [result (make-matrix n 1 0)])
        (do ([i 0 (+ i 1)])
            [(= i n) result]
            (matrix-set! result i 0 (matrix-ref m i j)))))

;;; ss-controllable? : SS × Num → Boolean
;;; Check if system is controllable (controllability matrix has full row rank).
;;; Uses tolerance for numerical rank check.
(define (ss-controllable? sys tolerance)
  (let* ([C-mat (ss-controllability-matrix sys)]
         [n (ss-order sys)]
         [rank (matrix-rank C-mat tolerance)])
        (= rank n)))

;;; ============================================================
;;; Observability
;;; ============================================================

;;; ss-observability-matrix : SS → Matrix
;;; Compute the observability matrix O = [C; CA; CA²; ...; CA^(n-1)]
(define (ss-observability-matrix sys)
  (let* ([A (ss-A sys)]
         [C (ss-C sys)]
         [n (ss-order sys)]
         [p (ss-outputs sys)])
        ;; Build observability matrix row-by-row
        (let build ([k 0] [A-power (identity n)] [rows '()])
             (if (>= k n)
                 ;; Combine all rows into result matrix
                 (let* ([total-rows (* n p)]
                        [result (make-matrix total-rows n 0)])
                       (let fill-rows ([row-list (reverse rows)] [row-idx 0])
                            (if (null? row-list)
                                result
                                (let ([curr-mat (car row-list)])
                                     (do ([i 0 (+ i 1)])
                                         [(= i p)]
                                         (do ([j 0 (+ j 1)])
                                             [(= j n)]
                                             (matrix-set! result (+ row-idx i) j
                                                          (matrix-ref curr-mat i j))))
                                     (fill-rows (cdr row-list) (+ row-idx p))))))
                 ;; C * A^k
                 (let ([CAk (matrix-mul C A-power)])
                      (build (+ k 1)
                             (matrix-mul A-power A)
                             (cons CAk rows)))))))

;;; ss-observable? : SS × Num → Boolean
;;; Check if system is observable (observability matrix has full column rank).
(define (ss-observable? sys tolerance)
  (let* ([O-mat (ss-observability-matrix sys)]
         [n (ss-order sys)]
         [rank (matrix-rank O-mat tolerance)])
        (= rank n)))

;;; ============================================================
;;; Gramians
;;; ============================================================

;;; For continuous-time systems, the controllability Gramian Wc satisfies:
;;;   A*Wc + Wc*A' + B*B' = 0  (Lyapunov equation)
;;;
;;; The observability Gramian Wo satisfies:
;;;   A'*Wo + Wo*A + C'*C = 0  (Lyapunov equation)
;;;
;;; For discrete-time systems:
;;;   A*Wc*A' - Wc + B*B' = 0
;;;   A'*Wo*A - Wo + C'*C = 0

;;; ss-controllability-gramian-finite : SS × Nat → Matrix
;;; Compute finite-horizon controllability Gramian by direct sum.
;;; Wc = sum_{k=0}^{N-1} A^k * B * B' * (A')^k
(define (ss-controllability-gramian-finite sys N)
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [n (ss-order sys)]
         [Wc (make-matrix n n 0)])
        (let loop ([k 0] [Ak (identity n)])
             (if (>= k N)
                 Wc
                 (let* ([AkB (matrix-mul Ak B)]
                        [term (matrix-mul AkB (matrix-transpose AkB))])
                       ;; Add term to Wc
                       (do ([i 0 (+ i 1)])
                           [(= i n)]
                           (do ([j 0 (+ j 1)])
                               [(= j n)]
                               (matrix-set! Wc i j
                                            (+ (matrix-ref Wc i j)
                                               (matrix-ref term i j)))))
                       (loop (+ k 1) (matrix-mul Ak A)))))))

;;; ss-observability-gramian-finite : SS × Nat → Matrix
;;; Compute finite-horizon observability Gramian by direct sum.
;;; Wo = sum_{k=0}^{N-1} (A')^k * C' * C * A^k
(define (ss-observability-gramian-finite sys N)
  (let* ([A (ss-A sys)]
         [C (ss-C sys)]
         [n (ss-order sys)]
         [Wo (make-matrix n n 0)]
         [At (matrix-transpose A)]
         [Ct (matrix-transpose C)])
        (let loop ([k 0] [Ak (identity n)] [Atk (identity n)])
             (if (>= k N)
                 Wo
                 (let* ([CtC (matrix-mul Ct C)]
                        [term (matrix-mul Atk (matrix-mul CtC Ak))])
                       ;; Add term to Wo
                       (do ([i 0 (+ i 1)])
                           [(= i n)]
                           (do ([j 0 (+ j 1)])
                               [(= j n)]
                               (matrix-set! Wo i j
                                            (+ (matrix-ref Wo i j)
                                               (matrix-ref term i j)))))
                       (loop (+ k 1)
                             (matrix-mul Ak A)
                             (matrix-mul Atk At)))))))

;;; ============================================================
;;; Matrix Rank (for controllability/observability)
;;; ============================================================
;;;
;;; SVD-based rank is the gold standard for numerical rank determination.
;;; Falls back to QR if SVD fails.

;;; matrix-rank-svd : Matrix × Num → Nat
;;; Compute numerical rank using SVD (gold standard).
;;; Counts singular values that exceed tolerance.
(define (matrix-rank-svd m tolerance)
  (guard (e [else (matrix-rank-qr m tolerance)])  ; Fall back to QR on error
         (let ([svd-result (svd m)])
              (if (and (pair? svd-result) (eq? (car svd-result) 'error))
                  (matrix-rank-qr m tolerance)
                  (let* ([sigma (cadr svd-result)]
                         [min-dim (min (matrix-rows sigma) (matrix-cols sigma))])
                        (let count ([i 0] [rank 0])
                             (if (>= i min-dim)
                                 rank
                                 (if (> (abs (matrix-ref sigma i i)) tolerance)
                                     (count (+ i 1) (+ rank 1))
                                     (count (+ i 1) rank)))))))))

;;; matrix-rank-qr : Matrix × Num → Nat
;;; Compute numerical rank using QR decomposition (faster but less robust).
;;; Counts diagonal elements of R that exceed tolerance.
(define (matrix-rank-qr m tolerance)
  (let ([qr-result (matrix-qr m)])
       (if (and (pair? qr-result) (eq? (car qr-result) 'error))
           0
           (let* ([R (cadr qr-result)]
                  [min-dim (min (matrix-rows R) (matrix-cols R))])
                 (let count ([i 0] [rank 0])
                      (if (>= i min-dim)
                          rank
                          (if (> (abs (matrix-ref R i i)) tolerance)
                              (count (+ i 1) (+ rank 1))
                              (count (+ i 1) rank))))))))

;;; matrix-rank : Matrix × Num → Nat
;;; Compute numerical rank. Uses SVD for robustness.
(define (matrix-rank m tolerance)
  (matrix-rank-svd m tolerance))

;;; ============================================================
;;; Modal Decomposition (Diagonalization)
;;; ============================================================

;;; For a diagonalizable system, there exists a transformation T such that:
;;;   Ā = T⁻¹AT is diagonal (eigenvalues on diagonal)
;;;   B̄ = T⁻¹B
;;;   C̄ = CT
;;;   D̄ = D
;;;
;;; This decouples the state equations into independent modes.
;;;
;;; Note: Full modal decomposition requires eigenvalue computation,
;;; which is complex. Here we provide a simplified version that
;;; applies a given transformation matrix.

;;; ss-transform : SS × Matrix × Matrix → SS
;;; Apply similarity transformation: new states z = T⁻¹x
;;; Returns transformed system (T⁻¹AT, T⁻¹B, CT, D)
(define (ss-transform sys T T-inv)
  (let ([A (ss-A sys)]
        [B (ss-B sys)]
        [C (ss-C sys)]
        [D (ss-D sys)])
       (make-ss (matrix-mul T-inv (matrix-mul A T))
                (matrix-mul T-inv B)
                (matrix-mul C T)
                D)))

;;; ============================================================
;;; Common State Space Forms
;;; ============================================================

;;; ss-identity-output : Matrix × Matrix → SS
;;; Create a system with identity C (full state output) and no feedthrough.
(define (ss-identity-output A B)
  (let* ([n (matrix-rows A)]
         [m (matrix-cols B)]
         [C (identity n)]
         [D (make-matrix n m 0)])
        (make-ss A B C D)))

;;; ss-scalar : Num × Num × Num × Num → SS
;;; Create a first-order scalar system.
;;; x' = a*x + b*u
;;; y = c*x + d*u
(define (ss-scalar a b c d)
  (make-ss (matrix-from-lists (list (list a)))
           (matrix-from-lists (list (list b)))
           (matrix-from-lists (list (list c)))
           (matrix-from-lists (list (list d)))))

;;; ss-integrator : Nat → SS
;;; Create an n-th order integrator chain.
;;; x₁' = x₂, x₂' = x₃, ..., xₙ' = u
;;; y = x₁
(define (ss-integrator n)
  (let ([A (make-matrix n n 0)]
        [B (make-matrix n 1 0)]
        [C (make-matrix 1 n 0)]
        [D (make-matrix 1 1 0)])
       ;; A has 1's on superdiagonal
       (do ([i 0 (+ i 1)])
           [(>= i (- n 1))]
           (matrix-set! A i (+ i 1) 1))
       ;; B has 1 in last row
       (matrix-set! B (- n 1) 0 1)
       ;; C has 1 in first column
       (matrix-set! C 0 0 1)
       (make-ss A B C D)))

;;; ============================================================
;;; Display
;;; ============================================================

;;; ss->string : SS → String
(define (ss->string sys)
  (let ([n (ss-order sys)]
        [m (ss-inputs sys)]
        [p (ss-outputs sys)])
       (string-append
        "State-Space System:\n"
        "  Order: " (number->string n) "\n"
        "  Inputs: " (number->string m) "\n"
        "  Outputs: " (number->string p))))
