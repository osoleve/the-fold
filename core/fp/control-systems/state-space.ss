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
(load "core/linalg/matrix.ss")
(load "core/linalg/matrix-decomp.ss")

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

;;; matrix-exp-taylor : Matrix × Nat → Matrix
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
                 (let* ([new-term (matrix-scale (matrix-mul term A) (/ 1 (* factorial k)))]
                        [new-factorial (* factorial k)])
                       (loop (+ k 1)
                             (matrix-mul term A)
                             (matrix-add result new-term)
                             new-factorial))))))

;;; ss-transition-matrix : SS × Num × Nat → Matrix
;;; Compute the state transition matrix Φ(t) = e^(A*t)
;;; using Taylor series with given number of terms.
(define (ss-transition-matrix sys t terms)
  (matrix-exp-taylor (matrix-scale (ss-A sys) t) terms))

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

;;; matrix-rank : Matrix × Num → Nat
;;; Compute numerical rank using QR decomposition.
;;; Counts diagonal elements of R that exceed tolerance.
(define (matrix-rank m tolerance)
  (let ([qr-result (matrix-qr m)])
       (if (and (pair? qr-result) (eq? (car qr-result) 'error))
           ;; QR failed (e.g., underdetermined matrix)
           0
           ;; qr-result is (Q R), extract R which is the second element
           (let* ([R (cadr qr-result)]
                  [min-dim (min (matrix-rows R) (matrix-cols R))])
                 (let count ([i 0] [rank 0])
                      (if (>= i min-dim)
                          rank
                          (if (> (abs (matrix-ref R i i)) tolerance)
                              (count (+ i 1) (+ rank 1))
                              (count (+ i 1) rank))))))))

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
