(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-eigen.ss")
(load "lattice/linalg/svd.ss")

(doc 'module 'state-space)
(doc 'description "State Space Models - State space representation of linear time-invariant (LTI) systems.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "A continuous-time state space model is:
  x'(t) = A*x(t) + B*u(t)   (state equation)
  y(t)  = C*x(t) + D*u(t)   (output equation)

where:
  x(t) is the n×1 state vector
  u(t) is the m×1 input vector
  y(t) is the p×1 output vector
  A is the n×n state (system) matrix
  B is the n×m input matrix
  C is the p×n output matrix
  D is the p×m feedthrough matrix")

(doc 'section 'representation)

(doc 'note "A state space system is: (ss A B C D)
where A, B, C, D are matrices with compatible dimensions.")

(define (ss? sys)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if value is a state space system")
  (and (pair? sys)
       (eq? (car sys) 'ss)
       (= (length sys) 5)
       (matrix? (cadr sys))
       (matrix? (caddr sys))
       (matrix? (cadddr sys))
       (matrix? (car (cddddr sys)))))

(doc 'section 'accessors)

(define (ss-A sys)
  (doc 'type '(-> SS Matrix))
  (doc 'description "Get the A (state) matrix")
  (cadr sys))

(define (ss-B sys)
  (doc 'type '(-> SS Matrix))
  (doc 'description "Get the B (input) matrix")
  (caddr sys))

(define (ss-C sys)
  (doc 'type '(-> SS Matrix))
  (doc 'description "Get the C (output) matrix")
  (cadddr sys))

(define (ss-D sys)
  (doc 'type '(-> SS Matrix))
  (doc 'description "Get the D (feedthrough) matrix")
  (car (cddddr sys)))

(define (ss-order sys)
  (doc 'type '(-> SS Nat))
  (doc 'description "Get the order (number of states) of the system")
  (matrix-rows (ss-A sys)))

(define (ss-inputs sys)
  (doc 'type '(-> SS Nat))
  (doc 'description "Get the number of inputs")
  (matrix-cols (ss-B sys)))

(define (ss-outputs sys)
  (doc 'type '(-> SS Nat))
  (doc 'description "Get the number of outputs")
  (matrix-rows (ss-C sys)))

(doc 'section 'construction)

(define (make-ss A B C D)
  (doc 'type '(-> Matrix Matrix Matrix Matrix (Either SS Error)))
  (doc 'description "Create a state space system, validating dimensions")
  (let ([n (matrix-rows A)])
       (cond
        [(not (= n (matrix-cols A)))
         `(error A-not-square ,(matrix-shape A))]
        [(not (= n (matrix-rows B)))
         `(error B-rows-mismatch ,(matrix-rows B) ,n)]
        [(not (= n (matrix-cols C)))
         `(error C-cols-mismatch ,(matrix-cols C) ,n)]
        [(not (= (matrix-rows D) (matrix-rows C)))
         `(error D-rows-mismatch ,(matrix-rows D) ,(matrix-rows C))]
        [(not (= (matrix-cols D) (matrix-cols B)))
         `(error D-cols-mismatch ,(matrix-cols D) ,(matrix-cols B))]
        [else
         (list 'ss A B C D)])))

(define (ss-from-lists A-lists B-lists C-lists D-lists)
  (doc 'type '(-> (List (List Number)) (List (List Number)) (List (List Number)) (List (List Number)) SS))
  (doc 'description "Create state space from nested lists")
  (make-ss (matrix-from-lists A-lists)
           (matrix-from-lists B-lists)
           (matrix-from-lists C-lists)
           (matrix-from-lists D-lists)))

(doc 'section 'operations)

(define (ss-state-equation sys x u)
  (doc 'type '(-> SS Vec Vec Vec))
  (doc 'description "Compute x' = A*x + B*u")
  (vec-add (matrix-vec-mul (ss-A sys) x)
           (matrix-vec-mul (ss-B sys) u)))

(define (ss-output-equation sys x u)
  (doc 'type '(-> SS Vec Vec Vec))
  (doc 'description "Compute y = C*x + D*u")
  (vec-add (matrix-vec-mul (ss-C sys) x)
           (matrix-vec-mul (ss-D sys) u)))

(doc 'section 'matrix-exponential)

(doc 'note "Matrix Exponential - Scaling and Squaring with Padé

The Scaling and Squaring algorithm computes e^A as follows:
1. Find s such that ||A/2^s|| < 1 (scaling)
2. Compute e^(A/2^s) using Padé approximant
3. Square the result s times: e^A = (e^(A/2^s))^(2^s)

This is much more numerically stable than Taylor series.")

(doc *pade-coeffs-6* 'description "Coefficients for [6,6] diagonal Padé approximant
N(X) = sum_{k=0}^6 b_k * X^k, D(X) = sum_{k=0}^6 b_k * (-X)^k
Precomputed: b_k = (2n-k)! * n! / ((2n)! * (n-k)! * k!) for n=6")
(define *pade-coeffs-6*
  (list 1 1/2 1/10 1/120 1/1680 1/30240 1/665280))

(define (matrix-1norm m)
  (doc 'type '(-> Matrix Real))
  (doc 'description "Compute 1-norm (max column sum of absolute values)")
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

(define (matrix-pade-6 A)
  (doc 'type '(-> Matrix (Pair Matrix Matrix)))
  (doc 'description "Compute [6,6] Padé numerator N and denominator D matrices. Returns (N . D) where e^A ≈ D^(-1) * N")
  (let* ([n (matrix-rows A)]
         [I (identity n)]
         [A2 (matrix-mul A A)]
         [A4 (matrix-mul A2 A2)]
         [A6 (matrix-mul A4 A2)]
         [b (list-ref *pade-coeffs-6* 0)]
         [b1 (list-ref *pade-coeffs-6* 1)]
         [b2 (list-ref *pade-coeffs-6* 2)]
         [b3 (list-ref *pade-coeffs-6* 3)]
         [b4 (list-ref *pade-coeffs-6* 4)]
         [b5 (list-ref *pade-coeffs-6* 5)]
         [b6 (list-ref *pade-coeffs-6* 6)]
         [A3 (matrix-mul A2 A)]
         [A5 (matrix-mul A4 A)]
         [even-terms (matrix-add I
                                 (matrix-add (matrix-scale b2 A2)
                                             (matrix-add (matrix-scale b4 A4)
                                                         (matrix-scale b6 A6))))]
         [odd-terms (matrix-add (matrix-scale b1 A)
                                (matrix-add (matrix-scale b3 A3)
                                            (matrix-scale b5 A5)))]
         [N (matrix-add even-terms odd-terms)]
         [D (matrix-sub even-terms odd-terms)])
        (cons N D)))

(define (matrix-exp A)
  (doc 'type '(-> Matrix Matrix))
  (doc 'description "Compute matrix exponential using Scaling and Squaring with [6,6] Padé. Numerically robust for a wide range of matrices.")
  (let* ([n (matrix-rows A)]
         [norm-A (matrix-1norm A)]
         [s (if (<= norm-A 0.5)
                0
                (+ 1 (exact (floor (log (inexact norm-A) 2)))))]
         [scale-factor (expt 2 s)]
         [A-scaled (matrix-scale (/ 1 scale-factor) A)]
         [pade-result (matrix-pade-6 A-scaled)]
         [N (car pade-result)]
         [D (cdr pade-result)]
         [exp-scaled (matrix-solve-system D N)])
        (let square ([k 0] [result exp-scaled])
             (if (>= k s)
                 result
                 (square (+ k 1) (matrix-mul result result))))))

(define (matrix-solve-system A B)
  (doc 'type '(-> Matrix Matrix Matrix))
  (doc 'description "Solve A * X = B for X using LU decomposition. Returns X = A^(-1) * B")
  (let* ([n (matrix-rows A)]
         [m (matrix-cols B)]
         [lu-result (matrix-lu A)])
        (if (and (pair? lu-result) (eq? (car lu-result) 'error))
            (matrix-mul (matrix-inverse A) B)
            (let* ([L (car lu-result)]
                   [U (cadr lu-result)]
                   [P (caddr lu-result)]
                   [Pb (matrix-mul P B)])
                  (let ([Y (forward-substitute L Pb)])
                       (back-substitute U Y))))))

(define (forward-substitute L B)
  (doc 'type '(-> Matrix Matrix Matrix))
  (doc 'description "Solve L * Y = B where L is lower triangular")
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

(define (back-substitute U Y)
  (doc 'type '(-> Matrix Matrix Matrix))
  (doc 'description "Solve U * X = Y where U is upper triangular")
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

(define (matrix-exp-taylor A fuel)
  (doc 'type '(-> Matrix Nat Matrix))
  (doc 'description "DEPRECATED: Use matrix-exp instead for numerical stability. Compute matrix exponential using Taylor series: e^A = I + A + A²/2! + A³/3! + ... Fuel limits number of terms.")
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

(define (ss-transition-matrix sys t)
  (doc 'type '(-> SS Num Matrix))
  (doc 'description "Compute the state transition matrix Φ(t) = e^(A*t) using numerically robust Scaling and Squaring with Padé")
  (matrix-exp (matrix-scale t (ss-A sys))))

(define (ss-transition-matrix-taylor sys t terms)
  (doc 'type '(-> SS Num Nat Matrix))
  (doc 'description "DEPRECATED: Use ss-transition-matrix instead. Compute the state transition matrix using Taylor series.")
  (matrix-exp-taylor (matrix-scale t (ss-A sys)) terms))

(doc 'section 'controllability)

(define (ss-controllability-matrix sys)
  (doc 'type '(-> SS Matrix))
  (doc 'description "Compute the controllability matrix C = [B AB A²B ... A^(n-1)B]")
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [n (ss-order sys)]
         [m (ss-inputs sys)])
        (let build ([k 0] [A-power (identity n)] [cols '()])
             (if (>= k n)
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
                 (let* ([AkB (matrix-mul A-power B)]
                        [new-cols (let extract-cols ([j 0] [acc '()])
                                       (if (>= j m)
                                           (reverse acc)
                                           (extract-cols (+ j 1)
                                                         (cons (matrix-column-as-matrix AkB j) acc))))])
                       (build (+ k 1)
                              (matrix-mul A-power A)
                              (append cols new-cols)))))))

(define (matrix-column-as-matrix m j)
  (doc 'type '(-> Matrix Nat Matrix))
  (doc 'description "Extract column j as an n×1 matrix")
  (let* ([n (matrix-rows m)]
         [result (make-matrix n 1 0)])
        (do ([i 0 (+ i 1)])
            [(= i n) result]
            (matrix-set! result i 0 (matrix-ref m i j)))))

(define (ss-controllable? sys tolerance)
  (doc 'type '(-> SS Num Boolean))
  (doc 'description "Check if system is controllable (controllability matrix has full row rank). Uses tolerance for numerical rank check.")
  (let* ([C-mat (ss-controllability-matrix sys)]
         [n (ss-order sys)]
         [rank (matrix-rank C-mat tolerance)])
        (= rank n)))

(doc 'section 'observability)

(define (ss-observability-matrix sys)
  (doc 'type '(-> SS Matrix))
  (doc 'description "Compute the observability matrix O = [C; CA; CA²; ...; CA^(n-1)]")
  (let* ([A (ss-A sys)]
         [C (ss-C sys)]
         [n (ss-order sys)]
         [p (ss-outputs sys)])
        (let build ([k 0] [A-power (identity n)] [rows '()])
             (if (>= k n)
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
                 (let ([CAk (matrix-mul C A-power)])
                      (build (+ k 1)
                             (matrix-mul A-power A)
                             (cons CAk rows)))))))

(define (ss-observable? sys tolerance)
  (doc 'type '(-> SS Num Boolean))
  (doc 'description "Check if system is observable (observability matrix has full column rank)")
  (let* ([O-mat (ss-observability-matrix sys)]
         [n (ss-order sys)]
         [rank (matrix-rank O-mat tolerance)])
        (= rank n)))

(doc 'section 'gramians)

(doc 'note "For continuous-time systems, the controllability Gramian Wc satisfies:
  A*Wc + Wc*A' + B*B' = 0  (Lyapunov equation)

The observability Gramian Wo satisfies:
  A'*Wo + Wo*A + C'*C = 0  (Lyapunov equation)

For discrete-time systems:
  A*Wc*A' - Wc + B*B' = 0
  A'*Wo*A - Wo + C'*C = 0")

(define (ss-controllability-gramian-finite sys N)
  (doc 'type '(-> SS Nat Matrix))
  (doc 'description "Compute finite-horizon controllability Gramian by direct sum. Wc = sum_{k=0}^{N-1} A^k * B * B' * (A')^k")
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [n (ss-order sys)]
         [Wc (make-matrix n n 0)])
        (let loop ([k 0] [Ak (identity n)])
             (if (>= k N)
                 Wc
                 (let* ([AkB (matrix-mul Ak B)]
                        [term (matrix-mul AkB (matrix-transpose AkB))])
                       (do ([i 0 (+ i 1)])
                           [(= i n)]
                           (do ([j 0 (+ j 1)])
                               [(= j n)]
                               (matrix-set! Wc i j
                                            (+ (matrix-ref Wc i j)
                                               (matrix-ref term i j)))))
                       (loop (+ k 1) (matrix-mul Ak A)))))))

(define (ss-observability-gramian-finite sys N)
  (doc 'type '(-> SS Nat Matrix))
  (doc 'description "Compute finite-horizon observability Gramian by direct sum. Wo = sum_{k=0}^{N-1} (A')^k * C' * C * A^k")
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

(doc 'section 'matrix-rank)

(doc 'note "SVD-based rank is the gold standard for numerical rank determination. Falls back to QR if SVD fails.")

(define (matrix-rank-svd m tolerance)
  (doc 'type '(-> Matrix Num Nat))
  (doc 'description "Compute numerical rank using SVD (gold standard). Counts singular values that exceed tolerance.")
  (guard (e [else (matrix-rank-qr m tolerance)])
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

(define (matrix-rank-qr m tolerance)
  (doc 'type '(-> Matrix Num Nat))
  (doc 'description "Compute numerical rank using QR decomposition (faster but less robust). Counts diagonal elements of R that exceed tolerance.")
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

(define (matrix-rank m tolerance)
  (doc 'type '(-> Matrix Num Nat))
  (doc 'description "Compute numerical rank. Uses SVD for robustness.")
  (matrix-rank-svd m tolerance))

(doc 'section 'modal-decomposition)

(doc 'note "For a diagonalizable system, there exists a transformation T such that:
  Ā = T⁻¹AT is diagonal (eigenvalues on diagonal)
  B̄ = T⁻¹B
  C̄ = CT
  D̄ = D

This decouples the state equations into independent modes.

Note: Full modal decomposition requires eigenvalue computation, which is complex. Here we provide a simplified version that applies a given transformation matrix.")

(define (ss-transform sys T T-inv)
  (doc 'type '(-> SS Matrix Matrix SS))
  (doc 'description "Apply similarity transformation: new states z = T⁻¹x. Returns transformed system (T⁻¹AT, T⁻¹B, CT, D)")
  (let ([A (ss-A sys)]
        [B (ss-B sys)]
        [C (ss-C sys)]
        [D (ss-D sys)])
       (make-ss (matrix-mul T-inv (matrix-mul A T))
                (matrix-mul T-inv B)
                (matrix-mul C T)
                D)))

(doc 'section 'common-forms)

(define (ss-identity-output A B)
  (doc 'type '(-> Matrix Matrix SS))
  (doc 'description "Create a system with identity C (full state output) and no feedthrough")
  (let* ([n (matrix-rows A)]
         [m (matrix-cols B)]
         [C (identity n)]
         [D (make-matrix n m 0)])
        (make-ss A B C D)))

(define (ss-scalar a b c d)
  (doc 'type '(-> Num Num Num Num SS))
  (doc 'description "Create a first-order scalar system. x' = a*x + b*u, y = c*x + d*u")
  (make-ss (matrix-from-lists (list (list a)))
           (matrix-from-lists (list (list b)))
           (matrix-from-lists (list (list c)))
           (matrix-from-lists (list (list d)))))

(define (ss-integrator n)
  (doc 'type '(-> Nat SS))
  (doc 'description "Create an n-th order integrator chain. x₁' = x₂, x₂' = x₃, ..., xₙ' = u, y = x₁")
  (let ([A (make-matrix n n 0)]
        [B (make-matrix n 1 0)]
        [C (make-matrix 1 n 0)]
        [D (make-matrix 1 1 0)])
       (do ([i 0 (+ i 1)])
           [(>= i (- n 1))]
           (matrix-set! A i (+ i 1) 1))
       (matrix-set! B (- n 1) 0 1)
       (matrix-set! C 0 0 1)
       (make-ss A B C D)))

(doc 'section 'display)

(define (ss->string sys)
  (doc 'type '(-> SS String))
  (doc 'description "Convert state space system to string representation")
  (let ([n (ss-order sys)]
        [m (ss-inputs sys)]
        [p (ss-outputs sys)])
       (string-append
        "State-Space System:\n"
        "  Order: " (number->string n) "\n"
        "  Inputs: " (number->string m) "\n"
        "  Outputs: " (number->string p))))
