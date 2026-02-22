(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'matrix)
(require 'matrix-solvers)
(require 'matrix-eigen)
(require 'control/state-space)

(doc 'module 'discrete-control)
(doc 'description "Discretization methods for converting continuous-time state-space systems to discrete-time, plus discrete-time simulation")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'discrete-state-space)
(doc 'description "A discrete state-space system uses the same (ss A B C D) structure but with the interpretation: x[k+1] = A*x[k] + B*u[k], y[k] = C*x[k] + D*u[k]. We add metadata to distinguish discrete from continuous systems")

(define (make-dss A B C D Ts)
  (doc 'type '(-> Matrix Matrix Matrix Matrix Number (U SS Error)))
  (doc 'description "Create a discrete-time state-space system with sample time Ts")
  (let ([result (make-ss A B C D)])
       (if (error-result? result)
           result
           (list 'dss A B C D Ts))))

;;; dss? : Any → Boolean
;;; Check if value is a discrete state-space system.
(define (dss? x)
  (and (pair? x)
       (eq? (car x) 'dss)
       (= (length x) 6)))

;;; dss-Ts : DSS → Number
;;; Get the sample time.
(define (dss-Ts sys)
  (if (dss? sys)
      (list-ref sys 5)
      #f))

;;; dss-A, dss-B, dss-C, dss-D work like ss- accessors
(define (dss-A sys) (if (dss? sys) (cadr sys) (ss-A sys)))
(define (dss-B sys) (if (dss? sys) (caddr sys) (ss-B sys)))
(define (dss-C sys) (if (dss? sys) (cadddr sys) (ss-C sys)))
(define (dss-D sys) (if (dss? sys) (car (cddddr sys)) (ss-D sys)))

;;; dss-order : DSS → Nat
(define (dss-order sys)
  (matrix-rows (dss-A sys)))

;;; error-result? : Any → Boolean
(define (error-result? x)
  (and (pair? x) (eq? (car x) 'error)))

;;; ====
;;; Zero-Order Hold (ZOH) Discretization
;;; ====

;;; c2d-zoh : SS × Number → DSS
;;; Convert continuous-time to discrete-time using Zero-Order Hold.
;;; Uses the Van Loan method which is robust for singular A matrices.
;;;
;;; Van Loan method: Compute exp of the block matrix:
;;;   | A  B |        | Ad  Bd |
;;;   | 0  0 | * Ts = | 0   I  |
;;;
;;; This avoids the A^{-1} computation in the naive formula.
(define (c2d-zoh sys Ts)
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [C (ss-C sys)]
         [D (ss-D sys)]
         [n (ss-order sys)]
         [m (ss-inputs sys)])
        (if (= n 0)
            ;; Trivial case: static gain
            (make-dss A B C D Ts)
            ;; Van Loan method
            (let* ([block-size (+ n m)]
                   [block (make-matrix block-size block-size 0)])
                  ;; Fill upper-left with A
                  (do ([i 0 (+ i 1)])
                      ((= i n))
                      (do ([j 0 (+ j 1)])
                          ((= j n))
                          (matrix-set! block i j (matrix-ref A i j))))
                  ;; Fill upper-right with B
                  (do ([i 0 (+ i 1)])
                      ((= i n))
                      (do ([j 0 (+ j 1)])
                          ((= j m))
                          (matrix-set! block i (+ n j) (matrix-ref B i j))))
                  ;; Lower-left and lower-right are zeros (already initialized)
                  ;; Compute exp(block * Ts) using numerically robust Padé approximation
                  (let* ([scaled-block (matrix-scale Ts block)]
                         [exp-block (matrix-exp scaled-block)])
                        ;; Extract Ad from upper-left n×n
                        (let ([Ad (make-matrix n n 0)])
                             (do ([i 0 (+ i 1)])
                                 ((= i n))
                                 (do ([j 0 (+ j 1)])
                                     ((= j n))
                                     (matrix-set! Ad i j (matrix-ref exp-block i j))))
                             ;; Extract Bd from upper-right n×m
                             (let ([Bd (make-matrix n m 0)])
                                  (do ([i 0 (+ i 1)])
                                      ((= i n))
                                      (do ([j 0 (+ j 1)])
                                          ((= j m))
                                          (matrix-set! Bd i j (matrix-ref exp-block i (+ n j)))))
                                  (make-dss Ad Bd C D Ts))))))))

;;; ====
;;; Tustin (Bilinear) Discretization
;;; ====

;;; c2d-tustin : SS × Number → DSS
;;; Convert continuous-time to discrete-time using Tustin transform.
;;; s = (2/Ts) * (z-1)/(z+1)
;;;
;;; State-space form:
;;;   Ad = (I - A*Ts/2)^{-1} * (I + A*Ts/2)
;;;   Bd = sqrt(Ts) * (I - A*Ts/2)^{-1} * B
;;;   Cd = sqrt(Ts) * C * (I - A*Ts/2)^{-1}
;;;   Dd = D + (Ts/2) * C * (I - A*Ts/2)^{-1} * B
;;;
;;; Simplified form (avoiding sqrt for clarity):
;;;   Ad = (I - A*Ts/2)^{-1} * (I + A*Ts/2)
;;;   Bd = Ts * (I - A*Ts/2)^{-1} * B
;;;   Cd = C
;;;   Dd = D + (Ts/2) * C * (I - A*Ts/2)^{-1} * B
(define (c2d-tustin sys Ts)
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [C (ss-C sys)]
         [D (ss-D sys)]
         [n (ss-order sys)])
        (if (= n 0)
            (make-dss A B C D Ts)
            (let* ([I (matrix-identity n)]
                   [half-Ts (/ Ts 2)]
                   ;; I - A*Ts/2
                   [I-ATs2 (matrix-sub I (matrix-scale half-Ts A))]
                   ;; I + A*Ts/2
                   [I+ATs2 (matrix-add I (matrix-scale half-Ts A))]
                   ;; (I - A*Ts/2)^{-1}
                   [inv (matrix-inverse I-ATs2)])
                  (if (error-result? inv)
                      `(error tustin-singular-matrix ,inv)
                      (let* ([Ad (matrix-mul inv I+ATs2)]
                             [Bd (matrix-mul (matrix-scale Ts inv) B)]
                             [Cd C]
                             ;; Dd = D + (Ts/2) * C * inv * B
                             [invB (matrix-mul inv B)]
                             [CinvB (matrix-mul C invB)]
                             [Dd (matrix-add D (matrix-scale half-Ts CinvB))])
                            (make-dss Ad Bd Cd Dd Ts)))))))

;;; c2d-tustin-prewarp : SS × Number × Number → DSS
;;; Tustin transform with frequency prewarping.
;;; Matches continuous frequency response exactly at frequency wc.
;;;   wc: critical frequency in rad/s
;;;
;;; The prewarped equivalent sample time ensures the discrete frequency
;;; wc maps exactly to the continuous frequency wc.
;;; FIX: Corrected prewarping formula: Ts_mod = (2/wc) * tan(wc * Ts / 2)
(define (c2d-tustin-prewarp sys Ts wc)
  (let* ([;; Prewarped sample time: Ts_mod = (2/wc) * tan(wc * Ts / 2)
          Ts-mod (* (/ 2 wc) (tan (* wc Ts 0.5)))])
        (c2d-tustin sys Ts-mod)))

;;; ====
;;; Discrete to Continuous (Inverse)
;;; ====

;;; d2c-tustin : DSS × Number → SS
;;; Convert discrete-time to continuous-time using inverse Tustin.
;;; z = (1 + s*Ts/2) / (1 - s*Ts/2)
;;;
;;; This is the inverse of c2d-tustin.
(define (d2c-tustin dsys Ts)
  (let* ([Ad (dss-A dsys)]
         [Bd (dss-B dsys)]
         [Cd (dss-C dsys)]
         [Dd (dss-D dsys)]
         [n (dss-order dsys)])
        (if (= n 0)
            (make-ss Ad Bd Cd Dd)
            (let* ([I (matrix-identity n)]
                   ;; A = (2/Ts) * (Ad - I) * (Ad + I)^{-1}
                   [Ad-I (matrix-sub Ad I)]
                   [Ad+I (matrix-add Ad I)]
                   [inv (matrix-inverse Ad+I)])
                  (if (error-result? inv)
                      `(error d2c-singular-matrix ,inv)
                      (let* ([A (matrix-scale (/ 2 Ts) (matrix-mul Ad-I inv))]
                             ;; B = (2/sqrt(Ts)) * (Ad + I)^{-1} * Bd
                             ;; Simplified: B = (2/Ts) * (Ad + I)^{-1} * Bd
                             [B (matrix-mul (matrix-scale (/ 2 Ts) inv) Bd)]
                             [C Cd]
                             ;; D = Dd - (Ts/2) * Cd * (Ad + I)^{-1} * Bd
                             [invBd (matrix-mul inv Bd)]
                             [CinvBd (matrix-mul Cd invBd)]
                             [D (matrix-sub Dd (matrix-scale (/ Ts 2) CinvBd))])
                            (make-ss A B C D)))))))

;;; ====
;;; Discrete-Time Simulation
;;; ====

;;; dss-step : DSS × Vec × Vec → Vec
;;; Compute one step: x[k+1] = A*x[k] + B*u[k]
(define (dss-step sys x u)
  (let ([A (dss-A sys)]
        [B (dss-B sys)])
       (vec-add (matrix-vec-mul-v A x)
                (matrix-vec-mul-v B u))))

;;; dss-output : DSS × Vec × Vec → Vec
;;; Compute output: y[k] = C*x[k] + D*u[k]
(define (dss-output sys x u)
  (let ([C (dss-C sys)]
        [D (dss-D sys)])
       (vec-add (matrix-vec-mul-v C x)
                (matrix-vec-mul-v D u))))

;;; dss-simulate : DSS × Vec × (List Vec) → (List Vec × List Vec)
;;; Simulate discrete system over input sequence.
;;; Returns (states . outputs) where each is a list of vectors.
(define (dss-simulate sys x0 inputs)
  (let loop ([x x0]
             [us inputs]
             [states (list x0)]
             [outputs '()])
       (if (null? us)
           (cons (reverse states) (reverse outputs))
           (let* ([u (car us)]
                  [y (dss-output sys x u)]
                  [x-next (dss-step sys x u)])
                 (loop x-next
                       (cdr us)
                       (cons x-next states)
                       (cons y outputs))))))

;;; dss-impulse-response : DSS × Nat → (List Vec)
;;; Compute impulse response for n samples.
;;; Input is 1 at k=0, 0 elsewhere (for first input channel).
(define (dss-impulse-response sys n)
  (let* ([m (matrix-cols (dss-B sys))]
         [n-states (dss-order sys)]
         [x0 (make-vector n-states 0)]
         [impulse (make-vector m 0)]
         [zero-input (make-vector m 0)])
        (vector-set! impulse 0 1)
        (let ([inputs (cons impulse (make-list (- n 1) zero-input))])
             (cdr (dss-simulate sys x0 inputs)))))  ; Return outputs

;;; dss-step-response : DSS × Nat → (List Vec)
;;; Compute step response for n samples.
;;; Input is 1 for all k >= 0 (for first input channel).
(define (dss-step-response sys n)
  (let* ([m (matrix-cols (dss-B sys))]
         [n-states (dss-order sys)]
         [x0 (make-vector n-states 0)]
         [step-input (make-vector m 0)])
        (vector-set! step-input 0 1)
        (let ([inputs (make-list n step-input)])
             (cdr (dss-simulate sys x0 inputs)))))

;;; ====
;;; Discrete-Time Stability
;;; ====

;;; dss-stable? : DSS → Boolean
;;; Check if discrete system is stable.
;;; Stable if all eigenvalues have magnitude < 1 (inside unit circle).
(define (dss-stable? sys)
  (let* ([A (dss-A sys)]
         [eigs (qr-algorithm-shifted A)])
        (cond
         [(vector? eigs)
          ;; All real eigenvalues
          (let ([n (vector-length eigs)])
               (let loop ([i 0])
                    (if (= i n)
                        #t
                        (and (< (abs (vector-ref eigs i)) 1)
                             (loop (+ i 1))))))]
         [(and (pair? eigs) (eq? (car eigs) 'complex-eigenvalues))
          ;; Has complex eigenvalues
          (let* ([eig-vec (cadr eigs)]
                 [complex-info (caddr eigs)]
                 [n (vector-length eig-vec)])
                ;; Check all eigenvalues
                (let check-all ([i 0] [info complex-info])
                     (if (= i n)
                         #t
                         ;; Check if this index is part of a complex pair
                         (let ([complex-pair (find-complex-at-index i info)])
                              (if complex-pair
                                  ;; Complex eigenvalue: |a + bi| = sqrt(a^2 + b^2)
                                  (let ([a (car complex-pair)]
                                        [b (cdr complex-pair)])
                                       (and (< (sqrt (+ (* a a) (* b b))) 1)
                                            (check-all (+ i 1) info)))
                                  ;; Real eigenvalue
                                  (and (< (abs (vector-ref eig-vec i)) 1)
                                       (check-all (+ i 1) info)))))))]
         [else #f])))  ; Error case

;;; find-complex-at-index : Nat × List → (Real . Imag) | #f
(define (find-complex-at-index i info)
  (cond
   [(null? info) #f]
   [(= i (caar info))
    ;; First of pair
    (cons (cadar info) (caddar info))]
   [(= i (+ 1 (caar info)))
    ;; Second of pair (conjugate)
    (cons (cadar info) (- (caddar info)))]
   [else (find-complex-at-index i (cdr info))]))

;;; ====
;;; Sample Rate Selection
;;; ====

;;; recommend-sample-rate : SS → Number
;;; Recommend sample rate based on system bandwidth.
;;; Rule of thumb: Ts should be 1/10 to 1/20 of fastest time constant.
(define (recommend-sample-rate sys)
  (let* ([A (ss-A sys)]
         [eigs (qr-algorithm-shifted A)])
        (cond
         [(vector? eigs)
          ;; Find fastest pole (largest magnitude real part)
          (let ([max-rate (apply max (map abs (vector->list eigs)))])
               (/ 1 (* 10 max-rate)))]
         [(and (pair? eigs) (eq? (car eigs) 'complex-eigenvalues))
          ;; Consider complex eigenvalue magnitudes
          (let* ([eig-vec (cadr eigs)]
                 [complex-info (caddr eigs)]
                 [rates '()])
                ;; Collect all natural frequencies
                (do ([i 0 (+ i 1)])
                    ((= i (vector-length eig-vec)))
                    (let ([cp (find-complex-at-index i complex-info)])
                         (if cp
                             (let ([omega (sqrt (+ (* (car cp) (car cp))
                                                   (* (cdr cp) (cdr cp))))])
                                  (set! rates (cons omega rates)))
                             (set! rates (cons (abs (vector-ref eig-vec i)) rates)))))
                (if (null? rates)
                    0.1  ; Default
                    (/ 1 (* 10 (apply max rates)))))]
         [else 0.1])))

;;; ====
;;; Helper Functions
;;; ====

;;; vec-add : Vec × Vec → Vec
(define (vec-add v1 v2)
  (let* ([n (vector-length v1)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (+ (vector-ref v1 i) (vector-ref v2 i))))))

;;; matrix-vec-mul-v : Matrix × Vec → Vec
(define (matrix-vec-mul-v M v)
  (let* ([rows (matrix-rows M)]
         [cols (matrix-cols M)]
         [result (make-vector rows 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (let loop ([j 0] [sum 0])
                 (if (= j cols)
                     (vector-set! result i sum)
                     (loop (+ j 1) (+ sum (* (matrix-ref M i j) (vector-ref v j)))))))))

;; make-list is provided by prelude (alias for replicate)

;;; matrix-inverse : Matrix → Matrix | Error
;;; Compute matrix inverse using LU decomposition.
(define (matrix-inverse M)
  (let* ([n (matrix-rows M)]
         [result (make-matrix n n 0)])
        ;; Solve M * X = I for each column of X
        (let loop ([j 0])
             (if (= j n)
                 result
                 (let* ([e-j (make-vector n 0)]
                        [_ (vector-set! e-j j 1)]
                        [col (solve-linear-system M e-j)])
                       (if (error-result? col)
                           col
                           (begin
                            (do ([i 0 (+ i 1)])
                                ((= i n))
                                (matrix-set! result i j (vector-ref col i)))
                            (loop (+ j 1)))))))))

;;; solve-linear-system : Matrix × Vec → Vec | Error
;;; Solve A*x = b using Gaussian elimination with partial pivoting.
(define (solve-linear-system A b)
  (let* ([n (matrix-rows A)]
         ;; Create augmented matrix
         [aug (make-matrix n (+ n 1) 0)])
        ;; Copy A and b into augmented matrix
        (do ([i 0 (+ i 1)])
            ((= i n))
            (do ([j 0 (+ j 1)])
                ((= j n))
                (matrix-set! aug i j (matrix-ref A i j)))
            (matrix-set! aug i n (vector-ref b i)))
        ;; Forward elimination with partial pivoting
        (let forward ([k 0])
             (if (= k n)
                 ;; Back substitution
                 (let ([x (make-vector n 0)])
                      (let back ([i (- n 1)])
                           (if (< i 0)
                               x
                               (let* ([sum (let sum-loop ([j (+ i 1)] [s 0])
                                                (if (= j n)
                                                    s
                                                    (sum-loop (+ j 1)
                                                              (+ s (* (matrix-ref aug i j)
                                                                      (vector-ref x j))))))]
                                      [xi (/ (- (matrix-ref aug i n) sum)
                                             (matrix-ref aug i i))])
                                     (vector-set! x i xi)
                                     (back (- i 1))))))
                 ;; Find pivot
                 (let* ([max-row k]
                        [max-val (abs (matrix-ref aug k k))])
                       (do ([i (+ k 1) (+ i 1)])
                           ((= i n))
                           (let ([val (abs (matrix-ref aug i k))])
                                (when (> val max-val)
                                      (set! max-row i)
                                      (set! max-val val))))
                       (if (< max-val 1e-14)
                           '(error singular-matrix)
                           (begin
                            ;; Swap rows if needed
                            (when (not (= max-row k))
                                  (do ([j 0 (+ j 1)])
                                      ((= j (+ n 1)))
                                      (let ([tmp (matrix-ref aug k j)])
                                           (matrix-set! aug k j (matrix-ref aug max-row j))
                                           (matrix-set! aug max-row j tmp))))
                            ;; Eliminate
                            (do ([i (+ k 1) (+ i 1)])
                                ((= i n))
                                (let ([factor (/ (matrix-ref aug i k) (matrix-ref aug k k))])
                                     (do ([j k (+ j 1)])
                                         ((= j (+ n 1)))
                                         (matrix-set! aug i j
                                                      (- (matrix-ref aug i j)
                                                         (* factor (matrix-ref aug k j)))))))
                            (forward (+ k 1)))))))))
