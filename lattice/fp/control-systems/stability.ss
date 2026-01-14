;;; lattice/fp/control-systems/stability.ss — Stability Analysis
;;;
;;; Comprehensive stability analysis for linear time-invariant systems.
;;;
;;; This module provides:
;;;   - Pole-based stability (continuous and discrete)
;;;   - Routh-Hurwitz criterion
;;;   - Lyapunov stability (via Lyapunov equation)
;;;   - Root locus computation
;;;   - Nyquist analysis
;;;   - Gain and phase margins
;;;   - BIBO stability
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/numeric/polynomial.ss
;;;   - lattice/numeric/complex.ss
;;;   - lattice/linalg/matrix.ss
;;;   - lattice/linalg/matrix-eigen.ss
;;;   - lattice/fp/control-systems/transfer-function.ss
;;;   - lattice/fp/control-systems/state-space.ss

(load "core/base/prelude.ss")
(load "lattice/numeric/polynomial.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-eigen.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/fp/control-systems/transfer-function.ss")
(load "lattice/fp/control-systems/state-space.ss")

;;; ====
;;; Pole-Based Stability
;;; ====

;;; Continuous-time system is stable if all poles have Re(s) < 0.
;;; Discrete-time system is stable if all poles have |z| < 1.

;;; ss-poles : SS → (List Complex)
;;; Get the poles (eigenvalues of A matrix) of a state space system.
(define (ss-poles sys)
  (let* ([A (ss-A sys)]
         [n (matrix-rows A)])
        (if (= n 1)
            ;; 1x1 case: eigenvalue is the single element
            (list (make-complex (matrix-ref A 0 0) 0))
            ;; General case: use QR algorithm
            (qr-algorithm-shifted A 100 1e-10))))

;;; ss-stable? : SS → Boolean
;;; Check if continuous-time state space system is stable.
;;; All eigenvalues of A must have negative real parts.
(define (ss-stable? sys)
  (let ([poles (ss-poles sys)])
       (all? (lambda (p) (< (complex-real p) 0)) poles)))

;;; tf-stable? : TF → Boolean
;;; Check if continuous-time transfer function is stable.
;;; All poles must have negative real parts.
(define (tf-stable? tf)
  (let ([poles (tf-poles tf)])
       (all? (lambda (p) (< (complex-real p) 0)) poles)))

;;; ss-marginally-stable? : SS → Boolean
;;; Check if system is marginally stable (poles on imaginary axis, none in RHP).
(define (ss-marginally-stable? sys)
  (let ([poles (ss-poles sys)])
       (and (all? (lambda (p) (<= (complex-real p) 0)) poles)
            (any? (lambda (p) (< (abs (complex-real p)) 1e-10)) poles))))

;;; discrete-stable? : (List Complex) → Boolean
;;; Check if discrete-time system is stable.
;;; All poles must be inside unit circle (|z| < 1).
(define (discrete-stable? poles)
  (all? (lambda (p) (< (complex-magnitude p) 1)) poles))

;;; ====
;;; Routh-Hurwitz Criterion
;;; ====

;;; The Routh-Hurwitz criterion determines polynomial stability
;;; without explicitly computing roots.
;;;
;;; For polynomial: a_n*s^n + a_{n-1}*s^{n-1} + ... + a_1*s + a_0
;;;
;;; Build Routh array and check for sign changes in first column.
;;; System is stable iff there are no sign changes.

;;; routh-array : Poly → (List (Vector Number))
;;; Compute the Routh array for a polynomial.
;;; Returns list of row vectors.
(define (routh-array poly)
  (let* ([coeffs (poly-coeffs poly)]
         [n (vector-length coeffs)]
         [degree (- n 1)])
        (if (< n 2)
            ;; Degree 0 polynomial - check if positive
            (list (vector (vector-ref coeffs 0)))
            (let* (;; Build first two rows from coefficients
                   [row1-len (ceiling (/ n 2))]
                   [row1 (make-vector row1-len 0)]
                   [row2 (make-vector row1-len 0)])
                  ;; Fill row1: a_n, a_{n-2}, a_{n-4}, ...
                  (do ([i 0 (+ i 1)])
                      ((>= i row1-len))
                      (let ([idx (* i 2)])
                           (when (< idx n)
                                 (vector-set! row1 i (vector-ref coeffs idx)))))
                  ;; Fill row2: a_{n-1}, a_{n-3}, a_{n-5}, ...
                  (do ([i 0 (+ i 1)])
                      ((>= i row1-len))
                      (let ([idx (+ 1 (* i 2))])
                           (when (< idx n)
                                 (vector-set! row2 i (vector-ref coeffs idx)))))
                  ;; Build remaining rows
                  (routh-array-iter (list row2 row1) degree)))))

;;; routh-array-iter : (List (Vector Number)) × Nat → (List (Vector Number))
;;; Build remaining rows of Routh array.
(define (routh-array-iter rows remaining)
  (if (<= remaining 0)
      (reverse rows)
      (let* ([prev-row (car rows)]
             [prev-prev-row (cadr rows)]
             [len (vector-length prev-row)]
             [new-row (make-vector len 0)]
             [a (vector-ref prev-prev-row 0)]
             [b (vector-ref prev-row 0)])
            ;; Handle zero in pivot position
            (let ([b (if (< (abs b) 1e-15) 1e-15 b)])
                 ;; Compute new row entries
                 (do ([i 0 (+ i 1)])
                     ((>= i (- len 1)))
                     (let* ([c (if (< (+ i 1) len) (vector-ref prev-prev-row (+ i 1)) 0)]
                            [d (if (< (+ i 1) len) (vector-ref prev-row (+ i 1)) 0)]
                            [val (/ (- (* b c) (* a d)) b)])
                           (vector-set! new-row i val)))
                 (routh-array-iter (cons new-row rows) (- remaining 1))))))

;;; routh-hurwitz-stable? : Poly → Boolean
;;; Check if polynomial has all roots in left half plane using Routh-Hurwitz.
(define (routh-hurwitz-stable? poly)
  (let* ([array (routh-array poly)]
         [first-col (map (lambda (row) (vector-ref row 0)) array)])
        ;; Check for sign changes
        (not (has-sign-changes? first-col))))

;;; has-sign-changes? : (List Number) → Boolean
;;; Check if a list of numbers has sign changes.
(define (has-sign-changes? nums)
  (cond
   [(null? nums) #f]
   [(null? (cdr nums)) #f]
   [else
    (let* ([a (car nums)]
           [b (cadr nums)]
           ;; Sign change if product is negative
           [sign-change? (< (* a b) 0)])
          (or sign-change?
              (has-sign-changes? (cdr nums))))]))

;;; routh-hurwitz-count-rhp : Poly → Nat
;;; Count number of right-half-plane poles using Routh-Hurwitz.
;;; Returns the number of sign changes in first column.
(define (routh-hurwitz-count-rhp poly)
  (let* ([array (routh-array poly)]
         [first-col (map (lambda (row) (vector-ref row 0)) array)])
        (count-sign-changes first-col)))

;;; count-sign-changes : (List Number) → Nat
(define (count-sign-changes nums)
  (cond
   [(null? nums) 0]
   [(null? (cdr nums)) 0]
   [else
    (let* ([a (car nums)]
           [b (cadr nums)]
           [change (if (< (* a b) 0) 1 0)])
          (+ change (count-sign-changes (cdr nums))))]))

;;; ====
;;; Lyapunov Stability
;;; ====

;;; For continuous-time systems, solve the Lyapunov equation:
;;;   A'P + PA = -Q
;;; If Q is positive definite and P is positive definite,
;;; then the system is asymptotically stable.
;;;
;;; For discrete-time systems:
;;;   A'PA - P = -Q

;;; lyapunov-solve-continuous : Matrix × Matrix → Matrix | Error
;;; Solve the continuous Lyapunov equation A'P + PA = -Q for P.
;;; Uses vectorization: (A' ⊗ I + I ⊗ A') * vec(P) = vec(-Q)
;;; Simplified iterative approach for small systems.
(define (lyapunov-solve-continuous A Q)
  (let ([n (matrix-rows A)])
       (if (= n 1)
           ;; 1x1 case: 2*a*p = -q, p = -q/(2a)
           (let ([a (matrix-ref A 0 0)]
                 [q (matrix-ref Q 0 0)])
                (if (>= a 0)
                    '(error unstable-system)
                    (make-matrix 1 1 (/ (- q) (* 2 a)))))
           ;; General case: iterative method (Smith iteration)
           (lyapunov-iterate-continuous A Q 100))))

;;; lyapunov-iterate-continuous : Matrix × Matrix × Nat → Matrix
;;; Iterative solution using fixed-point iteration.
;;; P_{k+1} = A' * P_k * A + Q (adapted for continuous case)
(define (lyapunov-iterate-continuous A Q max-iter)
  (let* ([n (matrix-rows A)]
         ;; Initial guess: P = Q
         [P Q]
         ;; Scale factor for iteration
         [At (matrix-transpose A)]
         [dt 0.01])  ; Small time step
        (let loop ([P P] [iter 0])
             (if (>= iter max-iter)
                 P
                 ;; P_new = P + dt * (A'P + PA + Q)
                 (let* ([AtP (matrix-mul At P)]
                        [PA (matrix-mul P A)]
                        [residual (matrix-add (matrix-add AtP PA) Q)]
                        [P-new (matrix-add P (matrix-scale dt residual))]
                        ;; Check convergence
                        [diff (matrix-frobenius-norm (matrix-sub P-new P))])
                       (if (< diff 1e-10)
                           P-new
                           (loop P-new (+ iter 1))))))))

;;; lyapunov-solve-discrete : Matrix × Matrix → Matrix | Error
;;; Solve the discrete Lyapunov equation A'PA - P = -Q for P.
(define (lyapunov-solve-discrete A Q)
  (let ([n (matrix-rows A)])
       (if (= n 1)
           ;; 1x1 case: a²p - p = -q, p = q/(1-a²)
           (let* ([a (matrix-ref A 0 0)]
                  [q (matrix-ref Q 0 0)]
                  [denom (- 1 (* a a))])
                 (if (<= (abs denom) 1e-15)
                     '(error marginally-stable)
                     (make-matrix 1 1 (/ q denom))))
           ;; General case: iterative method
           (lyapunov-iterate-discrete A Q 1000))))

;;; lyapunov-iterate-discrete : Matrix × Matrix × Nat → Matrix
;;; Iterative solution: P_{k+1} = A' * P_k * A + Q
(define (lyapunov-iterate-discrete A Q max-iter)
  (let* ([At (matrix-transpose A)]
         [P Q])
        (let loop ([P P] [iter 0])
             (if (>= iter max-iter)
                 P
                 (let* ([P-new (matrix-add (matrix-mul At (matrix-mul P A)) Q)]
                        [diff (matrix-frobenius-norm (matrix-sub P-new P))])
                       (if (< diff 1e-10)
                           P-new
                           (loop P-new (+ iter 1))))))))

;;; lyapunov-stable? : SS → Boolean
;;; Check stability using Lyapunov equation.
;;; If we can find P > 0 satisfying A'P + PA = -I, system is stable.
(define (lyapunov-stable? sys)
  (let* ([A (ss-A sys)]
         [n (matrix-rows A)]
         [Q (identity n)]
         [P (lyapunov-solve-continuous A Q)])
        (if (and (pair? P) (eq? (car P) 'error))
            #f
            (matrix-positive-definite? P))))

;;; matrix-positive-definite? : Matrix → Boolean
;;; Check if symmetric matrix is positive definite.
;;; Uses eigenvalue test: all eigenvalues must be positive.
(define (matrix-positive-definite? M)
  (let* ([n (matrix-rows M)]
         [eigenvalues (if (= n 1)
                          (list (matrix-ref M 0 0))
                          (map complex-real (qr-algorithm-shifted M 50 1e-10)))])
        (all? (lambda (ev) (> ev 0)) eigenvalues)))

;;; matrix-frobenius-norm : Matrix → Number
;;; Compute Frobenius norm of a matrix.
(define (matrix-frobenius-norm M)
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

;;; ====
;;; Root Locus
;;; ====

;;; Root locus shows how closed-loop poles move as gain K varies.
;;; For open-loop G(s)H(s) = N(s)/D(s), closed-loop poles satisfy:
;;;   D(s) + K*N(s) = 0

;;; root-locus-points : TF × (List Number) → (List (List Complex))
;;; Compute root locus for a list of gain values.
;;; Returns list of pole lists, one for each gain.
(define (root-locus-points tf gains)
  (let ([num (tf-num tf)]
        [den (tf-den tf)])
       (map (lambda (K)
                    (let ([cl-poly (poly-add den (poly-scale num K))])
                         (poly-roots cl-poly)))
            gains)))

;;; root-locus-breakaway : TF → (List Number)
;;; Find breakaway/break-in points on the real axis.
;;; These are where d(K)/ds = 0, or equivalently where
;;; N(s)*D'(s) - N'(s)*D(s) = 0.
(define (root-locus-breakaway tf)
  (let* ([N (tf-num tf)]
         [D (tf-den tf)]
         [N-prime (poly-derivative N)]
         [D-prime (poly-derivative D)]
         ;; Characteristic equation: N*D' - N'*D = 0
         [char-poly (poly-sub (poly-mul N D-prime)
                              (poly-mul N-prime D))]
         [candidates (poly-roots char-poly)])
        ;; Filter to real candidates on the real axis
        ;; that lie on the root locus (where K > 0)
        (filter (lambda (c)
                        (and (< (abs (complex-imag c)) 1e-6)
                             (let* ([s (complex-real c)]
                                    [Ns (poly-eval N s)]
                                    [Ds (poly-eval D s)])
                                   (and (not (< (abs Ns) 1e-10))
                                        (> (/ (- Ds) Ns) 0)))))
                candidates)))

;;; root-locus-asymptotes : TF → (angles centroid)
;;; Compute asymptote angles and centroid for root locus.
;;; n-m asymptotes at angles (2k+1)*180°/(n-m) from centroid.
(define (root-locus-asymptotes tf)
  (let* ([poles (tf-poles tf)]
         [zeros (tf-zeros tf)]
         [n (length poles)]
         [m (length zeros)]
         [diff (- n m)])
        (if (<= diff 0)
            (list '() 0)  ; No asymptotes
            (let* (;; Centroid = (sum of poles - sum of zeros) / (n - m)
                   [pole-sum (apply + (map complex-real poles))]
                   [zero-sum (apply + (map complex-real zeros))]
                   [centroid (/ (- pole-sum zero-sum) diff)]
                   ;; Angles: (2k+1)*π/(n-m) for k = 0, 1, ..., n-m-1
                   [angles (map (lambda (k)
                                        (* (/ (+ (* 2 k) 1) diff) pi))
                                (iota diff))])
                  (list angles centroid)))))

;;; ====
;;; Nyquist Criterion
;;; ====

;;; The Nyquist criterion determines closed-loop stability by
;;; examining encirclements of the -1 point by the Nyquist plot.
;;;
;;; For open-loop G(s), closed-loop is stable iff:
;;;   N = -P
;;; where N = number of clockwise encirclements of -1
;;;       P = number of open-loop RHP poles

;;; nyquist-plot-points : TF × Nat → (List Complex)
;;; Compute points on Nyquist contour for positive frequencies.
;;; Returns G(jw) for w from 0 to large value.
(define (nyquist-plot-points tf num-points)
  (let* ([w-max 1000]
         [points '()])
        (do ([i 0 (+ i 1)])
            ((= i num-points) (reverse points))
            (let* ([w (if (= i 0)
                          0.001  ; Avoid w=0 for integrators
                          (* w-max (/ i (- num-points 1))))]
                   [jw (make-complex 0 w)]
                   [Gjw (tf-eval tf jw)])
                  (set! points (cons Gjw points))))))

;;; nyquist-count-encirclements : (List Complex) × Complex → Int
;;; Count the number of clockwise encirclements of a point.
;;; Uses the winding number algorithm.
(define (nyquist-count-encirclements contour center)
  (let ([cx (complex-real center)]
        [cy (complex-imag center)])
       ;; Translate contour so center is at origin
       (let ([translated (map (lambda (p)
                                      (make-complex (- (complex-real p) cx)
                                                    (- (complex-imag p) cy)))
                              contour)])
            ;; Compute winding number
            (winding-number translated))))

;;; winding-number : (List Complex) → Int
;;; Compute winding number around origin.
;;; Positive = counterclockwise, negative = clockwise.
(define (winding-number contour)
  (if (< (length contour) 2)
      0
      (let loop ([pts contour] [total-angle 0])
           (if (null? (cdr pts))
               ;; Connect last point to first
               (let ([angle-diff (angle-difference (car pts) (car contour))])
                    (round (/ (+ total-angle angle-diff) (* 2 pi))))
               (let ([angle-diff (angle-difference (car pts) (cadr pts))])
                    (loop (cdr pts) (+ total-angle angle-diff)))))))

;;; angle-difference : Complex × Complex → Number
;;; Compute angle swept from point p1 to p2 relative to origin.
(define (angle-difference p1 p2)
  (let* ([a1 (complex-angle p1)]
         [a2 (complex-angle p2)]
         [diff (- a2 a1)])
        ;; Normalize to [-π, π]
        (cond
         [(> diff pi) (- diff (* 2 pi))]
         [(< diff (- pi)) (+ diff (* 2 pi))]
         [else diff])))

;;; nyquist-stable? : TF → Boolean
;;; Check closed-loop stability using Nyquist criterion.
;;; Assumes open-loop system (input transfer function).
(define (nyquist-stable? tf)
  (let* ([open-loop-poles (tf-poles tf)]
         [rhp-poles (length (filter (lambda (p) (> (complex-real p) 0)) open-loop-poles))]
         [contour (nyquist-plot-points tf 500)]
         [encirclements (nyquist-count-encirclements contour (make-complex -1 0))])
        ;; For stability: N + P = 0, so N = -P
        (= encirclements (- rhp-poles))))

;;; ====
;;; Gain and Phase Margins
;;; ====

;;; Gain margin: how much we can increase gain before instability.
;;; Phase margin: how much phase lag we can add before instability.
;;;
;;; GM = 1/|G(jw_pc)| where w_pc is phase crossover frequency (phase = -180°)
;;; PM = 180° + angle(G(jw_gc)) where w_gc is gain crossover frequency (|G| = 1)

;;; gain-margin : TF → (gm w_pc)
;;; Compute gain margin and phase crossover frequency.
;;; Returns (gain-margin-db phase-crossover-freq) or ('infinite 0) if stable.
(define (gain-margin tf)
  (let* ([w-test (logspace -2 4 1000)]
         [phases (tf-phase tf w-test)]
         [mags (tf-magnitude tf w-test)]
         ;; Find phase crossover: where phase crosses -π
         [pc-result (find-crossover phases (- pi) w-test)])
        (if pc-result
            (let* ([w-pc (car pc-result)]
                   [idx (cdr pc-result)]
                   [mag-at-pc (vector-ref mags idx)]
                   [gm (/ 1 mag-at-pc)]
                   [gm-db (* 20 (log10 gm))])
                  (list gm-db w-pc))
            (list 'infinite 0))))

;;; phase-margin : TF → (pm w_gc)
;;; Compute phase margin and gain crossover frequency.
;;; Returns (phase-margin-deg gain-crossover-freq) or ('infinite 0).
(define (phase-margin tf)
  (let* ([w-test (logspace -2 4 1000)]
         [mags (tf-magnitude tf w-test)]
         [phases (tf-phase tf w-test)]
         ;; Find gain crossover: where magnitude crosses 1
         [gc-result (find-crossover-value mags 1 w-test)])
        (if gc-result
            (let* ([w-gc (car gc-result)]
                   [idx (cdr gc-result)]
                   [phase-at-gc (vector-ref phases idx)]
                   [pm-rad (+ pi phase-at-gc)]
                   [pm-deg (* pm-rad (/ 180 pi))])
                  (list pm-deg w-gc))
            (list 'infinite 0))))

;;; stability-margins : TF → (gm pm w_pc w_gc)
;;; Compute both gain and phase margins.
(define (stability-margins tf)
  (let ([gm-result (gain-margin tf)]
        [pm-result (phase-margin tf)])
       (list (car gm-result)   ; GM in dB
             (car pm-result)   ; PM in degrees
             (cadr gm-result)  ; w_pc
             (cadr pm-result)))) ; w_gc

;;; find-crossover : Vector × Number × Vector → (freq . index) | #f
;;; Find where signal crosses a threshold (for phase).
(define (find-crossover signal threshold freqs)
  (let ([n (vector-length signal)])
       (let loop ([i 0])
            (if (>= i (- n 1))
                #f
                (let ([v1 (vector-ref signal i)]
                      [v2 (vector-ref signal (+ i 1))])
                     (if (and (<= (min v1 v2) threshold)
                              (>= (max v1 v2) threshold))
                         ;; Linear interpolation
                         (let* ([t (/ (- threshold v1) (- v2 v1))]
                                [w1 (vector-ref freqs i)]
                                [w2 (vector-ref freqs (+ i 1))]
                                [w (+ w1 (* t (- w2 w1)))])
                               (cons w i))
                         (loop (+ i 1))))))))

;;; find-crossover-value : Vector × Number × Vector → (freq . index) | #f
;;; Find where signal crosses a value (for magnitude).
(define (find-crossover-value signal target freqs)
  (let ([n (vector-length signal)])
       (let loop ([i 0])
            (if (>= i (- n 1))
                #f
                (let ([v1 (vector-ref signal i)]
                      [v2 (vector-ref signal (+ i 1))])
                     ;; Check for crossing in either direction
                     (if (or (and (<= v1 target) (>= v2 target))
                             (and (>= v1 target) (<= v2 target)))
                         (let* ([t (/ (- target v1) (- v2 v1))]
                                [w1 (vector-ref freqs i)]
                                [w2 (vector-ref freqs (+ i 1))]
                                [w (+ w1 (* t (- w2 w1)))])
                               (cons w i))
                         (loop (+ i 1))))))))

;;; ====
;;; BIBO Stability
;;; ====

;;; BIBO (Bounded-Input Bounded-Output) stability:
;;; A system is BIBO stable if every bounded input produces bounded output.
;;;
;;; For LTI systems:
;;; - Continuous: BIBO stable iff all poles have Re(s) < 0
;;; - Discrete: BIBO stable iff all poles have |z| < 1

;;; bibo-stable? : TF → Boolean
;;; Check BIBO stability for continuous-time transfer function.
(define (bibo-stable? tf)
  (tf-stable? tf))

;;; bibo-stable-discrete? : TF → Boolean
;;; Check BIBO stability for discrete-time transfer function.
(define (bibo-stable-discrete? tf)
  (let ([poles (tf-poles tf)])
       (discrete-stable? poles)))

;;; ====
;;; Controllability and Observability
;;; ====

;;; Controllability matrix: C = [B AB A²B ... A^{n-1}B]
;;; System is controllable if rank(C) = n

;;; controllability-matrix : SS → Matrix
;;; Build the controllability matrix.
(define (controllability-matrix sys)
  (let* ([A (ss-A sys)]
         [B (ss-B sys)]
         [n (matrix-rows A)]
         [m (matrix-cols B)]
         ;; Build columns: B, AB, A²B, ..., A^{n-1}B
         [cols (build-controllability-cols A B n)])
        ;; Combine into single matrix
        (matrix-hcat-list cols)))

;;; build-controllability-cols : Matrix × Matrix × Nat → (List Matrix)
(define (build-controllability-cols A B n)
  (let loop ([i 0] [Ai-B B] [cols '()])
       (if (>= i n)
           (reverse cols)
           (loop (+ i 1)
                 (matrix-mul A Ai-B)
                 (cons Ai-B cols)))))

;;; matrix-hcat-list : (List Matrix) → Matrix
;;; Horizontally concatenate a list of matrices.
(define (matrix-hcat-list matrices)
  (if (null? matrices)
      '(error empty-list)
      (let* ([rows (matrix-rows (car matrices))]
             [total-cols (apply + (map matrix-cols matrices))]
             [result (make-matrix rows total-cols 0)])
            (let loop ([ms matrices] [col-offset 0])
                 (if (null? ms)
                     result
                     (let* ([m (car ms)]
                            [m-cols (matrix-cols m)])
                           (do ([i 0 (+ i 1)])
                               ((= i rows))
                               (do ([j 0 (+ j 1)])
                                   ((= j m-cols))
                                   (matrix-set! result i (+ col-offset j)
                                                (matrix-ref m i j))))
                           (loop (cdr ms) (+ col-offset m-cols))))))))

;;; controllable? : SS → Boolean
;;; Check if system is controllable.
(define (controllable? sys)
  (let* ([C (controllability-matrix sys)]
         [n (ss-order sys)]
         [rank (stability-matrix-rank C)])
        (= rank n)))

;;; observability-matrix : SS → Matrix
;;; Build the observability matrix: O = [C; CA; CA²; ...; CA^{n-1}]
(define (observability-matrix sys)
  (let* ([A (ss-A sys)]
         [C (ss-C sys)]
         [n (matrix-rows A)]
         ;; Build rows: C, CA, CA², ..., CA^{n-1}
         [rows (build-observability-rows C A n)])
        ;; Combine into single matrix
        (matrix-vcat-list rows)))

;;; build-observability-rows : Matrix × Matrix × Nat → (List Matrix)
(define (build-observability-rows C A n)
  (let loop ([i 0] [CAi C] [rows '()])
       (if (>= i n)
           (reverse rows)
           (loop (+ i 1)
                 (matrix-mul CAi A)
                 (cons CAi rows)))))

;;; matrix-vcat-list : (List Matrix) → Matrix
;;; Vertically concatenate a list of matrices.
(define (matrix-vcat-list matrices)
  (if (null? matrices)
      '(error empty-list)
      (let* ([cols (matrix-cols (car matrices))]
             [total-rows (apply + (map matrix-rows matrices))]
             [result (make-matrix total-rows cols 0)])
            (let loop ([ms matrices] [row-offset 0])
                 (if (null? ms)
                     result
                     (let* ([m (car ms)]
                            [m-rows (matrix-rows m)])
                           (do ([i 0 (+ i 1)])
                               ((= i m-rows))
                               (do ([j 0 (+ j 1)])
                                   ((= j cols))
                                   (matrix-set! result (+ row-offset i) j
                                                (matrix-ref m i j))))
                           (loop (cdr ms) (+ row-offset m-rows))))))))

;;; observable? : SS → Boolean
;;; Check if system is observable.
(define (observable? sys)
  (let* ([O (observability-matrix sys)]
         [n (ss-order sys)]
         [rank (stability-matrix-rank O)])
        (= rank n)))

;;; ====
;;; Utility Functions
;;; ====

;;; stability-matrix-rank : Matrix → Nat
;;; Compute matrix rank using row echelon form.
;;; Local definition to avoid conflicts with other rank implementations.
(define (stability-matrix-rank M)
  (let* ([m (matrix-rows M)]
         [n (matrix-cols M)]
         ;; Copy matrix for in-place operations
         [A (matrix-copy M)]
         [tol 1e-10])
        ;; Row echelon form, count non-zero pivot rows
        (let row-loop ([row 0] [col 0] [rank 0])
             (if (or (>= row m) (>= col n))
                 rank
                 ;; Find pivot
                 (let pivot-loop ([pr row])
                      (cond
                       [(>= pr m)
                        ;; No pivot in this column, try next
                        (row-loop row (+ col 1) rank)]
                       [(> (abs (matrix-ref A pr col)) tol)
                        ;; Found pivot, swap rows if needed
                        (when (> pr row)
                              (matrix-swap-rows! A row pr))
                        ;; Eliminate below
                        (do ([i (+ row 1) (+ i 1)])
                            ((>= i m))
                            (let ([factor (/ (matrix-ref A i col) (matrix-ref A row col))])
                                 (do ([j col (+ j 1)])
                                     ((>= j n))
                                     (matrix-set! A i j
                                                  (- (matrix-ref A i j)
                                                     (* factor (matrix-ref A row j)))))))
                        (row-loop (+ row 1) (+ col 1) (+ rank 1))]
                       [else
                        (pivot-loop (+ pr 1))]))))))

;;; matrix-copy : Matrix → Matrix
(define (matrix-copy M)
  (let* ([m (matrix-rows M)]
         [n (matrix-cols M)]
         [result (make-matrix m n 0)])
        (do ([i 0 (+ i 1)])
            ((= i m) result)
            (do ([j 0 (+ j 1)])
                ((= j n))
                (matrix-set! result i j (matrix-ref M i j))))))

;;; matrix-swap-rows! : Matrix × Nat × Nat → void
(define (matrix-swap-rows! M i j)
  (let ([n (matrix-cols M)])
       (do ([k 0 (+ k 1)])
           ((= k n))
           (let ([tmp (matrix-ref M i k)])
                (matrix-set! M i k (matrix-ref M j k))
                (matrix-set! M j k tmp)))))

;;; all? : (α → Boolean) × (List α) → Boolean
(define (all? pred lst)
  (cond
   [(null? lst) #t]
   [(not (pred (car lst))) #f]
   [else (all? pred (cdr lst))]))

;;; any? : (α → Boolean) × (List α) → Boolean
(define (any? pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any? pred (cdr lst))]))

;;; iota : Nat → (List Nat)
;;; Generate list (0 1 2 ... n-1)
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; pi constant
(define pi 3.141592653589793)

;;; ====
;;; Display Functions
;;; ====

;;; stability-report : TF → String
;;; Generate a stability report for a transfer function.
(define (stability-report tf)
  (let* ([poles (tf-poles tf)]
         [stable (tf-stable? tf)]
         [rh-stable (routh-hurwitz-stable? (tf-den tf))]
         [margins (stability-margins tf)]
         [gm (car margins)]
         [pm (cadr margins)])
        (string-append
         "=== Stability Report ===\n"
         "Poles: " (poles->string poles) "\n"
         "Pole-based stable: " (if stable "YES" "NO") "\n"
         "Routh-Hurwitz stable: " (if rh-stable "YES" "NO") "\n"
         "Gain margin: " (if (eq? gm 'infinite) "infinite" (number->string gm)) " dB\n"
         "Phase margin: " (if (eq? pm 'infinite) "infinite" (number->string pm)) " deg\n")))

;;; poles->string : (List Complex) → String
(define (poles->string poles)
  (if (null? poles)
      "()"
      (let ([strs (map complex->string poles)])
           (string-append "(" (string-join strs ", ") ")"))))

;;; complex->string : Complex → String
(define (complex->string c)
  (let ([re (complex-real c)]
        [im (complex-imag c)])
       (cond
        [(< (abs im) 1e-10)
         (number->string re)]
        [(< (abs re) 1e-10)
         (string-append (number->string im) "j")]
        [(>= im 0)
         (string-append (number->string re) "+" (number->string im) "j")]
        [else
         (string-append (number->string re) (number->string im) "j")])))

;;; string-join : (List String) × String → String
(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ([s (car strs)] [rest (cdr strs)])
           (if (null? rest)
               s
               (loop (string-append s sep (car rest)) (cdr rest))))))
