;;; lattice/fp/control-systems/tf-convert.ss — State Space ↔ Transfer Function Conversions
;;;
;;; Algorithms for converting between state-space and transfer function
;;; representations of linear time-invariant systems.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/fp/control-systems/state-space.ss
;;;   - lattice/fp/control-systems/transfer-function.ss
;;;   - lattice/linalg/matrix.ss
;;;   - lattice/linalg/matrix-solvers.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/fp/control-systems/state-space.ss")
(load "lattice/fp/control-systems/transfer-function.ss")

;;; ============================================================
;;; State Space to Transfer Function
;;; ============================================================

;;; ss->tf : SS × [Nat] × [Nat] → TF
;;; Convert state-space system to transfer function.
;;; For MIMO systems, specify input-idx and output-idx (0-based).
;;; For SISO systems (single input, single output), these default to 0.
;;;
;;; The transfer function is: H(s) = C(sI - A)^{-1}B + D
;;;
;;; For computation, we use the formula:
;;;   H(s) = (C * adj(sI-A) * B + D * det(sI-A)) / det(sI-A)
;;;
;;; The denominator is the characteristic polynomial: det(sI - A)
;;; The numerator comes from the adjugate computation.
(define (ss->tf sys . opts)
  (let* ([input-idx (if (and (pair? opts) (car opts)) (car opts) 0)]
         [output-idx (if (and (pair? opts) (pair? (cdr opts)) (cadr opts)) (cadr opts) 0)]
         [A (ss-A sys)]
         [B (ss-B sys)]
         [C (ss-C sys)]
         [D (ss-D sys)]
         [n (ss-order sys)])
        (if (= n 0)
            ;; Trivial case: static gain
            (tf-from-lists (list (matrix-ref D output-idx input-idx))
                           (list 1))
            ;; General case
            (ss->tf-leverrier A B C D input-idx output-idx n))))

;;; ss->tf-leverrier : Matrix × Matrix × Matrix × Matrix × Nat × Nat × Nat → TF
;;; Use Faddeev-LeVerrier algorithm to compute the transfer function.
;;; This computes the characteristic polynomial and adjugate simultaneously.
(define (ss->tf-leverrier A B C D input-idx output-idx n)
  (let* ([b-col (matrix-column B input-idx)]
         [c-row (matrix-row C output-idx)]
         [d-val (matrix-ref D output-idx input-idx)]
         ;; Faddeev-LeVerrier iteration
         [fl-result (faddeev-leverrier A n)]
         [char-coeffs (car fl-result)]   ; Characteristic polynomial coefficients
         [adj-matrices (cdr fl-result)]  ; Adjugate polynomial matrices
         ;; Compute numerator coefficients: C * adj_k * B + d * char_k
         [num-coeffs (compute-tf-numerator c-row adj-matrices b-col d-val char-coeffs n)])
        (tf-from-coeffs num-coeffs char-coeffs)))

;;; faddeev-leverrier : Matrix × Nat → (char-coeffs . adj-matrices)
;;; Faddeev-LeVerrier algorithm for computing characteristic polynomial
;;; and adjugate matrices.
;;;
;;; Computes: det(sI - A) = s^n + p_1*s^{n-1} + ... + p_n
;;; And: adj(sI - A) = M_0*s^{n-1} + M_1*s^{n-2} + ... + M_{n-1}
;;;
;;; Returns: (coeffs . matrices) where
;;;   coeffs = vector of char poly coefficients [1, p_1, p_2, ..., p_n]
;;;   matrices = list of adjugate coefficient matrices [M_0, M_1, ..., M_{n-1}]
(define (faddeev-leverrier A n)
  (let ([coeffs (make-vector (+ n 1) 0)]
        [matrices '()])
       ;; Initialize
       (vector-set! coeffs 0 1)  ; Leading coefficient is 1
       (let loop ([k 1]
                  [M (identity n)]
                  [mats (list (identity n))])
            (if (> k n)
                (cons coeffs (reverse mats))
                (let* ([AM (matrix-mul A M)]
                       [p-k (/ (- (matrix-trace AM)) k)]
                       [M-next (if (= k n)
                                   (make-matrix n n 0)  ; Not used
                                   (matrix-add AM (matrix-scale p-k (identity n))))])
                      (vector-set! coeffs k p-k)
                      (if (= k n)
                          (cons coeffs (reverse mats))
                          (loop (+ k 1) M-next (cons M-next mats))))))))

;;; compute-tf-numerator : Vec × (List Matrix) × Vec × Number × Vec × Nat → Vec
;;; Compute the numerator polynomial coefficients.
;;; num(s) = C * (M_0*s^{n-1} + ... + M_{n-1}) * B + D * (s^n + p_1*s^{n-1} + ...)
(define (compute-tf-numerator c-row adj-matrices b-col d-val char-coeffs n)
  (let* ([num-len (+ n 1)]
         [result (make-vector num-len 0)])
        ;; D contributes: D * char_poly
        (do ([i 0 (+ i 1)])
            ((= i num-len))
            (vector-set! result i (* d-val (vector-ref char-coeffs i))))
        ;; C * adj_k * B contributes to coefficient of s^{n-1-k}
        (let loop ([k 0] [mats adj-matrices])
             (if (null? mats)
                 result
                 (let* ([M (car mats)]
                        [CB (dot-product-cv c-row (matrix-vec-mul M b-col))]
                        [coeff-idx (+ 1 k)])  ; s^{n-1-k} maps to index k+1
                       (if (< coeff-idx num-len)
                           (vector-set! result coeff-idx
                                        (+ (vector-ref result coeff-idx) CB)))
                       (loop (+ k 1) (cdr mats)))))))

;;; ============================================================
;;; Transfer Function to State Space
;;; ============================================================

;;; tf->ss : TF → SS
;;; Convert transfer function to state-space in controllable canonical form.
;;;
;;; For H(s) = (b_m*s^m + ... + b_0) / (s^n + a_{n-1}*s^{n-1} + ... + a_0)
;;; where m <= n (proper TF), the controllable canonical form is:
;;;
;;;   A = | 0   1   0  ...  0     |     B = | 0 |
;;;       | 0   0   1  ...  0     |         | 0 |
;;;       | ...                   |         |...|
;;;       | 0   0   0  ...  1     |         | 0 |
;;;       |-a_0 -a_1 -a_2 ... -a_{n-1}|     | 1 |
;;;
;;;   C = | b_0-a_0*b_n, b_1-a_1*b_n, ..., b_{n-1}-a_{n-1}*b_n |
;;;
;;;   D = | b_n | (zero if strictly proper, i.e., m < n)
(define (tf->ss tf)
  (let* ([num (tf-num tf)]
         [den (tf-den tf)]
         [n (poly-degree den)]
         [m (poly-degree num)])
        (cond
         [(= n 0)
          ;; Constant gain: D = num/den, A,B,C all zero
          (let ([gain (/ (poly-leading num) (poly-leading den))])
               (make-ss (make-matrix 0 0 0)
                        (make-matrix 0 1 0)
                        (make-matrix 1 0 0)
                        (make-matrix 1 1 gain)))]
         [else
          ;; General case: build controllable canonical form
          (tf->ss-controllable num den n m)])))

;;; tf->ss-controllable : Poly × Poly × Nat × Nat → SS
;;; Build controllable canonical form.
(define (tf->ss-controllable num den n m)
  (let* ([den-coeffs (poly-coeffs den)]  ; Normalized (monic)
         [num-coeffs (poly-coeffs num)]
         ;; Pad numerator if degree < n
         [num-padded (pad-poly-coeffs num-coeffs (+ n 1))]
         ;; Extract D (direct feedthrough)
         [b-n (vector-ref num-padded 0)]  ; Coefficient of s^n in numerator
         ;; Build A matrix
         [A (make-matrix n n 0)])
        ;; Set superdiagonal to 1
        (do ([i 0 (+ i 1)])
            ((= i (- n 1)))
            (matrix-set! A i (+ i 1) 1))
        ;; Set last row to -a_i (coefficients in reverse order)
        (do ([j 0 (+ j 1)])
            ((= j n))
            (matrix-set! A (- n 1) j (- (vector-ref den-coeffs (- n j)))))
        ;; Build B matrix (column vector with 1 at bottom)
        (let ([B (make-matrix n 1 0)])
             (matrix-set! B (- n 1) 0 1)
             ;; Build C matrix
             (let ([C (make-matrix 1 n 0)])
                  ;; C[j] = b_j - a_j * b_n for j = 0..n-1
                  (do ([j 0 (+ j 1)])
                      ((= j n))
                      (let* ([b-j (vector-ref num-padded (- n j))]
                             [a-j (vector-ref den-coeffs (- n j))]
                             [c-j (- b-j (* a-j b-n))])
                            (matrix-set! C 0 j c-j)))
                  ;; Build D matrix
                  (let ([D (make-matrix 1 1 b-n)])
                       (make-ss A B C D))))))

;;; pad-poly-coeffs : Vec × Nat → Vec
;;; Pad polynomial coefficients to length n (prepending zeros).
(define (pad-poly-coeffs coeffs n)
  (let ([len (vector-length coeffs)])
       (if (>= len n)
           coeffs
           (let ([result (make-vector n 0)]
                 [offset (- n len)])
                (do ([i 0 (+ i 1)])
                    ((= i len) result)
                    (vector-set! result (+ i offset) (vector-ref coeffs i)))))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; matrix-column : Matrix × Nat → Vec
;;; Extract column j as a vector.
(define (matrix-column M j)
  (let* ([rows (matrix-rows M)]
         [result (make-vector rows)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (vector-set! result i (matrix-ref M i j)))))

;;; matrix-row : Matrix × Nat → Vec
;;; Extract row i as a vector.
(define (matrix-row M i)
  (let* ([cols (matrix-cols M)]
         [result (make-vector cols)])
        (do ([j 0 (+ j 1)])
            ((= j cols) result)
            (vector-set! result j (matrix-ref M i j)))))

;;; matrix-vec-mul : Matrix × Vec → Vec
;;; Multiply matrix by column vector.
(define (matrix-vec-mul M v)
  (let* ([rows (matrix-rows M)]
         [cols (matrix-cols M)]
         [result (make-vector rows 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (let loop ([j 0] [sum 0])
                 (if (= j cols)
                     (vector-set! result i sum)
                     (loop (+ j 1) (+ sum (* (matrix-ref M i j) (vector-ref v j)))))))))

;;; dot-product-cv : Vec × Vec → Number
;;; Dot product of two vectors (c-row . b-col).
(define (dot-product-cv v1 v2)
  (let ([n (vector-length v1)])
       (let loop ([i 0] [sum 0])
            (if (= i n)
                sum
                (loop (+ i 1) (+ sum (* (vector-ref v1 i) (vector-ref v2 i))))))))

;;; matrix-trace : Matrix → Number
;;; Sum of diagonal elements.
(define (matrix-trace M)
  (let ([n (min (matrix-rows M) (matrix-cols M))])
       (let loop ([i 0] [sum 0])
            (if (= i n)
                sum
                (loop (+ i 1) (+ sum (matrix-ref M i i)))))))
