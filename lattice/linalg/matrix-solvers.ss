(doc 'module 'matrix-solvers
     'description "Linear Equation Solvers

Solvers for Ax = b and related problems.

This is Core code: pure, total, assumes reasonable input.

Dependencies (must be loaded by client in correct order):
  - prelude.ss
  - matrix.ss
  - matrix-decomp.ss
  - matrix-solvers.ss (this file)")

(doc 'section 'substitution
     'description "Basic forward and back substitution utilities")

(doc matrix-forward-substitute
     'type (-> Matrix Vec Vec)
     'description "Solve Ly = b where L is lower triangular")
(define (matrix-forward-substitute l b)
  (let* ([n (vector-length b)]
         [y (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            [(= i n) y]
            (let ([sum (let loop ([j 0] [s 0])
                            (if (= j i)
                                s
                                (loop (+ j 1)
                                      (+ s (* (matrix-ref l i j)
                                              (vector-ref y j))))))])
                 (vector-set! y i (/ (- (vector-ref b i) sum)
                                     (matrix-ref l i i)))))))

;;; matrix-back-substitute : Matrix × Vec → Vec
;;; Solve Ux = y where U is upper triangular.
(define (matrix-back-substitute u y)
  (let* ([n (vector-length y)]
         [x (make-vector n 0)])
        (do ([i (- n 1) (- i 1)])
            [(< i 0) x]
            (let ([sum (let loop ([j (+ i 1)] [s 0])
                            (if (= j n)
                                s
                                (loop (+ j 1)
                                      (+ s (* (matrix-ref u i j)
                                              (vector-ref x j))))))])
                 (vector-set! x i (/ (- (vector-ref y i) sum)
                                     (matrix-ref u i i)))))))

(doc 'section 'lu-solvers
     'description "LU decomposition-based solvers")

(doc matrix-solve
     'type (-> Matrix Vec (or Vec Error))
     'description "Solve Ax = b for square A using LU decomposition")
(define (matrix-solve a b)
  (let ([lu (matrix-lu a)])
       (if (and (pair? lu) (eq? (car lu) 'error))
           lu
           (matrix-lu-solve lu b))))

;;; matrix-inverse : Matrix → Matrix | Error
;;; Compute the inverse of a square matrix A using LU decomposition.
(define (matrix-inverse a)
  (let* ([n (matrix-rows a)]
         [lu (matrix-lu a)])
        (if (and (pair? lu) (eq? (car lu) 'error))
            lu
            (let* ([inv-data (make-vector (* n n) 0)]
                   [e (make-vector n 0)])
                  (do ([j 0 (+ j 1)])
                      [(= j n) (list 'matrix n n inv-data)]
                      ;; Setup e_j (j-th column of identity matrix)
                      (do ([i 0 (+ i 1)]) [(= i n)] (vector-set! e i 0))
                      (vector-set! e j 1)
                      ;; Solve Ax_j = e_j
                      (let ([xj (matrix-lu-solve lu e)])
                           ;; Copy xj to j-th column of inv-data
                           (do ([i 0 (+ i 1)])
                               [(= i n)]
                               (vector-set! inv-data (+ (* i n) j) (vector-ref xj i)))))))))

;;; ====
;;; Determinant
;;; ====

;;; permutation-parity : Vec Nat → Int
;;; Compute the parity of a permutation vector. Returns 1 for even, -1 for odd.
(define (permutation-parity p)
  (let* ([n (vector-length p)]
         [visited (make-vector n #f)])
        (let loop ([i 0] [swaps 0])
             (if (= i n)
                 (if (even? swaps) 1 -1)
                 (if (vector-ref visited i)
                     (loop (+ i 1) swaps)
                     (begin
                      (let cycle-loop ([curr i] [len 0])
                           (if (vector-ref visited curr)
                               (loop (+ i 1) (+ swaps (- len 1)))
                               (begin
                                (vector-set! visited curr #t)
                                (cycle-loop (vector-ref p curr) (+ len 1)))))))))))

;;; matrix-determinant : Matrix → Num | Error
;;; Compute the determinant of a square matrix A using LU decomposition.
(define (matrix-determinant a)
  (if (not (matrix-square? a))
      `(error not-square ,(matrix-rows a) ,(matrix-cols a))
      (let ([lu (matrix-lu a)])
           (if (and (pair? lu) (eq? (car lu) 'error))
               (if (eq? (cadr lu) 'singular-matrix)
                   0
                   lu)
               (let* ([u (cadr lu)]
                      [p (caddr lu)]
                      [diag (matrix-diagonal u)]
                      [det-u (vec-fold * 1 diag)]
                      [parity (permutation-parity p)])
                     (* parity det-u))))))

(doc 'section 'least-squares
     'description "Least squares solutions for overdetermined systems")

(doc matrix-least-squares
     'type (-> Matrix Vec (or Vec Error))
     'description "Solve the overdetermined system Ax = b in the least-squares sense using QR. Assumes A is m x n with m >= n.")
(define (matrix-least-squares a b)
  (let ([qr (matrix-qr a)])
       (if (and (pair? qr) (eq? (car qr) 'error))
           qr
           (let* ([q (car qr)]
                  [r (cadr qr)]
                  ;; Solve Rx = Q^T b
                  ;; Qt-b = Q^T * b (transpose Q then multiply by b)
                  [qt-b (matrix-vec-mul (matrix-transpose q) b)])
                 (matrix-back-substitute r qt-b)))))

;;; ====
;;; Gaussian Elimination and Rank
;;; ====

;;; matrix-gauss-elim : Matrix → Matrix
;;; Compute Row Echelon Form (REF) using Gaussian elimination with partial pivoting.
(define (matrix-gauss-elim a)
  (let* ([m (matrix-rows a)]
         [n (matrix-cols a)]
         [ref (matrix-copy a)])
        (let loop ([k 0] [i 0]) ; k is column, i is row
             (if (or (= k n) (= i m))
                 ref
                 (let find-pivot ([r i] [max-row i] [max-val (abs (matrix-ref ref i k))])
                      (if (= r m)
                          (if (< max-val *matrix-tolerance*)
                              ;; Skip this column, no pivot found
                              (loop (+ k 1) i)
                              (begin
                               ;; Swap rows
                               (when (not (= max-row i))
                                     (let ([cols (matrix-cols ref)]
                                           [data (matrix-data ref)])
                                          (do ([col 0 (+ col 1)])
                                              [(= col cols)]
                                              (let ([idx-i (+ (* i cols) col)]
                                                    [idx-max (+ (* max-row cols) col)])
                                                   (let ([temp (vector-ref data idx-i)])
                                                        (vector-set! data idx-i (vector-ref data idx-max))
                                                        (vector-set! data idx-max temp))))))
                               ;; Eliminate below
                               (let ([pivot-val (matrix-ref ref i k)])
                                    (do ([r-elim (+ i 1) (+ r-elim 1)])
                                        [(= r-elim m)]
                                        (let ([factor (/ (matrix-ref ref r-elim k) pivot-val)])
                                             (do ([c k (+ c 1)])
                                                 [(= c n)]
                                                 (matrix-set! ref r-elim c
                                                              (- (matrix-ref ref r-elim c)
                                                                 (* factor (matrix-ref ref i c))))))))
                               (loop (+ k 1) (+ i 1))))
                          (let ([val (abs (matrix-ref ref r k))])
                               (if (> val max-val)
                                   (find-pivot (+ r 1) r val)
                                   (find-pivot (+ r 1) max-row max-val)))))))))

;;; matrix-rank : Matrix → Nat
;;; Compute the rank of a matrix (number of linearly independent rows/columns).
(define (matrix-rank a)
  (let* ([ref (matrix-gauss-elim a)]
         [m (matrix-rows ref)]
         [n (matrix-cols ref)])
        (let loop ([i 0] [rank 0])
             (if (= i m)
                 rank
                 (let row-loop ([j 0])
                      (cond
                       [(= j n) (loop (+ i 1) rank)]
                       [(> (abs (matrix-ref ref i j)) *matrix-tolerance*)
                        (loop (+ i 1) (+ rank 1))]
                       [else (row-loop (+ j 1))]))))))

;;; ====
;;; Numerical Properties
;;; ====

;;; matrix-condition-number : Matrix → Num | Error
;;; Compute the condition number of a matrix using the Frobenius norm.
;;; kappa(A) = ||A||_F * ||A^-1||_F. High values indicate ill-conditioning.
(define (matrix-condition-number a)
  (let ([inv (matrix-inverse a)])
       (if (and (pair? inv) (eq? (car inv) 'error))
           inv
           (* (frobenius-norm a) (frobenius-norm inv)))))

(doc 'section 'toeplitz-solvers
     'description "Levinson-Durbin recursion for Toeplitz systems")

(doc levinson-durbin
     'type (-> Vec Vec)
     'description "Solve the Yule-Walker equations using Levinson-Durbin recursion.")
;;;
;;; Input:  r = [r_0, r_1, ..., r_p] autocorrelations (r_0 = 1 for normalized ACF)
;;; Output: phi = [phi_1, ..., phi_p] the AR coefficients
;;;
;;; Solves: R * phi = [r_1, ..., r_p]
;;; where R is the p×p Toeplitz matrix with R[i,j] = r_{|i-j|}
;;;
;;; O(p²) time, O(p) space - much faster than O(p³) LU decomposition.
;;;
;;; Returns (error singular k) if the matrix is singular at step k.
(define (levinson-durbin r)
  (let* ([n (- (vector-length r) 1)]  ; p = n
         [phi (make-vector n 0.0)]
         [phi-tmp (make-vector n 0.0)]
         [e (vector-ref r 0)])        ; prediction error variance
    (if (= n 0)
        (make-vector 0)
        (let loop ([m 1])
          (if (> m n)
              phi
              ;; Compute reflection coefficient lambda_m
              (let ([lambda-num
                     (- (vector-ref r m)
                        (let inner ([k 1] [sum 0.0])
                          (if (>= k m)
                              sum
                              (inner (+ k 1)
                                     (+ sum (* (vector-ref phi (- k 1))
                                               (vector-ref r (- m k))))))))])
                (if (< (abs e) 1e-15)
                    ;; Matrix is singular
                    (list 'error 'singular m)
                    (let ([lambda-m (/ lambda-num e)])
                      ;; Update AR coefficients using order-update recursion
                      ;; phi^(m)_k = phi^(m-1)_k - lambda_m * phi^(m-1)_{m-k}
                      (do ([k 1 (+ k 1)])
                          [(>= k m)]
                        (vector-set! phi-tmp (- k 1)
                                     (- (vector-ref phi (- k 1))
                                        (* lambda-m (vector-ref phi (- m k 1))))))
                      ;; phi^(m)_m = lambda_m
                      (vector-set! phi-tmp (- m 1) lambda-m)
                      ;; Copy tmp back to phi
                      (do ([k 0 (+ k 1)])
                          [(= k m)]
                        (vector-set! phi k (vector-ref phi-tmp k)))
                      ;; Update error variance: E_m = E_{m-1} * (1 - lambda_m^2)
                      (set! e (* e (- 1 (* lambda-m lambda-m))))
                      (loop (+ m 1))))))))))

;;; levinson-durbin-general : Vec × Vec → Vec
;;; Solve Tx = b where T is a symmetric Toeplitz matrix.
;;;
;;; Input:  r = [r_0, r_1, ..., r_{n-1}] defining T where T[i,j] = r_{|i-j|}
;;;         b = [b_0, ..., b_{n-1}] right-hand side
;;; Output: x = [x_0, ..., x_{n-1}] solution
;;;
;;; Uses generalized Levinson recursion: O(n²) time.
;;;
;;; Note: For Yule-Walker equations (AR fitting), use levinson-durbin instead
;;; as it's optimized for that specific structure.
(define (levinson-durbin-general r b)
  (let* ([n (vector-length r)]
         [x (make-vector n 0.0)]
         [x-tmp (make-vector n 0.0)]
         [a (make-vector n 0.0)]       ; auxiliary vector for reflection coeffs
         [a-tmp (make-vector n 0.0)]
         [r0 (vector-ref r 0)])
    (if (= n 0)
        x
        (if (< (abs r0) 1e-15)
            (list 'error 'singular 0)
            ;; Initialize: x^(0) = b_0/r_0, a^(0) = -r_1/r_0
            (begin
              (vector-set! x 0 (/ (vector-ref b 0) r0))
              (if (= n 1)
                  x
                  (let ([e r0])  ; error term
                    (vector-set! a 0 (- (/ (vector-ref r 1) r0)))
                    (set! e (* e (- 1 (expt (vector-ref a 0) 2))))
                    (let loop ([m 1])
                      (if (= m n)
                          x
                          (if (< (abs e) 1e-15)
                              (list 'error 'singular m)
                              ;; Compute delta for x update
                              (let* ([delta
                                      (- (vector-ref b m)
                                         (let inner ([k 0] [sum 0.0])
                                           (if (= k m)
                                               sum
                                               (inner (+ k 1)
                                                      (+ sum (* (vector-ref x k)
                                                                (vector-ref r (- m k))))))))]
                                     ;; Update x
                                     [delta-e (/ delta e)])
                                ;; x^(m)_k = x^(m-1)_k + delta/e * a^(m-1)_{m-1-k}
                                (do ([k 0 (+ k 1)])
                                    [(= k m)]
                                  (vector-set! x-tmp k
                                               (+ (vector-ref x k)
                                                  (* delta-e (vector-ref a (- m 1 k))))))
                                (vector-set! x-tmp m delta-e)
                                ;; Copy x-tmp to x
                                (do ([k 0 (+ k 1)])
                                    [(= k (+ m 1))]
                                  (vector-set! x k (vector-ref x-tmp k)))
                                ;; Update auxiliary vector a if not done
                                (if (< m (- n 1))
                                    (let* ([lambda-num
                                            (let inner ([k 0] [sum (vector-ref r (+ m 1))])
                                              (if (= k m)
                                                  sum
                                                  (inner (+ k 1)
                                                         (+ sum (* (vector-ref a k)
                                                                   (vector-ref r (- m k)))))))]
                                           [lambda-m (- (/ lambda-num e))])
                                      ;; a^(m)_k = a^(m-1)_k + lambda * a^(m-1)_{m-1-k}
                                      (do ([k 0 (+ k 1)])
                                          [(= k m)]
                                        (vector-set! a-tmp k
                                                     (+ (vector-ref a k)
                                                        (* lambda-m (vector-ref a (- m 1 k))))))
                                      (vector-set! a-tmp m lambda-m)
                                      ;; Copy a-tmp to a
                                      (do ([k 0 (+ k 1)])
                                          [(= k (+ m 1))]
                                        (vector-set! a k (vector-ref a-tmp k)))
                                      ;; Update error
                                      (set! e (* e (- 1 (* lambda-m lambda-m))))
                                      (loop (+ m 1)))
                                    (loop (+ m 1))))))))))))))
