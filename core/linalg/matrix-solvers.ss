;;; fabric/stitches/matrix-solvers.ss — Linear Equation Solvers
;;;
;;; Solvers for Ax = b and related problems.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies (must be loaded by client in correct order):
;;;   - prelude.ss
;;;   - matrix.ss
;;;   - matrix-decomp.ss
;;;   - matrix-solvers.ss (this file)

;;; ============================================================
;;; Basic Substitution Utilities
;;; ============================================================

;;; matrix-forward-substitute : Matrix × Vec → Vec
;;; Solve Ly = b where L is lower triangular.
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

;;; ============================================================
;;; LU-Based Solvers
;;; ============================================================

;;; matrix-solve : Matrix × Vec → Vec | Error
;;; Solve Ax = b for square A using LU decomposition.
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

;;; ============================================================
;;; Determinant
;;; ============================================================

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

;;; ============================================================
;;; Least Squares
;;; ============================================================

;;; matrix-least-squares : Matrix × Vec → Vec | Error
;;; Solve the overdetermined system Ax = b in the least-squares sense using QR.
;;; Assumes A is m x n with m >= n.
(define (matrix-least-squares a b)
  (let ([qr (matrix-qr a)])
       (if (and (pair? qr) (eq? (car qr) 'error))
           qr
           (let* ([q (car qr)]
                  [r (cadr qr)]
                  ;; Solve Rx = Q^T b
                  ;; Qt-b = Q^T * b
                  [qt-b (vec-matrix-mul b q)])
                 (matrix-back-substitute r qt-b)))))

;;; ============================================================
;;; Gaussian Elimination and Rank
;;; ============================================================

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

;;; ============================================================
;;; Numerical Properties
;;; ============================================================

;;; matrix-condition-number : Matrix → Num | Error
;;; Compute the condition number of a matrix using the Frobenius norm.
;;; kappa(A) = ||A||_F * ||A^-1||_F. High values indicate ill-conditioning.
(define (matrix-condition-number a)
  (let ([inv (matrix-inverse a)])
       (if (and (pair? inv) (eq? (car inv) 'error))
           inv
           (* (frobenius-norm a) (frobenius-norm inv)))))
