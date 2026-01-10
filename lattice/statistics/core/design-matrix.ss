;;; lattice/statistics/core/design-matrix.ss — Design Matrix Construction
;;;
;;; Utilities for constructing design matrices for regression.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - Intercept addition
;;;   - Column standardization (z-score)
;;;   - Dummy encoding for categorical variables
;;;   - Polynomial feature expansion
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/matrix.ss
;;;   - summary-stats.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/statistics/core/summary-stats.ss")

;;; ============================================================
;;; Intercept Handling
;;; ============================================================

;;; add-intercept : Matrix → Matrix
;;; Prepend a column of 1s to the matrix.
(define (add-intercept X)
  (let* ([m (matrix-rows X)]
         [n (matrix-cols X)]
         [new-data (make-vector (* m (+ n 1)))])
        ;; Fill new matrix: first column is 1s, rest is X
        (do ([i 0 (+ i 1)])
            [(= i m)]
            ;; Set intercept column
            (vector-set! new-data (* i (+ n 1)) 1)
            ;; Copy existing columns
            (do ([j 0 (+ j 1)])
                [(= j n)]
                (vector-set! new-data (+ (* i (+ n 1)) j 1)
                             (matrix-ref X i j))))
        (list 'matrix m (+ n 1) new-data)))

;;; has-intercept? : Matrix → Bool
;;; Check if first column is all 1s (likely an intercept).
(define (has-intercept? X)
  (let ([m (matrix-rows X)])
       (let loop ([i 0])
            (if (= i m)
                #t
                (if (= (matrix-ref X i 0) 1)
                    (loop (+ i 1))
                    #f)))))

;;; ============================================================
;;; Standardization
;;; ============================================================

;;; column-mean : Matrix × Nat → Num
;;; Compute mean of a column.
(define (column-mean X j)
  (let* ([m (matrix-rows X)]
         [sum (let loop ([i 0] [s 0])
                   (if (= i m)
                       s
                       (loop (+ i 1) (+ s (matrix-ref X i j)))))])
        (/ sum m)))

;;; column-std : Matrix × Nat → Num
;;; Compute sample standard deviation of a column.
(define (column-std X j)
  (let* ([m (matrix-rows X)]
         [mu (column-mean X j)]
         [ss (let loop ([i 0] [s 0])
                  (if (= i m)
                      s
                      (loop (+ i 1)
                            (+ s (expt (- (matrix-ref X i j) mu) 2)))))])
        (if (<= m 1)
            0
            (sqrt (/ ss (- m 1))))))

;;; standardize-columns : Matrix → (Values Matrix Vec Vec)
;;; Standardize all columns to mean 0, std 1.
;;; Returns (standardized-matrix, means-vector, stds-vector).
;;; Note: Columns with std=0 are left unchanged (set to 0).
(define (standardize-columns X)
  (let* ([m (matrix-rows X)]
         [n (matrix-cols X)]
         [means (make-vector n)]
         [stds (make-vector n)]
         [new-data (make-vector (* m n))])
        ;; Compute means and stds
        (do ([j 0 (+ j 1)])
            [(= j n)]
            (vector-set! means j (column-mean X j))
            (vector-set! stds j (column-std X j)))
        ;; Standardize
        (do ([i 0 (+ i 1)])
            [(= i m)]
            (do ([j 0 (+ j 1)])
                [(= j n)]
                (let* ([mu (vector-ref means j)]
                       [s (vector-ref stds j)]
                       [x (matrix-ref X i j)]
                       [z (if (= s 0) 0 (/ (- x mu) s))])
                      (vector-set! new-data (+ (* i n) j) z))))
        (values (list 'matrix m n new-data) means stds)))

;;; unstandardize-coefficients : Vec × Vec × Vec → Vec
;;; Convert coefficients from standardized to original scale.
;;; beta_original[j] = beta_std[j] / std[j]
;;; intercept adjustment: beta_0 = beta_0_std - sum(beta_j * mean_j / std_j)
(define (unstandardize-coefficients beta-std means stds)
  (let* ([p (vector-length beta-std)]
         [beta (make-vector p)])
        ;; First, scale non-intercept coefficients
        (do ([j 1 (+ j 1)])
            [(= j p)]
            (let ([s (vector-ref stds (- j 1))])
                 (vector-set! beta j
                              (if (= s 0)
                                  0
                                  (/ (vector-ref beta-std j) s)))))
        ;; Adjust intercept
        (let ([intercept-adj
               (let loop ([j 1] [sum 0])
                    (if (= j p)
                        sum
                        (loop (+ j 1)
                              (+ sum (* (vector-ref beta j)
                                        (vector-ref means (- j 1)))))))])
             (vector-set! beta 0 (- (vector-ref beta-std 0) intercept-adj)))
        beta))

;;; ============================================================
;;; Dummy Encoding
;;; ============================================================

;;; unique-values : (List Any) → (List Any)
;;; Get sorted unique values from a list.
(define (unique-values xs)
  (let loop ([xs xs] [seen '()])
       (if (null? xs)
           (reverse seen)
           (if (member (car xs) seen)
               (loop (cdr xs) seen)
               (loop (cdr xs) (cons (car xs) seen))))))

;;; dummy-encode : (List Any) → Matrix
;;; Create dummy variable matrix from categorical variable.
;;; Uses (k-1) columns for k categories (reference coding).
;;; First unique value is the reference category.
(define (dummy-encode categories)
  (let* ([n (length categories)]
         [levels (unique-values categories)]
         [k (length levels)]
         [num-cols (- k 1)]
         [level-index (lambda (x)
                              (let loop ([ls levels] [i 0])
                                   (if (null? ls)
                                       -1
                                       (if (equal? x (car ls))
                                           i
                                           (loop (cdr ls) (+ i 1))))))]
         [data (make-vector (* n num-cols) 0)])
        ;; For each observation, set appropriate dummy to 1
        (let loop ([xs categories] [row 0])
             (if (null? xs)
                 (list 'matrix n num-cols data)
                 (let ([idx (level-index (car xs))])
                      ;; Reference category (idx=0) gets all 0s
                      ;; Other categories get 1 in column (idx-1)
                      (when (> idx 0)
                            (vector-set! data (+ (* row num-cols) (- idx 1)) 1))
                      (loop (cdr xs) (+ row 1)))))))

;;; one-hot-encode : (List Any) → Matrix
;;; Create one-hot encoding (k columns for k categories).
(define (one-hot-encode categories)
  (let* ([n (length categories)]
         [levels (unique-values categories)]
         [k (length levels)]
         [level-index (lambda (x)
                              (let loop ([ls levels] [i 0])
                                   (if (null? ls)
                                       -1
                                       (if (equal? x (car ls))
                                           i
                                           (loop (cdr ls) (+ i 1))))))]
         [data (make-vector (* n k) 0)])
        (let loop ([xs categories] [row 0])
             (if (null? xs)
                 (list 'matrix n k data)
                 (let ([idx (level-index (car xs))])
                      (vector-set! data (+ (* row k) idx) 1)
                      (loop (cdr xs) (+ row 1)))))))

;;; ============================================================
;;; Polynomial Features
;;; ============================================================

;;; polynomial-features : Matrix × Nat → Matrix
;;; Add polynomial terms up to given degree.
;;; For matrix with columns [x1, x2], degree 2 produces:
;;; [x1, x2, x1^2, x1*x2, x2^2]
;;; Currently only supports single-column expansion for simplicity.
(define (polynomial-features X degree)
  (let* ([m (matrix-rows X)]
         [n (matrix-cols X)]
         [total-cols (+ n (* n degree))]  ; original + powers
         [new-data (make-vector (* m total-cols))])
        ;; Copy original columns
        (do ([i 0 (+ i 1)])
            [(= i m)]
            (do ([j 0 (+ j 1)])
                [(= j n)]
                (vector-set! new-data (+ (* i total-cols) j)
                             (matrix-ref X i j))))
        ;; Add polynomial terms for each original column
        (do ([i 0 (+ i 1)])
            [(= i m)]
            (do ([j 0 (+ j 1)])
                [(= j n)]
                (let ([x (matrix-ref X i j)])
                     (do ([d 2 (+ d 1)])
                         [(> d degree)]
                         (let ([col (+ n (* j degree) (- d 2))])
                              (vector-set! new-data (+ (* i total-cols) col)
                                           (expt x d)))))))
        (list 'matrix m total-cols new-data)))

;;; ============================================================
;;; Matrix Construction from Data
;;; ============================================================

;;; lists-to-design-matrix : (List (List Num)) × Bool → Matrix
;;; Convert list of rows to design matrix, optionally adding intercept.
(define (lists-to-design-matrix rows add-intercept?)
  (let* ([base (matrix-from-lists rows)])
        (if add-intercept?
            (add-intercept base)
            base)))

;;; cbind : Matrix × Matrix → Matrix
;;; Bind columns of two matrices.
(define (cbind X1 X2)
  (let* ([m (matrix-rows X1)]
         [n1 (matrix-cols X1)]
         [n2 (matrix-cols X2)]
         [n (+ n1 n2)]
         [data (make-vector (* m n))])
        (if (not (= m (matrix-rows X2)))
            (error 'cbind "matrices must have same number of rows")
            (begin
             (do ([i 0 (+ i 1)])
                 [(= i m)]
                 ;; Copy from X1
                 (do ([j 0 (+ j 1)])
                     [(= j n1)]
                     (vector-set! data (+ (* i n) j) (matrix-ref X1 i j)))
                 ;; Copy from X2
                 (do ([j 0 (+ j 1)])
                     [(= j n2)]
                     (vector-set! data (+ (* i n) n1 j) (matrix-ref X2 i j))))
             (list 'matrix m n data)))))

;;; rbind : Matrix × Matrix → Matrix
;;; Bind rows of two matrices.
(define (rbind X1 X2)
  (let* ([m1 (matrix-rows X1)]
         [m2 (matrix-rows X2)]
         [m (+ m1 m2)]
         [n (matrix-cols X1)]
         [data (make-vector (* m n))])
        (if (not (= n (matrix-cols X2)))
            (error 'rbind "matrices must have same number of columns")
            (begin
             ;; Copy X1
             (do ([i 0 (+ i 1)])
                 [(= i m1)]
                 (do ([j 0 (+ j 1)])
                     [(= j n)]
                     (vector-set! data (+ (* i n) j) (matrix-ref X1 i j))))
             ;; Copy X2
             (do ([i 0 (+ i 1)])
                 [(= i m2)]
                 (do ([j 0 (+ j 1)])
                     [(= j n)]
                     (vector-set! data (+ (* (+ m1 i) n) j) (matrix-ref X2 i j))))
             (list 'matrix m n data)))))
