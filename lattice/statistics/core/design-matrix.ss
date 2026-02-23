(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module design-matrix
;;; @requires prelude matrix summary-stats iteration
(require 'prelude)
(require 'matrix)
(require 'summary-stats)
(require 'iteration)

(doc 'module 'design-matrix)
(doc 'description "Design Matrix Construction — Utilities for constructing design matrices for regression")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'description "Intercept addition")
(doc 'description "Column standardization (z-score)")
(doc 'description "Dummy encoding for categorical variables")
(doc 'description "Polynomial feature expansion")

(doc 'section 'intercept-handling)
(define (add-intercept X)
  (doc 'export #t)
  (doc 'type '(-> Matrix Matrix))
  (doc 'description "Prepend a column of 1s to the matrix")
  (let* ([m (matrix-rows X)]
         [n (matrix-cols X)]
         [w (+ n 1)]
         [new-data (vec-tabulate (* m w) k
                     (let ([i (quotient k w)]
                           [j (remainder k w)])
                       (if (= j 0) 1 (matrix-ref X i (- j 1)))))])
        (list 'matrix m w new-data)))

(define (has-intercept? X)
  (doc 'type '(-> Matrix Bool))
  (doc 'description "Check if first column is all 1s (likely an intercept)")
  (let ([m (matrix-rows X)])
       (let loop ([i 0])
            (if (= i m)
                #t
                (if (= (matrix-ref X i 0) 1)
                    (loop (+ i 1))
                    #f)))))

(doc 'section 'standardization)

(define (column-mean X j)
  (doc 'type '(-> Matrix Nat Num))
  (doc 'description "Compute mean of a column")
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
  (doc 'export #t)
  (let* ([m (matrix-rows X)]
         [n (matrix-cols X)]
         [means (vec-tabulate n j (column-mean X j))]
         [stds  (vec-tabulate n j (column-std X j))]
         [new-data (vec-tabulate (* m n) k
                     (let* ([i (quotient k n)]
                            [j (remainder k n)]
                            [mu (vector-ref means j)]
                            [s (vector-ref stds j)]
                            [x (matrix-ref X i j)])
                       (if (= s 0) 0 (/ (- x mu) s))))])
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

;;; ====
;;; Dummy Encoding
;;; ====

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
  (doc 'export #t)
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

;;; ====
;;; Polynomial Features
;;; ====

;;; polynomial-features : Matrix × Nat → Matrix
;;; Add polynomial terms up to given degree.
;;; For matrix with columns [x1, x2], degree 2 produces:
;;; [x1, x2, x1^2, x1*x2, x2^2]
;;; Currently only supports single-column expansion for simplicity.
(define (polynomial-features X degree)
  (doc 'export #t)
  (let* ([m (matrix-rows X)]
         [n (matrix-cols X)]
         [total-cols (+ n (* n degree))]  ; original + powers
         [new-data (vec-tabulate (* m total-cols) k
                     (let ([i (quotient k total-cols)]
                           [j (remainder k total-cols)])
                       (if (< j n)
                           ;; Original column
                           (matrix-ref X i j)
                           ;; Polynomial term: which original col and what degree?
                           (let* ([off (- j n)]
                                  [src-col (quotient off degree)]
                                  [d (+ (remainder off degree) 2)])
                             (expt (matrix-ref X i src-col) d)))))])
        (list 'matrix m total-cols new-data)))

;;; ====
;;; Matrix Construction from Data
;;; ====

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
         [n (+ n1 n2)])
        (if (not (= m (matrix-rows X2)))
            (error 'cbind "matrices must have same number of rows")
            (let ([data (vec-tabulate (* m n) k
                          (let ([i (quotient k n)]
                                [j (remainder k n)])
                            (if (< j n1)
                                (matrix-ref X1 i j)
                                (matrix-ref X2 i (- j n1)))))])
              (list 'matrix m n data)))))

;;; rbind : Matrix × Matrix → Matrix
;;; Bind rows of two matrices.
(define (rbind X1 X2)
  (let* ([m1 (matrix-rows X1)]
         [m2 (matrix-rows X2)]
         [m (+ m1 m2)]
         [n (matrix-cols X1)])
        (if (not (= n (matrix-cols X2)))
            (error 'rbind "matrices must have same number of columns")
            (let ([data (vec-tabulate (* m n) k
                          (let ([i (quotient k n)]
                                [j (remainder k n)])
                            (if (< i m1)
                                (matrix-ref X1 i j)
                                (matrix-ref X2 (- i m1) j))))])
              (list 'matrix m n data)))))

;;; ====
;;; Orthogonal Polynomial Bases
;;; ====
;;;
;;; Orthogonal polynomials provide better numerical stability than
;;; standard polynomial bases (1, x, x², ...) because they avoid
;;; the ill-conditioning of Vandermonde matrices.
;;;
;;; Each family is orthogonal with respect to a specific weight function:
;;;   - Legendre: w(x) = 1 on [-1, 1]
;;;   - Chebyshev: w(x) = 1/sqrt(1-x²) on [-1, 1]
;;;   - Hermite: w(x) = e^(-x²) on (-∞, ∞)
;;;   - Laguerre: w(x) = e^(-x) on [0, ∞)

;;; legendre-p : Nat × Num → Num
;;; Evaluate Legendre polynomial P_n(x) using recurrence relation.
;;; P_0(x) = 1, P_1(x) = x
;;; P_n(x) = ((2n-1)·x·P_{n-1}(x) - (n-1)·P_{n-2}(x)) / n
(define (legendre-p n x)
  (doc 'export #t)
  (cond
   [(= n 0) 1]
   [(= n 1) x]
   [else
    (let loop ([k 2] [p-2 1] [p-1 x])
      (if (> k n)
          p-1
          (let ([p-k (/ (- (* (- (* 2 k) 1) x p-1)
                           (* (- k 1) p-2))
                        k)])
            (loop (+ k 1) p-1 p-k))))]))

;;; chebyshev-t : Nat × Num → Num
;;; Evaluate Chebyshev polynomial T_n(x) of first kind.
;;; T_0(x) = 1, T_1(x) = x
;;; T_n(x) = 2x·T_{n-1}(x) - T_{n-2}(x)
(define (chebyshev-t n x)
  (doc 'export #t)
  (cond
   [(= n 0) 1]
   [(= n 1) x]
   [else
    (let loop ([k 2] [t-2 1] [t-1 x])
      (if (> k n)
          t-1
          (let ([t-k (- (* 2 x t-1) t-2)])
            (loop (+ k 1) t-1 t-k))))]))

;;; hermite-h : Nat × Num → Num
;;; Evaluate (probabilist's) Hermite polynomial He_n(x).
;;; He_0(x) = 1, He_1(x) = x
;;; He_n(x) = x·He_{n-1}(x) - (n-1)·He_{n-2}(x)
;;; Note: Uses probabilist's convention, not physicist's.
(define (hermite-h n x)
  (doc 'export #t)
  (cond
   [(= n 0) 1]
   [(= n 1) x]
   [else
    (let loop ([k 2] [h-2 1] [h-1 x])
      (if (> k n)
          h-1
          (let ([h-k (- (* x h-1) (* (- k 1) h-2))])
            (loop (+ k 1) h-1 h-k))))]))

;;; laguerre-l : Nat × Num → Num
;;; Evaluate Laguerre polynomial L_n(x).
;;; L_0(x) = 1, L_1(x) = 1 - x
;;; L_n(x) = ((2n-1-x)·L_{n-1}(x) - (n-1)·L_{n-2}(x)) / n
(define (laguerre-l n x)
  (doc 'export #t)
  (cond
   [(= n 0) 1]
   [(= n 1) (- 1 x)]
   [else
    (let loop ([k 2] [l-2 1] [l-1 (- 1 x)])
      (if (> k n)
          l-1
          (let ([l-k (/ (- (* (- (- (* 2 k) 1) x) l-1)
                           (* (- k 1) l-2))
                        k)])
            (loop (+ k 1) l-1 l-k))))]))

;;; ====
;;; Orthogonal Polynomial Design Matrices
;;; ====

;;; legendre-features : (Vector Num) × Nat → Matrix
;;; Create design matrix with Legendre polynomial basis.
;;; Maps input to [-1, 1] before evaluation for numerical stability.
;;; Columns: [P_0(x), P_1(x), ..., P_degree(x)]
(define (legendre-features xs degree)
  (doc 'export #t)
  (let* ([n (vector-length xs)]
         [ncols (+ degree 1)]
         ;; Map to [-1, 1]
         [x-min (vec-min xs)]
         [x-max (vec-max xs)]
         [x-range (- x-max x-min)]
         [scale (lambda (x)
                  (if (= x-range 0)
                      0
                      (- (* 2 (/ (- x x-min) x-range)) 1)))]
         [data (vec-tabulate (* n ncols) k
                 (let ([i (quotient k ncols)]
                       [d (remainder k ncols)])
                   (legendre-p d (scale (vector-ref xs i)))))])
    (list 'matrix n ncols data)))

;;; chebyshev-features : (Vector Num) × Nat → Matrix
;;; Create design matrix with Chebyshev polynomial basis.
;;; Maps input to [-1, 1] before evaluation.
;;; Columns: [T_0(x), T_1(x), ..., T_degree(x)]
(define (chebyshev-features xs degree)
  (doc 'export #t)
  (let* ([n (vector-length xs)]
         [ncols (+ degree 1)]
         [x-min (vec-min xs)]
         [x-max (vec-max xs)]
         [x-range (- x-max x-min)]
         [scale (lambda (x)
                  (if (= x-range 0)
                      0
                      (- (* 2 (/ (- x x-min) x-range)) 1)))]
         [data (vec-tabulate (* n ncols) k
                 (let ([i (quotient k ncols)]
                       [d (remainder k ncols)])
                   (chebyshev-t d (scale (vector-ref xs i)))))])
    (list 'matrix n ncols data)))

;;; hermite-features : (Vector Num) × Nat → Matrix
;;; Create design matrix with Hermite polynomial basis.
;;; Standardizes input (z-score) before evaluation.
;;; Columns: [He_0(x), He_1(x), ..., He_degree(x)]
(define (hermite-features xs degree)
  (doc 'export #t)
  (let* ([n (vector-length xs)]
         [ncols (+ degree 1)]
         ;; Standardize for Hermite (Gaussian weighting)
         [mu (vec-mean xs)]
         [sigma (vec-std xs)]
         [scale (lambda (x)
                  (if (= sigma 0)
                      0
                      (/ (- x mu) sigma)))]
         [data (vec-tabulate (* n ncols) k
                 (let ([i (quotient k ncols)]
                       [d (remainder k ncols)])
                   (hermite-h d (scale (vector-ref xs i)))))])
    (list 'matrix n ncols data)))

;;; laguerre-features : (Vector Num) × Nat → Matrix
;;; Create design matrix with Laguerre polynomial basis.
;;; Shifts input so minimum is 0 (for [0, ∞) domain).
;;; Columns: [L_0(x), L_1(x), ..., L_degree(x)]
(define (laguerre-features xs degree)
  (doc 'export #t)
  (let* ([n (vector-length xs)]
         [ncols (+ degree 1)]
         ;; Shift to [0, ∞)
         [x-min (vec-min xs)]
         [scale (lambda (x) (- x x-min))]
         [data (vec-tabulate (* n ncols) k
                 (let ([i (quotient k ncols)]
                       [d (remainder k ncols)])
                   (laguerre-l d (scale (vector-ref xs i)))))])
    (list 'matrix n ncols data)))

;;; orthogonal-features : (Vector Num) × Nat × Symbol → Matrix
;;; Create design matrix with specified orthogonal polynomial basis.
;;; basis: 'legendre, 'chebyshev, 'hermite, or 'laguerre
(define (orthogonal-features xs degree basis)
  (doc 'export #t)
  (case basis
    [(legendre) (legendre-features xs degree)]
    [(chebyshev) (chebyshev-features xs degree)]
    [(hermite) (hermite-features xs degree)]
    [(laguerre) (laguerre-features xs degree)]
    [else (error 'orthogonal-features
                 "unknown basis (use legendre, chebyshev, hermite, or laguerre)"
                 basis)]))

;;; vec-min : Vector → Num
;;; Errors on empty vectors.
(define (vec-min v)
  (let ([n (vector-length v)])
    (if (= n 0)
        (error 'vec-min "empty vector")
        (let loop ([i 1] [m (vector-ref v 0)])
          (if (= i n)
              m
              (loop (+ i 1) (min m (vector-ref v i))))))))

;;; vec-max : Vector → Num
;;; Errors on empty vectors.
(define (vec-max v)
  (let ([n (vector-length v)])
    (if (= n 0)
        (error 'vec-max "empty vector")
        (let loop ([i 1] [m (vector-ref v 0)])
          (if (= i n)
              m
              (loop (+ i 1) (max m (vector-ref v i))))))))

;;; vec-std : Vector → Num
(define (vec-std v)
  (let* ([n (vector-length v)]
         [mu (vec-mean v)])
    (if (<= n 1)
        0
        (let ([ss (let loop ([i 0] [s 0])
                    (if (= i n)
                        s
                        (loop (+ i 1)
                              (+ s (expt (- (vector-ref v i) mu) 2)))))])
          (sqrt (/ ss (- n 1)))))))
