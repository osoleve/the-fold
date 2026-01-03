;;; core/autodiff/test-sparse-autodiff.ss --- Tests for Sparse Automatic Differentiation
;;;
;;; Comprehensive tests for sparse Jacobian, Hessian, and gradient computation.

(load "core/test-framework.ss")
(load "core/autodiff/sparse-autodiff.ss")

;;; Helper for numeric comparison with tolerance
(define (assert-= actual expected tolerance)
  (unless (< (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    x ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (display " +/- ")
          (display tolerance)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; Helper for checking sparse COO entries
(define (sparse-coo-has-entry? coo row col expected-val tolerance)
  (let ([actual (sparse-coo-ref coo row col)])
       (< (abs (- actual expected-val)) tolerance)))

(display "
==============================================================
         SPARSE AUTODIFF TESTS
==============================================================
")

;;; ============================================================
;;; Sparse Gradient Tests
;;; ============================================================

(test-group sparse-gradient-tests
            (define-test sparse-grad-creation
              ;; Create sparse gradient and verify
              (let* ([indices (vector 0 2 5)]
                     [values (vector 1.0 3.0 5.0)]
                     [g (make-sparse-grad indices values)])
                    (assert-equal #t (sparse-grad? g))
                    (assert-equal 3 (sparse-grad-nnz g))
                    (assert-= (sparse-grad-ref g 0) 1.0 0.0001)
                    (assert-= (sparse-grad-ref g 2) 3.0 0.0001)
                    (assert-= (sparse-grad-ref g 5) 5.0 0.0001)
                    (assert-= (sparse-grad-ref g 1) 0.0 0.0001)  ; Not present
                    (assert-= (sparse-grad-ref g 3) 0.0 0.0001)))
            
            (define-test sparse-grad-to-dense
              ;; Convert sparse gradient to dense vector
              (let* ([g (make-sparse-grad (vector 1 3) (vector 2.0 4.0))]
                     [dense (sparse-grad->dense g 5)])
                    (assert-equal 5 (vector-length dense))
                    (assert-= (vector-ref dense 0) 0.0 0.0001)
                    (assert-= (vector-ref dense 1) 2.0 0.0001)
                    (assert-= (vector-ref dense 2) 0.0 0.0001)
                    (assert-= (vector-ref dense 3) 4.0 0.0001)
                    (assert-= (vector-ref dense 4) 0.0 0.0001)))
            
            (define-test dense-to-sparse-grad
              ;; Convert dense vector to sparse gradient
              (let* ([dense (vector 0 2.0 0 0 5.0 0)]
                     [g (dense->sparse-grad dense)])
                    (assert-equal 2 (sparse-grad-nnz g))
                    (assert-= (sparse-grad-ref g 1) 2.0 0.0001)
                    (assert-= (sparse-grad-ref g 4) 5.0 0.0001)))
            
            (define-test dense-to-sparse-with-tolerance
              ;; Convert with tolerance to filter small values
              (let* ([dense (vector 1e-10 2.0 1e-12 0.001 5.0)]
                     [g (dense->sparse-grad dense 0.01)])
                    ;; Only 2.0 and 5.0 should remain (0.001 is below tolerance)
                    (assert-equal 2 (sparse-grad-nnz g))
                    (assert-= (sparse-grad-ref g 1) 2.0 0.0001)
                    (assert-= (sparse-grad-ref g 4) 5.0 0.0001)))
            
            (define-test sparse-grad-add
              ;; Add two sparse gradients
              (let* ([g1 (make-sparse-grad (vector 0 2) (vector 1.0 3.0))]
                     [g2 (make-sparse-grad (vector 1 2) (vector 2.0 4.0))]
                     [sum (sparse-grad-add g1 g2)])
                    (assert-= (sparse-grad-ref sum 0) 1.0 0.0001)
                    (assert-= (sparse-grad-ref sum 1) 2.0 0.0001)
                    (assert-= (sparse-grad-ref sum 2) 7.0 0.0001)))  ; 3+4
            
            (define-test sparse-grad-scale
              ;; Scale sparse gradient
              (let* ([g (make-sparse-grad (vector 1 3) (vector 2.0 4.0))]
                     [scaled (sparse-grad-scale 3 g)])
                    (assert-= (sparse-grad-ref scaled 1) 6.0 0.0001)
                    (assert-= (sparse-grad-ref scaled 3) 12.0 0.0001))))

;;; ============================================================
;;; Sparsity Pattern Tests
;;; ============================================================

(test-group sparsity-pattern-tests
            (define-test diagonal-pattern-creation
              ;; Create diagonal sparsity pattern
              (let ([p (diagonal-pattern 4)])
                   (assert-equal 4 (pattern-rows p))
                   (assert-equal 4 (pattern-cols p))
                   (assert-equal 4 (pattern-nnz p))))
            
            (define-test banded-pattern-creation
              ;; Create banded pattern with bandwidth 1 (tridiagonal)
              (let ([p (banded-pattern 4 1)])
                   (assert-equal 4 (pattern-rows p))
                   (assert-equal 4 (pattern-cols p))
                   ;; 4x4 tridiagonal has: 4 diagonal + 3 above + 3 below = 10
                   (assert-equal 10 (pattern-nnz p))))
            
            (define-test explicit-pattern-creation
              ;; Create pattern from explicit pairs
              (let ([p (pattern-from-explicit 3 4 '((0 . 0) (0 . 2) (1 . 1) (2 . 3)))])
                   (assert-equal 3 (pattern-rows p))
                   (assert-equal 4 (pattern-cols p))
                   (assert-equal 4 (pattern-nnz p))))
            
            (define-test sparsity-detection
              ;; Detect sparsity pattern of a function
              ;; f(x,y,z) = (x^2, y*z) - only (0,0), (1,1), (1,2) non-zero
              (let* ([f (lambda (x y z)
                                (list (traced-sq x)
                                      (traced-mul y z)))]
                     [p (detect-sparsity f '(1 2 3) 0.0001)])
                    (assert-equal 2 (pattern-rows p))
                    (assert-equal 3 (pattern-cols p))
                    ;; Should detect 3 non-zero entries
                    (assert-equal 3 (pattern-nnz p)))))

;;; ============================================================
;;; Sparse Jacobian Tests
;;; ============================================================

(test-group sparse-jacobian-tests
            (define-test sparse-jacobian-identity
              ;; f(x,y) = (x, y) - Jacobian is identity
              (let* ([f (lambda (x y) (list x y))]
                     [J (sparse-jacobian f '(1 2))])
                    (assert-equal 2 (sparse-coo-rows J))
                    (assert-equal 2 (sparse-coo-cols J))
                    ;; Only diagonal entries should be non-zero
                    (assert-true (sparse-coo-has-entry? J 0 0 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 1 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 0 1 0.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 0 0.0 0.0001))))
            
            (define-test sparse-jacobian-linear
              ;; f(x,y) = (2x+y, x-3y)
              ;; J = [[2, 1], [1, -3]]
              (let* ([f (lambda (x y)
                                (list (traced-add (traced-mul x 2) y)
                                      (traced-sub x (traced-mul y 3))))]
                     [J (sparse-jacobian f '(1 1))])
                    (assert-equal 2 (sparse-coo-rows J))
                    (assert-equal 2 (sparse-coo-cols J))
                    ;; All entries non-zero
                    (assert-true (sparse-coo-has-entry? J 0 0 2.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 0 1 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 0 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 1 -3.0 0.0001))))
            
            (define-test sparse-jacobian-truly-sparse
              ;; f(x,y,z) = (x^2, y^2, z^2) - diagonal Jacobian
              ;; J at (1,2,3) = [[2,0,0], [0,4,0], [0,0,6]]
              (let* ([f (lambda (x y z)
                                (list (traced-sq x)
                                      (traced-sq y)
                                      (traced-sq z)))]
                     [J (sparse-jacobian f '(1 2 3))])
                    (assert-equal 3 (sparse-coo-rows J))
                    (assert-equal 3 (sparse-coo-cols J))
                    ;; Should only have 3 non-zero entries (diagonal)
                    (assert-equal 3 (sparse-coo-nnz J))
                    (assert-true (sparse-coo-has-entry? J 0 0 2.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 1 4.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 2 2 6.0 0.0001))))
            
            (define-test sparse-jacobian-with-pattern
              ;; Use known sparsity pattern for efficient computation
              (let* ([f (lambda (x y z)
                                (list (traced-sq x)
                                      (traced-sq y)
                                      (traced-sq z)))]
                     [pattern (diagonal-pattern 3)]
                     [J (sparse-jacobian-with-pattern f '(1 2 3) pattern)])
                    (assert-equal 3 (sparse-coo-nnz J))
                    (assert-true (sparse-coo-has-entry? J 0 0 2.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 1 4.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 2 2 6.0 0.0001))))
            
            (define-test sparse-jacobian-scalar-output
              ;; f(x,y) = x*y (scalar), J = [[y, x]]
              (let* ([f (lambda (x y) (traced-mul x y))]
                     [J (sparse-jacobian f '(3 4))])
                    (assert-equal 1 (sparse-coo-rows J))
                    (assert-equal 2 (sparse-coo-cols J))
                    (assert-true (sparse-coo-has-entry? J 0 0 4.0 0.0001))  ; df/dx = y = 4
                    (assert-true (sparse-coo-has-entry? J 0 1 3.0 0.0001)))) ; df/dy = x = 3
            
            (define-test sparse-jacobian-conversion
              ;; Convert sparse Jacobian to CSR and back
              (let* ([f (lambda (x y) (list (traced-sq x) (traced-sq y)))]
                     [J-coo (sparse-jacobian f '(2 3))]
                     [J-csr (sparse-jacobian->csr J-coo)]
                     [J-dense (sparse-jacobian->dense J-coo)])
                    (assert-true (sparse-csr? J-csr))
                    (assert-= (matrix-ref J-dense 0 0) 4.0 0.0001)
                    (assert-= (matrix-ref J-dense 1 1) 6.0 0.0001)
                    (assert-= (matrix-ref J-dense 0 1) 0.0 0.0001))))

;;; ============================================================
;;; Sparse JVP/VJP Tests
;;; ============================================================

(test-group sparse-jvp-vjp-tests
            (define-test sparse-jvp-computation
              ;; Compute J*v using sparse Jacobian
              (let* ([f (lambda (x y) (list (traced-sq x) (traced-sq y)))]
                     [J (sparse-jacobian f '(2 3))]
                     [v (vector 1 2)]
                     [result (sparse-jvp J v)])
                    ;; J = [[4,0],[0,6]], v = [1,2]
                    ;; J*v = [4*1+0*2, 0*1+6*2] = [4, 12]
                    (assert-= (vector-ref result 0) 4.0 0.0001)
                    (assert-= (vector-ref result 1) 12.0 0.0001)))
            
            (define-test sparse-vjp-computation
              ;; Compute v^T*J using sparse Jacobian
              (let* ([f (lambda (x y) (list (traced-sq x) (traced-sq y)))]
                     [J (sparse-jacobian f '(2 3))]
                     [v (vector 1 2)]
                     [result (sparse-vjp J v)])
                    ;; J = [[4,0],[0,6]], v = [1,2]
                    ;; v^T*J = [1*4+2*0, 1*0+2*6] = [4, 12]
                    (assert-= (vector-ref result 0) 4.0 0.0001)
                    (assert-= (vector-ref result 1) 12.0 0.0001)))
            
            (define-test sparse-jvp-non-diagonal
              ;; JVP for non-diagonal Jacobian
              (let* ([f (lambda (x y)
                                (list (traced-add x y)
                                      (traced-sub x y)))]
                     [J (sparse-jacobian f '(1 1))]
                     [v (vector 3 4)]
                     [result (sparse-jvp J v)])
                    ;; J = [[1,1],[1,-1]], v = [3,4]
                    ;; J*v = [3+4, 3-4] = [7, -1]
                    (assert-= (vector-ref result 0) 7.0 0.0001)
                    (assert-= (vector-ref result 1) -1.0 0.0001))))

;;; ============================================================
;;; Sparse Hessian Tests
;;; ============================================================

(test-group sparse-hessian-tests
            (define-test sparse-hessian-quadratic
              ;; f(x,y) = x^2 + y^2, H = [[2,0],[0,2]]
              (let* ([f (lambda (x y) (traced-add (traced-sq x) (traced-sq y)))]
                     [H (sparse-hessian f '(1 1))])
                    (assert-equal 2 (sparse-coo-rows H))
                    (assert-equal 2 (sparse-coo-cols H))
                    ;; Diagonal entries should be 2
                    (assert-true (sparse-coo-has-entry? H 0 0 2.0 0.1))
                    (assert-true (sparse-coo-has-entry? H 1 1 2.0 0.1))))
            
            (define-test sparse-hessian-cross-term
              ;; f(x,y) = x*y, H = [[0,1],[1,0]]
              (let* ([f (lambda (x y) (traced-mul x y))]
                     [H (sparse-hessian f '(1 1))])
                    ;; Off-diagonal entries should be 1
                    (assert-true (sparse-coo-has-entry? H 0 1 1.0 0.1))
                    (assert-true (sparse-coo-has-entry? H 1 0 1.0 0.1))))
            
            (define-test sparse-hessian-exact-quadratic
              ;; Exact Hessian using hyperdual numbers
              (let* ([f (lambda (x y) (hd-add (hd-sq x) (hd-sq y)))]
                     [H (sparse-hessian-exact f '(1 1))])
                    (assert-equal 2 (sparse-coo-rows H))
                    (assert-equal 2 (sparse-coo-cols H))
                    (assert-true (sparse-coo-has-entry? H 0 0 2.0 0.0001))
                    (assert-true (sparse-coo-has-entry? H 1 1 2.0 0.0001)))))

;;; ============================================================
;;; Sparse Hessian-Vector Product Tests
;;; ============================================================

(test-group sparse-hvp-tests
            (define-test sparse-hvp-quadratic
              ;; H*v where f(x,y) = x^2 + y^2, H = [[2,0],[0,2]]
              (let* ([f (lambda (x y) (traced-add (traced-sq x) (traced-sq y)))]
                     [v (vector 3 4)]
                     [result (sparse-hessian-vector-product f '(1 1) v)])
                    ;; H*v = [[2,0],[0,2]] * [3,4] = [6, 8]
                    (assert-= (vector-ref result 0) 6.0 0.1)
                    (assert-= (vector-ref result 1) 8.0 0.1)))
            
            (define-test sparse-hvp-mixed
              ;; f(x,y) = x*y, H = [[0,1],[1,0]], v = [2,3]
              ;; H*v = [3, 2]
              (let* ([f (lambda (x y) (traced-mul x y))]
                     [v (vector 2 3)]
                     [result (sparse-hessian-vector-product f '(1 1) v)])
                    (assert-= (vector-ref result 0) 3.0 0.1)
                    (assert-= (vector-ref result 1) 2.0 0.1))))

;;; ============================================================
;;; Graph Coloring Tests
;;; ============================================================

(test-group graph-coloring-tests
            (define-test color-diagonal-pattern
              ;; Diagonal pattern: all columns can be same color
              (let* ([p (diagonal-pattern 5)]
                     [colors (color-columns p)]
                     [nc (num-colors colors)])
                    ;; Diagonal columns don't share rows, so 1 color suffices
                    (assert-equal 1 nc)))
            
            (define-test color-dense-pattern
              ;; Dense 3x3 pattern: each column shares rows with others
              ;; Need 3 different colors
              (let* ([p (pattern-from-explicit 3 3
                                               '((0 . 0) (0 . 1) (0 . 2)
                                                 (1 . 0) (1 . 1) (1 . 2)
                                                 (2 . 0) (2 . 1) (2 . 2)))]
                     [colors (color-columns p)]
                     [nc (num-colors colors)])
                    ;; Each column shares rows with all others
                    (assert-equal 3 nc)))
            
            (define-test color-banded-pattern
              ;; Tridiagonal: columns can be 3-colored
              (let* ([p (banded-pattern 6 1)]
                     [colors (color-columns p)]
                     [nc (num-colors colors)])
                    ;; Tridiagonal can use 3 colors
                    (assert-true (<= nc 3)))))

;;; ============================================================
;;; Compressed Jacobian Tests
;;; ============================================================

(test-group compressed-jacobian-tests
            (define-test colored-jacobian-diagonal
              ;; Use coloring to compute diagonal Jacobian efficiently
              (let* ([f (lambda (x y z)
                                (list (dual-sq x)
                                      (dual-sq y)
                                      (dual-sq z)))]
                     [pattern (diagonal-pattern 3)]
                     [J (sparse-jacobian-colored f '(1 2 3) pattern)])
                    (assert-equal 3 (sparse-coo-nnz J))
                    (assert-true (sparse-coo-has-entry? J 0 0 2.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 1 4.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 2 2 6.0 0.0001))))
            
            (define-test colored-jacobian-banded
              ;; Tridiagonal system
              (let* ([;; f(x0, x1, x2) = (x0+x1, x0+x1+x2, x1+x2)
                      f (lambda (x0 x1 x2)
                                (list (dual-add x0 x1)
                                      (dual-add (dual-add x0 x1) x2)
                                      (dual-add x1 x2)))]
                     [pattern (banded-pattern 3 1)]
                     [J (sparse-jacobian-colored f '(1 1 1) pattern)])
                    ;; J = [[1,1,0],[1,1,1],[0,1,1]]
                    (assert-true (sparse-coo-has-entry? J 0 0 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 0 1 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 0 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 1 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 1 2 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 2 1 1.0 0.0001))
                    (assert-true (sparse-coo-has-entry? J 2 2 1.0 0.0001)))))

;;; ============================================================
;;; Integration with Sparse Matrix Types
;;; ============================================================

(test-group integration-tests
            (define-test jacobian-to-csr-multiply
              ;; Compute J in sparse format, convert to CSR, do SpMV
              (let* ([f (lambda (x y) (list (traced-sq x) (traced-sq y)))]
                     [J-coo (sparse-jacobian f '(2 3))]
                     [J-csr (sparse-jacobian->csr J-coo)]
                     [v (vector 1 2)]
                     [result (sparse-csr-vec-mul J-csr v)])
                    ;; J = [[4,0],[0,6]], v = [1,2]
                    ;; J*v = [4, 12]
                    (assert-= (vector-ref result 0) 4.0 0.0001)
                    (assert-= (vector-ref result 1) 12.0 0.0001)))
            
            (define-test jacobian-transpose
              ;; Transpose sparse Jacobian
              (let* ([f (lambda (x y) (list (traced-add x y) (traced-sub x y)))]
                     [J-coo (sparse-jacobian f '(1 1))]
                     [J-csr (sparse-jacobian->csr J-coo)]
                     [Jt (sparse-csr-transpose J-csr)]
                     [Jt-dense (sparse-csr->dense Jt)])
                    ;; J = [[1,1],[1,-1]], J^T = [[1,1],[1,-1]]
                    (assert-= (matrix-ref Jt-dense 0 0) 1.0 0.0001)
                    (assert-= (matrix-ref Jt-dense 0 1) 1.0 0.0001)
                    (assert-= (matrix-ref Jt-dense 1 0) 1.0 0.0001)
                    (assert-= (matrix-ref Jt-dense 1 1) -1.0 0.0001)))
            
            (define-test jacobian-add
              ;; Add two sparse Jacobians
              (let* ([f1 (lambda (x y) (list x (traced-sq y)))]
                     [f2 (lambda (x y) (list (traced-sq x) y))]
                     [J1 (sparse-jacobian f1 '(2 3))]
                     [J2 (sparse-jacobian f2 '(2 3))]
                     [sum (sparse-coo-add J1 J2)]
                     [sum-dense (sparse-coo->dense sum)])
                    ;; J1 = [[1,0],[0,6]], J2 = [[4,0],[0,1]]
                    ;; sum = [[5,0],[0,7]]
                    (assert-= (matrix-ref sum-dense 0 0) 5.0 0.0001)
                    (assert-= (matrix-ref sum-dense 1 1) 7.0 0.0001))))

;;; ============================================================
;;; Large-Scale Efficiency Tests
;;; ============================================================

(test-group efficiency-tests
            (define-test sparse-efficiency-ratio
              ;; Verify sparse is more efficient than dense for diagonal Jacobian
              (let* ([f (lambda args
                                (map traced-sq args))]
                     [n 10]
                     [args (make-list n 1.0)]
                     [J (sparse-jacobian f args)]
                     [density (sparse-density J)])
                    ;; Diagonal matrix: density = n/(n*n) = 1/n
                    (assert-= density (/ 1 n) 0.0001)
                    ;; nnz should be n
                    (assert-equal n (sparse-coo-nnz J))))
            
            (define-test memory-ratio-diagonal
              ;; Verify memory efficiency for diagonal sparse matrix
              (let* ([f (lambda args (map traced-sq args))]
                     [args (make-list 20 1.0)]
                     [J (sparse-jacobian f args)]
                     [ratio (sparse-memory-ratio J)])
                    ;; COO uses 3*nnz = 3*20 = 60, dense uses 20*20 = 400
                    ;; ratio = 60/400 = 0.15
                    (assert-true (< ratio 0.5)))))

;;; ============================================================
;;; Scalability Tests (fold-hls0)
;;; ============================================================

(test-group scalability-tests
            
            (define-test detect-sparsity-large-diagonal
              ;; Large diagonal function f(x) = (x_1^2, x_2^2, ..., x_N^2)
              ;; Sparsity pattern should have exactly N entries (diagonal)
              ;; Tests that detect-sparsity doesn't allocate dense M×N matrix
              (let* ([n 50]
                     [f (lambda args (map traced-sq args))]
                     [args (make-list n 1.0)]
                     [pattern (detect-sparsity f args 1e-8)])
                    ;; Should have exactly N non-zero entries (diagonal)
                    (assert-equal n (pattern-nnz pattern))
                    (assert-equal n (pattern-rows pattern))
                    (assert-equal n (pattern-cols pattern))))
            
            (define-test detect-sparsity-sum
              ;; Function f(x1,...,xn) = x1 + x2 + ... + xn
              ;; Gradient should be all 1s, so all n entries should be non-zero
              (let* ([n 30]
                     ;; Sum all inputs
                     [f (lambda args
                                (fold-left traced-add (car args) (cdr args)))]
                     [args (make-list n 1.0)]
                     [pattern (detect-sparsity f args 1e-8)])
                    ;; Scalar output, so m=1
                    (assert-equal 1 (pattern-rows pattern))
                    (assert-equal n (pattern-cols pattern))
                    ;; All n entries should be non-zero (gradient is all 1s)
                    (assert-equal n (pattern-nnz pattern))))
            
            (define-test sparse-hessian-exact-quadratic-sum
              ;; f(x,y,z) = x*y + y*z
              ;; Hessian has off-diagonal entries at (0,1), (1,0), (1,2), (2,1)
              (let* ([f (lambda (x y z)
                                (hd-add (hd-mul x y) (hd-mul y z)))]
                     [args '(1.0 2.0 3.0)]
                     [H (sparse-hessian-exact f args)])
                    ;; Should have 4 non-zero entries
                    (assert-equal 4 (sparse-coo-nnz H))
                    ;; x*y contributes 1 to H[0,1] and H[1,0]
                    (assert-= (sparse-coo-ref H 0 1) 1.0 0.0001)
                    (assert-= (sparse-coo-ref H 1 0) 1.0 0.0001)
                    ;; y*z contributes 1 to H[1,2] and H[2,1]
                    (assert-= (sparse-coo-ref H 1 2) 1.0 0.0001)
                    (assert-= (sparse-coo-ref H 2 1) 1.0 0.0001)))
            
            (define-test sparse-hessian-exact-sparse-structure
              ;; Verify sparse Hessian only stores non-zeros
              ;; f(x,y,z) = x^2 + y^2 - diagonal Hessian
              (let* ([f (lambda (x y z)
                                (hd-add (hd-mul x x) (hd-mul y y)))]
                     [args '(1.0 2.0 3.0)]
                     [H (sparse-hessian-exact f args)])
                    ;; Only 2 non-zero entries: H[0,0]=2, H[1,1]=2
                    ;; z has no second derivative
                    (assert-equal 2 (sparse-coo-nnz H))
                    (assert-= (sparse-coo-ref H 0 0) 2.0 0.0001)
                    (assert-= (sparse-coo-ref H 1 1) 2.0 0.0001)
                    (assert-= (sparse-coo-ref H 2 2) 0.0 0.0001))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
==============================================================
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All sparse autodiff tests passed.
")
    (display "
[FAILURE] Some sparse autodiff tests failed.
"))
