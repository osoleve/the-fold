;;; core/linalg/test-svd.ss --- Tests for Singular Value Decomposition
;;;
;;; Comprehensive test suite for SVD module.

(load "core/lang/module.ss")
(load "lattice/linalg/svd.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~a (±~a)\n" expected tolerance)
       (printf "      Got:      ~a\n" actual))))

(define (test-true name result)
  (test name #t result))

(define (test-false name result)
  (test name #f result))

(define (is-error? result)
  (and (pair? result) (eq? (car result) 'error)))

(printf "\n=== SVD Tests ===\n\n")

;;; ====
;;; Basic SVD Tests
;;; ====

(printf "--- Basic SVD ---\n")

;; Test: SVD of 2x2 identity matrix
(let* ([a (matrix-identity 2)]
       [result (svd a)])
      (test-false "SVD 2x2 identity succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  (test-approx "Identity: σ₁ = 1" 1.0 (matrix-ref sigma 0 0) 1e-8)
                  (test-approx "Identity: σ₂ = 1" 1.0 (matrix-ref sigma 1 1) 1e-8)
                  (test-true "Identity: reconstruction" (verify-svd a u sigma v)))))

;; Test: SVD of 2x2 diagonal matrix
(let* ([a (matrix-from-lists '((3 0) (0 2)))]
       [result (svd a)])
      (test-false "SVD 2x2 diagonal succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([sigma (cadr result)])
                  ;; Singular values should be 3, 2 (descending order)
                  (test-approx "Diagonal: σ₁ = 3" 3.0 (matrix-ref sigma 0 0) 1e-8)
                  (test-approx "Diagonal: σ₂ = 2" 2.0 (matrix-ref sigma 1 1) 1e-8))))

;; Test: SVD of 2x2 symmetric matrix
(let* ([a (matrix-from-lists '((4 2) (2 4)))]
       [result (svd a)])
      (test-false "SVD 2x2 symmetric succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  ;; Eigenvalues of A are 6 and 2, so singular values are 6, 2
                  (test-approx "Symmetric: σ₁ = 6" 6.0 (matrix-ref sigma 0 0) 1e-6)
                  (test-approx "Symmetric: σ₂ = 2" 2.0 (matrix-ref sigma 1 1) 1e-6)
                  (test-true "Symmetric: reconstruction" (verify-svd a u sigma v)))))

;; Test: SVD of 3x3 matrix
(let* ([a (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))]
       [result (svd a)])
      (test-false "SVD 3x3 succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  (test-true "3x3: reconstruction" (verify-svd a u sigma v 1e-6)))))

;;; ====
;;; Rectangular Matrix Tests
;;; ====

(printf "\n--- Rectangular Matrices ---\n")

;; Test: SVD of 3x2 tall matrix
(let* ([a (matrix-from-lists '((1 2) (3 4) (5 6)))]
       [result (svd a)])
      (test-false "SVD 3x2 tall succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  (test "3x2: U rows" 3 (matrix-rows u))
                  (test "3x2: U cols" 3 (matrix-cols u))
                  (test "3x2: Σ rows" 3 (matrix-rows sigma))
                  (test "3x2: Σ cols" 2 (matrix-cols sigma))
                  (test "3x2: V rows" 2 (matrix-rows v))
                  (test "3x2: V cols" 2 (matrix-cols v))
                  (test-true "3x2: reconstruction" (verify-svd a u sigma v 1e-6)))))

;; Test: SVD of 2x3 wide matrix
(let* ([a (matrix-from-lists '((1 2 3) (4 5 6)))]
       [result (svd a)])
      (test-false "SVD 2x3 wide succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  (test "2x3: U rows" 2 (matrix-rows u))
                  (test "2x3: U cols" 2 (matrix-cols u))
                  (test "2x3: Σ rows" 2 (matrix-rows sigma))
                  (test "2x3: Σ cols" 3 (matrix-cols sigma))
                  (test "2x3: V rows" 3 (matrix-rows v))
                  (test "2x3: V cols" 3 (matrix-cols v))
                  (test-true "2x3: reconstruction" (verify-svd a u sigma v 1e-6)))))

;; Test: SVD of 4x2 matrix
(let* ([a (matrix-from-lists '((1 0) (0 1) (1 1) (2 1)))]
       [result (svd a)])
      (test-false "SVD 4x2 succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  (test-true "4x2: reconstruction" (verify-svd a u sigma v 1e-6)))))

;;; ====
;;; Thin SVD Tests
;;; ====

(printf "\n--- Thin SVD ---\n")

;; Test: Thin SVD of tall matrix
(let* ([a (matrix-from-lists '((1 2) (3 4) (5 6)))]
       [result (svd-thin a)])
      (test-false "Thin SVD 3x2 succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  ;; For 3x2: U is 3x2, Σ is 2x2, V is 2x2
                  (test "Thin 3x2: U rows" 3 (matrix-rows u))
                  (test "Thin 3x2: U cols" 2 (matrix-cols u))
                  (test "Thin 3x2: Σ rows" 2 (matrix-rows sigma))
                  (test "Thin 3x2: Σ cols" 2 (matrix-cols sigma)))))

;; Test: Thin SVD of wide matrix
(let* ([a (matrix-from-lists '((1 2 3) (4 5 6)))]
       [result (svd-thin a)])
      (test-false "Thin SVD 2x3 succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  ;; For 2x3: U is 2x2, Σ is 2x2, V is 3x2
                  (test "Thin 2x3: U rows" 2 (matrix-rows u))
                  (test "Thin 2x3: U cols" 2 (matrix-cols u))
                  (test "Thin 2x3: Σ rows" 2 (matrix-rows sigma))
                  (test "Thin 2x3: Σ cols" 2 (matrix-cols sigma))
                  (test "Thin 2x3: V rows" 3 (matrix-rows v))
                  (test "Thin 2x3: V cols" 2 (matrix-cols v)))))

;;; ====
;;; Singular Values Tests
;;; ====

(printf "\n--- Singular Values ---\n")

;; Test: Singular values of diagonal matrix
(let* ([a (matrix-from-lists '((5 0 0) (0 3 0) (0 0 1)))]
       [sv (singular-values a)])
      (test-false "Singular values diagonal succeeds" (is-error? sv))
      (when (not (is-error? sv))
            ;; Should be sorted descending: 5, 3, 1
            (test-approx "Diagonal SV: σ₁ = 5" 5.0 (vector-ref sv 0) 1e-8)
            (test-approx "Diagonal SV: σ₂ = 3" 3.0 (vector-ref sv 1) 1e-8)
            (test-approx "Diagonal SV: σ₃ = 1" 1.0 (vector-ref sv 2) 1e-8)))

;; Test: Singular values preserve Frobenius norm
(let* ([a (matrix-from-lists '((1 2) (3 4)))]
       [sv (singular-values a)]
       [frob-a (matrix-frobenius-norm a)]
       [sv-sum (let loop ([i 0] [sum 0])
                    (if (= i (vector-length sv))
                        sum
                        (loop (+ i 1) (+ sum (expt (vector-ref sv i) 2)))))])
      (test-approx "Frobenius norm preserved" (expt frob-a 2) sv-sum 1e-8))

;;; ====
;;; Pseudoinverse Tests
;;; ====

(printf "\n--- Pseudoinverse ---\n")

;; Test: Pseudoinverse of invertible matrix equals inverse
(let* ([a (matrix-from-lists '((4 7) (2 6)))]
       [pinv (pseudoinverse a)])
      (test-false "Pseudoinverse 2x2 succeeds" (is-error? pinv))
      (when (not (is-error? pinv))
            (let ([prod (matrix-mul a pinv)])
                 ;; A × A^+ should be identity for invertible A
                 (test-approx "Pinv invertible: I[0,0]" 1.0 (matrix-ref prod 0 0) 1e-6)
                 (test-approx "Pinv invertible: I[1,1]" 1.0 (matrix-ref prod 1 1) 1e-6)
                 (test-approx "Pinv invertible: I[0,1]" 0.0 (matrix-ref prod 0 1) 1e-6)
                 (test-approx "Pinv invertible: I[1,0]" 0.0 (matrix-ref prod 1 0) 1e-6))))

;; Test: Pseudoinverse property: A A+ A = A
(let* ([a (matrix-from-lists '((1 2) (3 4) (5 6)))]
       [pinv (pseudoinverse a)])
      (test-false "Pseudoinverse 3x2 succeeds" (is-error? pinv))
      (when (not (is-error? pinv))
            (let* ([a-pinv (matrix-mul a pinv)]
                   [result (matrix-mul a-pinv a)]
                   [diff (matrix-sub result a)]
                   [err (matrix-frobenius-norm diff)])
                  (test-true "AA+A = A" (< err 1e-6)))))

;; Test: Pseudoinverse property: A+ A A+ = A+
(let* ([a (matrix-from-lists '((1 2) (3 4)))]
       [pinv (pseudoinverse a)])
      (test-false "Pseudoinverse property succeeds" (is-error? pinv))
      (when (not (is-error? pinv))
            (let* ([pinv-a (matrix-mul pinv a)]
                   [result (matrix-mul pinv-a pinv)]
                   [diff (matrix-sub result pinv)]
                   [err (matrix-frobenius-norm diff)])
                  (test-true "A+AA+ = A+" (< err 1e-6)))))

;;; ====
;;; Low-Rank Approximation Tests
;;; ====

(printf "\n--- Low-Rank Approximation ---\n")

;; Test: Rank-1 approximation of rank-1 matrix
(let* ([a (matrix-from-lists '((2 4) (1 2) (3 6)))]  ; rank 1
       [approx (low-rank-approx a 1)])
      (test-false "Low-rank approx rank-1 succeeds" (is-error? approx))
      (when (not (is-error? approx))
            (let* ([diff (matrix-sub approx a)]
                   [err (matrix-frobenius-norm diff)])
                  (test-true "Rank-1 approx of rank-1 exact" (< err 1e-6)))))

;; Test: Rank-1 approximation reduces error to σ₂
(let* ([a (matrix-from-lists '((1 2) (3 4)))]
       [approx1 (low-rank-approx a 1)])
      (test-false "Low-rank approx 2x2 succeeds" (is-error? approx1))
      (when (not (is-error? approx1))
            (let* ([sv (singular-values a)]
                   [sigma2 (vector-ref sv 1)]
                   [diff (matrix-sub a approx1)]
                   [err (matrix-frobenius-norm diff)])
                  (test-approx "Error = σ₂" sigma2 err 1e-6))))

;; Test: Invalid rank returns error
(let ([result (low-rank-approx (matrix-identity 3) -1)])
     (test-true "Negative rank errors" (is-error? result)))
(let ([result (low-rank-approx (matrix-identity 3) 5)])
     (test-true "Rank > min(m,n) errors" (is-error? result)))

;;; ====
;;; Matrix Rank Tests
;;; ====

(printf "\n--- Matrix Rank ---\n")

;; Test: Rank of identity matrix
(let ([r (matrix-rank (matrix-identity 3))])
     (test "Identity rank" 3 r))

;; Test: Rank of zero matrix
(let ([r (matrix-rank (make-matrix 3 3 0))])
     (test "Zero matrix rank" 0 r))

;; Test: Rank of rank-1 matrix
(let* ([a (matrix-from-lists '((1 2 3) (2 4 6) (3 6 9)))]
       [r (matrix-rank a)])
      (test "Rank-1 matrix rank" 1 r))

;; Test: Rank of rank-2 matrix
(let* ([a (matrix-from-lists '((1 0 1) (0 1 1) (1 1 2)))]
       [r (matrix-rank a)])
      (test "Rank-2 matrix rank" 2 r))

;; Test: Rank of full-rank rectangular matrix
(let* ([a (matrix-from-lists '((1 2) (3 4) (5 6)))]
       [r (matrix-rank a)])
      (test "3x2 full-rank matrix" 2 r))

;;; ====
;;; Orthogonality Tests
;;; ====

(printf "\n--- Orthogonality Properties ---\n")

;; Test: U is orthogonal
(let* ([a (matrix-from-lists '((1 2 3) (4 5 6) (7 8 10)))]
       [result (svd a)]
       [u (car result)]
       [ut (matrix-transpose u)]
       [utu (matrix-mul ut u)]
       [i (matrix-identity 3)]
       [diff (matrix-sub utu i)]
       [err (matrix-frobenius-norm diff)])
      (test-true "U is orthogonal (U^TU = I)" (< err 1e-6)))

;; Test: V is orthogonal
(let* ([a (matrix-from-lists '((1 2 3) (4 5 6) (7 8 10)))]
       [result (svd a)]
       [v (caddr result)]
       [vt (matrix-transpose v)]
       [vtv (matrix-mul vt v)]
       [i (matrix-identity 3)]
       [diff (matrix-sub vtv i)]
       [err (matrix-frobenius-norm diff)])
      (test-true "V is orthogonal (V^TV = I)" (< err 1e-6)))

;;; ====
;;; Edge Cases
;;; ====

(printf "\n--- Edge Cases ---\n")

;; Test: SVD of 1x1 matrix
(let* ([a (matrix-from-lists '((5)))]
       [result (svd a)])
      (test-false "SVD 1x1 succeeds" (is-error? result))
      (when (not (is-error? result))
            (let ([sigma (cadr result)])
                 (test-approx "1x1: σ = 5" 5.0 (matrix-ref sigma 0 0) 1e-8))))

;; Test: SVD of matrix with zero singular value
(let* ([a (matrix-from-lists '((1 0) (0 0)))]
       [result (svd a)])
      (test-false "SVD with zero SV succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  (test-approx "Zero SV: σ₁ = 1" 1.0 (matrix-ref sigma 0 0) 1e-8)
                  (test-approx "Zero SV: σ₂ = 0" 0.0 (matrix-ref sigma 1 1) 1e-8)
                  (test-true "Zero SV: reconstruction" (verify-svd a u sigma v)))))

;;; ====
;;; Numerical Stability Tests
;;; ====

(printf "\n--- Numerical Stability ---\n")

;; Test: SVD of nearly singular matrix
(let* ([a (matrix-from-lists '((1 1.000001) (1 1)))]
       [result (svd a)])
      (test-false "Nearly singular succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([u (car result)]
                   [sigma (cadr result)]
                   [v (caddr result)])
                  (test-true "Nearly singular: reconstruction" (verify-svd a u sigma v 1e-4)))))

;; Test: SVD with large condition number
(let* ([a (matrix-from-lists '((1000 0) (0 0.001)))]
       [result (svd a)])
      (test-false "Large condition succeeds" (is-error? result))
      (when (not (is-error? result))
            (let* ([sigma (cadr result)])
                  (test-approx "Large cond: σ₁ = 1000" 1000.0 (matrix-ref sigma 0 0) 1e-6)
                  (test-approx "Large cond: σ₂ = 0.001" 0.001 (matrix-ref sigma 1 1) 1e-9))))

;;; ====
;;; Summary
;;; ====

(printf "\n=== SVD Test Summary ===\n")
(printf "Passed: ~a\n" tests-passed)
(printf "Failed: ~a\n" tests-failed)
(printf "Total:  ~a\n" (+ tests-passed tests-failed))

(when (> tests-failed 0)
      (exit 1))
