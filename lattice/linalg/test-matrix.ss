;;; fabric/stitches/test-matrix.ss — Tests for Matrix Operations

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")

;;; ====
;;; Test Framework
;;; ====

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test-equiv? a b)
  (cond
   [(and (matrix? a) (matrix? b)) (matrix-equal? a b)]
   [(and (vector? a) (vector? b)) (vec-equal? a b)]
   [else (equal? a b)]))

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (test-equiv? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display name)
       (display ": ok")
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "  FAIL ")
       (display name)
       (newline)
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-error name actual)
  (test name #t (and (pair? actual) (eq? (car actual) 'error))))

(define (run-tests)
  (display "
=== Matrix Operations Test Suite ===

")
  
  ;; Construction
  (display "--- Matrix Construction ---
")
  
  (test "make-matrix"
        (matrix-from-lists '((0 0 0) (0 0 0)))
        (make-matrix 2 3 0))
  
  (test "matrix-from-lists 2x3"
        '((1 2 3) (4 5 6))
        (matrix->lists (matrix-from-lists '((1 2 3) (4 5 6)))))
  
  (test "matrix-rows"
        2
        (matrix-rows (matrix-from-lists '((1 2 3) (4 5 6)))))
  
  (test "matrix-cols"
        3
        (matrix-cols (matrix-from-lists '((1 2 3) (4 5 6)))))
  
  (test "matrix-shape"
        '(2 . 3)
        (matrix-shape (matrix-from-lists '((1 2 3) (4 5 6)))))
  
  (test-true "matrix?"
             (matrix? (matrix-from-lists '((1 2) (3 4)))))
  
  (test-false "matrix? on list"
              (matrix? '((1 2) (3 4))))
  
  (test-error "matrix-from-lists ragged - row too short"
              (matrix-from-lists '((1 2 3) (4 5))))
  
  (test-error "matrix-from-lists ragged - row too long"
              (matrix-from-lists '((1 2) (3 4 5))))
  
  (test-error "matrix-from-lists ragged - multiple inconsistent rows"
              (matrix-from-lists '((1 2 3) (4 5) (6 7 8 9))))
  
  (test "matrix-from-lists single row"
        '((1 2 3))
        (matrix->lists (matrix-from-lists '((1 2 3)))))
  
  (test "matrix-from-lists single column"
        '((1) (2) (3))
        (matrix->lists (matrix-from-lists '((1) (2) (3)))))
  
  ;; Accessors
  (display "
--- Matrix Accessors ---
")
  
  (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
       (test "matrix-ref (0,0)"
             1
             (matrix-ref m 0 0))
       
       (test "matrix-ref (1,2)"
             6
             (matrix-ref m 1 2))
       
       (test-error "matrix-ref out of bounds"
                   (matrix-ref m 5 0))
       
       (test "matrix-row 0"
             (vector 1 2 3)
             (matrix-row m 0))
       
       (test "matrix-row 1"
             (vector 4 5 6)
             (matrix-row m 1))
       
       (test "matrix-col 0"
             (vector 1 4)
             (matrix-col m 0))
       
       (test "matrix-col 2"
             (vector 3 6)
             (matrix-col m 2)))
  
  (let ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))])
       (test "matrix-diagonal"
             (vector 1 5 9)
             (matrix-diagonal m)))
  
  ;; Transformations
  (display "
--- Matrix Transformations ---
")
  
  (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
       (test "matrix-transpose"
             '((1 4) (2 5) (3 6))
             (matrix->lists (matrix-transpose m))))
  
  (test "matrix-map"
        '((2 4) (6 8))
        (matrix->lists
         (matrix-map (lambda (x) (* x 2))
                     (matrix-from-lists '((1 2) (3 4))))))
  
  (test "matrix-fold sum"
        10
        (matrix-fold + 0 (matrix-from-lists '((1 2) (3 4)))))
  
  ;; Arithmetic
  (display "
--- Matrix Arithmetic ---
")
  
  (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
        [m2 (matrix-from-lists '((5 6) (7 8)))])
       
       (test "matrix-add"
             '((6 8) (10 12))
             (matrix->lists (matrix-add m1 m2)))
       
       (test "matrix-sub"
             '((-4 -4) (-4 -4))
             (matrix->lists (matrix-sub m1 m2)))
       
       (test "matrix-hadamard"
             '((5 12) (21 32))
             (matrix->lists (matrix-hadamard m1 m2)))
       
       (test "matrix-scale"
             '((2 4) (6 8))
             (matrix->lists (matrix-scale 2 m1)))
       
       (test "matrix-negate"
             '((-1 -2) (-3 -4))
             (matrix->lists (matrix-negate m1))))
  
  (test-error "matrix-add dimension mismatch"
              (matrix-add (matrix-from-lists '((1 2)))
                          (matrix-from-lists '((1) (2)))))
  
  ;; Matrix Multiplication
  (display "
--- Matrix Multiplication ---
")
  
  (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
        [m2 (matrix-from-lists '((5 6) (7 8)))])
       (test "matrix-mul 2x2 * 2x2"
             '((19 22) (43 50))
             (matrix->lists (matrix-mul m1 m2))))
  
  (let ([m1 (matrix-from-lists '((1 2 3) (4 5 6)))]   ; 2x3
        [m2 (matrix-from-lists '((1 2) (3 4) (5 6)))]) ; 3x2
       (test "matrix-mul 2x3 * 3x2 = 2x2"
             '((22 28) (49 64))
             (matrix->lists (matrix-mul m1 m2))))
  
  (test-error "matrix-mul dimension mismatch"
              (matrix-mul (matrix-from-lists '((1 2 3)))
                          (matrix-from-lists '((1 2)))))
  
  ;; Matrix-Vector Multiplication
  (display "
--- Matrix-Vector Multiplication ---
")
  
  (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))]
        [v (vector 1 2 3)])
       (test "matrix-vec-mul"
             (vector 14 32)
             (matrix-vec-mul m v)))
  
  (let ([m (matrix-from-lists '((1 2) (3 4)))]  ; 2x2
        [v (vector 1 2)])                        ; length 2
       (test "vec-matrix-mul"
             (vector 7 10)  ; [1,2] * [[1,2],[3,4]] = [1*1+2*3, 1*2+2*4] = [7, 10]
             (vec-matrix-mul v m)))
  
  (test-error "matrix-vec-mul dimension mismatch"
              (matrix-vec-mul (matrix-from-lists '((1 2 3)))
                              (vector 1 2)))
  
  ;; Comparisons
  (display "
--- Matrix Comparisons ---
")
  
  (test-true "matrix-equal? same"
             (matrix-equal? (matrix-from-lists '((1 2) (3 4)))
                            (matrix-from-lists '((1 2) (3 4)))))
  
  (test-false "matrix-equal? different"
              (matrix-equal? (matrix-from-lists '((1 2) (3 4)))
                             (matrix-from-lists '((1 2) (3 5)))))
  
  (test-false "matrix-equal? different size"
              (matrix-equal? (matrix-from-lists '((1 2) (3 4)))
                             (matrix-from-lists '((1 2 3)))))
  
  (test-true "matrix-approx-equal?"
             (matrix-approx-equal?
              (matrix-from-lists '((1.0 2.0) (3.0 4.0)))
              (matrix-from-lists '((1.00000000001 2.0) (3.0 4.0)))))
  
  ;; Properties
  (display "
--- Matrix Properties ---
")
  
  (test-true "matrix-square? true"
             (matrix-square? (matrix-from-lists '((1 2) (3 4)))))
  
  (test-false "matrix-square? false"
              (matrix-square? (matrix-from-lists '((1 2 3) (4 5 6)))))
  
  (test-true "matrix-symmetric?"
             (matrix-symmetric? (matrix-from-lists '((1 2) (2 1)))))
  
  (test-false "matrix-symmetric? false"
              (matrix-symmetric? (matrix-from-lists '((1 2) (3 1)))))
  
  (test "trace"
        5
        (trace (matrix-from-lists '((1 2) (3 4)))))
  
  ;; Frobenius norm of 2x2 identity = sqrt(1^2 + 0^2 + 0^2 + 1^2) = sqrt(2)
  (test-true "frobenius-norm identity"
             (< (abs (- (frobenius-norm (identity 2)) (sqrt 2))) 1e-10))
  
  ;; Special Matrices
  (display "
--- Special Matrices ---
")
  
  (test "zeros"
        '((0 0 0) (0 0 0))
        (matrix->lists (zeros 2 3)))
  
  (test "ones"
        '((1 1) (1 1) (1 1))
        (matrix->lists (ones 3 2)))
  
  (test "identity 3x3"
        '((1 0 0) (0 1 0) (0 0 1))
        (matrix->lists (identity 3)))
  
  (test "diagonal"
        '((1 0 0) (0 2 0) (0 0 3))
        (matrix->lists (diagonal (vector 1 2 3))))
  
  ;; Slicing
  (display "
--- Matrix Slicing ---
")
  
  (let ([m (matrix-from-lists '((1 2 3 4) (5 6 7 8) (9 10 11 12)))])
       (test "matrix-submatrix"
             '((6 7) (10 11))
             (matrix->lists (matrix-submatrix m 1 1 3 3))))
  
  ;; Concatenation
  (display "
--- Matrix Concatenation ---
")
  
  (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
        [m2 (matrix-from-lists '((5 6) (7 8)))])
       (test "matrix-hstack"
             '((1 2 5 6) (3 4 7 8))
             (matrix->lists (matrix-hstack m1 m2)))
       
       (test "matrix-vstack"
             '((1 2) (3 4) (5 6) (7 8))
             (matrix->lists (matrix-vstack m1 m2))))
  
  (test-error "matrix-hstack dimension mismatch"
              (matrix-hstack (matrix-from-lists '((1 2)))
                             (matrix-from-lists '((1) (2)))))
  
  ;; Identity properties
  (display "
--- Identity Properties ---
")
  
  (let ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))]
        [I (identity 3)])
       (test "A * I = A"
             (matrix->lists m)
             (matrix->lists (matrix-mul m I)))
       
       (test "I * A = A"
             (matrix->lists m)
             (matrix->lists (matrix-mul I m))))
  
  ;; Summary
  (display "
=== Test Summary ===
")
  (display "Total: ")
  (display *test-count*)
  (display ", Passed: ")
  (display *pass-count*)
  (display ", Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!
")
      (display "Some tests failed.
"))
  
  (= *fail-count* 0))

;; Run tests
(run-tests)
