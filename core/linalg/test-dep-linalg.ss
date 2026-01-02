;;; fabric/stitches/test-dep-linalg.ss — Tests for Dependent Linear Algebra
;;;
;;; Verifies dimension-safe vector and matrix operations.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/lang/nbe.ss")
(load "core/types/infer.ss")
(load "core/types/dep-infer.ss")
(load "core/linalg/dep-linalg.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "✗ ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define linalg-ctx
  (list (list 'vec-append-typed '(Π ((n : Nat) (m : Nat) (A : Type)) (-> (Vec n A) (Vec m A) (Vec (+ n m) A))))
        (list 'matrix-mul-typed '(Π ((m : Nat) (n : Nat) (p : Nat) (A : Type)) (-> (Matrix m n A) (Matrix n p A) (Matrix m p A))))
        (list 'vec-zip-typed '(Π ((n : Nat) (A : Type) (B : Type)) (-> (Vec n A) (Vec n B) (Vec n (× A B)))))))

(define (test-type name expr expected-type)
  (let ([result (dep-synth expr linalg-ctx)])
       (if (eq? (car result) 'ok)
           (test name expected-type (cadr result))
           (test name expected-type result))))

(define (run-tests)
  (display "
=== Dependent Linear Algebra Test Suite ===

")

  ;; Type Synthesis for Vec and Matrix
  (display "--- Type Synthesis ---
")
  (test-type "(Vec 3 Int) : Type" '(Vec 3 Int) 'Type)
  (test-type "(Matrix 2 2 Float) : Type" '(Matrix 2 2 Float) 'Type)

  ;; Dimension-safe vector operations
  (display "
--- Dimension-Safe Vector Operations ---
")

  ;; vec-append-typed synthesis
  (test-type "vec-append-typed type synthesis"
             'vec-append-typed
             '(Π ((n : Nat)) (Π ((m : Nat)) (Π ((A : Type)) (-> (Vec n A) (Vec m A) (Vec (+ n m) A))))))

  ;; vec-append-typed application
  (test-type "vec-append-typed application"
             '(vec-append-typed 2 3 Int)
             '(-> (Vec 2 Int) (Vec 3 Int) (Vec 5 Int)))

  ;; Matrix operations
  (display "
--- Dimension-Safe Matrix Operations ---
")

  (test-type "matrix-mul-typed type synthesis"
             'matrix-mul-typed
             '(Π ((m : Nat)) (Π ((n : Nat)) (Π ((p : Nat)) (Π ((A : Type)) (-> (Matrix m n A) (Matrix n p A) (Matrix m p A)))))))

  (test-type "matrix-mul-typed application"
             '(matrix-mul-typed 2 3 4 Int)
             '(-> (Matrix 2 3 Int) (Matrix 3 4 Int) (Matrix 2 4 Int)))

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
