;;; fabric/stitches/test-type-compute.ss — Tests for Type-Level Computation
;;;
;;; Tests for type-level arithmetic, conditionals, and functions.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/lang/nbe.ss")

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
       (display "  ")
       (display name)
       (display ": ")
       (write actual)
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

(define (run-tests)
  (display "
=== Type-Level Computation Test Suite ===

")

  ;; Type-level arithmetic with known values
  (display "--- Type-Level Arithmetic (Known Values) ---
")

  (test "(+ 2 3) normalizes to 5"
        5
        (normalize-closed '(+ 2 3)))

  (test "(- 10 4) normalizes to 6"
        6
        (normalize-closed '(- 10 4)))

  (test "(* 3 7) normalizes to 21"
        21
        (normalize-closed '(* 3 7)))

  (test "(+ 1 (+ 2 3)) normalizes to 6"
        6
        (normalize-closed '(+ 1 (+ 2 3))))

  (test "(* 2 (+ 3 4)) normalizes to 14"
        14
        (normalize-closed '(* 2 (+ 3 4))))

  ;; Type-level comparisons
  (display "
--- Type-Level Comparisons ---
")

  (test "(< 3 5) normalizes to #t"
        #t
        (normalize-closed '(< 3 5)))

  (test "(< 5 3) normalizes to #f"
        #f
        (normalize-closed '(< 5 3)))

  (test "(> 10 5) normalizes to #t"
        #t
        (normalize-closed '(> 10 5)))

  (test "(= 42 42) normalizes to #t"
        #t
        (normalize-closed '(= 42 42)))

  (test "(= 1 2) normalizes to #f"
        #f
        (normalize-closed '(= 1 2)))

  ;; Type-level conditionals with known conditions
  (display "
--- Type-Level Conditionals (Known) ---
")

  (test "(if #t Int Bool) normalizes to Int"
        'Int
        (normalize-closed '(if #t Int Bool)))

  (test "(if #f Int Bool) normalizes to Bool"
        'Bool
        (normalize-closed '(if #f Int Bool)))

  (test "(if (< 1 2) Nat String) normalizes to Nat"
        'Nat
        (normalize-closed '(if (< 1 2) Nat String)))

  (test "(if (> 1 2) Nat String) normalizes to String"
        'String
        (normalize-closed '(if (> 1 2) Nat String)))

  ;; Nested conditionals
  (display "
--- Nested Conditionals ---
")

  (test "(if #t (if #f A B) C) normalizes to B"
        'B
        (normalize-closed '(if #t (if #f A B) C)))

  (test "(if (= (+ 1 1) 2) Yes No) normalizes to Yes"
        'Yes
        (normalize-closed '(if (= (+ 1 1) 2) Yes No)))

  ;; Stuck computations (with variables)
  (display "
--- Stuck Computations ---
")

  ;; When a variable is unknown, computation gets stuck
  (let ([result (normalize '(+ n 3) (env-extend nbe-empty-env 'n (V-neutral (N-var 'n))))])
       (test "(+ n 3) with unknown n stays stuck"
             '(+ n 3)
             result))

  (let ([result (normalize '(if b Int Bool) (env-extend nbe-empty-env 'b (V-neutral (N-var 'b))))])
       (test "(if b Int Bool) with unknown b stays stuck"
             '(if b Int Bool)
             result))

  ;; Type equality through computation
  (display "
--- Type Equality via Computation ---
")

  (test-true "(+ 2 3) = 5"
             (types-equal? '(+ 2 3) 5))

  (test-true "(* 2 (+ 1 2)) = 6"
             (types-equal? '(* 2 (+ 1 2)) 6))

  (test-true "(if #t Int Bool) = Int"
             (types-equal? '(if #t Int Bool) 'Int))

  (test-true "(if (< 1 2) A B) = A"
             (types-equal? '(if (< 1 2) A B) 'A))

  ;; Type-level functions via lambda
  (display "
--- Type-Level Functions ---
")

  ;; A type-level function that adds 1
  (let ([succ-type '(fn (n) (+ n 1))])
       ;; Apply it to 5
       (test "((fn (n) (+ n 1)) 5) normalizes to 6"
             6
             (normalize-closed `(,succ-type 5))))

  ;; A type-level function for double
  (let ([double-type '(fn (n) (* n 2))])
       (test "((fn (n) (* n 2)) 7) normalizes to 14"
             14
             (normalize-closed `(,double-type 7))))

  ;; Type-level composition
  (let ([add1 '(fn (n) (+ n 1))]
        [double '(fn (n) (* n 2))])
       (test "(double (add1 3)) normalizes to 8"
             8
             (normalize-closed `(,double (,add1 3)))))

  ;; Vec-like type indexing
  (display "
--- Vec-Like Type Indexing ---
")

  ;; Simulate Vec n A where we can compute with n
  ;; Note: Type application is curried, so (Vec 5 Int) reads back as ((Vec 5) Int)
  (test "(Vec (+ 2 3) Int) normalizes to ((Vec 5) Int)"
        '((Vec 5) Int)
        (normalize-closed '(Vec (+ 2 3) Int)))

  ;; Matrix dimension computation
  (test "(Matrix (* 2 3) (* 3 4) Float) normalizes to (((Matrix 6) 12) Float)"
        '(((Matrix 6) 12) Float)
        (normalize-closed '(Matrix (* 2 3) (* 3 4) Float)))

  ;; Edge cases
  (display "
--- Edge Cases ---
")

  (test "(+ 0 0) normalizes to 0"
        0
        (normalize-closed '(+ 0 0)))

  (test "(* 0 1000) normalizes to 0"
        0
        (normalize-closed '(* 0 1000)))

  (test "(- 5 5) normalizes to 0"
        0
        (normalize-closed '(- 5 5)))

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
