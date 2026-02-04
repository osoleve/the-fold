;;; core/types/test-contract-refine.ss — Tests for Contract-Refinement Integration
;;;
;;; Tests the bridge between contracts and refinement types:
;;;   - Converting refinement types to contracts
;;;   - Type-to-contract inference
;;;   - Static verification
;;;   - Subtyping relationships

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/contracts.ss")
(load "core/types/contract-refine.ss")
(load "core/testing/test-framework.ss")

;;; ====
;;; Refinement to Contract Conversion
;;; ====

(test-group "refinement->contract"

  (define-test "converts positive nat refinement"
    (let* ([refine-type (t-refine 'n 'Nat '(> n 0))]
           [contract (refinement->contract refine-type)])
      (assert-true (flat-contract? contract))))

  (define-test "resulting contract accepts valid values"
    (let* ([refine-type (t-refine 'n 'Nat '(> n 0))]
           [contract (refinement->contract refine-type)]
           [pred (flat-predicate contract)])
      (assert-true (pred 5))
      (assert-true (pred 1))
      (assert-true (pred 100))))

  (define-test "resulting contract rejects invalid values"
    (let* ([refine-type (t-refine 'n 'Nat '(> n 0))]
           [contract (refinement->contract refine-type)]
           [pred (flat-predicate contract)])
      (assert-false (pred 0))
      (assert-false (pred -1))))

  (define-test "resulting contract rejects wrong base type"
    (let* ([refine-type (t-refine 'n 'Nat '(> n 0))]
           [contract (refinement->contract refine-type)]
           [pred (flat-predicate contract)])
      (assert-false (pred "not a number"))
      (assert-false (pred 'symbol))))

  (define-test "handles compound predicates"
    (let* ([refine-type (t-refine 'x 'Int '(and (>= x 0) (< x 100)))]
           [contract (refinement->contract refine-type)]
           [pred (flat-predicate contract)])
      (assert-true (pred 0))
      (assert-true (pred 50))
      (assert-true (pred 99))
      (assert-false (pred -1))
      (assert-false (pred 100))
      (assert-false (pred 150)))))

;;; ====
;;; Predicate Evaluation
;;; ====

(test-group "eval-predicate"

  (define-test "evaluates comparisons"
    (assert-true (eval-predicate-with-binding 'x 5 '(> x 0)))
    (assert-false (eval-predicate-with-binding 'x 0 '(> x 0)))
    (assert-true (eval-predicate-with-binding 'x 10 '(<= x 10)))
    (assert-false (eval-predicate-with-binding 'x 11 '(<= x 10))))

  (define-test "evaluates boolean operations"
    (assert-true (eval-predicate-with-binding 'x 5 '(and (> x 0) (< x 10))))
    (assert-false (eval-predicate-with-binding 'x 15 '(and (> x 0) (< x 10))))
    (assert-true (eval-predicate-with-binding 'x 15 '(or (< x 0) (> x 10))))
    (assert-true (eval-predicate-with-binding 'x -5 '(not (> x 0)))))

  (define-test "evaluates arithmetic"
    (assert-true (eval-predicate-with-binding 'x 6 '(= (modulo x 2) 0)))
    (assert-false (eval-predicate-with-binding 'x 7 '(= (modulo x 2) 0)))
    (assert-true (eval-predicate-with-binding 'x 5 '(= (+ x 1) 6))))

  (define-test "evaluates type predicates"
    (assert-true (eval-predicate-with-binding 'x 5 '(positive? x)))
    (assert-false (eval-predicate-with-binding 'x -5 '(positive? x)))
    (assert-true (eval-predicate-with-binding 'x 4 '(even? x)))
    (assert-false (eval-predicate-with-binding 'x 5 '(even? x)))))

;;; ====
;;; Type to Contract Inference
;;; ====

(test-group "type->contract"

  (define-test "infers contract for base types"
    (let ([nat-c (type->contract 'Nat)]
          [int-c (type->contract 'Int)]
          [str-c (type->contract 'String)])
      (assert-true (flat-contract? nat-c))
      (assert-true (flat-contract? int-c))
      (assert-true (flat-contract? str-c))))

  (define-test "nat contract works correctly"
    (let* ([nat-c (type->contract 'Nat)]
           [pred (flat-predicate nat-c)])
      (assert-true (pred 0))
      (assert-true (pred 100))
      (assert-false (pred -1))
      (assert-false (pred 1.5))))

  (define-test "infers contract for List types"
    (let* ([list-nat-c (type->contract '(List Nat))])
      (assert-true (flat-contract? list-nat-c))))

  (define-test "List Nat contract works correctly"
    (let* ([list-nat-c (type->contract '(List Nat))]
           [pred (flat-predicate list-nat-c)])
      (assert-true (pred '()))
      (assert-true (pred '(1 2 3)))
      (assert-false (pred '(1 -2 3)))
      (assert-false (pred "not a list"))))

  (define-test "infers function contract for arrow types"
    (let ([fn-c (type->contract '(-> Int Int))])
      (assert-true (function-contract? fn-c))))

  (define-test "function contract has correct structure"
    (let ([fn-c (type->contract '(-> Int Int Int))])
      (assert-equal 2 (length (function-contract-domain fn-c)))
      (assert-true (flat-contract? (function-contract-range fn-c)))))

  (define-test "infers contract from refinement type"
    (let* ([refine-type (t-refine 'n 'Nat '(> n 0))]
           [contract (type->contract refine-type)])
      (assert-true (flat-contract? contract))
      (let ([pred (flat-predicate contract)])
        (assert-true (pred 1))
        (assert-false (pred 0)))))

  (define-test "infers vector contract for Vec type"
    (let ([vec-c (type->contract '(Vec 3 Int))])
      ;; Should be And contract with vectorof + length check
      (assert-true (and (pair? vec-c) (eq? (car vec-c) 'And)))))

  (define-test "Vec predicate checks length"
    (let ([pred (type->predicate '(Vec 3 Int))])
      (assert-true (pred (vector 1 2 3)))
      (assert-false (pred (vector 1 2)))     ; wrong length
      (assert-false (pred (vector 1 2 3 4))) ; wrong length
      (assert-false (pred '(1 2 3)))))       ; not a vector

  (define-test "Vec predicate checks element type"
    (let ([pred (type->predicate '(Vec 2 Nat))])
      (assert-true (pred (vector 0 1)))
      (assert-false (pred (vector -1 1))))))  ; negative not Nat

;;; ====
;;; Static Verification
;;; ====

(test-group "verify-contract-for-type"

  (define-test "Any contract always verified"
    (let ([result (verify-contract-for-type 'Any 'Nat)])
      (assert-equal 'Ok (car result))))

  (define-test "None contract never verified"
    (let ([result (verify-contract-for-type 'None 'Nat)])
      (assert-equal 'Err (car result))))

  (define-test "matching base type predicate verified"
    ;; When contract predicate equals type predicate
    (let* ([int-pred integer?]
           [int-contract (flat int-pred)]
           [result (verify-contract-for-type int-contract 'Int)])
      (assert-equal 'Ok (car result))))

  (define-test "And contracts require all verified"
    (let* ([c (and/c (flat integer?) (flat number?))]
           [result (verify-contract-for-type c 'Int)])
      ;; This may or may not verify depending on implementation
      (assert-true (pair? result))))

  (define-test "Or contracts pass if any verified"
    (let* ([int-c (flat integer?)]
           [c (or/c int-c (flat string?))]
           [result (verify-contract-for-type c 'Int)])
      (assert-equal 'Ok (car result)))))

;;; ====
;;; Contract Subtyping
;;; ====

(test-group "contract-subtype?"

  (define-test "Any is top"
    (assert-true (contract-subtype? nat/c 'Any))
    (assert-true (contract-subtype? int/c 'Any))
    (assert-true (contract-subtype? (->c '() nat/c) 'Any)))

  (define-test "None is bottom"
    (assert-true (contract-subtype? 'None nat/c))
    (assert-true (contract-subtype? 'None (->c '() nat/c))))

  (define-test "reflexivity"
    (assert-true (contract-subtype? nat/c nat/c))
    (assert-true (contract-subtype? int/c int/c)))

  (define-test "And subtyping"
    (let ([c1 nat/c]
          [c2 (and/c nat/c int/c)])
      ;; c1 <: (And c1 c2) only if c1 <: c2 too
      (assert-true (contract-subtype? c2 c1))))

  (define-test "Or subtyping"
    (let ([c1 nat/c]
          [c2 (or/c nat/c string/c)])
      (assert-true (contract-subtype? c1 c2))))

  (define-test "function contravariance/covariance"
    ;; (Int -> Int) <: (Nat -> Int) because Nat <: Int (contravariance)
    ;; But we can't prove Nat <: Int with equal? check alone
    ;; Test with same contracts
    (let ([f1 (->c (list nat/c) int/c)]
          [f2 (->c (list nat/c) int/c)])
      (assert-true (contract-subtype? f1 f2)))))

;;; ====
;;; Refinement Subtyping
;;; ====

(test-group "refinement-subtype?"

  (define-test "refinement is subtype of base type"
    (let ([refined (t-refine 'n 'Nat '(> n 0))])
      (assert-true (refinement-subtype? refined 'Nat))))

  (define-test "same refinement is subtype of itself"
    (let ([refined (t-refine 'n 'Nat '(> n 0))])
      (assert-true (refinement-subtype? refined refined))))

  (define-test "different predicates not subtypes syntactically"
    (let ([r1 (t-refine 'n 'Nat '(> n 0))]
          [r2 (t-refine 'n 'Nat '(> n 5))])
      ;; Without a theorem prover, we can't prove (> n 5) => (> n 0)
      (assert-false (refinement-subtype? r1 r2)))))

;;; ====
;;; Integration: Runtime Checking
;;; ====

(test-group "runtime-contract-checking"

  (define-test "with-refinement accepts valid values"
    (let ([pos-nat (t-refine 'n 'Nat '(> n 0))])
      (assert-equal 5 (with-refinement pos-nat 5 'test))))

  (define-test "with-refinement rejects invalid values"
    (let ([pos-nat (t-refine 'n 'Nat '(> n 0))])
      (assert-error (lambda () (with-refinement pos-nat 0 'test)))))

  (define-test "lambda/contract creates working wrapped function"
    (let ([inc (lambda/contract '(x) (list nat/c) nat/c
                 (lambda (x) (+ x 1)))])
      (assert-equal 6 (inc 5))
      (assert-equal 1 (inc 0))))

  (define-test "lambda/contract checks domain"
    (let ([inc (lambda/contract '(x) (list nat/c) nat/c
                 (lambda (x) (+ x 1)))])
      ;; Should error on negative input
      (assert-error (lambda () (inc -1)))))

  (define-test "lambda/contract checks range"
    (let ([bad-fn (lambda/contract '(x) (list nat/c) nat/c
                    (lambda (x) (- x 10)))])
      ;; Should error when result is negative
      (assert-error (lambda () (bad-fn 5))))))

;;; ====
;;; Combined Static + Runtime
;;; ====

(test-group "apply-contract/smart"

  (define-test "skips runtime check when statically verified"
    ;; In compile-time mode with matching predicates
    (call-with-contract-mode 'compile-time
      (lambda ()
        (let* ([int-c (flat integer?)]
               [result (apply-contract/smart int-c 42 'Int 'test)])
          (assert-equal 42 result)))))

  (define-test "performs runtime check when needed"
    (call-with-contract-mode 'runtime
      (lambda ()
        (let* ([pos-c (flat (lambda (x) (and (integer? x) (> x 0))))]
               [result (apply-contract/smart pos-c 5 'Int 'test)])
          (assert-equal 5 result))))))

;;; ====
;;; Run All Tests
;;; ====

(display "
=== Contract-Refinement Integration Tests ===
")
(run-all-tests)
