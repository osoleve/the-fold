;;; lattice/fp/category/test-adjunction.ss — Adjunction Tests
;;;
;;; Tests for adjoint functors, triangle identities, hom-set bijection,
;;; and Galois connections.
;;;
;;; Run from project root: scheme --script lattice/fp/category/test-adjunction.ss

(load "lattice/fp/category/adjunction.ss")

;;; ==== 
;;; Test Helpers
;;; ====

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(define (test-true name actual)
  (test name #t (if actual #t #f)))

(define (test-false name actual)
  (test name #f actual))

(define (section name)
  (newline)
  (display name)
  (newline))

;;; ==== 
;;; Test: Adjunction Basics
;;; ====

(section "Adjunction Basics")

(test-true "make-adjunction creates adjunction"
           (adjunction? adj-free-list))

(test "adjunction-name extracts name"
      'free-list
      (adjunction-name adj-free-list))

(test-true "adjunction-left extracts functor"
           (functor? (adjunction-left adj-free-list)))

(test-true "adjunction-right extracts functor"
           (functor? (adjunction-right adj-free-list)))

(test-true "adjunction-unit extracts nat-transform"
           (nat-transform? (adjunction-unit adj-free-list)))

(test-true "adjunction-counit extracts nat-transform"
           (nat-transform? (adjunction-counit adj-free-list)))

;;; ==== 
;;; Test: Triangle Identities
;;; ====

(section "Triangle Identities (Free Monoid: List ⊣ Id)")

;; Left triangle: ε_{F(A)} ∘ F(η_A) = id_{F(A)}
;; Test with val = '(1 2) in F(A)
(test-true "verify-triangle-left on list"
           (verify-triangle-left adj-free-list '(1 2 3)))

(test-true "verify-triangle-left on empty list"
           (verify-triangle-left adj-free-list '()))

;; Right triangle: G(ε_B) ∘ η_{G(B)} = id_{G(B)}
;; Test with val = '(a b) in G(B) (must be list for this adjunction)
(test-true "verify-triangle-right on list"
           (verify-triangle-right adj-free-list '(a b)))

(test-true "verify-triangle-right on empty list"
           (verify-triangle-right adj-free-list '()))

;; Helper wrapper
(test-true "verify-adjunction"
           (verify-adjunction adj-free-list '(1 2) '(a b)))

;;; ==== 
;;; Test: Hom-Set Bijection
;;; ====

(section "Hom-Set Bijection")

;; Left-to-Right: (F(A) → B) → (A → G(B))
;; f: List(Int) -> Int (sum)
;; g: Int -> Id(Int) (should return sum of singleton?)
;; g(x) = G(f)(η_A(x)) = f([x])
(let* ([f (lambda (xs) (apply + xs))] ; F(A) -> B
       [transpose (adjunction-transpose-left adj-free-list f)]
       [g transpose])
  (test "transpose-left: sum([10])" 10 (g 10))
  (test "transpose-left: sum([5])" 5 (g 5)))

;; Right-to-Left: (A → G(B)) → (F(A) → B)
;; g: Int -> Id(List(Int)) (g(x) = [x, x])
;; f: List(Int) -> List(Int)
;; f(xs) = ε_B(F(g)(xs)) = concat(map g xs)
(let* ([g (lambda (x) (list x x))] ; A -> G(B)
       [transpose (adjunction-transpose-right adj-free-list g)]
       [f transpose])
  ;; f([1 2]) = concat([[1 1] [2 2]]) = [1 1 2 2]
  (test "transpose-right: duplicate elements" '(1 1 2 2) (f '(1 2)))
  (test "transpose-right: empty" '() (f '())))

;;; ====
;;; Test: Adjunction Composition
;;; ====

(section "Adjunction Composition")

;; Define Identity Adjunction
(define adj-id
  (make-adjunction
   'id
   functor-id
   functor-id
   (nat-id functor-id)
   (nat-id functor-id)))

;; Compose adj-free-list with adj-id
;; (List ⊣ Id) ∘ (Id ⊣ Id) = List ⊣ Id
(let ([composed (adjunction-compose adj-free-list adj-id)])
  (test-true "adjunction-compose creates adjunction"
             (adjunction? composed))
  
  (test "composed name" 'free-list∘id (adjunction-name composed))
  
  ;; Verify Left Triangle (List ∘ Id = List)
  ;; val = '(1 2)
  (test-true "verify-triangle-left composed"
             (verify-triangle-left composed '(1 2)))
             
  ;; Verify Right Triangle (Id ∘ Id = Id)
  ;; val = '(a b)
  (test-true "verify-triangle-right composed"
             (verify-triangle-right composed '(a b))))
;;; ====
;;; Test: Input Validation
;;; ====

(section "Input Validation")

;; Helper to test that an expression throws an error
(define (test-error name thunk)
  (display "  ")
  (display name)
  (display ": ")
  (let ([result (guard (e [else 'error-raised])
                  (thunk)
                  'no-error)])
    (if (eq? result 'error-raised)
        (display "✓")
        (begin
          (display "✗ (expected error, got: ")
          (display result)
          (display ")"))))
  (newline))

(test-error "adjunction-compose rejects non-adjunction arg1"
            (lambda () (adjunction-compose adj-free-list 'not-an-adjunction)))

(test-error "adjunction-compose rejects non-adjunction arg2"
            (lambda () (adjunction-compose 'not-an-adjunction adj-free-list)))

(test-false "galois? rejects malformed galois (wrong length)"
            (galois? '(galois too few)))

(test-false "galois? rejects non-galois"
            (galois? '(not-galois a b c)))

;;; ====
;;; Test: Galois Connections
;;; ====

(section "Galois Connections")

(test-true "make-galois creates galois"
           (galois? galois-floor-ceil))

(test "galois-lower is ceiling" 4.0 ((galois-lower galois-floor-ceil) 3.5))
(test "galois-upper is identity" 4 ((galois-upper galois-floor-ceil) 4))

;; Closure: u ∘ l = ceil(x)
(test "galois-closure (ceil)" 4.0 (galois-closure galois-floor-ceil 3.1))
(test "galois-closure idempotent" 4 (galois-closure galois-floor-ceil 4))

;; Kernel: l ∘ u = ceil(x) (since u is id)
;; Note: Inclusion ⊣ Floor is usually u∘l = floor.
;; Here Ceil ⊣ Inclusion.
;; l = ceil. u = id.
;; u(l(x)) = ceil(x).
;; l(u(n)) = ceil(n) = n.
(test "galois-kernel (id on ints)" 5 (galois-kernel galois-floor-ceil 5))

;;; ==== 
;;; Test: Display
;;; ====

(section "Display")

(test-true "adjunction->string produces string"
           (string? (adjunction->string adj-free-list)))

;;; ==== 
;;; Summary
;;; ====

(newline)
(display "Adjunction tests complete.")
(newline)
