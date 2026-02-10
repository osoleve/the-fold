;;; lattice/fp/category/test-logic-adjunction.ss — Tests for Logic Adjunctions
;;;
;;; Tests for:
;;;   - Diagonal functor and pair category operations
;;;   - Product/coproduct as adjoints to diagonal
;;;   - Dependent type families and substitution
;;;   - Curry-Howard logical connectives
;;;
;;; Run from project root: scheme --script lattice/fp/category/test-logic-adjunction.ss

(load "core/lang/module.ss")
(load "lattice/fp/category/logic-adjunction.ss")

;;; ====
;;; Test Helpers
;;; ====

(define test-passed 0)
(define test-failed 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
        (set! test-passed (+ test-passed 1))
        (display "ok"))
      (begin
        (set! test-failed (+ test-failed 1))
        (display "FAIL")
        (newline)
        (display "    Expected: ")
        (write expected)
        (newline)
        (display "    Actual:   ")
        (write actual)))
  (newline))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Test: Pair Category Operations
;;; ====

(section "Pair Category Operations")

(test "make-pair-obj constructs pair"
      (cons 1 2)
      (make-pair-obj 1 2))

(test "pair-fst extracts first"
      1
      (pair-fst (make-pair-obj 1 2)))

(test "pair-snd extracts second"
      2
      (pair-snd (make-pair-obj 1 2)))

(test "make-pair-mor applies both functions"
      (cons 2 6)
      ((make-pair-mor add1 (lambda (x) (* x 2)))
       (make-pair-obj 1 3)))

;;; ====
;;; Test: Diagonal Functor
;;; ====

(section "Diagonal Functor")

(test "diagonal-obj duplicates"
      (cons 5 5)
      (diagonal-obj 5))

(test "diagonal-mor applies function to both"
      (cons 10 10)
      ((diagonal-mor (lambda (x) (* x 2)))
       (make-pair-obj 5 5)))

(test-true "functor-diagonal is a functor"
           (functor? functor-diagonal))

(test "functor-diagonal name is Δ"
      'Δ
      (functor-name functor-diagonal))

;;; ====
;;; Test: Product and Coproduct Operations
;;; ====

(section "Product and Coproduct")

;; Product morphisms
(test "product-mor applies pair of functions"
      (cons 2 6)
      ((product-mor (make-pair-obj add1 (lambda (x) (* x 2))))
       (make-pair-obj 1 3)))

;; Coproduct (sum) construction
(test-true "make-left creates left value"
           (left? (make-left 5)))

(test-true "make-right creates right value"
           (right? (make-right 7)))

(test-false "left is not right"
            (right? (make-left 5)))

(test "from-left extracts value"
      5
      (from-left (make-left 5)))

(test "from-right extracts value"
      7
      (from-right (make-right 7)))

;; Coproduct morphisms
(test "coproduct-mor on left"
      (make-left 2)
      ((coproduct-mor (make-pair-obj add1 (lambda (x) (* x 2))))
       (make-left 1)))

(test "coproduct-mor on right"
      (make-right 6)
      ((coproduct-mor (make-pair-obj add1 (lambda (x) (* x 2))))
       (make-right 3)))

;;; ====
;;; Test: Diagonal-Product Adjunction (Δ ⊣ ×)
;;; ====

(section "Adjunction Δ ⊣ ×")

(test-true "adj-diagonal-product is an adjunction"
           (adjunction? adj-diagonal-product))

(test "unit duplicates value"
      (cons 5 5)
      (unit-diagonal-product 5))

(test "counit extracts from diagonal of product"
      (cons 1 2)
      (counit-diagonal-product (make-pair-obj (cons 1 2) (cons 1 2))))

;; Triangle identities for Δ ⊣ ×
;; Left triangle: ε_{F(A)} ∘ F(η_A) = id_{F(A)}
;; Here F = Δ, so: ε_{(A,A)} ∘ Δ(η_A) = id
(test "left triangle: counit . Δ(unit) = id"
      (cons 5 5)
      (counit-diagonal-product
       ((diagonal-mor unit-diagonal-product)
        (cons 5 5))))

;;; ====
;;; Test: Coproduct-Diagonal Adjunction (+ ⊣ Δ)
;;; ====

(section "Adjunction + ⊣ Δ")

(test-true "adj-coproduct-diagonal is an adjunction"
           (adjunction? adj-coproduct-diagonal))

(test "unit injects pair into coproduct-diagonal"
      (cons (make-left 1) (make-right 2))
      (unit-coproduct-diagonal (make-pair-obj 1 2)))

(test "counit folds left"
      5
      (counit-coproduct-diagonal (make-left 5)))

(test "counit folds right"
      7
      (counit-coproduct-diagonal (make-right 7)))

;;; ====
;;; Test: Dependent Type Families
;;; ====

(section "Dependent Type Families")

;; Family: Vec indexed by Nat
;; Vec(0) = Unit, Vec(n+1) = A × Vec(n)
(define fam-vec
  (make-family 'Nat
               (lambda (n)
                 (if (= n 0)
                     'Unit
                     (list '× 'A (list 'Vec (- n 1)))))))

(test-true "make-family creates family"
           (family? fam-vec))

(test "family-index returns index type"
      'Nat
      (family-index fam-vec))

(test "family-at 0 = Unit"
      'Unit
      (family-at fam-vec 0))

(test "family-at 1 = A × Vec 0"
      '(× A (Vec 0))
      (family-at fam-vec 1))

(test "family-at 3 = nested product"
      '(× A (Vec 2))
      (family-at fam-vec 3))

;; Substitution: compose with doubling
(define fam-vec-doubled
  (subst-family (lambda (n) (* n 2)) fam-vec 'Nat))

(test-true "subst-family creates family"
           (family? fam-vec-doubled))

(test "substituted family at 1 = original at 2"
      (family-at fam-vec 2)
      (family-at fam-vec-doubled 1))

;; Substitution functor
(define subst-double (make-subst-functor (lambda (n) (* n 2)) 'Nat))

(test-true "make-subst-functor creates functor"
           (functor? subst-double))

;; Test fmap: transform a family morphism
;; h wraps types in a list
(define (type-wrapper t) (list 'Wrapped t))
(define transformed-fam
  ((functor-fmap subst-double) type-wrapper fam-vec))

(test-true "subst-functor fmap returns family"
           (family? transformed-fam))

(test "subst-functor fmap applies h at f(i)"
      '(Wrapped (× A (Vec 1)))
      (family-at transformed-fam 1))  ; h(fam-vec(2)) since f(1)=2

;;; ====
;;; Test: Sigma and Pi Types
;;; ====

(section "Sigma and Pi Types")

;; Sigma: dependent pair
(test "sigma-fst on (5, \"hello\")"
      5
      (sigma-fst (make-sigma-pair 5 "hello")))

(test "sigma-snd on (5, \"hello\")"
      "hello"
      (sigma-snd (make-sigma-pair 5 "hello")))

;; Pi: dependent function (just functions at term level)
(define pi-example (make-pi-fn (lambda (x) (* x x))))

(test "pi-apply on squaring function"
      25
      (pi-apply pi-example 5))

;;; ====
;;; Test: Curry-Howard Logical Connectives
;;; ====

(section "Curry-Howard Logical Connectives")

;; Conjunction (product)
(test "conj-intro creates pair"
      (cons 'a 'b)
      (conj-intro 'a 'b))

(test "conj-elim-left projects first"
      'a
      (conj-elim-left (conj-intro 'a 'b)))

(test "conj-elim-right projects second"
      'b
      (conj-elim-right (conj-intro 'a 'b)))

;; Disjunction (coproduct)
(test-true "disj-intro-left creates left"
           (left? (disj-intro-left 'a)))

(test-true "disj-intro-right creates right"
           (right? (disj-intro-right 'b)))

(test "disj-elim on left"
      10
      (disj-elim (disj-intro-left 5)
                 (lambda (x) (* x 2))
                 (lambda (x) (+ x 100))))

(test "disj-elim on right"
      107
      (disj-elim (disj-intro-right 7)
                 (lambda (x) (* x 2))
                 (lambda (x) (+ x 100))))

;; Existential (sigma)
(test "exists-intro creates witness-proof pair"
      (cons 5 #t)
      (exists-intro 5 #t))

(test "exists-elim extracts and uses"
      10
      (exists-elim (exists-intro 5 "proof")
                   (lambda (witness proof) (* witness 2))))

;; Universal (pi)
(define all-even (forall-intro (lambda (n) (* n 2))))

(test "forall-elim applies function"
      10
      (forall-elim all-even 5))

;;; ====
;;; Test: Properties and Laws
;;; ====

(section "Logical Laws as Type Isomorphisms")

;; A ∧ B ↔ B ∧ A (commutativity of conjunction)
(define (swap-conj ab)
  (conj-intro (conj-elim-right ab) (conj-elim-left ab)))

(test "conjunction commutativity"
      (cons 'b 'a)
      (swap-conj (conj-intro 'a 'b)))

;; A ∨ B ↔ B ∨ A (commutativity of disjunction)
(define (swap-disj ab)
  (disj-elim ab disj-intro-right disj-intro-left))

(test "disjunction commutativity on left"
      (make-right 'a)
      (swap-disj (disj-intro-left 'a)))

(test "disjunction commutativity on right"
      (make-left 'b)
      (swap-disj (disj-intro-right 'b)))

;; A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) (distributivity)
(define (distrib-and-or a-and-bc)
  (let ([a (conj-elim-left a-and-bc)]
        [bc (conj-elim-right a-and-bc)])
    (disj-elim bc
               (lambda (b) (disj-intro-left (conj-intro a b)))
               (lambda (c) (disj-intro-right (conj-intro a c))))))

(test "distributivity: A∧(B∨C) → (A∧B)∨(A∧C) on left"
      (make-left (cons 'a 'b))
      (distrib-and-or (conj-intro 'a (disj-intro-left 'b))))

(test "distributivity: A∧(B∨C) → (A∧B)∨(A∧C) on right"
      (make-right (cons 'a 'c))
      (distrib-and-or (conj-intro 'a (disj-intro-right 'c))))

;;; ====
;;; Summary
;;; ====

(newline)
(display "==========================================")
(newline)
(display "Logic Adjunction Tests: ")
(display test-passed)
(display " passed, ")
(display test-failed)
(display " failed")
(newline)

(when (> test-failed 0)
  (exit 1))
