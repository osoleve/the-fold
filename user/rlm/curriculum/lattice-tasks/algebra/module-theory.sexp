;;; module-theory.sexp — Development tasks for lattice/algebra/module.ss
;;;
;;; Module: module-theory (tier 0, depends on prelude, ring)
;;; 567 lines, ~25 exported functions
;;; Module theory over rings: generalizations of vector spaces where
;;; scalars come from a ring (not necessarily a field). Submodules,
;;; homomorphisms, quotient modules, tensor products. Used for
;;; Diophantine systems, homological algebra, representation theory.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/algebra/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement every?
  ;; ──────────────────────────────────────────────
  ;; Universal predicate check.

  (task
    (id "algebra/module-theory/every-pred-impl")
    (module module-theory)
    (tier 0)
    (type implement)
    (description "Implement every?: check whether a predicate holds for every element in a list. Return #t if the list is empty (vacuous truth) or if pred returns true for all elements. This is the universal quantifier over lists. Use short-circuit evaluation: return #f as soon as any element fails the predicate. Recurse on the tail, combining with and.")
    (context "Available: null?, and, or, car, cdr. Simple recursion with short-circuit.")
    (before "(define (every? pred lst)
  ;; TODO: #t iff pred holds for all elements
  #t)")
    (test-file "lattice/algebra/test-module.ss")
    (test-forms (";; All even
(assert-true (every? even? '(2 4 6)))"
                 ";; Not all even
(assert-false (every? even? '(2 3 6)))"
                 ";; Empty list: vacuously true
(assert-true (every? even? '()))"
                 ";; Single element
(assert-true (every? positive? '(5)))"
                 ";; Single failing element
(assert-false (every? positive? '(-1)))"))
    (available-modules (prelude ring))
    (hints ("Base: (null? lst) → #t"
            "Step: (and (pred (car lst)) (every? pred (cdr lst)))"
            "Short-circuit: and stops at first #f"))
    (reference-solution "(define (every? pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (every? pred (cdr lst)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement cartesian-product
  ;; ──────────────────────────────────────────────
  ;; All pairs from two lists.

  (task
    (id "algebra/module-theory/cartesian-product-impl")
    (module module-theory)
    (tier 0)
    (type implement)
    (description "Implement cartesian-product: compute all pairs (a . b) where a is from the first list and b is from the second list. For lists of length m and n, the result has m×n pairs. Use append-map over the first list: for each element a, map over the second list creating (cons a b) for each b. This is fundamental for enumerating tensor product bases and testing module properties.")
    (context "Available: append-map, map, cons, lambda. One append-map wrapping one map.")
    (before "(define (cartesian-product as bs)
  ;; TODO: all (a . b) pairs
  '())")
    (test-file "lattice/algebra/test-module.ss")
    (test-forms (";; 2×2 = 4 pairs
(assert-equal '((a . 1) (a . 2) (b . 1) (b . 2))
              (cartesian-product '(a b) '(1 2)))"
                 ";; Empty first list
(assert-equal '() (cartesian-product '() '(1 2 3)))"
                 ";; Empty second list
(assert-equal '() (cartesian-product '(a b) '()))"
                 ";; 1×3 = 3 pairs
(assert-equal '((x . 1) (x . 2) (x . 3))
              (cartesian-product '(x) '(1 2 3)))"))
    (available-modules (prelude ring))
    (hints ("Outer: append-map over as"
            "Inner: map over bs, creating (cons a b)"
            "(append-map (lambda (a) (map (lambda (b) (cons a b)) bs)) as)"))
    (reference-solution "(define (cartesian-product as bs)
  (append-map (lambda (a)
                (map (lambda (b) (cons a b)) bs))
              as))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement module-sum
  ;; ──────────────────────────────────────────────
  ;; Sum of module elements.

  (task
    (id "algebra/module-theory/module-sum-impl")
    (module module-theory)
    (tier 0)
    (type implement)
    (description "Implement module-sum: compute the sum of a list of module elements using the module's addition operation. If the list is empty, return the module's zero element. Otherwise, fold through the list, accumulating with module-add. A module provides (module-add-op mod) for addition and (module-zero mod) for the identity element. Use a named let loop starting with the first element as accumulator.")
    (context "Available: module-add, module-zero, null?, car, cdr, let. Named let accumulator loop.")
    (before "(define (module-sum mod elements)
  ;; TODO: sum all elements using module addition
  (module-zero mod))")
    (test-file "lattice/algebra/test-module.ss")
    (test-forms (";; Sum in Z_5^3: (1,0,0) + (0,2,0) + (0,0,3) = (1,2,3)
(let ([mod (make-free-module-zn 5 3)])
  (assert-equal '(1 2 3)
    (module-sum mod '((1 0 0) (0 2 0) (0 0 3)))))"
                 ";; Empty sum is zero
(let ([mod (make-free-module-zn 5 3)])
  (assert-equal '(0 0 0) (module-sum mod '())))"
                 ";; Single element
(let ([mod (make-free-module-zn 5 3)])
  (assert-equal '(3 1 4) (module-sum mod '((3 1 4)))))"))
    (available-modules (prelude ring))
    (hints ("Guard: (null? elements) → (module-zero mod)"
            "Named let: (let loop ([elems (cdr elements)] [acc (car elements)]) ...)"
            "Step: (loop (cdr elems) (module-add mod acc (car elems)))"))
    (reference-solution "(define (module-sum mod elements)
  (if (null? elements)
      (module-zero mod)
      (let loop ([elems (cdr elements)] [acc (car elements)])
        (if (null? elems)
            acc
            (loop (cdr elems) (module-add mod acc (car elems)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement module-linear-combination
  ;; ──────────────────────────────────────────────
  ;; Compute r₁·e₁ + r₂·e₂ + ... + rₙ·eₙ.

  (task
    (id "algebra/module-theory/linear-combination-impl")
    (module module-theory)
    (tier 0)
    (type implement)
    (description "Implement module-linear-combination: compute r₁·e₁ + r₂·e₂ + ... + rₙ·eₙ where rᵢ are ring scalars and eᵢ are module elements. This is the fundamental operation of module theory — it defines the span of a set of generators. Start with zero, then for each (scalar, element) pair, add scalar*element to the accumulator using module-smul and module-add. Process scalars and elements lists in parallel.")
    (context "Available: module-add, module-smul, module-zero, null?, or, car, cdr, let. Named let with parallel traversal.")
    (before "(define (module-linear-combination mod scalars elements)
  ;; TODO: r1*e1 + r2*e2 + ... + rn*en
  (module-zero mod))")
    (test-file "lattice/algebra/test-module.ss")
    (test-forms (";; 2*(1,0,0) + 3*(0,1,0) = (2,3,0) in Z_5^3
(let ([mod (make-free-module-zn 5 3)])
  (assert-equal '(2 3 0)
    (module-linear-combination mod '(2 3) '((1 0 0) (0 1 0)))))"
                 ";; Empty combination is zero
(let ([mod (make-free-module-zn 5 3)])
  (assert-equal '(0 0 0)
    (module-linear-combination mod '() '())))"
                 ";; With wrap-around: 4*(2,0,0) = (8 mod 5, 0, 0) = (3,0,0)
(let ([mod (make-free-module-zn 5 3)])
  (assert-equal '(3 0 0)
    (module-linear-combination mod '(4) '((2 0 0)))))"))
    (available-modules (prelude ring))
    (hints ("Guard: (or (null? scalars) (null? elements)) → (module-zero mod)"
            "Named let: (let loop ([rs scalars] [es elements] [acc (module-zero mod)]) ...)"
            "Step: (module-add mod acc (module-smul mod (car rs) (car es)))"))
    (reference-solution "(define (module-linear-combination mod scalars elements)
  (if (or (null? scalars) (null? elements))
      (module-zero mod)
      (let loop ([rs scalars] [es elements] [acc (module-zero mod)])
        (if (or (null? rs) (null? es))
            acc
            (loop (cdr rs) (cdr es)
                  (module-add mod acc (module-smul mod (car rs) (car es))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement quotient-classes
  ;; ──────────────────────────────────────────────
  ;; Compute equivalence class representatives.

  (task
    (id "algebra/module-theory/quotient-classes-impl")
    (module module-theory)
    (tier 0)
    (type implement)
    (description "Implement quotient-classes: given a list of elements and an equivalence relation, return one representative from each equivalence class. Walk through the elements, maintaining a list of representatives seen so far. For each element, check if it's equivalent to any existing representative (using any?). If not, add it as a new representative. Return representatives in order of first appearance. This is used to compute cosets for quotient modules.")
    (context "Available: null?, any?, car, cdr, cons, reverse, let, if, lambda. Named let with representative accumulator.")
    (before "(define (quotient-classes elements equiv?)
  ;; TODO: one rep per equivalence class
  '())")
    (test-file "lattice/algebra/test-module.ss")
    (test-forms (";; Mod 3 equivalence on integers
(let ([result (quotient-classes '(0 1 2 3 4 5) (lambda (a b) (= (modulo a 3) (modulo b 3))))])
  (assert-equal 3 (length result))
  ;; First representatives should be 0, 1, 2
  (assert-equal '(0 1 2) result))"
                 ";; All equivalent → single class
(assert-equal '(a) (quotient-classes '(a b c) (lambda (x y) #t)))"
                 ";; All distinct → all are representatives
(assert-equal '(1 2 3) (quotient-classes '(1 2 3) (lambda (x y) #f)))"
                 ";; Empty list
(assert-equal '() (quotient-classes '() equal?))"))
    (available-modules (prelude ring))
    (hints ("Named let: (let loop ([elems elements] [reps '()]) ...)"
            "Check: (any? (lambda (r) (equiv? e r)) reps)"
            "If equivalent to existing rep → skip, else → (cons e reps)"
            "Return (reverse reps) at end"))
    (reference-solution "(define (quotient-classes elements equiv?)
  (let loop ([elems elements] [reps '()])
    (if (null? elems)
        (reverse reps)
        (let ([e (car elems)])
          (if (any? (lambda (r) (equiv? e r)) reps)
              (loop (cdr elems) reps)
              (loop (cdr elems) (cons e reps)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
