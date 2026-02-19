;;; groebner.sexp — Development tasks for lattice/algebra/groebner.ss
;;;
;;; Module: groebner (tier 0, depends on prelude, multivariate)
;;; 303 lines, ~15 exported functions
;;; Gröbner bases: S-polynomial computation, polynomial reduction,
;;; Buchberger's algorithm with pair elimination criteria,
;;; reduced Gröbner bases, ideal membership testing.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/algebra/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement all-pairs
  ;; ──────────────────────────────────────────────
  ;; Generate all index pairs (i,j) with i < j.

  (task
    (id "algebra/groebner/all-pairs-impl")
    (module groebner)
    (tier 0)
    (type implement)
    (description "Implement all-pairs: generate all pairs (i,j) with 0 <= i < j < n. This is a fundamental combinatorial utility used in Buchberger's algorithm to enumerate all polynomial pairs whose S-polynomials must be checked. Use two nested named let loops: the outer iterates i from 0 to n-2, the inner iterates j from i+1 to n-1. Accumulate pairs as (cons i j) dotted pairs, collecting into a result list that is reversed at the end to maintain order.")
    (context "Available: >=, -, +, cons, reverse, let. Two nested named let loops.")
    (before "(define (all-pairs n)
  ;; TODO: all (i,j) with 0 <= i < j < n
  '())")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; 4 elements: 6 pairs
(assert-equal '((0 . 1) (0 . 2) (0 . 3) (1 . 2) (1 . 3) (2 . 3))
  (all-pairs 4))"
                 ";; 2 elements: 1 pair
(assert-equal '((0 . 1)) (all-pairs 2))"
                 ";; 1 element: no pairs
(assert-equal '() (all-pairs 1))"
                 ";; 0 elements: no pairs
(assert-equal '() (all-pairs 0))"))
    (available-modules (prelude multivariate))
    (hints ("Outer loop: (let outer ([i 0] [result '()]) ...)"
            "Inner loop: (let inner ([j (+ i 1)] [acc result]) ...)"
            "Outer terminates when (>= i (- n 1)), returning (reverse result)"
            "Inner terminates when (>= j n), advancing to (outer (+ i 1) acc)"
            "Each inner step: (inner (+ j 1) (cons (cons i j) acc))"))
    (reference-solution "(define (all-pairs n)
  (let outer ([i 0] [result '()])
    (if (>= i (- n 1))
        (reverse result)
        (let inner ([j (+ i 1)] [acc result])
          (if (>= j n)
              (outer (+ i 1) acc)
              (inner (+ j 1) (cons (cons i j) acc)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement any
  ;; ──────────────────────────────────────────────
  ;; Check if any element satisfies a predicate.

  (task
    (id "algebra/groebner/any-impl")
    (module groebner)
    (tier 0)
    (type implement)
    (description "Implement any: check if any element in a list satisfies a predicate. Return #t as soon as a matching element is found (short-circuit evaluation), or #f if none match. This is a standard higher-order function used throughout Buchberger's algorithm — for example, to check if any basis element's leading monomial divides a given monomial. Use a cond with three cases: empty list returns #f, predicate matches car returns #t, otherwise recurse on cdr.")
    (context "Available: null?, car, cdr, cond. Simple recursive walk with short-circuit.")
    (before "(define (any pred lst)
  ;; TODO: does any element satisfy pred?
  #f)")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; Found: element > 3 exists
(assert-true (any (lambda (x) (> x 3)) '(1 2 5)))"
                 ";; Not found: no element > 3
(assert-false (any (lambda (x) (> x 3)) '(1 2 3)))"
                 ";; Empty list
(assert-false (any (lambda (x) #t) '()))"
                 ";; First element matches
(assert-true (any (lambda (x) (= x 1)) '(1 2 3)))"))
    (available-modules (prelude multivariate))
    (hints ("Base: (null? lst) -> #f"
            "Match: (pred (car lst)) -> #t"
            "Recurse: (any pred (cdr lst))"
            "Short-circuit: returns as soon as pred is satisfied"))
    (reference-solution "(define (any pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement remove-equal
  ;; ──────────────────────────────────────────────
  ;; Remove all elements equal to x from a list.

  (task
    (id "algebra/groebner/remove-equal-impl")
    (module groebner)
    (tier 0)
    (type implement)
    (description "Implement remove-equal: remove all elements from a list that are equal to x under a given equality function eq-fn. This is used in the minimize-basis step of Gröbner basis reduction — when checking if a polynomial's leading monomial is divisible by others, we need to exclude the polynomial itself from the list. Use filter with a lambda that negates the equality test.")
    (context "Available: filter, not, lambda. Single filter call.")
    (before "(define (remove-equal x lst eq-fn)
  ;; TODO: remove elements equal to x
  lst)")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; Remove 3 from list (using = for numbers)
(assert-equal '(1 2 4 5) (remove-equal 3 '(1 2 3 4 3 5) =))"
                 ";; Remove from empty list
(assert-equal '() (remove-equal 1 '() =))"
                 ";; Element not present
(assert-equal '(1 2 3) (remove-equal 4 '(1 2 3) =))"
                 ";; Remove all occurrences
(assert-equal '() (remove-equal 1 '(1 1 1) =))"))
    (available-modules (prelude multivariate))
    (hints ("Use filter to keep elements that are NOT equal to x"
            "(filter (lambda (y) (not (eq-fn x y))) lst)"
            "The equality function is passed as a parameter for generality"))
    (reference-solution "(define (remove-equal x lst eq-fn)
  (filter (lambda (y) (not (eq-fn x y))) lst))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement buchberger-criterion-1?
  ;; ──────────────────────────────────────────────
  ;; First Buchberger criterion for pair elimination.

  (task
    (id "algebra/groebner/buchberger-criterion-1-impl")
    (module groebner)
    (tier 0)
    (type implement)
    (description "Implement buchberger-criterion-1?: check if two polynomials' leading monomials are coprime (share no common variables). If they are coprime, their S-polynomial is guaranteed to reduce to zero, so the pair can be skipped in Buchberger's algorithm — a critical optimization. Two monomials are coprime when their GCD is the constant monomial 1 (represented as the empty list). Extract the leading monomials with mpoly-leading-mono, compute their GCD with mono-gcd, and check if the result is null.")
    (context "Available: mpoly-leading-mono, mono-gcd, null?, let. Extract + GCD + null check.")
    (before "(define (buchberger-criterion-1? f g)
  ;; TODO: are leading monomials coprime?
  #f)")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; x and y are coprime (no shared variables)
(let* ([F Q-field]
       [ord (make-ordering 'lex '(x y))]
       [px (mpoly-var F '(x y) ord 'x)]
       [py (mpoly-var F '(x y) ord 'y)])
  (assert-true (buchberger-criterion-1? px py)))"
                 ";; x^2 and x*y share variable x — not coprime
(let* ([F Q-field]
       [ord (make-ordering 'lex '(x y))]
       [px (mpoly-var F '(x y) ord 'x)]
       [py (mpoly-var F '(x y) ord 'y)]
       [px2 (mpoly-mul px px)]
       [pxy (mpoly-mul px py)])
  (assert-false (buchberger-criterion-1? px2 pxy)))"))
    (available-modules (prelude multivariate))
    (hints ("Extract leading monomials: (let ([lm-f (mpoly-leading-mono f)] [lm-g (mpoly-leading-mono g)]) ...)"
            "Compute GCD: (mono-gcd lm-f lm-g)"
            "Coprime means GCD = 1, which is the empty monomial: (null? (mono-gcd lm-f lm-g))"))
    (reference-solution "(define (buchberger-criterion-1? f g)
  (let ([lm-f (mpoly-leading-mono f)]
        [lm-g (mpoly-leading-mono g)])
    (null? (mono-gcd lm-f lm-g))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement ideal-membership?
  ;; ──────────────────────────────────────────────
  ;; Test if a polynomial is in an ideal.

  (task
    (id "algebra/groebner/ideal-membership-impl")
    (module groebner)
    (tier 0)
    (type implement)
    (description "Implement ideal-membership?: test whether a polynomial f belongs to the ideal generated by a set of polynomials G. When G is a Gröbner basis, this is equivalent to checking whether f reduces to zero modulo G. This is one of the key applications of Gröbner bases — the ideal membership problem is decidable precisely because Gröbner bases provide a canonical form. Use reduce-poly to compute the remainder of f divided by G, then check if the remainder is zero with mpoly-zero?.")
    (context "Available: reduce-poly, mpoly-zero?, let. Reduce + zero check.")
    (before "(define (ideal-membership? f G)
  ;; TODO: does f belong to the ideal <G>?
  #f)")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; x is in the ideal <x>
(let* ([F Q-field]
       [ord (make-ordering 'lex '(x y))]
       [px (mpoly-var F '(x y) ord 'x)])
  (assert-true (ideal-membership? px (list px))))"
                 ";; y is NOT in the ideal <x>
(let* ([F Q-field]
       [ord (make-ordering 'lex '(x y))]
       [px (mpoly-var F '(x y) ord 'x)]
       [py (mpoly-var F '(x y) ord 'y)])
  (assert-false (ideal-membership? py (list px))))"
                 ";; x^2 is in the ideal <x>
(let* ([F Q-field]
       [ord (make-ordering 'lex '(x y))]
       [px (mpoly-var F '(x y) ord 'x)]
       [px2 (mpoly-mul px px)])
  (assert-true (ideal-membership? px2 (list px))))"))
    (available-modules (prelude multivariate))
    (hints ("Reduce f modulo G: (let ([r (reduce-poly f G)]) ...)"
            "Check if remainder is zero: (mpoly-zero? r)"
            "This works because G is a Gröbner basis — remainder is unique"))
    (reference-solution "(define (ideal-membership? f G)
  (let ([r (reduce-poly f G)])
    (mpoly-zero? r)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
