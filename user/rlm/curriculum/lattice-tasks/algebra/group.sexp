;;; group.sexp — Development tasks for lattice/algebra/group.ss
;;;
;;; Module: group (tier 0, depends only on prelude)
;;; 648 lines, ~30 exported functions
;;; Provides: cyclic groups (Z_n), symmetric groups (S_n), dihedral groups (D_n),
;;;           subgroup testing, homomorphisms, kernel/image, Cayley tables
;;;
;;; Test file: lattice/algebra/test-group.ss (303 lines, 30 tests)
;;;
;;; Git history:
;;;   d83deba2 feat(module): migrate lattice/algebra/ to require
;;;   ea298e02 refactor: Consolidate filter, ormap, fold-left to use prelude
;;;   555b079a docs(lattice): Convert topology, sim, algebra, diffgeo to (doc ...) forms

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement element-order
  ;; ──────────────────────────────────────────────
  ;; Fundamental group theory concept: the smallest positive n
  ;; such that a^n = e. Requires composing group operations
  ;; iteratively and understanding the identity element.

  (task
    (id "algebra/group/element-order-impl")
    (module group)
    (tier 0)
    (type implement)
    (description "Implement element-order: given a group G and an element a, find the smallest positive integer n such that a^n = e (the identity element). If n exceeds the group's order, return #f. You must iterate by repeatedly composing with a using group-compose, and check equality to identity using group-equal?.")
    (context "Available: group-compose (binary op), group-identity (identity element), group-equal? (equality test), group-order (number of elements). The group is finite, so the order always exists and divides |G| by Lagrange's theorem.")
    (before "(define (element-order g a)
  ;; TODO: implement
  ;; Find smallest positive n with a^n = e
  #f)")
    (test-file "lattice/algebra/test-group.ss")
    (test-forms ("(assert-equal 6 (element-order (Z 6) 1))"
                 "(assert-equal 3 (element-order (Z 6) 2))"
                 "(assert-equal 2 (element-order (Z 6) 3))"
                 "(assert-equal 1 (element-order (Z 6) 0))"
                 "(assert-equal 5 (element-order (Z 5) 1))"))
    (available-modules (prelude group))
    (hints ("Start with current = a, n = 1"
            "Each step: current = group-compose g current a, n = n + 1"
            "Stop when group-equal? g current (group-identity g)"))
    (reference-solution "(define (element-order g a)
  (let loop ([n 1] [current a])
    (if (> n (group-order g))
        #f
        (if (group-equal? g current (group-identity g))
            n
            (loop (+ n 1) (group-compose g current a))))))")
    (alternatives ())
    (source-commit "d83deba2")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement permutation-inverse
  ;; ──────────────────────────────────────────────
  ;; If perm maps i → perm[i], then the inverse maps perm[i] → i.
  ;; Requires building a new list where result[perm[i]] = i.
  ;; The functional list-set pattern (no mutation) is key.

  (task
    (id "algebra/group/permutation-inverse-impl")
    (module group)
    (tier 0)
    (type implement)
    (description "Implement permutation-inverse: given a permutation (a list where position i maps to value perm[i]), compute the inverse permutation. The inverse satisfies: if perm[i] = j, then inv[j] = i. That is, composing a permutation with its inverse gives the identity. Build the result using a functional list-set (no mutation): start with a list of zeros, then for each index i, set result[perm[i]] = i.")
    (context "Available: length, list-ref, make-list, iota. A permutation of n elements is a list of n integers (0..n-1) in some order. The helper list-set is defined in the module: (list-set lst idx val) returns a new list with lst[idx] replaced by val.")
    (before "(define (permutation-inverse perm)
  ;; TODO: implement
  ;; Build inv such that (compose perm inv) = identity
  perm)")
    (test-file "lattice/algebra/test-group.ss")
    (test-forms ("(assert-equal '(2 0 1) (permutation-inverse '(1 2 0)))"
                 "(assert-equal '(0 1 2) (permutation-inverse '(0 1 2)))"
                 "(assert-equal '(1 0 2) (permutation-inverse '(1 0 2)))"
                 "(assert-equal '(0 1 2) (permutation-compose '(1 2 0) (permutation-inverse '(1 2 0))))"))
    (available-modules (prelude group))
    (hints ("Start with result = (make-list n 0)"
            "For each i from 0 to n-1, do result[perm[i]] = i"
            "Use the functional list-set to build each intermediate list"))
    (reference-solution "(define (permutation-inverse perm)
  (let* ([n (length perm)]
         [result (make-list n 0)])
    (let loop ([i 0] [res result])
      (if (= i n)
          res
          (loop (+ i 1)
                (list-set res (list-ref perm i) i))))))")
    (alternatives ())
    (source-commit "d83deba2")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Fix buggy dihedral group operation
  ;; ──────────────────────────────────────────────
  ;; The dihedral group operation has four cases based on element types
  ;; (rotation or reflection). The tricky part is the sign in the
  ;; mixed cases: sr^a * r^b = sr^(a+b), but r^a * sr^b = sr^(b-a).
  ;; A common error is swapping the sign in one case.

  (task
    (id "algebra/group/fix-dihedral-op")
    (module group)
    (tier 0)
    (type fix)
    (description "The dihedral group D_n operation has a bug. Dihedral elements are pairs: (r . k) for rotation r^k and (s . k) for reflection sr^k. The operation rules are: r^a * r^b = r^(a+b mod n); r^a * sr^b = sr^(b-a mod n); sr^a * r^b = sr^(a+b mod n); sr^a * sr^b = r^(b-a mod n). The bug is in the r*s case: it uses (+ b-k a-k) instead of (- b-k a-k). Fix the dihedral-op function.")
    (context "Available: modulo, cons, car, cdr, eq?. Elements are pairs: '(r . 0) is the identity, '(s . 0) is a reflection. The key relation is sr = r^(-1)s, which means r^a * sr^b = sr^(b-a).")
    (before "(define (dihedral-op n)
  (lambda (a b)
    (let ([a-type (car a)] [a-k (cdr a)]
          [b-type (car b)] [b-k (cdr b)])
      (cond
        [(and (eq? a-type 'r) (eq? b-type 'r))
         (cons 'r (modulo (+ a-k b-k) n))]
        [(and (eq? a-type 'r) (eq? b-type 's))
         (cons 's (modulo (+ b-k a-k) n))]  ;; BUG: should be (- b-k a-k)
        [(and (eq? a-type 's) (eq? b-type 'r))
         (cons 's (modulo (+ a-k b-k) n))]
        [(and (eq? a-type 's) (eq? b-type 's))
         (cons 'r (modulo (- b-k a-k) n))]))))")
    (test-file "lattice/algebra/test-group.ss")
    (test-forms ("(let ([op (dihedral-op 4)]) (assert-equal (cons 'r 3) (op (cons 'r 1) (cons 'r 2))))"
                 "(let ([op (dihedral-op 4)]) (assert-equal (cons 's 1) (op (cons 'r 1) (cons 's 2))))"
                 "(let ([op (dihedral-op 4)]) (assert-equal (cons 's 3) (op (cons 's 1) (cons 'r 2))))"
                 "(let ([op (dihedral-op 4)]) (assert-equal (cons 'r 1) (op (cons 's 1) (cons 's 2))))"
                 "(assert-true (verify-group-axioms (D 3)))"))
    (available-modules (prelude group))
    (hints ("Check the multiplication table for dihedral groups"
            "The relation sr = r^(-1)s tells you that r^a * sr^b = sr^(b-a mod n)"
            "The bug is in the r*s case: it adds instead of subtracting"))
    (reference-solution "(define (dihedral-op n)
  (lambda (a b)
    (let ([a-type (car a)] [a-k (cdr a)]
          [b-type (car b)] [b-k (cdr b)])
      (cond
        [(and (eq? a-type 'r) (eq? b-type 'r))
         (cons 'r (modulo (+ a-k b-k) n))]
        [(and (eq? a-type 'r) (eq? b-type 's))
         (cons 's (modulo (- b-k a-k) n))]
        [(and (eq? a-type 's) (eq? b-type 'r))
         (cons 's (modulo (+ a-k b-k) n))]
        [(and (eq? a-type 's) (eq? b-type 's))
         (cons 'r (modulo (- b-k a-k) n))]))))")
    (alternatives ())
    (source-commit "d83deba2")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Extend with is-abelian?
  ;; ──────────────────────────────────────────────
  ;; Abelian groups have commutative operations: a*b = b*a for all a,b.
  ;; Tests composition with existing group infrastructure.

  (task
    (id "algebra/group/is-abelian-extend")
    (module group)
    (tier 0)
    (type extend)
    (description "Add a function is-abelian? that checks whether a group is abelian (commutative). A group is abelian if for every pair of elements a, b: group-compose g a b equals group-compose g b a. Check all pairs of elements in the group. Return #t if all pairs commute, #f if any pair doesn't.")
    (context "Available: group-elements (list of all elements), group-compose (binary op), group-equal? (equality). You need to check all pairs (a, b) from group-elements.")
    (before ";; is-abelian? does not yet exist in the module")
    (test-file "lattice/algebra/test-group.ss")
    (test-forms ("(assert-true (is-abelian? (Z 5)))"
                 "(assert-true (is-abelian? (Z 12)))"
                 "(assert-false (is-abelian? (S 3)))"
                 "(assert-true (is-abelian? (D 1)))"
                 "(assert-false (is-abelian? (D 3)))"))
    (available-modules (prelude group))
    (hints ("Iterate over all pairs of elements"
            "For each pair (a, b), check group-equal? g (group-compose g a b) (group-compose g b a)"
            "Return #f as soon as you find a non-commuting pair"))
    (reference-solution "(define (is-abelian? g)
  (let ([elems (group-elements g)])
    (let loop-a ([as elems])
      (if (null? as)
          #t
          (let loop-b ([bs elems])
            (if (null? bs)
                (loop-a (cdr as))
                (if (group-equal? g
                      (group-compose g (car as) (car bs))
                      (group-compose g (car bs) (car as)))
                    (loop-b (cdr bs))
                    #f)))))))")
    (alternatives ())
    (source-commit "d83deba2")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement group-power
  ;; ──────────────────────────────────────────────
  ;; Repeated squaring is a classic algorithmic pattern. The group
  ;; version must also handle negative exponents via inverse.

  (task
    (id "algebra/group/group-power-impl")
    (module group)
    (tier 0)
    (type implement)
    (description "Implement group-power: compute a^n in a group G using repeated squaring. For n=0, return the identity. For negative n, compute (inverse of a)^|n|. For positive n, use the standard repeated squaring: if n is even, compute (a^(n/2))^2; if odd, compute a * a^(n-1). Use group-compose for the binary operation and group-inverse for inverses.")
    (context "Available: group-compose (binary op), group-identity (returns e), group-inverse (returns a^-1), even?, =, <, /. This is the standard square-and-multiply algorithm adapted for abstract groups.")
    (before "(define (group-power g a n)
  ;; TODO: implement using repeated squaring
  (group-identity g))")
    (test-file "lattice/algebra/test-group.ss")
    (test-forms ("(assert-equal 0 (group-power (Z 7) 3 0))"
                 "(assert-equal 3 (group-power (Z 7) 3 1))"
                 "(assert-equal 6 (group-power (Z 7) 3 2))"
                 "(assert-equal 2 (group-power (Z 7) 3 3))"
                 "(assert-equal 3 (group-power (Z 5) 2 -1))"
                 "(assert-equal 1 (group-power (Z 5) 2 -2))"))
    (available-modules (prelude group))
    (hints ("Handle three cases: n=0 → identity, n<0 → power of inverse, n>0 → squaring"
            "For even n: half = power(a, n/2), return compose(half, half)"
            "For odd n: return compose(a, power(a, n-1))"))
    (reference-solution "(define (group-power g a n)
  (cond
    [(= n 0) (group-identity g)]
    [(< n 0) (group-power g (group-inverse g a) (- n))]
    [(= n 1) a]
    [(even? n)
     (let ([half (group-power g a (/ n 2))])
       (group-compose g half half))]
    [else
     (group-compose g a (group-power g a (- n 1)))]))")
    (alternatives ())
    (source-commit "d83deba2")
    (difficulty 3))

) ;; end tasks
