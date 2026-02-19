;;; set.sexp — Development tasks for lattice/data/set.ss
;;;
;;; Module: set (tier 0, depends only on prelude)
;;; 111 lines, ~12 exported functions
;;; Simple list-based immutable set (no duplicates).
;;;
;;; No test file exists.
;;;
;;; The interesting angle vs dict: sets have algebraic laws
;;; (commutativity, associativity, De Morgan's, idempotency)
;;; that can serve as verification properties.

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement set-intersection
  ;; ──────────────────────────────────────────────
  ;; Classic set operation. Walk set1, keep elements that also
  ;; appear in set2. O(n*m) with the list representation.

  (task
    (id "data/set/intersection-impl")
    (module set)
    (tier 0)
    (type implement)
    (description "Implement set-intersection: return a new set containing only elements that appear in BOTH set1 and set2. Walk set1, test each element against set2 using set-member?, accumulate matches. Preserve order from set1.")
    (context "Available: prelude, set-member? (already defined: tests if an element is in a set using equal?).")
    (before "(define (set-intersection set1 set2)
  (doc 'type (-> Set Set Set))
  (doc 'description \"Intersection of two sets\")
  ;; TODO: keep elements of set1 that are also in set2
  set-empty)")
    (test-file #f)
    (test-forms ("(assert-equal '(2 3) (set-intersection '(1 2 3) '(2 3 4)))"
                 "(assert-equal '() (set-intersection '(1 2) '(3 4)))"
                 "(assert-equal '(1 2 3) (set-intersection '(1 2 3) '(1 2 3)))"))
    (available-modules (prelude))
    (hints ("Use a tail-recursive loop with an accumulator"
            "For each element of set1: if (set-member? elem set2), include it"
            "Reverse the accumulator at the end"))
    (reference-solution "(define (set-intersection set1 set2)
  (let loop ([remaining set1]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining) (cons (car remaining) acc))]
        [else (loop (cdr remaining) acc)])))")
    (alternatives ("(filter (lambda (x) (set-member? x set2)) set1)"))
    (source-commit "f00e3d4d")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement list->set (deduplication)
  ;; ──────────────────────────────────────────────
  ;; Convert a list with potential duplicates into a set.
  ;; Uses set-add which checks membership before inserting.

  (task
    (id "data/set/list-to-set-impl")
    (module set)
    (tier 0)
    (type implement)
    (description "Implement list->set: convert a list (which may contain duplicates) into a set (no duplicates). Walk the list, using set-add to build up the set. set-add already handles the duplicate check. Note: the resulting set will have elements in reverse order of first appearance.")
    (context "Available: prelude, set-empty (the empty set, which is '()), set-add (adds element to set, skipping if already present).")
    (before "(define (list->set lst)
  (doc 'type (-> (List a) Set))
  (doc 'description \"Convert list to set (removes duplicates)\")
  ;; TODO: fold over the list, using set-add to accumulate
  lst)")
    (test-file #f)
    (test-forms ("(assert-equal 3 (set-size (list->set '(1 2 3 2 1))))"
                 "(assert-true (set-member? 'a (list->set '(a b a c b))))"
                 "(assert-equal 0 (set-size (list->set '())))"))
    (available-modules (prelude))
    (hints ("Iterate through the list, accumulating into a set starting from set-empty"
            "set-add handles the membership check — you just need the loop"))
    (reference-solution "(define (list->set lst)
  (let loop ([remaining lst]
             [acc set-empty])
       (if (null? remaining)
           acc
           (loop (cdr remaining) (set-add (car remaining) acc)))))")
    (alternatives ("(fold-left (lambda (s x) (set-add x s)) set-empty lst)"))
    (source-commit "f00e3d4d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Extend with set-equal?
  ;; ──────────────────────────────────────────────
  ;; Two sets are equal if each is a subset of the other.
  ;; Can't use equal? directly because list order may differ.

  (task
    (id "data/set/set-equal-extend")
    (module set)
    (tier 0)
    (type extend)
    (description "Add a set-equal? function that checks if two sets contain exactly the same elements. You cannot use equal? directly because sets are represented as lists and element order is not guaranteed (e.g., (set-add 'b (set-add 'a set-empty)) and (set-add 'a (set-add 'b set-empty)) are equal sets but different lists). Use the mathematical definition: A = B iff A is a subset of B AND B is a subset of A.")
    (context "Available: prelude, set-subset? (already defined: checks if every element of set1 is in set2), set-size.")
    (before ";; Add to set.ss:
;; set-equal? is not currently in the module.
;; Two sets are equal when they have the same elements regardless of list order.")
    (test-file #f)
    (test-forms ("(assert-true (set-equal? (list->set '(1 2 3)) (list->set '(3 2 1))))"
                 "(assert-false (set-equal? (list->set '(1 2)) (list->set '(1 2 3))))"
                 "(assert-true (set-equal? set-empty set-empty))"))
    (available-modules (prelude))
    (hints ("A = B iff (set-subset? A B) AND (set-subset? B A)"
            "Optionally check sizes first as a fast path"))
    (reference-solution "(define (set-equal? set1 set2)
  (and (= (set-size set1) (set-size set2))
       (set-subset? set1 set2)))")
    (alternatives ("(and (set-subset? set1 set2) (set-subset? set2 set1))"))
    (source-commit #f)
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Write tests with algebraic laws
  ;; ──────────────────────────────────────────────
  ;; Sets satisfy algebraic laws that serve as strong verification.
  ;; Testing these laws exercises deeper understanding than just
  ;; testing individual functions.

  (task
    (id "data/set/algebraic-law-tests")
    (module set)
    (tier 0)
    (type test)
    (description "Write tests for the set module that verify algebraic laws. Beyond basic CRUD tests, verify: (1) union is commutative: A ∪ B has same elements as B ∪ A, (2) intersection distributes over union: A ∩ (B ∪ C) has same elements as (A ∩ B) ∪ (A ∩ C), (3) De Morgan's law: complement of union equals intersection of complements (using set-difference from a universe set U), (4) idempotency: A ∪ A = A, A ∩ A = A, (5) empty set identity: A ∪ ∅ = A, A ∩ ∅ = ∅. Use set-equal? (defined above) or set-subset? checks to compare sets regardless of list order.")
    (context "Available: full set module plus set-equal? if it was added. Since sets are lists and order may differ, use set-subset? in both directions to check equality, or compare sorted lists.")
    (before ";; No test file exists for set.ss.
;; Write tests that verify algebraic laws, not just individual function behavior.")
    (test-file #f)
    (test-forms ())
    (available-modules (prelude set))
    (hints ("Define helper: (define (sets-equal? a b) (and (set-subset? a b) (set-subset? b a)))"
            "For De Morgan's: define U = {1,2,3,4,5}, A = {1,2,3}, B = {3,4,5}. Then complement(A ∪ B) w.r.t. U should equal complement(A) ∩ complement(B)"
            "Test edge cases: empty sets, single-element sets, identical sets"))
    (reference-solution "(define (sets-equal? a b) (and (set-subset? a b) (set-subset? b a)))

;; Commutativity of union
(let ([a (list->set '(1 2 3))] [b (list->set '(3 4 5))])
  (assert-true (sets-equal? (set-union a b) (set-union b a))))

;; Idempotency
(let ([a (list->set '(1 2 3))])
  (assert-true (sets-equal? (set-union a a) a))
  (assert-true (sets-equal? (set-intersection a a) a)))

;; Identity
(let ([a (list->set '(1 2 3))])
  (assert-true (sets-equal? (set-union a set-empty) a))
  (assert-true (set-empty? (set-intersection a set-empty))))

;; De Morgan's
(let ([u (list->set '(1 2 3 4 5))]
      [a (list->set '(1 2 3))]
      [b (list->set '(3 4 5))])
  (let ([comp-union (set-difference u (set-union a b))]
        [inter-comps (set-intersection (set-difference u a) (set-difference u b))])
    (assert-true (sets-equal? comp-union inter-comps))))")
    (alternatives ())
    (source-commit #f)
    (difficulty 3))

) ;; end tasks
