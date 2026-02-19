;;; union-find.sexp — Development tasks for lattice/egraph/union-find.ss
;;;
;;; Module: egraph/union-find (tier 0, depends on prelude)
;;; 267 lines, ~12 exported functions
;;; Classic union-find with path compression and union by rank.
;;; Foundation for e-graph e-class management. Elements are integers 0..n-1.
;;; O(alpha(n)) amortized for find and union (inverse Ackermann).
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement uf-same-set?
  ;; ──────────────────────────────────────────────
  ;; Check equivalence — the simplest query.

  (task
    (id "egraph/union-find/uf-same-set-impl")
    (module egraph/union-find)
    (tier 0)
    (type implement)
    (description "Implement uf-same-set?: check if two elements are in the same equivalence class. Two elements are in the same set if and only if they have the same representative (root). Use uf-find to get the representative of each element and compare with =. This is the fundamental query operation on disjoint sets.")
    (context "Available: uf-find (returns representative of an element's set), =. One-line implementation.")
    (before "(define (uf-same-set? uf id1 id2)
  (doc 'export #t)
  ;; TODO: check if id1 and id2 have the same representative
  #f)")
    (test-file "lattice/egraph/test-union-find.ss")
    (test-forms (";; Distinct singletons are not in the same set
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)])
    (assert-false (uf-same-set? uf a b))))"
                 ";; After union, elements are in the same set
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)])
    (uf-union! uf a b)
    (assert-true (uf-same-set? uf a b))))"
                 ";; Element is in the same set as itself
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)])
    (assert-true (uf-same-set? uf a a))))"))
    (available-modules (prelude))
    (hints ("Get root1 = (uf-find uf id1)"
            "Get root2 = (uf-find uf id2)"
            "Return (= root1 root2)"))
    (reference-solution "(define (uf-same-set? uf id1 id2)
  (= (uf-find uf id1) (uf-find uf id2)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement uf-find with path compression
  ;; ──────────────────────────────────────────────
  ;; The core lookup with the key optimization.

  (task
    (id "egraph/union-find/uf-find-impl")
    (module egraph/union-find)
    (tier 0)
    (type implement)
    (description "Implement uf-find: find the representative (root) of the set containing a given element. The root is the element whose parent is itself. Use path compression: after finding the root, update every node on the path to point directly to the root. This flattens the tree and gives amortized O(alpha(n)) performance (inverse Ackermann, effectively constant). The parent array is accessed via (uf-parent uf) which returns a vector where parent[i] is the parent of element i. A root has parent[i] = i.")
    (context "Available: uf-parent (returns parent vector), vector-ref, vector-set!, =. Recursive find with mutation for path compression.")
    (before "(define (uf-find uf id)
  (doc 'export #t)
  ;; TODO: find root with path compression
  id)")
    (test-file "lattice/egraph/test-union-find.ss")
    (test-forms (";; Singleton: find returns self
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)])
    (assert-equal a (uf-find uf a))))"
                 ";; After union: both elements find same root
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)])
    (uf-union! uf a b)
    (assert-equal (uf-find uf a) (uf-find uf b))))"
                 ";; Path compression: chain of 4 elements all compress to root
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)]
        [c (uf-make-set! uf)]
        [d (uf-make-set! uf)])
    (uf-union! uf a b)
    (uf-union! uf b c)
    (uf-union! uf c d)
    (let ([root (uf-find uf d)])
      (assert-equal root (uf-find uf a))
      (assert-equal root (uf-find uf b))
      (assert-equal root (uf-find uf c)))))"))
    (available-modules (prelude))
    (hints ("Get parent vector: (uf-parent uf)"
            "Base case: if (vector-ref parent i) = i, then i is the root"
            "Recursive case: find root of parent, then set parent[i] = root (path compression)"
            "Use a named let loop with variable i starting at id"))
    (reference-solution "(define (uf-find uf id)
  (let ([parent (uf-parent uf)])
    (let loop ([i id])
      (let ([p (vector-ref parent i)])
        (if (= p i)
            i
            (let ([root (loop p)])
              (vector-set! parent i root)
              root))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement uf-union! with union by rank
  ;; ──────────────────────────────────────────────
  ;; Merge two sets efficiently.

  (task
    (id "egraph/union-find/uf-union-impl")
    (module egraph/union-find)
    (tier 0)
    (type implement)
    (description "Implement uf-union!: merge the sets containing two elements. Use union by rank to keep trees balanced: attach the shorter tree under the taller one. If ranks are equal, pick one as root and increment its rank. Steps: (1) Find roots of both elements. (2) If same root, already merged — return it. (3) Compare ranks. Attach the lower-rank root under the higher-rank root. If equal, attach root2 under root1 and increment root1's rank. (4) Decrement the set count (uf-count-box). Return the new root. The rank is accessed via (uf-rank uf) vector.")
    (context "Available: uf-find, uf-parent, uf-rank, uf-count-box, vector-ref, vector-set!, set-box!, unbox, =, <, >, -, +, cond. Mutation for parent/rank/count updates.")
    (before "(define (uf-union! uf id1 id2)
  (doc 'export #t)
  ;; TODO: merge sets with union by rank
  (uf-find uf id1))")
    (test-file "lattice/egraph/test-union-find.ss")
    (test-forms (";; Union merges two sets, count decreases
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)])
    (uf-union! uf a b)
    (assert-true (uf-same-set? uf a b))
    (assert-equal 1 (uf-count uf))))"
                 ";; Union returns the merged root
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)])
    (let ([root (uf-union! uf a b)])
      (assert-equal root (uf-find uf a))
      (assert-equal root (uf-find uf b)))))"
                 ";; Union is idempotent
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)])
    (uf-union! uf a b)
    (uf-union! uf a b)
    (assert-equal 1 (uf-count uf))))"
                 ";; Transitive: union(a,b) + union(b,c) → same-set(a,c)
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)]
        [c (uf-make-set! uf)])
    (uf-union! uf a b)
    (uf-union! uf b c)
    (assert-true (uf-same-set? uf a c))
    (assert-equal 1 (uf-count uf))))"))
    (available-modules (prelude))
    (hints ("Find both roots: root1 = (uf-find uf id1), root2 = (uf-find uf id2)"
            "If root1 = root2, return root1 (already same set)"
            "Get ranks: rank1 = (vector-ref (uf-rank uf) root1), etc."
            "Decrement count: (set-box! (uf-count-box uf) (- (unbox (uf-count-box uf)) 1))"
            "If rank1 < rank2: set parent[root1] = root2, return root2"
            "If rank1 > rank2: set parent[root2] = root1, return root1"
            "If equal: set parent[root2] = root1, increment rank[root1], return root1"))
    (reference-solution "(define (uf-union! uf id1 id2)
  (let ([root1 (uf-find uf id1)]
        [root2 (uf-find uf id2)])
    (if (= root1 root2)
        root1
        (let ([parent (uf-parent uf)]
              [rank (uf-rank uf)]
              [rank1 (vector-ref (uf-rank uf) root1)]
              [rank2 (vector-ref (uf-rank uf) root2)])
          (set-box! (uf-count-box uf) (- (unbox (uf-count-box uf)) 1))
          (cond
            [(< rank1 rank2)
             (vector-set! parent root1 root2)
             root2]
            [(> rank1 rank2)
             (vector-set! parent root2 root1)
             root1]
            [else
             (vector-set! parent root2 root1)
             (vector-set! rank root1 (+ rank1 1))
             root1])))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement uf-roots
  ;; ──────────────────────────────────────────────
  ;; List all set representatives.

  (task
    (id "egraph/union-find/uf-roots-impl")
    (module egraph/union-find)
    (tier 0)
    (type implement)
    (description "Implement uf-roots: list all root elements (set representatives) in the union-find. An element i is a root if parent[i] = i. Scan all elements from 0 to n-1 (where n = uf-size) and collect those whose parent is themselves. Return the list in ascending order. Use a named let loop with an accumulator, building the list with cons and reversing at the end.")
    (context "Available: uf-parent (parent vector), uf-size (element count), vector-ref, =, >=, +, cons, reverse. Linear scan with accumulator.")
    (before "(define (uf-roots uf)
  (doc 'export #t)
  ;; TODO: collect elements where parent[i] = i
  '())")
    (test-file "lattice/egraph/test-union-find.ss")
    (test-forms (";; Three singletons: three roots
(let ([uf (make-uf)])
  (uf-make-set! uf)
  (uf-make-set! uf)
  (uf-make-set! uf)
  (assert-equal 3 (length (uf-roots uf))))"
                 ";; After one union: two roots
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)]
        [c (uf-make-set! uf)])
    (uf-union! uf a b)
    (assert-equal 2 (length (uf-roots uf)))))"
                 ";; After merging all: one root
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)]
        [c (uf-make-set! uf)])
    (uf-union! uf a b)
    (uf-union! uf b c)
    (assert-equal 1 (length (uf-roots uf)))))"))
    (available-modules (prelude))
    (hints ("Get n = (uf-size uf) and parent = (uf-parent uf)"
            "Named let loop with i from 0, accumulator acc = '()"
            "Base: (>= i n) → (reverse acc)"
            "Step: if (= (vector-ref parent i) i) then (cons i acc) else acc"))
    (reference-solution "(define (uf-roots uf)
  (let ([parent (uf-parent uf)]
        [n (uf-size uf)])
    (let loop ([i 0] [acc '()])
      (if (>= i n)
          (reverse acc)
          (loop (+ i 1)
                (if (= (vector-ref parent i) i)
                    (cons i acc)
                    acc))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement uf-set-members
  ;; ──────────────────────────────────────────────
  ;; List all members of a given set — O(N) scan.

  (task
    (id "egraph/union-find/uf-set-members-impl")
    (module egraph/union-find)
    (tier 0)
    (type implement)
    (description "Implement uf-set-members: list all elements in the same set as a given element. Since the union-find doesn't maintain per-set member lists, this requires an O(N) scan over all elements. For each element i from 0 to n-1, call uf-find to get its root. If the root matches the root of the given element, include i in the result. Return elements in ascending order.")
    (context "Available: uf-find, uf-size, >=, =, +, cons, reverse. Linear scan comparing roots.")
    (before "(define (uf-set-members uf id)
  (doc 'export #t)
  ;; TODO: collect all elements with same root as id
  '())")
    (test-file "lattice/egraph/test-union-find.ss")
    (test-forms (";; Singleton: only itself
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)])
    (assert-equal (list a) (uf-set-members uf a))))"
                 ";; After union: both members listed
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)]
        [c (uf-make-set! uf)])
    (uf-union! uf a b)
    (let ([members (uf-set-members uf a)])
      (assert-equal 2 (length members))
      (assert-true (pair? (memv a members)))
      (assert-true (pair? (memv b members))))))"
                 ";; Non-member not included
(let ([uf (make-uf)])
  (let ([a (uf-make-set! uf)]
        [b (uf-make-set! uf)]
        [c (uf-make-set! uf)])
    (uf-union! uf a b)
    (assert-false (memv c (uf-set-members uf a)))))"))
    (available-modules (prelude))
    (hints ("Find the target root: (uf-find uf id)"
            "Scan all elements: named let with i from 0 to (uf-size uf)"
            "For each i: if (= (uf-find uf i) root) then cons i to accumulator"
            "Reverse at end for ascending order"))
    (reference-solution "(define (uf-set-members uf id)
  (let ([root (uf-find uf id)]
        [n (uf-size uf)])
    (let loop ([i 0] [acc '()])
      (if (>= i n)
          (reverse acc)
          (loop (+ i 1)
                (if (= (uf-find uf i) root)
                    (cons i acc)
                    acc))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
