;;; heap.sexp — Development tasks for lattice/data/heap.ss
;;;
;;; Module: heap (tier 0, depends only on prelude)
;;; 285 lines, ~35 exported functions
;;; Purely functional leftist heap with O(log n) merge-based operations.
;;; Heap α = Empty | (Node rank value left right)
;;; Includes min-heap, max-heap, generic comparator, PQ aliases, heapsort.
;;;
;;; Test file exists: test-heap.ss (277 lines)
;;;
;;; The central insight of leftist heaps: everything reduces to merge.
;;; Insert = merge with singleton. Delete-min = merge children.
;;; The leftist property (rank(left) >= rank(right)) keeps the right
;;; spine short, making merge O(log n).
;;;
;;; Git history:
;;;   c4b5253e feat(data): Add leftist heap and priority queue
;;;   36eac558 fix(heap): Make heap-fold stack-safe for left-biased heaps
;;;   596c1d4d feat(module): migrate data/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement make-heap-node (smart constructor)
  ;; ──────────────────────────────────────────────
  ;; The smart constructor maintains the leftist property by
  ;; comparing ranks of the two subtrees and swapping if needed.
  ;; rank = length of rightmost path to empty. The invariant
  ;; is rank(left) >= rank(right), ensuring the right spine is
  ;; always shortest. The node's own rank = 1 + rank(right)
  ;; because the rightmost path goes through the right child.

  (task
    (id "data/heap/make-heap-node-impl")
    (module heap)
    (tier 0)
    (type implement)
    (description "Implement make-heap-node: smart constructor that maintains the leftist property. Compare ranks of the two subtrees. If rank(left) >= rank(right), put left on left and right on right (natural order). Otherwise, swap them so the higher-rank subtree is always on the left. The node's rank = 1 + rank of whichever child ends up on the right (the shorter side). Use heap-rank to get a subtree's rank (returns 0 for heap-empty).")
    (context "Available: prelude, heap-node (raw constructor: (heap-node rank value left right) → tagged list), heap-rank (returns rank, 0 for empty), heap-empty.")
    (before "(define (make-heap-node value left right)
  (doc 'type (-> a Heap Heap Heap))
  (doc 'description \"Smart constructor that maintains leftist property\")
  ;; TODO: compare ranks, swap if needed, compute new rank
  (heap-node 1 value left right))")
    (test-file "lattice/data/test-heap.ss")
    (test-forms (";; Leftist property: rank(left) >= rank(right)
(let* ([h (list->heap '(5 3 8 1 4))])
  (assert-true (>= (heap-rank (heap-left h))
                    (heap-rank (heap-right h)))))"
                 ";; Sequential inserts still maintain leftist property
(let* ([h (list->heap (iota 20))])
  (assert-true (>= (heap-rank (heap-left h))
                    (heap-rank (heap-right h)))))"))
    (available-modules (prelude))
    (hints ("Get both ranks: (let ([rank-l (heap-rank left)] [rank-r (heap-rank right)]) ...)"
            "If rank-l >= rank-r: (heap-node (+ 1 rank-r) value left right)"
            "If rank-l < rank-r: (heap-node (+ 1 rank-l) value right left) — note the swap"))
    (reference-solution "(define (make-heap-node value left right)
  (let ([rank-l (heap-rank left)]
        [rank-r (heap-rank right)])
    (if (>= rank-l rank-r)
        (heap-node (+ 1 rank-r) value left right)
        (heap-node (+ 1 rank-l) value right left))))")
    (alternatives ())
    (source-commit "c4b5253e")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement heap-merge
  ;; ──────────────────────────────────────────────
  ;; The fundamental operation. All other heap operations are
  ;; defined in terms of merge. Compare roots of two heaps.
  ;; The smaller root wins (min-heap). Recursively merge the
  ;; loser heap with the winner's RIGHT child (keeping the
  ;; right spine short via make-heap-node's swapping).

  (task
    (id "data/heap/merge-impl")
    (module heap)
    (tier 0)
    (type implement)
    (description "Implement heap-merge: merge two min-heaps into one. Base cases: if either heap is empty, return the other. Otherwise, compare the two root values. The smaller root becomes the new root (min-heap property). Its LEFT child stays as-is. Recursively merge its RIGHT child with the entire other heap. Use make-heap-node to build the result — it handles the leftist property automatically. This is O(log n) because we always recurse along right spines, which are O(log n) by the leftist invariant.")
    (context "Available: prelude, heap-empty?, heap-value, heap-left, heap-right, make-heap-node (smart constructor that maintains leftist property by swapping children if needed).")
    (before "(define (heap-merge h1 h2)
  (doc 'type (-> Heap Heap Heap))
  (doc 'description \"Merge two min-heaps into one\")
  (doc 'complexity \"O(log n) where n = total size\")
  ;; TODO: compare roots, recurse along right spine
  (cond
    [(heap-empty? h1) h2]
    [(heap-empty? h2) h1]
    [else h1]))")
    (test-file "lattice/data/test-heap.ss")
    (test-forms ("(let ([h (heap-merge (list->heap '(1 5 9)) (list->heap '(2 6 10)))])
  (assert-equal 1 (heap-min h))
  (assert-equal 6 (heap-size h))
  (assert-equal '(1 2 5 6 9 10) (heap->list h)))"
                 "(assert-equal 5 (heap-min (heap-merge (list->heap '(5)) heap-empty)))"
                 "(assert-true (heap-empty? (heap-merge heap-empty heap-empty)))"
                 ";; Merge is the basis of insert
(let ([h (heap-insert 3 (heap-insert 5 heap-empty))])
  (assert-equal 3 (heap-min h))
  (assert-equal 2 (heap-size h)))"))
    (available-modules (prelude))
    (hints ("Two base cases: either heap empty → return the other"
            "Compare (heap-value h1) and (heap-value h2)"
            "Winner's left child stays: (heap-left winner)"
            "Recursively merge winner's right child with the loser: (heap-merge (heap-right winner) loser)"
            "Build result with make-heap-node: (make-heap-node winner-value (heap-left winner) recursive-result)"))
    (reference-solution "(define (heap-merge h1 h2)
  (cond
    [(heap-empty? h1) h2]
    [(heap-empty? h2) h1]
    [else
     (let ([v1 (heap-value h1)]
           [v2 (heap-value h2)])
       (if (<= v1 v2)
           (make-heap-node v1 (heap-left h1) (heap-merge (heap-right h1) h2))
           (make-heap-node v2 (heap-left h2) (heap-merge h1 (heap-right h2)))))]))")
    (alternatives ())
    (source-commit "c4b5253e")
    (difficulty 5))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Fix heap-fold stack safety
  ;; ──────────────────────────────────────────────
  ;; Real bug from commit 36eac558. The original heap-fold
  ;; was standard-recursive on the left spine and tail-recursive
  ;; on the right spine. In a leftist heap, rank(left) >= rank(right),
  ;; so the left spine can be O(n) deep while the right spine is
  ;; O(log n). The non-tail recursion on the O(n) side blows the
  ;; stack for large heaps. Fix: swap to recurse (non-tail) on the
  ;; short right spine, then tail-call on the long left spine.

  (task
    (id "data/heap/fix-fold-stack-safety")
    (module heap)
    (tier 0)
    (type fix)
    (description "Fix heap-fold so it doesn't blow the stack on large heaps. The current implementation processes the value, then standard-recurses on the LEFT child, then tail-calls on the RIGHT child. In a leftist heap, rank(left) >= rank(right), meaning the left spine can be O(n) deep while the right spine is always O(log n). The non-tail recursion on the left side means O(n) stack frames. Fix: swap the recursion order — standard-recurse on the short RIGHT spine (O(log n) stack frames), then tail-call on the long LEFT spine (safe in tail position).")
    (context "Available: prelude, heap-empty?, heap-value, heap-left, heap-right. The fold processes elements in tree-traversal order (not heap order). The fix is a simple reordering of the two recursive calls. The key insight: in (let* ([a (f x)] [b (f y)]) (f z)), only the last call (f z) is in tail position. Put the potentially-deep recursion there.")
    (before "(define (heap-fold fn init heap)
  (doc 'type (-> (-> b a b) b Heap b))
  (doc 'description \"Fold over all heap elements. Order is unspecified.\")
  (if (heap-empty? heap)
      init
      (let* ([acc (fn init (heap-value heap))]
             [acc-left (heap-fold fn acc (heap-left heap))])
        (heap-fold fn acc-left (heap-right heap)))))")
    (test-file "lattice/data/test-heap.ss")
    (test-forms (";; Basic fold works
(assert-equal 15 (heap-fold (lambda (acc x) (+ acc x)) 0 (list->heap '(1 2 3 4 5))))"
                 ";; Fold over large heap shouldn't blow stack
(let ([h (list->heap (iota 10000))])
  (assert-equal 49995000 (heap-fold (lambda (acc x) (+ acc x)) 0 h)))"))
    (available-modules (prelude))
    (hints ("The leftist property means rank(left) >= rank(right) — left is deep, right is short"
            "Non-tail recursion uses stack frames proportional to depth"
            "Swap: recurse on right first (short, safe), then tail-call on left (long, but tail position)"
            "Change: let* binds acc-right from right child, then tail-call fold on left child"))
    (reference-solution "(define (heap-fold fn init heap)
  (if (heap-empty? heap)
      init
      (let* ([acc (fn init (heap-value heap))]
             [acc-right (heap-fold fn acc (heap-right heap))])
        (heap-fold fn acc-right (heap-left heap)))))")
    (alternatives ())
    (source-commit "36eac558")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement heapsort-by
  ;; ──────────────────────────────────────────────
  ;; Sort using the heap as intermediate structure with a custom
  ;; comparator. Build the heap, then drain it in order.
  ;; The trick: repeatedly extract the top element and accumulate.

  (task
    (id "data/heap/heapsort-by-impl")
    (module heap)
    (tier 0)
    (type implement)
    (description "Implement heapsort-by: sort a list using a custom comparator via a heap. Build a heap from the list using list->heap-by with the comparator. Then drain it: repeatedly extract the top element (heap-value) and remove it (heap-delete-top-by), collecting elements into an accumulator. Reverse the accumulator at the end since we cons onto the front. The comparator cmp determines ordering: (heapsort-by <= lst) gives ascending, (heapsort-by >= lst) gives descending.")
    (context "Available: prelude, list->heap-by (builds heap from list with custom comparator), heap-empty?, heap-value (get top element), heap-delete-top-by (remove top using comparator).")
    (before "(define (heapsort-by cmp lst)
  (doc 'type (-> (-> a a Boolean) (List a) (List a)))
  (doc 'description \"Sort list using custom comparator via heap\")
  ;; TODO: build heap, drain in order
  lst)")
    (test-file "lattice/data/test-heap.ss")
    (test-forms ("(assert-equal '(1 1 3 4 5) (heapsort-by <= '(3 1 4 1 5)))"
                 "(assert-equal '(5 4 3 1 1) (heapsort-by >= '(3 1 4 1 5)))"
                 "(assert-equal '() (heapsort-by <= '()))"))
    (available-modules (prelude))
    (hints ("Build: (list->heap-by cmp lst)"
            "Drain loop: if empty return (reverse acc), else cons (heap-value h) onto acc and recur with (heap-delete-top-by cmp h)"
            "Use a named let for the loop"))
    (reference-solution "(define (heapsort-by cmp lst)
  (let loop ([h (list->heap-by cmp lst)] [acc '()])
    (if (heap-empty? h)
        (reverse acc)
        (loop (heap-delete-top-by cmp h) (cons (heap-value h) acc)))))")
    (alternatives ("(heap->list (list->heap-by cmp lst))  ;; only works if heap->list respects the comparator"))
    (source-commit "c4b5253e")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Extend with leftist-valid? (invariant checker)
  ;; ──────────────────────────────────────────────
  ;; The test file defines verify-leftist locally, but the module
  ;; itself has no invariant checker (unlike avl-tree which exports
  ;; avl-valid?). Add one that verifies both the leftist property
  ;; and the heap ordering property.

  (task
    (id "data/heap/leftist-valid-extend")
    (module heap)
    (tier 0)
    (type extend)
    (description "Add a heap-valid? function that verifies both the leftist property AND the min-heap ordering property. The leftist property: at every node, rank(left) >= rank(right). The heap ordering property: at every node, the value is <= the values of both children (for a min-heap). Both must hold recursively for all subtrees. Return #t if both properties hold everywhere, #f otherwise. An empty heap is valid.")
    (context "Available: prelude, heap-empty?, heap-value, heap-left, heap-right, heap-rank. The test file already has a local verify-leftist that checks only the leftist property — yours should also verify heap ordering.")
    (before ";; Add to heap.ss:
;; heap-valid? is not currently in the module.
;; The test file defines a local verify-leftist but it only checks
;; the leftist property, not heap ordering.")
    (test-file #f)
    (test-forms (";; Valid heap
(assert-true (heap-valid? (list->heap '(5 3 8 1 4))))"
                 ";; Empty heap is valid
(assert-true (heap-valid? heap-empty))"
                 ";; Singleton is valid
(assert-true (heap-valid? (heap-insert 42 heap-empty)))"
                 ";; After many operations, still valid
(let* ([h (list->heap (iota 100))]
       [h2 (heap-delete-min (heap-delete-min h))])
  (assert-true (heap-valid? h2)))"))
    (available-modules (prelude))
    (hints ("Check leftist: (>= (heap-rank (heap-left h)) (heap-rank (heap-right h)))"
            "Check ordering: value <= child values (only check non-empty children)"
            "Recurse on both subtrees"
            "Combine with (and leftist-ok? order-ok? left-valid? right-valid?)"))
    (reference-solution "(define (heap-valid? heap)
  (if (heap-empty? heap)
      #t
      (let ([v (heap-value heap)]
            [l (heap-left heap)]
            [r (heap-right heap)])
        (and (>= (heap-rank l) (heap-rank r))
             (or (heap-empty? l) (<= v (heap-value l)))
             (or (heap-empty? r) (<= v (heap-value r)))
             (heap-valid? l)
             (heap-valid? r)))))")
    (alternatives ("(define (heap-valid? heap)
  (define (leftist? h)
    (or (heap-empty? h)
        (and (>= (heap-rank (heap-left h)) (heap-rank (heap-right h)))
             (leftist? (heap-left h))
             (leftist? (heap-right h)))))
  (define (ordered? h)
    (or (heap-empty? h)
        (and (or (heap-empty? (heap-left h)) (<= (heap-value h) (heap-value (heap-left h))))
             (or (heap-empty? (heap-right h)) (<= (heap-value h) (heap-value (heap-right h))))
             (ordered? (heap-left h))
             (ordered? (heap-right h)))))
  (and (leftist? heap) (ordered? heap)))"))
    (source-commit #f)
    (difficulty 3))

) ;; end tasks
