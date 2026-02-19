;;; avl-tree.sexp — Development tasks for lattice/data/avl-tree.ss
;;;
;;; Module: avl-tree (tier 0, depends only on prelude)
;;; 447 lines, ~40 exported functions
;;; Purely functional self-balancing BST with O(log n) operations.
;;; AVL α = Empty | (Node height key value left right)
;;;
;;; Comprehensive test file exists: test-avl-tree.ss (279 lines)
;;;
;;; This is the first genuinely complex data structure in the curriculum.
;;; Key pedagogical content: tree rotations, 4-case rebalancing,
;;; deletion with in-order successor, range queries with BST pruning.
;;; The module includes built-in invariant checkers (avl-valid?, avl-bst-valid?)
;;; that serve as oracle functions for self-verifying tests.
;;;
;;; Git history:
;;;   a3fda791 feat(data): Add AVL tree (self-balancing binary search tree)
;;;   2dd3c927 docs(lattice): Convert data and linalg files to doc forms
;;;   596c1d4d feat(module): migrate data/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement rotate-left
  ;; ──────────────────────────────────────────────
  ;; Single left rotation for right-heavy trees. The fundamental
  ;; building block of AVL rebalancing. Requires destructuring
  ;; the tree representation and reassembling with a new root.
  ;;
  ;; Diagram:
  ;;     x            y
  ;;    / \          / \
  ;;   a   y  →    x   c
  ;;      / \     / \
  ;;     b   c   a   b

  (task
    (id "data/avl-tree/rotate-left-impl")
    (module avl-tree)
    (tier 0)
    (type implement)
    (description "Implement rotate-left: restructure a right-heavy tree by promoting the right child to root. Given tree rooted at x with right child y: the result is rooted at y, with x as y's left child. x gets y's former left subtree (b) as its new right child. Use the accessors (avl-key, avl-value, avl-left, avl-right) to destructure, and make-avl-node to rebuild (it recomputes heights automatically).")
    (context "Available: prelude, make-avl-node (smart constructor that recomputes height from children), avl-key, avl-value, avl-left, avl-right. A tree node is (list 'avl-node height key value left right). You should NOT set height manually — make-avl-node handles it.")
    (before "(define (rotate-left tree)
  (doc 'type (-> AVL AVL))
  (doc 'description \"Left rotation for right-heavy trees\")
  ;; TODO: promote right child to root
  ;; x's right becomes y's left (b), y's left becomes x
  tree)")
    (test-file "lattice/data/test-avl-tree.ss")
    (test-forms ("(let ([t (avl-insert 1 \"a\" (avl-insert 2 \"b\" (avl-insert 3 \"c\" avl-empty)))])
  (assert-true (avl-valid? t))
  (assert-true (avl-bst-valid? t)))"
                 "(let ([t (fold-left (lambda (t k) (avl-insert k k t)) avl-empty (iota 20))])
  (assert-true (avl-valid? t))
  (assert-true (<= (avl-height t) 7)))"))
    (available-modules (prelude))
    (hints ("Bind the parts of the input tree: x-key, x-val, a (left), y (right)"
            "Then bind the parts of y: y-key, y-val, b (y's left), c (y's right)"
            "Build result: (make-avl-node y-key y-val (make-avl-node x-key x-val a b) c)"))
    (reference-solution "(define (rotate-left tree)
  (let ([x-key (avl-key tree)]
        [x-val (avl-value tree)]
        [a (avl-left tree)]
        [y (avl-right tree)])
    (let ([y-key (avl-key y)]
          [y-val (avl-value y)]
          [b (avl-left y)]
          [c (avl-right y)])
      (make-avl-node y-key y-val
                     (make-avl-node x-key x-val a b)
                     c))))")
    (alternatives ())
    (source-commit "a3fda791")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement rebalance (4-case dispatch)
  ;; ──────────────────────────────────────────────
  ;; The heart of AVL trees. After an insert or delete, the balance
  ;; factor may be ±2. Rebalance restores the invariant using
  ;; single or double rotations depending on the shape of the
  ;; imbalanced subtree.
  ;;
  ;; Cases:
  ;;   bf > 1, left child bf >= 0 → Right rotation (LL case)
  ;;   bf > 1, left child bf < 0  → Left-then-right (LR case)
  ;;   bf < -1, right child bf <= 0 → Left rotation (RR case)
  ;;   bf < -1, right child bf > 0  → Right-then-left (RL case)
  ;;   otherwise → tree is balanced, return unchanged

  (task
    (id "data/avl-tree/rebalance-impl")
    (module avl-tree)
    (tier 0)
    (type implement)
    (description "Implement rebalance: restore the AVL invariant after insert/delete. Compute the balance factor (height(left) - height(right)). If bf > 1 (left-heavy): check if the left child's bf < 0 (LR case — rotate left child left, then rotate tree right) or >= 0 (LL case — just rotate tree right). If bf < -1 (right-heavy): mirror logic. If -1 <= bf <= 1: already balanced, return unchanged. For the double-rotation cases, rebuild the node with the rotated child before the outer rotation.")
    (context "Available: prelude, rotate-left, rotate-right, avl-balance-factor, make-avl-node, avl-key, avl-value, avl-left, avl-right. rotate-left/right are already implemented and correct. avl-balance-factor returns height(left) - height(right).")
    (before "(define (rebalance tree)
  (doc 'type (-> AVL AVL))
  (doc 'description \"Rebalance tree after insertion or deletion\")
  ;; TODO: check balance factor, dispatch to correct rotation case
  ;; Four cases: LL, LR, RR, RL
  tree)")
    (test-file "lattice/data/test-avl-tree.ss")
    (test-forms (";; LL case: sequential descending inserts
(let ([t (avl-insert 3 \"a\" (avl-insert 2 \"b\" (avl-insert 1 \"c\" avl-empty)))])
  (assert-true (avl-valid? t))
  (assert-true (avl-bst-valid? t)))"
                 ";; LR case
(let ([t (avl-insert 2 \"a\" (avl-insert 3 \"b\" (avl-insert 1 \"c\" avl-empty)))])
  (assert-true (avl-valid? t))
  (assert-true (avl-bst-valid? t)))"
                 ";; RL case
(let ([t (avl-insert 2 \"a\" (avl-insert 1 \"b\" (avl-insert 3 \"c\" avl-empty)))])
  (assert-true (avl-valid? t))
  (assert-true (avl-bst-valid? t)))"
                 ";; Stress: sequential insert of 0..19 must stay balanced
(let ([t (fold-left (lambda (t k) (avl-insert k k t)) avl-empty (iota 20))])
  (assert-true (avl-valid? t))
  (assert-true (avl-bst-valid? t))
  (assert-true (<= (avl-height t) 7)))"))
    (available-modules (prelude))
    (hints ("Start by computing (avl-balance-factor tree) and binding it"
            "For the LR case: first rotate the LEFT child LEFT, rebuild the tree with that rotated child, then rotate the whole tree RIGHT"
            "The double-rotation rebuild: (make-avl-node (avl-key tree) (avl-value tree) (rotate-left (avl-left tree)) (avl-right tree)) — then rotate-right the result"
            "Use cond with (> bf 1), (< bf -1), and else"))
    (reference-solution "(define (rebalance tree)
  (let ([bf (avl-balance-factor tree)])
    (cond
      [(> bf 1)
       (if (< (avl-balance-factor (avl-left tree)) 0)
           (rotate-right (make-avl-node (avl-key tree) (avl-value tree)
                                        (rotate-left (avl-left tree))
                                        (avl-right tree)))
           (rotate-right tree))]
      [(< bf -1)
       (if (> (avl-balance-factor (avl-right tree)) 0)
           (rotate-left (make-avl-node (avl-key tree) (avl-value tree)
                                       (avl-left tree)
                                       (rotate-right (avl-right tree))))
           (rotate-left tree))]
      [else tree])))")
    (alternatives ())
    (source-commit "a3fda791")
    (difficulty 6))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Fix broken rebalance (LR/RL detection)
  ;; ──────────────────────────────────────────────
  ;; The bug: the inner balance factor checks are inverted.
  ;; When bf > 1 (left-heavy), the code checks if left child's
  ;; bf > 0 instead of < 0 for the LR case. This means:
  ;;   - LR patterns get single rotation (wrong, breaks BST)
  ;;   - LL patterns get double rotation (wasteful but technically works)
  ;; The avl-bst-valid? check catches the BST violation.

  (task
    (id "data/avl-tree/fix-rebalance-lr-rl")
    (module avl-tree)
    (tier 0)
    (type fix)
    (description "Fix the rebalance function. It handles single rotation cases correctly (LL and RR), but the detection for double-rotation cases (LR and RL) is inverted — it checks the wrong direction for the inner subtree's balance factor. This means some insertion sequences produce trees that satisfy the AVL height invariant but violate the BST ordering property. Use avl-bst-valid? to verify your fix.")
    (context "Available: all avl-tree functions including avl-valid? (checks balance) and avl-bst-valid? (checks BST ordering). The bug is in how rebalance decides between single and double rotation. rotate-left and rotate-right are correct.")
    (before "(define (rebalance tree)
  (let ([bf (avl-balance-factor tree)])
    (cond
      [(> bf 1)
       ;; BUG: checks > 0 instead of < 0 for the LR case
       (if (> (avl-balance-factor (avl-left tree)) 0)
           (rotate-right (make-avl-node (avl-key tree) (avl-value tree)
                                        (rotate-left (avl-left tree))
                                        (avl-right tree)))
           (rotate-right tree))]
      [(< bf -1)
       ;; BUG: checks < 0 instead of > 0 for the RL case
       (if (< (avl-balance-factor (avl-right tree)) 0)
           (rotate-left (make-avl-node (avl-key tree) (avl-value tree)
                                       (avl-left tree)
                                       (rotate-right (avl-right tree))))
           (rotate-left tree))]
      [else tree])))")
    (test-file "lattice/data/test-avl-tree.ss")
    (test-forms (";; This insertion sequence triggers the LR case
(let ([t (avl-insert 3 \"c\" (avl-insert 1 \"a\" (avl-insert 2 \"b\" avl-empty)))])
  (assert-true (avl-valid? t))
  (assert-true (avl-bst-valid? t))
  (assert-equal \"a\" (avl-lookup 1 t))
  (assert-equal \"b\" (avl-lookup 2 t))
  (assert-equal \"c\" (avl-lookup 3 t)))"
                 ";; This triggers the RL case
(let ([t (avl-insert 1 \"a\" (avl-insert 3 \"c\" (avl-insert 2 \"b\" avl-empty)))])
  (assert-true (avl-valid? t))
  (assert-true (avl-bst-valid? t)))"
                 ";; Stress test — sequential insert should maintain both invariants
(let ([t (fold-left (lambda (t k) (avl-insert k k t)) avl-empty (iota 20))])
  (assert-true (avl-valid? t))
  (assert-true (avl-bst-valid? t)))"))
    (available-modules (prelude))
    (hints ("The LR case happens when the tree is left-heavy (bf > 1) but the LEFT child is RIGHT-heavy (its bf < 0)"
            "The buggy code has the comparison direction swapped for both inner checks"
            "Fix: change > to < in the first if, and < to > in the second if"))
    (reference-solution "(define (rebalance tree)
  (let ([bf (avl-balance-factor tree)])
    (cond
      [(> bf 1)
       (if (< (avl-balance-factor (avl-left tree)) 0)
           (rotate-right (make-avl-node (avl-key tree) (avl-value tree)
                                        (rotate-left (avl-left tree))
                                        (avl-right tree)))
           (rotate-right tree))]
      [(< bf -1)
       (if (> (avl-balance-factor (avl-right tree)) 0)
           (rotate-left (make-avl-node (avl-key tree) (avl-value tree)
                                       (avl-left tree)
                                       (rotate-right (avl-right tree))))
           (rotate-left tree))]
      [else tree])))")
    (alternatives ())
    (source-commit #f)
    (difficulty 5))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement avl-delete-by
  ;; ──────────────────────────────────────────────
  ;; The hardest operation on BSTs. Three subcases when the
  ;; target key is found: leaf (return empty), one child
  ;; (return that child), two children (replace with in-order
  ;; successor, delete successor from right subtree).
  ;; Must rebalance on the way up the recursion.

  (task
    (id "data/avl-tree/delete-impl")
    (module avl-tree)
    (tier 0)
    (type implement)
    (description "Implement avl-delete-by: delete a key from an AVL tree using a custom comparator. Search like avl-lookup-by (use cmp to go left or right). When the key is found, handle three cases: (1) both children empty — return avl-empty, (2) one child empty — return the other child, (3) both children present — find the in-order successor (minimum of right subtree via avl-min-node), replace current node's key/value with successor's key/value, then delete the successor from the right subtree using avl-delete-min-by. Rebalance after every recursive step. If key not found, return tree unchanged.")
    (context "Available: prelude, avl-empty, avl-empty?, avl-key, avl-value, avl-left, avl-right, make-avl-node, rebalance (correct), avl-min-node (returns (cons key value) of minimum), avl-delete-min-by (deletes minimum from subtree, rebalances).")
    (before "(define (avl-delete-by cmp key tree)
  (doc 'type (-> (-> k k Boolean) k AVL AVL))
  (doc 'description \"Delete key from AVL tree with custom comparator\")
  ;; TODO: search for key using cmp
  ;; When found: handle leaf, one-child, two-child cases
  ;; Rebalance after recursive descent
  tree)")
    (test-file "lattice/data/test-avl-tree.ss")
    (test-forms (";; Delete a leaf
(let* ([t (avl-insert 10 \"b\" (avl-insert 5 \"d\" (avl-insert 20 \"a\" (avl-insert 15 \"e\" (avl-insert 30 \"c\" avl-empty)))))]
       [t2 (avl-delete 5 t)])
  (assert-equal 4 (avl-size t2))
  (assert-false (avl-lookup 5 t2))
  (assert-true (avl-valid? t2)))"
                 ";; Delete node with two children
(let* ([t (avl-insert 10 \"b\" (avl-insert 5 \"d\" (avl-insert 20 \"a\" (avl-insert 15 \"e\" (avl-insert 30 \"c\" avl-empty)))))]
       [t2 (avl-delete 10 t)])
  (assert-equal 4 (avl-size t2))
  (assert-true (avl-valid? t2))
  (assert-true (avl-bst-valid? t2)))"
                 ";; Delete non-existent key
(let ([t (avl-insert 5 \"x\" avl-empty)])
  (assert-equal 1 (avl-size (avl-delete 99 t))))"
                 ";; Stress: insert 100, delete evens, verify invariants
(let* ([t (fold-left (lambda (t k) (avl-insert k (* k k) t)) avl-empty (iota 100))]
       [t2 (fold-left (lambda (t k) (avl-delete k t)) t (filter even? (iota 100)))])
  (assert-equal 50 (avl-size t2))
  (assert-true (avl-valid? t2))
  (assert-true (avl-bst-valid? t2)))"))
    (available-modules (prelude))
    (hints ("Recurse left if (cmp key k), right if (cmp k key), otherwise you found the node"
            "For the two-children case: (let ([succ (avl-min-node right)]) ...) gives you the replacement key/value"
            "After replacing, delete the successor from right: (avl-delete-min-by cmp right)"
            "Wrap every recursive make-avl-node in (rebalance ...)"))
    (reference-solution "(define (avl-delete-by cmp key tree)
  (if (avl-empty? tree)
      tree
      (let ([k (avl-key tree)]
            [v (avl-value tree)]
            [left (avl-left tree)]
            [right (avl-right tree)])
        (cond
          [(cmp key k)
           (rebalance (make-avl-node k v (avl-delete-by cmp key left) right))]
          [(cmp k key)
           (rebalance (make-avl-node k v left (avl-delete-by cmp key right)))]
          [else
           (cond
             [(avl-empty? left) right]
             [(avl-empty? right) left]
             [else
              (let ([succ (avl-min-node right)])
                (rebalance (make-avl-node (car succ) (cdr succ)
                                          left
                                          (avl-delete-min-by cmp right))))])]))))")
    (alternatives ())
    (source-commit "a3fda791")
    (difficulty 7))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement avl-range-by (range query)
  ;; ──────────────────────────────────────────────
  ;; Range queries exploit BST ordering to prune the search.
  ;; At each node: go left only if lo < key, include current
  ;; node only if lo <= key <= hi, go right only if key < hi.
  ;; The comparator cmp stands for <, so "not less and not greater"
  ;; means equal-or-between.

  (task
    (id "data/avl-tree/range-query-impl")
    (module avl-tree)
    (tier 0)
    (type implement)
    (description "Implement avl-range-by: return all key-value pairs where lo <= key <= hi, as a sorted association list. Use the BST property to prune: only recurse left if (cmp lo key) — the left subtree might contain keys >= lo. Only include the current node if it's in range: (not (cmp key lo)) AND (not (cmp hi key)) — meaning key >= lo AND key <= hi. Only recurse right if (cmp key hi) — the right subtree might contain keys <= hi. Combine results with append in left-current-right order to maintain sorted output.")
    (context "Available: prelude, avl-empty?, avl-key, avl-value, avl-left, avl-right. The comparator cmp is a strict less-than function (like <). Use (not (cmp a b)) to mean a >= b, and (not (cmp b a)) to mean a <= b. The combination (and (not (cmp k lo)) (not (cmp hi k))) means lo <= k <= hi.")
    (before "(define (avl-range-by cmp lo hi tree)
  (doc 'type (-> (-> k k Boolean) k k AVL (List (Pair k v))))
  (doc 'description \"Get all key-value pairs where lo <= key <= hi\")
  ;; TODO: BST-guided range extraction
  ;; Only recurse into subtrees that might contain in-range keys
  '())")
    (test-file "lattice/data/test-avl-tree.ss")
    (test-forms ("(let ([t (avl-from-keys '(1 3 5 7 9 11 13 15))])
  (assert-equal '((5 . 5) (7 . 7) (9 . 9) (11 . 11)) (avl-range 5 11 t)))"
                 "(let ([t (avl-from-keys '(1 3 5 7 9 11 13 15))])
  (assert-equal '() (avl-range 100 200 t)))"
                 "(let ([t (avl-from-keys '(1 3 5 7 9 11 13 15))])
  (assert-equal '((7 . 7)) (avl-range 7 7 t)))"
                 "(let ([t (avl-from-keys '(1 3 5 7 9 11 13 15))])
  (assert-equal '(5 7 9) (avl-keys-between 5 9 t)))"))
    (available-modules (prelude))
    (hints ("At each node, make three decisions independently: go left?, include current?, go right?"
            "Go left if (cmp lo key) — there might be keys >= lo in left subtree"
            "Include current if (and (not (cmp key lo)) (not (cmp hi key)))"
            "Go right if (cmp key hi) — there might be keys <= hi in right subtree"
            "Use (append left-results current-if-any right-results)"))
    (reference-solution "(define (avl-range-by cmp lo hi tree)
  (if (avl-empty? tree)
      '()
      (let ([k (avl-key tree)]
            [v (avl-value tree)])
        (append
         (if (cmp lo k)
             (avl-range-by cmp lo hi (avl-left tree))
             '())
         (if (and (not (cmp k lo)) (not (cmp hi k)))
             (list (cons k v))
             '())
         (if (cmp k hi)
             (avl-range-by cmp lo hi (avl-right tree))
             '())))))")
    (alternatives ())
    (source-commit "a3fda791")
    (difficulty 5))

) ;; end tasks
