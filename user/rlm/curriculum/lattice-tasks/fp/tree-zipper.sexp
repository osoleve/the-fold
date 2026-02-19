;;; tree-zipper.sexp — Development tasks for lattice/fp/data/tree-zipper.ss
;;;
;;; Module: tree-zipper (tier 0, depends on prelude, combinators)
;;; 753 lines, ~40 exported functions
;;; Rose tree type with Huet-style zipper for efficient local navigation
;;; and modification. Breadcrumb-based context tracking, comonad instance,
;;; pre-order traversal, insertion, deletion, and path operations.
;;;
;;; Git history:
;;;   8581029b feat(fp/data): Add TreeZipper for rose trees
;;;   8ae4ce61 fix(fp/data): Fix TreeZipper Comonad law violation with duplicate values

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement tree-size
  ;; ──────────────────────────────────────────────
  ;; Count all nodes in a rose tree.

  (task
    (id "fp/tree-zipper/tree-size-impl")
    (module tree-zipper)
    (tier 0)
    (type implement)
    (description "Implement tree-size: count the total number of nodes in a rose tree. A tree node contains a value and a list of children (which are themselves trees). A leaf is a tree with no children. The count is 1 (for the current node) plus the sum of sizes of all children. For a non-tree value, return 0. Recursively map tree-size over the children list, then sum the results with (apply +). This is a natural fold over the tree structure.")
    (context "Available: tree?, tree-children, map, apply, +, if, not. Recursive map + sum.")
    (before "(define (tree-size t)
  ;; TODO: count all nodes
  0)")
    (test-file "lattice/fp/data/test-tree-zipper.ss")
    (test-forms (";; Single leaf
(assert-equal 1 (tree-size (tree-leaf 1)))"
                 ";; Simple tree with 3 children
(assert-equal 4 (tree-size (tree-node 1 (tree-leaf 2) (tree-leaf 3) (tree-leaf 4))))"
                 ";; Deeper tree
(assert-equal 9
  (tree-size (tree-node 1
    (tree-node 2 (tree-leaf 5) (tree-leaf 6))
    (tree-node 3 (tree-leaf 7))
    (tree-node 4 (tree-leaf 8) (tree-leaf 9)))))"
                 ";; Non-tree returns 0
(assert-equal 0 (tree-size 42))"))
    (available-modules (prelude combinators))
    (hints ("Guard: (if (not (tree? t)) 0 ...)"
            "Children: (tree-children t)"
            "Recurse: (map tree-size children)"
            "Sum: (apply + child-sizes)"
            "Result: (+ 1 (apply + (map tree-size (tree-children t))))"))
    (reference-solution "(define (tree-size t)
  (if (not (tree? t))
      0
      (+ 1 (apply + (map tree-size (tree-children t))))))")
    (alternatives ())
    (source-commit "8581029b")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement tree-depth
  ;; ──────────────────────────────────────────────
  ;; Maximum depth of a rose tree.

  (task
    (id "fp/tree-zipper/tree-depth-impl")
    (module tree-zipper)
    (tier 0)
    (type implement)
    (description "Implement tree-depth: compute the maximum depth of a rose tree, where a single leaf has depth 1. For a non-tree, return 0. For a leaf (no children), return 1. For an internal node, return 1 plus the maximum depth among all children. Recursively map tree-depth over the children, then take the maximum with (apply max). This measures the longest root-to-leaf path in the tree.")
    (context "Available: tree?, tree-children, null?, map, apply, max, +, if, not. Three-case dispatch.")
    (before "(define (tree-depth t)
  ;; TODO: maximum depth (leaf = 1)
  0)")
    (test-file "lattice/fp/data/test-tree-zipper.ss")
    (test-forms (";; Leaf has depth 1
(assert-equal 1 (tree-depth (tree-leaf 42)))"
                 ";; Two levels
(assert-equal 2 (tree-depth (tree-node 1 (tree-leaf 2) (tree-leaf 3))))"
                 ";; Three levels
(assert-equal 3
  (tree-depth (tree-node 1
    (tree-node 2 (tree-leaf 5) (tree-leaf 6))
    (tree-node 3 (tree-leaf 7)))))"
                 ";; Asymmetric: depth follows longest path
(assert-equal 4
  (tree-depth (tree-node 1
    (tree-node 2
      (tree-node 3 (tree-leaf 6))
      (tree-leaf 4)
      (tree-leaf 5)))))"))
    (available-modules (prelude combinators))
    (hints ("Non-tree: return 0"
            "Leaf (null children): return 1"
            "Internal: (+ 1 (apply max (map tree-depth children)))"
            "Three-case if/if or cond"))
    (reference-solution "(define (tree-depth t)
  (if (not (tree? t))
      0
      (if (null? (tree-children t))
          1
          (+ 1 (apply max (map tree-depth (tree-children t)))))))")
    (alternatives ())
    (source-commit "8581029b")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement tree-map
  ;; ──────────────────────────────────────────────
  ;; Functor map over rose tree values.

  (task
    (id "fp/tree-zipper/tree-map-impl")
    (module tree-zipper)
    (tier 0)
    (type implement)
    (description "Implement tree-map: apply a function to every node value in a rose tree, preserving the tree structure. This is the Functor fmap for rose trees. For a non-tree, return it unchanged. For a tree node, construct a new tree where: the value is (f (tree-value t)), and the children are the result of recursively mapping tree-map over each child. Uses make-tree (not tree-node) to construct the result since children are already a list. The recursive step maps a lambda that calls tree-map over the children list.")
    (context "Available: tree?, tree-value, tree-children, make-tree, map, lambda, if, not. Recursive structure-preserving map.")
    (before "(define (tree-map f t)
  ;; TODO: apply f to every node value
  t)")
    (test-file "lattice/fp/data/test-tree-zipper.ss")
    (test-forms (";; Double all values
(let ([doubled (tree-map (lambda (x) (* x 2))
                         (tree-node 1 (tree-leaf 2) (tree-leaf 3)))])
  (assert-equal 2 (tree-value doubled))
  (assert-equal 4 (tree-value (car (tree-children doubled)))))"
                 ";; Identity law
(let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
       [t2 (tree-map (lambda (x) x) t)])
  (assert-true (tree-equal? t t2)))"
                 ";; Leaf
(let ([t (tree-map (lambda (x) (+ x 10)) (tree-leaf 5))])
  (assert-equal 15 (tree-value t))
  (assert-true (tree-leaf? t)))"
                 ";; Flattening shows all values transformed
(assert-equal '(10 20 30 40)
  (tree-flatten (tree-map (lambda (x) (* x 10))
    (tree-node 1 (tree-leaf 2) (tree-leaf 3) (tree-leaf 4)))))"))
    (available-modules (prelude combinators))
    (hints ("Guard: (if (not (tree? t)) t ...)"
            "New value: (f (tree-value t))"
            "Recursive children: (map (lambda (child) (tree-map f child)) (tree-children t))"
            "Construct: (make-tree new-value new-children)"))
    (reference-solution "(define (tree-map f t)
  (if (not (tree? t))
      t
      (make-tree (f (tree-value t))
                 (map (lambda (child) (tree-map f child))
                      (tree-children t)))))")
    (alternatives ())
    (source-commit "8581029b")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement tree-zipper-up
  ;; ──────────────────────────────────────────────
  ;; Navigate from child back to parent in a zipper.

  (task
    (id "fp/tree-zipper/tree-zipper-up-impl")
    (module tree-zipper)
    (tier 0)
    (type implement)
    (description "Implement tree-zipper-up: move the focus from the current node back to its parent. Returns a Maybe — nothing if already at root, (just new-zipper) otherwise. The zipper stores a list of 'crumbs' (breadcrumbs), where each crumb records the parent's value, the left siblings (reversed, closest first), and the right siblings. To go up: pop the top crumb, reconstruct the parent's children list by concatenating (reverse left-siblings), the current focus, and right-siblings. Build the parent tree with make-tree, then wrap in a new zipper with the remaining crumbs. This is the inverse of tree-zipper-down.")
    (context "Available: tree-zipper-crumbs, tree-zipper-focus, null?, car, cdr, crumb-value, crumb-left, crumb-right, reverse, append, list, cons, make-tree, make-tree-zipper, just, nothing. Crumb deconstruction + child list reconstruction.")
    (before "(define (tree-zipper-up z)
  ;; TODO: move focus to parent, return Maybe
  nothing)")
    (test-file "lattice/fp/data/test-tree-zipper.ss")
    (test-forms (";; Up from child returns to parent
(let* ([z (tree->zipper (tree-node 1 (tree-leaf 2) (tree-leaf 3)))]
       [z2 (from-just (tree-zipper-down z))]
       [z3 (from-just (tree-zipper-up z2))])
  (assert-equal 1 (tree-zipper-get z3))
  (assert-true (tree-zipper-at-root? z3)))"
                 ";; Up from root returns nothing
(let ([z (tree->zipper (tree-node 1 (tree-leaf 2)))])
  (assert-true (nothing? (tree-zipper-up z))))"
                 ";; Down then up preserves tree
(let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3) (tree-leaf 4))]
       [z (tree->zipper t)]
       [z2 (from-just (tree-zipper-down z))]
       [z3 (from-just (tree-zipper-up z2))]
       [t2 (zipper->tree z3)])
  (assert-true (tree-equal? t t2)))"
                 ";; Up from middle child reconstructs correctly
(let* ([z (tree->zipper (tree-node 1 (tree-leaf 2) (tree-leaf 3) (tree-leaf 4)))]
       [z2 (from-just (tree-zipper-down z))]
       [z3 (from-just (tree-zipper-right z2))]  ;; at 3
       [z4 (from-just (tree-zipper-up z3))])
  (assert-equal 1 (tree-zipper-get z4))
  (assert-equal '(1 2 3 4) (tree-flatten (zipper->tree z4))))"))
    (available-modules (prelude combinators))
    (hints ("Check: (null? crumbs) -> nothing (at root)"
            "Pop crumb: (car crumbs), remaining: (cdr crumbs)"
            "Extract from crumb: parent-value, left, right"
            "Reconstruct siblings: (append (reverse left) (list focus) right)"
            "Build parent: (make-tree parent-value siblings)"
            "Wrap: (just (make-tree-zipper parent-tree remaining-crumbs))"))
    (reference-solution "(define (tree-zipper-up z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (if (null? crumbs)
        nothing
        (let* ([crumb (car crumbs)]
               [parent-value (crumb-value crumb)]
               [left (crumb-left crumb)]
               [right (crumb-right crumb)]
               [focus (tree-zipper-focus z)]
               [siblings (append (reverse left) (list focus) right)])
          (just (make-tree-zipper
                 (make-tree parent-value siblings)
                 (cdr crumbs)))))))")
    (alternatives ())
    (source-commit "8581029b")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement tree-zipper-right
  ;; ──────────────────────────────────────────────
  ;; Move focus to the right sibling.

  (task
    (id "fp/tree-zipper/tree-zipper-right-impl")
    (module tree-zipper)
    (tier 0)
    (type implement)
    (description "Implement tree-zipper-right: move the focus to the right sibling. Returns a Maybe — nothing if at root (no siblings) or no right sibling exists. The current focus's crumb stores the right siblings list. If it's non-empty, the new focus becomes (car right), and we update the crumb: the old focus is consed onto the left siblings, and (cdr right) becomes the new right siblings. The crumb's parent value stays the same. The remaining crumbs (cdr crumbs) are unchanged. This is the horizontal navigation that complements up/down vertical navigation.")
    (context "Available: tree-zipper-crumbs, tree-zipper-focus, null?, car, cdr, cons, crumb-value, crumb-left, crumb-right, make-crumb, make-tree-zipper, just, nothing. Crumb update with left/right swap.")
    (before "(define (tree-zipper-right z)
  ;; TODO: move to right sibling, return Maybe
  nothing)")
    (test-file "lattice/fp/data/test-tree-zipper.ss")
    (test-forms (";; Move right through siblings
(let* ([z (tree->zipper (tree-node 1 (tree-leaf 2) (tree-leaf 3) (tree-leaf 4)))]
       [z2 (from-just (tree-zipper-down z))]    ;; at 2
       [z3 (from-just (tree-zipper-right z2))]   ;; at 3
       [z4 (from-just (tree-zipper-right z3))])   ;; at 4
  (assert-equal 3 (tree-zipper-get z3))
  (assert-equal 4 (tree-zipper-get z4)))"
                 ";; No right sibling returns nothing
(let* ([z (tree->zipper (tree-node 1 (tree-leaf 2) (tree-leaf 3)))]
       [z2 (from-just (tree-zipper-down z))]
       [z3 (from-just (tree-zipper-right z2))])  ;; at 3, last child
  (assert-true (nothing? (tree-zipper-right z3))))"
                 ";; At root returns nothing
(let ([z (tree->zipper (tree-node 1 (tree-leaf 2)))])
  (assert-true (nothing? (tree-zipper-right z))))"
                 ";; Right then up preserves tree
(let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
       [z (tree->zipper t)]
       [z2 (from-just (tree-zipper-down z))]
       [z3 (from-just (tree-zipper-right z2))]
       [z4 (from-just (tree-zipper-up z3))]
       [t2 (zipper->tree z4)])
  (assert-true (tree-equal? t t2)))"))
    (available-modules (prelude combinators))
    (hints ("Root check: (null? crumbs) -> nothing"
            "Get right siblings: (crumb-right (car crumbs))"
            "Empty right: nothing"
            "New focus: (car right)"
            "New crumb: old focus goes to left, (cdr right) is new right"
            "(make-crumb (crumb-value crumb) (cons (tree-zipper-focus z) (crumb-left crumb)) (cdr right))"
            "Wrap: (just (make-tree-zipper (car right) (cons new-crumb (cdr crumbs))))"))
    (reference-solution "(define (tree-zipper-right z)
  (let ([crumbs (tree-zipper-crumbs z)])
    (if (null? crumbs)
        nothing
        (let* ([crumb (car crumbs)]
               [right (crumb-right crumb)])
          (if (null? right)
              nothing
              (let ([new-crumb (make-crumb
                                (crumb-value crumb)
                                (cons (tree-zipper-focus z)
                                      (crumb-left crumb))
                                (cdr right))])
                (just (make-tree-zipper
                       (car right)
                       (cons new-crumb (cdr crumbs))))))))))")
    (alternatives ())
    (source-commit "8581029b")
    (difficulty 2))

) ;; end tasks
