; Tree utilities - operations on nested list structures

; tree-depth: Maximum nesting depth of structure
; (tree-depth '((1 2) (3 (4 5)))) => 3
(tree-depth (fix tree-depth
                 (fn (tree)
                     (if (pair? tree)
                         (+ 1 (foldl max 0 (map tree-depth tree)))
                         0))))

; tree-size: Count all atoms in nested structure
; (tree-size '((1 2) (3 (4 5)))) => 5
(tree-size (fix tree-size
                (fn (tree)
                    (if (null? tree)
                        0
                        (if (pair? tree)
                            (+ (tree-size (car tree)) (tree-size (cdr tree)))
                            1)))))

; tree-find: Find first atom matching predicate in tree
; (tree-find even? '((1 3) (5 (6 7)))) => 6
(tree-find (fix tree-find
                (fn (p tree)
                    (if (null? tree)
                        #f
                        (if (pair? tree)
                            (let ((left (tree-find p (car tree))))
                                 (if left left (tree-find p (cdr tree))))
                            (if (p tree) tree #f))))))

; tree-leaves: Get all leaves of tree
(tree-leaves (fix tree-leaves
                  (fn (tree)
                      (if (not (pair? tree))
                          (list tree)
                          (concat (map tree-leaves tree))))))

; tree-count: Count nodes in tree
(tree-count (fix tree-count
                 (fn (tree)
                     (if (not (pair? tree))
                         1
                         (+ 1 (sum-list (map tree-count tree)))))))

; tree-height: Alias for tree-depth
(tree-height tree-depth)

; fold-tree: Fold over tree structure
(fold-tree (fix fold-tree
                (fn (leaf-fn node-fn tree)
                    (if (not (pair? tree))
                        (leaf-fn tree)
                        (node-fn (car tree)
                                 (map (fn (child) (fold-tree leaf-fn node-fn child))
                                      (cdr tree)))))))
