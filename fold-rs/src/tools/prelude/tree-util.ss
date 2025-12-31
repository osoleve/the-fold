; ============================================================
; Nested Structure and Tree Utilities
; Deep traversal, metrics, and search functions
; ============================================================

; deep-map: Apply function to all atoms in nested structure
(deep-map (fix deep-map
              (fn (f tree)
                  (if (null? tree)
                      '()
                      (if (pair? tree)
                          (cons (deep-map f (car tree))
                                (deep-map f (cdr tree)))
                          (f tree))))))

; deep-filter: Keep atoms matching predicate
(deep-filter (fix deep-filter
                 (fn (p tree)
                     (if (null? tree)
                         '()
                         (if (pair? tree)
                             (cons (deep-filter p (car tree))
                                   (deep-filter p (cdr tree)))
                             (if (p tree) tree '()))))))

; flatten-deep: Flatten all nested lists
(flatten-deep (fix flatten-deep
                  (fn (tree)
                      (if (null? tree)
                          '()
                          (if (pair? tree)
                              (append (flatten-deep (car tree))
                                      (flatten-deep (cdr tree)))
                              (list tree))))))

; tree-depth: Maximum nesting depth
(tree-depth (fix tree-depth
                (fn (tree)
                    (if (null? tree)
                        0
                        (if (pair? tree)
                            (+ 1 (max (tree-depth (car tree))
                                      (tree-depth (cdr tree))))
                            0)))))

; tree-height: Alias for tree-depth
(tree-height tree-depth)

; tree-size: Count all atoms
(tree-size (fix tree-size
               (fn (tree)
                   (if (null? tree)
                       0
                       (if (pair? tree)
                           (+ (tree-size (car tree))
                              (tree-size (cdr tree)))
                           1)))))

; tree-count: Alias for tree-size
(tree-count tree-size)

; tree-find: Find first atom matching predicate
(tree-find (fix tree-find
               (fn (p tree)
                   (if (null? tree)
                       #f
                       (if (pair? tree)
                           (let ((left (tree-find p (car tree))))
                                (if left
                                    left
                                    (tree-find p (cdr tree))))
                           (if (p tree) tree #f))))))

; tree-leaves: Get all leaves
(tree-leaves flatten-deep)
