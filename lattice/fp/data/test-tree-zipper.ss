;;; lattice/fp/data/test-tree-zipper.ss — Tests for Tree Zipper
;;;
;;; Comprehensive tests for rose tree zipper implementation.

(load "core/testing/test-framework.ss")
(load "lattice/fp/data/tree-zipper.ss")

;;; ====
;;; Test Data: Sample Trees
;;; ====

;;; Single node tree
(define tree-single (tree-leaf 1))

;;; Simple tree with two levels:
;;;        1
;;;      / | \
;;;     2  3  4
(define tree-simple
  (tree-node 1
             (tree-leaf 2)
             (tree-leaf 3)
             (tree-leaf 4)))

;;; Deeper tree:
;;;            1
;;;         /  |  \
;;;        2   3   4
;;;       /|   |   |\
;;;      5 6   7   8 9
(define tree-deep
  (tree-node 1
             (tree-node 2
                        (tree-leaf 5)
                        (tree-leaf 6))
             (tree-node 3
                        (tree-leaf 7))
             (tree-node 4
                        (tree-leaf 8)
                        (tree-leaf 9))))

;;; Asymmetric tree:
;;;        1
;;;        |
;;;        2
;;;       /|\
;;;      3 4 5
;;;      |
;;;      6
(define tree-asymmetric
  (tree-node 1
             (tree-node 2
                        (tree-node 3
                                   (tree-leaf 6))
                        (tree-leaf 4)
                        (tree-leaf 5))))

;;; ====
;;; Tree Type Tests
;;; ====

(test-group "tree-type"
  (define-test "tree? recognizes trees"
    (assert-true (tree? tree-single))
    (assert-true (tree? tree-simple))
    (assert-true (tree? tree-deep)))

  (define-test "tree? rejects non-trees"
    (assert-false (tree? 42))
    (assert-false (tree? '()))
    (assert-false (tree? '(not a tree))))

  (define-test "tree-value extracts value"
    (assert-equal 1 (tree-value tree-single))
    (assert-equal 1 (tree-value tree-simple)))

  (define-test "tree-children returns children"
    (assert-true (null? (tree-children tree-single)))
    (assert-equal 3 (length (tree-children tree-simple))))

  (define-test "tree-leaf creates leaf"
    (let ([leaf (tree-leaf 42)])
      (assert-true (tree? leaf))
      (assert-equal 42 (tree-value leaf))
      (assert-true (tree-leaf? leaf))))

  (define-test "tree-node creates node with children"
    (let ([node (tree-node 1 (tree-leaf 2) (tree-leaf 3))])
      (assert-true (tree? node))
      (assert-equal 1 (tree-value node))
      (assert-equal 2 (length (tree-children node))))))

;;; ====
;;; Tree Utility Tests
;;; ====

(test-group "tree-utilities"
  (define-test "tree-size counts all nodes"
    (assert-equal 1 (tree-size tree-single))
    (assert-equal 4 (tree-size tree-simple))
    (assert-equal 9 (tree-size tree-deep)))

  (define-test "tree-depth measures depth"
    (assert-equal 1 (tree-depth tree-single))
    (assert-equal 2 (tree-depth tree-simple))
    (assert-equal 3 (tree-depth tree-deep))
    (assert-equal 4 (tree-depth tree-asymmetric)))

  (define-test "tree-map transforms all values"
    (let ([doubled (tree-map (lambda (x) (* x 2)) tree-simple)])
      (assert-equal 2 (tree-value doubled))
      (assert-equal 4 (tree-value (car (tree-children doubled))))))

  (define-test "tree-flatten produces preorder list"
    (assert-equal '(1) (tree-flatten tree-single))
    (assert-equal '(1 2 3 4) (tree-flatten tree-simple))
    (assert-equal '(1 2 5 6 3 7 4 8 9) (tree-flatten tree-deep))))

;;; ====
;;; Tree Zipper Type Tests
;;; ====

(test-group "tree-zipper-type"
  (define-test "tree->zipper creates zipper at root"
    (let ([z (tree->zipper tree-simple)])
      (assert-true (tree-zipper? z))
      (assert-true (tree-zipper-at-root? z))
      (assert-equal 1 (tree-zipper-get z))))

  (define-test "zipper->tree reconstructs tree"
    (let* ([z (tree->zipper tree-simple)]
           [t (zipper->tree z)])
      (assert-true (tree-equal? tree-simple t))))

  (define-test "tree-zipper-at-leaf? detects leaves"
    (let ([z (tree->zipper tree-single)])
      (assert-true (tree-zipper-at-leaf? z)))
    (let ([z (tree->zipper tree-simple)])
      (assert-false (tree-zipper-at-leaf? z))))

  (define-test "tree-zipper-depth tracks depth"
    (let ([z (tree->zipper tree-simple)])
      (assert-equal 0 (tree-zipper-depth z))
      (let ([z2 (from-just (tree-zipper-down z))])
        (assert-equal 1 (tree-zipper-depth z2))))))

;;; ====
;;; Navigation Tests
;;; ====

(test-group "navigation"
  (define-test "tree-zipper-down moves to first child"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))])
      (assert-equal 2 (tree-zipper-get z2))))

  (define-test "tree-zipper-down returns nothing at leaf"
    (let ([z (tree->zipper tree-single)])
      (assert-true (nothing? (tree-zipper-down z)))))

  (define-test "tree-zipper-down-left equals down"
    (let* ([z (tree->zipper tree-simple)]
           [left (from-just (tree-zipper-down-left z))]
           [down (from-just (tree-zipper-down z))])
      (assert-true (tree-zipper-equal? left down))))

  (define-test "tree-zipper-down-right moves to last child"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down-right z))])
      (assert-equal 4 (tree-zipper-get z2))))

  (define-test "tree-zipper-up returns to parent"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]
           [z3 (from-just (tree-zipper-up z2))])
      (assert-equal 1 (tree-zipper-get z3))
      (assert-true (tree-zipper-at-root? z3))))

  (define-test "tree-zipper-up returns nothing at root"
    (let ([z (tree->zipper tree-simple)])
      (assert-true (nothing? (tree-zipper-up z)))))

  (define-test "tree-zipper-left moves to left sibling"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]       ; at 2
           [z3 (from-just (tree-zipper-right z2))]     ; at 3
           [z4 (from-just (tree-zipper-left z3))])     ; back at 2
      (assert-equal 2 (tree-zipper-get z4))))

  (define-test "tree-zipper-right moves to right sibling"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]       ; at 2
           [z3 (from-just (tree-zipper-right z2))]     ; at 3
           [z4 (from-just (tree-zipper-right z3))])    ; at 4
      (assert-equal 4 (tree-zipper-get z4))))

  (define-test "tree-zipper-left returns nothing at first sibling"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))])      ; at 2, first child
      (assert-true (nothing? (tree-zipper-left z2)))))

  (define-test "tree-zipper-right returns nothing at last sibling"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down-right z))]) ; at 4, last child
      (assert-true (nothing? (tree-zipper-right z2)))))

  (define-test "tree-zipper-root returns to root"
    (let* ([z (tree->zipper tree-deep)]
           [z2 (from-just (tree-zipper-down z))]       ; at 2
           [z3 (from-just (tree-zipper-down z2))]      ; at 5
           [z4 (tree-zipper-root z3)])
      (assert-true (tree-zipper-at-root? z4))
      (assert-equal 1 (tree-zipper-get z4))))

  (define-test "tree-zipper-nth-child navigates to specific child"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-nth-child z 1))])  ; 0-indexed, so 3
      (assert-equal 3 (tree-zipper-get z2)))
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-nth-child z 2))])  ; 0-indexed, so 4
      (assert-equal 4 (tree-zipper-get z2))))

  (define-test "tree-zipper-nth-child returns nothing for invalid index"
    (let ([z (tree->zipper tree-simple)])
      (assert-true (nothing? (tree-zipper-nth-child z 10)))))

  (define-test "tree-zipper-leftmost moves to first sibling"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down-right z))]  ; at 4, last child
           [z3 (tree-zipper-leftmost z2)])
      (assert-equal 2 (tree-zipper-get z3))))

  (define-test "tree-zipper-rightmost moves to last sibling"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]        ; at 2, first child
           [z3 (tree-zipper-rightmost z2)])
      (assert-equal 4 (tree-zipper-get z3)))))

;;; ====
;;; Navigation Predicate Tests
;;; ====

(test-group "navigation-predicates"
  (define-test "tree-zipper-can-go-up?"
    (let ([z (tree->zipper tree-simple)])
      (assert-false (tree-zipper-can-go-up? z)))
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))])
      (assert-true (tree-zipper-can-go-up? z2))))

  (define-test "tree-zipper-can-go-down?"
    (let ([z (tree->zipper tree-simple)])
      (assert-true (tree-zipper-can-go-down? z)))
    (let ([z (tree->zipper tree-single)])
      (assert-false (tree-zipper-can-go-down? z))))

  (define-test "tree-zipper-can-go-left?"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))])      ; first child
      (assert-false (tree-zipper-can-go-left? z2)))
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]
           [z3 (from-just (tree-zipper-right z2))])    ; middle child
      (assert-true (tree-zipper-can-go-left? z3))))

  (define-test "tree-zipper-can-go-right?"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down-right z))]) ; last child
      (assert-false (tree-zipper-can-go-right? z2)))
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))])       ; first child
      (assert-true (tree-zipper-can-go-right? z2)))))

;;; ====
;;; Focus Operation Tests
;;; ====

(test-group "focus-operations"
  (define-test "tree-zipper-get returns focused value"
    (let ([z (tree->zipper tree-simple)])
      (assert-equal 1 (tree-zipper-get z))))

  (define-test "tree-zipper-set updates focused value"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (tree-zipper-set z 100)])
      (assert-equal 100 (tree-zipper-get z2))
      ;; Original unchanged (functional)
      (assert-equal 1 (tree-zipper-get z))))

  (define-test "tree-zipper-modify applies function"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (tree-zipper-modify z (lambda (x) (* x 10)))])
      (assert-equal 10 (tree-zipper-get z2))))

  (define-test "tree-zipper-set-tree replaces subtree"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]       ; at node 2
           [z3 (tree-zipper-set-tree z2 (tree-node 20 (tree-leaf 21)))]
           [root (tree-zipper-root z3)]
           [tree (zipper->tree root)])
      (assert-equal 20 (tree-value (car (tree-children tree))))))

  (define-test "tree-zipper-children-count returns child count"
    (let ([z (tree->zipper tree-simple)])
      (assert-equal 3 (tree-zipper-children-count z)))
    (let ([z (tree->zipper tree-single)])
      (assert-equal 0 (tree-zipper-children-count z)))))

;;; ====
;;; Insertion Tests
;;; ====

(test-group "insertion"
  (define-test "tree-zipper-insert-child-left adds leftmost child"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (tree-zipper-insert-child-left z (tree-leaf 0))]
           [first-child (from-just (tree-zipper-down z2))])
      (assert-equal 0 (tree-zipper-get first-child))
      (assert-equal 4 (tree-zipper-children-count z2))))

  (define-test "tree-zipper-insert-child-right adds rightmost child"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (tree-zipper-insert-child-right z (tree-leaf 5))]
           [last-child (from-just (tree-zipper-down-right z2))])
      (assert-equal 5 (tree-zipper-get last-child))
      (assert-equal 4 (tree-zipper-children-count z2))))

  (define-test "tree-zipper-insert-left adds left sibling"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]        ; at 2
           [z3 (from-just (tree-zipper-right z2))]      ; at 3
           [z4 (from-just (tree-zipper-insert-left z3 (tree-leaf 99)))]
           [z5 (from-just (tree-zipper-left z4))])
      (assert-equal 99 (tree-zipper-get z5))))

  (define-test "tree-zipper-insert-right adds right sibling"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]        ; at 2
           [z3 (from-just (tree-zipper-insert-right z2 (tree-leaf 99)))]
           [z4 (from-just (tree-zipper-right z3))])
      (assert-equal 99 (tree-zipper-get z4))))

  (define-test "tree-zipper-insert-left returns nothing at root"
    (let ([z (tree->zipper tree-simple)])
      (assert-true (nothing? (tree-zipper-insert-left z (tree-leaf 0))))))

  (define-test "tree-zipper-insert-right returns nothing at root"
    (let ([z (tree->zipper tree-simple)])
      (assert-true (nothing? (tree-zipper-insert-right z (tree-leaf 0)))))))

;;; ====
;;; Deletion Tests
;;; ====

(test-group "deletion"
  (define-test "tree-zipper-delete removes focused node"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]        ; at 2
           [z3 (from-just (tree-zipper-delete z2))]     ; delete 2
           [root (tree-zipper-root z3)]
           [tree (zipper->tree root)])
      ;; Focus should move to right sibling (3)
      (assert-equal 3 (tree-zipper-get z3))
      ;; Tree should have 2 children now
      (assert-equal 2 (length (tree-children tree)))))

  (define-test "tree-zipper-delete moves focus to left if no right"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down-right z))]  ; at 4 (rightmost)
           [z3 (from-just (tree-zipper-delete z2))])    ; delete 4
      (assert-equal 3 (tree-zipper-get z3))))

  (define-test "tree-zipper-delete moves focus to parent if only child"
    (let* ([tree (tree-node 1 (tree-leaf 2))]           ; single child
           [z (tree->zipper tree)]
           [z2 (from-just (tree-zipper-down z))]        ; at 2
           [z3 (from-just (tree-zipper-delete z2))])    ; delete 2
      (assert-equal 1 (tree-zipper-get z3))
      (assert-true (tree-zipper-at-leaf? z3))))

  (define-test "tree-zipper-delete returns nothing at root"
    (let ([z (tree->zipper tree-simple)])
      (assert-true (nothing? (tree-zipper-delete z)))))

  (define-test "tree-zipper-delete-children makes node a leaf"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (tree-zipper-delete-children z)])
      (assert-true (tree-zipper-at-leaf? z2))
      (assert-equal 1 (tree-zipper-get z2)))))

;;; ====
;;; Traversal Tests
;;; ====

(test-group "traversal"
  (define-test "tree-zipper-next-preorder visits in preorder"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-next-preorder z))]
           [z3 (from-just (tree-zipper-next-preorder z2))]
           [z4 (from-just (tree-zipper-next-preorder z3))])
      (assert-equal 2 (tree-zipper-get z2))
      (assert-equal 3 (tree-zipper-get z3))
      (assert-equal 4 (tree-zipper-get z4))))

  (define-test "tree-zipper-next-preorder returns nothing at end"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down-right z))])  ; at 4, last in preorder
      (assert-true (nothing? (tree-zipper-next-preorder z2)))))

  (define-test "tree-zipper-preorder returns all zippers"
    (let* ([z (tree->zipper tree-simple)]
           [zippers (tree-zipper-preorder z)])
      (assert-equal 4 (length zippers))
      (assert-equal '(1 2 3 4) (map tree-zipper-get zippers))))

  (define-test "tree-zipper-preorder on deeper tree"
    (let* ([z (tree->zipper tree-deep)]
           [zippers (tree-zipper-preorder z)])
      (assert-equal 9 (length zippers))
      (assert-equal '(1 2 5 6 3 7 4 8 9) (map tree-zipper-get zippers)))))

;;; ====
;;; Path Operation Tests
;;; ====

(test-group "path-operations"
  (define-test "tree-zipper-path returns path from root"
    (let* ([z (tree->zipper tree-deep)]
           [z2 (from-just (tree-zipper-down z))]       ; at 2
           [z3 (from-just (tree-zipper-down z2))])     ; at 5
      (assert-equal '(1) (tree-zipper-path z))
      (assert-equal '(1 2) (tree-zipper-path z2))
      (assert-equal '(1 2 5) (tree-zipper-path z3))))

  (define-test "tree-zipper-sibling-index returns index among siblings"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]        ; at 2, index 0
           [z3 (from-just (tree-zipper-right z2))]      ; at 3, index 1
           [z4 (from-just (tree-zipper-right z3))])     ; at 4, index 2
      (assert-true (nothing? (tree-zipper-sibling-index z)))  ; root has no index
      (assert-equal 0 (from-just (tree-zipper-sibling-index z2)))
      (assert-equal 1 (from-just (tree-zipper-sibling-index z3)))
      (assert-equal 2 (from-just (tree-zipper-sibling-index z4))))))

;;; ====
;;; Functor Tests
;;; ====

(test-group "functor"
  (define-test "tree-zipper-map preserves structure"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]       ; at 2
           [z3 (tree-zipper-map (lambda (x) (* x 10)) z2)]
           [tree (zipper->tree (tree-zipper-root z3))])
      (assert-equal 20 (tree-zipper-get z3))
      (assert-equal '(10 20 30 40) (tree-flatten tree))))

  (define-test "tree-zipper-map identity law"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (tree-zipper-map (lambda (x) x) z)])
      (assert-true (tree-zipper-equal? z z2)))))

;;; ====
;;; Comonad Tests
;;; ====

(test-group "comonad"
  (define-test "tree-zipper-extract gets focused value"
    (let ([z (tree->zipper tree-simple)])
      (assert-equal 1 (tree-zipper-extract z))))

  (define-test "tree-zipper-extend applies at each position"
    ;; Use extend to count depth at each position
    (let* ([z (tree->zipper tree-simple)]
           [z2 (tree-zipper-extend tree-zipper-depth z)])
      ;; Root has depth 0
      (assert-equal 0 (tree-zipper-get z2))
      ;; Children have depth 1
      (let ([child (from-just (tree-zipper-down z2))])
        (assert-equal 1 (tree-zipper-get child)))))

  (define-test "tree-zipper-duplicate creates zipper of zippers"
    (let* ([z (tree->zipper tree-simple)]
           [z2 (tree-zipper-duplicate z)])
      ;; Extract from duplicate gives original zipper
      (assert-true (tree-zipper-equal? z (tree-zipper-extract z2)))))

  (define-test "comonad law: extract . extend f = f"
    (let* ([z (tree->zipper tree-simple)]
           [f (lambda (zz) (+ 1 (tree-zipper-get zz)))]
           [extended (tree-zipper-extend f z)])
      (assert-equal (f z) (tree-zipper-extract extended))))

  ;; Test that fixes the bug identified by Gemini QA:
  ;; Trees with duplicate sibling values must preserve correct focus.
  (define-test "comonad law: extend extract = id (with duplicate values)"
    ;; Tree with duplicate sibling values: (parent (A) (A) (A))
    ;; The old value-based navigation would always return to the first A.
    (let* ([tree-with-dupes (tree-node 'parent
                                       (tree-leaf 'A)
                                       (tree-leaf 'A)
                                       (tree-leaf 'A))]
           [z (tree->zipper tree-with-dupes)]
           ;; Navigate to the SECOND 'A child (index 1)
           [z-second (from-just (tree-zipper-nth-child z 1))]
           ;; Apply extend extract - this should be identity
           [z-result (tree-zipper-extend tree-zipper-extract z-second)])
      ;; Verify we're still focused on the SECOND sibling (index 1)
      (assert-equal 1 (from-just (tree-zipper-sibling-index z-result)))
      ;; And the trees are equal
      (assert-true (tree-zipper-equal? z-second z-result))))

  (define-test "comonad law: extend extract = id (with duplicate values - third sibling)"
    ;; Same test but for the third sibling
    (let* ([tree-with-dupes (tree-node 'parent
                                       (tree-leaf 'A)
                                       (tree-leaf 'A)
                                       (tree-leaf 'A))]
           [z (tree->zipper tree-with-dupes)]
           ;; Navigate to the THIRD 'A child (index 2)
           [z-third (from-just (tree-zipper-nth-child z 2))]
           ;; Apply extend extract - this should be identity
           [z-result (tree-zipper-extend tree-zipper-extract z-third)])
      ;; Verify we're still focused on the THIRD sibling (index 2)
      (assert-equal 2 (from-just (tree-zipper-sibling-index z-result)))
      ;; And the trees are equal
      (assert-true (tree-zipper-equal? z-third z-result))))

  (define-test "tree-zipper-index-path returns correct indices"
    (let* ([z (tree->zipper tree-deep)]
           [z-down (from-just (tree-zipper-down z))]
           [z-right (from-just (tree-zipper-right z-down))]
           [z-deep (from-just (tree-zipper-down z-right))])
      ;; Path to root is empty
      (assert-equal '() (tree-zipper-index-path z))
      ;; Path to first child is (0)
      (assert-equal '(0) (tree-zipper-index-path z-down))
      ;; Path to second child is (1)
      (assert-equal '(1) (tree-zipper-index-path z-right))
      ;; Path to first child of second child is (1 0)
      (assert-equal '(1 0) (tree-zipper-index-path z-deep)))))

;;; ====
;;; Equality and Display Tests
;;; ====

(test-group "equality-display"
  (define-test "tree-equal? recognizes equal trees"
    (assert-true (tree-equal? tree-simple tree-simple))
    (assert-true (tree-equal?
                  (tree-node 1 (tree-leaf 2))
                  (tree-node 1 (tree-leaf 2)))))

  (define-test "tree-equal? recognizes different trees"
    (assert-false (tree-equal? tree-simple tree-single))
    (assert-false (tree-equal?
                   (tree-node 1 (tree-leaf 2))
                   (tree-node 1 (tree-leaf 3)))))

  (define-test "tree-zipper-equal? recognizes equal zippers"
    (let ([z1 (tree->zipper tree-simple)]
          [z2 (tree->zipper tree-simple)])
      (assert-true (tree-zipper-equal? z1 z2))))

  (define-test "tree->string produces readable output"
    (let ([str (tree->string tree-simple)])
      (assert-true (string? str))))

  (define-test "tree-zipper->string produces readable output"
    (let* ([z (tree->zipper tree-simple)]
           [str (tree-zipper->string z)])
      (assert-true (string? str)))))

;;; ====
;;; Round-Trip Tests
;;; ====

(test-group "round-trip"
  (define-test "navigation round-trip preserves tree"
    ;; Go down, modify, go up, verify tree is correctly updated
    (let* ([z (tree->zipper tree-simple)]
           [z2 (from-just (tree-zipper-down z))]
           [z3 (tree-zipper-set z2 100)]
           [z4 (from-just (tree-zipper-up z3))]
           [tree (zipper->tree z4)])
      (assert-equal 100 (tree-value (car (tree-children tree))))))

  (define-test "complex navigation preserves tree"
    ;; Navigate around, make changes, verify tree integrity
    (let* ([z (tree->zipper tree-deep)]
           [z2 (from-just (tree-zipper-down z))]         ; at 2
           [z3 (from-just (tree-zipper-right z2))]       ; at 3
           [z4 (from-just (tree-zipper-down z3))]        ; at 7
           [z5 (tree-zipper-set z4 70)]                  ; change to 70
           [tree (zipper->tree (tree-zipper-root z5))])
      (assert-equal 9 (tree-size tree))
      ;; Find 70 in flattened tree (member returns truthy tail, not #t)
      (assert-true (if (member 70 (tree-flatten tree)) #t #f)))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
