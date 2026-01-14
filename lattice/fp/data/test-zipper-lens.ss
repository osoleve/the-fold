;;; lattice/fp/data/test-zipper-lens.ss — Tests for Zipper-Lens Integration
;;;
;;; Comprehensive tests for lens operations on zippers and the
;;; connection between zipper navigation and lens composition.

(load "core/testing/test-framework.ss")
(load "lattice/fp/data/zipper-lens.ss")

;;; ====
;;; Test: List Zipper Lenses
;;; ====

(test-group "zipper-focus-lens"
  (define-test "view focus through lens"
    (let ([z (list->zipper '(1 2 3 4 5))])
      (let ([z2 (from-just (zipper-right! z))])  ; Focus on 2
        (assert-equal 2 (view zipper-focus-lens z2)))))

  (define-test "set focus through lens"
    (let ([z (list->zipper '(1 2 3 4 5))])
      (let* ([z2 (from-just (zipper-right! z))]  ; Focus on 2
             [z3 (set-lens zipper-focus-lens 99 z2)])
        (assert-equal 99 (zipper-focus z3))
        (assert-equal '(1 99 3 4 5) (zipper->list z3)))))

  (define-test "over focus through lens"
    (let ([z (list->zipper '(10 20 30 40 50))])
      (let* ([z2 (from-just (zipper-right! z))]  ; Focus on 20
             [z3 (over zipper-focus-lens (lambda (x) (* x 2)) z2)])
        (assert-equal 40 (zipper-focus z3))
        (assert-equal '(10 40 30 40 50) (zipper->list z3))))))

(test-group "zipper-focus-maybe-lens"
  (define-test "view focus-maybe on empty zipper"
    (assert-true (nothing? (view zipper-focus-maybe-lens zipper-empty))))

  (define-test "view focus-maybe on non-empty zipper"
    (let ([z (list->zipper '(1 2 3))])
      (let ([result (view zipper-focus-maybe-lens z)])
        (assert-true (just? result))
        (assert-equal 1 (from-just result)))))

  (define-test "set focus-maybe to nothing"
    (let* ([z (list->zipper '(1 2 3))]
           [z2 (set-lens zipper-focus-maybe-lens nothing z)])
      (assert-false (zipper-has-focus? z2))
      (assert-equal '(2 3) (zipper-right z2)))))

(test-group "zipper-left-lens and zipper-right-lens"
  (define-test "view left context"
    (let* ([z (list->zipper '(1 2 3 4 5))]
           [z2 (zipper-goto z 2)])  ; Focus on 3
      (assert-equal '(2 1) (view zipper-left-lens z2))))

  (define-test "view right context"
    (let* ([z (list->zipper '(1 2 3 4 5))]
           [z2 (zipper-goto z 2)])  ; Focus on 3
      (assert-equal '(4 5) (view zipper-right-lens z2))))

  (define-test "set left context"
    (let* ([z (list->zipper '(1 2 3 4 5))]
           [z2 (zipper-goto z 2)]  ; Focus on 3
           [z3 (set-lens zipper-left-lens '(99 88) z2)])
      (assert-equal '(88 99 3 4 5) (zipper->list z3)))))

(test-group "zipper-position-lens"
  (define-test "view position"
    (let* ([z (list->zipper '(a b c d e))]
           [z2 (zipper-goto z 3)])
      (assert-equal 3 (view zipper-position-lens z2))))

  (define-test "set position moves zipper"
    (let* ([z (list->zipper '(a b c d e))]
           [z2 (set-lens zipper-position-lens 3 z)])
      (assert-equal 'd (zipper-focus z2))
      (assert-equal 3 (zipper-position z2)))))

;;; ====
;;; Test: Tree Zipper Lenses
;;; ====

(test-group "tree-zipper-value-lens"
  (define-test "view tree value"
    (let* ([tree (tree-node 'root
                   (tree-leaf 'a)
                   (tree-leaf 'b))]
           [z (tree->zipper tree)])
      (assert-equal 'root (view tree-zipper-value-lens z))))

  (define-test "set tree value"
    (let* ([tree (tree-node 'root (tree-leaf 'a))]
           [z (tree->zipper tree)]
           [z2 (set-lens tree-zipper-value-lens 'new-root z)])
      (assert-equal 'new-root (tree-zipper-get z2))))

  (define-test "over tree value"
    (let* ([tree (tree-node 10 (tree-leaf 20))]
           [z (tree->zipper tree)]
           [z2 (over tree-zipper-value-lens (lambda (x) (* x 2)) z)])
      (assert-equal 20 (tree-zipper-get z2)))))

(test-group "tree-zipper-children-lens"
  (define-test "view children"
    (let* ([tree (tree-node 'root
                   (tree-leaf 'a)
                   (tree-leaf 'b)
                   (tree-leaf 'c))]
           [z (tree->zipper tree)]
           [children (view tree-zipper-children-lens z)])
      (assert-equal 3 (length children))))

  (define-test "set children replaces all children"
    (let* ([tree (tree-node 'root (tree-leaf 'old))]
           [z (tree->zipper tree)]
           [new-children (list (tree-leaf 'new1) (tree-leaf 'new2))]
           [z2 (set-lens tree-zipper-children-lens new-children z)])
      (assert-equal 2 (tree-zipper-children-count z2)))))

(test-group "tree-zipper-focus-lens"
  (define-test "view focused subtree"
    (let* ([tree (tree-node 'root
                   (tree-node 'child
                     (tree-leaf 'grandchild)))]
           [z (tree->zipper tree)]
           [z2 (from-just (tree-zipper-down z))]
           [subtree (view tree-zipper-focus-lens z2)])
      (assert-true (tree? subtree))
      (assert-equal 'child (tree-value subtree))))

  (define-test "set focused subtree"
    (let* ([tree (tree-node 'root (tree-leaf 'old))]
           [z (tree->zipper tree)]
           [z2 (from-just (tree-zipper-down z))]
           [z3 (set-lens tree-zipper-focus-lens (tree-leaf 'new) z2)])
      (assert-equal 'new (tree-zipper-get z3)))))

;;; ====
;;; Test: Affine (Partial Lens) Operations
;;; ====

(test-group "affine-basics"
  (define-test "preview-affine on successful navigation"
    (let* ([z (list->zipper '(1 2 3))]
           [result (preview-affine zipper-right-affine z)])
      (assert-true (just? result))
      (assert-equal 2 (zipper-focus (from-just result)))))

  (define-test "preview-affine on failed navigation"
    (let* ([z (list->zipper '(1 2 3))]
           [result (preview-affine zipper-left-affine z)])
      (assert-true (nothing? result))))

  (define-test "set-affine when focus exists"
    (let* ([z (list->zipper '(1 2 3))]
           [z2 (set-affine zipper-right-affine
                           (list->zipper '(99))  ; New zipper
                           z)])
      ;; The affine replaces with the new zipper
      (assert-equal 99 (zipper-focus z2))))

  (define-test "set-affine returns unchanged when no focus"
    (let* ([z (list->zipper '(1 2 3))]
           [z2 (set-affine zipper-left-affine
                           (list->zipper '(99))
                           z)])
      ;; No change since left navigation failed
      (assert-equal 1 (zipper-focus z2)))))

(test-group "zipper-nth-affine"
  (define-test "access nth element forward"
    (let* ([z (list->zipper '(a b c d e))]
           [affine (zipper-nth-affine 2)]
           [result (preview-affine affine z)])
      (assert-true (just? result))
      (assert-equal 'c (from-just result))))

  (define-test "access nth element out of bounds"
    (let* ([z (list->zipper '(a b c))]
           [affine (zipper-nth-affine 10)]
           [result (preview-affine affine z)])
      (assert-true (nothing? result)))))

;;; ====
;;; Test: Tree Navigation Affines
;;; ====

(test-group "tree-navigation-affines"
  (define-test "tree-down-affine success"
    (let* ([tree (tree-node 'root (tree-leaf 'child))]
           [z (tree->zipper tree)]
           [result (preview-affine tree-down-affine z)])
      (assert-true (just? result))
      (assert-equal 'child (tree-zipper-get (from-just result)))))

  (define-test "tree-down-affine on leaf fails"
    (let* ([tree (tree-leaf 'alone)]
           [z (tree->zipper tree)]
           [result (preview-affine tree-down-affine z)])
      (assert-true (nothing? result))))

  (define-test "tree-up-affine from child"
    (let* ([tree (tree-node 'root (tree-leaf 'child))]
           [z (tree->zipper tree)]
           [z2 (from-just (tree-zipper-down z))]
           [result (preview-affine tree-up-affine z2)])
      (assert-true (just? result))
      (assert-equal 'root (tree-zipper-get (from-just result)))))

  (define-test "tree-up-affine at root fails"
    (let* ([tree (tree-leaf 'root)]
           [z (tree->zipper tree)]
           [result (preview-affine tree-up-affine z)])
      (assert-true (nothing? result))))

  (define-test "tree-nth-child-affine"
    (let* ([tree (tree-node 'root
                   (tree-leaf 'a)
                   (tree-leaf 'b)
                   (tree-leaf 'c))]
           [z (tree->zipper tree)]
           [affine (tree-nth-child-affine 1)]
           [result (preview-affine affine z)])
      (assert-true (just? result))
      (assert-equal 'b (tree-zipper-get (from-just result))))))

;;; ====
;;; Test: Zipper to Lens Conversion
;;; ====

(test-group "zipper-to-lens"
  (define-test "zipper at position 0 gives lens-nth 0"
    (let* ([z (list->zipper '(a b c d e))]
           [lens (zipper-to-lens z)]
           [data '(1 2 3 4 5)])
      (assert-equal 1 (view lens data))))

  (define-test "zipper at position 2 gives lens-nth 2"
    (let* ([z (zipper-goto (list->zipper '(a b c d e)) 2)]
           [lens (zipper-to-lens z)]
           [data '(10 20 30 40 50)])
      (assert-equal 30 (view lens data))))

  (define-test "lens from zipper can modify"
    (let* ([z (zipper-goto (list->zipper '(a b c d e)) 1)]
           [lens (zipper-to-lens z)]
           [data '(1 2 3 4 5)]
           [modified (set-lens lens 99 data)])
      (assert-equal '(1 99 3 4 5) modified))))

(test-group "tree-path-to-lens"
  (define-test "empty path focuses on root value"
    (let* ([tree (tree-node 'root (tree-leaf 'child))]
           [lens (tree-path-to-lens '())])
      (assert-equal 'root (view lens tree))))

  (define-test "single index path focuses on child"
    (let* ([tree (tree-node 'root
                   (tree-leaf 'a)
                   (tree-leaf 'b))]
           [lens (tree-path-to-lens '(1))])
      (assert-equal 'b (view lens tree))))

  (define-test "nested path focuses deep"
    (let* ([tree (tree-node 'root
                   (tree-node 'child
                     (tree-leaf 'deep)))]
           [lens (tree-path-to-lens '(0 0))])
      (assert-equal 'deep (view lens tree))))

  (define-test "tree-path-to-lens can modify deep value"
    (let* ([tree (tree-node 'root
                   (tree-node 'child
                     (tree-leaf 'old)))]
           [lens (tree-path-to-lens '(0 0))]
           [modified (set-lens lens 'new tree)])
      ;; Verify the new value is in place
      (let* ([z (tree->zipper modified)]
             [z2 (from-just (tree-zipper-down z))]
             [z3 (from-just (tree-zipper-down z2))])
        (assert-equal 'new (tree-zipper-get z3))))))

;;; ====
;;; Test: Comonad-Lens Integration
;;; ====

(test-group "extend-with-lens"
  (define-test "extend with focus lens"
    (let* ([z (list->zipper '(1 2 3 4 5))]
           [result (extend-with-lens zipper-focus-lens
                     (lambda (val _z) (* val 10))
                     z)])
      (assert-equal '(10 20 30 40 50) (zipper->list result)))))

(test-group "neighbor-lens"
  (define-test "view neighbors at middle"
    (let* ([z (zipper-goto (list->zipper '(1 2 3 4 5)) 2)]
           [neighbors (view neighbor-lens z)])
      (assert-equal '(2 4) neighbors)))

  (define-test "view neighbors at start"
    (let* ([z (list->zipper '(1 2 3 4 5))]
           [neighbors (view neighbor-lens z)])
      (assert-equal '(2) neighbors)))

  (define-test "view neighbors at end"
    (let* ([z (zipper-end (list->zipper '(1 2 3 4 5)))]
           [neighbors (view neighbor-lens z)])
      (assert-equal '(4) neighbors))))

(test-group "window-lens"
  (define-test "window of 1-1 at middle"
    (let* ([z (zipper-goto (list->zipper '(1 2 3 4 5)) 2)]
           [lens (window-lens 1 1)]
           [window (view lens z)])
      (assert-equal '(2 3 4) window)))

  (define-test "window of 2-2 at middle"
    (let* ([z (zipper-goto (list->zipper '(1 2 3 4 5)) 2)]
           [lens (window-lens 2 2)]
           [window (view lens z)])
      (assert-equal '(1 2 3 4 5) window)))

  (define-test "window clamps at edges"
    (let* ([z (list->zipper '(1 2 3 4 5))]
           [lens (window-lens 3 3)]
           [window (view lens z)])
      ;; At position 0, can't go left 3
      (assert-equal '(1 2 3 4) window))))

;;; ====
;;; Test: Traversal Utilities
;;; ====

(test-group "zipper-traversal-utilities"
  (define-test "zipper-all? true case"
    (let ([z (list->zipper '(2 4 6 8))])
      (assert-true (zipper-all? even? z))))

  (define-test "zipper-all? false case"
    (let ([z (list->zipper '(2 4 5 8))])
      (assert-false (zipper-all? even? z))))

  (define-test "zipper-any? true case"
    (let ([z (list->zipper '(1 3 4 7))])
      (assert-true (zipper-any? even? z))))

  (define-test "zipper-any? false case"
    (let ([z (list->zipper '(1 3 5 7))])
      (assert-false (zipper-any? even? z))))

  (define-test "zipper-collect with filter"
    (let* ([z (list->zipper '(1 2 3 4 5 6))]
           [evens (zipper-collect
                    (lambda (x) (if (even? x) (just x) nothing))
                    z)])
      (assert-equal '(2 4 6) evens))))

;;; ====
;;; Test: Composed Lens Paths
;;; ====

(test-group "at-focus"
  (define-test "at-focus is alias for zipper-focus-lens"
    (let ([z (list->zipper '(1 2 3))])
      (assert-equal (view at-focus z)
                    (view zipper-focus-lens z)))))

(test-group "at-left"
  (define-test "at-left 1 gets immediate left"
    (let ([z (zipper-goto (list->zipper '(a b c d e)) 2)])
      (assert-equal 'b (view (at-left 1) z))))

  (define-test "at-left 2 gets second to left"
    (let ([z (zipper-goto (list->zipper '(a b c d e)) 2)])
      (assert-equal 'a (view (at-left 2) z))))

  (define-test "set at-left modifies correct element"
    (let* ([z (zipper-goto (list->zipper '(a b c d e)) 2)]
           [z2 (set-lens (at-left 1) 'X z)])
      (assert-equal '(a X c d e) (zipper->list z2))))

  (define-test "at-left 0 errors with helpful message"
    (assert-error (lambda () (at-left 0)))))

(test-group "at-right"
  (define-test "at-right 1 gets immediate right"
    (let ([z (zipper-goto (list->zipper '(a b c d e)) 2)])
      (assert-equal 'd (view (at-right 1) z))))

  (define-test "at-right 2 gets second to right"
    (let ([z (zipper-goto (list->zipper '(a b c d e)) 2)])
      (assert-equal 'e (view (at-right 2) z))))

  (define-test "set at-right modifies correct element"
    (let* ([z (zipper-goto (list->zipper '(a b c d e)) 2)]
           [z2 (set-lens (at-right 1) 'X z)])
      (assert-equal '(a b c X e) (zipper->list z2))))

  (define-test "at-right 0 errors with helpful message"
    (assert-error (lambda () (at-right 0)))))

;;; ====
;;; Test: Edge Cases (from QA review)
;;; ====

(test-group "edge-cases"
  (define-test "position-lens clamping on out-of-bounds"
    ;; zipper-goto clamps to valid range
    (let* ([z (list->zipper '(a b c))]
           [z2 (set-lens zipper-position-lens 100 z)])
      ;; Should clamp to valid position (past-end state)
      (assert-true (>= (zipper-position z2) 0))))

  (define-test "zipper-to-lens captures position at creation time"
    ;; The lens is a snapshot - modifying the list doesn't affect it
    (let* ([z (zipper-goto (list->zipper '(a b c d e)) 2)]
           [lens (zipper-to-lens z)]
           [data1 '(1 2 3 4 5)]
           [data2 '(x y z)])
      ;; Same lens works on different lists (if long enough)
      (assert-equal 3 (view lens data1))
      (assert-equal 'z (view lens data2))))

  (define-test "tree-zipper-depth-lens is read-only"
    ;; Setting depth is a no-op (documented behavior)
    (let* ([tree (tree-node 'root (tree-node 'child (tree-leaf 'deep)))]
           [z (tree->zipper tree)]
           [z2 (from-just (tree-zipper-down z))]
           [z3 (set-lens tree-zipper-depth-lens 999 z2)])
      ;; Depth unchanged because setter is identity
      (assert-equal (tree-zipper-depth z2) (tree-zipper-depth z3)))))

;;; ====
;;; Test: Lens Laws for Zipper Lenses
;;; ====

(test-group "lens-laws-zipper-focus"
  (define-test "get-put law: set l (view l s) s = s"
    (let* ([z (list->zipper '(1 2 3))]
           [z2 (from-just (zipper-right! z))]
           [v (view zipper-focus-lens z2)]
           [z3 (set-lens zipper-focus-lens v z2)])
      (assert-true (zipper-equal? z2 z3))))

  (define-test "put-get law: view l (set l a s) = a"
    (let* ([z (list->zipper '(1 2 3))]
           [z2 (from-just (zipper-right! z))]
           [z3 (set-lens zipper-focus-lens 99 z2)]
           [v (view zipper-focus-lens z3)])
      (assert-equal 99 v)))

  (define-test "put-put law: set l a' (set l a s) = set l a' s"
    (let* ([z (list->zipper '(1 2 3))]
           [z2 (from-just (zipper-right! z))]
           [z3 (set-lens zipper-focus-lens 50 z2)]
           [z4 (set-lens zipper-focus-lens 99 z3)]
           [z5 (set-lens zipper-focus-lens 99 z2)])
      (assert-true (zipper-equal? z4 z5)))))

(test-group "lens-laws-tree-value"
  (define-test "get-put law for tree value lens"
    (let* ([tree (tree-node 'root (tree-leaf 'child))]
           [z (tree->zipper tree)]
           [v (view tree-zipper-value-lens z)]
           [z2 (set-lens tree-zipper-value-lens v z)])
      (assert-true (tree-zipper-equal? z z2))))

  (define-test "put-get law for tree value lens"
    (let* ([tree (tree-node 'root (tree-leaf 'child))]
           [z (tree->zipper tree)]
           [z2 (set-lens tree-zipper-value-lens 'new-root z)]
           [v (view tree-zipper-value-lens z2)])
      (assert-equal 'new-root v))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
