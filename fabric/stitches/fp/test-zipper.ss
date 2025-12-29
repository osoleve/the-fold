;;; fabric/stitches/fp/test-zipper.ss — Tests for Zipper Data Structures

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/zipper.ss")

(display "
")
(display "==============================================================
")
(display "         ZIPPER DATA STRUCTURE TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; List Zipper Construction Tests
;;; ============================================================

(test-group list-zipper-construction
            (define-test list-to-zipper-empty-test
              (assert-true (nothing? (list-to-zipper '()))))
            
            (define-test list-to-zipper-single-test
              (let ([z (from-just (list-to-zipper '(42)))])
                   (assert-equal 42 (list-zipper-focus z))
                   (assert-equal '() (list-zipper-left z))
                   (assert-equal '() (list-zipper-right z))))
            
            (define-test list-to-zipper-multiple-test
              (let ([z (from-just (list-to-zipper '(1 2 3 4 5)))])
                   (assert-equal 1 (list-zipper-focus z))
                   (assert-equal '() (list-zipper-left z))
                   (assert-equal '(2 3 4 5) (list-zipper-right z))))
            
            (define-test zipper-to-list-roundtrip-test
              (let* ([lst '(1 2 3 4 5)]
                     [z (from-just (list-to-zipper lst))])
                    (assert-equal lst (zipper-to-list z)))))

;;; ============================================================
;;; List Zipper Movement Tests
;;; ============================================================

(test-group list-zipper-movement
            (define-test move-right-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3)))]
                     [z2 (from-just (list-zipper-move-right z1))])
                    (assert-equal 2 (list-zipper-focus z2))
                    (assert-equal '(1) (list-zipper-left z2))
                    (assert-equal '(3) (list-zipper-right z2))))
            
            (define-test move-left-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3)))]
                     [z2 (from-just (list-zipper-move-right z1))]
                     [z3 (from-just (list-zipper-move-left z2))])
                    (assert-equal 1 (list-zipper-focus z3))))
            
            (define-test move-right-at-end-test
              (let* ([z (from-just (list-to-zipper '(1)))])
                    (assert-true (nothing? (list-zipper-move-right z)))))
            
            (define-test move-left-at-start-test
              (let* ([z (from-just (list-to-zipper '(1 2 3)))])
                    (assert-true (nothing? (list-zipper-move-left z)))))
            
            (define-test move-multiple-right-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3 4 5)))]
                     [z2 (from-just (list-zipper-move-right z1))]
                     [z3 (from-just (list-zipper-move-right z2))]
                     [z4 (from-just (list-zipper-move-right z3))])
                    (assert-equal 4 (list-zipper-focus z4))))
            
            (define-test move-preserves-list-test
              (let* ([lst '(a b c d e)]
                     [z1 (from-just (list-to-zipper lst))]
                     [z2 (from-just (list-zipper-move-right z1))]
                     [z3 (from-just (list-zipper-move-right z2))]
                     [z4 (from-just (list-zipper-move-left z3))])
                    (assert-equal lst (zipper-to-list z4)))))

;;; ============================================================
;;; List Zipper Modification Tests
;;; ============================================================

(test-group list-zipper-modification
            (define-test set-focus-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3)))]
                     [z2 (list-zipper-set 100 z1)])
                    (assert-equal 100 (list-zipper-focus z2))
                    (assert-equal '(100 2 3) (zipper-to-list z2))))
            
            (define-test modify-focus-test
              (let* ([z1 (from-just (list-to-zipper '(10 20 30)))]
                     [z2 (from-just (list-zipper-move-right z1))]
                     [z3 (list-zipper-modify (lambda (x) (* x 2)) z2)])
                    (assert-equal 40 (list-zipper-focus z3))
                    (assert-equal '(10 40 30) (zipper-to-list z3))))
            
            (define-test insert-left-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3)))]
                     [z2 (from-just (list-zipper-move-right z1))]
                     [z3 (list-zipper-insert-left 99 z2)])
                    (assert-equal 2 (list-zipper-focus z3))
                    (assert-equal '(1 99 2 3) (zipper-to-list z3))))
            
            (define-test insert-right-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3)))]
                     [z2 (list-zipper-insert-right 99 z1)])
                    (assert-equal 1 (list-zipper-focus z2))
                    (assert-equal '(1 99 2 3) (zipper-to-list z2))))
            
            (define-test delete-right-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3)))]
                     [z2 (from-just (list-zipper-delete-right z1))])
                    (assert-equal '(1 3) (zipper-to-list z2))))
            
            (define-test delete-right-at-end-test
              (let* ([z (from-just (list-to-zipper '(1)))])
                    (assert-true (nothing? (list-zipper-delete-right z))))))

;;; ============================================================
;;; List Zipper Navigation Tests
;;; ============================================================

(test-group list-zipper-navigation
            (define-test start-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3 4 5)))]
                     [z2 (from-just (list-zipper-move-right z1))]
                     [z3 (from-just (list-zipper-move-right z2))]
                     [z4 (list-zipper-start z3)])
                    (assert-equal 1 (list-zipper-focus z4))))
            
            (define-test end-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3 4 5)))]
                     [z2 (list-zipper-end z1)])
                    (assert-equal 5 (list-zipper-focus z2))))
            
            (define-test length-test
              (let ([z (from-just (list-to-zipper '(a b c d e)))])
                   (assert-equal 5 (list-zipper-length z))))
            
            (define-test position-at-start-test
              (let ([z (from-just (list-to-zipper '(1 2 3)))])
                   (assert-equal 0 (list-zipper-position z))))
            
            (define-test position-after-movement-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3 4 5)))]
                     [z2 (from-just (list-zipper-move-right z1))]
                     [z3 (from-just (list-zipper-move-right z2))])
                    (assert-equal 2 (list-zipper-position z3))))
            
            (define-test move-to-test
              (let* ([z1 (from-just (list-to-zipper '(a b c d e)))]
                     [z2 (from-just (list-zipper-move-to 3 z1))])
                    (assert-equal 'd (list-zipper-focus z2))))
            
            (define-test move-to-invalid-test
              (let ([z (from-just (list-to-zipper '(1 2 3)))])
                   (assert-true (nothing? (list-zipper-move-to 10 z)))))
            
            (define-test find-right-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 30 4 50)))]
                     [z2 (from-just (list-zipper-find-right (lambda (x) (> x 20)) z1))])
                    (assert-equal 30 (list-zipper-focus z2))))
            
            (define-test find-right-not-found-test
              (let ([z (from-just (list-to-zipper '(1 2 3)))])
                   (assert-true (nothing? (list-zipper-find-right (lambda (x) (> x 100)) z)))))
            
            (define-test find-left-test
              (let* ([z1 (from-just (list-to-zipper '(10 20 30 40 50)))]
                     [z2 (list-zipper-end z1)]
                     [z3 (from-just (list-zipper-find-left (lambda (x) (< x 25)) z2))])
                    (assert-equal 20 (list-zipper-focus z3)))))

;;; ============================================================
;;; Binary Tree Construction Tests
;;; ============================================================

(test-group tree-construction
            (define-test tree-leaf-test
              (let ([t (tree-leaf 42)])
                   (assert-true (tree-leaf? t))
                   (assert-equal 42 (tree-value t))))
            
            (define-test tree-node-test
              (let ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))])
                   (assert-true (tree-node? t))
                   (assert-equal 1 (tree-value t))
                   (assert-equal 2 (tree-value (tree-left t)))
                   (assert-equal 3 (tree-value (tree-right t)))))
            
            (define-test tree-to-zipper-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z (tree-to-zipper t)])
                    (assert-true (tree-zipper? z))
                    (assert-equal 1 (tree-value (tree-zipper-focus z)))
                    (assert-equal '() (tree-zipper-crumbs z)))))

;;; ============================================================
;;; Tree Zipper Movement Tests
;;; ============================================================

(test-group tree-zipper-movement
            (define-test go-left-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))])
                    (assert-equal 2 (tree-value (tree-zipper-focus z2)))))
            
            (define-test go-right-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-right z1))])
                    (assert-equal 3 (tree-value (tree-zipper-focus z2)))))
            
            (define-test go-up-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))]
                     [z3 (from-just (tree-zipper-go-up z2))])
                    (assert-equal 1 (tree-value (tree-zipper-focus z3)))))
            
            (define-test go-left-on-leaf-test
              (let* ([t (tree-leaf 42)]
                     [z (tree-to-zipper t)])
                    (assert-true (nothing? (tree-zipper-go-left z)))))
            
            (define-test go-up-at-root-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z (tree-to-zipper t)])
                    (assert-true (nothing? (tree-zipper-go-up z)))))
            
            (define-test root-navigation-test
              (let* ([t (tree-node 1
                                   (tree-node 2 (tree-leaf 4) (tree-leaf 5))
                                   (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))]
                     [z3 (from-just (tree-zipper-go-right z2))]
                     [z4 (tree-zipper-root z3)])
                    (assert-equal 1 (tree-value (tree-zipper-focus z4))))))

;;; ============================================================
;;; Tree Zipper Modification Tests
;;; ============================================================

(test-group tree-zipper-modification
            (define-test set-focus-tree-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))]
                     [z3 (tree-zipper-set (tree-leaf 99) z2)]
                     [z4 (tree-zipper-root z3)])
                    (assert-equal 99 (tree-value (tree-left (tree-zipper-focus z4))))))
            
            (define-test set-value-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (tree-zipper-set-value 100 z1)]
                     [focus (tree-zipper-focus z2)])
                    (assert-equal 100 (tree-value focus))
                    (assert-equal 2 (tree-value (tree-left focus)))
                    (assert-equal 3 (tree-value (tree-right focus)))))
            
            (define-test modify-subtree-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (tree-zipper-modify
                          (lambda (t)
                                  (tree-node (+ (tree-value t) 10)
                                             (tree-left t)
                                             (tree-right t)))
                          z1)])
                    (assert-equal 11 (tree-value (tree-zipper-focus z2))))))

;;; ============================================================
;;; Tree Zipper State Tests
;;; ============================================================

(test-group tree-zipper-state
            (define-test is-leaf-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))])
                    (assert-false (tree-zipper-is-leaf? z1))
                    (assert-true (tree-zipper-is-leaf? z2))))
            
            (define-test is-root-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))])
                    (assert-true (tree-zipper-is-root? z1))
                    (assert-false (tree-zipper-is-root? z2))))
            
            (define-test depth-test
              (let* ([t (tree-node 1
                                   (tree-node 2 (tree-leaf 4) (tree-leaf 5))
                                   (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))]
                     [z3 (from-just (tree-zipper-go-left z2))])
                    (assert-equal 0 (tree-zipper-depth z1))
                    (assert-equal 1 (tree-zipper-depth z2))
                    (assert-equal 2 (tree-zipper-depth z3)))))

;;; ============================================================
;;; Path Navigation Tests
;;; ============================================================

(test-group path-navigation
            (define-test follow-path-test
              (let* ([t (tree-node 1
                                   (tree-node 2 (tree-leaf 4) (tree-leaf 5))
                                   (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-follow-path '(left right) z1))])
                    (assert-equal 5 (tree-value (tree-zipper-focus z2)))))
            
            (define-test follow-invalid-path-test
              (let* ([t (tree-leaf 42)]
                     [z (tree-to-zipper t)])
                    (assert-true (nothing? (tree-zipper-follow-path '(left) z)))))
            
            (define-test path-from-root-test
              (let* ([t (tree-node 1
                                   (tree-node 2 (tree-leaf 4) (tree-leaf 5))
                                   (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))]
                     [z3 (from-just (tree-zipper-go-right z2))]
                     [path (tree-zipper-path-from-root z3)])
                    (assert-equal '(left right) path)))
            
            (define-test path-roundtrip-test
              (let* ([t (tree-node 1
                                   (tree-node 2 (tree-leaf 4) (tree-leaf 5))
                                   (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))]
                     [z3 (from-just (tree-zipper-go-right z2))]
                     [path (tree-zipper-path-from-root z3)]
                     [z4 (from-just (tree-zipper-follow-path path (tree-to-zipper t)))])
                    (assert-equal 5 (tree-value (tree-zipper-focus z4))))))

;;; ============================================================
;;; Comonad Tests
;;; ============================================================

(test-group zipper-comonad
            (define-test extract-test
              (let ([z (from-just (list-to-zipper '(1 2 3)))])
                   (assert-equal 1 (list-zipper-extract z))))
            
            (define-test duplicate-focus-test
              (let* ([z (from-just (list-to-zipper '(1 2 3)))]
                     [dup (list-zipper-duplicate z)])
                    (assert-equal 1 (list-zipper-focus (list-zipper-focus dup)))))
            
            (define-test duplicate-contains-all-positions-test
              (let* ([z (from-just (list-to-zipper '(1 2 3)))]
                     [z2 (from-just (list-zipper-move-right z))]
                     [dup (list-zipper-duplicate z2)])
                    ;; Focus should be zipper at position 1 (element 2)
                    (assert-equal 2 (list-zipper-focus (list-zipper-focus dup)))
                    ;; Left should have zipper at position 0
                    (assert-equal 1 (list-zipper-focus (car (list-zipper-left dup))))
                    ;; Right should have zipper at position 2
                    (assert-equal 3 (list-zipper-focus (car (list-zipper-right dup))))))
            
            (define-test extend-test
              (let* ([z (from-just (list-to-zipper '(1 2 3 4 5)))]
                     [z2 (from-just (list-zipper-move-right z))]
                     [z3 (from-just (list-zipper-move-right z2))]
                     [result (list-zipper-extend list-zipper-focus z3)])
                    (assert-equal '(1 2 3 4 5) (zipper-to-list result)))))

;;; ============================================================
;;; Practical Example Tests
;;; ============================================================

(test-group practical-examples
            (define-test local-sum-center-test
              (let* ([z (from-just (list-to-zipper '(1 2 3 4 5)))]
                     [z2 (from-just (list-zipper-move-right z))]
                     [z3 (from-just (list-zipper-move-right z2))])
                    (assert-equal 9 (local-sum z3))))  ; 2 + 3 + 4 = 9
            
            (define-test local-sum-edge-test
              (let ([z (from-just (list-to-zipper '(10 20 30)))])
                   (assert-equal 30 (local-sum z))))  ; 0 + 10 + 20 = 30
            
            (define-test tree-sum-test
              (let* ([t (tree-node 1
                                   (tree-node 2 (tree-leaf 4) (tree-leaf 5))
                                   (tree-leaf 3))]
                     [z (tree-to-zipper t)])
                    (assert-equal 15 (tree-sum z))))  ; 1+2+3+4+5 = 15
            
            (define-test tree-find-test
              (let* ([t (tree-node 1
                                   (tree-node 2 (tree-leaf 4) (tree-leaf 5))
                                   (tree-leaf 3))]
                     [z (tree-to-zipper t)]
                     [found (from-just (tree-find (lambda (x) (= x 4)) z))])
                    (assert-equal 4 (tree-value (tree-zipper-focus found)))))
            
            (define-test tree-find-not-found-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z (tree-to-zipper t)])
                    (assert-true (nothing? (tree-find (lambda (x) (= x 99)) z))))))

;;; ============================================================
;;; String Zipper Tests
;;; ============================================================

(test-group string-zipper
            (define-test string-to-zipper-test
              (let ([z (from-just (string-to-zipper "hello"))])
                   (assert-equal #\h (list-zipper-focus z))))
            
            (define-test zipper-to-string-test
              (let* ([z (from-just (string-to-zipper "hello"))]
                     [z2 (from-just (list-zipper-move-right z))]
                     [z3 (list-zipper-set # z2)])
                    (assert-equal "hallo" (zipper-to-string z3))))
            
            (define-test string-empty-test
              (assert-true (nothing? (string-to-zipper "")))))

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(test-group utility-functions
            (define-test with-focus-test
              (let* ([z (from-just (list-to-zipper '(10 20 30)))]
                     [result (with-focus (lambda (x) (* x 2)) z)])
                    (assert-equal 20 (car result))
                    (assert-true (list-zipper? (cdr result)))))
            
            (define-test focus-satisfies-test
              (let ([z (from-just (list-to-zipper '(10 20 30)))])
                   (assert-true (focus-satisfies? even? z))
                   (assert-false (focus-satisfies? odd? z)))))

;;; ============================================================
;;; Reconstruction Tests
;;; ============================================================

(test-group reconstruction
            (define-test zipper-to-tree-test
              (let* ([t (tree-node 1 (tree-leaf 2) (tree-leaf 3))]
                     [z1 (tree-to-zipper t)]
                     [z2 (from-just (tree-zipper-go-left z1))]
                     [z3 (tree-zipper-set (tree-leaf 99) z2)]
                     [reconstructed (zipper-to-tree z3)])
                    (assert-equal 1 (tree-value reconstructed))
                    (assert-equal 99 (tree-value (tree-left reconstructed)))
                    (assert-equal 3 (tree-value (tree-right reconstructed)))))
            
            (define-test list-modification-preserves-structure-test
              (let* ([z1 (from-just (list-to-zipper '(1 2 3 4 5)))]
                     [z2 (from-just (list-zipper-move-right z1))]
                     [z3 (from-just (list-zipper-move-right z2))]
                     [z4 (list-zipper-set 100 z3)]
                     [z5 (list-zipper-start z4)])
                    (assert-equal '(1 2 100 4 5) (zipper-to-list z5)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All zipper tests passed.
")
    (display "
[FAILURE] Some zipper tests failed.
"))
