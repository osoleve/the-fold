;;; lattice/automata/test-statechart-zipper.ss — Tests for Statechart Zipper
;;;
;;; Tests the zipper-based navigation for statechart hierarchies.

;; Load order matters: statechart.ss defines current-context which conflicts
;; with test-framework.ss's parameter. Load statechart first, save its version,
;; then load test-framework which will override it.
(load "lattice/automata/statechart.ss")
(load "lattice/automata/statechart-zipper.ss")

;; Save statechart's current-context before test-framework overwrites it
(define statechart-current-context current-context)

(load "core/testing/test-framework.ss")

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

;;; Build a test hierarchy:
;;;   root (composite)
;;;     ├── a (atomic)
;;;     ├── b (composite)
;;;     │     ├── b1 (atomic)
;;;     │     └── b2 (atomic)
;;;     └── c (atomic)

(define test-b1 (atomic 'b1))
(define test-b2 (atomic 'b2))
(define test-b (composite 'b (list test-b1 test-b2)))
(define test-a (atomic 'a))
(define test-c (atomic 'c))
(define test-root (composite 'root (list test-a test-b test-c)))

;;; Deeper hierarchy for LCA tests:
;;;   deep-root
;;;     ├── x
;;;     │   ├── x1
;;;     │   │   └── x1a
;;;     │   └── x2
;;;     └── y
;;;         └── y1

(define test-x1a (atomic 'x1a))
(define test-x1 (composite 'x1 (list test-x1a)))
(define test-x2 (atomic 'x2))
(define test-x (composite 'x (list test-x1 test-x2)))
(define test-y1 (atomic 'y1))
(define test-y (composite 'y (list test-y1)))
(define deep-root (composite 'deep-root (list test-x test-y)))

;;; ============================================================
;;; Basic Navigation Tests
;;; ============================================================

(test-group "state-zipper-navigation"

  (define-test "state->zipper creates zipper at root"
    (let ([sz (state->zipper test-root)])
      (assert-true (state-zipper? sz))
      (assert-equal 'root (state-zipper-id sz))
      (assert-true (state-zipper-at-root? sz))))

  (define-test "state-zipper-down moves to first child"
    (let* ([sz (state->zipper test-root)]
           [down (state-zipper-down sz)])
      (assert-true (just? down))
      (assert-equal 'a (state-zipper-id (from-just down)))))

  (define-test "state-zipper-right moves to next sibling"
    (let* ([sz (state->zipper test-root)]
           [a-z (from-just (state-zipper-down sz))]
           [b-z (from-just (state-zipper-right a-z))])
      (assert-equal 'b (state-zipper-id b-z))))

  (define-test "state-zipper-up moves to parent"
    (let* ([sz (state->zipper test-root)]
           [a-z (from-just (state-zipper-down sz))]
           [back-up (state-zipper-up a-z)])
      (assert-true (just? back-up))
      (assert-equal 'root (state-zipper-id (from-just back-up)))))

  (define-test "state-zipper-up at root returns nothing"
    (let ([sz (state->zipper test-root)])
      (assert-true (nothing? (state-zipper-up sz)))))

  (define-test "state-zipper-down at leaf returns nothing"
    (let* ([sz (state->zipper test-root)]
           [a-z (from-just (state-zipper-down sz))])
      (assert-true (nothing? (state-zipper-down a-z)))))

  (define-test "state-zipper-root returns to root from anywhere"
    (let* ([sz (state->zipper test-root)]
           [b1-z (from-just (state-zipper-find sz 'b1))]
           [back-root (state-zipper-root b1-z)])
      (assert-equal 'root (state-zipper-id back-root))))

  (define-test "state-zipper-nth-child navigates to correct child"
    (let* ([sz (state->zipper test-root)]
           [c-z (from-just (state-zipper-nth-child sz 2))])
      (assert-equal 'c (state-zipper-id c-z)))))

;;; ============================================================
;;; Find Tests
;;; ============================================================

(test-group "state-zipper-find"

  (define-test "state-zipper-find finds root"
    (let* ([sz (state->zipper test-root)]
           [found (state-zipper-find sz 'root)])
      (assert-true (just? found))
      (assert-equal 'root (state-zipper-id (from-just found)))))

  (define-test "state-zipper-find finds direct child"
    (let* ([sz (state->zipper test-root)]
           [found (state-zipper-find sz 'a)])
      (assert-true (just? found))
      (assert-equal 'a (state-zipper-id (from-just found)))))

  (define-test "state-zipper-find finds nested child"
    (let* ([sz (state->zipper test-root)]
           [found (state-zipper-find sz 'b1)])
      (assert-true (just? found))
      (assert-equal 'b1 (state-zipper-id (from-just found)))))

  (define-test "state-zipper-find finds deeply nested child"
    (let* ([sz (state->zipper deep-root)]
           [found (state-zipper-find sz 'x1a)])
      (assert-true (just? found))
      (assert-equal 'x1a (state-zipper-id (from-just found)))))

  (define-test "state-zipper-find returns nothing for non-existent"
    (let* ([sz (state->zipper test-root)]
           [found (state-zipper-find sz 'nonexistent)])
      (assert-true (nothing? found)))))

;;; ============================================================
;;; Ancestor Tests
;;; ============================================================

(test-group "state-zipper-ancestors"

  (define-test "root has no ancestors"
    (let ([sz (state->zipper test-root)])
      (assert-equal '() (state-zipper-ancestors sz))))

  (define-test "direct child has one ancestor"
    (let* ([sz (state->zipper test-root)]
           [a-z (from-just (state-zipper-find sz 'a))])
      (assert-equal '(root) (state-zipper-ancestor-ids a-z))))

  (define-test "nested child has two ancestors"
    (let* ([sz (state->zipper test-root)]
           [b1-z (from-just (state-zipper-find sz 'b1))])
      (assert-equal '(b root) (state-zipper-ancestor-ids b1-z))))

  (define-test "deeply nested has correct ancestors"
    (let* ([sz (state->zipper deep-root)]
           [x1a-z (from-just (state-zipper-find sz 'x1a))])
      (assert-equal '(x1 x deep-root) (state-zipper-ancestor-ids x1a-z))))

  (define-test "path includes self and all ancestors"
    (let* ([sz (state->zipper deep-root)]
           [x1a-z (from-just (state-zipper-find sz 'x1a))])
      (assert-equal '(deep-root x x1 x1a) (state-zipper-path-ids x1a-z)))))

;;; ============================================================
;;; LCA Tests
;;; ============================================================

(test-group "state-zipper-lca"

  (define-test "LCA of same state is itself"
    (let* ([sz (state->zipper test-root)]
           [lca (state-zipper-lca sz 'b1 'b1)])
      (assert-true (just? lca))
      (assert-equal 'b1 (state-id (from-just lca)))))

  (define-test "LCA of siblings is parent"
    (let* ([sz (state->zipper test-root)]
           [lca (state-zipper-lca sz 'b1 'b2)])
      (assert-true (just? lca))
      (assert-equal 'b (state-id (from-just lca)))))

  (define-test "LCA of cousins is grandparent"
    (let* ([sz (state->zipper test-root)]
           [lca (state-zipper-lca sz 'a 'b1)])
      (assert-true (just? lca))
      (assert-equal 'root (state-id (from-just lca)))))

  (define-test "LCA across branches"
    (let* ([sz (state->zipper deep-root)]
           [lca (state-zipper-lca sz 'x1a 'y1)])
      (assert-true (just? lca))
      (assert-equal 'deep-root (state-id (from-just lca)))))

  (define-test "LCA of parent and child is parent"
    (let* ([sz (state->zipper test-root)]
           [lca (state-zipper-lca sz 'b 'b1)])
      (assert-true (just? lca))
      (assert-equal 'b (state-id (from-just lca)))))

  (define-test "LCA with nonexistent state returns nothing"
    (let* ([sz (state->zipper test-root)]
           [lca (state-zipper-lca sz 'b1 'nonexistent)])
      (assert-true (nothing? lca)))))

;;; ============================================================
;;; Exit/Entry Path Tests
;;; ============================================================

(test-group "state-zipper-exit-entry-paths"

  (define-test "exit path from leaf to sibling"
    (let* ([sz (state->zipper test-root)]
           [exit-path (state-zipper-exit-path sz 'b1 'b2)])
      (assert-equal '(b1) (map state-id exit-path))))

  (define-test "entry path to sibling"
    (let* ([sz (state->zipper test-root)]
           [entry-path (state-zipper-entry-path sz 'b1 'b2)])
      (assert-equal '(b2) (map state-id entry-path))))

  (define-test "exit path across branches includes all states to LCA"
    (let* ([sz (state->zipper test-root)]
           [exit-path (state-zipper-exit-path sz 'b1 'c)])
      (assert-equal '(b1 b) (map state-id exit-path))))

  (define-test "entry path across branches includes all states from LCA"
    (let* ([sz (state->zipper test-root)]
           [entry-path (state-zipper-entry-path sz 'b1 'c)])
      (assert-equal '(c) (map state-id entry-path))))

  (define-test "deep exit path"
    (let* ([sz (state->zipper deep-root)]
           [exit-path (state-zipper-exit-path sz 'x1a 'y1)])
      (assert-equal '(x1a x1 x) (map state-id exit-path))))

  (define-test "deep entry path"
    (let* ([sz (state->zipper deep-root)]
           [entry-path (state-zipper-entry-path sz 'x1a 'y1)])
      (assert-equal '(y y1) (map state-id entry-path)))))

;;; ============================================================
;;; Descendant Check Tests
;;; ============================================================

(test-group "state-zipper-is-descendant"

  (define-test "child is descendant of parent"
    (let ([sz (state->zipper test-root)])
      (assert-true (state-zipper-is-descendant? sz 'b 'b1))))

  (define-test "grandchild is descendant of grandparent"
    (let ([sz (state->zipper test-root)])
      (assert-true (state-zipper-is-descendant? sz 'root 'b1))))

  (define-test "parent is not descendant of child"
    (let ([sz (state->zipper test-root)])
      (assert-false (state-zipper-is-descendant? sz 'b1 'b))))

  (define-test "sibling is not descendant"
    (let ([sz (state->zipper test-root)])
      (assert-false (state-zipper-is-descendant? sz 'a 'b)))))

;;; ============================================================
;;; Round-trip Tests
;;; ============================================================

(test-group "state-zipper-roundtrip"

  (define-test "zipper->state preserves structure"
    (let* ([sz (state->zipper test-root)]
           [reconstructed (zipper->state sz)])
      (assert-equal 'root (state-id reconstructed))
      (assert-equal 3 (length (state-substates reconstructed)))))

  (define-test "modifications persist through round-trip"
    (let* ([sz (state->zipper test-root)]
           [b1-z (from-just (state-zipper-find sz 'b1))]
           [modified-z (state-zipper-modify b1-z
                         (lambda (s)
                           (set-entry-action s (lambda (ctx) ctx))))]
           [reconstructed (zipper->state modified-z)]
           [b-state (find-state reconstructed 'b)]
           [b1-state (find-state (from-just b-state) 'b1)])
      (assert-true (just? b1-state))
      ;; Entry action was set
      (assert-true (procedure? (state-entry-action (from-just b1-state)))))))

;;; ============================================================
;;; Traversal Tests
;;; ============================================================

(test-group "state-zipper-traversal"

  (define-test "preorder visits all states"
    (let* ([sz (state->zipper test-root)]
           [all-zippers (state-zipper-preorder sz)]
           [all-ids (map state-zipper-id all-zippers)])
      ;; Should visit: root, a, b, b1, b2, c
      (assert-equal 6 (length all-ids))
      (assert-true (and (member 'root all-ids) #t))
      (assert-true (and (member 'a all-ids) #t))
      (assert-true (and (member 'b all-ids) #t))
      (assert-true (and (member 'b1 all-ids) #t))
      (assert-true (and (member 'b2 all-ids) #t))
      (assert-true (and (member 'c all-ids) #t))))

  (define-test "state-zipper-next iterates correctly"
    (let* ([sz (state->zipper test-root)]
           [ids (let loop ([current (just sz)] [acc '()])
                  (if (nothing? current)
                      (reverse acc)
                      (loop (state-zipper-next (from-just current))
                            (cons (state-zipper-id (from-just current)) acc))))])
      (assert-equal 6 (length ids))
      (assert-equal 'root (car ids)))))

;;; ============================================================
;;; Collect Tests
;;; ============================================================

(test-group "state-zipper-collect"

  (define-test "collect atomic states"
    (let* ([sz (state->zipper test-root)]
           [atomics (state-zipper-collect sz atomic-state?)])
      (assert-equal 4 (length atomics))
      (assert-true (andmap atomic-state? atomics))))

  (define-test "collect composite states"
    (let* ([sz (state->zipper test-root)]
           [composites (state-zipper-collect sz composite-state?)])
      (assert-equal 2 (length composites)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
