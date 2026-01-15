;;; lattice/data/test-avl-tree.ss — Tests for AVL Tree
;;;
;;; Comprehensive tests for AVL tree operations, balancing,
;;; and invariant verification.
;;;
;;; Dependencies:
;;;   - avl-tree.ss

(load "lattice/data/avl-tree.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(define (test-error name thunk)
  (guard (ex
          [(serious-condition? ex)
           (set! tests-passed (+ tests-passed 1))
           (display "  ✓ ") (display name) (display " (error raised)") (newline)]
          [else
           (set! tests-failed (+ tests-failed 1))
           (display "  ✗ ") (display name) (display " (wrong exception type)") (newline)])
         (thunk)
         (set! tests-failed (+ tests-failed 1))
         (display "  ✗ ") (display name) (display " (no error raised)") (newline)))

(display "Testing AVL Tree\n")
(display "====\n\n")

;;; ====
;;; Basic Operations
;;; ====

(display "Basic Operations:\n")
(display "----\n")

(test "avl-empty? on empty" #t (avl-empty? avl-empty))

(define t1 (avl-insert 5 "five" avl-empty))
(test "avl-empty? after insert" #f (avl-empty? t1))
(test "avl-lookup found" "five" (avl-lookup 5 t1))
(test "avl-lookup not found" #f (avl-lookup 99 t1))
(test "avl-contains?" #t (avl-contains? 5 t1))
(test "avl-contains? missing" #f (avl-contains? 99 t1))
(test "avl-size after one insert" 1 (avl-size t1))

(define t2 (avl-insert 3 "three" t1))
(define t3 (avl-insert 7 "seven" t2))
(test "avl-size after three inserts" 3 (avl-size t3))
(test "avl-lookup left child" "three" (avl-lookup 3 t3))
(test "avl-lookup right child" "seven" (avl-lookup 7 t3))

;;; Update existing key
(define t4 (avl-insert 5 "FIVE" t3))
(test "update existing key" "FIVE" (avl-lookup 5 t4))
(test "size unchanged after update" 3 (avl-size t4))

;;; ====
;;; Min/Max Operations
;;; ====

(display "\nMin/Max Operations:\n")
(display "----\n")

(define tm (avl-insert 10 "x" (avl-insert 5 "x" (avl-insert 15 "x" (avl-insert 3 "x" (avl-insert 7 "x" avl-empty))))))
(test "avl-min" 3 (avl-min tm))
(test "avl-max" 15 (avl-max tm))
(test "avl-min-value" "x" (avl-min-value tm))
(test "avl-max-value" "x" (avl-max-value tm))

(test-error "avl-min on empty" (lambda () (avl-min avl-empty)))
(test-error "avl-max on empty" (lambda () (avl-max avl-empty)))

;;; ====
;;; Deletion Tests
;;; ====

(display "\nDeletion:\n")
(display "----\n")

(define td (avl-insert 20 "a" (avl-insert 10 "b" (avl-insert 30 "c" (avl-insert 5 "d" (avl-insert 15 "e" avl-empty))))))
(test "avl-size before delete" 5 (avl-size td))

;;; Delete leaf
(define td1 (avl-delete 5 td))
(test "delete leaf size" 4 (avl-size td1))
(test "deleted key not found" #f (avl-lookup 5 td1))
(test "avl-valid? after delete leaf" #t (avl-valid? td1))

;;; Delete node with one child
(define td2 (avl-delete 30 td))
(test "delete one-child size" 4 (avl-size td2))
(test "avl-valid? after delete one-child" #t (avl-valid? td2))

;;; Delete node with two children
(define td3 (avl-delete 10 td))
(test "delete two-child size" 4 (avl-size td3))
(test "avl-valid? after delete two-child" #t (avl-valid? td3))

;;; Delete root
(define td4 (avl-delete 20 td))
(test "delete root size" 4 (avl-size td4))
(test "avl-valid? after delete root" #t (avl-valid? td4))

;;; Delete non-existent key
(define td5 (avl-delete 999 td))
(test "delete non-existent unchanged" 5 (avl-size td5))

;;; ====
;;; Balancing Tests
;;; ====

(display "\nBalancing (AVL Property):\n")
(display "----\n")

;;; Left-Left case (requires right rotation)
(define ll (avl-insert 1 "a" (avl-insert 2 "b" (avl-insert 3 "c" avl-empty))))
(test "LL case valid" #t (avl-valid? ll))
(test "LL case BST valid" #t (avl-bst-valid? ll))

;;; Right-Right case (requires left rotation)
(define rr (avl-insert 3 "a" (avl-insert 2 "b" (avl-insert 1 "c" avl-empty))))
(test "RR case valid" #t (avl-valid? rr))
(test "RR case BST valid" #t (avl-bst-valid? rr))

;;; Left-Right case (requires LR double rotation)
(define lr (avl-insert 2 "a" (avl-insert 3 "b" (avl-insert 1 "c" avl-empty))))
(test "LR case valid" #t (avl-valid? lr))
(test "LR case BST valid" #t (avl-bst-valid? lr))

;;; Right-Left case (requires RL double rotation)
(define rl (avl-insert 2 "a" (avl-insert 1 "b" (avl-insert 3 "c" avl-empty))))
(test "RL case valid" #t (avl-valid? rl))
(test "RL case BST valid" #t (avl-bst-valid? rl))

;;; Sequential insertion (worst case for unbalanced BST)
(define seq (fold-left (lambda (t k) (avl-insert k k t)) avl-empty (iota 20)))
(test "sequential insert valid" #t (avl-valid? seq))
(test "sequential insert BST valid" #t (avl-bst-valid? seq))
(test "sequential insert height reasonable" #t (<= (avl-height seq) 7))  ; log2(20) ~= 4.3, AVL allows up to 1.44*log2

;;; Reverse sequential insertion
(define rseq (fold-left (lambda (t k) (avl-insert k k t)) avl-empty (reverse (iota 20))))
(test "reverse sequential valid" #t (avl-valid? rseq))
(test "reverse sequential BST valid" #t (avl-bst-valid? rseq))

;;; ====
;;; Conversion Tests
;;; ====

(display "\nConversions:\n")
(display "----\n")

(define tc (avl-insert 5 "e" (avl-insert 3 "c" (avl-insert 7 "g" (avl-insert 1 "a" (avl-insert 9 "i" avl-empty))))))
(test "avl->list sorted" '((1 . "a") (3 . "c") (5 . "e") (7 . "g") (9 . "i")) (avl->list tc))
(test "avl-keys sorted" '(1 3 5 7 9) (avl-keys tc))
(test "avl-values in key order" '("a" "c" "e" "g" "i") (avl-values tc))

(define from-list (list->avl '((2 . "two") (1 . "one") (3 . "three"))))
(test "list->avl lookup" "two" (avl-lookup 2 from-list))
(test "list->avl valid" #t (avl-valid? from-list))

(define from-keys (avl-from-keys '(5 3 7 1 9)))
(test "avl-from-keys contains all" #t (and (avl-contains? 1 from-keys)
                                            (avl-contains? 5 from-keys)
                                            (avl-contains? 9 from-keys)))

;;; ====
;;; Higher-Order Operations
;;; ====

(display "\nHigher-Order Operations:\n")
(display "----\n")

(define th (avl-insert 3 30 (avl-insert 1 10 (avl-insert 2 20 avl-empty))))

;;; avl-fold (sum all values)
(test "avl-fold sum" 60 (avl-fold (lambda (acc k v) (+ acc v)) 0 th))

;;; avl-fold-right (like foldr, maintains original order with cons)
(test "avl-fold-right keys" '(1 2 3) (avl-fold-right (lambda (k v acc) (cons k acc)) '() th))

;;; avl-map (double all values)
(define mapped (avl-map (lambda (v) (* v 2)) th))
(test "avl-map" 60 (avl-lookup 3 mapped))
(test "avl-map valid" #t (avl-valid? mapped))

;;; avl-filter
(define filtered (avl-filter (lambda (k v) (> v 15)) th))
(test "avl-filter size" 2 (avl-size filtered))
(test "avl-filter excludes" #f (avl-contains? 1 filtered))
(test "avl-filter includes" #t (avl-contains? 2 filtered))

;;; ====
;;; Range Query Tests
;;; ====

(display "\nRange Queries:\n")
(display "----\n")

(define tr (avl-from-keys '(1 3 5 7 9 11 13 15)))
(test "avl-range [5,11]" '((5 . 5) (7 . 7) (9 . 9) (11 . 11)) (avl-range 5 11 tr))
(test "avl-range empty" '() (avl-range 100 200 tr))
(test "avl-range single" '((7 . 7)) (avl-range 7 7 tr))

(test "avl-keys-between" '(5 7 9) (avl-keys-between 5 9 tr))

(test "avl-less-than" '((1 . 1) (3 . 3) (5 . 5)) (avl-less-than 6 tr))
(test "avl-greater-than" '((11 . 11) (13 . 13) (15 . 15)) (avl-greater-than 10 tr))

;;; ====
;;; Set Operations
;;; ====

(display "\nSet Operations:\n")
(display "----\n")

(define s1 (avl-from-keys '(1 3 5 7)))
(define s2 (avl-from-keys '(5 7 9 11)))

(test "avl-set-member?" #t (avl-set-member? 3 s1))
(test "avl-set-member? missing" #f (avl-set-member? 9 s1))

(define sunion (avl-set-union s1 s2))
(test "avl-set-union keys" '(1 3 5 7 9 11) (avl-keys sunion))

(define sinter (avl-set-intersection s1 s2))
(test "avl-set-intersection keys" '(5 7) (avl-keys sinter))

(define sdiff (avl-set-difference s1 s2))
(test "avl-set-difference keys" '(1 3) (avl-keys sdiff))

;;; ====
;;; Stress Tests
;;; ====

(display "\nStress Tests:\n")
(display "----\n")

;;; Insert and delete many elements
(define large (fold-left (lambda (t k) (avl-insert k (* k k) t)) avl-empty (iota 100)))
(test "large tree size" 100 (avl-size large))
(test "large tree valid" #t (avl-valid? large))
(test "large tree BST valid" #t (avl-bst-valid? large))
(test "large tree lookup" 2500 (avl-lookup 50 large))

;;; Delete half the elements
(define half-deleted (fold-left (lambda (t k) (avl-delete k t)) large (filter even? (iota 100))))
(test "after delete half size" 50 (avl-size half-deleted))
(test "after delete valid" #t (avl-valid? half-deleted))
(test "after delete BST valid" #t (avl-bst-valid? half-deleted))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(display "Tests passed: ") (display tests-passed) (newline)
(display "Tests failed: ") (display tests-failed) (newline)

(if (= tests-failed 0)
    (begin
     (display "\n✓ All tests passed!\n")
     (exit 0))
    (begin
     (display "\n✗ Some tests failed.\n")
     (exit 1)))
