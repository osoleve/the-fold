;;; fabric/patterns/test-data-structures.ss — Tests for data-structures.ss
;;;
;;; Comprehensive tests for Stack, Queue, Set, and Dictionary.
;;;
;;; Dependencies:
;;;   - data-structures.ss

(load "fabric/patterns/data-structures.ss")

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
          [(error? ex)
           (set! tests-passed (+ tests-passed 1))
           (display "  ✓ ") (display name) (display " (error raised)") (newline)]
          [else
           (set! tests-failed (+ tests-failed 1))
           (display "  ✗ ") (display name) (display " (wrong exception type)") (newline)])
         (thunk)
         (set! tests-failed (+ tests-failed 1))
         (display "  ✗ ") (display name) (display " (no error raised)") (newline)))

(display "Testing data-structures.ss\n")
(display "===========================\n\n")

;;; ============================================================
;;; Stack Tests
;;; ============================================================

(display "Stack:\n")
(display "------\n")

;;; Basic operations
(test "stack-empty? on empty stack" #t (stack-empty? stack-empty))
(test "stack-size of empty" 0 (stack-size stack-empty))

(define s1 (stack-push 'a stack-empty))
(test "stack-empty? after push" #f (stack-empty? s1))
(test "stack-size after one push" 1 (stack-size s1))
(test "stack-peek after push" 'a (stack-peek s1))

(define s2 (stack-push 'b s1))
(define s3 (stack-push 'c s2))
(test "stack-size after three pushes" 3 (stack-size s3))
(test "stack-peek shows top" 'c (stack-peek s3))
(test "stack->list order" '(c b a) (stack->list s3))

;;; Pop operations
(define-values (s4 elem1) (stack-pop s3))
(test "first pop returns top element" 'c elem1)
(test "stack after pop has size 2" 2 (stack-size s4))
(define-values (s5 elem2) (stack-pop s4))
(test "second pop returns next element" 'b elem2)

;;; List conversions
(test "list->stack->list" '(1 2 3) (stack->list (list->stack '(1 2 3))))

;;; Error cases
(test-error "stack-peek on empty" (lambda () (stack-peek stack-empty)))
(test-error "stack-pop on empty" (lambda () (stack-pop stack-empty)))

;;; ============================================================
;;; Queue Tests
;;; ============================================================

(display "\nQueue:\n")
(display "------\n")

;;; Basic operations
(test "queue-empty? on empty queue" #t (queue-empty? queue-empty))
(test "queue-size of empty" 0 (queue-size queue-empty))

(define q1 (queue-enqueue 'x queue-empty))
(test "queue-empty? after enqueue" #f (queue-empty? q1))
(test "queue-size after one enqueue" 1 (queue-size q1))
(test "queue-peek after enqueue" 'x (queue-peek q1))

(define q2 (queue-enqueue 'y q1))
(define q3 (queue-enqueue 'z q2))
(test "queue-size after three enqueues" 3 (queue-size q3))
(test "queue-peek shows front" 'x (queue-peek q3))
(test "queue->list order" '(x y z) (queue->list q3))

;;; Dequeue operations
(define-values (q4 qelem1) (queue-dequeue q3))
(test "first dequeue returns front element" 'x qelem1)
(test "queue after dequeue has size 2" 2 (queue-size q4))
(define-values (q5 qelem2) (queue-dequeue q4))
(test "second dequeue returns next element" 'y qelem2)

;;; FIFO property (enqueue order = dequeue order)
(define q-test (list->queue '(1 2 3 4 5)))
(define-values (q-test2 e1) (queue-dequeue q-test))
(define-values (q-test3 e2) (queue-dequeue q-test2))
(define-values (q-test4 e3) (queue-dequeue q-test3))
(test "FIFO: first out" 1 e1)
(test "FIFO: second out" 2 e2)
(test "FIFO: third out" 3 e3)
(test "FIFO: remaining" '(4 5) (queue->list q-test4))

;;; List conversions
(test "list->queue->list" '(a b c) (queue->list (list->queue '(a b c))))

;;; Error cases
(test-error "queue-peek on empty" (lambda () (queue-peek queue-empty)))
(test-error "queue-dequeue on empty" (lambda () (queue-dequeue queue-empty)))

;;; ============================================================
;;; Set Tests
;;; ============================================================

(display "\nSet:\n")
(display "----\n")

;;; Basic operations
(test "set-empty? on empty set" #t (set-empty? set-empty))
(test "set-size of empty" 0 (set-size set-empty))

(define set1 (set-add 'a set-empty))
(test "set-empty? after add" #f (set-empty? set1))
(test "set-size after one add" 1 (set-size set1))
(test "set-member? finds element" #t (set-member? 'a set1))
(test "set-member? doesn't find absent" #f (set-member? 'z set1))

;;; Duplicate handling
(define set2 (set-add 'b set1))
(define set3 (set-add 'a set2))  ; Add duplicate
(test "set-size with duplicate" 2 (set-size set3))
(test "set contains first element" #t (set-member? 'a set3))
(test "set contains second element" #t (set-member? 'b set3))

;;; Remove operations
(define set4 (set-remove 'a set3))
(test "set-size after remove" 1 (set-size set4))
(test "removed element not present" #f (set-member? 'a set4))
(test "other element still present" #t (set-member? 'b set4))

;;; Set operations
(define setA (list->set '(1 2 3)))
(define setB (list->set '(2 3 4)))
(define union-result (set-union setA setB))
(test "union contains all elements" 4 (set-size union-result))
(test "union has 1" #t (set-member? 1 union-result))
(test "union has 4" #t (set-member? 4 union-result))

(define inter-result (set-intersection setA setB))
(test "intersection size" 2 (set-size inter-result))
(test "intersection has 2" #t (set-member? 2 inter-result))
(test "intersection has 3" #t (set-member? 3 inter-result))
(test "intersection doesn't have 1" #f (set-member? 1 inter-result))

(define diff-result (set-difference setA setB))
(test "difference size" 1 (set-size diff-result))
(test "difference has 1" #t (set-member? 1 diff-result))
(test "difference doesn't have 2" #f (set-member? 2 diff-result))

;;; Subset tests
(test "set is subset of itself" #t (set-subset? setA setA))
(test "empty is subset of any" #t (set-subset? set-empty setA))
(test "smaller is subset" #t (set-subset? (list->set '(1 2)) setA))
(test "larger is not subset" #f (set-subset? setA (list->set '(1 2))))

;;; List conversions
(define lst '(x y z x y))  ; Has duplicates
(define set-from-list (list->set lst))
(test "list->set removes duplicates" 3 (set-size set-from-list))

;;; ============================================================
;;; Dictionary Tests
;;; ============================================================

(display "\nDictionary:\n")
(display "-----------\n")

;;; Basic operations
(test "dict-empty? on empty dict" #t (dict-empty? dict-empty))
(test "dict-size of empty" 0 (dict-size dict-empty))

(define d1 (dict-assoc 'name "Alice" dict-empty))
(test "dict-empty? after assoc" #f (dict-empty? d1))
(test "dict-size after one assoc" 1 (dict-size d1))
(test "dict-lookup finds value" "Alice" (dict-lookup 'name d1))
(test "dict-has-key? finds key" #t (dict-has-key? 'name d1))
(test "dict-lookup missing key" #f (dict-lookup 'missing d1))
(test "dict-has-key? missing" #f (dict-has-key? 'missing d1))

;;; Multiple entries
(define d2 (dict-assoc 'age 30 d1))
(define d3 (dict-assoc 'city "NYC" d2))
(test "dict-size with three entries" 3 (dict-size d3))
(test "dict-lookup first key" "Alice" (dict-lookup 'name d3))
(test "dict-lookup second key" 30 (dict-lookup 'age d3))
(test "dict-lookup third key" "NYC" (dict-lookup 'city d3))

;;; Update existing key
(define d4 (dict-assoc 'age 31 d3))
(test "dict-size after update" 3 (dict-size d4))
(test "dict-lookup updated value" 31 (dict-lookup 'age d4))

;;; Remove operations
(define d5 (dict-dissoc 'city d4))
(test "dict-size after dissoc" 2 (dict-size d5))
(test "dict-has-key? after dissoc" #f (dict-has-key? 'city d5))
(test "other keys still present" #t (dict-has-key? 'name d5))

;;; Keys, values, entries
(define d6 (dict-assoc 'z 26 (dict-assoc 'y 25 (dict-assoc 'x 24 dict-empty))))
(test "dict-size for keys/values test" 3 (dict-size d6))
(test "dict-keys length" 3 (length (dict-keys d6)))
(test "dict-values length" 3 (length (dict-values d6)))
(test "dict-entries length" 3 (length (dict-entries d6)))

;;; Merge
(define dA (dict-assoc 'a 1 (dict-assoc 'b 2 dict-empty)))
(define dB (dict-assoc 'b 99 (dict-assoc 'c 3 dict-empty)))
(define merged (dict-merge dA dB))
(test "merge size" 3 (dict-size merged))
(test "merge keeps dB value for overlap" 99 (dict-lookup 'b merged))
(test "merge has dA key" #t (dict-has-key? 'a merged))
(test "merge has dB key" #t (dict-has-key? 'c merged))

;;; Map values
(define d7 (dict-assoc 'x 10 (dict-assoc 'y 20 dict-empty)))
(define d8 (dict-map-values (lambda (v) (* v 2)) d7))
(test "map-values size unchanged" 2 (dict-size d8))
(test "map-values transforms first" 20 (dict-lookup 'x d8))
(test "map-values transforms second" 40 (dict-lookup 'y d8))

;;; Filter
(define d9 (dict-assoc 'a 1 (dict-assoc 'b 2 (dict-assoc 'c 3 dict-empty))))
(define d10 (dict-filter (lambda (k v) (> v 1)) d9))
(test "filter size" 2 (dict-size d10))
(test "filter removes low value" #f (dict-has-key? 'a d10))
(test "filter keeps high values" #t (dict-has-key? 'b d10))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "===========================\n")
(display "Tests passed: ") (display tests-passed) (newline)
(display "Tests failed: ") (display tests-failed) (newline)

(if (= tests-failed 0)
    (begin
     (display "\n✓ All tests passed!\n")
     (exit 0))
    (begin
     (display "\n✗ Some tests failed.\n")
     (exit 1)))
