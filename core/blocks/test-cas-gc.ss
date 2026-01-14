;;; fabric/stitches/test-cas-gc.ss — Tests for CAS Lifecycle/GC Features
;;;
;;; Tests pin/unpin, tree operations, and garbage collection.

(load "core/blocks/cas.ss")

(display "CAS Lifecycle Tests
")
(display "====

")

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

;;; ====
;;; Setup: Create test blocks
;;; ====

(display "Creating test blocks...
")

;; Clear store for fresh tests
(hashtable-clear! *store*)
(hashtable-clear! *pinned*)

;; Create a tree of blocks:
;;   root → [child1, child2]
;;   child1 → [grandchild]
;;   child2 → []
;;   grandchild → []

(define grandchild-blk (make-block 'leaf (string->utf8 "grandchild") empty-refs))
(define grandchild-hash (store! grandchild-blk))

(define child1-blk (make-block 'node (string->utf8 "child1") (vector grandchild-hash)))
(define child1-hash (store! child1-blk))

(define child2-blk (make-block 'leaf (string->utf8 "child2") empty-refs))
(define child2-hash (store! child2-blk))

(define root-blk (make-block 'root (string->utf8 "root") (vector child1-hash child2-hash)))
(define root-hash (store! root-blk))

;; Standalone block (not in tree)
(define standalone-blk (make-block 'solo (string->utf8 "alone") empty-refs))
(define standalone-hash (store! standalone-blk))

(display "  Created 5 test blocks

")

;;; ====
;;; Pin/Unpin Tests
;;; ====

(display "Pin/Unpin:
")

(test "initial not pinned" #f (pinned? root-hash))
(pin! root-hash)
(test "pinned after pin!" #t (pinned? root-hash))
(unpin! root-hash)
(test "unpinned after unpin!" #f (pinned? root-hash))

;;; ====
;;; Tree Operations
;;; ====

(display "
Tree Operations:
")

;; Reset pins
(hashtable-clear! *pinned*)

;; pin-tree! should pin root and all descendants
(let ([count (pin-tree! root-hash)])
     (test "pin-tree! returns count" 4 count))

(test "root pinned after pin-tree!" #t (pinned? root-hash))
(test "child1 pinned after pin-tree!" #t (pinned? child1-hash))
(test "child2 pinned after pin-tree!" #t (pinned? child2-hash))
(test "grandchild pinned after pin-tree!" #t (pinned? grandchild-hash))
(test "standalone NOT pinned" #f (pinned? standalone-hash))

;; unpin-tree! should unpin root and all descendants
(let ([count (unpin-tree! root-hash)])
     (test "unpin-tree! returns count" 4 count))

(test "root unpinned after unpin-tree!" #f (pinned? root-hash))
(test "child1 unpinned" #f (pinned? child1-hash))
(test "grandchild unpinned" #f (pinned? grandchild-hash))

;;; ====
;;; GC Statistics
;;; ====

(display "
GC Statistics:
")

;; Pin some blocks for stats
(pin! root-hash)
(pin! child1-hash)

(let ([stats (gc-stats)])
     (test "stats total" 5 (cdr (assq 'total stats)))
     (test "stats pinned" 2 (cdr (assq 'pinned stats)))
     (test "stats unpinned" 3 (cdr (assq 'unpinned stats))))

;;; ====
;;; Garbage Collection
;;; ====

(display "
Garbage Collection:
")

;; Reset: pin only root tree
(hashtable-clear! *pinned*)
(pin-tree! root-hash)
;; standalone is unpinned

(test "before gc: 5 blocks" 5 (store-count))
(let-values ([(collected remaining) (gc!)])
            (test "gc collected unpinned" 1 collected)
            (test "gc remaining" 4 remaining))

(test "after gc: 4 blocks" 4 (store-count))
(test "standalone removed" #f (stored? standalone-hash))
(test "root still exists" #t (stored? root-hash))

;;; ====
;;; GC with Roots
;;; ====

(display "
GC with Roots:
")

;; Recreate standalone
(set! standalone-hash (store! standalone-blk))
(test "recreated standalone" #t (stored? standalone-hash))

;; Clear all pins
(hashtable-clear! *pinned*)

;; GC with only child2 as root should keep child2, remove others
(let-values ([(collected remaining) (gc-with-roots! (list child2-hash))])
            (test "gc-with-roots collected" 4 collected)
            (test "gc-with-roots remaining" 1 remaining))

(test "child2 survives" #t (stored? child2-hash))
(test "root gone" #f (stored? root-hash))
(test "child1 gone" #f (stored? child1-hash))
(test "grandchild gone" #f (stored? grandchild-hash))
(test "standalone gone" #f (stored? standalone-hash))

;;; ====
;;; Edge Cases
;;; ====

(display "
Edge Cases:
")

;; GC on empty store
(hashtable-clear! *store*)
(hashtable-clear! *pinned*)

(let-values ([(collected remaining) (gc!)])
            (test "gc empty store collected" 0 collected)
            (test "gc empty store remaining" 0 remaining))

;; Pin non-existent hash
(let ([fake-hash (make-bytevector address-size 0)])
     (pin! fake-hash)
     (test "can pin non-existent" #t (pinned? fake-hash))
     (unpin! fake-hash))

;; pin-tree! on non-existent
(let ([fake-hash (make-bytevector address-size 1)])
     (let ([count (pin-tree! fake-hash)])
          (test "pin-tree! on missing returns 1" 1 count)))

;;; ====
;;; Summary
;;; ====

(newline)
(display "====
")
(display (string-append "Passed: " (number->string tests-passed) "
"))
(display (string-append "Failed: " (number->string tests-failed) "
"))

(if (= tests-failed 0)
    (display "
✓ All CAS lifecycle tests passed!
")
    (display "
✗ Some tests failed!
"))
