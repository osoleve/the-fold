;;; Tests for advanced block query capabilities

(load "fabric/stitches/block.ss")
(load "fabric/stitches/sha256.ss")
(load "fabric/stitches/cas.ss")
(load "thimble/block-query.ss")
(load "thimble/block-query-advanced.ss")

(import (shell block-query))
(import (shell block-query-advanced))

;;; ============================================================
;;; Test Setup: Create blocks with varying characteristics
;;; ============================================================

(display "Setting up test blocks...\n")

;; Small blocks (< 100 bytes)
(define tiny-block
  (make-block 'tiny
              (string->utf8 "x")
              empty-refs))

(define small-block
  (make-block 'small
              (string->utf8 "This is a small payload")
              empty-refs))

;; Medium blocks (100-1000 bytes)
(define medium-block
  (make-block 'medium
              (string->utf8 (make-string 500 #\a))
              empty-refs))

;; Large blocks (> 1000 bytes)
(define large-block
  (make-block 'large
              (string->utf8 (make-string 2000 #\b))
              empty-refs))

;; Store the size test blocks
(define hash-tiny (store! tiny-block))
(define hash-small (store! small-block))
(define hash-medium (store! medium-block))
(define hash-large (store! large-block))

;; Blocks for depth testing
;; Leaf: no references (depth 0)
(define leaf-block
  (make-block 'leaf
              (string->utf8 "leaf node")
              empty-refs))
(define hash-leaf (store! leaf-block))

;; Branch1: references one leaf (depth 1)
(define branch1-block
  (make-block 'branch1
              (string->utf8 "points to leaf")
              (vector hash-leaf)))
(define hash-branch1 (store! branch1-block))

;; Branch2: references branch1 (depth 2)
(define branch2-block
  (make-block 'branch2
              (string->utf8 "points to branch1")
              (vector hash-branch1)))
(define hash-branch2 (store! branch2-block))

;; Multi-ref: references multiple blocks at different depths
(define multi-ref-block
  (make-block 'multi
              (string->utf8 "points to leaf and branch1")
              (vector hash-leaf hash-branch1)))
(define hash-multi (store! multi-ref-block))

;; Deep: references branch2 (depth 3)
(define deep-block
  (make-block 'deep
              (string->utf8 "deep reference chain")
              (vector hash-branch2)))
(define hash-deep (store! deep-block))

;;; ============================================================
;;; Test Size-based Queries
;;; ============================================================

(display "\n=== Testing Size-based Queries ===\n")

(display "Testing query-size with lambda...\n")
(assert ((query-size (lambda (n) (< n 100))) tiny-block))
(assert (not ((query-size (lambda (n) (< n 100))) medium-block)))

(display "Testing payload-size-bytes...\n")
(assert ((payload-size-bytes 1) tiny-block))
(assert (not ((payload-size-bytes 1) small-block)))

(display "Testing payload-size-between...\n")
(assert ((payload-size-between 0 100) tiny-block))
(assert ((payload-size-between 0 100) small-block))
(assert (not ((payload-size-between 0 100) medium-block)))
(assert ((payload-size-between 400 600) medium-block))
(assert ((payload-size-between 1500 2500) large-block))

(display "Testing empty payload detection...\n")
(define empty-block (make-block 'empty empty-payload empty-refs))
(assert ((payload-size-bytes 0) empty-block))

;;; ============================================================
;;; Test Graph Depth Queries
;;; ============================================================

(display "\n=== Testing Graph Depth Queries ===\n")

(display "Testing depth-exactly...\n")
(assert ((depth-exactly 0 fetch) leaf-block))
(assert ((depth-exactly 1 fetch) branch1-block))
(assert ((depth-exactly 2 fetch) branch2-block))
(assert ((depth-exactly 3 fetch) deep-block))
(assert (not ((depth-exactly 1 fetch) leaf-block)))

(display "Testing depth-at-least...\n")
(assert ((depth-at-least 0 fetch) leaf-block))
(assert ((depth-at-least 0 fetch) branch1-block))
(assert ((depth-at-least 2 fetch) branch2-block))
(assert ((depth-at-least 3 fetch) deep-block))
(assert (not ((depth-at-least 1 fetch) leaf-block)))

(display "Testing depth-at-most...\n")
(assert ((depth-at-most 0 fetch) leaf-block))
(assert ((depth-at-most 1 fetch) branch1-block))
(assert ((depth-at-most 3 fetch) deep-block))
(assert (not ((depth-at-most 0 fetch) branch1-block)))

(display "Testing depth-between...\n")
(assert ((depth-between 0 1 fetch) leaf-block))
(assert ((depth-between 0 1 fetch) branch1-block))
(assert ((depth-between 1 3 fetch) branch2-block))
(assert (not ((depth-between 0 1 fetch) branch2-block)))

(display "Testing multi-ref depth (takes max)...\n")
;; multi-ref points to leaf (depth 0) and branch1 (depth 1)
;; So its depth should be max(0, 1) + 1 = 2
(assert ((depth-exactly 2 fetch) multi-ref-block))

;;; ============================================================
;;; Test Time-based Queries
;;; ============================================================

(display "\n=== Testing Time-based Queries ===\n")

;; Create mock timestamp metadata
;; Map blocks to timestamps (simulating creation times)
(define timestamp-table (make-eq-hashtable))

;; Helper to set timestamps
(define (set-timestamp! blk time)
  (hashtable-set! timestamp-table blk time))

;; Set some test timestamps
(set-timestamp! tiny-block 1000)
(set-timestamp! small-block 2000)
(set-timestamp! medium-block 3000)
(set-timestamp! large-block 4000)

;; Metadata function that looks up timestamps
(define (get-timestamp blk)
  (hashtable-ref timestamp-table blk #f))

(display "Testing created-after...\n")
(assert ((created-after 2500 get-timestamp) medium-block))
(assert ((created-after 2500 get-timestamp) large-block))
(assert (not ((created-after 2500 get-timestamp) small-block)))

(display "Testing created-before...\n")
(assert ((created-before 2500 get-timestamp) tiny-block))
(assert ((created-before 2500 get-timestamp) small-block))
(assert (not ((created-before 2500 get-timestamp) medium-block)))

(display "Testing created-between...\n")
(assert ((created-between 1500 3500 get-timestamp) small-block))
(assert ((created-between 1500 3500 get-timestamp) medium-block))
(assert (not ((created-between 1500 3500 get-timestamp) tiny-block)))
(assert (not ((created-between 1500 3500 get-timestamp) large-block)))

(display "Testing with unknown timestamp...\n")
(assert (not ((created-after 1000 get-timestamp) leaf-block)))

;;; ============================================================
;;; Test Higher-Order Query Composition
;;; ============================================================

(display "\n=== Testing Higher-Order Composition ===\n")

(display "Testing query-all-of...\n")
(let ([pattern (query-all-of (list
                              (payload-size-between 0 100)
                              (tag-is 'tiny)))])
     (assert (pattern tiny-block))
     (assert (not (pattern small-block))))

(display "Testing query-any-of...\n")
(let ([pattern (query-any-of (list
                              (tag-is 'tiny)
                              (tag-is 'large)))])
     (assert (pattern tiny-block))
     (assert (pattern large-block))
     (assert (not (pattern medium-block))))

(display "Testing query-none-of...\n")
(let ([pattern (query-none-of (list
                               (tag-is 'tiny)
                               (tag-is 'large)))])
     (assert (pattern medium-block))
     (assert (not (pattern tiny-block)))
     (assert (not (pattern large-block))))

(display "Testing query-count...\n")
(let* ([blocks (list tiny-block small-block medium-block large-block)]
       [counter (query-count (payload-size-between 0 100))])
      (assert (= (counter blocks) 2)))

(display "Testing query-limit...\n")
(let* ([blocks (list tiny-block small-block medium-block large-block)]
       [limiter (query-limit 2)])
      (assert (= (length (limiter blocks)) 2))
      (assert (eq? (car (limiter blocks)) tiny-block)))

(display "Testing query-map...\n")
(let* ([blocks (list tiny-block small-block)]
       [mapper (query-map block-tag)])
      (assert (equal? (mapper blocks) '(tiny small))))

(display "Testing query-pipe...\n")
;; Pipe multiple transformations
(let* ([base-pattern (tag-is 'medium)]
       [enhanced (query-pipe
                  base-pattern
                  (lambda (p) (query-and p (payload-size-between 400 600))))])
      (assert (enhanced medium-block))
      (assert (not (enhanced small-block))))

;;; ============================================================
;;; Combined Advanced Queries
;;; ============================================================

(display "\n=== Testing Combined Queries ===\n")

(display "Testing size + depth combination...\n")
;; Find blocks that are small AND have depth >= 1
(let ([pattern (query-all-of (list
                              (payload-size-between 0 50)
                              (depth-at-least 1 fetch)))])
     (assert (pattern branch1-block))
     (assert (not (pattern leaf-block)))  ;; depth 0
     (assert (not (pattern medium-block)))) ;; too large

(display "Testing size + timestamp combination...\n")
;; Find medium-sized blocks created in a time range
(let ([pattern (query-all-of (list
                              (payload-size-between 100 1000)
                              (created-between 2500 3500 get-timestamp)))])
     (assert (pattern medium-block))
     (assert (not (pattern small-block)))   ;; too small
     (assert (not (pattern large-block))))  ;; outside time range

(display "Testing complex composition...\n")
;; Find blocks that are either:
;; - Small and created early, OR
;; - Large and deep
(let ([pattern (query-any-of (list
                              (query-all-of (list
                                             (payload-size-between 0 100)
                                             (created-before 2000 get-timestamp)))
                              (query-all-of (list
                                             (payload-size-between 1000 10000)
                                             (depth-at-least 2 fetch)))))])
     (assert (pattern tiny-block))  ;; small and early
     ;; Would need a large deep block to test second condition
     (assert (not (pattern medium-block))))

;;; ============================================================
;;; Integration with Basic Queries
;;; ============================================================

(display "\n=== Testing Integration with Basic Queries ===\n")

(display "Testing advanced + basic query combination...\n")
;; Combine basic query (from shell/block-query.ss) with advanced
(let ([pattern (query-all-of (list
                              (tag-matches "^branch")
                              (depth-at-least 1 fetch)
                              (payload-size-between 0 100)))])
     (assert (pattern branch1-block))
     (assert (pattern branch2-block))
     (assert (not (pattern leaf-block))))

(display "Testing query-and with size and content...\n")
;; Use query-and from basic library with advanced size query
(let ([pattern (query-and
                (payload-contains "leaf")
                (payload-size-between 5 20))])
     (assert (pattern leaf-block))
     (assert (not (pattern branch1-block))))

(display "\nAll advanced block query tests passed!\n")
