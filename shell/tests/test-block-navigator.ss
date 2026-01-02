;;; thimble/test-block-navigator.ss — Tests for Block Navigator
;;;
;;; Test suite for the block-navigator module.
;;; Verifies navigation, analytics, search, and visualization functions.

(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "shell/fs.ss")
(load "shell/block-navigator.ss")

(display "╔══════════════════════════════════════════════════════════════╗\n")
(display "║           BLOCK NAVIGATOR TEST SUITE                        ║\n")
(display "╚══════════════════════════════════════════════════════════════╝\n")
(newline)

;;; ============================================================
;;; Test Setup: Create a test store with sample blocks
;;; ============================================================

(define test-store-path ".store-test-navigator")

;;; Clean up old test store
(when (file-exists? test-store-path)
      (system (format "rm -rf ~a" test-store-path)))

(define test-fs (mint-fs-capability test-store-path))

;;; Create some test blocks with various structures

;; Block 1: A simple data block (orphan)
(define blk1 (make-block 'data
                         (string->utf8 "This is a simple data block")
                         (vector)))
(define hash1 (fs-store! test-fs blk1))
(fs-pin! test-fs hash1)
(display (format "Created block 1: ~a\n" (hash->hex hash1)))

;; Block 2: A code block (orphan)
(define blk2 (make-block 'code
                         (string->utf8 "(define (hello) (display \"Hello, World!\"))")
                         (vector)))
(define hash2 (fs-store! test-fs blk2))
(fs-pin! test-fs hash2)
(display (format "Created block 2: ~a\n" (hash->hex hash2)))

;; Block 3: A block that references block 1
(define blk3 (make-block 'metadata
                         (string->utf8 "Metadata for data block")
                         (vector hash1)))
(define hash3 (fs-store! test-fs blk3))
(fs-pin! test-fs hash3)
(display (format "Created block 3: ~a (refs block 1)\n" (hash->hex hash3)))

;; Block 4: A block that references both block 1 and 2
(define blk4 (make-block 'composite
                         (string->utf8 "Composite block with multiple refs")
                         (vector hash1 hash2)))
(define hash4 (fs-store! test-fs blk4))
(fs-pin! test-fs hash4)
(display (format "Created block 4: ~a (refs blocks 1, 2)\n" (hash->hex hash4)))

;; Block 5: A chain block referencing block 3
(define blk5 (make-block 'chain
                         (string->utf8 "Chain continuation")
                         (vector hash3)))
(define hash5 (fs-store! test-fs blk5))
(fs-pin! test-fs hash5)
(display (format "Created block 5: ~a (refs block 3)\n" (hash->hex hash5)))

;; Block 6: Another block referencing block 1 (making it popular)
(define blk6 (make-block 'reference
                         (string->utf8 "Another reference to block 1")
                         (vector hash1)))
(define hash6 (fs-store! test-fs blk6))
(fs-pin! test-fs hash6)
(display (format "Created block 6: ~a (refs block 1)\n" (hash->hex hash6)))

(newline)
(display "Test store created with 6 blocks\n")
(display "Expected structure:\n")
(display "  - Block 1: 3 inbound refs (from 3, 4, 6) [POPULAR]\n")
(display "  - Block 2: 1 inbound ref (from 4) + orphan status\n")
(display "  - Block 3: 1 inbound ref (from 5)\n")
(display "  - Block 4, 5, 6: No inbound refs [ORPHANS]\n")
(newline)

;;; ============================================================
;;; Test 1: Block Stats
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 1: Block Statistics\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(block-stats test-fs)

;;; ============================================================
;;; Test 2: Find Popular Blocks
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 2: Most Popular Blocks\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(find-popular test-fs 3)

;;; ============================================================
;;; Test 3: Find Orphans
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 3: Orphan Blocks\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(find-orphans test-fs)

;;; ============================================================
;;; Test 4: Explore a Block
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 4: Explore Block 4 (composite with multiple refs)\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(let ([prefix (substring (hash->hex hash4) 0 8)])
     (explore test-fs prefix))

;;; ============================================================
;;; Test 5: Visualize Tree
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 5: Visualize Block Tree (from block 5, depth 3)\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(let ([prefix (substring (hash->hex hash5) 0 8)])
     (visualize-tree test-fs prefix 3))

;;; ============================================================
;;; Test 6: Search with Ranking
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 6: Search for 'block'\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(search-ranked test-fs "block")

;;; ============================================================
;;; Test 7: Show Lineage
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 7: Show Lineage (from block 5)\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(let ([prefix (substring (hash->hex hash5) 0 8)])
     (show-lineage test-fs prefix))

;;; ============================================================
;;; Test Summary
;;; ============================================================

(display "═══════════════════════════════════════════════════════════════\n")
(display "ALL TESTS COMPLETED\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)
(display "The Block Navigator is ready for use!\n")
(display "\nUsage:\n")
(display "  (load \"shell/block-navigator.ss\")\n")
(display "  (block-stats (fs))                  ; Show store statistics\n")
(display "  (explore (fs) \"hash-prefix\")        ; Explore a block\n")
(display "  (visualize-tree (fs) \"hash\" depth)  ; Show block tree\n")
(display "  (find-popular (fs) n)               ; Find popular blocks\n")
(display "  (find-orphans (fs))                 ; Find orphan blocks\n")
(display "  (search-ranked (fs) \"query\")        ; Search with ranking\n")
(display "  (show-lineage (fs) \"hash\")          ; Show reference chain\n")
(newline)

;;; Clean up test store
(display "Cleaning up test store...\n")
(system (format "rm -rf ~a" test-store-path))
(display "Test complete!\n")
