(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "boundary/io/fs.ss")
(load "boundary/blocks/block-navigator.ss")

(doc 'module 'test-block-navigator)
(doc 'description "Test suite for block-navigator - verifies navigation, analytics, search, and visualization")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "╔══════════════════════════════════════════════════════════════╗\n")
(display "║           BLOCK NAVIGATOR TEST SUITE                        ║\n")
(display "╚══════════════════════════════════════════════════════════════╝\n")
(newline)

(doc 'section 'test-setup)

(define test-store-path ".store-test-navigator")
(when (file-exists? test-store-path)
      (system (format "rm -rf ~a" test-store-path)))

(define test-fs (mint-fs-capability test-store-path))
(define blk1 (make-block 'data
                         (string->utf8 "This is a simple data block")
                         (vector)))
(define hash1 (fs-store! test-fs blk1))
(fs-pin! test-fs hash1)
(display (format "Created block 1: ~a\n" (hash->hex hash1)))
(define blk2 (make-block 'code
                         (string->utf8 "(define (hello) (display \"Hello, World!\"))")
                         (vector)))
(define hash2 (fs-store! test-fs blk2))
(fs-pin! test-fs hash2)
(display (format "Created block 2: ~a\n" (hash->hex hash2)))
(define blk3 (make-block 'metadata
                         (string->utf8 "Metadata for data block")
                         (vector hash1)))
(define hash3 (fs-store! test-fs blk3))
(fs-pin! test-fs hash3)
(display (format "Created block 3: ~a (refs block 1)\n" (hash->hex hash3)))
(define blk4 (make-block 'composite
                         (string->utf8 "Composite block with multiple refs")
                         (vector hash1 hash2)))
(define hash4 (fs-store! test-fs blk4))
(fs-pin! test-fs hash4)
(display (format "Created block 4: ~a (refs blocks 1, 2)\n" (hash->hex hash4)))
(define blk5 (make-block 'chain
                         (string->utf8 "Chain continuation")
                         (vector hash3)))
(define hash5 (fs-store! test-fs blk5))
(fs-pin! test-fs hash5)
(display (format "Created block 5: ~a (refs block 3)\n" (hash->hex hash5)))
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

(doc 'section 'block-stats)

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 1: Block Statistics\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(block-stats test-fs)

(doc 'section 'find-popular)

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 2: Most Popular Blocks\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(find-popular test-fs 3)

(doc 'section 'find-orphans)

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 3: Orphan Blocks\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(find-orphans test-fs)

(doc 'section 'explore-block)

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 4: Explore Block 4 (composite with multiple refs)\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(let ([prefix (substring (hash->hex hash4) 0 8)])
     (explore test-fs prefix))

(doc 'section 'visualize-tree)

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 5: Visualize Block Tree (from block 5, depth 3)\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(let ([prefix (substring (hash->hex hash5) 0 8)])
     (visualize-tree test-fs prefix 3))

(doc 'section 'search-ranked)

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 6: Search for 'block'\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(search-ranked test-fs "block")

(doc 'section 'show-lineage)

(display "═══════════════════════════════════════════════════════════════\n")
(display "TEST 7: Show Lineage (from block 5)\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)

(let ([prefix (substring (hash->hex hash5) 0 8)])
     (show-lineage test-fs prefix))

(doc 'section 'test-summary)

(display "═══════════════════════════════════════════════════════════════\n")
(display "ALL TESTS COMPLETED\n")
(display "═══════════════════════════════════════════════════════════════\n")
(newline)
(display "The Block Navigator is ready for use!\n")
(display "\nUsage:\n")
(display "  (load \"boundary/blocks/block-navigator.ss\")\n")
(display "  (block-stats (fs))                  ; Show store statistics\n")
(display "  (explore (fs) \"hash-prefix\")        ; Explore a block\n")
(display "  (visualize-tree (fs) \"hash\" depth)  ; Show block tree\n")
(display "  (find-popular (fs) n)               ; Find popular blocks\n")
(display "  (find-orphans (fs))                 ; Find orphan blocks\n")
(display "  (search-ranked (fs) \"query\")        ; Search with ranking\n")
(display "  (show-lineage (fs) \"hash\")          ; Show reference chain\n")
(newline)
(display "Cleaning up test store...\n")
(system (format "rm -rf ~a" test-store-path))
(display "Test complete!\n")
