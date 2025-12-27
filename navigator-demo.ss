;;; navigator-demo.ss — Demonstrating the Block Navigator with Knowledge Engine
;;;
;;; This combines the Knowledge Engine I built with the existing Block Navigator
;;; to show off both systems working in harmony!

;; Load dependencies
(source-directories (cons "fabric/stitches" (source-directories)))
(load "fabric/stitches/block.ss")
(load "fabric/stitches/sha256.ss")
(load "thimble/fs.ss")
(load "thimble/block-navigator.ss")

(printf "\n╔════════════════════════════════════════════════════════════╗\n")
(printf "║   BLOCK NAVIGATOR + KNOWLEDGE ENGINE DEMONSTRATION         ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

;;; ============================================================
;;; Build Knowledge Graph (from knowledge-engine.ss)
;;; ============================================================

(printf "=== Building Knowledge Graph ===\n\n")

;; Entity constructors
(define (make-entity type name attributes)
  (make-block 'entity
              (string->utf8 (format "~a:~a ~s" type name attributes))
              (vector)))

(define (make-relation source-hash target-hash rel-type metadata)
  (make-block 'relation
              (string->utf8 (format "~a ~s" rel-type metadata))
              (vector source-hash target-hash)))

(define (make-collection coll-type name member-hashes)
  (make-block 'collection
              (string->utf8 (format "~a:~a (~a members)" coll-type name (length member-hashes)))
              (list->vector member-hashes)))

;; Build the graph
(define turing (make-entity 'person "Alan Turing" '((birth . 1912) (death . 1954))))
(define church (make-entity 'person "Alonzo Church" '((birth . 1903) (death . 1995))))
(define mccarthy (make-entity 'person "John McCarthy" '((birth . 1927) (death . 2011))))
(define sussman (make-entity 'person "Gerald Sussman" '((birth . 1947))))
(define abelson (make-entity 'person "Hal Abelson" '((birth . 1947))))

(define turing-machine (make-entity 'concept "Turing Machine" '((year . 1936))))
(define lambda-calculus (make-entity 'concept "Lambda Calculus" '((year . 1936))))
(define lisp-lang (make-entity 'concept "Lisp" '((year . 1958))))
(define scheme-lang (make-entity 'concept "Scheme" '((year . 1975))))
(define sicp-book (make-entity 'concept "SICP" '((year . 1985))))

(define r1 (make-relation (hash-block turing) (hash-block turing-machine)
                          'invented "Foundational computation model"))
(define r2 (make-relation (hash-block church) (hash-block lambda-calculus)
                          'invented "Functional computation"))
(define r3 (make-relation (hash-block mccarthy) (hash-block lisp-lang)
                          'created "First functional language"))
(define r4 (make-relation (hash-block sussman) (hash-block scheme-lang)
                          'co-created "Minimalist Lisp"))
(define r5 (make-relation (hash-block sussman) (hash-block sicp-book)
                          'co-authored "Classic textbook"))
(define r6 (make-relation (hash-block abelson) (hash-block sicp-book)
                          'co-authored "Classic textbook"))
(define r7 (make-relation (hash-block lambda-calculus) (hash-block lisp-lang)
                          'influenced "Lambda → Lisp"))
(define r8 (make-relation (hash-block lisp-lang) (hash-block scheme-lang)
                          'influenced "Lisp → Scheme"))
(define r9 (make-relation (hash-block scheme-lang) (hash-block sicp-book)
                          'used-in "Scheme in SICP"))

(define people (make-collection 'people "Pioneers"
                               (list (hash-block turing) (hash-block church)
                                     (hash-block mccarthy) (hash-block sussman)
                                     (hash-block abelson))))

(define concepts (make-collection 'concepts "Core Ideas"
                                 (list (hash-block turing-machine)
                                       (hash-block lambda-calculus)
                                       (hash-block lisp-lang)
                                       (hash-block scheme-lang)
                                       (hash-block sicp-book))))

(define relations (make-collection 'relations "Connections"
                                  (list (hash-block r1) (hash-block r2)
                                        (hash-block r3) (hash-block r4)
                                        (hash-block r5) (hash-block r6)
                                        (hash-block r7) (hash-block r8)
                                        (hash-block r9))))

(define kg (make-collection 'knowledge-graph "History of Computation"
                           (list (hash-block people)
                                 (hash-block concepts)
                                 (hash-block relations))))

(printf "✓ Created knowledge graph with 23 blocks\n")
(printf "  Root: ~a\n\n" (hash->hex (hash-block kg)))

;;; ============================================================
;;; Store Blocks in Filesystem
;;; ============================================================

(printf "=== Storing Blocks ===\n\n")

(define fs (mint-fs-capability ".store"))

;; Store all blocks
(define all-blocks
  (list turing church mccarthy sussman abelson
        turing-machine lambda-calculus lisp-lang scheme-lang sicp-book
        r1 r2 r3 r4 r5 r6 r7 r8 r9
        people concepts relations kg))

(define stored-count 0)
(for-each (lambda (block)
            (fs-store! fs block)
            (set! stored-count (+ stored-count 1)))
          all-blocks)

(printf "✓ Stored ~a blocks in .store\n\n" stored-count)

;;; ============================================================
;;; Demonstrate Block Navigator Features
;;; ============================================================

(printf "╔════════════════════════════════════════════════════════════╗\n")
(printf "║              DEMONSTRATION: BLOCK NAVIGATOR                ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

;;; DEMO 1: Block Statistics
(printf "─────────────────────────────────────────────────────────────\n")
(printf " DEMO 1: Store Statistics\n")
(printf "─────────────────────────────────────────────────────────────\n\n")

(block-stats fs)

;;; DEMO 2: Tree Visualization
(printf "\n─────────────────────────────────────────────────────────────\n")
(printf " DEMO 2: Knowledge Graph Tree Visualization\n")
(printf "─────────────────────────────────────────────────────────────\n\n")

(let ([kg-hash-hex (hash->hex (hash-block kg))])
  (visualize-tree fs (substring kg-hash-hex 0 8) 3))

;;; DEMO 3: Explore a specific block
(printf "\n─────────────────────────────────────────────────────────────\n")
(printf " DEMO 3: Exploring Alan Turing Entity\n")
(printf "─────────────────────────────────────────────────────────────\n\n")

(let ([turing-hash-hex (hash->hex (hash-block turing))])
  (describe-block fs (hash-block turing)))

;;; DEMO 4: Find Popular Blocks
(printf "─────────────────────────────────────────────────────────────\n")
(printf " DEMO 4: Most Referenced Blocks\n")
(printf "─────────────────────────────────────────────────────────────\n\n")

(find-popular fs 5)

;;; DEMO 5: Search
(printf "─────────────────────────────────────────────────────────────\n")
(printf " DEMO 5: Search for 'Lambda'\n")
(printf "─────────────────────────────────────────────────────────────\n\n")

(search-ranked fs "Lambda")

;;; DEMO 6: Find Orphans
(printf "─────────────────────────────────────────────────────────────\n")
(printf " DEMO 6: Orphan Detection\n")
(printf "─────────────────────────────────────────────────────────────\n\n")

(find-orphans fs)

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "\n╔════════════════════════════════════════════════════════════╗\n")
(printf "║                    DEMONSTRATION COMPLETE                   ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

(printf "What we demonstrated:\n\n")
(printf "  ✓ Built a 23-block knowledge graph\n")
(printf "  ✓ Stored it in the content-addressed .store\n")
(printf "  ✓ Analyzed block statistics and distribution\n")
(printf "  ✓ Visualized the merkle tree structure\n")
(printf "  ✓ Explored individual blocks\n")
(printf "  ✓ Found most-referenced blocks\n")
(printf "  ✓ Searched for content\n")
(printf "  ✓ Detected orphan blocks\n\n")

(printf "The Block Navigator and Knowledge Engine work together perfectly!\n")
(printf "Every block is eternal. Every reference is verifiable. Every query is pure.\n\n")

(printf "Knowledge Graph Root: ~a\n\n" (hash->hex (hash-block kg)))
