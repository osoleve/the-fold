;;; store-api-demo.ss — Demonstrating Persistent Knowledge Storage
;;;
;;; This shows how the Store API makes knowledge graphs truly eternal.

(source-directories (cons "core" (source-directories)))
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "boundary/io/fs.ss")
(load "boundary/storage/store-api.ss")

(printf "\n╔════════════════════════════════════════════════════════════╗\n")
(printf "║         STORE API DEMONSTRATION: ETERNAL KNOWLEDGE         ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

;;; ====
;;; Build a Small Knowledge Graph
;;; ====

(printf "=== Creating Knowledge Graph ===\n\n")

(define (make-entity type name attributes)
  (make-block 'entity
              (string->utf8 (format "~a:~a ~s" type name attributes))
              (vector)))

(define (make-relation source-hash target-hash rel-type metadata)
  (make-block 'relation
              (string->utf8 (format "~a ~s" rel-type metadata))
              (vector source-hash target-hash)))

;; Create some entities
(define dijkstra (make-entity 'person "Edsger Dijkstra"
                              '((birth . 1930) (death . 2002))))
(define goto-harmful (make-entity 'paper "Go To Statement Considered Harmful"
                                  '((year . 1968))))
(define structured-prog (make-entity 'concept "Structured Programming"
                                     '((year . 1968))))

;; Create relations
(define r1 (make-relation (hash-block dijkstra) (hash-block goto-harmful)
                          'wrote "Famous critique"))
(define r2 (make-relation (hash-block dijkstra) (hash-block structured-prog)
                          'pioneered "Advocated for clarity"))

(printf "✓ Created 3 entities + 2 relations\n\n")

;;; ====
;;; Demonstrate Store API
;;; ====

(printf "=== Using Store API ===\n\n")

(define fs (mint-fs-capability ".store"))

;; DEMO 1: Store blocks and get hashes
(printf "DEMO 1: Storing blocks\n")
(printf "─────────────────────────────────────\n")

(define dijkstra-hash (store-put! fs dijkstra))
(define goto-hash (store-put! fs goto-harmful))
(define struct-hash (store-put! fs structured-prog))
(define r1-hash (store-put! fs r1))
(define r2-hash (store-put! fs r2))

(printf "Stored 5 blocks:\n")
(printf "  Dijkstra:           ~a\n" (hash->hex dijkstra-hash))
(printf "  Go To Paper:        ~a\n" (hash->hex goto-hash))
(printf "  Structured Prog:    ~a\n" (hash->hex struct-hash))
(printf "  Relation 1:         ~a\n" (hash->hex r1-hash))
(printf "  Relation 2:         ~a\n" (hash->hex r2-hash))
(printf "\n")

;; DEMO 2: Retrieve blocks by hash
(printf "DEMO 2: Retrieving blocks\n")
(printf "─────────────────────────────────────\n")

(define retrieved-dijkstra (store-get fs dijkstra-hash))
(printf "Retrieved Dijkstra entity: ~a\n"
        (if retrieved-dijkstra
            (utf8->string (block-payload retrieved-dijkstra))
            "NOT FOUND"))
(printf "\n")

;; DEMO 3: Check existence
(printf "DEMO 3: Existence checks\n")
(printf "─────────────────────────────────────\n")

(printf "Dijkstra exists? ~a\n" (store-exists? fs dijkstra-hash))
(printf "Random hash exists? ~a\n"
        (store-exists? fs (make-bytevector 32 0)))
(printf "\n")

;; DEMO 4: Query by tag
(printf "DEMO 4: Query by tag\n")
(printf "─────────────────────────────────────\n")

(define all-entities (store-get-entities fs))
(define all-relations (store-get-relations fs))

(printf "Total entities in store: ~a\n" (length all-entities))
(printf "Total relations in store: ~a\n" (length all-relations))
(printf "\nEntities found:\n")
(for-each (lambda (e)
                  (printf "  • ~a\n" (utf8->string (block-payload e))))
          all-entities)
(printf "\n")

;; DEMO 5: Search by payload content
(printf "DEMO 5: Content search\n")
(printf "─────────────────────────────────────\n")

(define structured-blocks (store-find-by-payload-contains fs "Structured"))
(printf "Blocks containing 'Structured': ~a\n" (length structured-blocks))
(for-each (lambda (b)
                  (printf "  • [~a] ~a\n"
                          (block-tag b)
                          (utf8->string (block-payload b))))
          structured-blocks)
(printf "\n")

;; DEMO 6: Find referrers
(printf "DEMO 6: Graph navigation - who references Dijkstra?\n")
(printf "─────────────────────────────────────\n")

(define dijkstra-referrers (store-get-referrers fs dijkstra-hash))
(printf "Blocks referencing Dijkstra: ~a\n" (length dijkstra-referrers))
(for-each (lambda (b)
                  (printf "  • [~a] ~a\n"
                          (block-tag b)
                          (utf8->string (block-payload b))))
          dijkstra-referrers)
(printf "\n")

;; DEMO 7: Get references
(printf "DEMO 7: Graph navigation - what does relation r1 reference?\n")
(printf "─────────────────────────────────────\n")

(define r1-refs (store-get-refs fs r1))
(printf "Relation r1 references ~a blocks:\n" (length r1-refs))
(for-each (lambda (b)
                  (printf "  • [~a] ~a\n"
                          (block-tag b)
                          (utf8->string (block-payload b))))
          r1-refs)
(printf "\n")

;; DEMO 8: Statistics
(printf "DEMO 8: Store statistics\n")
(printf "─────────────────────────────────────\n")

(store-print-stats fs)
(printf "\n")

;; DEMO 9: Batch operations
(printf "DEMO 9: Batch operations\n")
(printf "─────────────────────────────────────\n")

(define new-entities
  (list (make-entity 'person "Donald Knuth" '((birth . 1938)))
        (make-entity 'book "The Art of Computer Programming" '((year . 1968)))))

(define new-hashes (store-put-many! fs new-entities))
(printf "Stored ~a new entities in batch\n" (length new-hashes))
(printf "  Hashes: ~a\n" (map (lambda (h) (substring (hash->hex h) 0 8))
                              new-hashes))

(define retrieved-many (store-get-many fs new-hashes))
(printf "Retrieved ~a blocks in batch\n" (length retrieved-many))
(printf "\n")

;;; ====
;;; Summary
;;; ====

(printf "╔════════════════════════════════════════════════════════════╗\n")
(printf "║                  DEMONSTRATION COMPLETE                     ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

(printf "What we demonstrated:\n\n")
(printf "  ✓ store-put! / store-get    - Basic storage & retrieval\n")
(printf "  ✓ store-exists?             - Existence checking\n")
(printf "  ✓ store-get-entities        - Query by tag\n")
(printf "  ✓ store-find-by-payload-contains - Content search\n")
(printf "  ✓ store-get-referrers       - Find inbound references\n")
(printf "  ✓ store-get-refs            - Get referenced blocks\n")
(printf "  ✓ store-print-stats         - Store analytics\n")
(printf "  ✓ store-put-many!           - Batch operations\n\n")

(printf "The Store API makes knowledge ETERNAL:\n")
(printf "  • Blocks persist across sessions\n")
(printf "  • Content-addressed (same data → same hash)\n")
(printf "  • Queryable (by tag, content, references)\n")
(printf "  • Navigable (traverse graph structure)\n")
(printf "  • Pure functional (deterministic operations)\n\n")

(define final-stats (store-stats fs))
(printf "Final store state: ~a blocks, ~a bytes\n"
        (cdr (assq 'total final-stats))
        (cdr (assq 'total-bytes final-stats)))
(printf "\n")
