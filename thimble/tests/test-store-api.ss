;;; test-store-api.ss --- Comprehensive test suite for Store API

(source-directories (cons "fabric/stitches" (source-directories)))
(load "fabric/stitches/block.ss")
(load "fabric/stitches/sha256.ss")
(load "thimble/fs.ss")
(load "thimble/store-api.ss")

(printf "\n")
(printf "================================================================\n")
(printf "              STORE API TEST SUITE                             \n")
(printf "================================================================\n\n")

;;; ============================================================
;;; Test Harness
;;; ============================================================

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (printf "  [PASS] ~a\n" name))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (printf "  [FAIL] ~a\n" name)
        (printf "      Expected: ~s\n" expected)
        (printf "      Got:      ~s\n" actual))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-pred name pred actual)
  (if (pred actual)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (printf "  [PASS] ~a\n" name))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (printf "  [FAIL] ~a\n" name)
        (printf "      Value ~s did not satisfy predicate\n" actual))))

;;; ============================================================
;;; Test Setup - Create test store and blocks
;;; ============================================================

(printf "Setting up test environment...\n")
(printf "----------------------------------------------------------------\n")

;; Use a dedicated test store directory
(define test-store-path ".test-store-api")

;; Clean up any previous test store
(when (file-exists? test-store-path)
  (system (format "rm -rf ~a" test-store-path)))

(define fs (mint-fs-capability test-store-path))

;; Create test blocks with different tags
(define entity-alice (make-block 'entity (string->utf8 "name:Alice") (vector)))
(define entity-bob (make-block 'entity (string->utf8 "name:Bob") (vector)))
(define entity-charlie (make-block 'entity (string->utf8 "name:Charlie") (vector)))

;; Relations that reference entities
(define hash-alice (hash-block entity-alice))
(define hash-bob (hash-block entity-bob))
(define hash-charlie (hash-block entity-charlie))

(define relation-alice-knows-bob
  (make-block 'relation (string->utf8 "knows")
              (vector hash-alice hash-bob)))

(define relation-bob-likes-charlie
  (make-block 'relation (string->utf8 "likes")
              (vector hash-bob hash-charlie)))

;; Collection block
(define collection-people
  (make-block 'collection (string->utf8 "people")
              (vector hash-alice hash-bob hash-charlie)))

;; Attribute block
(define attr-age (make-block 'attribute (string->utf8 "age:30") (vector)))

;; Event block
(define event-meeting (make-block 'event (string->utf8 "meeting:2024-01-15") (vector)))

(printf "Test blocks created.\n\n")

;;; ============================================================
;;; Test 1: Core Storage Operations (store-put!, store-get, store-exists?)
;;; ============================================================

(printf "Testing Core Storage Operations:\n")
(printf "----------------------------------------------------------------\n")

;; Test store-put! returns hash
(define stored-alice-hash (store-put! fs entity-alice))
(test-pred "store-put! returns a bytevector hash"
           bytevector?
           stored-alice-hash)

(test "store-put! returns correct hash"
      hash-alice
      stored-alice-hash)

;; Store more blocks
(store-put! fs entity-bob)
(store-put! fs entity-charlie)
(store-put! fs relation-alice-knows-bob)
(store-put! fs relation-bob-likes-charlie)
(store-put! fs collection-people)
(store-put! fs attr-age)
(store-put! fs event-meeting)

;; Test store-get retrieves correct block
(define retrieved-alice (store-get fs hash-alice))
(test-pred "store-get returns a block"
           (lambda (x) (not (eq? x #f)))
           retrieved-alice)

(test "store-get returns block with correct tag"
      'entity
      (block-tag retrieved-alice))

(test "store-get returns block with correct payload"
      "name:Alice"
      (utf8->string (block-payload retrieved-alice)))

;; Test store-get with non-existent hash
(define fake-hash (make-bytevector address-size 0))  ; All zeros bytevector
(test "store-get with non-existent hash returns #f"
      #f
      (store-get fs fake-hash))

;; Test store-exists?
(test-true "store-exists? returns #t for existing block"
           (store-exists? fs hash-alice))

(test-false "store-exists? returns #f for non-existent block"
            (store-exists? fs fake-hash))

(printf "\n")

;;; ============================================================
;;; Test 2: Store Enumeration (store-all-hashes, store-count)
;;; ============================================================

(printf "Testing Store Enumeration:\n")
(printf "----------------------------------------------------------------\n")

(define all-hashes (store-all-hashes fs))
(test-pred "store-all-hashes returns a list"
           list?
           all-hashes)

(test "store-all-hashes returns correct count"
      8
      (length all-hashes))

(test "store-count matches all-hashes length"
      (length all-hashes)
      (store-count fs))

(test "store-count returns 8 blocks"
      8
      (store-count fs))

;; Verify all stored hashes are present (member returns list, not #t)
(test-pred "all-hashes contains alice"
           (lambda (x) x)  ; truthy check
           (member hash-alice all-hashes))

(test-pred "all-hashes contains bob"
           (lambda (x) x)  ; truthy check
           (member hash-bob all-hashes))

(printf "\n")

;;; ============================================================
;;; Test 3: store-all-blocks
;;; ============================================================

(printf "Testing store-all-blocks:\n")
(printf "----------------------------------------------------------------\n")

(define all-blocks (store-all-blocks fs))
(test-pred "store-all-blocks returns a list"
           list?
           all-blocks)

(test "store-all-blocks returns correct count"
      8
      (length all-blocks))

(test-pred "store-all-blocks returns actual blocks"
           (lambda (bs)
             (and (not (null? bs))
                  (block? (car bs))))
           all-blocks)

(printf "\n")

;;; ============================================================
;;; Test 4: store-filter
;;; ============================================================

(printf "Testing store-filter:\n")
(printf "----------------------------------------------------------------\n")

;; Filter for entities only
(define entity-blocks (store-filter fs (lambda (b) (eq? (block-tag b) 'entity))))
(test "store-filter finds all entities"
      3
      (length entity-blocks))

;; Filter for relations only
(define relation-blocks (store-filter fs (lambda (b) (eq? (block-tag b) 'relation))))
(test "store-filter finds all relations"
      2
      (length relation-blocks))

;; Filter with no matches
(define no-matches (store-filter fs (lambda (b) (eq? (block-tag b) 'nonexistent))))
(test "store-filter returns empty list for no matches"
      '()
      no-matches)

;; Filter all blocks (always true predicate)
(define all-via-filter (store-filter fs (lambda (b) #t)))
(test "store-filter with always-true predicate returns all"
      8
      (length all-via-filter))

(printf "\n")

;;; ============================================================
;;; Test 5: store-find-by-tag
;;; ============================================================

(printf "Testing store-find-by-tag:\n")
(printf "----------------------------------------------------------------\n")

(define found-entities (store-find-by-tag fs 'entity))
(test "store-find-by-tag 'entity count"
      3
      (length found-entities))

(define found-relations (store-find-by-tag fs 'relation))
(test "store-find-by-tag 'relation count"
      2
      (length found-relations))

(define found-collections (store-find-by-tag fs 'collection))
(test "store-find-by-tag 'collection count"
      1
      (length found-collections))

(define found-attributes (store-find-by-tag fs 'attribute))
(test "store-find-by-tag 'attribute count"
      1
      (length found-attributes))

(define found-events (store-find-by-tag fs 'event))
(test "store-find-by-tag 'event count"
      1
      (length found-events))

(define found-none (store-find-by-tag fs 'nonexistent-tag))
(test "store-find-by-tag with no matches returns empty"
      0
      (length found-none))

(printf "\n")

;;; ============================================================
;;; Test 6: store-find-by-payload
;;; ============================================================

(printf "Testing store-find-by-payload:\n")
(printf "----------------------------------------------------------------\n")

;; Find blocks with payloads starting with "name:"
(define name-blocks
  (store-find-by-payload fs
                          (lambda (bv)
                            (let ([s (utf8->string bv)])
                              (and (>= (string-length s) 5)
                                   (string=? (substring s 0 5) "name:"))))))
(test "store-find-by-payload finds name: blocks"
      3
      (length name-blocks))

;; Find blocks with specific payload content
(define age-blocks
  (store-find-by-payload fs
                          (lambda (bv)
                            (string=? (utf8->string bv) "age:30"))))
(test "store-find-by-payload finds exact match"
      1
      (length age-blocks))

;; No matches
(define no-payload-match
  (store-find-by-payload fs
                          (lambda (bv)
                            (string=? (utf8->string bv) "nonexistent"))))
(test "store-find-by-payload with no matches returns empty"
      0
      (length no-payload-match))

(printf "\n")

;;; ============================================================
;;; Test 7: store-find-by-payload-contains
;;; ============================================================

(printf "Testing store-find-by-payload-contains:\n")
(printf "----------------------------------------------------------------\n")

(define contains-alice (store-find-by-payload-contains fs "Alice"))
(test "store-find-by-payload-contains 'Alice'"
      1
      (length contains-alice))

(define contains-name (store-find-by-payload-contains fs "name:"))
(test "store-find-by-payload-contains 'name:'"
      3
      (length contains-name))

(define contains-knows (store-find-by-payload-contains fs "knows"))
(test "store-find-by-payload-contains 'knows'"
      1
      (length contains-knows))

(define contains-nothing (store-find-by-payload-contains fs "zzzznotfound"))
(test "store-find-by-payload-contains with no matches"
      0
      (length contains-nothing))

;; Test partial substring matching
(define contains-lic (store-find-by-payload-contains fs "lic"))
(test "store-find-by-payload-contains partial 'lic'"
      1
      (length contains-lic))

(printf "\n")

;;; ============================================================
;;; Test 8: store-find-by-ref
;;; ============================================================

(printf "Testing store-find-by-ref:\n")
(printf "----------------------------------------------------------------\n")

;; Find blocks referencing Alice
(define refs-alice (store-find-by-ref fs hash-alice))
(test "store-find-by-ref finds blocks referencing Alice"
      2  ;; relation-alice-knows-bob and collection-people
      (length refs-alice))

;; Find blocks referencing Bob
(define refs-bob (store-find-by-ref fs hash-bob))
(test "store-find-by-ref finds blocks referencing Bob"
      3  ;; relation-alice-knows-bob, relation-bob-likes-charlie, collection-people
      (length refs-bob))

;; Find blocks referencing Charlie
(define refs-charlie (store-find-by-ref fs hash-charlie))
(test "store-find-by-ref finds blocks referencing Charlie"
      2  ;; relation-bob-likes-charlie and collection-people
      (length refs-charlie))

;; Non-existent reference
(define refs-none (store-find-by-ref fs fake-hash))
(test "store-find-by-ref with no referrers"
      0
      (length refs-none))

(printf "\n")

;;; ============================================================
;;; Test 9: Batch Operations (store-put-many!, store-get-many)
;;; ============================================================

(printf "Testing Batch Operations:\n")
(printf "----------------------------------------------------------------\n")

;; Create new blocks for batch test
(define batch-b1 (make-block 'batch (string->utf8 "batch1") (vector)))
(define batch-b2 (make-block 'batch (string->utf8 "batch2") (vector)))
(define batch-b3 (make-block 'batch (string->utf8 "batch3") (vector)))

(define batch-hashes (store-put-many! fs (list batch-b1 batch-b2 batch-b3)))
(test "store-put-many! returns list of hashes"
      3
      (length batch-hashes))

(test-pred "store-put-many! hashes are bytevectors"
           (lambda (hs) (andmap bytevector? hs))
           batch-hashes)

;; Verify all batch blocks stored
(test "store-count after batch put"
      11  ;; 8 + 3
      (store-count fs))

;; Test store-get-many
(define retrieved-batch (store-get-many fs batch-hashes))
(test "store-get-many retrieves all blocks"
      3
      (length retrieved-batch))

(test-pred "store-get-many returns actual blocks"
           (lambda (bs) (andmap block? bs))
           retrieved-batch)

;; Test store-get-many with mixed existing/non-existing
(define mixed-hashes (list (car batch-hashes) fake-hash (cadr batch-hashes)))
(define mixed-results (store-get-many fs mixed-hashes))
(test "store-get-many with mixed hashes - length"
      3
      (length mixed-results))

(test "store-get-many returns #f for non-existent"
      #f
      (cadr mixed-results))

(test-pred "store-get-many returns block for existing"
           block?
           (car mixed-results))

;; Test empty batch
(define empty-put (store-put-many! fs '()))
(test "store-put-many! with empty list"
      '()
      empty-put)

(define empty-get (store-get-many fs '()))
(test "store-get-many with empty list"
      '()
      empty-get)

(printf "\n")

;;; ============================================================
;;; Test 10: Statistics (store-stats, store-print-stats)
;;; ============================================================

(printf "Testing Statistics:\n")
(printf "----------------------------------------------------------------\n")

(define stats (store-stats fs))
(test-pred "store-stats returns an alist"
           list?
           stats)

(test "store-stats contains total"
      11
      (cdr (assq 'total stats)))

(test-pred "store-stats contains by-tag"
           list?
           (cdr (assq 'by-tag stats)))

(test-pred "store-stats contains total-bytes"
           (lambda (x) (and (integer? x) (> x 0)))
           (cdr (assq 'total-bytes stats)))

;; Check by-tag breakdown
(define by-tag (cdr (assq 'by-tag stats)))
(test "store-stats by-tag entity count"
      3
      (cdr (assq 'entity by-tag)))

(test "store-stats by-tag relation count"
      2
      (cdr (assq 'relation by-tag)))

(test "store-stats by-tag batch count"
      3
      (cdr (assq 'batch by-tag)))

;; Test store-print-stats (just ensure it runs without error)
(printf "  Testing store-print-stats output:\n")
(printf "  ----------------------------------\n")
(store-print-stats fs)
(printf "  ----------------------------------\n")
(set! tests-passed (+ tests-passed 1))
(printf "  [PASS] store-print-stats runs without error\n")

(printf "\n")

;;; ============================================================
;;; Test 11: Graph Navigation (store-get-refs, store-get-referrers)
;;; ============================================================

(printf "Testing Graph Navigation:\n")
(printf "----------------------------------------------------------------\n")

;; Get refs from relation block
(define relation-hash (hash-block relation-alice-knows-bob))
(define relation-block-retrieved (store-get fs relation-hash))
(define refs-from-relation (store-get-refs fs relation-block-retrieved))
(test "store-get-refs from relation gets 2 refs"
      2
      (length refs-from-relation))

(test-pred "store-get-refs returns entity blocks"
           (lambda (bs) (andmap (lambda (b) (eq? (block-tag b) 'entity)) bs))
           refs-from-relation)

;; Get refs from collection block
(define collection-hash (hash-block collection-people))
(define collection-retrieved (store-get fs collection-hash))
(define refs-from-collection (store-get-refs fs collection-retrieved))
(test "store-get-refs from collection gets 3 refs"
      3
      (length refs-from-collection))

;; Get refs from entity (no refs)
(define refs-from-entity (store-get-refs fs entity-alice))
(test "store-get-refs from entity with no refs"
      0
      (length refs-from-entity))

;; Test store-get-referrers (alias for store-find-by-ref)
(define referrers-alice (store-get-referrers fs hash-alice))
(test "store-get-referrers for Alice"
      2
      (length referrers-alice))

(define referrers-none (store-get-referrers fs fake-hash))
(test "store-get-referrers for non-existent"
      0
      (length referrers-none))

(printf "\n")

;;; ============================================================
;;; Test 12: Knowledge Graph Helpers
;;; ============================================================

(printf "Testing Knowledge Graph Helpers:\n")
(printf "----------------------------------------------------------------\n")

(define all-entities (store-get-entities fs))
(test "store-get-entities count"
      3
      (length all-entities))

(test-pred "store-get-entities returns only entities"
           (lambda (bs) (andmap (lambda (b) (eq? (block-tag b) 'entity)) bs))
           all-entities)

(define all-relations (store-get-relations fs))
(test "store-get-relations count"
      2
      (length all-relations))

(test-pred "store-get-relations returns only relations"
           (lambda (bs) (andmap (lambda (b) (eq? (block-tag b) 'relation)) bs))
           all-relations)

(define all-collections (store-get-collections fs))
(test "store-get-collections count"
      1
      (length all-collections))

(test-pred "store-get-collections returns only collections"
           (lambda (bs) (andmap (lambda (b) (eq? (block-tag b) 'collection)) bs))
           all-collections)

(printf "\n")

;;; ============================================================
;;; Test 13: Edge Cases
;;; ============================================================

(printf "Testing Edge Cases:\n")
(printf "----------------------------------------------------------------\n")

;; Empty store test - create a fresh store
(define empty-fs (mint-fs-capability ".test-store-empty"))
(when (file-exists? ".test-store-empty")
  (system "rm -rf .test-store-empty"))

(test "store-count on empty store"
      0
      (store-count empty-fs))

(test "store-all-hashes on empty store"
      '()
      (store-all-hashes empty-fs))

(test "store-all-blocks on empty store"
      '()
      (store-all-blocks empty-fs))

(test "store-get-entities on empty store"
      '()
      (store-get-entities empty-fs))

(test "store-stats on empty store - total"
      0
      (cdr (assq 'total (store-stats empty-fs))))

(test "store-stats on empty store - total-bytes"
      0
      (cdr (assq 'total-bytes (store-stats empty-fs))))

;; Duplicate puts should be idempotent
(define dup-hash1 (store-put! fs entity-alice))
(define dup-hash2 (store-put! fs entity-alice))
(test "duplicate store-put! returns same hash"
      dup-hash1
      dup-hash2)

(test "duplicate store-put! doesn't increase count"
      11  ;; Still 11
      (store-count fs))

;; Block with empty payload
(define empty-payload-block (make-block 'empty (string->utf8 "") (vector)))
(define empty-payload-hash (store-put! fs empty-payload-block))
(test-true "can store block with empty payload"
           (store-exists? fs empty-payload-hash))

(define retrieved-empty (store-get fs empty-payload-hash))
(test "empty payload block retrieved correctly"
      ""
      (utf8->string (block-payload retrieved-empty)))

;; Block with long payload
(define long-payload (make-string 10000 #\x))
(define long-block (make-block 'long (string->utf8 long-payload) (vector)))
(define long-hash (store-put! fs long-block))
(test-true "can store block with long payload"
           (store-exists? fs long-hash))

(define retrieved-long (store-get fs long-hash))
(test "long payload preserved"
      10000
      (string-length (utf8->string (block-payload retrieved-long))))

;; Block with many refs
(define many-refs-block
  (make-block 'many-refs
              (string->utf8 "many")
              (vector hash-alice hash-bob hash-charlie
                      hash-alice hash-bob hash-charlie
                      hash-alice hash-bob hash-charlie
                      hash-alice)))
(define many-refs-hash (store-put! fs many-refs-block))
(define retrieved-many-refs (store-get fs many-refs-hash))
(test "block with many refs preserved"
      10
      (vector-length (block-refs retrieved-many-refs)))

(printf "\n")

;;; ============================================================
;;; Test 14: string-contains? helper (internal function)
;;; ============================================================

(printf "Testing string-contains? helper:\n")
(printf "----------------------------------------------------------------\n")

(test-true "string-contains? finds at start"
           (string-contains? "hello world" "hello"))

(test-true "string-contains? finds at end"
           (string-contains? "hello world" "world"))

(test-true "string-contains? finds in middle"
           (string-contains? "hello world" "lo wo"))

(test-true "string-contains? finds exact match"
           (string-contains? "hello" "hello"))

(test-false "string-contains? returns #f when not found"
            (string-contains? "hello world" "xyz"))

(test-false "string-contains? handles needle longer than haystack"
            (string-contains? "hi" "hello"))

(test-true "string-contains? handles empty needle"
           (string-contains? "hello" ""))

(printf "\n")

;;; ============================================================
;;; Cleanup
;;; ============================================================

(printf "Cleaning up test stores...\n")
(system (format "rm -rf ~a" test-store-path))
(system "rm -rf .test-store-empty")
(printf "Cleanup complete.\n\n")

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "================================================================\n")
(printf "                    TEST RESULTS                               \n")
(printf "================================================================\n\n")

(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n[SUCCESS] All tests passed! Store API is working correctly.\n\n")
    (printf "\n[FAILURE] Some tests failed. Please review.\n\n"))
