;;; test-query-dsl.ss --- Comprehensive test suite for Query DSL

(source-directories (cons "core" (source-directories)))
(load "core/block.ss")
(load "core/sha256.ss")
(load "shell/fs.ss")
(load "shell/store-api.ss")
(load "core/query-dsl.ss")

(printf "\n")
(printf "================================================================\n")
(printf "              QUERY DSL TEST SUITE                             \n")
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
(define test-store-path ".test-query-dsl")

;; Clean up any previous test store
(when (file-exists? test-store-path)
      (system (format "rm -rf ~a" test-store-path)))

(define fs (mint-fs-capability test-store-path))

;; Create test entities with various names
(define entity-alice (make-block 'entity (string->utf8 "name:Alice,role:scientist") (vector)))
(define entity-bob (make-block 'entity (string->utf8 "name:Bob,role:engineer") (vector)))
(define entity-charlie (make-block 'entity (string->utf8 "name:Charlie,role:scientist") (vector)))
(define entity-diana (make-block 'entity (string->utf8 "name:Diana,role:manager") (vector)))

;; Entity hashes for references
(define hash-alice (hash-block entity-alice))
(define hash-bob (hash-block entity-bob))
(define hash-charlie (hash-block entity-charlie))
(define hash-diana (hash-block entity-diana))

;; Relations that reference entities
(define relation-alice-knows-bob
  (make-block 'relation (string->utf8 "knows")
              (vector hash-alice hash-bob)))

(define relation-bob-likes-charlie
  (make-block 'relation (string->utf8 "likes")
              (vector hash-bob hash-charlie)))

(define relation-charlie-reports-diana
  (make-block 'relation (string->utf8 "reports-to")
              (vector hash-charlie hash-diana)))

(define relation-alice-mentors-charlie
  (make-block 'relation (string->utf8 "mentors")
              (vector hash-alice hash-charlie)))

;; Collection blocks
(define collection-scientists
  (make-block 'collection (string->utf8 "scientists")
              (vector hash-alice hash-charlie)))

(define collection-all-people
  (make-block 'collection (string->utf8 "all-people")
              (vector hash-alice hash-bob hash-charlie hash-diana)))

;; Event blocks
(define event-meeting (make-block 'event (string->utf8 "meeting:2024-01-15,topic:planning") (vector)))
(define event-conference (make-block 'event (string->utf8 "conference:2024-03-20,topic:science") (vector)))

;; Attribute blocks
(define attr-age-alice (make-block 'attribute (string->utf8 "age:35") (vector hash-alice)))
(define attr-age-bob (make-block 'attribute (string->utf8 "age:28") (vector hash-bob)))

;; Block with no references
(define note-orphan (make-block 'note (string->utf8 "This is an orphan note with no references") (vector)))

;; Block with empty payload
(define marker-empty (make-block 'marker (string->utf8 "") (vector)))

;; Block with long payload
(define long-payload (make-string 500 #\x))
(define document-long (make-block 'document (string->utf8 long-payload) (vector)))

;; Store all blocks
(store-put! fs entity-alice)
(store-put! fs entity-bob)
(store-put! fs entity-charlie)
(store-put! fs entity-diana)
(store-put! fs relation-alice-knows-bob)
(store-put! fs relation-bob-likes-charlie)
(store-put! fs relation-charlie-reports-diana)
(store-put! fs relation-alice-mentors-charlie)
(store-put! fs collection-scientists)
(store-put! fs collection-all-people)
(store-put! fs event-meeting)
(store-put! fs event-conference)
(store-put! fs attr-age-alice)
(store-put! fs attr-age-bob)
(store-put! fs note-orphan)
(store-put! fs marker-empty)
(store-put! fs document-long)

(printf "Test blocks created and stored: ~a blocks\n\n" (store-count fs))

;;; ============================================================
;;; Test 1: Basic Tag Queries
;;; ============================================================

(printf "Testing Basic Tag Queries:\n")
(printf "----------------------------------------------------------------\n")

;; Test tag query using bare pattern (shorthand)
(define entity-results (query fs '(tag . entity)))
(test "query (tag . entity) finds all entities"
      4
      (length entity-results))

(test-pred "query (tag . entity) returns entity blocks"
           (lambda (bs) (andmap (lambda (b) (eq? (block-tag b) 'entity)) bs))
           entity-results)

;; Test tag query using match syntax
(define entity-results-match (query fs '(match (tag . entity))))
(test "query (match (tag . entity)) finds all entities"
      4
      (length entity-results-match))

;; Test different tags
(define relation-results (query fs '(tag . relation)))
(test "query (tag . relation) finds all relations"
      4
      (length relation-results))

(define collection-results (query fs '(tag . collection)))
(test "query (tag . collection) finds all collections"
      2
      (length collection-results))

(define event-results (query fs '(tag . event)))
(test "query (tag . event) finds all events"
      2
      (length event-results))

(define attribute-results (query fs '(tag . attribute)))
(test "query (tag . attribute) finds all attributes"
      2
      (length attribute-results))

;; Test tag query with no matches
(define no-tag-results (query fs '(tag . nonexistent-tag)))
(test "query with non-existent tag returns empty"
      0
      (length no-tag-results))

(printf "\n")

;;; ============================================================
;;; Test 2: Content Search Queries
;;; ============================================================

(printf "Testing Content Search Queries:\n")
(printf "----------------------------------------------------------------\n")

;; Test payload-contains
(define alice-results (query fs '(payload-contains . "Alice")))
(test "query (payload-contains . 'Alice') finds Alice blocks"
      1
      (length alice-results))

(define name-results (query fs '(payload-contains . "name:")))
(test "query (payload-contains . 'name:') finds all entities"
      4
      (length name-results))

(define scientist-results (query fs '(payload-contains . "scientist")))
(test "query (payload-contains . 'scientist') finds scientist blocks"
      3  ;; scientists collection + Alice + Charlie
      (length scientist-results))

;; Test payload-contains using match syntax
(define meeting-results (query fs '(match (payload-contains . "meeting"))))
(test "query (match (payload-contains . 'meeting')) finds meeting event"
      1
      (length meeting-results))

;; Test partial substring match
(define role-results (query fs '(payload-contains . "role:")))
(test "query (payload-contains . 'role:') finds role entries"
      4
      (length role-results))

;; Test no matches
(define no-content-results (query fs '(payload-contains . "zzzznotfound")))
(test "query with non-matching content returns empty"
      0
      (length no-content-results))

(printf "\n")

;;; ============================================================
;;; Test 3: Compound AND Queries
;;; ============================================================

(printf "Testing Compound AND Queries:\n")
(printf "----------------------------------------------------------------\n")

;; AND: tag AND payload-contains
(define entity-scientist-results
  (query fs '(and (tag . entity) (payload-contains . "scientist"))))
(test "query (and (tag . entity) (payload-contains . 'scientist')) finds scientists"
      2  ;; Alice and Charlie
      (length entity-scientist-results))

;; AND: multiple tag conditions (should match nothing)
(define multi-tag-results
  (query fs '(and (tag . entity) (tag . relation))))
(test "query (and (tag . entity) (tag . relation)) returns empty"
      0
      (length multi-tag-results))

;; AND: multiple content conditions
(define multi-content-results
  (query fs '(and (payload-contains . "name:") (payload-contains . "Alice"))))
(test "query (and content AND content) narrows results"
      1
      (length multi-content-results))

;; AND with three conditions
(define triple-and-results
  (query fs '(and (tag . entity)
              (payload-contains . "name:")
              (payload-contains . "scientist"))))
(test "query with triple AND finds matching blocks"
      2  ;; Alice and Charlie
      (length triple-and-results))

;; AND: event AND topic
(define event-science-results
  (query fs '(and (tag . event) (payload-contains . "science"))))
(test "query (and (tag . event) (payload-contains . 'science')) finds science events"
      1
      (length event-science-results))

(printf "\n")

;;; ============================================================
;;; Test 4: Compound OR Queries
;;; ============================================================

(printf "Testing Compound OR Queries:\n")
(printf "----------------------------------------------------------------\n")

;; OR: tag OR tag
(define entity-or-relation-results
  (query fs '(or (tag . entity) (tag . relation))))
(test "query (or (tag . entity) (tag . relation)) finds entities and relations"
      8  ;; 4 entities + 4 relations
      (length entity-or-relation-results))

;; OR: content OR content
(define alice-or-bob-results
  (query fs '(or (payload-contains . "Alice") (payload-contains . "Bob"))))
(test "query (or content OR content) finds multiple matches"
      2  ;; Alice and Bob entities
      (length alice-or-bob-results))

;; OR with three conditions
(define triple-or-results
  (query fs '(or (tag . entity) (tag . event) (tag . collection))))
(test "query with triple OR finds all matching tags"
      8  ;; 4 entities + 2 events + 2 collections
      (length triple-or-results))

;; OR where only one matches
(define one-match-or-results
  (query fs '(or (payload-contains . "Diana") (payload-contains . "zzzznotfound"))))
(test "query (or match OR no-match) still finds matching block"
      1
      (length one-match-or-results))

;; OR where none match
(define no-match-or-results
  (query fs '(or (payload-contains . "zzz1") (payload-contains . "zzz2"))))
(test "query (or no-match OR no-match) returns empty"
      0
      (length no-match-or-results))

(printf "\n")

;;; ============================================================
;;; Test 5: Compound NOT Queries
;;; ============================================================

(printf "Testing Compound NOT Queries:\n")
(printf "----------------------------------------------------------------\n")

;; NOT: exclude tag
(define not-entity-results (query fs '(not (tag . entity))))
(test "query (not (tag . entity)) excludes entities"
      13  ;; 17 total - 4 entities = 13
      (length not-entity-results))

(test-pred "query (not (tag . entity)) contains no entities"
           (lambda (bs) (not (ormap (lambda (b) (eq? (block-tag b) 'entity)) bs)))
           not-entity-results)

;; NOT: exclude content
(define not-alice-results (query fs '(not (payload-contains . "Alice"))))
(test "query (not (payload-contains . 'Alice')) excludes Alice"
      16  ;; 17 total - 1 Alice
      (length not-alice-results))

;; Combined: AND with NOT
(define entity-not-scientist-results
  (query fs '(and (tag . entity) (not (payload-contains . "scientist")))))
(test "query (and entity (not scientist)) finds non-scientist entities"
      2  ;; Bob and Diana
      (length entity-not-scientist-results))

;; NOT of a compound query
(define not-entity-or-relation-results
  (query fs '(not (or (tag . entity) (tag . relation)))))
(test "query (not (or entity relation)) excludes both"
      9  ;; 17 - 4 entities - 4 relations = 9
      (length not-entity-or-relation-results))

(printf "\n")

;;; ============================================================
;;; Test 6: Reference Queries - refs-to
;;; ============================================================

(printf "Testing Reference Queries (refs-to):\n")
(printf "----------------------------------------------------------------\n")

;; Find blocks that reference Alice
(define refs-to-alice (query fs `(refs-to ,hash-alice)))
(test "query (refs-to alice) finds blocks referencing Alice"
      5  ;; relation-alice-knows-bob, relation-alice-mentors-charlie, collection-scientists, collection-all-people, attr-age-alice
      (length refs-to-alice))

;; Find blocks that reference Bob
(define refs-to-bob (query fs `(refs-to ,hash-bob)))
(test "query (refs-to bob) finds blocks referencing Bob"
      4  ;; relation-alice-knows-bob, relation-bob-likes-charlie, collection-all-people, attr-age-bob
      (length refs-to-bob))

;; Find blocks that reference Charlie
(define refs-to-charlie (query fs `(refs-to ,hash-charlie)))
(test "query (refs-to charlie) finds blocks referencing Charlie"
      5  ;; relation-bob-likes-charlie, relation-charlie-reports-diana, relation-alice-mentors-charlie, collection-scientists, collection-all-people
      (length refs-to-charlie))

;; refs-to for a non-existent hash
(define fake-hash (make-bytevector 32 0))
(define refs-to-fake (query fs `(refs-to ,fake-hash)))
(test "query (refs-to fake-hash) returns empty"
      0
      (length refs-to-fake))

(printf "\n")

;;; ============================================================
;;; Test 7: Reference Queries - refs-from
;;; ============================================================

(printf "Testing Reference Queries (refs-from):\n")
(printf "----------------------------------------------------------------\n")

;; Find blocks referenced BY collection-all-people
(define collection-hash (hash-block collection-all-people))
(define refs-from-collection (query fs `(refs-from ,collection-hash)))
(test "query (refs-from collection-all-people) finds referenced blocks"
      4  ;; alice, bob, charlie, diana
      (length refs-from-collection))

;; Find blocks referenced BY a relation
(define relation-hash (hash-block relation-alice-knows-bob))
(define refs-from-relation (query fs `(refs-from ,relation-hash)))
(test "query (refs-from relation) finds referenced blocks"
      2  ;; alice, bob
      (length refs-from-relation))

;; refs-from for an entity (no outgoing refs)
(define refs-from-entity (query fs `(refs-from ,hash-alice)))
(test "query (refs-from entity) returns empty (no outgoing refs)"
      0
      (length refs-from-entity))

;; refs-from for non-existent hash
(define refs-from-fake (query fs `(refs-from ,fake-hash)))
(test "query (refs-from fake-hash) returns empty"
      0
      (length refs-from-fake))

(printf "\n")

;;; ============================================================
;;; Test 8: Transitive Reference Traversal
;;; ============================================================

(printf "Testing Transitive Reference Traversal:\n")
(printf "----------------------------------------------------------------\n")

;; Start from relation-alice-knows-bob and traverse
;; With depth=1, only start block is collected (depth >= max-depth stops before expanding)
(define transitive-from-relation (refs-transitive fs relation-hash 1))
(test "refs-transitive depth 1 from relation finds start block only"
      1  ;; just the start block at depth 0
      (length transitive-from-relation))

;; Transitive from collection-scientists
(define scientists-hash (hash-block collection-scientists))
(define transitive-from-scientists (refs-transitive fs scientists-hash 2))
(test-pred "refs-transitive depth 2 from collection finds multiple blocks"
           (lambda (bs) (>= (length bs) 2))
           transitive-from-scientists)

;; Transitive from entity with no refs (depth doesn't matter)
(define transitive-from-entity (refs-transitive fs hash-alice 5))
(test "refs-transitive from entity with no refs"
      1  ;; just the entity itself
      (length transitive-from-entity))

;; Transitive with depth 0
(define transitive-depth-0 (refs-transitive fs relation-hash 0))
(test "refs-transitive depth 0 returns empty"
      0
      (length transitive-depth-0))

(printf "\n")

;;; ============================================================
;;; Test 9: Projection Queries (select)
;;; ============================================================

(printf "Testing Projection Queries:\n")
(printf "----------------------------------------------------------------\n")

;; Select single field
(define select-tags (query fs '(select (tag) (where (tag . entity)))))
(test "select (tag) from entities returns projections"
      4
      (length select-tags))

(test-pred "select (tag) returns alists with tag field"
           (lambda (results)
                   (and (pair? results)
                        (assq 'tag (car results))))
           select-tags)

;; Select multiple fields
(define select-multiple
  (query fs '(select (tag payload-size) (where (tag . entity)))))
(test "select (tag payload-size) returns multiple fields"
      4
      (length select-multiple))

(test-pred "select results have both fields"
           (lambda (results)
                   (and (pair? results)
                        (let ([first-result (car results)])
                             (and (assq 'tag first-result)
                                  (assq 'payload-size first-result)))))
           select-multiple)

;; Select with refs-count
(define select-refs-count
  (query fs '(select (tag refs-count) (where (tag . relation)))))
(test "select (tag refs-count) from relations"
      4
      (length select-refs-count))

(test-pred "relations have 2 refs each"
           (lambda (results)
                   (andmap (lambda (r)
                                   (= (cdr (assq 'refs-count r)) 2))
                           results))
           select-refs-count)

;; Select payload-str field
(define select-payload-str
  (query fs '(select (tag payload-str) (where (payload-contains . "Alice")))))
(test "select (tag payload-str) returns payload as string"
      1
      (length select-payload-str))

(test-pred "payload-str contains Alice"
           (lambda (results)
                   (and (pair? results)
                        (let ([payload (cdr (assq 'payload-str (car results)))])
                             (query-string-contains? payload "Alice"))))
           select-payload-str)

(printf "\n")

;;; ============================================================
;;; Test 10: Aggregation - query-count
;;; ============================================================

(printf "Testing Aggregation - query-count:\n")
(printf "----------------------------------------------------------------\n")

;; Count entities
(test "query-count entities"
      4
      (query-count fs '(tag . entity)))

;; Count relations
(test "query-count relations"
      4
      (query-count fs '(tag . relation)))

;; Count compound query
(test "query-count (and entity scientist)"
      2
      (query-count fs '(and (tag . entity) (payload-contains . "scientist"))))

;; Count with no matches
(test "query-count no matches"
      0
      (query-count fs '(tag . nonexistent)))

;; Count all blocks
(test "query-count all blocks (empty query)"
      17
      (query-count fs '()))

;; Count using nested count syntax
(define count-via-query (query fs '(count (tag . entity))))
(test "query (count (tag . entity)) returns count"
      4
      count-via-query)

(printf "\n")

;;; ============================================================
;;; Test 11: Aggregation - query-group-by
;;; ============================================================

(printf "Testing Aggregation - query-group-by:\n")
(printf "----------------------------------------------------------------\n")

;; Group all blocks by tag
(define grouped-by-tag (query-group-by fs 'tag '()))
(test-pred "query-group-by tag returns alist"
           list?
           grouped-by-tag)

(test-pred "group-by tag has entity group"
           (lambda (groups) (assq 'entity groups))
           grouped-by-tag)

(test "entity group has 4 blocks"
      4
      (length (cdr (assq 'entity grouped-by-tag))))

(test "relation group has 4 blocks"
      4
      (length (cdr (assq 'relation grouped-by-tag))))

;; Group with filter
(define grouped-entities-by-tag
  (query-group-by fs 'tag '(or (tag . entity) (tag . relation))))
(test "group-by with filter"
      2  ;; only entity and relation groups
      (length grouped-entities-by-tag))

;; Group by refs-count
(define grouped-by-refs
  (query-group-by fs 'refs-count '()))
(test-pred "group-by refs-count returns groups"
           list?
           grouped-by-refs)

;; group-by via query syntax
(define group-via-query
  (query fs '(group-by tag (tag . entity))))
(test-pred "query (group-by tag ...) returns grouping"
           list?
           group-via-query)

(printf "\n")

;;; ============================================================
;;; Test 12: has-refs and refs-count Predicates
;;; ============================================================

(printf "Testing has-refs and refs-count Predicates:\n")
(printf "----------------------------------------------------------------\n")

;; Find blocks with any references
(define has-refs-results (query fs '(match (has-refs . #t))))
(test "query (has-refs) finds blocks with refs"
      8  ;; 4 relations + 2 collections + 2 attributes
      (length has-refs-results))

(test-pred "has-refs results all have refs"
           (lambda (bs)
                   (andmap (lambda (b) (> (vector-length (block-refs b)) 0)) bs))
           has-refs-results)

;; Find blocks with exactly 2 refs
(define refs-2-results (query fs '(match (refs-count . 2))))
(test "query (refs-count . 2) finds blocks with exactly 2 refs"
      5  ;; 4 relations (2 each) + collection-scientists (2)
      (length refs-2-results))

;; Find blocks with 0 refs
(define refs-0-results (query fs '(match (refs-count . 0))))
(test "query (refs-count . 0) finds blocks with no refs"
      9  ;; 4 entities + 2 events + note + marker + document
      (length refs-0-results))

;; Find blocks with 4 refs
(define refs-4-results (query fs '(match (refs-count . 4))))
(test "query (refs-count . 4) finds collection-all-people"
      1
      (length refs-4-results))

;; Combined: relations with exactly 2 refs
(define relations-with-2-refs
  (query fs '(and (tag . relation) (match (refs-count . 2)))))
(test "query relations with exactly 2 refs"
      4
      (length relations-with-2-refs))

(printf "\n")

;;; ============================================================
;;; Test 13: Payload Size Predicates
;;; ============================================================

(printf "Testing Payload Size Predicates:\n")
(printf "----------------------------------------------------------------\n")

;; Find blocks with payload > 100 bytes
(define large-payload-results
  (query fs '(match (payload-size-gt . 100))))
(test-pred "query (payload-size-gt . 100) finds large payloads"
           (lambda (bs)
                   (andmap (lambda (b) (> (bytevector-length (block-payload b)) 100)) bs))
           large-payload-results)

;; Find blocks with payload < 10 bytes
(define small-payload-results
  (query fs '(match (payload-size-lt . 10))))
(test-pred "query (payload-size-lt . 10) finds small payloads"
           (lambda (bs)
                   (andmap (lambda (b) (< (bytevector-length (block-payload b)) 10)) bs))
           small-payload-results)

;; Find empty payload blocks
(define empty-payload-results
  (query fs '(match (payload-size-eq . 0))))
(test "query (payload-size-eq . 0) finds empty payloads"
      1  ;; marker-empty
      (length empty-payload-results))

;; Combined: small entities
(define small-entity-results
  (query fs '(and (tag . entity) (match (payload-size-lt . 30)))))
(test-pred "query entities with small payloads"
           list?
           small-entity-results)

(printf "\n")

;;; ============================================================
;;; Test 14: Convenience Functions
;;; ============================================================

(printf "Testing Convenience Functions:\n")
(printf "----------------------------------------------------------------\n")

;; find-entities
(define found-entities (find-entities fs))
(test "find-entities returns all entities"
      4
      (length found-entities))

;; find-relations
(define found-relations (find-relations fs))
(test "find-relations returns all relations"
      4
      (length found-relations))

;; find-collections
(define found-collections (find-collections fs))
(test "find-collections returns all collections"
      2
      (length found-collections))

;; find-by-content
(define found-by-content (find-by-content fs "scientist"))
(test "find-by-content 'scientist'"
      3
      (length found-by-content))

;; find-with-refs
(define found-with-refs (find-with-refs fs))
(test "find-with-refs finds blocks with references"
      8
      (length found-with-refs))

;; find-orphans (blocks not referenced by anyone)
(define found-orphans (find-orphans fs))
(test-pred "find-orphans returns list"
           list?
           found-orphans)

;; Orphans should include: events, note, marker, document, collections
;; (things that aren't referenced by other blocks)
(test-pred "find-orphans includes unreferenced blocks"
           (lambda (orphans)
                   (>= (length orphans) 1))
           found-orphans)

(printf "\n")

;;; ============================================================
;;; Test 15: Query Builders
;;; ============================================================

(printf "Testing Query Builders:\n")
(printf "----------------------------------------------------------------\n")

;; make-tag-query
(define built-tag-query (make-tag-query 'entity))
(test "make-tag-query creates correct query"
      '(tag . entity)
      built-tag-query)

(define built-tag-results (query fs built-tag-query))
(test "query with built tag query works"
      4
      (length built-tag-results))

;; make-content-query
(define built-content-query (make-content-query "Alice"))
(test "make-content-query creates correct query"
      '(payload-contains . "Alice")
      built-content-query)

;; make-and-query
(define built-and-query
  (make-and-query (list (make-tag-query 'entity)
                        (make-content-query "scientist"))))
(test-pred "make-and-query creates and query"
           (lambda (q) (eq? (car q) 'and))
           built-and-query)

(define built-and-results (query fs built-and-query))
(test "query with built AND query works"
      2
      (length built-and-results))

;; make-or-query
(define built-or-query
  (make-or-query (list (make-tag-query 'entity)
                       (make-tag-query 'event))))
(test-pred "make-or-query creates or query"
           (lambda (q) (eq? (car q) 'or))
           built-or-query)

(define built-or-results (query fs built-or-query))
(test "query with built OR query works"
      6  ;; 4 entities + 2 events
      (length built-or-results))

;; make-not-query
(define built-not-query (make-not-query (make-tag-query 'entity)))
(test-pred "make-not-query creates not query"
           (lambda (q) (eq? (car q) 'not))
           built-not-query)

;; make-select-query
(define built-select-query
  (make-select-query '(tag payload-size) (make-tag-query 'entity)))
(test-pred "make-select-query creates select query"
           (lambda (q) (eq? (car q) 'select))
           built-select-query)

(printf "\n")

;;; ============================================================
;;; Test 16: Empty Query (returns all blocks)
;;; ============================================================

(printf "Testing Empty Query:\n")
(printf "----------------------------------------------------------------\n")

(define all-blocks-query (query fs '()))
(test "empty query returns all blocks"
      17
      (length all-blocks-query))

(test "empty query count equals store count"
      (store-count fs)
      (length all-blocks-query))

(printf "\n")

;;; ============================================================
;;; Test 17: Edge Cases
;;; ============================================================

(printf "Testing Edge Cases:\n")
(printf "----------------------------------------------------------------\n")

;; Query on empty store
(define empty-store-path ".test-query-dsl-empty")
(when (file-exists? empty-store-path)
      (system (format "rm -rf ~a" empty-store-path)))
(define empty-fs (mint-fs-capability empty-store-path))

(define empty-entity-results (query empty-fs '(tag . entity)))
(test "query on empty store returns empty"
      0
      (length empty-entity-results))

(define empty-count (query-count empty-fs '(tag . entity)))
(test "query-count on empty store returns 0"
      0
      empty-count)

(define empty-group (query-group-by empty-fs 'tag '()))
(test "query-group-by on empty store returns empty"
      '()
      empty-group)

;; Nested compound queries
(define complex-nested
  (query fs '(and (or (tag . entity) (tag . relation))
              (not (payload-contains . "Diana")))))
(test-pred "complex nested query returns results"
           (lambda (bs)
                   (andmap (lambda (b)
                                   (and (or (eq? (block-tag b) 'entity)
                                            (eq? (block-tag b) 'relation))
                                        (not (query-string-contains?
                                              (utf8->string (block-payload b)) "Diana"))))
                           bs))
           complex-nested)

;; Query with special characters in content (ensure no crashes)
(define special-char-results
  (query fs '(payload-contains . ":")))
(test-pred "query with special characters works"
           list?
           special-char-results)

;; Deep AND nesting
(define deep-and-results
  (query fs '(and (and (tag . entity) (payload-contains . "name:"))
              (and (payload-contains . "scientist") (payload-contains . "Alice")))))
(test "deeply nested AND works"
      1  ;; Only Alice the scientist
      (length deep-and-results))

;; Deep OR nesting
(define deep-or-results
  (query fs '(or (or (tag . marker) (tag . document))
              (or (tag . note) (tag . attribute)))))
(test "deeply nested OR works"
      5  ;; 1 marker + 1 document + 1 note + 2 attributes
      (length deep-or-results))

(printf "\n")

;;; ============================================================
;;; Test 18: Predicate Builders (Internal Testing)
;;; ============================================================

(printf "Testing Predicate Builders:\n")
(printf "----------------------------------------------------------------\n")

;; build-tag-predicate
(define tag-pred (build-tag-predicate 'entity))
(test-true "build-tag-predicate matches entity"
           (tag-pred entity-alice))
(test-false "build-tag-predicate rejects non-entity"
            (tag-pred relation-alice-knows-bob))

;; build-payload-contains-predicate
(define contains-pred (build-payload-contains-predicate "Alice"))
(test-true "build-payload-contains-predicate matches Alice"
           (contains-pred entity-alice))
(test-false "build-payload-contains-predicate rejects Bob"
            (contains-pred entity-bob))

;; build-has-refs-predicate
(define has-refs-pred (build-has-refs-predicate))
(test-true "build-has-refs-predicate matches relation"
           (has-refs-pred relation-alice-knows-bob))
(test-false "build-has-refs-predicate rejects entity"
            (has-refs-pred entity-alice))

;; build-refs-count-predicate
(define refs-2-pred (build-refs-count-predicate 2))
(test-true "build-refs-count-predicate matches relation with 2 refs"
           (refs-2-pred relation-alice-knows-bob))
(test-false "build-refs-count-predicate rejects entity with 0 refs"
            (refs-2-pred entity-alice))

;; build-refs-to-predicate
(define refs-to-alice-pred (build-refs-to-predicate hash-alice))
(test-true "build-refs-to-predicate matches block referencing Alice"
           (refs-to-alice-pred relation-alice-knows-bob))
(test-false "build-refs-to-predicate rejects block not referencing Alice"
            (refs-to-alice-pred relation-bob-likes-charlie))

;; build-payload-size-predicate
(define size-gt-100-pred (build-payload-size-predicate > 100))
(test-true "build-payload-size-predicate matches large payload"
           (size-gt-100-pred document-long))
(test-false "build-payload-size-predicate rejects small payload"
            (size-gt-100-pred entity-alice))

(printf "\n")

;;; ============================================================
;;; Test 19: Field Extraction
;;; ============================================================

(printf "Testing Field Extraction:\n")
(printf "----------------------------------------------------------------\n")

;; extract-field for tag
(test "extract-field tag"
      'entity
      (extract-field entity-alice 'tag))

;; extract-field for payload
(test-pred "extract-field payload returns bytevector"
           bytevector?
           (extract-field entity-alice 'payload))

;; extract-field for payload-str
(test-pred "extract-field payload-str contains Alice"
           (lambda (s) (query-string-contains? s "Alice"))
           (extract-field entity-alice 'payload-str))

;; extract-field for payload-size
(test-pred "extract-field payload-size returns integer"
           integer?
           (extract-field entity-alice 'payload-size))

;; extract-field for refs
(test-pred "extract-field refs returns vector"
           vector?
           (extract-field entity-alice 'refs))

;; extract-field for refs-count
(test "extract-field refs-count for entity"
      0
      (extract-field entity-alice 'refs-count))

(test "extract-field refs-count for relation"
      2
      (extract-field relation-alice-knows-bob 'refs-count))

;; extract-fields for multiple fields
(define extracted (extract-fields entity-alice '(tag payload-size refs-count)))
(test-pred "extract-fields returns alist"
           list?
           extracted)
(test "extract-fields has correct tag"
      'entity
      (cdr (assq 'tag extracted)))

(printf "\n")

;;; ============================================================
;;; Test 20: Query String Contains Helper
;;; ============================================================

(printf "Testing query-string-contains? Helper:\n")
(printf "----------------------------------------------------------------\n")

(test-true "query-string-contains? finds at start"
           (query-string-contains? "hello world" "hello"))

(test-true "query-string-contains? finds at end"
           (query-string-contains? "hello world" "world"))

(test-true "query-string-contains? finds in middle"
           (query-string-contains? "hello world" "lo wo"))

(test-true "query-string-contains? finds exact match"
           (query-string-contains? "hello" "hello"))

(test-false "query-string-contains? returns #f when not found"
            (query-string-contains? "hello world" "xyz"))

(test-false "query-string-contains? handles needle longer than haystack"
            (query-string-contains? "hi" "hello"))

(test-true "query-string-contains? handles empty needle"
           (query-string-contains? "hello" ""))

(printf "\n")

;;; ============================================================
;;; Cleanup
;;; ============================================================

(printf "Cleaning up test stores...\n")
(system (format "rm -rf ~a" test-store-path))
(system (format "rm -rf ~a" empty-store-path))
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
    (printf "\n[SUCCESS] All tests passed! Query DSL is working correctly.\n\n")
    (printf "\n[FAILURE] Some tests failed. Please review.\n\n"))
