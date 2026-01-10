;;; test-collection-utils.ss — Test suite for collection utilities

(source-directories (cons "core" (source-directories)))
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "shell/fs.ss")
(load "core/data/collection-utils.ss")

(printf "\n╔════════════════════════════════════════════════════════════╗\n")
(printf "║       COLLECTION UTILITIES TEST SUITE                      ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  ✗ ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

;;; Setup: Create test blocks and store
(define fs (mint-fs-capability ".store"))

(define b1 (make-block 'entity (string->utf8 "entity:Alice") (vector)))
(define b2 (make-block 'entity (string->utf8 "entity:Bob") (vector)))
(define b3 (make-block 'entity (string->utf8 "entity:Charlie") (vector)))
(define b4 (make-block 'relation (string->utf8 "knows") (vector)))
(define b5 (make-block 'relation (string->utf8 "likes") (vector)))

(fs-store! fs b1)
(fs-store! fs b2)
(fs-store! fs b3)
(fs-store! fs b4)
(fs-store! fs b5)

(define test-coll (make-block 'collection
                              (string->utf8 "test collection")
                              (vector (hash-block b1)
                                      (hash-block b2)
                                      (hash-block b3))))

(define mixed-coll (make-block 'collection
                               (string->utf8 "mixed collection")
                               (vector (hash-block b1)
                                       (hash-block b4)
                                       (hash-block b5))))

;;; ============================================================
;;; Test General List Utilities
;;; ============================================================

(printf "Testing General List Utilities:\n")
(printf "─────────────────────────────────────\n")

(test "foldr - identity with cons"
      '(1 2 3)
      (foldr cons '() '(1 2 3)))

(test "foldr - sum"
      10
      (foldr + 0 '(1 2 3 4)))

(test "foldr - product"
      24
      (foldr * 1 '(1 2 3 4)))

(test "foldr - string append"
      "abc"
      (foldr string-append "" '("a" "b" "c")))

(test "foldr - map with cons (double)"
      '(2 4 6)
      (foldr (lambda (x acc) (cons (* x 2) acc)) '() '(1 2 3)))

(test "foldr - empty list"
      42
      (foldr + 42 '()))

(test "foldr - subtraction (right-associative)"
      2
      (foldr - 0 '(1 2 3)))
      ; 1 - (2 - (3 - 0)) = 1 - (2 - 3) = 1 - (-1) = 2
      
      (printf "\n")

;;; ============================================================
;;; Test Core Collection Operations
;;; ============================================================

(printf "Testing Core Collection Operations:\n")
(printf "─────────────────────────────────────\n")

(test "collection-size"
      3
      (collection-size test-coll))

(test "collection-empty? - false"
      #f
      (collection-empty? test-coll))

(define empty-coll (make-block 'collection
                               (string->utf8 "empty")
                               (vector)))

(test "collection-empty? - true"
      #t
      (collection-empty? empty-coll))

(test "collection-hashes - count"
      3
      (length (collection-hashes test-coll)))

(printf "\n")

;;; ============================================================
;;; Test Higher-Order Functions
;;; ============================================================

(printf "Testing Higher-Order Functions:\n")
(printf "─────────────────────────────────────\n")

(test "map-collection - tags"
      '(entity entity entity)
      (map-collection fs block-tag test-coll))

(test "filter-collection - entities only"
      1
      (length (filter-collection fs
                                 (lambda (b) (eq? (block-tag b) 'entity))
                                 mixed-coll)))

(test "fold-collection - count"
      3
      (fold-collection fs
                       (lambda (acc b) (+ acc 1))
                       0
                       test-coll))

(test "foldr-collection - count"
      3
      (foldr-collection fs
                        (lambda (b acc) (+ acc 1))
                        0
                        test-coll))

(test "foldr-collection - collect tags"
      '(entity entity entity)
      (foldr-collection fs
                        (lambda (b acc) (cons (block-tag b) acc))
                        '()
                        test-coll))

(test "foldr-collection vs fold-collection - order difference"
      #t
      (let ([left-result (fold-collection fs
                                          (lambda (acc b) (cons (block-tag b) acc))
                                          '()
                                          test-coll)]
            [right-result (foldr-collection fs
                                            (lambda (b acc) (cons (block-tag b) acc))
                                            '()
                                            test-coll)])
           (equal? left-result (reverse right-result))))

(printf "\n")

;;; ============================================================
;;; Test Collection Queries
;;; ============================================================

(printf "Testing Collection Queries:\n")
(printf "─────────────────────────────────────\n")

(test "collection-find - found"
      'entity
      (let ([result (collection-find fs
                                     (lambda (b) (eq? (block-tag b) 'entity))
                                     test-coll)])
           (if result (block-tag result) #f)))

(test "collection-find - not found"
      #f
      (collection-find fs
                       (lambda (b) (eq? (block-tag b) 'nonexistent))
                       test-coll))

(test "collection-any? - true"
      #t
      (collection-any? fs
                       (lambda (b) (eq? (block-tag b) 'entity))
                       mixed-coll))

(test "collection-any? - false"
      #f
      (collection-any? fs
                       (lambda (b) (eq? (block-tag b) 'nonexistent))
                       test-coll))

(test "collection-all? - true"
      #t
      (collection-all? fs
                       (lambda (b) (eq? (block-tag b) 'entity))
                       test-coll))

(test "collection-all? - false"
      #f
      (collection-all? fs
                       (lambda (b) (eq? (block-tag b) 'entity))
                       mixed-coll))

(test "collection-count-matching"
      2
      (collection-count-matching fs
                                 (lambda (b) (eq? (block-tag b) 'relation))
                                 mixed-coll))

(printf "\n")

;;; ============================================================
;;; Test Collection Transformation
;;; ============================================================

(printf "Testing Collection Transformation:\n")
(printf "─────────────────────────────────────\n")

(let-values ([(entities relations)
              (collection-partition fs
                                    (lambda (b) (eq? (block-tag b) 'entity))
                                    mixed-coll)])
            (test "collection-partition - entities"
                  1
                  (length entities))
            (test "collection-partition - relations"
                  2
                  (length relations)))

(test "collection-group-by - by tag"
      2
      (length (collection-group-by fs block-tag mixed-coll)))

(printf "\n")

;;; ============================================================
;;; Test Collection Construction
;;; ============================================================

(printf "Testing Collection Construction:\n")
(printf "─────────────────────────────────────\n")

(test "make-collection-from-blocks"
      2
      (collection-size (make-collection-from-blocks 'test "new" (list b1 b2))))

(define added-coll (collection-add test-coll b4))

(test "collection-add - size increases"
      4
      (collection-size added-coll))

(test "collection-add - original unchanged"
      3
      (collection-size test-coll))

(define removed-coll (collection-remove test-coll (hash-block b2)))

(test "collection-remove - size decreases"
      2
      (collection-size removed-coll))

(define merged-coll (collection-merge test-coll mixed-coll))

(test "collection-merge - combined size"
      6
      (collection-size merged-coll))

(printf "\n")

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "╔════════════════════════════════════════════════════════════╗\n")
(printf "║                    TEST RESULTS                             ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n✓ All tests passed! Collection utilities are working correctly.\n\n")
    (printf "\n✗ Some tests failed. Please review.\n\n"))
