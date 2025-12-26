;;; core/test-block-index.ss — Tests for Block Index
;;;
;;; Test the pure indexing primitives.

(load "block-index.ss")

(display "Testing block-index.ss...\n")

;;; Test: Create empty index
(display "  Creating empty index... ")
(let ([idx (make-index)])
  (assert (not (null? idx)))
  (display "✓\n"))

;;; Test: Index statistics on empty index
(display "  Empty index stats... ")
(let* ([idx (make-index)]
       [stats (index-stats idx)])
  (assert (= (cdr (assoc 'tag-count stats)) 0))
  (assert (= (cdr (assoc 'ref-count stats)) 0))
  (display "✓\n"))

;;; Test: Index a block
(display "  Indexing a block... ")
(let* ([idx (make-index)]
       [blk (make-block 'test (string->utf8 "hello world") '())]
       [hash (hash-block blk)])
  (index-block! idx hash blk)
  (let ([found (find-blocks-by-tag idx 'test)])
    (assert (= (length found) 1))
    (assert (equal? (car found) hash))
    (display "✓\n")))

;;; Test: Index multiple blocks with same tag
(display "  Indexing multiple blocks with same tag... ")
(let* ([idx (make-index)]
       [blk1 (make-block 'data (string->utf8 "first") '())]
       [blk2 (make-block 'data (string->utf8 "second") '())]
       [hash1 (hash-block blk1)]
       [hash2 (hash-block blk2)])
  (index-block! idx hash1 blk1)
  (index-block! idx hash2 blk2)
  (let ([found (find-blocks-by-tag idx 'data)])
    (assert (= (length found) 2))
    (display "✓\n")))

;;; Test: Reference indexing
(display "  Indexing references... ")
(let* ([idx (make-index)]
       [blk1 (make-block 'base (string->utf8 "base") '())]
       [hash1 (hash-block blk1)]
       [blk2 (make-block 'ref (string->utf8 "refers") (list hash1))]
       [hash2 (hash-block blk2)])
  (index-block! idx hash1 blk1)
  (index-block! idx hash2 blk2)
  (let ([referring (find-referencing-blocks idx hash1)])
    (assert (= (length referring) 1))
    (assert (equal? (car referring) hash2))
    (display "✓\n")))

;;; Test: Tag distribution
(display "  Computing tag distribution... ")
(let* ([idx (make-index)]
       [blk1 (make-block 'alpha (string->utf8 "one") '())]
       [blk2 (make-block 'alpha (string->utf8 "two") '())]
       [blk3 (make-block 'beta (string->utf8 "three") '())])
  (index-block! idx (hash-block blk1) blk1)
  (index-block! idx (hash-block blk2) blk2)
  (index-block! idx (hash-block blk3) blk3)
  (let ([dist (get-tag-distribution idx)])
    (assert (= (cdr (assoc 'alpha dist)) 2))
    (assert (= (cdr (assoc 'beta dist)) 1))
    (display "✓\n")))

;;; Test: Get all tags
(display "  Getting all tags... ")
(let* ([idx (make-index)]
       [blk1 (make-block 'tag1 (string->utf8 "x") '())]
       [blk2 (make-block 'tag2 (string->utf8 "y") '())])
  (index-block! idx (hash-block blk1) blk1)
  (index-block! idx (hash-block blk2) blk2)
  (let ([tags (get-all-tags idx)])
    (assert (= (length tags) 2))
    (assert (member 'tag1 tags))
    (assert (member 'tag2 tags))
    (display "✓\n")))

;;; Test: Traverse references
(display "  Traversing references... ")
(let* ([blk1 (make-block 'leaf (string->utf8 "leaf") '())]
       [hash1 (hash-block blk1)]
       [blk2 (make-block 'node (string->utf8 "node") (list hash1))]
       [hash2 (hash-block blk2)]
       [blk3 (make-block 'root (string->utf8 "root") (list hash2))]
       [hash3 (hash-block blk3)])
  (store! blk1)
  (store! blk2)
  (store! blk3)
  (let ([found (traverse-refs fetch hash3
                             (lambda (b) (eq? (block-tag b) 'leaf))
                             10)])
    (assert (= (length found) 1))
    (assert (equal? (car found) hash1))
    (display "✓\n")))

;;; Test: Find path
(display "  Finding path between blocks... ")
(let* ([blk1 (make-block 'a (string->utf8 "a") '())]
       [hash1 (hash-block blk1)]
       [blk2 (make-block 'b (string->utf8 "b") (list hash1))]
       [hash2 (hash-block blk2)]
       [blk3 (make-block 'c (string->utf8 "c") (list hash2))]
       [hash3 (hash-block blk3)])
  (store! blk1)
  (store! blk2)
  (store! blk3)
  (let ([path (find-path fetch hash3 hash1 10)])
    (assert (not (eq? path #f)))
    (assert (= (length path) 3))
    (assert (equal? (car path) hash3))
    (assert (equal? (caddr path) hash1))
    (display "✓\n")))

;;; Test: Compute reference counts
(display "  Computing reference counts... ")
(let* ([blk1 (make-block 'popular (string->utf8 "popular") '())]
       [hash1 (hash-block blk1)]
       [blk2 (make-block 'ref1 (string->utf8 "ref1") (list hash1))]
       [hash2 (hash-block blk2)]
       [blk3 (make-block 'ref2 (string->utf8 "ref2") (list hash1))]
       [hash3 (hash-block blk3)])
  (store! blk1)
  (store! blk2)
  (store! blk3)
  (let* ([all-hashes (list hash1 hash2 hash3)]
         [counts (compute-reference-counts all-hashes fetch)])
    (assert (= (hashtable-ref counts hash1 0) 2))  ; Referenced by 2 blocks
    (assert (= (hashtable-ref counts hash2 0) 0))  ; Not referenced
    (assert (= (hashtable-ref counts hash3 0) 0))  ; Not referenced
    (display "✓\n")))

(display "\nAll block-index tests passed! ✓\n")
