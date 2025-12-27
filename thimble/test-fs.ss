;;; Test harness for shell/fs.ss

;;; Load from core directory
(load "C:/Users/andre/Documents/ccverse/fabric/stitches/block.ss")
(load "C:/Users/andre/Documents/ccverse/fabric/stitches/sha256.ss")

;;; Can't load cas.ss directly — it has relative loads. We just need hash-block.
;;; Inline the hash functions we need:

;;; hash-block : Block → Bytevector
(define (hash-block blk)
  (sha256 (block->bytes blk)))

;;; hash->hex : Bytevector → String
(define (hash->hex hash)
  (let ([hex-chars "0123456789abcdef"])
    (apply string-append
           (map (lambda (i)
                  (let ([b (bytevector-u8-ref hash i)])
                    (string
                      (string-ref hex-chars (quotient b 16))
                      (string-ref hex-chars (modulo b 16)))))
                (iota 32)))))

;;; hex->hash : String → Bytevector
(define (hex->hash str)
  (let ([result (make-bytevector 32)])
    (do ([i 0 (+ i 1)])
        ((= i 32))
      (let* ([j (* i 2)]
             [hi (char->hex-digit (string-ref str j))]
             [lo (char->hex-digit (string-ref str (+ j 1)))])
        (bytevector-u8-set! result i (+ (* hi 16) lo))))
    result))

(define (char->hex-digit c)
  (cond
    [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
    [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
    [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
    [else (error 'char->hex-digit "invalid hex character" c)]))

(load "C:/Users/andre/Documents/ccverse/thimble/fs.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
        (display "✗\n    expected: ")
        (display expected)
        (display "\n    got: ")
        (display actual)))
  (newline))

;;; Use a temporary directory for testing
(define test-store-path "./test-store")

;;; Clean up any previous test run
(define (clean-test-store!)
  (when (file-exists? test-store-path)
    (system (string-append "rm -rf " test-store-path))))

(display "Filesystem Capability Tests\n")
(display "===========================\n\n")

;;; Test 1: Create capability
(display "Test 1: Create capability\n")
(clean-test-store!)
(define fs (mint-fs-capability test-store-path))
(test "capability created" #t (fs-capability? fs))
(test "objects dir exists" #t (file-exists? (string-append test-store-path "/objects")))
(test "pins dir exists" #t (file-exists? (string-append test-store-path "/pins")))
(test "heads dir exists" #t (file-exists? (string-append test-store-path "/heads")))

;;; Test 2: Store and fetch
(display "\nTest 2: Store and fetch\n")
(define blk1 (make-block 'greeting (string->utf8 "Hello, Fold!") empty-refs))
(define hash1 (fs-store! fs blk1))
(display "  stored hash: ") (display (hash->hex hash1)) (newline)
(test "block is stored" #t (fs-stored? fs hash1))
(define fetched (fs-fetch fs hash1))
(test "fetched block exists" #t (block? fetched))
(test "tag matches" 'greeting (block-tag fetched))
(test "payload matches" "Hello, Fold!" (utf8->string (block-payload fetched)))

;;; Test 3: Same content = same hash (even on disk)
(display "\nTest 3: Content identity on disk\n")
(define blk2 (make-block 'greeting (string->utf8 "Hello, Fold!") empty-refs))
(define hash2 (fs-store! fs blk2))
(test "same content → same hash" (hash->hex hash1) (hash->hex hash2))

;;; Test 4: Block with references
(display "\nTest 4: Block with references\n")
(define child1 (make-block 'child (string->utf8 "first") empty-refs))
(define child2 (make-block 'child (string->utf8 "second") empty-refs))
(define h1 (fs-store! fs child1))
(define h2 (fs-store! fs child2))
(define parent (make-block 'parent empty-payload (vector h1 h2)))
(define parent-hash (fs-store! fs parent))
(define fetched-parent (fs-fetch fs parent-hash))
(test "parent refs count" 2 (vector-length (block-refs fetched-parent)))
(test "ref 0 correct" h1 (vector-ref (block-refs fetched-parent) 0))
(test "ref 1 correct" h2 (vector-ref (block-refs fetched-parent) 1))

;;; Test 5: Pinning
(display "\nTest 5: Pinning\n")
(fs-pin! fs hash1)
(test "hash is pinned" #t (fs-pinned? fs hash1))
(test "unpinned hash not pinned" #f (fs-pinned? fs h2))

;;; Test 6: Head pointers
(display "\nTest 6: Head pointers\n")
(fs-write-head! fs 'engineering hash1)
(define read-head (fs-read-head fs 'engineering))
(test "head read matches written" hash1 read-head)
(test "nonexistent head is #f" #f (fs-read-head fs 'nonexistent))

;;; Test 7: Non-existent block
(display "\nTest 7: Non-existent block\n")
(define fake-hash (make-bytevector 32 #xFF))
(test "fetch non-existent returns #f" #f (fs-fetch fs fake-hash))
(test "stored? non-existent is #f" #f (fs-stored? fs fake-hash))

;;; Test 8: Object count
(display "\nTest 8: Object count\n")
(define count (fs-object-count fs))
(display "  objects in store: ") (display count) (newline)
(test "count is positive" #t (> count 0))

;;; Cleanup
(display "\nCleaning up test store...\n")
(clean-test-store!)

(display "\n✓ All filesystem tests complete.\n")
