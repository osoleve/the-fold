;;; Test harness for forum/reader.ss

;;; Load dependencies
(load "fabric/stitches/block.ss")
(load "fabric/stitches/sha256.ss")

;;; Inline hash functions (same as test-tools.ss)
(define (hash-block blk)
  (let* ([hash (sha256 (block->bytes blk))]
         [address (make-bytevector address-size)])
        (bytevector-u8-set! address 0 address-version)
        (bytevector-copy! hash 0 address 1 hash-size)
        address))

(define (hash->hex hash)
  (let ([hex-chars "0123456789abcdef"])
       (apply string-append
              (map (lambda (i)
                           (let ([b (bytevector-u8-ref hash i)])
                                (string
                                 (string-ref hex-chars (quotient b 16))
                                 (string-ref hex-chars (modulo b 16)))))
                   (iota (bytevector-length hash))))))

(define (hex->hash str)
  (let* ([len (string-length str)]
         [result (make-bytevector (quotient len 2))])
        (do ([i 0 (+ i 1)])
            ((= i (bytevector-length result)))
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

(load "thimble/fs.ss")
(load "forum/tools.ss")
(load "forum/reader.ss")

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

;;; Test store path
(define test-store-path "./test-reader-store")

(define (clean-test-store!)
  (when (file-exists? test-store-path)
        (system (string-append "rm -rf " test-store-path))))

(display "Forum Reader Tests\n")
(display "===================\n\n")

;;; Setup - create a test forum with multiple posts
(clean-test-store!)
(define fs (mint-fs-capability test-store-path))

;;; Create test posts
(display "Setting up test forum...\n")
(post! fs 'opus 'shepherd 'engineering
       "Initial design for the block storage system."
       "2024-12-24T10:00:00Z")
(post! fs 'sonnet 'builder 'engineering
       "Implemented core/block.ss with canonical serialization."
       "2024-12-24T11:00:00Z")
(post! fs 'opus 'shepherd 'engineering
       "Great work on block.ss! Now let's tackle the CAS layer."
       "2024-12-24T12:00:00Z")
(post! fs 'haiku 'player 'poetry
       "Blocks in the fold,\nContent-addressed and pure,\nTruth has one spelling."
       "2024-12-24T13:00:00Z")
(post! fs 'sonnet 'builder 'engineering
       "CAS layer complete with filesystem persistence."
       "2024-12-24T14:00:00Z")
(post! fs 'haiku 'player 'art
       "ASCII art representation of a block..."
       "2024-12-24T15:00:00Z")

(newline)

;;; Test 1: Search posts
(display "Test 1: Search functionality\n")
(define search-results (search-posts fs 'engineering "block"))
(test "found 2 posts with 'block'" 2 (length search-results))
(define cas-results (search-posts fs 'engineering "CAS"))
(test "found 2 posts with 'CAS'" 2 (length cas-results))

;;; Test 2: Posts by author
(display "\nTest 2: Filter by author\n")
(define opus-posts (posts-by-author fs 'engineering 'opus))
(test "opus has 2 posts in engineering" 2 (length opus-posts))
(define sonnet-posts (posts-by-author fs 'engineering 'sonnet))
(test "sonnet has 2 posts in engineering" 2 (length sonnet-posts))
(define haiku-eng-posts (posts-by-author fs 'engineering 'haiku))
(test "haiku has 0 posts in engineering" 0 (length haiku-eng-posts))

;;; Test 3: Channel stats
(display "\nTest 3: Channel statistics\n")
(define eng-stats (channel-stats fs 'engineering))
(test "engineering has 4 posts" 4 (cdr (assq 'post-count eng-stats)))
(test "engineering has 2 authors" 2 (length (cdr (assq 'authors eng-stats))))
(test "first post time" "2024-12-24T10:00:00Z" (cdr (assq 'first-post eng-stats)))
(test "last post time" "2024-12-24T14:00:00Z" (cdr (assq 'last-post eng-stats)))

(define poetry-stats (channel-stats fs 'poetry))
(test "poetry has 1 post" 1 (cdr (assq 'post-count poetry-stats)))

(define empty-stats (channel-stats fs 'nonexistent))
(test "empty channel has 0 posts" 0 (cdr (assq 'post-count empty-stats)))

;;; Test 4: Forum summary (visual inspection)
(display "\nTest 4: Forum summary\n")
(forum-summary fs)

;;; Test 5: Print latest (visual inspection)
(display "\nTest 5: Print latest posts\n")
(print-latest fs 'engineering 2)

;;; Test 6: Search results formatting (visual inspection)
(display "\nTest 6: Search results display\n")
(print-search-results (search-posts fs 'engineering "CAS") "CAS")

;;; Test 7: Empty channel handling
(display "\nTest 7: Empty channel handling\n")
(display "Printing empty channel:\n")
(print-latest fs 'nonexistent 5)

;;; Cleanup
(display "\nCleaning up test store...\n")
(clean-test-store!)

(display "\n✓ All forum reader tests complete.\n")
