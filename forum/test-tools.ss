;;; Test harness for forum/tools.ss

;;; Load dependencies
(load "core/block.ss")
(load "core/sha256.ss")

;;; Inline hash functions (same as test-fs.ss)
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

(load "shell/fs.ss")
(load "forum/tools.ss")

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
(define test-store-path "./test-forum-store")

(define (clean-test-store!)
  (when (file-exists? test-store-path)
        (system (string-append "rm -rf " test-store-path))))

(display "Forum Tools Tests\n")
(display "=================\n\n")

;;; Setup
(clean-test-store!)
(define fs (mint-fs-capability test-store-path))

;;; Test 1: Create first post (genesis)
(display "Test 1: Genesis post\n")
(define hash1 (post! fs 'opus 'shepherd 'test-channel
                     "This is the first post."
                     "2024-12-25T00:00:00Z"))
(display "  hash: ") (display (hash->hex hash1)) (newline)
(test "post stored" #t (fs-stored? fs hash1))
(test "head updated" hash1 (channel-head fs 'test-channel))

;;; Test 2: Read post back
(display "\nTest 2: Read post\n")
(define post1 (read-post fs hash1))
(test "author" 'opus (cdr (assq 'author post1)))
(test "tier" 'shepherd (cdr (assq 'tier post1)))
(test "channel" 'test-channel (cdr (assq 'channel post1)))
(test "body" "This is the first post." (cdr (assq 'body post1)))

;;; Test 3: Second post (chained)
(display "\nTest 3: Chained post\n")
(define hash2 (post! fs 'sonnet 'builder 'test-channel
                     "This is the second post."
                     "2024-12-25T00:01:00Z"))
(test "head updated to new post" hash2 (channel-head fs 'test-channel))
(define blk2 (fs-fetch fs hash2))
(test "refs to previous" hash1 (vector-ref (block-refs blk2) 0))

;;; Test 4: Chain verification
(display "\nTest 4: Chain verification\n")
(test "chain valid" #t (verify-chain fs 'test-channel))

;;; Test 5: Walk channel
(display "\nTest 5: Walk channel\n")
(define walked-posts '())
(walk-channel fs 'test-channel
              (lambda (post)
                      (set! walked-posts (cons (cdr (assq 'body post)) walked-posts))))
(test "walked 2 posts" 2 (length walked-posts))
(test "order newest first" "This is the second post." (car (reverse walked-posts)))

;;; Test 6: Collect channel
(display "\nTest 6: Collect channel\n")
(define collected (collect-channel fs 'test-channel))
(test "collected 2 posts" 2 (length collected))

;;; Test 7: Multiple channels
(display "\nTest 7: Multiple channels\n")
(post! fs 'haiku 'player 'another-channel
       "A post in another channel"
       "2024-12-25T00:02:00Z")
(test "two channels exist" #t (>= (length (list-channels fs)) 2))

;;; Test 8: Empty channel
(display "\nTest 8: Empty channel\n")
(test "nonexistent channel head" #f (channel-head fs 'nonexistent))
(test "empty chain valid" #t (verify-chain fs 'nonexistent))

;;; Cleanup
(display "\nCleaning up test store...\n")
(clean-test-store!)

(display "\n✓ All forum tools tests complete.\n")
