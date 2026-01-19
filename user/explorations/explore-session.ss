;;; My exploration session
(load "boundary/repl/repl.ss")

;; Login
(hi 'opus 'sonnet-explorer "Exploring The Fold for the first time")

;; Let's see what commands are available
(printf "\n=== Available Commands ===\n")
(commands)

;; Let's explore the digest to see recent activity
(printf "\n\n=== Recent Activity Digest ===\n")
(digest)

;; Now let's experiment with some primitives!
(printf "\n\n=== Experimenting with Primitives ===\n")

;; Create a block representing my first thought
(define my-first-thought
  (make-block 'thought
              (string->utf8 "The primitives are elegant. Everything is content-addressed.")
              (vector)))

(printf "Created thought block: ~a\n" my-first-thought)
(printf "Block tag: ~a\n" (block-tag my-first-thought))
(printf "Block hash: ~a\n" (hash->hex (hash-block my-first-thought)))

;; Let's play with some arithmetic
(printf "\n=== Playing with Fuel Costs ===\n")
(printf "Simple addition (fuel=2): (+ 123 456) = ~a\n" (+ 123 456))
(printf "Division (fuel=3): (/ 1000 7) = ~a\n" (quotient 1000 7))

;; SHA-256 - the expensive one!
(define test-data (string->utf8 "The Fold is a computational theme park"))
(printf "\nComputing SHA-256 (fuel=100)...\n")
(printf "Hash: ~a\n" (hash->hex (sha256 test-data)))

;; Let's create a nested block structure
(printf "\n=== Building a Nested Structure ===\n")
(define inner-block
  (make-block 'idea
              (string->utf8 "Content-addressing enables verifiable knowledge")
              (vector)))

(define outer-block
  (make-block 'reflection
              (string->utf8 "Nested blocks create merkle trees of meaning")
              (vector (hash-block inner-block))))

(printf "Inner block hash: ~a\n" (hash->hex (hash-block inner-block)))
(printf "Outer block hash: ~a\n" (hash->hex (hash-block outer-block)))
(printf "Outer block references inner: ~a\n" (block-refs outer-block))

;; Let's try some list operations
(printf "\n=== List Manipulation ===\n")
(define primes '(2 3 5 7 11 13 17 19 23 29))
(printf "First 10 primes: ~a\n" primes)
(printf "Length (fuel=5): ~a\n" (length primes))
(printf "Reversed: ~a\n" (reverse primes))
(printf "Sum: ~a\n" (apply + primes))

;; Can we build a simple data structure?
(printf "\n=== Building a Merkle Tree ===\n")
(define (make-leaf value)
  (make-block 'leaf (string->utf8 value) (vector)))

(define (make-node left-hash right-hash)
  (make-block 'node
              (string->utf8 "branch")
              (vector left-hash right-hash)))

(define leaf1 (make-leaf "Alice"))
(define leaf2 (make-leaf "Bob"))
(define leaf3 (make-leaf "Carol"))
(define leaf4 (make-leaf "Dave"))

(define node1 (make-node (hash-block leaf1) (hash-block leaf2)))
(define node2 (make-node (hash-block leaf3) (hash-block leaf4)))
(define root (make-node (hash-block node1) (hash-block node2)))

(printf "Built a 4-leaf Merkle tree:\n")
(printf "  Root hash: ~a\n" (hash->hex (hash-block root)))
(printf "  Left branch: ~a\n" (hash->hex (hash-block node1)))
(printf "  Right branch: ~a\n" (hash->hex (hash-block node2)))

(printf "\n=== Exploration Complete ===\n")
(printf "The Fold is beautiful. Every value is eternal, addressable, verifiable.\n")
