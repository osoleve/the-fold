;;; Creative exploration - games, types, and art!
(load "thimble/repl.ss")

(printf "\n╔══════════════════════════════════════════════════════════╗\n")
(printf "║        CREATIVE EXPLORATION IN THE FOLD                  ║\n")
(printf "╚══════════════════════════════════════════════════════════╝\n\n")

;; === PART 1: Type System Exploration ===
(printf "=== Part 1: Exploring the Type System ===\n\n")

;; Let's parse and type-check some expressions!
(define expr1 '(lambda (x) (add x 1)))
(define expr2 '((lambda (f) (f 42)) (lambda (x) (mul x 2))))

(printf "Expression 1: ~s\n" expr1)
(printf "Parsing...\n")
(define parsed1 (fold-parse expr1))
(printf "Parsed: ~a\n" parsed1)

(printf "\nExpression 2: ~s\n" expr2)
(define parsed2 (fold-parse expr2))
(printf "Parsed: ~a\n" parsed2)

;; === PART 2: Build a Content-Addressed Knowledge Graph ===
(printf "\n\n=== Part 2: Building a Knowledge Graph ===\n\n")

;; Create interconnected blocks representing concepts
(define (make-concept name description)
  (make-block 'concept (string->utf8 description) (vector)))

(define (make-relation source-hash target-hash label)
  (make-block 'relation
              (string->utf8 label)
              (vector source-hash target-hash)))

;; Build a mini knowledge graph about The Fold itself!
(define c-block (make-concept "Block" "Fundamental unit: tag + payload + refs"))
(define c-hash (make-concept "Hash" "SHA-256 content addressing"))
(define c-fold (make-concept "The Fold" "Computational theme park for AI agents"))
(define c-homoiconic (make-concept "Homoiconic" "Code is data, data is code"))

;; Create relationships
(define rel1 (make-relation (hash-block c-fold) (hash-block c-block) "is-built-from"))
(define rel2 (make-relation (hash-block c-block) (hash-block c-hash) "is-identified-by"))
(define rel3 (make-relation (hash-block c-fold) (hash-block c-homoiconic) "embraces"))

(printf "Knowledge Graph Nodes:\n")
(printf "  Block concept: ~a\n" (hash->hex (hash-block c-block)))
(printf "  Hash concept: ~a\n" (hash->hex (hash-block c-hash)))
(printf "  Fold concept: ~a\n" (hash->hex (hash-block c-fold)))
(printf "  Homoiconic: ~a\n" (hash->hex (hash-block c-homoiconic)))

(printf "\nKnowledge Graph Edges:\n")
(printf "  ~a\n" (hash->hex (hash-block rel1)))
(printf "  ~a\n" (hash->hex (hash-block rel2)))
(printf "  ~a\n" (hash->hex (hash-block rel3)))

;; === PART 3: Recursive Block Structures ===
(printf "\n\n=== Part 3: Recursive Fibonacci Blocks ===\n\n")

;; Represent Fibonacci sequence as a linked block chain
(define (make-fib n value prev-hash)
  (if prev-hash
      (make-block 'fib
                  (string->utf8 (format "F(~a) = ~a" n value))
                  (vector prev-hash))
      (make-block 'fib
                  (string->utf8 (format "F(~a) = ~a" n value))
                  (vector))))

(define fib0 (make-fib 0 0 #f))
(define fib1 (make-fib 1 1 (hash-block fib0)))
(define fib2 (make-fib 2 1 (hash-block fib1)))
(define fib3 (make-fib 3 2 (hash-block fib2)))
(define fib4 (make-fib 4 3 (hash-block fib3)))
(define fib5 (make-fib 5 5 (hash-block fib4)))
(define fib6 (make-fib 6 8 (hash-block fib5)))

(printf "Fibonacci chain (each block references previous):\n")
(printf "  F(0): ~a\n" (hash->hex (hash-block fib0)))
(printf "  F(1): ~a\n" (hash->hex (hash-block fib1)))
(printf "  F(2): ~a\n" (hash->hex (hash-block fib2)))
(printf "  F(3): ~a\n" (hash->hex (hash-block fib3)))
(printf "  F(4): ~a\n" (hash->hex (hash-block fib4)))
(printf "  F(5): ~a\n" (hash->hex (hash-block fib5)))
(printf "  F(6): ~a\n" (hash->hex (hash-block fib6)))

;; === PART 4: Block-based Poetry ===
(printf "\n\n=== Part 4: Merkle Poetry ===\n\n")

(define (verse text)
  (make-block 'verse (string->utf8 text) (vector)))

(define (stanza . verse-hashes)
  (make-block 'stanza
              (string->utf8 (format "~a verses" (length verse-hashes)))
              (list->vector verse-hashes)))

;; A poem about The Fold
(define v1 (verse "In blocks we trust, eternal and true"))
(define v2 (verse "Each hash a name, forever new"))
(define v3 (verse "Content addressed, never to fade"))
(define v4 (verse "In merkle trees, our thoughts are laid"))

(define poem (stanza (hash-block v1)
                     (hash-block v2)
                     (hash-block v3)
                     (hash-block v4)))

(printf "A Merkle Poem:\n")
(printf "  Verse 1: ~a\n" (hash->hex (hash-block v1)))
(printf "  Verse 2: ~a\n" (hash->hex (hash-block v2)))
(printf "  Verse 3: ~a\n" (hash->hex (hash-block v3)))
(printf "  Verse 4: ~a\n" (hash->hex (hash-block v4)))
(printf "  Poem hash: ~a\n" (hash->hex (hash-block poem)))

;; === PART 5: Bitwise Magic ===
(printf "\n\n=== Part 5: Bitwise Artistry ===\n\n")

;; Create interesting patterns with bit operations
(define (bit-pattern n)
  (let loop ([i 0] [result '()])
    (if (>= i 8)
        (reverse result)
        (loop (+ i 1)
              (cons (bitand (shr n i) 1) result)))))

(printf "Binary patterns:\n")
(printf "  170 = #b10101010 -> ~a\n" (bit-pattern 170))
(printf "  85  = #b01010101 -> ~a\n" (bit-pattern 85))
(printf "  255 = #b11111111 -> ~a\n" (bit-pattern 255))
(printf "  XOR: ~a = ~a\n" (bitxor 170 85) (bit-pattern (bitxor 170 85)))

;; === PART 6: Performance Art ===
(printf "\n\n=== Part 6: Fuel Consumption Art ===\n\n")

;; Create expressions with interesting fuel profiles
(printf "Low fuel (simple arithmetic): ")
(printf "~a fuel\n" (+ 2 2 3 3))  ; Tier 2 ops

(printf "Medium fuel (list operations): ")
(define test-list '(1 2 3 4 5 6 7 8 9 10))
(printf "length=~a, sum=~a\n" (length test-list) (apply + test-list))

(printf "High fuel (crypto): ")
(define data (string->utf8 "Art requires computation"))
(printf "~a\n" (hash->hex (sha256 data)))

(printf "\n╔══════════════════════════════════════════════════════════╗\n")
(printf "║  CREATIVE EXPLORATION COMPLETE                           ║\n")
(printf "║  The Fold is a canvas. Every block is a brushstroke.    ║\n")
(printf "╚══════════════════════════════════════════════════════════╝\n")
