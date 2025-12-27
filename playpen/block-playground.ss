;;; playpen/block-playground.ss — Playing with Content-Addressed Blocks
;;;
;;; Let's explore the block system that powers The Fold!

;; Change to the fabric/stitches directory for loading
(parameterize ([current-directory "/home/oso/the-fold/fabric/stitches"])
  (load "prelude.ss")
  (load "sha256.ss")
  (load "block.ss"))

;; Load string utilities
(load "thimble/string-utils.ss")

(printf "\n")
(printf "╔═══════════════════════════════════════════════════════════════╗\n")
(printf "║              🔗 CONTENT-ADDRESSED BLOCKS EXPLORER 🔗           ║\n")
(printf "║                                                               ║\n")
(printf "║  Understanding the fundamental unit of The Fold               ║\n")
(printf "╚═══════════════════════════════════════════════════════════════╝\n\n")

;;; Helper to display hash in hex
(define (hash->hex h)
  (let ([hex-chars "0123456789abcdef"])
    (list->string
      (let loop ([i 0] [acc '()])
        (if (= i (bytevector-length h))
            (reverse acc)
            (let ([byte (bytevector-u8-ref h i)])
              (loop (+ i 1)
                    (cons (string-ref hex-chars (bitwise-and byte #x0F))
                          (cons (string-ref hex-chars (bitwise-arithmetic-shift-right byte 4))
                                acc)))))))))

;;; ============================================================
;;; Experiment 1: Creating Simple Blocks
;;; ============================================================

(printf "🎯 Experiment 1: Creating Blocks\n")
(printf "═════════════════════════════════════\n\n")

;; Create a simple text block
(define hello-block
  (make-block
    'text
    (string->utf8 "Hello, Fold!")
    (vector)))

(printf "Block 1: Simple text\n")
(printf "  Tag:     ~a\n" (block-tag hello-block))
(printf "  Payload: ~s\n" (utf8->string (block-payload hello-block)))
(printf "  Refs:    ~a references\n\n" (vector-length (block-refs hello-block)))

;; Create a block with metadata
(define poem-block
  (make-block
    'poem
    (string->utf8 "Roses are red\nViolets are blue\nBlocks are content-addressed\nAnd immutable too")
    (vector)))

(printf "Block 2: A poem\n")
(printf "  Tag:     ~a\n" (block-tag poem-block))
(printf "  Lines:   ~a\n" (length (string-split (utf8->string (block-payload poem-block)) #\newline)))
(printf "  Refs:    ~a references\n\n" (vector-length (block-refs poem-block)))

;;; ============================================================
;;; Experiment 2: Content Addressing - Same Content = Same Hash
;;; ============================================================

(printf "🔐 Experiment 2: Content Addressing\n")
(printf "════════════════════════════════════════\n\n")

(define msg1 (make-block 'data (string->utf8 "test") (vector)))
(define msg2 (make-block 'data (string->utf8 "test") (vector)))
(define msg3 (make-block 'data (string->utf8 "TEST") (vector)))

(define hash1 (hash-block msg1))
(define hash2 (hash-block msg2))
(define hash3 (hash-block msg3))

(printf "Same content should produce same hash:\n")
(printf "  Block A: 'test' → ~a\n" (hash->hex hash1))
(printf "  Block B: 'test' → ~a\n" (hash->hex hash2))
(printf "  Match? ~a\n\n" (bytevector=? hash1 hash2))

(printf "Different content produces different hash:\n")
(printf "  Block C: 'TEST' → ~a\n" (hash->hex hash3))
(printf "  Match A? ~a\n\n" (bytevector=? hash1 hash3))

;;; ============================================================
;;; Experiment 3: Block References - Building a Graph
;;; ============================================================

(printf "🕸️  Experiment 3: Block References\n")
(printf "═══════════════════════════════════════════\n\n")

;; Create leaf nodes (no references)
(define leaf1 (make-block 'leaf (string->utf8 "Leaf A") (vector)))
(define leaf2 (make-block 'leaf (string->utf8 "Leaf B") (vector)))
(define leaf3 (make-block 'leaf (string->utf8 "Leaf C") (vector)))

(define leaf1-hash (hash-block leaf1))
(define leaf2-hash (hash-block leaf2))
(define leaf3-hash (hash-block leaf3))

(printf "Created 3 leaf blocks:\n")
(printf "  Leaf A: ~a...\n" (substring (hash->hex leaf1-hash) 0 16))
(printf "  Leaf B: ~a...\n" (substring (hash->hex leaf2-hash) 0 16))
(printf "  Leaf C: ~a...\n\n" (substring (hash->hex leaf3-hash) 0 16))

;; Create a parent block that references the leaves
(define parent
  (make-block
    'parent
    (string->utf8 "I am the parent")
    (vector leaf1-hash leaf2-hash leaf3-hash)))

(define parent-hash (hash-block parent))

(printf "Created parent block:\n")
(printf "  Hash: ~a...\n" (substring (hash->hex parent-hash) 0 16))
(printf "  References: ~a children\n\n" (vector-length (block-refs parent)))

;; Create a grandparent
(define grandparent
  (make-block
    'root
    (string->utf8 "Family tree root")
    (vector parent-hash)))

(define grandparent-hash (hash-block grandparent))

(printf "Created grandparent block:\n")
(printf "  Hash: ~a...\n" (substring (hash->hex grandparent-hash) 0 16))
(printf "  References: 1 parent (which has 3 children)\n\n")

(printf "Block Graph:\n")
(printf "  Root [~a...]\n" (substring (hash->hex grandparent-hash) 0 8))
(printf "   └─> Parent [~a...]\n" (substring (hash->hex parent-hash) 0 8))
(printf "        ├─> Leaf A [~a...]\n" (substring (hash->hex leaf1-hash) 0 8))
(printf "        ├─> Leaf B [~a...]\n" (substring (hash->hex leaf2-hash) 0 8))
(printf "        └─> Leaf C [~a...]\n\n" (substring (hash->hex leaf3-hash) 0 8))

;;; ============================================================
;;; Experiment 4: Serialization Round-Trip
;;; ============================================================

(printf "♻️  Experiment 4: Serialization\n")
(printf "════════════════════════════════════\n\n")

(define original
  (make-block
    'test
    (string->utf8 "Round-trip test")
    (vector leaf1-hash)))

(printf "Original block:\n")
(printf "  Tag: ~a\n" (block-tag original))
(printf "  Payload: ~s\n" (utf8->string (block-payload original)))

(define serialized (block->bytes original))
(printf "\nSerialized to ~a bytes\n" (bytevector-length serialized))

(define deserialized (bytes->block serialized))
(printf "\nDeserialized block:\n")
(printf "  Tag: ~a\n" (block-tag deserialized))
(printf "  Payload: ~s\n" (utf8->string (block-payload deserialized)))

(define original-hash (hash-block original))
(define deserialized-hash (hash-block deserialized))

(printf "\nHash comparison:\n")
(printf "  Original:     ~a\n" (hash->hex original-hash))
(printf "  Deserialized: ~a\n" (hash->hex deserialized-hash))
(printf "  Match? ~a ✓\n\n" (bytevector=? original-hash deserialized-hash))

;;; ============================================================
;;; Experiment 5: Building a Mini Knowledge Graph
;;; ============================================================

(printf "🧠 Experiment 5: Mini Knowledge Graph\n")
(printf "══════════════════════════════════════════\n\n")

;; Entities
(define turing
  (make-block 'person (string->utf8 "Alan Turing") (vector)))

(define turing-machine
  (make-block 'concept (string->utf8 "Turing Machine") (vector)))

(define turing-hash (hash-block turing))
(define tm-hash (hash-block turing-machine))

;; Relation: Turing invented the Turing Machine
(define invented-relation
  (make-block
    'relation
    (string->utf8 "invented")
    (vector turing-hash tm-hash)))

(define relation-hash (hash-block invented-relation))

(printf "Knowledge Graph:\n")
(printf "  Entity 1: Alan Turing\n")
(printf "    Hash: ~a...\n" (substring (hash->hex turing-hash) 0 16))
(printf "\n")
(printf "  Entity 2: Turing Machine\n")
(printf "    Hash: ~a...\n" (substring (hash->hex tm-hash) 0 16))
(printf "\n")
(printf "  Relation: invented\n")
(printf "    Hash: ~a...\n" (substring (hash->hex relation-hash) 0 16))
(printf "    Links: Entity1 ─[invented]→ Entity2\n\n")

(printf "This is a Merkle DAG! Each hash cryptographically commits to\n")
(printf "the entire subgraph. The relation block proves that these\n")
(printf "specific entities are connected in this specific way.\n\n")

;;; ============================================================
;;; Victory!
;;; ============================================================

(printf "╔═══════════════════════════════════════════════════════════════╗\n")
(printf "║                    ✨ DISCOVERIES MADE! ✨                     ║\n")
(printf "║                                                               ║\n")
(printf "║  • Blocks are {tag, payload, refs[]} triplets                 ║\n")
(printf "║  • Content addressing: Same content = Same hash               ║\n")
(printf "║  • References create Merkle DAGs                              ║\n")
(printf "║  • Serialization is deterministic & reversible                ║\n")
(printf "║  • Perfect for knowledge graphs & versioned data              ║\n")
(printf "║                                                               ║\n")
(printf "║  This is why The Fold is special: Everything is content-     ║\n")
(printf "║  addressed, immutable, and cryptographically verifiable!      ║\n")
(printf "╚═══════════════════════════════════════════════════════════════╝\n\n")

;; Helper function
(define (string-split str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) result))]
      [(char=? (car chars) delim)
       (loop (cdr chars)
             '()
             (cons (list->string (reverse current)) result))]
      [else
       (loop (cdr chars)
             (cons (car chars) current)
             result)])))
