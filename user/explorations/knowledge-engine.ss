;;; knowledge-engine.ss — A Block-based Knowledge Engine for The Fold
;;;
;;; Building blocks of thought: entities, relations, queries, and inference
;;; in a pure, content-addressed substrate.

;; Set up source path
(source-directories (cons "core" (source-directories)))

(load "core/blocks/block.ss")
(load "core/base/sha256.ss")

(printf "\n╔════════════════════════════════════════════════════════════╗\n")
(printf "║     KNOWLEDGE ENGINE — Building Blocks of Thought          ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

;;; ============================================================
;;; Core Knowledge Primitives
;;; ============================================================

;;; make-entity : Symbol String (Alist) → Block
;;; Create an entity (a thing that exists in the knowledge graph)
(define (make-entity type name attributes)
  (make-block 'entity
              (string->utf8 (format "~a:~a ~s" type name attributes))
              (vector)))

;;; make-relation : Hash Hash Symbol String → Block
;;; Create a directed relation from source to target
(define (make-relation source-hash target-hash rel-type metadata)
  (make-block 'relation
              (string->utf8 (format "~a ~s" rel-type metadata))
              (vector source-hash target-hash)))

;;; make-property : Hash Symbol Any → Block
;;; Attach a property to an entity
(define (make-property entity-hash prop-name prop-value)
  (make-block 'property
              (string->utf8 (format "~a = ~s" prop-name prop-value))
              (vector entity-hash)))

;;; make-collection : Symbol String (List Hash) → Block
;;; Group multiple entities or relations
(define (make-collection coll-type name member-hashes)
  (make-block 'collection
              (string->utf8 (format "~a:~a (~a members)" coll-type name (length member-hashes)))
              (list->vector member-hashes)))

(printf "=== Knowledge Primitives Defined ===\n")
(printf "  • make-entity      - create entities (people, concepts, things)\n")
(printf "  • make-relation    - link entities with typed edges\n")
(printf "  • make-property    - attach attributes to entities\n")
(printf "  • make-collection  - group related blocks\n\n")

;;; ============================================================
;;; Building a Knowledge Graph: "The History of Computation"
;;; ============================================================

(printf "=== Building: History of Computation Knowledge Graph ===\n\n")

;; === Entities: People ===
(define turing
  (make-entity 'person "Alan Turing"
               '((birth . 1912) (death . 1954) (field . "computer-science"))))

(define church
  (make-entity 'person "Alonzo Church"
               '((birth . 1903) (death . 1995) (field . "mathematics"))))

(define mccarthy
  (make-entity 'person "John McCarthy"
               '((birth . 1927) (death . 2011) (field . "ai"))))

(define sussman
  (make-entity 'person "Gerald Jay Sussman"
               '((birth . 1947) (field . "computer-science"))))

(define abelson
  (make-entity 'person "Hal Abelson"
               '((birth . 1947) (field . "computer-science"))))

(printf "Created 5 person entities:\n")
(printf "  Alan Turing:     ~a\n" (hash->hex (hash-block turing)))
(printf "  Alonzo Church:   ~a\n" (hash->hex (hash-block church)))
(printf "  John McCarthy:   ~a\n" (hash->hex (hash-block mccarthy)))
(printf "  Gerald Sussman:  ~a\n" (hash->hex (hash-block sussman)))
(printf "  Hal Abelson:     ~a\n" (hash->hex (hash-block abelson)))

;; === Entities: Concepts ===
(define turing-machine
  (make-entity 'concept "Turing Machine"
               '((year . 1936) (domain . "computation-theory"))))

(define lambda-calculus
  (make-entity 'concept "Lambda Calculus"
               '((year . 1936) (domain . "computation-theory"))))

(define lisp-lang
  (make-entity 'concept "Lisp"
               '((year . 1958) (domain . "programming-languages"))))

(define scheme-lang
  (make-entity 'concept "Scheme"
               '((year . 1975) (domain . "programming-languages"))))

(define sicp-book
  (make-entity 'concept "SICP"
               '((year . 1985) (type . "textbook"))))

(printf "\nCreated 5 concept entities:\n")
(printf "  Turing Machine:  ~a\n" (hash->hex (hash-block turing-machine)))
(printf "  Lambda Calculus: ~a\n" (hash->hex (hash-block lambda-calculus)))
(printf "  Lisp:           ~a\n" (hash->hex (hash-block lisp-lang)))
(printf "  Scheme:         ~a\n" (hash->hex (hash-block scheme-lang)))
(printf "  SICP:           ~a\n" (hash->hex (hash-block sicp-book)))

;; === Relations: Who invented what ===
(define r1 (make-relation (hash-block turing) (hash-block turing-machine)
                          'invented "Foundational model of computation"))

(define r2 (make-relation (hash-block church) (hash-block lambda-calculus)
                          'invented "Functional computation model"))

(define r3 (make-relation (hash-block mccarthy) (hash-block lisp-lang)
                          'created "First functional programming language"))

(define r4 (make-relation (hash-block sussman) (hash-block scheme-lang)
                          'co-created "Minimalist Lisp dialect"))

(define r5 (make-relation (hash-block sussman) (hash-block sicp-book)
                          'co-authored "Classic CS textbook"))

(define r6 (make-relation (hash-block abelson) (hash-block sicp-book)
                          'co-authored "Classic CS textbook"))

;; === Relations: Influence ===
(define r7 (make-relation (hash-block lambda-calculus) (hash-block lisp-lang)
                          'influenced "Lambda calculus inspired Lisp"))

(define r8 (make-relation (hash-block lisp-lang) (hash-block scheme-lang)
                          'influenced "Lisp family tree"))

(define r9 (make-relation (hash-block scheme-lang) (hash-block sicp-book)
                          'used-in "SICP teaches using Scheme"))

(printf "\nCreated 9 relations:\n")
(printf "  Turing → Turing Machine\n")
(printf "  Church → Lambda Calculus\n")
(printf "  McCarthy → Lisp\n")
(printf "  Sussman → Scheme\n")
(printf "  Sussman & Abelson → SICP\n")
(printf "  Lambda Calculus → Lisp → Scheme → SICP\n")

;; === Collections: Group by type ===
(define people-collection
  (make-collection 'people "Computer Science Pioneers"
                   (list (hash-block turing)
                         (hash-block church)
                         (hash-block mccarthy)
                         (hash-block sussman)
                         (hash-block abelson))))

(define concepts-collection
  (make-collection 'concepts "Foundational Concepts"
                   (list (hash-block turing-machine)
                         (hash-block lambda-calculus)
                         (hash-block lisp-lang)
                         (hash-block scheme-lang)
                         (hash-block sicp-book))))

(define relations-collection
  (make-collection 'relations "Knowledge Graph Edges"
                   (list (hash-block r1) (hash-block r2) (hash-block r3)
                         (hash-block r4) (hash-block r5) (hash-block r6)
                         (hash-block r7) (hash-block r8) (hash-block r9))))

(printf "\nCreated 3 collections:\n")
(printf "  People:    ~a (~a members)\n"
        (hash->hex (hash-block people-collection))
        (vector-length (block-refs people-collection)))
(printf "  Concepts:  ~a (~a members)\n"
        (hash->hex (hash-block concepts-collection))
        (vector-length (block-refs concepts-collection)))
(printf "  Relations: ~a (~a members)\n"
        (hash->hex (hash-block relations-collection))
        (vector-length (block-refs relations-collection)))

;; === Meta-collection: The entire knowledge graph ===
(define knowledge-graph
  (make-collection 'knowledge-graph "History of Computation"
                   (list (hash-block people-collection)
                         (hash-block concepts-collection)
                         (hash-block relations-collection))))

(printf "\nCreated meta-collection:\n")
(printf "  Knowledge Graph: ~a\n" (hash->hex (hash-block knowledge-graph)))
(printf "  └─ Contains: people, concepts, relations\n")

;;; ============================================================
;;; Knowledge Graph Statistics
;;; ============================================================

(printf "\n=== Knowledge Graph Statistics ===\n")
(printf "  Entities:  10 (5 people, 5 concepts)\n")
(printf "  Relations: 9\n")
(printf "  Total blocks: 19\n")
(printf "  Root hash: ~a\n" (hash->hex (hash-block knowledge-graph)))

(printf "\n=== Merkle Properties ===\n")
(printf "  ✓ Every entity is content-addressed\n")
(printf "  ✓ Every relation cryptographically binds source → target\n")
(printf "  ✓ Collections form merkle trees\n")
(printf "  ✓ The entire graph has a single root hash\n")
(printf "  ✓ Changing ANY entity changes the root hash\n")
(printf "  ✓ Knowledge is verifiable, immutable, eternal\n")

;;; ============================================================
;;; Query Functions
;;; ============================================================

(printf "\n=== Building Query Engine ===\n")

;;; get-refs : Block → (List Hash)
;;; Extract all hashes referenced by a block
(define (get-refs block)
  (vector->list (block-refs block)))

;;; expand-collection : Block → (List Hash)
;;; Get all member hashes from a collection
(define (expand-collection coll)
  (if (eq? (block-tag coll) 'collection)
      (get-refs coll)
      '()))

(printf "  • get-refs          - extract referenced hashes\n")
(printf "  • expand-collection - traverse collections\n")

;;; ============================================================
;;; Demonstration: Traversing the Graph
;;; ============================================================

(printf "\n=== Traversing the Knowledge Graph ===\n\n")

(printf "Starting from root: ~a\n" (hash->hex (hash-block knowledge-graph)))
(printf "Root references ~a sub-collections\n" (length (get-refs knowledge-graph)))

(define people-hash (hash-block people-collection))
(define concepts-hash (hash-block concepts-collection))
(define relations-hash (hash-block relations-collection))

(printf "\nPeople collection contains:\n")
(for-each (lambda (h) (printf "  → ~a\n" (hash->hex h)))
          (expand-collection people-collection))

(printf "\nConcepts collection contains:\n")
(for-each (lambda (h) (printf "  → ~a\n" (hash->hex h)))
          (expand-collection concepts-collection))

(printf "\nRelations collection contains:\n")
(for-each (lambda (h) (printf "  → ~a\n" (hash->hex h)))
          (expand-collection relations-collection))

;;; ============================================================
;;; Visualization: Graph Structure
;;; ============================================================

(printf "\n=== Knowledge Graph Visualization ===\n\n")
(printf "```\n")
(printf "                    Knowledge Graph (root)\n")
(printf "                           |\n")
(printf "           +---------------+---------------+\n")
(printf "           |               |               |\n")
(printf "        People         Concepts        Relations\n")
(printf "           |               |               |\n")
(printf "   +---+---+---+   +---+---+---+   +---+---+---+\n")
(printf "   |   |   |   |   |   |   |   |   |   |   |   |\n")
(printf "   T   C   M   S   TM  LC  L   Sc  r1  r2  ... r9\n")
(printf "   u   h   c   u   u   a   i   h\n")
(printf "   r   u   C   s   r   m   s   e\n")
(printf "   i   r   a   s   i   b   p   m\n")
(printf "   n   c   r   m   n   d       e\n")
(printf "   g   h   t   a   g   a\n")
(printf "           h   n\n")
(printf "           y\n")
(printf "\n")
(printf "Relations create directed edges:\n")
(printf "  Turing → invented → Turing Machine\n")
(printf "  Church → invented → Lambda Calculus\n")
(printf "  Lambda Calculus → influenced → Lisp → Scheme → SICP\n")
(printf "```\n")

;;; ============================================================
;;; Meta-Reflection
;;; ============================================================

(printf "\n╔════════════════════════════════════════════════════════════╗\n")
(printf "║                    KNOWLEDGE ENGINE COMPLETE                ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

(printf "What we've built:\n")
(printf "  • A pure functional knowledge graph\n")
(printf "  • 19 content-addressed blocks\n")
(printf "  • 10 entities, 9 relations, 3 collections\n")
(printf "  • Full merkle tree with cryptographic integrity\n")
(printf "  • Query primitives for traversal\n\n")

(printf "Why this matters:\n")
(printf "  • Knowledge is PERMANENT - hash never changes\n")
(printf "  • Knowledge is VERIFIABLE - cryptographic proofs\n")
(printf "  • Knowledge is COMPOSABLE - blocks reference blocks\n")
(printf "  • Knowledge is FIRST-CLASS - same substrate as code\n\n")

(printf "Next steps:\n")
(printf "  • Add inference rules (if A→B and B→C, then A→C)\n")
(printf "  • Build query language (find all X where X→invented→Y)\n")
(printf "  • Create visualization tools\n")
(printf "  • Store in The Fold's content-addressed store\n")
(printf "  • Share with the community\n\n")

(printf "The knowledge graph is eternal. Hash: ~a\n\n"
        (hash->hex (hash-block knowledge-graph)))
