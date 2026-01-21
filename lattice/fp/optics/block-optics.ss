;;; lattice/fp/optics/block-optics.ss — Block Optics
;;; @module block-optics
;;; @requires block optics

(load "core/blocks/block.ss")
(load "lattice/fp/optics/optics.ss")

(doc 'module 'block-optics)
(doc 'description "Optics for The Fold's Block System

Composable optics for accessing and traversing content-addressed blocks:

  - block-tag-lens     : Focus on block tag
  - block-payload-lens : Focus on block payload
  - block-refs-lens    : Focus on refs vector
  - block-ref-at       : Affine for specific ref by index
  - block-refs-each    : Traversal over all refs
  - follow-ref         : Affine that follows a ref through a CAS")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'basic-lenses)

(doc block-tag-lens 'type '(Lens Block Symbol))
(doc block-tag-lens 'description "Focus on the tag of a block.

Note: Modifying a block creates a new block (immutable record).
The new block will have a different hash.")
(doc block-tag-lens 'export #t)
(define block-tag-lens
  (make-lens
   block-tag
   (lambda (new-tag blk)
     (make-block new-tag
                 (block-payload blk)
                 (block-refs blk)))))

(doc block-payload-lens 'type '(Lens Block Bytevector))
(doc block-payload-lens 'description "Focus on the payload of a block.")
(doc block-payload-lens 'export #t)
(define block-payload-lens
  (make-lens
   block-payload
   (lambda (new-payload blk)
     (make-block (block-tag blk)
                 new-payload
                 (block-refs blk)))))

(doc block-refs-lens 'type '(Lens Block (Vector Bytevector)))
(doc block-refs-lens 'description "Focus on the refs vector of a block.")
(doc block-refs-lens 'export #t)
(define block-refs-lens
  (make-lens
   block-refs
   (lambda (new-refs blk)
     (make-block (block-tag blk)
                 (block-payload blk)
                 new-refs))))

(doc 'section 'ref-access)

(define (block-ref-at n)
  (doc 'export #t)
  (doc 'type '(-> Nat (Affine Block Bytevector)))
  (doc 'description "Focus on a specific ref by index.
Returns nothing if index is out of bounds.")
  (make-affine
   ;; getter: Block -> Maybe Bytevector
   (lambda (blk)
     (let ([refs (block-refs blk)])
       (if (and (>= n 0) (< n (vector-length refs)))
           (just (vector-ref refs n))
           nothing)))
   ;; setter: Bytevector -> Block -> Block
   (lambda (new-ref blk)
     (let* ([refs (block-refs blk)]
            [len (vector-length refs)])
       (if (< n len)
           ;; Create new vector with updated ref
           (let ([new-refs (make-vector len)])
             (do ([i 0 (+ i 1)])
                 ((= i len))
               (vector-set! new-refs i
                            (if (= i n) new-ref (vector-ref refs i))))
             (make-block (block-tag blk)
                         (block-payload blk)
                         new-refs))
           ;; Index out of bounds - return unchanged
           blk)))))

(doc block-refs-each 'type '(Traversal Block Block Bytevector Bytevector))
(doc block-refs-each 'description "Traverse over all refs in a block.")
(doc block-refs-each 'export #t)
(define block-refs-each
  (make-traversal
   ;; traverse: (Bytevector -> Bytevector) -> Block -> Block
   (lambda (f blk)
     (let* ([refs (block-refs blk)]
            [len (vector-length refs)]
            [new-refs (make-vector len)])
       (do ([i 0 (+ i 1)])
           ((= i len))
         (vector-set! new-refs i (f (vector-ref refs i))))
       (make-block (block-tag blk)
                   (block-payload blk)
                   new-refs)))
   ;; fold: Block -> List Bytevector
   (lambda (blk)
     (vector->list (block-refs blk)))))

(define (block-refs-count blk)
  (doc 'export #t)
  (doc 'type '(-> Block Nat))
  (doc 'description "Helper: count number of refs in a block.")
  (vector-length (block-refs blk)))

(doc 'section 'cas-aware)

(define (follow-ref fetch)
  (doc 'export #t)
  (doc 'type '(-> (-> Bytevector (Maybe Block)) (Affine Bytevector Block)))
  (doc 'description "Create an affine that follows a ref (hash) to its block.
The fetch function should be like CAS's fetch: hash -> Block | #f.

Note: Setting through this affine is meaningless since hashes
are derived from content. We return the ref unchanged.")
  (make-affine
   ;; getter: Bytevector -> Maybe Block
   (lambda (ref)
     (let ([blk (fetch ref)])
       (if blk (just blk) nothing)))
   ;; setter: Block -> Bytevector -> Bytevector
   ;; Cannot update ref to point to new block (hash-derived identity).
   ;; Caller must store new block and use that hash instead.
   (lambda (_new-block ref)
     ref)))

(define (block-child-at fetch n)
  (doc 'export #t)
  (doc 'type '(-> (-> Bytevector (Maybe Block)) Nat (Affine Block Block)))
  (doc 'description "Compose block-ref-at with follow-ref to access a child block.")
  (affine-compose (block-ref-at n) (follow-ref fetch)))

(define (block-children-each fetch)
  (doc 'export #t)
  (doc 'type '(-> (-> Bytevector (Maybe Block)) (Traversal Block ? Block Block)))
  (doc 'description "Traverse all child blocks (following refs through CAS).
This composes block-refs-each with follow-ref.

Note: Uses a custom traversal since we need to filter out
missing refs (hashes not in CAS).")
  (make-traversal
   ;; traverse: (Block -> Block) -> Block -> Block
   ;; Cannot meaningfully update children (hash-derived identity).
   ;; Returns input unchanged.
   (lambda (_f blk) blk)
   ;; fold: Block -> List Block
   (lambda (blk)
     (filter-map
      (lambda (ref) (fetch ref))
      (vector->list (block-refs blk))))))

(define (filter-map f xs)
  (doc 'type '(-> (-> a (Maybe b)) (List a) (List b)))
  (doc 'description "Map function, keeping only non-#f results.")
  (let loop ([xs xs] [acc '()])
    (if (null? xs)
        (reverse acc)
        (let ([result (f (car xs))])
          (loop (cdr xs)
                (if result (cons result acc) acc))))))

(doc 'section 'type-prisms)

(define (block-type-prism expected-tag)
  (doc 'export #t)
  (doc 'type '(-> Symbol (Prism Block Block)))
  (doc 'description "Match blocks with a specific tag.")
  (make-prism
   ;; match: Block -> Maybe Block
   (lambda (blk)
     (if (eq? (block-tag blk) expected-tag)
         (just blk)
         nothing))
   ;; build: Block -> Block (identity, tag already matches)
   identity))

(doc block-lambda-prism 'type '(Prism Block Block))
(doc block-lambda-prism 'description "Match blocks with 'lambda tag.")
(doc block-lambda-prism 'export #t)
(define block-lambda-prism
  (block-type-prism 'lambda))

(doc block-app-prism 'type '(Prism Block Block))
(doc block-app-prism 'description "Match blocks with 'app tag.")
(doc block-app-prism 'export #t)
(define block-app-prism
  (block-type-prism 'app))

(doc block-ref-prism 'type '(Prism Block Block))
(doc block-ref-prism 'description "Match blocks with 'ref tag.")
(doc block-ref-prism 'export #t)
(define block-ref-prism
  (block-type-prism 'ref))

(doc block-literal-prism 'type '(Prism Block Block))
(doc block-literal-prism 'description "Match blocks with 'lit tag.")
(doc block-literal-prism 'export #t)
(define block-literal-prism
  (block-type-prism 'lit))

(doc block-expr-prism 'type '(Prism Block Block))
(doc block-expr-prism 'description "Match blocks with 'expr tag (stored S-expressions).")
(doc block-expr-prism 'export #t)
(define block-expr-prism
  (block-type-prism 'expr))

(doc 'section 'payload-optics)

(doc block-payload-string-iso 'type '(Iso Bytevector String))
(doc block-payload-string-iso 'description "Convert between bytevector payload and UTF-8 string.")
(doc block-payload-string-iso 'export #t)
(define block-payload-string-iso
  (make-iso
   utf8->string
   string->utf8))

(doc block-payload-as-string-lens 'type '(Lens Block String))
(doc block-payload-as-string-lens 'description "Access block payload as a string (assuming UTF-8 encoding).")
(doc block-payload-as-string-lens 'export #t)
(define block-payload-as-string-lens
  (lens-compose block-payload-lens (iso->lens block-payload-string-iso)))

(doc block-payload-sexpr-affine 'type '(Affine Block Sexp))
(doc block-payload-sexpr-affine 'description "Access block payload as an S-expression.
Returns nothing if payload cannot be parsed as valid S-expression.")
(doc block-payload-sexpr-affine 'export #t)
(define block-payload-sexpr-affine
  (make-affine
   ;; getter: Block -> Maybe Sexp
   (lambda (blk)
     (guard (ex [else nothing])
       (let* ([str (utf8->string (block-payload blk))]
              [port (open-input-string str)]
              [result (read port)])
         ;; Check result first, then verify no trailing data
         (if (eof-object? result)
             nothing  ; Empty payload
             (if (eof-object? (read port))
                 (just result)
                 nothing)))))  ; Trailing data means malformed
   ;; setter: Sexp -> Block -> Block
   (lambda (sexpr blk)
     (let ([new-payload (string->utf8 (format "~s" sexpr))])
       (make-block (block-tag blk)
                   new-payload
                   (block-refs blk))))))

(doc 'section 'composition)

(define (>>> outer inner)
  (doc 'export #t)
  (doc 'type '(-> Optic Optic Optic))
  (doc 'description "Left-to-right optic composition (like >>>).
(a >>> b) focuses first on a's target, then b within that.")
  (optic-compose outer inner))

(doc 'section 'utilities)

(define (block-has-refs? blk)
  (doc 'export #t)
  (doc 'type '(-> Block Boolean))
  (doc 'description "Check if block has any refs.")
  (> (vector-length (block-refs blk)) 0))

(define (block-is-leaf? blk)
  (doc 'export #t)
  (doc 'type '(-> Block Boolean))
  (doc 'description "Check if block is a leaf (no refs).")
  (= (vector-length (block-refs blk)) 0))

(define (block-tag-is? tag)
  (doc 'export #t)
  (doc 'type '(-> Symbol (-> Block Boolean)))
  (doc 'description "Check if block has specific tag.")
  (lambda (blk)
    (eq? (block-tag blk) tag)))

(define (collect-block-tree fetch root)
  (doc 'export #t)
  (doc 'type '(-> (-> Bytevector (Maybe Block)) Block (List Block)))
  (doc 'description "Collect all blocks reachable from a root block (DFS).
Does not include duplicates (based on refs visited).")
  (let ([visited (make-hashtable equal-hash equal?)])
    ;; DFS with accumulator passed down for O(N) performance
    (let dfs ([blk root] [acc '()])
      (let ([refs (block-refs blk)])
        (let loop ([i 0] [acc (cons blk acc)])  ; Add current block to acc
          (if (= i (vector-length refs))
              acc
              (let ([ref (vector-ref refs i)])
                (if (hashtable-ref visited ref #f)
                    ;; Already visited this ref
                    (loop (+ i 1) acc)
                    ;; New ref - mark visited and recurse with acc
                    (begin
                      (hashtable-set! visited ref #t)
                      (let ([child (fetch ref)])
                        (if child
                            (loop (+ i 1) (dfs child acc))
                            (loop (+ i 1) acc))))))))))))

(display "Loaded: lattice/fp/optics/block-optics.ss\n")
