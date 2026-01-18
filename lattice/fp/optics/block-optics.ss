;;; lattice/fp/optics/block-optics.ss — Optics for The Fold's Block System
;;;
;;; Composable optics for accessing and traversing content-addressed blocks:
;;;
;;;   - block-tag-lens     : Focus on block tag
;;;   - block-payload-lens : Focus on block payload
;;;   - block-refs-lens    : Focus on refs vector
;;;   - block-ref-at       : Affine for specific ref by index
;;;   - block-refs-each    : Traversal over all refs
;;;   - follow-ref         : Affine that follows a ref through a CAS
;;;
;;; This is pure lattice code: no CAS mutation, just accessors.
;;;
;;; Dependencies:
;;;   - core/blocks/block.ss (for block record type)
;;;   - lattice/fp/optics/optics.ss (for optics tower)

(load "core/blocks/block.ss")
(load "lattice/fp/optics/optics.ss")

;;; ============================================================
;;; Part 1: Basic Block Lenses
;;; ============================================================
;;;
;;; These lenses focus on the three fundamental components of a block:
;;;   - tag: Symbol identifying block type
;;;   - payload: Raw bytevector data
;;;   - refs: Vector of hash references

;;; block-tag-lens : Lens Block Symbol
;;; Focus on the tag of a block.
;;;
;;; Note: Modifying a block creates a new block (immutable record).
;;; The new block will have a different hash.
(define block-tag-lens
  (make-lens
   block-tag
   (lambda (new-tag blk)
     (make-block new-tag
                 (block-payload blk)
                 (block-refs blk)))))

;;; block-payload-lens : Lens Block Bytevector
;;; Focus on the payload of a block.
(define block-payload-lens
  (make-lens
   block-payload
   (lambda (new-payload blk)
     (make-block (block-tag blk)
                 new-payload
                 (block-refs blk)))))

;;; block-refs-lens : Lens Block (Vector Bytevector)
;;; Focus on the refs vector of a block.
(define block-refs-lens
  (make-lens
   block-refs
   (lambda (new-refs blk)
     (make-block (block-tag blk)
                 (block-payload blk)
                 new-refs))))

;;; ============================================================
;;; Part 2: Ref Access (Affines and Traversals)
;;; ============================================================
;;;
;;; Affines for partial access (ref may not exist at index).
;;; Traversals for iterating over all refs.

;;; block-ref-at : Nat -> Affine Block Bytevector
;;; Focus on a specific ref by index.
;;; Returns nothing if index is out of bounds.
(define (block-ref-at n)
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

;;; block-refs-each : Traversal Block Block Bytevector Bytevector
;;; Traverse over all refs in a block.
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

;;; block-refs-count : Block -> Nat
;;; Helper: count number of refs in a block.
(define (block-refs-count blk)
  (vector-length (block-refs blk)))

;;; ============================================================
;;; Part 3: CAS-Aware Optics (Reference Following)
;;; ============================================================
;;;
;;; These optics use a CAS (Content-Addressed Store) to follow
;;; references and access the blocks they point to.

;;; follow-ref : (Bytevector -> Block | #f) -> Affine Bytevector Block
;;; Create an affine that follows a ref (hash) to its block.
;;; The fetch function should be like CAS's fetch: hash -> Block | #f.
;;;
;;; Note: Setting through this affine is meaningless since hashes
;;; are derived from content. We return the ref unchanged.
(define (follow-ref fetch)
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

;;; block-child-at : (Bytevector -> Block | #f) -> Nat -> Affine Block Block
;;; Compose block-ref-at with follow-ref to access a child block.
(define (block-child-at fetch n)
  (affine-compose (block-ref-at n) (follow-ref fetch)))

;;; block-children-each : (Bytevector -> Block | #f) -> Traversal Block ? Block Block
;;; Traverse all child blocks (following refs through CAS).
;;; This composes block-refs-each with follow-ref.
;;;
;;; Note: Uses a custom traversal since we need to filter out
;;; missing refs (hashes not in CAS).
(define (block-children-each fetch)
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

;;; filter-map : (a -> b | #f) -> List a -> List b
;;; Map function, keeping only non-#f results.
(define (filter-map f xs)
  (let loop ([xs xs] [acc '()])
    (if (null? xs)
        (reverse acc)
        (let ([result (f (car xs))])
          (loop (cdr xs)
                (if result (cons result acc) acc))))))

;;; ============================================================
;;; Part 4: Block Type Prisms
;;; ============================================================
;;;
;;; Prisms for matching specific block types by tag.
;;; These let you work with blocks of a particular kind.

;;; block-type-prism : Symbol -> Prism Block Block
;;; Match blocks with a specific tag.
(define (block-type-prism expected-tag)
  (make-prism
   ;; match: Block -> Maybe Block
   (lambda (blk)
     (if (eq? (block-tag blk) expected-tag)
         (just blk)
         nothing))
   ;; build: Block -> Block (identity, tag already matches)
   identity))

;;; Common block type prisms based on typical Fold block types:

;;; block-lambda-prism : Prism Block Block
;;; Match blocks with 'lambda tag.
(define block-lambda-prism
  (block-type-prism 'lambda))

;;; block-app-prism : Prism Block Block
;;; Match blocks with 'app tag.
(define block-app-prism
  (block-type-prism 'app))

;;; block-ref-prism : Prism Block Block
;;; Match blocks with 'ref tag.
(define block-ref-prism
  (block-type-prism 'ref))

;;; block-literal-prism : Prism Block Block
;;; Match blocks with 'lit tag.
(define block-literal-prism
  (block-type-prism 'lit))

;;; block-expr-prism : Prism Block Block
;;; Match blocks with 'expr tag (stored S-expressions).
(define block-expr-prism
  (block-type-prism 'expr))

;;; ============================================================
;;; Part 5: Payload Optics
;;; ============================================================
;;;
;;; Optics for working with block payloads in different formats.

;;; block-payload-string-iso : Iso Bytevector String
;;; Convert between bytevector payload and UTF-8 string.
(define block-payload-string-iso
  (make-iso
   utf8->string
   string->utf8))

;;; block-payload-as-string-lens : Lens Block String
;;; Access block payload as a string (assuming UTF-8 encoding).
(define block-payload-as-string-lens
  (lens-compose block-payload-lens (iso->lens block-payload-string-iso)))

;;; block-payload-sexpr-affine : Affine Block Sexp
;;; Access block payload as an S-expression.
;;; Returns nothing if payload cannot be parsed as valid S-expression.
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

;;; ============================================================
;;; Part 6: Composition Operators
;;; ============================================================
;;;
;;; Convenient composition for block optics.

;;; >>> : Optic -> Optic -> Optic
;;; Left-to-right optic composition (like >>>).
;;; (a >>> b) focuses first on a's target, then b within that.
(define (>>> outer inner)
  (optic-compose outer inner))

;;; ============================================================
;;; Part 7: Utility Functions
;;; ============================================================

;;; block-has-refs? : Block -> Boolean
;;; Check if block has any refs.
(define (block-has-refs? blk)
  (> (vector-length (block-refs blk)) 0))

;;; block-is-leaf? : Block -> Boolean
;;; Check if block is a leaf (no refs).
(define (block-is-leaf? blk)
  (= (vector-length (block-refs blk)) 0))

;;; block-tag-is? : Symbol -> Block -> Boolean
;;; Check if block has specific tag.
(define (block-tag-is? tag)
  (lambda (blk)
    (eq? (block-tag blk) tag)))

;;; collect-block-tree : (Bytevector -> Block | #f) -> Block -> List Block
;;; Collect all blocks reachable from a root block (DFS).
;;; Does not include duplicates (based on refs visited).
(define (collect-block-tree fetch root)
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

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Basic Lenses:
;;;   block-tag-lens, block-payload-lens, block-refs-lens
;;;
;;; Ref Access:
;;;   block-ref-at, block-refs-each, block-refs-count
;;;
;;; CAS-Aware:
;;;   follow-ref, block-child-at, block-children-each
;;;
;;; Type Prisms:
;;;   block-type-prism
;;;   block-lambda-prism, block-app-prism, block-ref-prism
;;;   block-literal-prism, block-expr-prism
;;;
;;; Payload Optics:
;;;   block-payload-string-iso, block-payload-as-string-lens
;;;   block-payload-sexpr-affine
;;;
;;; Composition:
;;;   >>>
;;;
;;; Utilities:
;;;   block-has-refs?, block-is-leaf?, block-tag-is?
;;;   collect-block-tree
