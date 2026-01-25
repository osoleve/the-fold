(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "core/blocks/normalize.ss")

(doc 'module 'cas)
(doc 'description "Content-Addressed Store. Every Block has a cryptographic hash that IS its identity.")
(doc 'layer 'core)

(doc 'section 'hashing)

(doc 'section 'address-versions)

(doc address-version-alpha 'type Byte)
(doc address-version-alpha 'description "Version 0x00: α-normalization only (de Bruijn indices).")
(doc address-version-alpha 'export #t)
(define address-version-alpha #x00)

(doc address-version-algebraic 'type Byte)
(doc address-version-algebraic 'description "Version 0x01: algebraic + α-normalization with commutative sorting and flattening.")
(doc address-version-algebraic 'export #t)
(define address-version-algebraic #x01)

(doc address-version-v2 'type Byte)
(doc address-version-v2 'description "Version 0x02: enhanced normalization with η-reduction, identity elimination, poly-canon, hash-consing.")
(doc address-version-v2 'export #t)
(define address-version-v2 #x02)

(doc address-version-v3 'type Byte)
(doc address-version-v3 'description "Version 0x03: NbE-enhanced normalization with β-reduction, projections, and semantic equivalence.")
(doc address-version-v3 'export #t)
(define address-version-v3 #x03)

(doc 'section 'blake2b-address-versions)
(doc 'note "BLAKE2b variants use 0x10-0x13, parallel to SHA-256 0x00-0x03. ~5x faster for large blocks.")

(doc address-version-blake2b-alpha 'type Byte)
(doc address-version-blake2b-alpha 'description "Version 0x10: BLAKE2b with α-normalization only.")
(doc address-version-blake2b-alpha 'export #t)
(define address-version-blake2b-alpha #x10)

(doc address-version-blake2b-algebraic 'type Byte)
(doc address-version-blake2b-algebraic 'description "Version 0x11: BLAKE2b with algebraic + α-normalization.")
(doc address-version-blake2b-algebraic 'export #t)
(define address-version-blake2b-algebraic #x11)

(doc address-version-blake2b-v2 'type Byte)
(doc address-version-blake2b-v2 'description "Version 0x12: BLAKE2b with v2 normalization.")
(doc address-version-blake2b-v2 'export #t)
(define address-version-blake2b-v2 #x12)

(doc address-version-blake2b-v3 'type Byte)
(doc address-version-blake2b-v3 'description "Version 0x13: BLAKE2b with v3 normalization.")
(doc address-version-blake2b-v3 'export #t)
(define address-version-blake2b-v3 #x13)

(doc 'section 'hashing-functions)

(define (hash-block blk)
  (doc 'type (-> Block Bytevector))
  (doc 'description "Compute versioned address of block (v0x00). No normalization applied.")
  (doc 'export #t)
  (let* ([hash (sha256 (block->bytes blk))]
         [address (make-bytevector address-size)])
        (bytevector-u8-set! address 0 address-version)
        (bytevector-copy! hash 0 address 1 hash-size)
        address))

(define (hash-sexpr tag sexpr)
  (doc 'type (-> Symbol Any Bytevector))
  (doc 'description "Hash S-expression with α-normalization only (v0x00). De Bruijn conversion for α-equivalence.")
  (doc 'export #t)
  (let* ([normalized (normalize sexpr)]
         [payload (string->utf8 (format "~s" normalized))]
         [blk (make-block tag payload empty-refs)])
    (hash-block blk)))

(define (hash-sexpr-algebraic tag sexpr)
  (doc 'type (-> Symbol Any Bytevector))
  (doc 'description "Hash S-expression with algebraic normalization (v0x01). Commutative sorting and flattening before α-norm.")
  (doc 'export #t)
  (let* ([normalized (normalize-full sexpr)]
         [payload (string->utf8 (format "~s" normalized))]
         [blk (make-block tag payload empty-refs)]
         [hash (sha256 (block->bytes blk))]
         [address (make-bytevector address-size)])
    (bytevector-u8-set! address 0 address-version-algebraic)
    (bytevector-copy! hash 0 address 1 hash-size)
    address))

(define (hash-sexpr-v2 tag sexpr)
  (doc 'type (-> Symbol Any Bytevector))
  (doc 'description "Hash S-expression with v2 normalization (v0x02). η-reduction, identity elim, poly-canon, hash-consing.")
  (doc 'export #t)
  (let* ([normalized (normalize-v2 sexpr)]
         [payload (string->utf8 (format "~s" normalized))]
         [blk (make-block tag payload empty-refs)]
         [hash (sha256 (block->bytes blk))]
         [address (make-bytevector address-size)])
    (bytevector-u8-set! address 0 address-version-v2)
    (bytevector-copy! hash 0 address 1 hash-size)
    address))

(define (hash-sexpr-v3 tag sexpr)
  (doc 'type (-> Symbol Any Bytevector))
  (doc 'description "Hash S-expression with v3 normalization (v0x03). NbE with β-reduction, projections, and all v2 transforms.")
  (doc 'export #t)
  (let* ([normalized (normalize-v3 sexpr)]
         [payload (string->utf8 (format "~s" normalized))]
         [blk (make-block tag payload empty-refs)]
         [hash (sha256 (block->bytes blk))]
         [address (make-bytevector address-size)])
    (bytevector-u8-set! address 0 address-version-v3)
    (bytevector-copy! hash 0 address 1 hash-size)
    address))

(define (address-version-byte addr)
  (doc 'type (-> Bytevector Byte))
  (doc 'description "Extract the version byte from an address.")
  (doc 'export #t)
  (bytevector-u8-ref addr 0))

(define (address-algebraic? addr)
  (doc 'type (-> Bytevector Boolean))
  (doc 'description "Check if address was computed with algebraic normalization (v0x01).")
  (doc 'export #t)
  (= (address-version-byte addr) address-version-algebraic))

(define (address-v2? addr)
  (doc 'type (-> Bytevector Boolean))
  (doc 'description "Check if address was computed with v2 normalization (v0x02).")
  (doc 'export #t)
  (= (address-version-byte addr) address-version-v2))

(define (address-v3? addr)
  (doc 'type (-> Bytevector Boolean))
  (doc 'description "Check if address was computed with v3 normalization (v0x03).")
  (doc 'export #t)
  (= (address-version-byte addr) address-version-v3))

(define (address-blake2b? addr)
  (doc 'type (-> Bytevector Boolean))
  (doc 'description "Check if address was computed with BLAKE2b (v0x10-0x13).")
  (doc 'export #t)
  (let ([v (address-version-byte addr)])
    (and (>= v #x10) (<= v #x13))))

(define (address-sha256? addr)
  (doc 'type (-> Bytevector Boolean))
  (doc 'description "Check if address was computed with SHA-256 (v0x00-0x03).")
  (doc 'export #t)
  (let ([v (address-version-byte addr)])
    (and (>= v #x00) (<= v #x03))))

(define (hash->hex hash)
  (doc 'type (-> Bytevector String))
  (doc 'description "Convert address bytes to hexadecimal string for display.")
  (doc 'export #t)
  (let ([hex-chars "0123456789abcdef"])
       (apply string-append
              (map (lambda (i)
                           (let ([b (bytevector-u8-ref hash i)])
                                (string
                                 (string-ref hex-chars (quotient b 16))
                                 (string-ref hex-chars (modulo b 16)))))
                   (iota (bytevector-length hash))))))

(define (hex->hash str)
  (doc 'type (-> String Bytevector))
  (doc 'description "Convert hexadecimal string to address bytes.")
  (doc 'export #t)
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
  (doc 'type (-> Char Nat))
  (doc 'description "Convert hexadecimal character to numeric value (0-15).")
  (doc 'export #t)
  (cond
   [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
   [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
   [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
   [else (error 'char->hex-digit "invalid hex character" c)]))

(doc 'section 'in-memory-store)

(doc *store* 'type (Hashtable Bytevector Block))
(doc *store* 'description "In-memory store hashtable. NOT thread-safe.")
(define *store* (make-hashtable equal-hash equal?))

(doc *pinned* 'type (Hashtable Bytevector Boolean))
(doc *pinned* 'description "Pinned hashes preserved during garbage collection.")
(define *pinned* (make-hashtable equal-hash equal?))

(define (store! blk)
  (doc 'type (-> Block Bytevector))
  (doc 'description "Store a block and return its hash.")
  (doc 'export #t)
  (let ([hash (hash-block blk)])
       (hashtable-set! *store* hash blk)
       hash))

(define (fetch hash)
  (doc 'type (-> Bytevector (Maybe Block)))
  (doc 'description "Retrieve a block by its hash, or #f if not found.")
  (doc 'export #t)
  (hashtable-ref *store* hash #f))

(define (stored? hash)
  (doc 'type (-> Bytevector Boolean))
  (doc 'description "Check if a block with this hash exists.")
  (doc 'export #t)
  (hashtable-contains? *store* hash))

(define (pin! hash)
  (doc 'type (-> Bytevector Void))
  (doc 'description "Mark a hash as pinned (should not be garbage collected).")
  (doc 'export #t)
  (hashtable-set! *pinned* hash #t))

(define (pinned? hash)
  (doc 'type (-> Bytevector Boolean))
  (doc 'description "Check if a hash is currently pinned.")
  (doc 'export #t)
  (hashtable-ref *pinned* hash #f))

(define (unpin! hash)
  (doc 'type (-> Bytevector Void))
  (doc 'description "Remove pin from a hash.")
  (doc 'export #t)
  (hashtable-delete! *pinned* hash))

(doc 'section 'tree-operations)

(define (collect-refs hash fetch)
  (doc 'type (-> Bytevector (-> Bytevector Block) (List Bytevector)))
  (doc 'description "Collect all transitive references from a block. Iterative traversal.")
  (doc 'export #t)
  (let ([visited (make-hashtable equal-hash equal?)]
        [queue (list hash)]
        [results '()])
       (let loop ()
            (if (null? queue)
                results
                (let ([current (car queue)]
                      [rest-queue (cdr queue)])
                     (if (hashtable-ref visited current #f)
                         ;; Already visited, skip
                         (begin
                          (set! queue rest-queue)
                          (loop))
                         ;; New node: mark visited, add to results
                         (begin
                          (hashtable-set! visited current #t)
                          (set! results (cons current results))
                          (let ([blk (fetch current)])
                               (if blk
                                   ;; Prepend new refs to queue
                                   (let ([new-refs (filter
                                                    (lambda (r) (not (hashtable-ref visited r #f)))
                                                    (vector->list (block-refs blk)))])
                                        (set! queue (append new-refs rest-queue)))
                                   (set! queue rest-queue)))
                          (loop))))))))

(define (pin-tree! hash)
  (doc 'type (-> Bytevector Nat))
  (doc 'description "Pin a hash and all its transitive references. Returns number of blocks pinned.")
  (doc 'export #t)
  (let* ([refs (collect-refs hash fetch)]
         [count 0])
        (for-each
         (lambda (h)
                 (unless (pinned? h)
                         (pin! h)
                         (set! count (+ count 1))))
         refs)
        count))

(define (unpin-tree! hash)
  (doc 'type (-> Bytevector Nat))
  (doc 'description "Unpin a hash and all its transitive references. Returns number of blocks unpinned.")
  (doc 'export #t)
  (let* ([refs (collect-refs hash fetch)]
         [count 0])
        (for-each
         (lambda (h)
                 (when (pinned? h)
                       (unpin! h)
                       (set! count (+ count 1))))
         refs)
        count))

(doc 'section 'garbage-collection)

(define (gc!)
  (doc 'type (-> (Values Nat Nat)))
  (doc 'description "Remove all unpinned blocks from store. Returns (collected-count remaining-count).")
  (doc 'export #t)
  (let ([to-remove '()]
        [initial-count (store-count)])
       ;; Collect unpinned hashes
       (vector-for-each
        (lambda (hash)
                (unless (pinned? hash)
                        (set! to-remove (cons hash to-remove))))
        (hashtable-keys *store*))
       ;; Remove them
       (for-each
        (lambda (hash)
                (hashtable-delete! *store* hash))
        to-remove)
       (values (length to-remove) (store-count))))

(define (gc-with-roots! roots)
  (doc 'type (-> (List Bytevector) (Values Nat Nat)))
  (doc 'description "Collect blocks not reachable from root hashes. Returns (collected-count remaining-count).")
  (doc 'export #t)
  ;; Save current pins
  (let ([saved-pins (make-hashtable equal-hash equal?)])
       (vector-for-each
        (lambda (h)
                (hashtable-set! saved-pins h #t))
        (hashtable-keys *pinned*))
       
       ;; Clear all pins
       (hashtable-clear! *pinned*)
       
       ;; Pin from roots
       (for-each
        (lambda (root)
                (when (stored? root)
                      (pin-tree! root)))
        roots)
       
       ;; Run GC
       (let-values ([(collected remaining) (gc!)])
                   ;; Restore original pins for remaining blocks
                   (vector-for-each
                    (lambda (h)
                            (when (hashtable-ref saved-pins h #f)
                                  (pin! h)))
                    (hashtable-keys *store*))
                   (values collected remaining))))

(define (gc-stats)
  (doc 'type (-> Alist))
  (doc 'description "Return statistics about pinned vs unpinned blocks.")
  (doc 'export #t)
  (let ([total 0]
        [pinned-count 0]
        [unpinned-count 0])
       (vector-for-each
        (lambda (hash)
                (set! total (+ total 1))
                (if (pinned? hash)
                    (set! pinned-count (+ pinned-count 1))
                    (set! unpinned-count (+ unpinned-count 1))))
        (hashtable-keys *store*))
       `((total . ,total)
         (pinned . ,pinned-count)
         (unpinned . ,unpinned-count)
         (gc-would-collect . ,unpinned-count))))

(doc 'section 'store-statistics)

(define (store-count)
  (doc 'type (-> Nat))
  (doc 'description "Number of blocks in the store.")
  (doc 'export #t)
  (hashtable-size *store*))

(define (store-hashes)
  (doc 'type (-> (List Bytevector)))
  (doc 'description "All hashes in the store.")
  (doc 'export #t)
  (vector->list (hashtable-keys *store*)))

(doc 'section 'convenience)

(define (store-sexpr! tag sexpr)
  (doc 'type (-> Symbol Any Bytevector))
  (doc 'description "Store an S-expression as a block with given tag.")
  (doc 'export #t)
  (let* ([payload (string->utf8 (format "~s" sexpr))]
         [blk (make-block tag payload empty-refs)])
        (store! blk)))

(define (fetch-sexpr hash)
  (doc 'type (-> Bytevector (Maybe Any)))
  (doc 'description "Retrieve and parse S-expression block. Uses 'read' - only for trusted data.")
  (doc 'export #t)
  (let ([blk (fetch hash)])
       (if blk
           (guard (ex [else #f])  ; Return #f on parse error
                  (let* ([str (utf8->string (block-payload blk))]
                         [port (open-input-string str)]
                         [result (read port)])
                        ;; Verify we consumed all input (no trailing garbage)
                        (if (eof-object? (read port))
                            result
                            #f)))  ; Trailing data = malformed
           #f)))
