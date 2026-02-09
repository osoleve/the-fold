(load "core/blocks/cas.ss")
(load "lattice/optics/block-migration.ss")

(doc 'module 'migrations/runner)
(doc 'description "Execute migrations against the content-addressed store")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/blocks/cas lattice/optics/block-migration))

(doc 'note "Handles single block migration, tree migration (bottom-up), memoization for DAG efficiency, and transaction-like semantics")
(doc 'note "Key insight: In a Merkle DAG, if a child block changes, its hash changes. Parents must update their refs to new child hashes. We use bottom-up (post-order) traversal: 1) Migrate children first, 2) Update parent refs to new child hashes, 3) Migrate parent payload, 4) Store parent with updated refs")

(doc 'section 'single-block-migration)

(define (run-migration! bm block-hash)
  (doc 'type (-> BlockMigration Bytevector (Result Bytevector String)))
  (doc 'description "Execute a migration on a single block, returns new hash on success")
  (doc 'export #t)
  (let ([blk (fetch block-hash)])
    (if (not blk)
        (list 'err (format "Block not found: ~a" (hash->hex block-hash)))
        (guard (ex [else (list 'err (format "Migration failed: ~a" ex))])
          (if (not (block-migration-applies? bm blk))
              (list 'ok block-hash)
              (let* ([migrated (block-migrate-payload bm blk)]
                     [new-hash (store! migrated)])
                (list 'ok new-hash)))))))

(define (run-rollback! bm block-hash)
  (doc 'type (-> BlockMigration Bytevector (Result Bytevector String)))
  (doc 'description "Execute a rollback on a single block")
  (doc 'export #t)
  (let ([blk (fetch block-hash)])
    (if (not blk)
        (list 'err (format "Block not found: ~a" (hash->hex block-hash)))
        (guard (ex [else (list 'err (format "Rollback failed: ~a" ex))])
          (let* ([rolled-back (block-rollback-payload bm blk)]
                 [new-hash (store! rolled-back)])
            (list 'ok new-hash))))))

(doc 'section 'tree-migration)
(doc 'note "For Merkle DAG correctness: 1) Traverse in post-order (children before parents), 2) Memoize to handle shared subtrees (DAGs, not just trees), 3) Update parent refs to point to new child hashes")

(define (migrate-tree! bm root-hash)
  (doc 'type (-> BlockMigration Bytevector (Result Bytevector String)))
  (doc 'description "Recursively migrate a block tree bottom-up, returns new root hash on success")
  (doc 'export #t)
  (let ([visited (make-hashtable equal-hash equal?)])
    (migrate-tree-impl! bm root-hash visited)))

(define (migrate-tree-impl! bm hash visited)
  (doc 'type (-> BlockMigration Bytevector Hashtable Bytevector))
  (doc 'description "Internal implementation with memoization table")
  ;; Check memoization first
  (let ([cached (hashtable-ref visited hash #f)])
    (if cached
        cached  ; Already migrated this subtree
        ;; Fetch and process
        (let ([blk (fetch hash)])
          (if (not blk)
              hash  ; Missing block - return original hash
              (let* ([refs (block-refs blk)]
                     [ref-count (vector-length refs)])
                ;; Step 1: Recursively migrate all children (post-order)
                (let* ([new-refs (make-vector ref-count)]
                       [_ (do ([i 0 (+ i 1)])
                              ((= i ref-count))
                            (vector-set! new-refs i
                                         (migrate-tree-impl! bm
                                                             (vector-ref refs i)
                                                             visited)))])
                  ;; Step 2: Migrate this block with updated refs
                  (let* ([migrated (block-migrate-with-refs bm blk new-refs)]
                         [new-hash (store! migrated)])
                    ;; Memoize and return
                    (hashtable-set! visited hash new-hash)
                    new-hash))))))))

(define (rollback-tree! bm root-hash)
  (doc 'type (-> BlockMigration Bytevector (Result Bytevector String)))
  (doc 'description "Recursively rollback a block tree bottom-up")
  (doc 'export #t)
  (let ([visited (make-hashtable equal-hash equal?)])
    (rollback-tree-impl! bm root-hash visited)))

(define (rollback-tree-impl! bm hash visited)
  (doc 'type (-> BlockMigration Bytevector Hashtable Bytevector))
  (doc 'description "Internal rollback implementation with memoization")
  (let ([cached (hashtable-ref visited hash #f)])
    (if cached
        cached
        (let ([blk (fetch hash)])
          (if (not blk)
              hash
              (let* ([refs (block-refs blk)]
                     [ref-count (vector-length refs)]
                     [new-refs (make-vector ref-count)])
                ;; Recursively rollback children
                (do ([i 0 (+ i 1)])
                    ((= i ref-count))
                  (vector-set! new-refs i
                               (rollback-tree-impl! bm
                                                    (vector-ref refs i)
                                                    visited)))
                ;; Rollback this block with updated refs
                (let* ([from-tag (block-migration-from-tag bm)]
                       [to-tag (block-migration-to-tag bm)]
                       [payload-iso (block-migration-payload-iso bm)]
                       [rolled-back
                        (if (not (eq? (block-tag blk) to-tag))
                            ;; Not matching tag - just update refs
                            (make-block (block-tag blk)
                                        (block-payload blk)
                                        new-refs)
                            ;; Matching tag - rollback payload and refs
                            (let* ([payload (block-payload blk)]
                                   [sexpr (bytevector->sexpr payload)]
                                   [new-sexpr ((p-iso-backward payload-iso) sexpr)]
                                   [new-payload (sexpr->bytevector new-sexpr)])
                              (make-block from-tag new-payload new-refs)))]
                       [new-hash (store! rolled-back)])
                  (hashtable-set! visited hash new-hash)
                  new-hash)))))))

(doc 'section 'batch-migration)

(define (migrate-many! bm hashes)
  (doc 'type (-> BlockMigration (List Bytevector) (List (Pair Bytevector Bytevector))))
  (doc 'description "Migrate multiple roots, returning mapping of old->new hashes")
  (doc 'export #t)
  (let ([visited (make-hashtable equal-hash equal?)])
    (map (lambda (h)
           (let ([new-h (migrate-tree-impl! bm h visited)])
             (cons h new-h)))
         hashes)))

(doc 'section 'migration-statistics)

(define (migration-stats bm root-hash)
  (doc 'type (-> BlockMigration Bytevector Alist))
  (doc 'description "Gather statistics about a potential migration")
  (doc 'export #t)
  (let ([visited (make-hashtable equal-hash equal?)]
        [total 0]
        [matching 0]
        [non-matching 0])
    ;; Traverse tree counting blocks
    (let traverse ([hash root-hash])
      (unless (hashtable-ref visited hash #f)
        (hashtable-set! visited hash #t)
        (let ([blk (fetch hash)])
          (when blk
            (set! total (+ total 1))
            (if (block-migration-applies? bm blk)
                (set! matching (+ matching 1))
                (set! non-matching (+ non-matching 1)))
            ;; Recurse on refs
            (vector-for-each traverse (block-refs blk))))))
    `((total . ,total)
      (matching . ,matching)
      (non-matching . ,non-matching)
      (would-migrate . ,matching))))

(doc 'section 'dry-run)

(define (migrate-dry-run bm root-hash)
  (doc 'type (-> BlockMigration Bytevector (List Block)))
  (doc 'description "Preview migration without storing anything, returns list of blocks that would be created")
  (doc 'export #t)
  (let ([visited (make-hashtable equal-hash equal?)]
        [results '()])
    (let traverse ([hash root-hash])
      (unless (hashtable-ref visited hash #f)
        (hashtable-set! visited hash #t)
        (let ([blk (fetch hash)])
          (when blk
            ;; Would this block be migrated?
            (when (block-migration-applies? bm blk)
              (let ([migrated (block-migrate-payload bm blk)])
                (set! results (cons migrated results))))
            ;; Recurse on refs
            (vector-for-each traverse (block-refs blk))))))
    (reverse results)))

(doc 'section 'head-update)

(define (update-head! head-name new-hash)
  (doc 'type (-> Symbol Bytevector Void))
  (doc 'description "Update a head file to point to new hash")
  (doc 'export #t)
  (let* ([head-path (format ".store/heads/bbs/~a.head" head-name)]
         [hex (hash->hex new-hash)])
    (call-with-output-file head-path
      (lambda (port)
        (display hex port)
        (newline port)))))

(define (read-head head-name)
  (doc 'type (-> Symbol (Maybe Bytevector)))
  (doc 'description "Read current hash from a head file")
  (doc 'export #t)
  (let ([head-path (format ".store/heads/bbs/~a.head" head-name)])
    (guard (ex [else #f])
      (call-with-input-file head-path
        (lambda (port)
          (let ([hex (get-line port)])
            (if (eof-object? hex)
                #f
                (hex->hash (string-trim hex)))))))))

(define (string-trim s)
  (doc 'type (-> String String))
  (doc 'description "Remove leading/trailing whitespace")
  (let* ([chars (string->list s)]
         [trimmed (let loop ([cs chars])
                    (if (and (pair? cs) (char-whitespace? (car cs)))
                        (loop (cdr cs))
                        cs))]
         [reversed (let loop ([cs (reverse trimmed)])
                     (if (and (pair? cs) (char-whitespace? (car cs)))
                         (loop (cdr cs))
                         cs))])
    (list->string (reverse reversed))))

(doc 'section 'migration-with-head-update)

(define (migrate-head! bm head-name)
  (doc 'type (-> BlockMigration Symbol (Result Bytevector String)))
  (doc 'description "Migrate an issue/post by its head name, updating the head file")
  (doc 'export #t)
  (let ([current-hash (read-head head-name)])
    (if (not current-hash)
        (list 'err (format "Head not found: ~a" head-name))
        (let ([new-hash (migrate-tree! bm current-hash)])
          (if (equal? new-hash current-hash)
              (list 'ok current-hash)  ; No change
              (begin
                (update-head! head-name new-hash)
                (list 'ok new-hash)))))))

(doc 'section 'exports)
(doc 'note "Single Block: run-migration!, run-rollback!")
(doc 'note "Tree Migration: migrate-tree!, rollback-tree!")
(doc 'note "Batch: migrate-many!")
(doc 'note "Statistics/Preview: migration-stats, migrate-dry-run")
(doc 'note "Head Management: update-head!, read-head, migrate-head!")

(display "Loaded: boundary/migrations/runner.ss\n")
