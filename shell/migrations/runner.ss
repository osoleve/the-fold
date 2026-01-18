;;; shell/migrations/runner.ss — Execute Migrations Against CAS
;;;
;;; The effectful layer for running migrations on the content-addressed store.
;;; Handles:
;;;
;;;   - Single block migration with CAS storage
;;;   - Tree migration (bottom-up for Merkle correctness)
;;;   - Memoization for DAG efficiency
;;;   - Transaction-like semantics (all-or-nothing)
;;;
;;; Key insight: In a Merkle DAG, if a child block changes, its hash
;;; changes. Parents must update their refs to new child hashes.
;;; We use **bottom-up (post-order) traversal**:
;;;   1. Migrate children first
;;;   2. Update parent refs to new child hashes
;;;   3. Migrate parent payload
;;;   4. Store parent with updated refs
;;;
;;; This is Shell code: effectful, handles CAS I/O.
;;;
;;; Dependencies:
;;;   - core/blocks/cas.ss (for store!/fetch)
;;;   - lattice/fp/optics/block-migration.ss (for block transformations)

(load "core/blocks/cas.ss")
(load "lattice/fp/optics/block-migration.ss")

;;; ============================================================
;;; Part 1: Single Block Migration
;;; ============================================================

;;; run-migration! : BlockMigration -> Bytevector -> (ok Bytevector) | (err String)
;;; Execute a migration on a single block.
;;; Returns the new hash on success.
(define (run-migration! bm block-hash)
  (let ([blk (fetch block-hash)])
    (if (not blk)
        (list 'err (format "Block not found: ~a" (hash->hex block-hash)))
        (guard (ex [else (list 'err (format "Migration failed: ~a" ex))])
          (if (not (block-migration-applies? bm blk))
              ;; Migration doesn't apply - return original hash
              (list 'ok block-hash)
              ;; Apply migration and store result
              (let* ([migrated (block-migrate-payload bm blk)]
                     [new-hash (store! migrated)])
                (list 'ok new-hash)))))))

;;; run-rollback! : BlockMigration -> Bytevector -> (ok Bytevector) | (err String)
;;; Execute a rollback on a single block.
(define (run-rollback! bm block-hash)
  (let ([blk (fetch block-hash)])
    (if (not blk)
        (list 'err (format "Block not found: ~a" (hash->hex block-hash)))
        (guard (ex [else (list 'err (format "Rollback failed: ~a" ex))])
          (let* ([rolled-back (block-rollback-payload bm blk)]
                 [new-hash (store! rolled-back)])
            (list 'ok new-hash))))))

;;; ============================================================
;;; Part 2: Tree Migration (Bottom-Up)
;;; ============================================================
;;;
;;; For Merkle DAG correctness, we must:
;;;   1. Traverse in post-order (children before parents)
;;;   2. Memoize to handle shared subtrees (DAGs, not just trees)
;;;   3. Update parent refs to point to new child hashes

;;; migrate-tree! : BlockMigration -> Bytevector -> (ok Bytevector) | (err String)
;;; Recursively migrate a block tree, bottom-up.
;;; Returns the new root hash on success.
(define (migrate-tree! bm root-hash)
  (let ([visited (make-hashtable equal-hash equal?)])
    (migrate-tree-impl! bm root-hash visited)))

;;; migrate-tree-impl! : BlockMigration -> Bytevector -> Hashtable -> Bytevector
;;; Internal implementation with memoization table.
(define (migrate-tree-impl! bm hash visited)
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

;;; rollback-tree! : BlockMigration -> Bytevector -> (ok Bytevector) | (err String)
;;; Recursively rollback a block tree, bottom-up.
(define (rollback-tree! bm root-hash)
  (let ([visited (make-hashtable equal-hash equal?)])
    (rollback-tree-impl! bm root-hash visited)))

;;; rollback-tree-impl! : BlockMigration -> Bytevector -> Hashtable -> Bytevector
(define (rollback-tree-impl! bm hash visited)
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

;;; ============================================================
;;; Part 3: Batch Migration
;;; ============================================================

;;; migrate-many! : BlockMigration -> (List Bytevector) -> (List (Pair Bytevector Bytevector))
;;; Migrate multiple roots, returning mapping of old->new hashes.
(define (migrate-many! bm hashes)
  (let ([visited (make-hashtable equal-hash equal?)])
    (map (lambda (h)
           (let ([new-h (migrate-tree-impl! bm h visited)])
             (cons h new-h)))
         hashes)))

;;; ============================================================
;;; Part 4: Migration Statistics
;;; ============================================================

;;; migration-stats : Bytevector -> Alist
;;; Gather statistics about a potential migration.
(define (migration-stats bm root-hash)
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

;;; ============================================================
;;; Part 5: Dry Run
;;; ============================================================

;;; migrate-dry-run : BlockMigration -> Bytevector -> (List Block)
;;; Preview migration without storing anything.
;;; Returns list of blocks that would be created.
(define (migrate-dry-run bm root-hash)
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

;;; ============================================================
;;; Part 6: Head Update (for issue tracking)
;;; ============================================================

;;; update-head! : Symbol -> Bytevector -> Void
;;; Update a head file to point to new hash.
(define (update-head! head-name new-hash)
  (let* ([head-path (format ".store/heads/bbs/~a.head" head-name)]
         [hex (hash->hex new-hash)])
    (call-with-output-file head-path
      (lambda (port)
        (display hex port)
        (newline port)))))

;;; read-head : Symbol -> Bytevector | #f
;;; Read current hash from a head file.
(define (read-head head-name)
  (let ([head-path (format ".store/heads/bbs/~a.head" head-name)])
    (guard (ex [else #f])
      (call-with-input-file head-path
        (lambda (port)
          (let ([hex (get-line port)])
            (if (eof-object? hex)
                #f
                (hex->hash (string-trim hex)))))))))

;;; string-trim : String -> String
;;; Remove leading/trailing whitespace.
(define (string-trim s)
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

;;; ============================================================
;;; Part 7: Migration with Head Update
;;; ============================================================

;;; migrate-head! : BlockMigration -> Symbol -> (ok Bytevector) | (err String)
;;; Migrate an issue/post by its head name, updating the head file.
(define (migrate-head! bm head-name)
  (let ([current-hash (read-head head-name)])
    (if (not current-hash)
        (list 'err (format "Head not found: ~a" head-name))
        (let ([new-hash (migrate-tree! bm current-hash)])
          (if (equal? new-hash current-hash)
              (list 'ok current-hash)  ; No change
              (begin
                (update-head! head-name new-hash)
                (list 'ok new-hash)))))))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Single Block:
;;;   run-migration!, run-rollback!
;;;
;;; Tree Migration:
;;;   migrate-tree!, rollback-tree!
;;;
;;; Batch:
;;;   migrate-many!
;;;
;;; Statistics/Preview:
;;;   migration-stats, migrate-dry-run
;;;
;;; Head Management:
;;;   update-head!, read-head, migrate-head!

(display "Loaded: shell/migrations/runner.ss\n")
