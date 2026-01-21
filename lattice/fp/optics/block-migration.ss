;;; lattice/fp/optics/block-migration.ss — Block Migrations
;;; @module block-migration
;;; @requires block bidirectional format-iso

(load "core/blocks/block.ss")
(load "lattice/fp/optics/bidirectional.ss")
(load "lattice/fp/optics/format-iso.ss")

(doc 'module 'block-migration)
(doc 'description "CAS Block-Specific Migrations

Migrations specialized for The Fold's content-addressed block system:

  - Block tag transformations
  - Payload transformations (with S-expression parsing)
  - Block tree traversal (bottom-up for Merkle correctness)
  - Version detection via tag

Key design decisions:

1. **Bottom-up traversal**: In a Merkle DAG, if a child changes,
   its hash changes, so parents must update their refs. We traverse
   post-order: migrate children first, then parent with new refs.

2. **Memoization**: Shared subtrees (DAG, not just tree) must only
   be migrated once. We track visited hashes.

3. **Tag-based versioning**: Block version encoded in tag (e.g., 'bbs-issue-v2).
   Migrations check tag match before applying.

4. **Payload type safety**: Only attempt sexpr parsing on known sexpr tags.

This is Pure lattice code: block transformations without CAS mutation.
The boundary layer (boundary/migrations/runner.ss) handles actual CAS I/O.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'block-migration-type)

(define (make-block-migration from-tag to-tag payload-iso)
  (doc 'export #t)
  (doc 'type '(-> Symbol Symbol PIso BlockMigration))
  (doc 'description "Create a block migration.
- from-tag: Expected tag of source blocks
- to-tag: Tag of migrated blocks
- payload-iso: Transforms payload content (sexpr -> sexpr)")
  (list 'block-migration from-tag to-tag payload-iso))

(define (block-migration? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'block-migration)))

(define (block-migration-from-tag bm)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration Symbol))
  (cadr bm))

(define (block-migration-to-tag bm)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration Symbol))
  (caddr bm))

(define (block-migration-payload-iso bm)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration PIso))
  (cadddr bm))

(doc 'section 'transformation)

(define (block-migration-applies? bm blk)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration Block Boolean))
  (doc 'description "Check if this migration applies to the given block.")
  (eq? (block-tag blk) (block-migration-from-tag bm)))

(define (block-migrate-payload bm blk)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration Block Block))
  (doc 'description "Apply migration's payload iso to a block.
Assumes block has sexpr-parseable payload.")
  (let* ([from-tag (block-migration-from-tag bm)]
         [to-tag (block-migration-to-tag bm)]
         [payload-iso (block-migration-payload-iso bm)])
    ;; Guard: only apply if tag matches
    (if (not (eq? (block-tag blk) from-tag))
        blk  ; Return unchanged if tag doesn't match
        ;; Parse payload as sexpr, apply iso, serialize back
        (let* ([payload (block-payload blk)]
               [sexpr (bytevector->sexpr payload)]
               [new-sexpr ((p-iso-forward payload-iso) sexpr)]
               [new-payload (sexpr->bytevector new-sexpr)])
          (make-block to-tag new-payload (block-refs blk))))))

(define (block-rollback-payload bm blk)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration Block Block))
  (doc 'description "Apply migration's payload iso backward.")
  (let* ([from-tag (block-migration-from-tag bm)]
         [to-tag (block-migration-to-tag bm)]
         [payload-iso (block-migration-payload-iso bm)])
    ;; Guard: only apply if tag matches to-tag (rolling back)
    (if (not (eq? (block-tag blk) to-tag))
        blk
        (let* ([payload (block-payload blk)]
               [sexpr (bytevector->sexpr payload)]
               [new-sexpr ((p-iso-backward payload-iso) sexpr)]
               [new-payload (sexpr->bytevector new-sexpr)])
          (make-block from-tag new-payload (block-refs blk))))))

(doc 'section 'refs)

(define (block-with-refs blk new-refs)
  (doc 'export #t)
  (doc 'type '(-> Block (Vector Bytevector) Block))
  (doc 'description "Create new block with updated refs.")
  (make-block (block-tag blk)
              (block-payload blk)
              new-refs))

(define (block-migrate-with-refs bm blk new-refs)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration Block (Vector Bytevector) Block))
  (doc 'description "Apply migration with updated refs (for tree traversal).")
  (let* ([from-tag (block-migration-from-tag bm)]
         [to-tag (block-migration-to-tag bm)]
         [payload-iso (block-migration-payload-iso bm)])
    (if (not (eq? (block-tag blk) from-tag))
        ;; Tag doesn't match - just update refs
        (make-block (block-tag blk) (block-payload blk) new-refs)
        ;; Tag matches - migrate payload and update refs
        (let* ([payload (block-payload blk)]
               [sexpr (bytevector->sexpr payload)]
               [new-sexpr ((p-iso-forward payload-iso) sexpr)]
               [new-payload (sexpr->bytevector new-sexpr)])
          (make-block to-tag new-payload new-refs)))))

(doc 'section 'version-detection)

(define (parse-versioned-tag tag)
  (doc 'export #t)
  (doc 'type '(-> Symbol (Pair Symbol (Maybe Number))))
  (doc 'description "Parse a versioned tag like 'bbs-issue-v2 into base and version.")
  (let* ([s (symbol->string tag)]
         [len (string-length s)])
    ;; Look for -v<number> suffix
    (let loop ([i (- len 1)])
      (cond
        [(< i 2) (cons tag #f)]  ; No version found
        [(and (char=? (string-ref s i) #\v)
              (char=? (string-ref s (- i 1)) #\-))
         ;; Found -v, parse number
         (let ([num-str (substring s (+ i 1) len)])
           (let ([num (string->number num-str)])
             (if num
                 (cons (string->symbol (substring s 0 (- i 1))) num)
                 (cons tag #f))))]
        [else (loop (- i 1))]))))

(define (make-versioned-tag base version)
  (doc 'export #t)
  (doc 'type '(-> Symbol Number Symbol))
  (doc 'description "Create a versioned tag like 'bbs-issue-v2.")
  (string->symbol (format "~a-v~a" base version)))

(define (block-tag-version blk)
  (doc 'export #t)
  (doc 'type '(-> Block (Maybe Number)))
  (doc 'description "Extract version number from block's tag.")
  (cdr (parse-versioned-tag (block-tag blk))))

(define (block-tag-base blk)
  (doc 'export #t)
  (doc 'type '(-> Block Symbol))
  (doc 'description "Extract base tag (without version) from block.")
  (car (parse-versioned-tag (block-tag blk))))

(doc 'section 'composition)

(define (block-migration-compose bm1 bm2)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration BlockMigration BlockMigration))
  (doc 'description "Compose two block migrations.
Requires: bm1.to-tag = bm2.from-tag")
  (unless (eq? (block-migration-to-tag bm1) (block-migration-from-tag bm2))
    (error 'block-migration-compose
           "Tag mismatch: ~a.to-tag (~a) != ~a.from-tag (~a)"
           bm1 (block-migration-to-tag bm1)
           bm2 (block-migration-from-tag bm2)))
  (make-block-migration
   (block-migration-from-tag bm1)
   (block-migration-to-tag bm2)
   (p-iso-compose (block-migration-payload-iso bm1)
                  (block-migration-payload-iso bm2))))

(define (block-migration-flip bm)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration BlockMigration))
  (doc 'description "Reverse a block migration (swap forward/backward and from/to tags).")
  (let ([payload-iso (block-migration-payload-iso bm)])
    (make-block-migration
     (block-migration-to-tag bm)
     (block-migration-from-tag bm)
     (make-p-iso
      (p-iso-backward payload-iso)
      (p-iso-forward payload-iso)))))

(doc 'section 'builders)

(define (make-tag-only-migration from-tag to-tag)
  (doc 'export #t)
  (doc 'type '(-> Symbol Symbol BlockMigration))
  (doc 'description "Create a migration that only changes the tag (no payload change).")
  (make-block-migration from-tag to-tag p-iso-id))

(define (make-schema-migration from-tag to-tag schema-isos)
  (doc 'export #t)
  (doc 'type '(-> Symbol Symbol (List PIso) BlockMigration))
  (doc 'description "Create a migration with schema field operations.")
  (let ([combined-iso (fold-left p-iso-compose p-iso-id schema-isos)])
    (make-block-migration from-tag to-tag combined-iso)))

(doc 'section 'guards)

(define (block-has-sexpr-payload? blk)
  (doc 'export #t)
  (doc 'type '(-> Block Boolean))
  (doc 'description "Check if block's payload is valid S-expression.")
  (guard (ex [else #f])
    (let* ([str (utf8->string (block-payload blk))]
           [port (open-input-string str)]
           [result (read port)])
      (and (not (eof-object? result))
           (eof-object? (read port))))))  ; No trailing data

(doc known-sexpr-tags 'type '(List Symbol))
(doc known-sexpr-tags 'description "Tags known to have sexpr payloads.")
(doc known-sexpr-tags 'export #t)
(define known-sexpr-tags
  '(expr bbs-issue bbs-issue-v1 bbs-issue-v2 bbs-post bbs-post-v1
    lambda app ref lit define let letrec if begin quote))

(define (block-is-sexpr-type? blk)
  (doc 'export #t)
  (doc 'type '(-> Block Boolean))
  (doc 'description "Check if block is a known sexpr-payload type.")
  (memq (block-tag blk) known-sexpr-tags))

(doc 'section 'bridge)

(define (block-migration->migration bm)
  (doc 'export #t)
  (doc 'type '(-> BlockMigration Migration))
  (doc 'description "Convert a block migration to a general migration.
The resulting migration works on blocks directly.")
  (make-migration
   (string->symbol (format "block-~a->~a"
                           (block-migration-from-tag bm)
                           (block-migration-to-tag bm)))
   (block-migration-from-tag bm)
   (block-migration-to-tag bm)
   (make-p-iso
    (lambda (blk) (block-migrate-payload bm blk))
    (lambda (blk) (block-rollback-payload bm blk)))))

(display "Loaded: lattice/fp/optics/block-migration.ss\n")
