;;; lattice/fp/optics/block-migration.ss — CAS Block-Specific Migrations
;;;
;;; Migrations specialized for The Fold's content-addressed block system:
;;;
;;;   - Block tag transformations
;;;   - Payload transformations (with S-expression parsing)
;;;   - Block tree traversal (bottom-up for Merkle correctness)
;;;   - Version detection via tag
;;;
;;; Key design decisions:
;;;
;;; 1. **Bottom-up traversal**: In a Merkle DAG, if a child changes,
;;;    its hash changes, so parents must update their refs. We traverse
;;;    post-order: migrate children first, then parent with new refs.
;;;
;;; 2. **Memoization**: Shared subtrees (DAG, not just tree) must only
;;;    be migrated once. We track visited hashes.
;;;
;;; 3. **Tag-based versioning**: Block version encoded in tag (e.g., 'bbs-issue-v2).
;;;    Migrations check tag match before applying.
;;;
;;; 4. **Payload type safety**: Only attempt sexpr parsing on known sexpr tags.
;;;
;;; This is Pure lattice code: block transformations without CAS mutation.
;;; The shell layer (shell/migrations/runner.ss) handles actual CAS I/O.
;;;
;;; Dependencies:
;;;   - core/blocks/block.ss (for block record type)
;;;   - bidirectional.ss (for migration infrastructure)
;;;   - format-iso.ss (for bytevector<->sexpr)

(load "core/blocks/block.ss")
(load "lattice/fp/optics/bidirectional.ss")
(load "lattice/fp/optics/format-iso.ss")

;;; ============================================================
;;; Part 1: Block Migration Type
;;; ============================================================
;;;
;;; A block migration transforms blocks from one tag to another,
;;; with a payload transformation encoded as an iso.

;;; make-block-migration : Symbol -> Symbol -> PIso -> BlockMigration
;;; Create a block migration.
;;; - from-tag: Expected tag of source blocks
;;; - to-tag: Tag of migrated blocks
;;; - payload-iso: Transforms payload content (sexpr -> sexpr)
(define (make-block-migration from-tag to-tag payload-iso)
  (list 'block-migration from-tag to-tag payload-iso))

;;; block-migration? : Any -> Boolean
(define (block-migration? x)
  (and (pair? x) (eq? (car x) 'block-migration)))

;;; block-migration-from-tag : BlockMigration -> Symbol
(define (block-migration-from-tag bm)
  (cadr bm))

;;; block-migration-to-tag : BlockMigration -> Symbol
(define (block-migration-to-tag bm)
  (caddr bm))

;;; block-migration-payload-iso : BlockMigration -> PIso
(define (block-migration-payload-iso bm)
  (cadddr bm))

;;; ============================================================
;;; Part 2: Block Transformation (Pure)
;;; ============================================================
;;;
;;; Pure block transformation functions. These don't touch the CAS;
;;; they take a block and return a new block.

;;; block-migration-applies? : BlockMigration -> Block -> Boolean
;;; Check if this migration applies to the given block.
(define (block-migration-applies? bm blk)
  (eq? (block-tag blk) (block-migration-from-tag bm)))

;;; block-migrate-payload : BlockMigration -> Block -> Block
;;; Apply migration's payload iso to a block.
;;; Assumes block has sexpr-parseable payload.
(define (block-migrate-payload bm blk)
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

;;; block-rollback-payload : BlockMigration -> Block -> Block
;;; Apply migration's payload iso backward.
(define (block-rollback-payload bm blk)
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

;;; ============================================================
;;; Part 3: Block with Updated Refs
;;; ============================================================
;;;
;;; When migrating a tree, we need to update refs after children are migrated.

;;; block-with-refs : Block -> (Vector Bytevector) -> Block
;;; Create new block with updated refs.
(define (block-with-refs blk new-refs)
  (make-block (block-tag blk)
              (block-payload blk)
              new-refs))

;;; block-migrate-with-refs : BlockMigration -> Block -> (Vector Bytevector) -> Block
;;; Apply migration with updated refs (for tree traversal).
(define (block-migrate-with-refs bm blk new-refs)
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

;;; ============================================================
;;; Part 4: Tag-Based Version Detection
;;; ============================================================

;;; parse-versioned-tag : Symbol -> (Symbol . (Number | #f))
;;; Parse a versioned tag like 'bbs-issue-v2 into base and version.
(define (parse-versioned-tag tag)
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

;;; make-versioned-tag : Symbol -> Number -> Symbol
;;; Create a versioned tag like 'bbs-issue-v2.
(define (make-versioned-tag base version)
  (string->symbol (format "~a-v~a" base version)))

;;; block-tag-version : Block -> Number | #f
;;; Extract version number from block's tag.
(define (block-tag-version blk)
  (cdr (parse-versioned-tag (block-tag blk))))

;;; block-tag-base : Block -> Symbol
;;; Extract base tag (without version) from block.
(define (block-tag-base blk)
  (car (parse-versioned-tag (block-tag blk))))

;;; ============================================================
;;; Part 5: Block Migration Composition
;;; ============================================================

;;; block-migration-compose : BlockMigration -> BlockMigration -> BlockMigration
;;; Compose two block migrations.
;;; Requires: bm1.to-tag = bm2.from-tag
(define (block-migration-compose bm1 bm2)
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

;;; block-migration-flip : BlockMigration -> BlockMigration
;;; Reverse a block migration (swap forward/backward and from/to tags).
(define (block-migration-flip bm)
  (let ([payload-iso (block-migration-payload-iso bm)])
    (make-block-migration
     (block-migration-to-tag bm)
     (block-migration-from-tag bm)
     (make-p-iso
      (p-iso-backward payload-iso)
      (p-iso-forward payload-iso)))))

;;; ============================================================
;;; Part 6: Migration Builders for Common Patterns
;;; ============================================================

;;; make-tag-only-migration : Symbol -> Symbol -> BlockMigration
;;; Create a migration that only changes the tag (no payload change).
(define (make-tag-only-migration from-tag to-tag)
  (make-block-migration from-tag to-tag p-iso-id))

;;; make-schema-migration : Symbol -> Symbol -> (List PIso) -> BlockMigration
;;; Create a migration with schema field operations.
(define (make-schema-migration from-tag to-tag schema-isos)
  (let ([combined-iso (fold-left p-iso-compose p-iso-id schema-isos)])
    (make-block-migration from-tag to-tag combined-iso)))

;;; ============================================================
;;; Part 7: Block Type Guards
;;; ============================================================
;;;
;;; Safety checks for payload parsing.

;;; block-has-sexpr-payload? : Block -> Boolean
;;; Check if block's payload is valid S-expression.
(define (block-has-sexpr-payload? blk)
  (guard (ex [else #f])
    (let* ([str (utf8->string (block-payload blk))]
           [port (open-input-string str)]
           [result (read port)])
      (and (not (eof-object? result))
           (eof-object? (read port))))))  ; No trailing data

;;; known-sexpr-tags : (List Symbol)
;;; Tags known to have sexpr payloads.
(define known-sexpr-tags
  '(expr bbs-issue bbs-issue-v1 bbs-issue-v2 bbs-post bbs-post-v1
    lambda app ref lit define let letrec if begin quote))

;;; block-is-sexpr-type? : Block -> Boolean
;;; Check if block is a known sexpr-payload type.
(define (block-is-sexpr-type? blk)
  (memq (block-tag blk) known-sexpr-tags))

;;; ============================================================
;;; Part 8: Convert Block Migration to Regular Migration
;;; ============================================================
;;;
;;; Bridge between block migrations and the general migration system.

;;; block-migration->migration : BlockMigration -> Migration
;;; Convert a block migration to a general migration.
;;; The resulting migration works on blocks directly.
(define (block-migration->migration bm)
  (make-migration
   (string->symbol (format "block-~a->~a"
                           (block-migration-from-tag bm)
                           (block-migration-to-tag bm)))
   (block-migration-from-tag bm)
   (block-migration-to-tag bm)
   (make-p-iso
    (lambda (blk) (block-migrate-payload bm blk))
    (lambda (blk) (block-rollback-payload bm blk)))))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Block Migration Type:
;;;   make-block-migration, block-migration?
;;;   block-migration-from-tag, block-migration-to-tag
;;;   block-migration-payload-iso
;;;
;;; Block Transformation:
;;;   block-migration-applies?, block-migrate-payload, block-rollback-payload
;;;   block-with-refs, block-migrate-with-refs
;;;
;;; Version Detection:
;;;   parse-versioned-tag, make-versioned-tag
;;;   block-tag-version, block-tag-base
;;;
;;; Composition:
;;;   block-migration-compose, block-migration-flip
;;;
;;; Builders:
;;;   make-tag-only-migration, make-schema-migration
;;;
;;; Type Guards:
;;;   block-has-sexpr-payload?, known-sexpr-tags, block-is-sexpr-type?
;;;
;;; Bridge:
;;;   block-migration->migration

(display "Loaded: lattice/fp/optics/block-migration.ss\n")
