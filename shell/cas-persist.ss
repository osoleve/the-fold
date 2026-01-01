;;; thimble/cas-persist.ss — Filesystem Persistence for CAS
;;;
;;; Extends the core/ in-memory CAS with filesystem persistence.
;;;
;;; Storage Layout:
;;;   .fold/cas/XX/YYYYYYYYYYYYYYYYYYYYYYYY...
;;;
;;;   Where XX is the first byte of the hash (2 hex chars)
;;;   and YYY... is the remainder (64 hex chars).
;;;
;;; This sharding prevents any single directory from having too many files.
;;;
;;; Operations:
;;;   persist-block! : Hash × Block → void    (write to disk)
;;;   load-block : Hash → Block | #f          (read from disk)
;;;   hydrate-cas! : void → Nat               (load all blocks into memory CAS)
;;;   cas-path : Hash → String                (compute file path for hash)
;;;
;;; This is Shell code: impure (filesystem IO).
;;;
;;; Dependencies:
;;;   - core/prelude.ss
;;;   - core/block.ss
;;;   - core/cas.ss
;;;   - thimble/fs.ss (for filesystem operations)
;;;
;;; See fabric/stitches/MODULES.md for dependency graph.

(source-directories (cons "core" (source-directories)))
(load "prelude.ss")
(load "block.ss")
(load "cas.ss")

;;; Note: We use Chez's built-in file operations for simplicity
;;; In production, this would use capability-gated shell/fs.ss

;;; ============================================================
;;; Configuration
;;; ============================================================

;;; Base directory for CAS storage
(define *cas-root* ".fold/cas")

;;; ============================================================
;;; Path Utilities
;;; ============================================================

;;; cas-path : Bytevector → String
;;; Compute the filesystem path for a block hash.
;;; Uses first byte for sharding: .fold/cas/XX/YYYYYYYY...
(define (cas-path hash)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [suffix (substring hex 2 (string-length hex))])
        (string-append *cas-root* "/" prefix "/" suffix)))

;;; ensure-dir : String → void
;;; Create directory if it doesn't exist (including parents).
(define (ensure-dir path)
  (unless (file-directory? path)
          ;; Create parent first
          (let ([parent (path-parent path)])
               (when (and parent (> (string-length parent) 0))
                     (ensure-dir parent)))
          (guard (exn [else #f])  ; ignore if already exists
                 (mkdir path))))

;;; path-parent : String → String | #f
;;; Get parent directory of a path.
(define (path-parent path)
  (let loop ([i (- (string-length path) 1)])
       (cond
        [(< i 0) #f]
        [(or (char=? (string-ref path i) #\\)
             (char=? (string-ref path i) #\/))
         (substring path 0 i)]
        [else (loop (- i 1))])))

;;; ============================================================
;;; Persistence Operations
;;; ============================================================

;;; persist-block! : Bytevector × Block → void
;;; Write a block to disk. Idempotent - safe to call multiple times.
(define (persist-block! hash blk)
  (let* ([path (cas-path hash)]
         [parent (path-parent path)])
        ;; Ensure shard directory exists
        (ensure-dir parent)
        ;; Write block bytes to file
        (let ([bytes (block->bytes blk)])
             (call-with-port
              (open-file-output-port path (file-options no-fail))
              (lambda (port)
                      (put-bytevector port bytes))))))

;;; load-block : Bytevector → Block | #f
;;; Load a block from disk by its hash. Returns #f if not found.
(define (load-block hash)
  (let ([path (cas-path hash)])
       (if (file-exists? path)
           (guard (exn [else #f])
                  (let ([bytes (call-with-port
                                (open-file-input-port path)
                                (lambda (port)
                                        (get-bytevector-all port)))])
                       (bytes->block bytes)))
           #f)))

;;; ============================================================
;;; Hydration (Loading All Blocks)
;;; ============================================================

;;; hydrate-cas! : void → Nat
;;; Load all blocks from disk into the in-memory CAS.
;;; Returns the number of blocks loaded.
(define (hydrate-cas!)
  (if (not (file-directory? *cas-root*))
      0
      (let ([count 0])
           ;; Iterate over shard directories (00-ff)
           (for-each
            (lambda (shard)
                    (let ([shard-path (string-append *cas-root* "/" shard)])
                         (when (file-directory? shard-path)
                               ;; Iterate over block files in this shard
                               (for-each
                                (lambda (suffix)
                                        (let* ([hash-hex (string-append shard suffix)]
                                               [hash (hex->hash hash-hex)]
                                               [blk (load-block hash)])
                                              (when blk
                                                    ;; Store in memory CAS (don't persist again)
                                                    (hashtable-set! *store* hash blk)
                                                    (set! count (+ count 1)))))
                                (directory-list shard-path)))))
            (directory-list *cas-root*))
           count)))

;;; ============================================================
;;; Store with Persistence
;;; ============================================================

;;; store-persistent! : Block → Bytevector
;;; Store a block in both memory and disk.
(define (store-persistent! blk)
  (let ([hash (hash-block blk)])
       (hashtable-set! *store* hash blk)
       (persist-block! hash blk)
       hash))

;;; fetch-persistent : Bytevector → Block | #f
;;; Fetch a block, checking disk if not in memory.
(define (fetch-persistent hash)
  (or (hashtable-ref *store* hash #f)
      (let ([blk (load-block hash)])
           (when blk
                 (hashtable-set! *store* hash blk))
           blk)))

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; cas-stats : void → Association list
;;; Get statistics about the CAS (memory and disk).
(define (cas-stats)
  (let ([memory-count (hashtable-size *store*)]
        [disk-count (if (file-directory? *cas-root*)
                        (let ([count 0])
                             (for-each
                              (lambda (shard)
                                      (let ([shard-path (string-append *cas-root* "/" shard)])
                                           (when (file-directory? shard-path)
                                                 (set! count (+ count (length (directory-list shard-path)))))))
                              (directory-list *cas-root*))
                             count)
                        0)])
       `((memory-blocks . ,memory-count)
         (disk-blocks . ,disk-count)
         (cas-root . ,*cas-root*))))
