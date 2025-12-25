;;; shell/fs.ss — Filesystem Capability Layer
;;;
;;; Persists the Content-Addressed Store to disk.
;;; All operations are capability-gated.
;;;
;;; This is Shell code: impure, defensive, handles failure.
;;;
;;; Capabilities:
;;;   (make-fs-capability store-path) → FS
;;;
;;; Operations (require FS capability):
;;;   (fs-store! fs block) → Bytevector (hash)
;;;   (fs-fetch fs hash) → Block | #f
;;;   (fs-stored? fs hash) → Boolean
;;;   (fs-pin! fs hash) → void
;;;   (fs-sync! fs) → void
;;;
;;; Storage layout:
;;;   {store-path}/
;;;     objects/
;;;       {aa}/
;;;         {aabbccdd...}  ; block files named by full hash
;;;     pins/
;;;       {hash}.pin       ; pinned hashes
;;;     heads/
;;;       {channel}.head   ; forum head pointers

;;; ============================================================
;;; Capability Token
;;; ============================================================

;;; Capabilities are opaque records — unforgeable at runtime.
;;; Core cannot construct these; only Shell can mint them.

(define-record-type fs-capability
  (fields store-path))

;;; ============================================================
;;; Path Utilities
;;; ============================================================

;;; hash->object-path : Bytevector × String → String
;;; Convert hash to filesystem path under store.
;;; Uses first 2 hex chars as subdirectory (like git).
(define (hash->object-path hash store-path)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [rest hex])
    (string-append store-path "/objects/" prefix "/" rest)))

;;; hash->pin-path : Bytevector × String → String
(define (hash->pin-path hash store-path)
  (string-append store-path "/pins/" (hash->hex hash) ".pin"))

;;; ensure-parent-dir! : String → void
;;; Create parent directory if it doesn't exist.
(define (ensure-parent-dir! path)
  (let ([parent (path-parent path)])
    (when (and parent (not (file-exists? parent)))
      (ensure-parent-dir! parent)
      (mkdir parent))))

;;; path-parent : String → String | #f
;;; Extract parent directory from path.
(define (path-parent path)
  (let ([sep-pos (find-last-sep path)])
    (if sep-pos
        (substring path 0 sep-pos)
        #f)))

(define (find-last-sep path)
  (let loop ([i (- (string-length path) 1)])
    (cond
      [(< i 0) #f]
      [(or (char=? (string-ref path i) #\/)
           (char=? (string-ref path i) #\\)) i]
      [else (loop (- i 1))])))

;;; ============================================================
;;; Store Initialization
;;; ============================================================

;;; mint-fs-capability : String → FS
;;; Mint a filesystem capability for the given store path.
;;; Creates directory structure if needed.
(define (mint-fs-capability store-path)
  (ensure-store-structure! store-path)
  (make-fs-capability store-path))

;;; ensure-store-structure! : String → void
(define (ensure-store-structure! store-path)
  (for-each
    (lambda (subdir)
      (let ([path (string-append store-path "/" subdir)])
        (unless (file-exists? path)
          (ensure-parent-dir! path)
          (mkdir path))))
    '("objects" "pins" "heads")))

;;; ============================================================
;;; Store Operations
;;; ============================================================

;;; fs-store! : FS × Block → Bytevector
;;; Persist a block to disk and return its hash.
(define (fs-store! fs blk)
  (let* ([hash (hash-block blk)]
         [path (hash->object-path hash (fs-capability-store-path fs))])
    (unless (file-exists? path)
      (ensure-parent-dir! path)
      (let ([bytes (block->bytes blk)]
            [port (open-file-output-port path)])
        (put-bytevector port bytes)
        (close-port port)))
    hash))

;;; fs-fetch : FS × Bytevector → Block | #f
;;; Load a block from disk by hash.
(define (fs-fetch fs hash)
  (let ([path (hash->object-path hash (fs-capability-store-path fs))])
    (if (file-exists? path)
        (let* ([port (open-file-input-port path)]
               [bytes (get-bytevector-all port)])
          (close-port port)
          (bytes->block bytes))
        #f)))

;;; fs-stored? : FS × Bytevector → Boolean
(define (fs-stored? fs hash)
  (file-exists? (hash->object-path hash (fs-capability-store-path fs))))

;;; fs-pin! : FS × Bytevector → void
;;; Mark a hash as pinned (preserved during GC).
(define (fs-pin! fs hash)
  (let ([path (hash->pin-path hash (fs-capability-store-path fs))])
    (ensure-parent-dir! path)
    (call-with-output-file path
      (lambda (port)
        (put-string port (hash->hex hash))))))

;;; fs-pinned? : FS × Bytevector → Boolean
(define (fs-pinned? fs hash)
  (file-exists? (hash->pin-path hash (fs-capability-store-path fs))))

;;; ============================================================
;;; Sync and Statistics
;;; ============================================================

;;; fs-sync! : FS → void
;;; Force all pending writes to disk.
;;; (Currently a no-op; writes are synchronous.)
(define (fs-sync! fs)
  (void))

;;; fs-object-count : FS → Nat
;;; Count objects in the store (expensive — scans filesystem).
(define (fs-object-count fs)
  (let ([objects-path (string-append (fs-capability-store-path fs) "/objects")])
    (if (file-exists? objects-path)
        (fold-left
          (lambda (count prefix-dir)
            (let ([prefix-path (string-append objects-path "/" prefix-dir)])
              (if (file-directory? prefix-path)
                  (+ count (length (directory-list prefix-path)))
                  count)))
          0
          (directory-list objects-path))
        0)))

;;; ============================================================
;;; Head Pointers (for forum channels)
;;; ============================================================

;;; fs-write-head! : FS × Symbol × Bytevector → void
;;; Write a channel head pointer (overwrites if exists).
(define (fs-write-head! fs channel hash)
  (let ([path (string-append (fs-capability-store-path fs)
                             "/heads/"
                             (symbol->string channel)
                             ".head")])
    (ensure-parent-dir! path)
    ;; Delete existing file if present
    (when (file-exists? path)
      (delete-file path))
    (call-with-output-file path
      (lambda (port)
        (put-string port (hash->hex hash))))))

;;; fs-read-head : FS × Symbol → Bytevector | #f
;;; Read a channel head pointer.
(define (fs-read-head fs channel)
  (let ([path (string-append (fs-capability-store-path fs)
                             "/heads/"
                             (symbol->string channel)
                             ".head")])
    (if (file-exists? path)
        (call-with-input-file path
          (lambda (port)
            (hex->hash (get-line port))))
        #f)))

;;; ============================================================
;;; Batch Operations
;;; ============================================================

;;; fs-store-all! : FS × (List Block) → (List Bytevector)
;;; Store multiple blocks, return their hashes.
(define (fs-store-all! fs blocks)
  (map (lambda (blk) (fs-store! fs blk)) blocks))

;;; fs-fetch-all : FS × (List Bytevector) → (List (Block | #f))
;;; Fetch multiple blocks by hash.
(define (fs-fetch-all fs hashes)
  (map (lambda (h) (fs-fetch fs h)) hashes))
