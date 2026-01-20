(load "core/base/prelude.ss")

(doc 'module 'fs)
(doc 'description "Filesystem Capability Layer — Optional persistence for the in-memory CAS. Core CAS is in-memory only; Shell adds persistence when needed. All operations are capability-gated.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(prelude))
(doc 'note "String utilities (string-starts-with?, string-ends-with?) provided by core/prelude.ss. string-prefix? and string-suffix? are aliases (defined locally for convenience). string-rindex is unique to this module.")

(doc 'section 'capability-token)
(doc 'note "Capabilities are opaque records — unforgeable at runtime. Core cannot construct these; only Shell can mint them.")

(define-record-type fs-capability
  (fields store-path))

(doc 'section 'path-utilities)

(define (hash->object-path hash store-path)
  (doc 'type (-> Bytevector String String))
  (doc 'description "Convert hash to filesystem path under store. Uses first 2 hex chars as subdirectory (like git).")
  (doc 'export #t)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [rest hex])
        (string-append store-path "/objects/" prefix "/" rest)))

(define (hash->pin-path hash store-path)
  (doc 'type (-> Bytevector String String))
  (doc 'description "Convert hash to pin file path under store.")
  (doc 'export #t)
  (string-append store-path "/pins/" (hash->hex hash) ".pin"))

(define (ensure-parent-dir! path)
  (doc 'type (-> String Void))
  (doc 'description "Create parent directory if it doesn't exist.")
  (let ([parent (path-parent path)])
       (when (and parent (not (file-exists? parent)))
             (ensure-parent-dir! parent)
             (mkdir parent))))

(define (path-parent path)
  (doc 'type (-> String (Maybe String)))
  (doc 'description "Extract parent directory from path.")
  (doc 'export #t)
  (let ([sep-pos (find-last-sep path)])
       (if sep-pos
           (substring path 0 sep-pos)
           #f)))

(define (find-last-sep path)
  (doc 'type (-> String (Maybe Nat)))
  (doc 'description "Find the last path separator (/ or \\) in a path.")
  (let loop ([i (- (string-length path) 1)])
       (cond
        [(< i 0) #f]
        [(or (char=? (string-ref path i) #\/)
             (char=? (string-ref path i) #\\)) i]
        [else (loop (- i 1))])))

(define (path-basename path)
  (doc 'type (-> String String))
  (doc 'description "Get filename from path (after last separator).")
  (doc 'export #t)
  (let ([sep-pos (find-last-sep path)])
       (if sep-pos
           (substring path (+ sep-pos 1) (string-length path))
           path)))

(define (path-stem path)
  (doc 'type (-> String String))
  (doc 'description "Remove directory and extension from filename.")
  (doc 'export #t)
  (let* ([base (path-basename path)]
         [dot-pos (string-rindex base #\.)])
        (if dot-pos
            (substring base 0 dot-pos)
            base)))

(doc string-suffix? 'type (-> String String Bool))
(doc string-suffix? 'description "Alias for string-ends-with? from prelude (note: reversed argument order).")
(doc string-suffix? 'export #t)
(define (string-suffix? suffix str)
  (string-ends-with? str suffix))

(doc string-prefix? 'type (-> String String Bool))
(doc string-prefix? 'description "Alias for string-starts-with? from prelude (note: reversed argument order).")
(doc string-prefix? 'export #t)
(define (string-prefix? prefix str)
  (string-starts-with? str prefix))

(define (string-rindex str char)
  (doc 'type (-> String Char (Maybe Nat)))
  (doc 'description "Find last occurrence of char in string.")
  (doc 'export #t)
  (let loop ([i (- (string-length str) 1)])
       (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) char) i]
        [else (loop (- i 1))])))

(doc 'section 'store-initialization)

(define (mint-fs-capability store-path)
  (doc 'type (-> String FsCapability))
  (doc 'description "Mint a filesystem capability for the given store path. Creates directory structure if needed.")
  (doc 'export #t)
  (ensure-store-structure! store-path)
  (make-fs-capability store-path))

(define (ensure-store-structure! store-path)
  (doc 'type (-> String Void))
  (doc 'description "Ensure store directory structure exists (objects/, pins/, heads/).")
  (for-each
   (lambda (subdir)
           (let ([path (string-append store-path "/" subdir)])
                (unless (file-exists? path)
                        (ensure-parent-dir! path)
                        (mkdir path))))
   '("objects" "pins" "heads")))

(doc 'section 'store-operations)

(define (fs-store! fs blk)
  (doc 'type (-> FsCapability Block Bytevector))
  (doc 'description "Persist a block to disk and return its hash.")
  (doc 'export #t)
  (let* ([hash (hash-block blk)]
         [path (hash->object-path hash (fs-capability-store-path fs))])
        (unless (file-exists? path)
                (ensure-parent-dir! path)
                (let ([bytes (block->bytes blk)]
                      [port (open-file-output-port path)])
                     (put-bytevector port bytes)
                     (close-port port)))
        hash))

(define (fs-fetch fs hash)
  (doc 'type (-> FsCapability Bytevector (Maybe Block)))
  (doc 'description "Load a block from disk by hash. Returns #f if the block doesn't exist or is corrupt.")
  (doc 'export #t)
  (let ([path (hash->object-path hash (fs-capability-store-path fs))])
       (if (file-exists? path)
           (guard (e [else #f])  ; Return #f for corrupt blocks
                  (let* ([port (open-file-input-port path)]
                         [bytes (get-bytevector-all port)])
                        (close-port port)
                        (bytes->block bytes)))
           #f)))

(define (fs-stored? fs hash)
  (doc 'type (-> FsCapability Bytevector Bool))
  (doc 'description "Check if a block exists in the store.")
  (doc 'export #t)
  (file-exists? (hash->object-path hash (fs-capability-store-path fs))))

(define (fs-pin! fs hash)
  (doc 'type (-> FsCapability Bytevector Void))
  (doc 'description "Mark a hash as pinned (preserved during GC).")
  (doc 'export #t)
  (let ([path (hash->pin-path hash (fs-capability-store-path fs))])
       (ensure-parent-dir! path)
       (call-with-output-file path
                              (lambda (port)
                                      (put-string port (hash->hex hash))))))

(define (fs-pinned? fs hash)
  (doc 'type (-> FsCapability Bytevector Bool))
  (doc 'description "Check if a hash is pinned.")
  (doc 'export #t)
  (file-exists? (hash->pin-path hash (fs-capability-store-path fs))))

(doc 'section 'sync-and-statistics)

(define (fs-sync! fs)
  (doc 'type (-> FsCapability Void))
  (doc 'description "Force all pending writes to disk. Currently a no-op; writes are synchronous.")
  (doc 'export #t)
  (void))

(define (fs-object-count fs)
  (doc 'type (-> FsCapability Nat))
  (doc 'description "Count objects in the store (expensive — scans filesystem).")
  (doc 'export #t)
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

(define (fs-all-hashes fs)
  (doc 'type (-> FsCapability (List Bytevector)))
  (doc 'description "List all object hashes in the store (expensive — scans filesystem).")
  (doc 'export #t)
  (let ([objects-path (string-append (fs-capability-store-path fs) "/objects")])
       (if (file-exists? objects-path)
           (apply append
                  (map
                   (lambda (prefix-dir)
                           (let ([prefix-path (string-append objects-path "/" prefix-dir)])
                                (if (file-directory? prefix-path)
                                    (map (lambda (filename)
                                                 (hex->hash filename))
                                         (directory-list prefix-path))
                                    '())))
                   (directory-list objects-path)))
           '())))

(doc 'section 'head-pointers)
(doc 'note "Head pointers for forum channels — track the latest block in each channel.")

(define (fs-write-head! fs channel hash)
  (doc 'type (-> FsCapability Symbol Bytevector Void))
  (doc 'description "Write a channel head pointer (overwrites if exists).")
  (doc 'export #t)
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

(define (fs-read-head fs channel)
  (doc 'type (-> FsCapability Symbol (Maybe Bytevector)))
  (doc 'description "Read a channel head pointer.")
  (doc 'export #t)
  (let ([path (string-append (fs-capability-store-path fs)
                             "/heads/"
                             (symbol->string channel)
                             ".head")])
       (if (file-exists? path)
           (call-with-input-file path
                                 (lambda (port)
                                         (let* ([line (get-line port)]
                                                [len (string-length line)]
                                                [clean (if (and (> len 0)
                                                                (char=? (string-ref line (- len 1)) #\return))
                                                           (substring line 0 (- len 1))
                                                           line)])
                                               (hex->hash clean))))
           #f)))

(doc 'section 'batch-operations)

(define (fs-store-all! fs blocks)
  (doc 'type (-> FsCapability (List Block) (List Bytevector)))
  (doc 'description "Store multiple blocks, return their hashes.")
  (doc 'export #t)
  (map (lambda (blk) (fs-store! fs blk)) blocks))

(define (fs-fetch-all fs hashes)
  (doc 'type (-> FsCapability (List Bytevector) (List (Maybe Block))))
  (doc 'description "Fetch multiple blocks by hash.")
  (doc 'export #t)
  (map (lambda (h) (fs-fetch fs h)) hashes))
