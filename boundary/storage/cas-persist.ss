(source-directories (cons "core" (source-directories)))
(load "base/prelude.ss")
(load "blocks/block.ss")
(load "blocks/cas.ss")

(doc 'module 'cas-persist)
(doc 'description "Filesystem Persistence for CAS")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude core/blocks/block core/blocks/cas))

(doc 'note "Extends the core/ in-memory CAS with filesystem persistence")

(doc 'storage-layout "
  .fold/cas/XX/YYYYYYYYYYYYYYYYYYYYYYYY...

  Where XX is the first byte of the hash (2 hex chars)
  and YYY... is the remainder (64 hex chars).

  This sharding prevents any single directory from having too many files.")

(doc 'note "In production, this would use capability-gated boundary/io/fs.ss")

(doc 'section 'configuration)

(doc *cas-root* 'type String)
(doc *cas-root* 'description "Base directory for CAS storage")
(define *cas-root* ".fold/cas")

(doc 'section 'path-utilities)

(define (cas-path hash)
  (doc 'type (-> Bytevector String))
  (doc 'description "Compute the filesystem path for a block hash")
  (doc 'note "Uses first byte for sharding: .fold/cas/XX/YYYYYYYY...")
  (doc 'export #t)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [suffix (substring hex 2 (string-length hex))])
        (string-append *cas-root* "/" prefix "/" suffix)))

(define (ensure-dir path)
  (doc 'type (-> String Void))
  (doc 'description "Create directory if it doesn't exist (including parents)")
  (unless (file-directory? path)
          (let ([parent (path-parent path)])
               (when (and parent (> (string-length parent) 0))
                     (ensure-dir parent)))
          (guard (exn [else #f])
                 (mkdir path))))

(define (path-parent path)
  (doc 'type (-> String (Maybe String)))
  (doc 'description "Get parent directory of a path")
  (let loop ([i (- (string-length path) 1)])
       (cond
        [(< i 0) #f]
        [(or (char=? (string-ref path i) #\\)
             (char=? (string-ref path i) #\/))
         (substring path 0 i)]
        [else (loop (- i 1))])))

(doc 'section 'persistence-operations)

(define (persist-block! hash blk)
  (doc 'type (-> Bytevector Block Void))
  (doc 'description "Write a block to disk")
  (doc 'note "Idempotent - safe to call multiple times")
  (doc 'export #t)
  (let* ([path (cas-path hash)]
         [parent (path-parent path)])
        (ensure-dir parent)
        (let ([bytes (block->bytes blk)])
             (call-with-port
              (open-file-output-port path (file-options no-fail))
              (lambda (port)
                      (put-bytevector port bytes))))))

(define (load-block hash)
  (doc 'type (-> Bytevector (Maybe Block)))
  (doc 'description "Load a block from disk by its hash")
  (doc 'returns "#f if not found")
  (doc 'export #t)
  (let ([path (cas-path hash)])
       (if (file-exists? path)
           (guard (exn [else #f])
                  (let ([bytes (call-with-port
                                (open-file-input-port path)
                                (lambda (port)
                                        (get-bytevector-all port)))])
                       (bytes->block bytes)))
           #f)))

(doc 'section 'hydration)

(define (hydrate-cas!)
  (doc 'type (-> Void Nat))
  (doc 'description "Load all blocks from disk into the in-memory CAS")
  (doc 'returns "Number of blocks loaded")
  (doc 'export #t)
  (if (not (file-directory? *cas-root*))
      0
      (let ([count 0])
           (for-each
            (lambda (shard)
                    (let ([shard-path (string-append *cas-root* "/" shard)])
                         (when (file-directory? shard-path)
                               (for-each
                                (lambda (suffix)
                                        (let* ([hash-hex (string-append shard suffix)]
                                               [hash (hex->hash hash-hex)]
                                               [blk (load-block hash)])
                                              (when blk
                                                    (hashtable-set! *store* hash blk)
                                                    (set! count (+ count 1)))))
                                (directory-list shard-path)))))
            (directory-list *cas-root*))
           count)))

(doc 'section 'store-with-persistence)

(define (store-persistent! blk)
  (doc 'type (-> Block Bytevector))
  (doc 'description "Store a block in both memory and disk")
  (doc 'export #t)
  (let ([hash (hash-block blk)])
       (hashtable-set! *store* hash blk)
       (persist-block! hash blk)
       hash))

(define (fetch-persistent hash)
  (doc 'type (-> Bytevector (Maybe Block)))
  (doc 'description "Fetch a block, checking disk if not in memory")
  (doc 'export #t)
  (or (hashtable-ref *store* hash #f)
      (let ([blk (load-block hash)])
           (when blk
                 (hashtable-set! *store* hash blk))
           blk)))

(doc 'section 'utilities)

(define (cas-stats)
  (doc 'type (-> Void Alist))
  (doc 'description "Get statistics about the CAS (memory and disk)")
  (doc 'export #t)
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
