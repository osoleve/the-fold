(load "boundary/bbs/blocks.ss")
(load "boundary/bbs/heads.ss")

(doc 'module 'boundary/bbs/store)
(doc 'description "BBS Issue Storage/Retrieval - High-level storage operations for issues. Combines CAS storage with head management")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/bbs/blocks boundary/bbs/heads))

(doc 'section 'storage-root)

(doc *bbs-cas-root* 'type 'String)
(doc *bbs-cas-root* 'description "Storage root (uses same .store as rest of Fold)")
(define *bbs-cas-root* ".store/objects")

(doc 'section 'block-persistence)

(define (bbs-cas-path hash)
  (doc 'type '(-> Bytevector String))
  (doc 'description "Compute filesystem path for a block hash. Uses first byte for sharding")
  (doc 'export #t)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [suffix (substring hex 2 (string-length hex))])
    (string-append *bbs-cas-root* "/" prefix "/" suffix)))

(define (bbs-ensure-shard-dir! prefix)
  (doc 'type '(-> String Void))
  (doc 'description "Ensure the shard directory exists")
  (let ([shard-dir (string-append *bbs-cas-root* "/" prefix)])
    (unless (file-exists? ".store")
      (mkdir ".store"))
    (unless (file-exists? *bbs-cas-root*)
      (mkdir *bbs-cas-root*))
    (unless (file-exists? shard-dir)
      (mkdir shard-dir))))

(define (bbs-persist-block! hash blk)
  (doc 'type '(-> Bytevector Block Void))
  (doc 'description "Write a block to disk")
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [path (bbs-cas-path hash)])
    (bbs-ensure-shard-dir! prefix)
    (let ([bytes (block->bytes blk)])
      (call-with-port
       (open-file-output-port path (file-options no-fail))
       (lambda (port)
         (put-bytevector port bytes))))))

(define (bbs-load-block hash)
  (doc 'type '(-> Bytevector (Option Block)))
  (doc 'description "Load a block from disk")
  (let ([path (bbs-cas-path hash)])
    (guard (e [else #f])
      (if (file-exists? path)
          (let ([bytes (call-with-port
                        (open-file-input-port path)
                        (lambda (port)
                          (get-bytevector-all port)))])
            (bytes->block bytes))
          #f))))

(doc 'section 'high-level-operations)

(define (bbs-store! blk)
  (doc 'type '(-> Block Bytevector))
  (doc 'description "Store a block and return its hash. Stores in both memory CAS and on disk")
  (doc 'export #t)
  (let ([hash (hash-block blk)])
    (store! blk)
    (bbs-persist-block! hash blk)
    hash))

(define (bbs-fetch hash)
  (doc 'type '(-> Bytevector (Option Block)))
  (doc 'description "Fetch a block by hash. Tries memory first, then disk")
  (doc 'export #t)
  (or (fetch hash)
      (let ([blk (bbs-load-block hash)])
        (when blk
          (store! blk))
        blk)))

(define (bbs-fetch-issue id)
  (doc 'type '(-> String (Option Block)))
  (doc 'description "Fetch the current version of an issue by ID. Uses bbs-issue-hash which auto-refreshes from disk on cache miss")
  (doc 'export #t)
  (let ([hash (bbs-issue-hash id)])
    (if hash
        (bbs-fetch hash)
        #f)))

(define (bbs-fetch-issue-data id)
  (doc 'type '(-> String (Option Alist)))
  (doc 'description "Fetch and parse issue data by ID")
  (doc 'export #t)
  (let ([blk (bbs-fetch-issue id)])
    (if blk
        (issue-block-data blk)
        #f)))

(doc 'section 'issue-history)

(define (bbs-issue-history id)
  (doc 'type '(-> String (List Block)))
  (doc 'description "Get all versions of an issue, newest first")
  (doc 'export #t)
  (let ([hash (bbs-read-head id)])
    (if (not hash)
        '()
        (let loop ([h hash] [acc '()])
          (let ([blk (bbs-fetch h)])
            (if (not blk)
                (reverse acc)
                (let ([prev (issue-block-prev blk)])
                  (if prev
                      (loop prev (cons blk acc))
                      (reverse (cons blk acc))))))))))

(define (bbs-issue-history-data id)
  (doc 'type '(-> String (List Alist)))
  (doc 'description "Get all versions as parsed data")
  (doc 'export #t)
  (map issue-block-data (bbs-issue-history id)))

(doc 'section 'dangling-head-detection)

(define (bbs-head-valid? id)
  (doc 'type '(-> String Boolean))
  (doc 'description "Check if a head points to a valid block")
  (doc 'export #t)
  (let ([hash (bbs-read-head id)])
    (and hash
         (bbs-fetch hash)
         #t)))
