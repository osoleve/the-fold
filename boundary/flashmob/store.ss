(load "boundary/flashmob/blocks.ss")
(load "boundary/flashmob/heads.ss")

(doc 'module 'flashmob/store)
(doc 'description "Flashmob Storage/Retrieval - High-level storage operations for flashmob blocks, combines CAS storage with head management")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'storage-root)
(doc 'note "Uses same .store as rest of Fold")

(define *flashmob-cas-root* ".store/objects")

(doc 'section 'block-persistence)

(doc flashmob-cas-path 'type (-> Bytevector String))
(doc flashmob-cas-path 'description "Compute filesystem path for a block hash using first byte for sharding")
(define (flashmob-cas-path hash)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [suffix (substring hex 2 (string-length hex))])
    (string-append *flashmob-cas-root* "/" prefix "/" suffix)))

(doc flashmob-ensure-shard-dir! 'type (-> String Void))
(doc flashmob-ensure-shard-dir! 'description "Ensure the shard directory exists")
(define (flashmob-ensure-shard-dir! prefix)
  (let ([shard-dir (string-append *flashmob-cas-root* "/" prefix)])
    (unless (file-exists? ".store")
      (mkdir ".store"))
    (unless (file-exists? *flashmob-cas-root*)
      (mkdir *flashmob-cas-root*))
    (unless (file-exists? shard-dir)
      (mkdir shard-dir))))

(doc flashmob-persist-block! 'type (-> Bytevector Block Void))
(doc flashmob-persist-block! 'description "Write a block to disk")
(define (flashmob-persist-block! hash blk)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [path (flashmob-cas-path hash)])
    (flashmob-ensure-shard-dir! prefix)
    (let ([bytes (block->bytes blk)])
      (call-with-port
       (open-file-output-port path (file-options no-fail))
       (lambda (port)
         (put-bytevector port bytes))))))

(doc flashmob-load-block 'type (-> Bytevector (U Block #f)))
(doc flashmob-load-block 'description "Load a block from disk")
(define (flashmob-load-block hash)
  (let ([path (flashmob-cas-path hash)])
    (guard (e [else #f])
      (if (file-exists? path)
          (let ([bytes (call-with-port
                        (open-file-input-port path)
                        (lambda (port)
                          (get-bytevector-all port)))])
            (bytes->block bytes))
          #f))))

(doc 'section 'high-level-operations)

(doc flashmob-store! 'type (-> Block Bytevector))
(doc flashmob-store! 'description "Store a block and return its hash - stores in both memory CAS and on disk")
(define (flashmob-store! blk)
  (let ([hash (hash-block blk)])
    ;; Store in memory CAS
    (store! blk)
    ;; Persist to disk
    (flashmob-persist-block! hash blk)
    hash))

(doc flashmob-fetch 'type (-> Bytevector (U Block #f)))
(doc flashmob-fetch 'description "Fetch a block by hash - tries memory first, then disk")
(define (flashmob-fetch hash)
  (or (fetch hash)
      (let ([blk (flashmob-load-block hash)])
        (when blk
          ;; Cache in memory for future fetches
          (store! blk))
        blk)))

(doc 'section 'session-operations)

(doc flashmob-fetch-session 'type (-> String (U Block #f)))
(doc flashmob-fetch-session 'description "Fetch the current version of a session by ID")
(define (flashmob-fetch-session id)
  (let ([hash (flashmob-read-head id)])
    (if hash
        (flashmob-fetch hash)
        #f)))

(doc flashmob-fetch-session-data 'type (-> String (U Alist #f)))
(doc flashmob-fetch-session-data 'description "Fetch and parse session data by ID")
(define (flashmob-fetch-session-data id)
  (let ([blk (flashmob-fetch-session id)])
    (if blk
        (session-block-data blk)
        #f)))

(doc 'section 'finding-operations)

(doc flashmob-fetch-finding 'type (-> Bytevector (U Block #f)))
(doc flashmob-fetch-finding 'description "Fetch a finding block by hash")
(define (flashmob-fetch-finding hash)
  (let ([blk (flashmob-fetch hash)])
    (if (and blk (eq? (block-tag blk) FLASHMOB-FINDING))
        blk
        #f)))

(doc flashmob-fetch-finding-data 'type (-> Bytevector (U Alist #f)))
(doc flashmob-fetch-finding-data 'description "Fetch and parse finding data by hash")
(define (flashmob-fetch-finding-data hash)
  (let ([blk (flashmob-fetch-finding hash)])
    (if blk
        (finding-block-data blk)
        #f)))

(doc 'section 'agent-operations)

(doc flashmob-fetch-agent 'type (-> Bytevector (U Block #f)))
(doc flashmob-fetch-agent 'description "Fetch an agent block by hash")
(define (flashmob-fetch-agent hash)
  (let ([blk (flashmob-fetch hash)])
    (if (and blk (eq? (block-tag blk) FLASHMOB-AGENT))
        blk
        #f)))

(doc flashmob-fetch-agent-data 'type (-> Bytevector (U Alist #f)))
(doc flashmob-fetch-agent-data 'description "Fetch and parse agent data by hash")
(define (flashmob-fetch-agent-data hash)
  (let ([blk (flashmob-fetch-agent hash)])
    (if blk
        (agent-block-data blk)
        #f)))

(doc 'section 'triage-operations)

(doc flashmob-fetch-triage 'type (-> Bytevector (U Block #f)))
(doc flashmob-fetch-triage 'description "Fetch a triage block by hash")
(define (flashmob-fetch-triage hash)
  (let ([blk (flashmob-fetch hash)])
    (if (and blk (eq? (block-tag blk) FLASHMOB-TRIAGE))
        blk
        #f)))

(doc flashmob-fetch-triage-data 'type (-> Bytevector (U Alist #f)))
(doc flashmob-fetch-triage-data 'description "Fetch and parse triage data by hash")
(define (flashmob-fetch-triage-data hash)
  (let ([blk (flashmob-fetch-triage hash)])
    (if blk
        (triage-block-data blk)
        #f)))

(doc 'section 'session-history)

(doc flashmob-session-history 'type (-> String (List Block)))
(doc flashmob-session-history 'description "Get all versions of a session, newest first")
(define (flashmob-session-history id)
  (let ([hash (flashmob-read-head id)])
    (if (not hash)
        '()
        (let loop ([h hash] [acc '()])
          (let ([blk (flashmob-fetch h)])
            (if (not blk)
                (reverse acc)
                (let ([prev (session-block-prev blk)])
                  (if prev
                      (loop prev (cons blk acc))
                      (reverse (cons blk acc))))))))))

(doc flashmob-session-history-data 'type (-> String (List Alist)))
(doc flashmob-session-history-data 'description "Get all versions as parsed data")
(define (flashmob-session-history-data id)
  (map session-block-data (flashmob-session-history id)))

(doc 'section 'dangling-head-detection)

(doc flashmob-head-valid? 'type (-> String Boolean))
(doc flashmob-head-valid? 'description "Check if a head points to a valid block")
(define (flashmob-head-valid? id)
  (let ([hash (flashmob-read-head id)])
    (and hash
         (flashmob-fetch hash)
         #t)))

(doc 'section 'bulk-operations)

(doc flashmob-fetch-all-findings 'type (-> (Vector Bytevector) (List Alist)))
(doc flashmob-fetch-all-findings 'description "Fetch and parse all findings from a vector of hashes")
(define (flashmob-fetch-all-findings finding-refs)
  (let ([result '()])
    (do ([i 0 (+ i 1)])
        ((>= i (vector-length finding-refs)) (reverse result))
      (let ([data (flashmob-fetch-finding-data (vector-ref finding-refs i))])
        (when data
          (set! result (cons data result)))))))

(doc flashmob-fetch-all-agents 'type (-> (Vector Bytevector) (List Alist)))
(doc flashmob-fetch-all-agents 'description "Fetch and parse all agents from a vector of hashes")
(define (flashmob-fetch-all-agents agent-refs)
  (let ([result '()])
    (do ([i 0 (+ i 1)])
        ((>= i (vector-length agent-refs)) (reverse result))
      (let ([data (flashmob-fetch-agent-data (vector-ref agent-refs i))])
        (when data
          (set! result (cons data result)))))))
