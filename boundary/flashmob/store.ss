;;; boundary/flashmob/store.ss — Flashmob Storage/Retrieval
;;;
;;; High-level storage operations for flashmob blocks.
;;; Combines CAS storage with head management.
;;;
;;; This is Shell code: impure (filesystem IO).

(load "boundary/flashmob/blocks.ss")
(load "boundary/flashmob/heads.ss")

;;; ====
;;; Storage Root (uses same .store as rest of Fold)
;;; ====

(define *flashmob-cas-root* ".store/objects")

;;; ====
;;; Block Persistence
;;; ====

;;; flashmob-cas-path : Bytevector -> String
;;; Compute filesystem path for a block hash.
;;; Uses first byte for sharding.
(define (flashmob-cas-path hash)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [suffix (substring hex 2 (string-length hex))])
    (string-append *flashmob-cas-root* "/" prefix "/" suffix)))

;;; flashmob-ensure-shard-dir! : String -> Void
;;; Ensure the shard directory exists.
(define (flashmob-ensure-shard-dir! prefix)
  (let ([shard-dir (string-append *flashmob-cas-root* "/" prefix)])
    (unless (file-exists? ".store")
      (mkdir ".store"))
    (unless (file-exists? *flashmob-cas-root*)
      (mkdir *flashmob-cas-root*))
    (unless (file-exists? shard-dir)
      (mkdir shard-dir))))

;;; flashmob-persist-block! : Bytevector Block -> Void
;;; Write a block to disk.
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

;;; flashmob-load-block : Bytevector -> Block | #f
;;; Load a block from disk.
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

;;; ====
;;; High-Level Operations
;;; ====

;;; flashmob-store! : Block -> Bytevector
;;; Store a block and return its hash.
;;; Stores in both memory CAS and on disk.
(define (flashmob-store! blk)
  (let ([hash (hash-block blk)])
    ;; Store in memory CAS
    (store! blk)
    ;; Persist to disk
    (flashmob-persist-block! hash blk)
    hash))

;;; flashmob-fetch : Bytevector -> Block | #f
;;; Fetch a block by hash.
;;; Tries memory first, then disk.
(define (flashmob-fetch hash)
  (or (fetch hash)
      (let ([blk (flashmob-load-block hash)])
        (when blk
          ;; Cache in memory for future fetches
          (store! blk))
        blk)))

;;; ====
;;; Session Operations
;;; ====

;;; flashmob-fetch-session : String -> Block | #f
;;; Fetch the current version of a session by ID.
(define (flashmob-fetch-session id)
  (let ([hash (flashmob-read-head id)])
    (if hash
        (flashmob-fetch hash)
        #f)))

;;; flashmob-fetch-session-data : String -> Alist | #f
;;; Fetch and parse session data by ID.
(define (flashmob-fetch-session-data id)
  (let ([blk (flashmob-fetch-session id)])
    (if blk
        (session-block-data blk)
        #f)))

;;; ====
;;; Finding Operations
;;; ====

;;; flashmob-fetch-finding : Bytevector -> Block | #f
;;; Fetch a finding block by hash.
(define (flashmob-fetch-finding hash)
  (let ([blk (flashmob-fetch hash)])
    (if (and blk (eq? (block-tag blk) FLASHMOB-FINDING))
        blk
        #f)))

;;; flashmob-fetch-finding-data : Bytevector -> Alist | #f
;;; Fetch and parse finding data by hash.
(define (flashmob-fetch-finding-data hash)
  (let ([blk (flashmob-fetch-finding hash)])
    (if blk
        (finding-block-data blk)
        #f)))

;;; ====
;;; Agent Operations
;;; ====

;;; flashmob-fetch-agent : Bytevector -> Block | #f
;;; Fetch an agent block by hash.
(define (flashmob-fetch-agent hash)
  (let ([blk (flashmob-fetch hash)])
    (if (and blk (eq? (block-tag blk) FLASHMOB-AGENT))
        blk
        #f)))

;;; flashmob-fetch-agent-data : Bytevector -> Alist | #f
;;; Fetch and parse agent data by hash.
(define (flashmob-fetch-agent-data hash)
  (let ([blk (flashmob-fetch-agent hash)])
    (if blk
        (agent-block-data blk)
        #f)))

;;; ====
;;; Triage Operations
;;; ====

;;; flashmob-fetch-triage : Bytevector -> Block | #f
;;; Fetch a triage block by hash.
(define (flashmob-fetch-triage hash)
  (let ([blk (flashmob-fetch hash)])
    (if (and blk (eq? (block-tag blk) FLASHMOB-TRIAGE))
        blk
        #f)))

;;; flashmob-fetch-triage-data : Bytevector -> Alist | #f
;;; Fetch and parse triage data by hash.
(define (flashmob-fetch-triage-data hash)
  (let ([blk (flashmob-fetch-triage hash)])
    (if blk
        (triage-block-data blk)
        #f)))

;;; ====
;;; Session History
;;; ====

;;; flashmob-session-history : String -> (List Block)
;;; Get all versions of a session, newest first.
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

;;; flashmob-session-history-data : String -> (List Alist)
;;; Get all versions as parsed data.
(define (flashmob-session-history-data id)
  (map session-block-data (flashmob-session-history id)))

;;; ====
;;; Dangling Head Detection
;;; ====

;;; flashmob-head-valid? : String -> Boolean
;;; Check if a head points to a valid block.
(define (flashmob-head-valid? id)
  (let ([hash (flashmob-read-head id)])
    (and hash
         (flashmob-fetch hash)
         #t)))

;;; ====
;;; Bulk Operations
;;; ====

;;; flashmob-fetch-all-findings : (Vector Bytevector) -> (List Alist)
;;; Fetch and parse all findings from a vector of hashes.
(define (flashmob-fetch-all-findings finding-refs)
  (let ([result '()])
    (do ([i 0 (+ i 1)])
        ((>= i (vector-length finding-refs)) (reverse result))
      (let ([data (flashmob-fetch-finding-data (vector-ref finding-refs i))])
        (when data
          (set! result (cons data result)))))))

;;; flashmob-fetch-all-agents : (Vector Bytevector) -> (List Alist)
;;; Fetch and parse all agents from a vector of hashes.
(define (flashmob-fetch-all-agents agent-refs)
  (let ([result '()])
    (do ([i 0 (+ i 1)])
        ((>= i (vector-length agent-refs)) (reverse result))
      (let ([data (flashmob-fetch-agent-data (vector-ref agent-refs i))])
        (when data
          (set! result (cons data result)))))))
