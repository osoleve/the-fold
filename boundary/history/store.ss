(load "boundary/history/blocks.ss")
(load "boundary/history/heads.ss")

(doc 'module 'boundary/history/store)
(doc 'description "History storage/retrieval combining CAS with head management")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/history/blocks boundary/history/heads))
(doc 'note "Reuses same .store/objects as rest of The Fold")

(doc 'section 'storage-root)

(doc *history-cas-root* 'type 'string)
(doc *history-cas-root* 'description "Storage root (shared with main CAS)")
(define *history-cas-root* ".store/objects")

(doc 'section 'block-persistence)

(define (history-cas-path hash)
  (doc 'type (-> Bytevector String))
  (doc 'description "Compute filesystem path for a block hash (first byte sharding)")
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [suffix (substring hex 2 (string-length hex))])
    (string-append *history-cas-root* "/" prefix "/" suffix)))

(define (history-ensure-shard-dir! prefix)
  (doc 'type (-> String Void))
  (doc 'description "Ensure shard directory exists")
  (let ([shard-dir (string-append *history-cas-root* "/" prefix)])
    (unless (file-exists? ".store")
      (mkdir ".store"))
    (unless (file-exists? *history-cas-root*)
      (mkdir *history-cas-root*))
    (unless (file-exists? shard-dir)
      (mkdir shard-dir))))

(define (history-persist-block! hash blk)
  (doc 'type (-> Bytevector Block Void))
  (doc 'description "Write a block to disk (idempotent)")
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [path (history-cas-path hash)])
    (history-ensure-shard-dir! prefix)
    (unless (file-exists? path)
      (let ([bytes (block->bytes blk)])
        (call-with-port
          (open-file-output-port path (file-options no-fail))
          (lambda (port)
            (put-bytevector port bytes)))))))

(define (history-load-block hash)
  (doc 'type (-> Bytevector (Option Block)))
  (doc 'description "Load a block from disk")
  (let ([path (history-cas-path hash)])
    (guard (e [else #f])
      (if (file-exists? path)
          (let ([bytes (call-with-port
                         (open-file-input-port path)
                         (lambda (port)
                           (get-bytevector-all port)))])
            (bytes->block bytes))
          #f))))

(doc 'section 'high-level-operations)

(define (history-store! blk)
  (doc 'type (-> Block Bytevector))
  (doc 'description "Store a block in both memory CAS and on disk")
  (doc 'export #t)
  (doc 'returns "Hash of the stored block")
  (let ([hash (hash-block blk)])
    (store! blk)
    (history-persist-block! hash blk)
    hash))

(define (history-fetch hash)
  (doc 'type (-> Bytevector (Option Block)))
  (doc 'description "Fetch a block by hash (tries memory first, then disk)")
  (doc 'export #t)
  (or (fetch hash)
      (let ([blk (history-load-block hash)])
        (when blk
          (store! blk))
        blk)))

(doc 'section 'entry-retrieval)

(define (history-fetch-entry session-id branch-name)
  (doc 'type (-> String String (Option Block)))
  (doc 'description "Fetch the current history entry for a session/branch")
  (doc 'export #t)
  (let ([hash (history-read-branch session-id branch-name)])
    (if hash
        (history-fetch hash)
        #f)))

(define (history-fetch-current-entry session-id)
  (doc 'type (-> String (Option Block)))
  (doc 'description "Fetch the current entry on the active branch")
  (doc 'export #t)
  (let ([branch (history-read-current-branch session-id)])
    (history-fetch-entry session-id branch)))

(define (history-fetch-entry-data session-id branch-name)
  (doc 'type (-> String String (Option Alist)))
  (doc 'description "Fetch and parse entry data")
  (doc 'export #t)
  (let ([blk (history-fetch-entry session-id branch-name)])
    (if blk
        (history-entry-data blk)
        #f)))

(doc 'section 'history-chain-walking)

(define (history-walk session-id branch-name)
  (doc 'type (-> String String (List Block)))
  (doc 'description "Walk the entire history chain, newest first")
  (doc 'export #t)
  (let ([hash (history-read-branch session-id branch-name)])
    (if (not hash)
        '()
        (let loop ([h hash] [acc '()])
          (let ([blk (history-fetch h)])
            (if (not blk)
                (reverse acc)
                (let ([prev (history-entry-prev blk)])
                  (if prev
                      (loop prev (cons blk acc))
                      (reverse (cons blk acc))))))))))

(define (history-walk-current session-id)
  (doc 'type (-> String (List Block)))
  (doc 'description "Walk the history chain on the active branch")
  (doc 'export #t)
  (let ([branch (history-read-current-branch session-id)])
    (history-walk session-id branch)))

(define (history-walk-data session-id branch-name)
  (doc 'type (-> String String (List Alist)))
  (doc 'description "Walk history and return parsed data")
  (doc 'export #t)
  (map history-entry-data (history-walk session-id branch-name)))

(doc 'section 'index-based-access)

(define (history-entry-at-index session-id branch-name target-index)
  (doc 'type (-> String String Int (Option Block)))
  (doc 'description "Get the history entry at a specific index (O(n) walk)")
  (doc 'export #t)
  (let ([entries (history-walk session-id branch-name)])
    (let loop ([entries entries])
      (if (null? entries)
          #f
          (let* ([entry (car entries)]
                 [data (history-entry-data entry)]
                 [index (cdr (assq 'index data))])
            (if (= index target-index)
                entry
                (loop (cdr entries))))))))

(define (history-current-index session-id)
  (doc 'type (-> String Int))
  (doc 'description "Get the current history index (highest index on active branch)")
  (doc 'export #t)
  (doc 'returns "-1 if no history")
  (let ([entry (history-fetch-current-entry session-id)])
    (if entry
        (let ([data (history-entry-data entry)])
          (cdr (assq 'index data)))
        -1)))

(doc 'section 'entry-count)

(define (history-count session-id branch-name)
  (doc 'type (-> String String Int))
  (doc 'description "Count entries on a branch")
  (doc 'export #t)
  (length (history-walk session-id branch-name)))

(define (history-count-current session-id)
  (doc 'type (-> String Int))
  (doc 'description "Count entries on the active branch")
  (doc 'export #t)
  (let ([branch (history-read-current-branch session-id)])
    (history-count session-id branch)))

(doc 'section 'validation)

(define (history-head-valid? session-id branch-name)
  (doc 'type (-> String String Boolean))
  (doc 'description "Check if a branch head points to a valid block")
  (doc 'export #t)
  (let ([hash (history-read-branch session-id branch-name)])
    (and hash
         (history-fetch hash)
         #t)))

(define (history-chain-valid? session-id branch-name)
  (doc 'type (-> String String Boolean))
  (doc 'description "Verify the entire history chain is intact")
  (doc 'export #t)
  (let ([hash (history-read-branch session-id branch-name)])
    (if (not hash)
        #t
        (let loop ([h hash])
          (let ([blk (history-fetch h)])
            (if (not blk)
                #f
                (let ([prev (history-entry-prev blk)])
                  (if prev
                      (loop prev)
                      #t))))))))
