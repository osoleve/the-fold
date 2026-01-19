;;; boundary/history/store.ss — History Storage/Retrieval
;;;
;;; High-level storage operations for history entries.
;;; Combines CAS storage with head management.
;;;
;;; Reuses the same .store/objects as the rest of The Fold.
;;;
;;; This is Shell code: impure (filesystem IO).

(load "boundary/history/blocks.ss")
(load "boundary/history/heads.ss")

;;; ====
;;; Storage Root (uses same .store as rest of Fold)
;;; ====

(define *history-cas-root* ".store/objects")

;;; ====
;;; Block Persistence
;;; ====

;;; history-cas-path : Bytevector -> String
;;; Compute filesystem path for a block hash.
;;; Uses first byte for sharding.
(define (history-cas-path hash)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [suffix (substring hex 2 (string-length hex))])
    (string-append *history-cas-root* "/" prefix "/" suffix)))

;;; history-ensure-shard-dir! : String -> Void
(define (history-ensure-shard-dir! prefix)
  (let ([shard-dir (string-append *history-cas-root* "/" prefix)])
    (unless (file-exists? ".store")
      (mkdir ".store"))
    (unless (file-exists? *history-cas-root*)
      (mkdir *history-cas-root*))
    (unless (file-exists? shard-dir)
      (mkdir shard-dir))))

;;; history-persist-block! : Bytevector Block -> Void
;;; Write a block to disk.
(define (history-persist-block! hash blk)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [path (history-cas-path hash)])
    (history-ensure-shard-dir! prefix)
    ;; Only write if not already present (content-addressed = idempotent)
    (unless (file-exists? path)
      (let ([bytes (block->bytes blk)])
        (call-with-port
          (open-file-output-port path (file-options no-fail))
          (lambda (port)
            (put-bytevector port bytes)))))))

;;; history-load-block : Bytevector -> Block | #f
;;; Load a block from disk.
(define (history-load-block hash)
  (let ([path (history-cas-path hash)])
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

;;; history-store! : Block -> Bytevector
;;; Store a block and return its hash.
;;; Stores in both memory CAS and on disk.
(define (history-store! blk)
  (let ([hash (hash-block blk)])
    ;; Store in memory CAS
    (store! blk)
    ;; Persist to disk
    (history-persist-block! hash blk)
    hash))

;;; history-fetch : Bytevector -> Block | #f
;;; Fetch a block by hash.
;;; Tries memory first, then disk.
(define (history-fetch hash)
  (or (fetch hash)
      (let ([blk (history-load-block hash)])
        (when blk
          ;; Cache in memory for future fetches
          (store! blk))
        blk)))

;;; ====
;;; Entry Retrieval
;;; ====

;;; history-fetch-entry : String String -> Block | #f
;;; Fetch the current history entry for a session/branch.
(define (history-fetch-entry session-id branch-name)
  (let ([hash (history-read-branch session-id branch-name)])
    (if hash
        (history-fetch hash)
        #f)))

;;; history-fetch-current-entry : String -> Block | #f
;;; Fetch the current entry on the active branch.
(define (history-fetch-current-entry session-id)
  (let ([branch (history-read-current-branch session-id)])
    (history-fetch-entry session-id branch)))

;;; history-fetch-entry-data : String String -> Alist | #f
;;; Fetch and parse entry data.
(define (history-fetch-entry-data session-id branch-name)
  (let ([blk (history-fetch-entry session-id branch-name)])
    (if blk
        (history-entry-data blk)
        #f)))

;;; ====
;;; History Chain Walking
;;; ====

;;; history-walk : String String -> (List Block)
;;; Walk the entire history chain, newest first.
(define (history-walk session-id branch-name)
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

;;; history-walk-current : String -> (List Block)
;;; Walk the history chain on the active branch.
(define (history-walk-current session-id)
  (let ([branch (history-read-current-branch session-id)])
    (history-walk session-id branch)))

;;; history-walk-data : String String -> (List Alist)
;;; Walk history and return parsed data.
(define (history-walk-data session-id branch-name)
  (map history-entry-data (history-walk session-id branch-name)))

;;; ====
;;; Index-Based Access
;;; ====

;;; history-entry-at-index : String String Int -> Block | #f
;;; Get the history entry at a specific index.
;;; Walks the chain to find the entry (O(n) but entries are small).
(define (history-entry-at-index session-id branch-name target-index)
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

;;; history-current-index : String -> Int
;;; Get the current history index (highest index on active branch).
;;; Returns -1 if no history.
(define (history-current-index session-id)
  (let ([entry (history-fetch-current-entry session-id)])
    (if entry
        (let ([data (history-entry-data entry)])
          (cdr (assq 'index data)))
        -1)))

;;; ====
;;; Entry Count
;;; ====

;;; history-count : String String -> Int
;;; Count entries on a branch.
(define (history-count session-id branch-name)
  (length (history-walk session-id branch-name)))

;;; history-count-current : String -> Int
;;; Count entries on the active branch.
(define (history-count-current session-id)
  (let ([branch (history-read-current-branch session-id)])
    (history-count session-id branch)))

;;; ====
;;; Validation
;;; ====

;;; history-head-valid? : String String -> Boolean
;;; Check if a branch head points to a valid block.
(define (history-head-valid? session-id branch-name)
  (let ([hash (history-read-branch session-id branch-name)])
    (and hash
         (history-fetch hash)
         #t)))

;;; history-chain-valid? : String String -> Boolean
;;; Verify the entire history chain is intact.
(define (history-chain-valid? session-id branch-name)
  (let ([hash (history-read-branch session-id branch-name)])
    (if (not hash)
        #t  ; Empty history is valid
        (let loop ([h hash])
          (let ([blk (history-fetch h)])
            (if (not blk)
                #f  ; Broken chain
                (let ([prev (history-entry-prev blk)])
                  (if prev
                      (loop prev)
                      #t))))))))
