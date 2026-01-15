;;; shell/bbs/store.ss — BBS Issue Storage/Retrieval
;;;
;;; High-level storage operations for issues.
;;; Combines CAS storage with head management.
;;;
;;; This is Shell code: impure (filesystem IO).

(load "shell/bbs/blocks.ss")
(load "shell/bbs/heads.ss")

;;; ====
;;; Storage Root (uses same .store as rest of Fold)
;;; ====

(define *bbs-cas-root* ".store/objects")

;;; ====
;;; Block Persistence
;;; ====

;;; bbs-cas-path : Bytevector -> String
;;; Compute filesystem path for a block hash.
;;; Uses first byte for sharding.
(define (bbs-cas-path hash)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [suffix (substring hex 2 (string-length hex))])
    (string-append *bbs-cas-root* "/" prefix "/" suffix)))

;;; bbs-ensure-shard-dir! : String -> Void
;;; Ensure the shard directory exists.
(define (bbs-ensure-shard-dir! prefix)
  (let ([shard-dir (string-append *bbs-cas-root* "/" prefix)])
    (unless (file-exists? ".store")
      (mkdir ".store"))
    (unless (file-exists? *bbs-cas-root*)
      (mkdir *bbs-cas-root*))
    (unless (file-exists? shard-dir)
      (mkdir shard-dir))))

;;; bbs-persist-block! : Bytevector Block -> Void
;;; Write a block to disk.
(define (bbs-persist-block! hash blk)
  (let* ([hex (hash->hex hash)]
         [prefix (substring hex 0 2)]
         [path (bbs-cas-path hash)])
    (bbs-ensure-shard-dir! prefix)
    (let ([bytes (block->bytes blk)])
      (call-with-port
       (open-file-output-port path (file-options no-fail))
       (lambda (port)
         (put-bytevector port bytes))))))

;;; bbs-load-block : Bytevector -> Block | #f
;;; Load a block from disk.
(define (bbs-load-block hash)
  (let ([path (bbs-cas-path hash)])
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

;;; bbs-store! : Block -> Bytevector
;;; Store a block and return its hash.
;;; Stores in both memory CAS and on disk.
(define (bbs-store! blk)
  (let ([hash (hash-block blk)])
    ;; Store in memory CAS
    (store! blk)
    ;; Persist to disk
    (bbs-persist-block! hash blk)
    hash))

;;; bbs-fetch : Bytevector -> Block | #f
;;; Fetch a block by hash.
;;; Tries memory first, then disk.
(define (bbs-fetch hash)
  (or (fetch hash)
      (let ([blk (bbs-load-block hash)])
        (when blk
          ;; Cache in memory for future fetches
          (store! blk))
        blk)))

;;; bbs-fetch-issue : String -> Block | #f
;;; Fetch the current version of an issue by ID.
;;; Uses bbs-issue-hash which auto-refreshes from disk on cache miss.
(define (bbs-fetch-issue id)
  (let ([hash (bbs-issue-hash id)])  ; Auto-refresh on cache miss
    (if hash
        (bbs-fetch hash)
        #f)))

;;; bbs-fetch-issue-data : String -> Alist | #f
;;; Fetch and parse issue data by ID.
(define (bbs-fetch-issue-data id)
  (let ([blk (bbs-fetch-issue id)])
    (if blk
        (issue-block-data blk)
        #f)))

;;; ====
;;; Issue History
;;; ====

;;; bbs-issue-history : String -> (List Block)
;;; Get all versions of an issue, newest first.
(define (bbs-issue-history id)
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

;;; bbs-issue-history-data : String -> (List Alist)
;;; Get all versions as parsed data.
(define (bbs-issue-history-data id)
  (map issue-block-data (bbs-issue-history id)))

;;; ====
;;; Dangling Head Detection
;;; ====

;;; bbs-head-valid? : String -> Boolean
;;; Check if a head points to a valid block.
(define (bbs-head-valid? id)
  (let ([hash (bbs-read-head id)])
    (and hash
         (bbs-fetch hash)
         #t)))
