;;; boundary/storage/store-api.ss — Persistent Block Storage API for The Fold
;;;
;;; A clean, functional API for storing and retrieving blocks from
;;; the content-addressed .store filesystem.
;;;
;;; DESIGN PRINCIPLES:
;;;   • Pure functional interface (except fs-capability mutation)
;;;   • Content-addressed (blocks identified by hash)
;;;   • Composable (works with existing block primitives)
;;;   • Efficient (leverage existing fs.ss operations)
;;;
;;; TIER ASSIGNMENT:
;;;   Tier 3-4: Most operations (disk I/O, minimal computation)
;;;   Tier 5-6: Filtering and searching operations

(source-directories (cons "core" (source-directories)))

;;; NOTE: string utilities provided by core/prelude.ss
(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "boundary/fs.ss")

;;; ====
;;; Core Storage Operations (Tier 3-4)
;;; ====

;;; store-put! : FSCap Block → Hash
;;; Store a block and return its hash.
;;; Pure operation: identical blocks always produce same hash.
(define (store-put! fs block)
  (let ([h (hash-block block)])
       (fs-store! fs block)
       h))

;;; store-get : FSCap Hash → (Maybe Block)
;;; Retrieve a block by hash, or #f if not found.
(define (store-get fs hash)
  (fs-fetch fs hash))

;;; store-exists? : FSCap Hash → Boolean
;;; Check if a block exists in the store.
(define (store-exists? fs hash)
  (not (eq? (fs-fetch fs hash) #f)))

;;; store-all-hashes : FSCap → (List Hash)
;;; Get all hashes in the store.
(define (store-all-hashes fs)
  (fs-all-hashes fs))

;;; store-count : FSCap → Integer
;;; Count total blocks in store.
(define (store-count fs)
  (length (fs-all-hashes fs)))

;;; ====
;;; Filtering and Querying Operations (Tier 5-6)
;;; ====

;;; store-all-blocks : FSCap → (List Block)
;;; Load all blocks from store.
;;; WARNING: Loads everything into memory. Use with care on large stores.
(define (store-all-blocks fs)
  (map (lambda (h) (fs-fetch fs h))
       (fs-all-hashes fs)))

;;; store-filter : FSCap (Block → Boolean) → (List Block)
;;; Find all blocks matching predicate.
;;; Loads blocks lazily during iteration.
(define (store-filter fs predicate)
  (filter predicate (store-all-blocks fs)))

;;; store-find-by-tag : FSCap Symbol → (List Block)
;;; Find all blocks with given tag.
(define (store-find-by-tag fs tag)
  (store-filter fs (lambda (b) (eq? (block-tag b) tag))))

;;; store-find-by-payload : FSCap (Bytevector → Boolean) → (List Block)
;;; Find all blocks where payload matches predicate.
(define (store-find-by-payload fs predicate)
  (store-filter fs (lambda (b) (predicate (block-payload b)))))

;;; store-find-by-payload-contains : FSCap String → (List Block)
;;; Find all blocks whose payload (as UTF-8 string) contains substring.
;;; BUGFIX: Guard against invalid UTF-8 in binary payloads
(define (store-find-by-payload-contains fs substring)
  (store-filter fs (lambda (b)
                           (guard (e [else #f])  ; Return #f if payload isn't valid UTF-8
                                  (let ([payload-str (utf8->string (block-payload b))])
                                       (string-contains? payload-str substring))))))


;;; store-find-by-ref : FSCap Hash → (List Block)
;;; Find all blocks that reference given hash.
(define (store-find-by-ref fs target-hash)
  (store-filter fs (lambda (b)
                           (let ([refs (block-refs b)])
                                (let check-refs ([i 0])
                                     (cond
                                      [(>= i (vector-length refs)) #f]
                                      [(equal? (vector-ref refs i) target-hash) #t]
                                      [else (check-refs (+ i 1))]))))))

;;; ====
;;; Batch Operations (Tier 4-5)
;;; ====

;;; store-put-many! : FSCap (List Block) → (List Hash)
;;; Store multiple blocks and return their hashes.
(define (store-put-many! fs blocks)
  (map (lambda (b) (store-put! fs b)) blocks))

;;; store-get-many : FSCap (List Hash) → (List (Maybe Block))
;;; Retrieve multiple blocks by hash.
(define (store-get-many fs hashes)
  (map (lambda (h) (store-get fs h)) hashes))

;;; ====
;;; Statistics and Inspection (Tier 5-6)
;;; ====

;;; store-stats : FSCap → Alist
;;; Compute statistics about the store.
;;; Returns: ((total . N) (by-tag . ((tag . count) ...)) (total-bytes . N))
(define (store-stats fs)
  (let* ([all-blocks (store-all-blocks fs)]
         [total (length all-blocks)]
         [total-bytes (fold-left (lambda (acc b)
                                         (+ acc (bytevector-length (block-payload b))))
                                 0
                                 all-blocks)]
         [by-tag (count-by-tag all-blocks)])
        `((total . ,total)
          (by-tag . ,by-tag)
          (total-bytes . ,total-bytes))))

;;; Helper: count-by-tag
(define (count-by-tag blocks)
  (let count-loop ([blocks blocks]
                   [counts '()])
       (if (null? blocks)
           counts
           (let* ([tag (block-tag (car blocks))]
                  [existing (assq tag counts)])
                 (count-loop (cdr blocks)
                             (if existing
                                 (map (lambda (pair)
                                              (if (eq? (car pair) tag)
                                                  (cons tag (+ 1 (cdr pair)))
                                                  pair))
                                      counts)
                                 (cons (cons tag 1) counts)))))))

;;; store-print-stats : FSCap → Void
;;; Print human-readable store statistics.
(define (store-print-stats fs)
  (let ([stats (store-stats fs)])
       (printf "Store Statistics:\n")
       (printf "  Total blocks: ~a\n" (cdr (assq 'total stats)))
       (printf "  Total bytes:  ~a\n" (cdr (assq 'total-bytes stats)))
       (printf "  By tag:\n")
       (for-each (lambda (pair)
                         (printf "    ~a: ~a\n" (car pair) (cdr pair)))
                 (cdr (assq 'by-tag stats)))))

;;; ====
;;; Graph Navigation Helpers (Tier 5-6)
;;; ====

;;; store-get-refs : FSCap Block → (List Block)
;;; Get all blocks referenced by given block.
(define (store-get-refs fs block)
  (let ([refs (block-refs block)])
       (let collect-refs ([i 0]
                          [result '()])
            (if (>= i (vector-length refs))
                (reverse result)
                (let ([ref-hash (vector-ref refs i)])
                     (let ([ref-block (store-get fs ref-hash)])
                          (collect-refs (+ i 1)
                                        (if ref-block
                                            (cons ref-block result)
                                            result))))))))

;;; store-get-referrers : FSCap Hash → (List Block)
;;; Get all blocks that reference given hash.
;;; Alias for store-find-by-ref for clarity.
(define (store-get-referrers fs hash)
  (store-find-by-ref fs hash))

;;; ====
;;; Knowledge Graph Helpers (Tier 6)
;;; ====

;;; store-get-entities : FSCap → (List Block)
;;; Get all entity blocks.
(define (store-get-entities fs)
  (store-find-by-tag fs 'entity))

;;; store-get-relations : FSCap → (List Block)
;;; Get all relation blocks.
(define (store-get-relations fs)
  (store-find-by-tag fs 'relation))

;;; store-get-collections : FSCap → (List Block)
;;; Get all collection blocks.
(define (store-get-collections fs)
  (store-find-by-tag fs 'collection))

(printf "✓ Store API loaded\n")
(printf "  Functions available:\n")
(printf "    Core:   store-put!, store-get, store-exists?\n")
(printf "    Query:  store-find-by-tag, store-find-by-payload-contains\n")
(printf "    Graph:  store-get-refs, store-get-referrers\n")
(printf "    Stats:  store-stats, store-print-stats\n")
