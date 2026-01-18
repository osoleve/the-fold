;;; shell/provenance/query.ss — Provenance Query API
;;;
;;; Query the provenance store to understand how values were created.
;;; Primary use case: "Explain this value" - trace the lineage of any
;;; CAS block or value back through all optic operations that created it.
;;;
;;; Features:
;;;   - Query provenance by result hash (what created this?)
;;;   - Query provenance by source hash (what transformations used this?)
;;;   - Full lineage traversal (recursive origin tracking)
;;;   - Human-readable explanations
;;;
;;; This is Shell code: uses filesystem for head lookups.
;;;
;;; Prerequisites (must be loaded before this file):
;;;   - shell/provenance/provenance.ss (or traced-optics.ss which loads it)

;; Note: provenance.ss should already be loaded via traced-optics.ss
;; Don't reload to avoid resetting hashtables

;;; ============================================================
;;; Direct Queries
;;; ============================================================

;;; provenance-for-result : String -> Block | #f
;;; Look up the provenance record that created a result.
;;; Argument: hex hash string of the result value.
(define (provenance-for-result result-hex)
  (let ([head-path (provenance-by-result-path result-hex)])
    (let ([hash (read-head-hash head-path)])
      (and hash (fetch-persistent hash)))))

;;; provenance-for-source : String -> Block | #f
;;; Look up the provenance record that used a source.
;;; Argument: hex hash string of the source value.
(define (provenance-for-source source-hex)
  (let ([head-path (provenance-by-source-path source-hex)])
    (let ([hash (read-head-hash head-path)])
      (and hash (fetch-persistent hash)))))

;;; latest-provenance : -> Block | #f
;;; Get the most recent provenance record.
(define (latest-provenance)
  (let ([hash (read-head-hash (provenance-log-head-path))])
    (and hash (fetch-persistent hash))))

;;; ============================================================
;;; Lineage Traversal
;;; ============================================================

;;; trace-lineage : String -> (List Block)
;;; Trace the full lineage of a value back to its origins.
;;; Returns a list of provenance records, oldest first.
;;;
;;; Algorithm:
;;;   1. Find provenance record that created this result
;;;   2. Get the source hash from that record
;;;   3. Recursively find what created that source
;;;   4. Continue until no more provenance found
(define (trace-lineage result-hex)
  (let loop ([current-hex result-hex]
             [acc '()]
             [visited (make-hashtable string-hash string=?)])
    ;; Prevent infinite loops from circular references
    (if (hashtable-contains? visited current-hex)
        (reverse acc)
        (begin
          (hashtable-set! visited current-hex #t)
          (let ([record (provenance-for-result current-hex)])
            (if (not record)
                (reverse acc)
                (let ([source-hex (provenance-source-hash record)])
                  (loop source-hex (cons record acc) visited))))))))

;;; trace-descendants : String -> (List Block)
;;; Trace all values derived from a source (forward lineage).
;;; Returns provenance records that used this value as source.
(define (trace-descendants source-hex)
  (let loop ([current-hex source-hex]
             [acc '()]
             [visited (make-hashtable string-hash string=?)])
    (if (hashtable-contains? visited current-hex)
        acc
        (begin
          (hashtable-set! visited current-hex #t)
          (let ([record (provenance-for-source current-hex)])
            (if (not record)
                acc
                (let ([result-hex (provenance-result-hash record)]
                      [new-acc (cons record acc)])
                  ;; Continue searching from the result
                  (loop result-hex new-acc visited))))))))

;;; ============================================================
;;; Explain Value API
;;; ============================================================

;;; explain-value : Any -> String
;;; Generate a human-readable explanation of how a value was created.
;;; The value is hashed and looked up in the provenance store.
(define (explain-value value)
  (let* ([hash (hash->hex (store-value! value))]
         [lineage (trace-lineage hash)])
    (if (null? lineage)
        (format "No provenance recorded for this value.\nHash: ~a" hash)
        (format-lineage hash lineage))))

;;; explain-hash : String -> String
;;; Generate explanation for a value given its hex hash.
(define (explain-hash hex)
  (let ([lineage (trace-lineage hex)])
    (if (null? lineage)
        (format "No provenance recorded for hash: ~a" hex)
        (format-lineage hex lineage))))

;;; format-lineage : String (List Block) -> String
;;; Format a lineage as a human-readable explanation.
(define (format-lineage result-hex lineage)
  (let ([lines '()]
        [step 1])
    (set! lines (cons (format "Lineage for ~a" (short-hash result-hex)) lines))
    (set! lines (cons (string-append (make-string 60 #\-)) lines))
    (for-each
     (lambda (record)
       (let* ([op (provenance-operation record)]
              [optic-name (or (provenance-optic-name record) "(anonymous)")]
              [optic-type (provenance-optic-type record)]
              [timestamp (provenance-timestamp record)]
              [agent (or (provenance-agent-id record) "unknown")]
              [source (short-hash (provenance-source-hash record))]
              [result (short-hash (provenance-result-hash record))]
              [value-hash (provenance-value-hash record)])
         (set! lines (cons (format "\nStep ~a: ~a via ~a (~a)" step op optic-name optic-type) lines))
         (set! lines (cons (format "  Source: ~a" source) lines))
         (set! lines (cons (format "  Result: ~a" result) lines))
         (when value-hash
           (set! lines (cons (format "  Value:  ~a" (short-hash value-hash)) lines)))
         (set! lines (cons (format "  Time:   ~a" timestamp) lines))
         (set! lines (cons (format "  Agent:  ~a" agent) lines))
         (set! step (+ step 1))))
     lineage)
    (string-join (reverse lines) "\n")))

;;; short-hash : String -> String
;;; Truncate a hash for display.
(define (short-hash hex)
  (if (> (string-length hex) 16)
      (string-append (substring hex 0 16) "...")
      hex))

;;; string-join : (List String) String -> String
;;; Join strings with a separator.
(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ([rest (cdr strs)]
                 [acc (car strs)])
        (if (null? rest)
            acc
            (loop (cdr rest)
                  (string-append acc sep (car rest)))))))

;;; ============================================================
;;; Structured Queries
;;; ============================================================

;;; provenance-summary : Block -> Alist
;;; Get a summary of a provenance record as an alist.
(define (provenance-summary record)
  (let ([data (provenance-record-data record)])
    (if data
        `((operation . ,(cdr (assq 'operation data)))
          (optic . ,(or (cdr (assq 'optic-name data)) 'anonymous))
          (type . ,(cdr (assq 'optic-type data)))
          (source . ,(short-hash (cdr (assq 'source-hash data))))
          (result . ,(short-hash (cdr (assq 'result-hash data))))
          (timestamp . ,(cdr (assq 'timestamp data)))
          (agent . ,(or (cdr (assq 'agent-id data)) 'unknown)))
        '())))

;;; lineage-summary : String -> (List Alist)
;;; Get summaries for all steps in a lineage.
(define (lineage-summary result-hex)
  (map provenance-summary (trace-lineage result-hex)))

;;; ============================================================
;;; Value Retrieval
;;; ============================================================

;;; provenance-get-source : Block -> Any | #f
;;; Retrieve the source value from a provenance record.
(define (provenance-get-source record)
  (let ([source-hex (provenance-source-hash record)])
    (and source-hex (fetch-value (hex->hash source-hex)))))

;;; provenance-get-result : Block -> Any | #f
;;; Retrieve the result value from a provenance record.
(define (provenance-get-result record)
  (let ([result-hex (provenance-result-hash record)])
    (and result-hex (fetch-value (hex->hash result-hex)))))

;;; provenance-get-value : Block -> Any | #f
;;; Retrieve the set/over value from a provenance record.
(define (provenance-get-value record)
  (let ([value-hex (provenance-value-hash record)])
    (and value-hex (fetch-value (hex->hash value-hex)))))

;;; ============================================================
;;; Searching and Filtering
;;; ============================================================

;;; find-provenance-by-optic : Symbol -> (List Block)
;;; Find all provenance records that used a specific named optic.
;;; Note: This is an expensive operation that scans the store.
(define (find-provenance-by-optic optic-name)
  (let ([results '()])
    (for-each
     (lambda (hash)
       (let ([blk (fetch-persistent hash)])
         (when (and blk (provenance-record? blk))
           (when (eq? (provenance-optic-name blk) optic-name)
             (set! results (cons blk results))))))
     (store-hashes))
    results))

;;; find-provenance-by-operation : Symbol -> (List Block)
;;; Find all provenance records with a specific operation type.
(define (find-provenance-by-operation op)
  (let ([results '()])
    (for-each
     (lambda (hash)
       (let ([blk (fetch-persistent hash)])
         (when (and blk (provenance-record? blk))
           (when (eq? (provenance-operation blk) op)
             (set! results (cons blk results))))))
     (store-hashes))
    results))

;;; find-provenance-by-agent : Symbol -> (List Block)
;;; Find all provenance records from a specific agent.
(define (find-provenance-by-agent agent-id)
  (let ([results '()])
    (for-each
     (lambda (hash)
       (let ([blk (fetch-persistent hash)])
         (when (and blk (provenance-record? blk))
           (when (eq? (provenance-agent-id blk) agent-id)
             (set! results (cons blk results))))))
     (store-hashes))
    results))

;;; ============================================================
;;; Statistics
;;; ============================================================

;;; provenance-stats : -> Alist
;;; Get statistics about provenance records in the store.
(define (provenance-stats)
  (let ([total 0]
        [by-operation (make-hashtable symbol-hash eq?)]
        [by-optic (make-hashtable symbol-hash eq?)]
        [by-agent (make-hashtable symbol-hash eq?)])
    (for-each
     (lambda (hash)
       (let ([blk (fetch-persistent hash)])
         (when (and blk (provenance-record? blk))
           (set! total (+ total 1))
           ;; Count by operation
           (let ([op (provenance-operation blk)])
             (hashtable-update! by-operation op
                                (lambda (n) (+ n 1)) 0))
           ;; Count by optic
           (let ([optic (or (provenance-optic-name blk) 'anonymous)])
             (hashtable-update! by-optic optic
                                (lambda (n) (+ n 1)) 0))
           ;; Count by agent
           (let ([agent (or (provenance-agent-id blk) 'unknown)])
             (hashtable-update! by-agent agent
                                (lambda (n) (+ n 1)) 0)))))
     (store-hashes))
    `((total . ,total)
      (by-operation . ,(hashtable->alist by-operation))
      (by-optic . ,(hashtable->alist by-optic))
      (by-agent . ,(hashtable->alist by-agent)))))

;;; hashtable->alist : Hashtable -> Alist
(define (hashtable->alist ht)
  (let ([keys (vector->list (hashtable-keys ht))])
    (map (lambda (k) (cons k (hashtable-ref ht k #f))) keys)))

;;; ============================================================
;;; REPL Integration
;;; ============================================================

;;; explain : Any -> Void
;;; User-friendly REPL command to explain a value.
(define (explain value)
  (display (explain-value value))
  (newline))

;;; explain-last : -> Void
;;; Explain the most recent traced operation.
(define (explain-last)
  (let ([record (latest-provenance)])
    (if record
        (begin
          (display (format-lineage (provenance-result-hash record)
                                   (list record)))
          (newline))
        (begin
          (display "No provenance records found.")
          (newline)))))

;;; show-lineage : Any -> Void
;;; Display the full lineage of a value.
(define (show-lineage value)
  (display (explain-value value))
  (newline))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Direct Queries:
;;;   provenance-for-result, provenance-for-source, latest-provenance
;;;
;;; Lineage Traversal:
;;;   trace-lineage, trace-descendants
;;;
;;; Explain Value API:
;;;   explain-value, explain-hash, format-lineage
;;;   short-hash, provenance-summary, lineage-summary
;;;
;;; Value Retrieval:
;;;   provenance-get-source, provenance-get-result, provenance-get-value
;;;
;;; Searching:
;;;   find-provenance-by-optic, find-provenance-by-operation
;;;   find-provenance-by-agent
;;;
;;; Statistics:
;;;   provenance-stats
;;;
;;; REPL Integration:
;;;   explain, explain-last, show-lineage
