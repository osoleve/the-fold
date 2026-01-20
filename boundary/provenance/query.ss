(load "core/base/prelude.ss")

(doc 'module 'provenance-query)
(doc 'description "Query the provenance store to understand how values were created")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'direct-queries)

(define (provenance-for-result result-hex)
  (doc 'description "Look up the provenance record that created a result")
  (let ([head-path (provenance-by-result-path result-hex)])
    (let ([hash (read-head-hash head-path)])
      (and hash (fetch-persistent hash)))))

(define (provenance-for-source source-hex)
  (doc 'description "Look up the provenance record that used a source")
  (let ([head-path (provenance-by-source-path source-hex)])
    (let ([hash (read-head-hash head-path)])
      (and hash (fetch-persistent hash)))))

(define (latest-provenance)
  (doc 'description "Get the most recent provenance record")
  (let ([hash (read-head-hash (provenance-log-head-path))])
    (and hash (fetch-persistent hash))))

(doc 'section 'lineage-traversal)

(define (trace-lineage result-hex)
  (doc 'description "Trace the full lineage of a value back to its origins")
  (doc 'returns "(List Block) - Provenance records, oldest first")
  (let loop ([current-hex result-hex]
             [acc '()]
             [visited (make-hashtable string-hash string=?)])
    (if (hashtable-contains? visited current-hex)
        (reverse acc)
        (begin
          (hashtable-set! visited current-hex #t)
          (let ([record (provenance-for-result current-hex)])
            (if (not record)
                (reverse acc)
                (let ([source-hex (provenance-source-hash record)])
                  (loop source-hex (cons record acc) visited))))))))

(define (trace-descendants source-hex)
  (doc 'description "Trace all values derived from a source (forward lineage)")
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
                  (loop result-hex new-acc visited))))))))

(doc 'section 'explain-value-api)

(define (explain-value value)
  (doc 'description "Generate human-readable explanation of how a value was created")
  (let* ([hash (hash->hex (store-value! value))]
         [lineage (trace-lineage hash)])
    (if (null? lineage)
        (format "No provenance recorded for this value.\nHash: ~a" hash)
        (format-lineage hash lineage))))

(define (explain-hash hex)
  (doc 'description "Generate explanation for a value given its hex hash")
  (let ([lineage (trace-lineage hex)])
    (if (null? lineage)
        (format "No provenance recorded for hash: ~a" hex)
        (format-lineage hex lineage))))

(define (format-lineage result-hex lineage)
  (doc 'description "Format a lineage as a human-readable explanation")
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

(define (short-hash hex)
  (doc 'description "Truncate a hash for display")
  (if (> (string-length hex) 16)
      (string-append (substring hex 0 16) "...")
      hex))

(define (string-join strs sep)
  (doc 'description "Join strings with a separator")
  (if (null? strs)
      ""
      (let loop ([rest (cdr strs)]
                 [acc (car strs)])
        (if (null? rest)
            acc
            (loop (cdr rest)
                  (string-append acc sep (car rest)))))))

(doc 'section 'structured-queries)

(define (provenance-summary record)
  (doc 'description "Get a summary of a provenance record as an alist")
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

(define (lineage-summary result-hex)
  (doc 'description "Get summaries for all steps in a lineage")
  (map provenance-summary (trace-lineage result-hex)))

(doc 'section 'value-retrieval)

(define (provenance-get-source record)
  (doc 'description "Retrieve the source value from a provenance record")
  (let ([source-hex (provenance-source-hash record)])
    (and source-hex (fetch-value (hex->hash source-hex)))))

(define (provenance-get-result record)
  (doc 'description "Retrieve the result value from a provenance record")
  (let ([result-hex (provenance-result-hash record)])
    (and result-hex (fetch-value (hex->hash result-hex)))))

(define (provenance-get-value record)
  (doc 'description "Retrieve the set/over value from a provenance record")
  (let ([value-hex (provenance-value-hash record)])
    (and value-hex (fetch-value (hex->hash value-hex)))))

(doc 'section 'searching-and-filtering)

(define (find-provenance-by-optic optic-name)
  (doc 'description "Find all provenance records that used a specific named optic")
  (doc 'note "Expensive operation that scans the store")
  (let ([results '()])
    (for-each
     (lambda (hash)
       (let ([blk (fetch-persistent hash)])
         (when (and blk (provenance-record? blk))
           (when (eq? (provenance-optic-name blk) optic-name)
             (set! results (cons blk results))))))
     (store-hashes))
    results))

(define (find-provenance-by-operation op)
  (doc 'description "Find all provenance records with a specific operation type")
  (let ([results '()])
    (for-each
     (lambda (hash)
       (let ([blk (fetch-persistent hash)])
         (when (and blk (provenance-record? blk))
           (when (eq? (provenance-operation blk) op)
             (set! results (cons blk results))))))
     (store-hashes))
    results))

(define (find-provenance-by-agent agent-id)
  (doc 'description "Find all provenance records from a specific agent")
  (let ([results '()])
    (for-each
     (lambda (hash)
       (let ([blk (fetch-persistent hash)])
         (when (and blk (provenance-record? blk))
           (when (eq? (provenance-agent-id blk) agent-id)
             (set! results (cons blk results))))))
     (store-hashes))
    results))

(doc 'section 'statistics)

(define (provenance-stats)
  (doc 'description "Get statistics about provenance records in the store")
  (let ([total 0]
        [by-operation (make-hashtable symbol-hash eq?)]
        [by-optic (make-hashtable symbol-hash eq?)]
        [by-agent (make-hashtable symbol-hash eq?)])
    (for-each
     (lambda (hash)
       (let ([blk (fetch-persistent hash)])
         (when (and blk (provenance-record? blk))
           (set! total (+ total 1))
           (let ([op (provenance-operation blk)])
             (hashtable-update! by-operation op
                                (lambda (n) (+ n 1)) 0))
           (let ([optic (or (provenance-optic-name blk) 'anonymous)])
             (hashtable-update! by-optic optic
                                (lambda (n) (+ n 1)) 0))
           (let ([agent (or (provenance-agent-id blk) 'unknown)])
             (hashtable-update! by-agent agent
                                (lambda (n) (+ n 1)) 0)))))
     (store-hashes))
    `((total . ,total)
      (by-operation . ,(hashtable->alist by-operation))
      (by-optic . ,(hashtable->alist by-optic))
      (by-agent . ,(hashtable->alist by-agent)))))

(define (hashtable->alist ht)
  (doc 'description "Convert hashtable to alist")
  (let ([keys (vector->list (hashtable-keys ht))])
    (map (lambda (k) (cons k (hashtable-ref ht k #f))) keys)))

(doc 'section 'repl-integration)

(define (explain value)
  (doc 'description "User-friendly REPL command to explain a value")
  (display (explain-value value))
  (newline))

(define (explain-last)
  (doc 'description "Explain the most recent traced operation")
  (let ([record (latest-provenance)])
    (if record
        (begin
          (display (format-lineage (provenance-result-hash record)
                                   (list record)))
          (newline))
        (begin
          (display "No provenance records found.")
          (newline)))))

(define (show-lineage value)
  (doc 'description "Display the full lineage of a value")
  (display (explain-value value))
  (newline))
