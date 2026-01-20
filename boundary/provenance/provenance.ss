(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")
(load "boundary/storage/cas-persist.ss")

(doc 'module 'provenance)
(doc 'description "Provenance tracking for optic operations with full data lineage")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Creates CAS blocks for every traced optic operation")

(doc 'section 'configuration)

(define *provenance-enabled?* #t)

(define (provenance-enable!)
  (doc 'description "Enable provenance tracking")
  (set! *provenance-enabled?* #t))

(define (provenance-disable!)
  (doc 'description "Disable provenance tracking")
  (set! *provenance-enabled?* #f))

(define *current-agent-id* #f)
(define *current-session-id* #f)

(define (with-agent-id agent-id thunk)
  (doc 'description "Execute thunk with a specific agent identity")
  (let ([old *current-agent-id*])
    (dynamic-wind
      (lambda () (set! *current-agent-id* agent-id))
      thunk
      (lambda () (set! *current-agent-id* old)))))

(define (with-session-id session-id thunk)
  (doc 'description "Execute thunk with a specific session identity")
  (let ([old *current-session-id*])
    (dynamic-wind
      (lambda () (set! *current-session-id* session-id))
      thunk
      (lambda () (set! *current-session-id* old)))))

(doc 'section 'timestamp-utility)

(define (current-iso8601-timestamp)
  (doc 'description "Get current time as ISO8601 string")
  (let* ([time-utc (current-time 'time-utc)]
         [date (time-utc->date time-utc 0)])
    (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year date)
            (date-month date)
            (date-day date)
            (date-hour date)
            (date-minute date)
            (date-second date))))

(doc 'section 'optic-name-registry)

(define *optic-registry* (make-eq-hashtable))
(define *optic-registry-reverse* (make-hashtable symbol-hash eq?))

(define (register-optic! name optic)
  (doc 'description "Register an optic with a symbolic name")
  (hashtable-set! *optic-registry* optic name)
  (hashtable-set! *optic-registry-reverse* name optic))

(define (lookup-optic-name optic)
  (doc 'description "Look up the registered name for an optic")
  (hashtable-ref *optic-registry* optic #f))

(define (lookup-optic name)
  (doc 'description "Look up an optic by its registered name")
  (hashtable-ref *optic-registry-reverse* name #f))

(define (list-registered-optics)
  (doc 'description "List all registered optics")
  (let ([names (vector->list (hashtable-keys *optic-registry-reverse*))])
    (map (lambda (name)
           (cons name (hashtable-ref *optic-registry-reverse* name #f)))
         names)))

(doc 'section 'provenance-record-schema)

(define PROVENANCE-TAG 'provenance/record)
(define PROVENANCE-VERSION 1)

(doc 'section 'provenance-head-management)

(define *provenance-heads-dir* ".store/heads/provenance")
(define *provenance-by-result-dir* ".store/heads/provenance/by-result")
(define *provenance-by-source-dir* ".store/heads/provenance/by-source")

(define (ensure-provenance-dirs!)
  (doc 'description "Ensure provenance directory structure exists")
  (unless (file-exists? ".store")
    (mkdir ".store"))
  (unless (file-exists? ".store/heads")
    (mkdir ".store/heads"))
  (unless (file-exists? *provenance-heads-dir*)
    (mkdir *provenance-heads-dir*))
  (unless (file-exists? *provenance-by-result-dir*)
    (mkdir *provenance-by-result-dir*))
  (unless (file-exists? *provenance-by-source-dir*)
    (mkdir *provenance-by-source-dir*)))

(define (valid-hex-string? s)
  (doc 'description "Validate hex string to prevent path traversal attacks")
  (and (string? s)
       (> (string-length s) 0)
       (let loop ([i 0])
         (if (>= i (string-length s))
             #t
             (let ([c (string-ref s i)])
               (and (or (char<=? #\0 c #\9)
                        (char<=? #\a c #\f)
                        (char<=? #\A c #\F))
                    (loop (+ i 1))))))))

(define (provenance-log-head-path)
  (doc 'description "Path for global provenance log head")
  (string-append *provenance-heads-dir* "/log.head"))

(define (provenance-by-result-path result-hex)
  (doc 'description "Path for provenance head keyed by result hash")
  (unless (valid-hex-string? result-hex)
    (error 'provenance-by-result-path "invalid hex string" result-hex))
  (string-append *provenance-by-result-dir* "/"
                 (substring result-hex 0 (min 16 (string-length result-hex)))
                 ".head"))

(define (provenance-by-source-path source-hex)
  (doc 'description "Path for provenance head keyed by source hash")
  (unless (valid-hex-string? source-hex)
    (error 'provenance-by-source-path "invalid hex string" source-hex))
  (string-append *provenance-by-source-dir* "/"
                 (substring source-hex 0 (min 16 (string-length source-hex)))
                 ".head"))

(define (read-head-hash path)
  (doc 'description "Read hash from head file")
  (guard (e [else #f])
    (and (file-exists? path)
         (let ([hex-str (call-with-input-file path get-line)])
           (and hex-str (hex->hash hex-str))))))

(define (write-head-hash! path hash)
  (doc 'description "Write hash to head file")
  (ensure-provenance-dirs!)
  (call-with-output-file path
    (lambda (p) (put-string p (hash->hex hash)))
    'replace))

(doc 'section 'value-storage)

(define (store-value! value)
  (doc 'description "Store a value as a CAS block and return its hash")
  (let* ([payload (string->utf8 (format "~s" value))]
         [blk (make-block 'value payload empty-refs)])
    (store-persistent! blk)))

(define (fetch-value hash)
  (doc 'description "Fetch a value from the CAS by hash")
  (let ([blk (fetch-persistent hash)])
    (and blk
         (eq? (block-tag blk) 'value)
         (guard (e [else #f])
           (read (open-input-string (utf8->string (block-payload blk))))))))

(doc 'section 'provenance-record-creation)

(define (make-provenance-record operation optic-name optic-type
                                 source-hash result-hash value-hash)
  (doc 'description "Create a provenance record block")
  (let* ([payload-data `((operation . ,operation)
                         (optic-name . ,optic-name)
                         (optic-type . ,optic-type)
                         (source-hash . ,source-hash)
                         (result-hash . ,result-hash)
                         (value-hash . ,value-hash)
                         (timestamp . ,(current-iso8601-timestamp))
                         (agent-id . ,*current-agent-id*)
                         (session-id . ,*current-session-id*)
                         (version . ,PROVENANCE-VERSION))]
         [payload (string->utf8 (format "~s" payload-data))]
         [source-hash-bv (hex->hash source-hash)]
         [result-hash-bv (hex->hash result-hash)]
         [refs (if value-hash
                   (vector source-hash-bv result-hash-bv (hex->hash value-hash))
                   (vector source-hash-bv result-hash-bv))])
    (make-block PROVENANCE-TAG payload refs)))

(define (store-provenance! record)
  (doc 'description "Store a provenance record and update all relevant head pointers")
  (let* ([hash (store-persistent! record)]
         [data (provenance-record-data record)]
         [source-hex (cdr (assq 'source-hash data))]
         [result-hex (cdr (assq 'result-hash data))])
    (write-head-hash! (provenance-log-head-path) hash)
    (write-head-hash! (provenance-by-result-path result-hex) hash)
    (write-head-hash! (provenance-by-source-path source-hex) hash)
    hash))

(doc 'section 'provenance-record-access)

(define (provenance-record? blk)
  (doc 'description "Check if block is a provenance record")
  (and (block? blk)
       (eq? (block-tag blk) PROVENANCE-TAG)))

(define (provenance-record-data blk)
  (doc 'description "Extract provenance data alist from block")
  (and (provenance-record? blk)
       (guard (e [else #f])
         (read (open-input-string (utf8->string (block-payload blk)))))))

(define (provenance-field blk field)
  (doc 'description "Get a specific field from provenance record")
  (let ([data (provenance-record-data blk)])
    (and data
         (let ([pair (assq field data)])
           (and pair (cdr pair))))))

(define (provenance-operation blk) (provenance-field blk 'operation))
(define (provenance-optic-name blk) (provenance-field blk 'optic-name))
(define (provenance-optic-type blk) (provenance-field blk 'optic-type))
(define (provenance-source-hash blk) (provenance-field blk 'source-hash))
(define (provenance-result-hash blk) (provenance-field blk 'result-hash))
(define (provenance-value-hash blk) (provenance-field blk 'value-hash))
(define (provenance-timestamp blk) (provenance-field blk 'timestamp))
(define (provenance-agent-id blk) (provenance-field blk 'agent-id))
(define (provenance-session-id blk) (provenance-field blk 'session-id))

(doc 'section 'core-recording)

(define (record-optic-operation! operation optic source result value)
  (doc 'description "Record an optic operation to the provenance store")
  (doc 'note "Returns result unchanged (passthrough for tracing)")
  (when *provenance-enabled?*
    (let* ([optic-name (lookup-optic-name optic)]
           [optic-type (if (pair? optic) (car optic) 'unknown)]
           [source-hash (hash->hex (store-value! source))]
           [result-hash (hash->hex (store-value! result))]
           [value-hash (and value (hash->hex (store-value! value)))]
           [record (make-provenance-record operation optic-name optic-type
                                           source-hash result-hash value-hash)])
      (store-provenance! record)))
  result)
