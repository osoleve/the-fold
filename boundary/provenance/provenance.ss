;;; boundary/provenance/provenance.ss — Provenance Tracking for Optic Operations
;;;
;;; Log every optic application to the CAS for full data lineage.
;;; Every traced operation creates a provenance record that captures:
;;;   - What optic operation was performed (view, set, over, preview, etc.)
;;;   - The source structure (stored as CAS block)
;;;   - The result structure (stored as CAS block)
;;;   - Any value involved (for set/over operations)
;;;   - Timestamp and optional agent/session identity
;;;
;;; Benefits:
;;;   - Full audit trail for any CAS block
;;;   - "Explain this value" queries
;;;   - Debugging complex transformations
;;;   - Regulatory compliance (data lineage)
;;;
;;; This is Shell code: uses timestamps, identity, filesystem persistence.
;;;
;;; Dependencies:
;;;   - core/blocks/block.ss
;;;   - core/blocks/cas.ss
;;;   - boundary/storage/identity.ss (for timestamps)

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")
(load "boundary/storage/cas-persist.ss")

;;; ============================================================
;;; Provenance Configuration
;;; ============================================================

;;; *provenance-enabled?* : Boolean
;;; Global toggle for provenance tracking. Defaults to #t.
(define *provenance-enabled?* #t)

;;; provenance-enable! : -> Void
;;; Enable provenance tracking.
(define (provenance-enable!)
  (set! *provenance-enabled?* #t))

;;; provenance-disable! : -> Void
;;; Disable provenance tracking.
(define (provenance-disable!)
  (set! *provenance-enabled?* #f))

;;; *current-agent-id* : Symbol | #f
;;; Current agent identity for provenance records.
(define *current-agent-id* #f)

;;; *current-session-id* : String | #f
;;; Current session identity for provenance records.
(define *current-session-id* #f)

;;; with-agent-id : Symbol (-> a) -> a
;;; Execute thunk with a specific agent identity.
(define (with-agent-id agent-id thunk)
  (let ([old *current-agent-id*])
    (dynamic-wind
      (lambda () (set! *current-agent-id* agent-id))
      thunk
      (lambda () (set! *current-agent-id* old)))))

;;; with-session-id : String (-> a) -> a
;;; Execute thunk with a specific session identity.
(define (with-session-id session-id thunk)
  (let ([old *current-session-id*])
    (dynamic-wind
      (lambda () (set! *current-session-id* session-id))
      thunk
      (lambda () (set! *current-session-id* old)))))

;;; ============================================================
;;; Timestamp Utility
;;; ============================================================

;;; current-iso8601-timestamp : -> String
;;; Get current time as ISO8601 string.
(define (current-iso8601-timestamp)
  (let* ([time-utc (current-time 'time-utc)]
         [date (time-utc->date time-utc 0)])
    (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year date)
            (date-month date)
            (date-day date)
            (date-hour date)
            (date-minute date)
            (date-second date))))

;;; ============================================================
;;; Optic Name Registry
;;; ============================================================
;;;
;;; Since optics are functions and can't be serialized directly,
;;; we maintain an optional registry of named optics. When a traced
;;; operation uses a registered optic, we can record its name.

;;; *optic-registry* : Hashtable (Optic -> Symbol)
;;; Maps optic values to their registered names.
(define *optic-registry* (make-eq-hashtable))

;;; *optic-registry-reverse* : Hashtable (Symbol -> Optic)
;;; Maps names to optic values (for lookup).
(define *optic-registry-reverse* (make-hashtable symbol-hash eq?))

;;; register-optic! : Symbol Optic -> Void
;;; Register an optic with a symbolic name.
(define (register-optic! name optic)
  (hashtable-set! *optic-registry* optic name)
  (hashtable-set! *optic-registry-reverse* name optic))

;;; lookup-optic-name : Optic -> Symbol | #f
;;; Look up the registered name for an optic.
(define (lookup-optic-name optic)
  (hashtable-ref *optic-registry* optic #f))

;;; lookup-optic : Symbol -> Optic | #f
;;; Look up an optic by its registered name.
(define (lookup-optic name)
  (hashtable-ref *optic-registry-reverse* name #f))

;;; list-registered-optics : -> (List (Symbol . Optic))
;;; List all registered optics.
(define (list-registered-optics)
  (let ([names (vector->list (hashtable-keys *optic-registry-reverse*))])
    (map (lambda (name)
           (cons name (hashtable-ref *optic-registry-reverse* name #f)))
         names)))

;;; ============================================================
;;; Provenance Record Block Schema
;;; ============================================================
;;;
;;; Block Tag: provenance/record
;;;
;;; Payload (S-expression alist):
;;;   operation    : Symbol (view, set, over, preview, to-list, etc.)
;;;   optic-name   : Symbol | #f (registered name if available)
;;;   optic-type   : Symbol (lens, prism, traversal, etc.)
;;;   source-hash  : String (hex hash of source structure block)
;;;   result-hash  : String (hex hash of result structure block)
;;;   value-hash   : String | #f (hex hash of value block, for set/over)
;;;   timestamp    : String (ISO8601)
;;;   agent-id     : Symbol | #f
;;;   session-id   : String | #f
;;;   version      : Integer (schema version, currently 1)
;;;
;;; Refs: Vector of hashes pointing to:
;;;   [0] source structure block
;;;   [1] result structure block
;;;   [2] value block (if applicable)

(define PROVENANCE-TAG 'provenance/record)
(define PROVENANCE-VERSION 1)

;;; ============================================================
;;; Provenance Head Management
;;; ============================================================
;;;
;;; We maintain head pointers for:
;;;   - Per-result tracking: .store/heads/provenance/by-result/<hash>.head
;;;   - Per-source tracking: .store/heads/provenance/by-source/<hash>.head
;;;   - Global log: .store/heads/provenance/log.head

(define *provenance-heads-dir* ".store/heads/provenance")
(define *provenance-by-result-dir* ".store/heads/provenance/by-result")
(define *provenance-by-source-dir* ".store/heads/provenance/by-source")

;;; ensure-provenance-dirs! : -> Void
(define (ensure-provenance-dirs!)
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

;;; valid-hex-string? : String -> Boolean
;;; Validate that a string contains only hex characters.
;;; Prevents path traversal attacks.
(define (valid-hex-string? s)
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

;;; provenance-log-head-path : -> String
(define (provenance-log-head-path)
  (string-append *provenance-heads-dir* "/log.head"))

;;; provenance-by-result-path : String -> String
;;; Path for the provenance head keyed by result hash.
;;; Validates hex input to prevent path traversal.
(define (provenance-by-result-path result-hex)
  (unless (valid-hex-string? result-hex)
    (error 'provenance-by-result-path "invalid hex string" result-hex))
  (string-append *provenance-by-result-dir* "/"
                 (substring result-hex 0 (min 16 (string-length result-hex)))
                 ".head"))

;;; provenance-by-source-path : String -> String
;;; Path for the provenance head keyed by source hash.
;;; Validates hex input to prevent path traversal.
(define (provenance-by-source-path source-hex)
  (unless (valid-hex-string? source-hex)
    (error 'provenance-by-source-path "invalid hex string" source-hex))
  (string-append *provenance-by-source-dir* "/"
                 (substring source-hex 0 (min 16 (string-length source-hex)))
                 ".head"))

;;; read-head-hash : String -> Bytevector | #f
(define (read-head-hash path)
  (guard (e [else #f])
    (and (file-exists? path)
         (let ([hex-str (call-with-input-file path get-line)])
           (and hex-str (hex->hash hex-str))))))

;;; write-head-hash! : String Bytevector -> Void
(define (write-head-hash! path hash)
  (ensure-provenance-dirs!)
  (call-with-output-file path
    (lambda (p) (put-string p (hash->hex hash)))
    'replace))

;;; ============================================================
;;; Value Storage
;;; ============================================================
;;;
;;; Store arbitrary values as CAS blocks for reference in provenance.

;;; store-value! : Any -> Bytevector
;;; Store a value as a CAS block and return its hash.
;;; Uses persistent storage to survive process restarts.
(define (store-value! value)
  (let* ([payload (string->utf8 (format "~s" value))]
         [blk (make-block 'value payload empty-refs)])
    (store-persistent! blk)))

;;; fetch-value : Bytevector -> Any | #f
;;; Fetch a value from the CAS by hash.
;;; Uses persistent fetch to recover from disk if needed.
(define (fetch-value hash)
  (let ([blk (fetch-persistent hash)])
    (and blk
         (eq? (block-tag blk) 'value)
         (guard (e [else #f])
           (read (open-input-string (utf8->string (block-payload blk))))))))

;;; ============================================================
;;; Provenance Record Creation
;;; ============================================================

;;; make-provenance-record : Symbol Symbol Symbol String String (Option String) -> Block
;;; Create a provenance record block.
;;;
;;; Arguments:
;;;   operation   - The optic operation (view, set, over, preview, etc.)
;;;   optic-name  - Registered name or #f
;;;   optic-type  - Type of optic (lens, prism, etc.)
;;;   source-hash - Hex hash of source structure
;;;   result-hash - Hex hash of result
;;;   value-hash  - Hex hash of value (or #f)
(define (make-provenance-record operation optic-name optic-type
                                 source-hash result-hash value-hash)
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
         ;; Build refs vector
         [source-hash-bv (hex->hash source-hash)]
         [result-hash-bv (hex->hash result-hash)]
         [refs (if value-hash
                   (vector source-hash-bv result-hash-bv (hex->hash value-hash))
                   (vector source-hash-bv result-hash-bv))])
    (make-block PROVENANCE-TAG payload refs)))

;;; store-provenance! : Block -> Bytevector
;;; Store a provenance record and update all relevant head pointers.
(define (store-provenance! record)
  (let* ([hash (store-persistent! record)]
         [data (provenance-record-data record)]
         [source-hex (cdr (assq 'source-hash data))]
         [result-hex (cdr (assq 'result-hash data))])
    ;; Update the global log head
    (write-head-hash! (provenance-log-head-path) hash)
    ;; Update the by-result index
    (write-head-hash! (provenance-by-result-path result-hex) hash)
    ;; Update the by-source index
    (write-head-hash! (provenance-by-source-path source-hex) hash)
    hash))

;;; ============================================================
;;; Provenance Record Access
;;; ============================================================

;;; provenance-record? : Block -> Boolean
(define (provenance-record? blk)
  (and (block? blk)
       (eq? (block-tag blk) PROVENANCE-TAG)))

;;; provenance-record-data : Block -> Alist | #f
(define (provenance-record-data blk)
  (and (provenance-record? blk)
       (guard (e [else #f])
         (read (open-input-string (utf8->string (block-payload blk)))))))

;;; provenance-field : Block Symbol -> Any | #f
(define (provenance-field blk field)
  (let ([data (provenance-record-data blk)])
    (and data
         (let ([pair (assq field data)])
           (and pair (cdr pair))))))

;; Convenience accessors
(define (provenance-operation blk) (provenance-field blk 'operation))
(define (provenance-optic-name blk) (provenance-field blk 'optic-name))
(define (provenance-optic-type blk) (provenance-field blk 'optic-type))
(define (provenance-source-hash blk) (provenance-field blk 'source-hash))
(define (provenance-result-hash blk) (provenance-field blk 'result-hash))
(define (provenance-value-hash blk) (provenance-field blk 'value-hash))
(define (provenance-timestamp blk) (provenance-field blk 'timestamp))
(define (provenance-agent-id blk) (provenance-field blk 'agent-id))
(define (provenance-session-id blk) (provenance-field blk 'session-id))

;;; ============================================================
;;; Core Recording Function
;;; ============================================================

;;; record-optic-operation! : Symbol Optic Any Any (Option Any) -> Any
;;; Record an optic operation to the provenance store.
;;; Returns the result unchanged (passthrough for tracing).
;;;
;;; Arguments:
;;;   operation - Symbol describing the operation (view, set, over, etc.)
;;;   optic     - The optic used
;;;   source    - The source structure
;;;   result    - The result of the operation
;;;   value     - The value argument (for set/over), or #f
(define (record-optic-operation! operation optic source result value)
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

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Configuration:
;;;   *provenance-enabled?*, provenance-enable!, provenance-disable!
;;;   *current-agent-id*, *current-session-id*
;;;   with-agent-id, with-session-id
;;;
;;; Optic Registry:
;;;   register-optic!, lookup-optic-name, lookup-optic
;;;   list-registered-optics
;;;
;;; Value Storage:
;;;   store-value!, fetch-value
;;;
;;; Provenance Records:
;;;   make-provenance-record, store-provenance!
;;;   provenance-record?, provenance-record-data
;;;   provenance-field, provenance-operation, provenance-optic-name
;;;   provenance-optic-type, provenance-source-hash, provenance-result-hash
;;;   provenance-value-hash, provenance-timestamp, provenance-agent-id
;;;   provenance-session-id
;;;
;;; Core Recording:
;;;   record-optic-operation!

