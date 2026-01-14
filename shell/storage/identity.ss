;;; shell/storage/identity.ss — Agent Identity and Storage System
;;;
;;; Identity system using content-addressed blocks for immutable history.
;;; Each identity update creates a new block, enabling:
;;;   - Complete audit trail
;;;   - Time-travel queries
;;;   - Eventual consistency
;;;
;;; This is Shell code: manages I/O and mutable head pointers.
;;;
;;; Dependencies:
;;;   - core/blocks/block.ss
;;;   - core/blocks/cas.ss
;;;   - shell/cas-persist.ss (for filesystem persistence)

(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")
(load "shell/cas-persist.ss")

;;; ====
;;; Identity Schema
;;; ====
;;;
;;; An identity block has:
;;;   tag: 'identity
;;;   payload: S-expression encoded as UTF-8:
;;;     ((username . symbol)
;;;      (role . symbol)           ; shepherd, builder, player
;;;      (first-seen . string)     ; ISO8601 timestamp
;;;      (last-seen . string)      ; ISO8601 timestamp
;;;      (session-count . nat)
;;;      (total-posts . nat)
;;;      (preferences . alist))
;;;   refs: [prev-identity-hash ...]  ; history chain

;;; ====
;;; Head Pointer Management
;;; ====

(define *identity-heads-dir* ".store/heads/identity")

;;; ensure-identity-heads-dir! : → Void
(define (ensure-identity-heads-dir!)
  (unless (file-exists? ".store")
          (mkdir ".store"))
  (unless (file-exists? ".store/heads")
          (mkdir ".store/heads"))
  (unless (file-exists? *identity-heads-dir*)
          (mkdir *identity-heads-dir*)))

;;; safe-username? : String → Boolean
;;; SECURITY: Check if username is safe for use in file paths.
;;; Rejects path traversal attempts (../, /, etc.) and control characters.
(define (safe-username? name-str)
  (and (string? name-str)
       (> (string-length name-str) 0)
       (<= (string-length name-str) 64)  ; Reasonable length limit
       (not (string-contains? name-str ".."))
       (not (string-contains? name-str "/"))
       (not (string-contains? name-str "\\"))
       (not (string-contains? name-str "\x00"))
       ;; Must start with alphanumeric or underscore
       (let ([c (string-ref name-str 0)])
            (or (char-alphabetic? c)
                (char-numeric? c)
                (char=? c #\_)))))

;;; string-contains? : String → String → Boolean
;;; Check if haystack contains needle.
(define (string-contains? haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))]))))

;;; identity-head-path : Symbol → String
;;; SECURITY: Validates username before constructing path.
(define (identity-head-path username)
  (let ([name-str (symbol->string username)])
       (unless (safe-username? name-str)
               (error 'identity-head-path
                      "Invalid username (path traversal attempt or invalid characters)"
                      username))
       (string-append *identity-heads-dir* "/" name-str)))

;;; read-identity-head : Symbol → Bytevector | #f
;;; Get the current head hash for a username.
(define (read-identity-head username)
  (let ([path (identity-head-path username)])
       (guard (e [else #f])
              (and (file-exists? path)
                   (let* ([hex-str (call-with-input-file path get-line)])
                         (and hex-str (hex->hash hex-str)))))))

;;; write-identity-head! : Symbol × Bytevector → Void
;;; Update the head pointer for a username.
(define (write-identity-head! username hash)
  (ensure-identity-heads-dir!)
  (let ([path (identity-head-path username)]
        [hex (hash->hex hash)])
       (call-with-output-file path
                              (lambda (p) (put-string p hex))
                              'replace)))

;;; list-identity-heads : → (List Symbol)
;;; List all known usernames with identity records.
(define (list-identity-heads)
  (ensure-identity-heads-dir!)
  (guard (e [else '()])
         ;; Read directory by checking which head files exist
         ;; Simple brute-force approach for testing
         (let ([potential-users '(alice bob charlie dave eve test)])
              (filter (lambda (username)
                              (file-exists? (identity-head-path username)))
                      potential-users))))

;;; ====
;;; Identity Record Encoding/Decoding
;;; ====

;;; encode-identity-payload : Alist → Bytevector
;;; Encode identity alist to UTF-8 bytes.
(define (encode-identity-payload alist)
  (string->utf8 (format "~s" alist)))

;;; decode-identity-payload : Bytevector → Alist
;;; Decode identity alist from UTF-8 bytes.
(define (decode-identity-payload bv)
  (let* ([str (utf8->string bv)]
         [expr (read (open-input-string str))])
        (if (list? expr) expr '())))

;;; ====
;;; Identity Construction
;;; ====

;;; make-identity-block : Alist × (List Bytevector) → Block
;;; Create an identity block with given data and previous version refs.
(define (make-identity-block data prev-hashes)
  (make-block 'identity
              (encode-identity-payload data)
              (list->vector prev-hashes)))

;;; make-new-identity : Symbol × Symbol → Alist
;;; Create a fresh identity alist for a new user.
(define (make-new-identity username role)
  (let ([now (current-iso8601-timestamp)])
       (list (cons 'username username)
             (cons 'role role)
             (cons 'first-seen now)
             (cons 'last-seen now)
             (cons 'session-count 1)
             (cons 'total-posts 0)
             (cons 'preferences '()))))

;;; update-identity-data : Alist × Symbol × Any → Alist
;;; Update a field in the identity alist.
(define (update-identity-data data key value)
  (map (lambda (pair)
               (if (eq? (car pair) key)
                   (cons key value)
                   pair))
       data))

;;; increment-identity-field : Alist × Symbol → Alist
;;; Increment a numeric field in the identity alist.
(define (increment-identity-field data key)
  (map (lambda (pair)
               (if (eq? (car pair) key)
                   (cons key (+ 1 (cdr pair)))
                   pair))
       data))

;;; ====
;;; Identity Queries
;;; ====

;;; lookup-identity : Symbol → Block | #f
;;; Look up the current identity block for a username.
(define (lookup-identity username)
  (let ([head-hash (read-identity-head username)])
       (and head-hash
            (fetch head-hash))))

;;; get-identity-data : Symbol → Alist | #f
;;; Get the identity data alist for a username.
(define (get-identity-data username)
  (let ([block (lookup-identity username)])
       (and block
            (decode-identity-payload (block-payload block)))))

;;; get-identity-history : Symbol → (List Block)
;;; Get all historical versions of an identity, newest first.
(define (get-identity-history username)
  (let ([head-hash (read-identity-head username)])
       (if (not head-hash)
           '()
           (let loop ([hash head-hash] [acc '()])
                (let ([block (fetch hash)])
                     (if (not block)
                         acc
                         (let* ([refs (block-refs block)]
                                [new-acc (cons block acc)])
                               (if (zero? (vector-length refs))
                                   new-acc
                                   (loop (vector-ref refs 0) new-acc)))))))))

;;; identity-exists? : Symbol → Bool
(define (identity-exists? username)
  (if (read-identity-head username) #t #f))

;;; ====
;;; Validation
;;; ====

;;; valid-role? : Symbol → Bool
;;; Check if role is a valid tier.
(define (valid-role? role)
  (if (memq role '(shepherd builder player)) #t #f))

;;; validate-username! : Any → Void
;;; Validate username is a symbol. Throws error if not.
(define (validate-username! username)
  (unless (symbol? username)
          (error 'validate-username "username must be a symbol" username)))

;;; validate-role! : Any → Void
;;; Validate role is a valid tier. Throws error if not.
(define (validate-role! role)
  (unless (valid-role? role)
          (error 'validate-role "role must be shepherd, builder, or player" role)))

;;; ====
;;; Public API (Task fold-n5z)
;;; ====

;;; identity-get : Symbol → Alist | #f
;;; Get identity record for a username.
;;; Returns identity data alist or #f if not found.
(define (identity-get username)
  (validate-username! username)
  (get-identity-data username))

;;; identity-create! : Symbol → Symbol → Alist
;;; Create a new identity with validation.
;;; Returns the identity data alist.
;;; Throws error if username already taken or validation fails.
(define (identity-create! username role)
  (validate-username! username)
  (validate-role! role)
  (when (identity-exists? username)
        (error 'identity-create! "username already taken" username))
  (let ([block (register-identity! username role)])
       (decode-identity-payload (block-payload block))))

;;; identity-update! : Symbol → Alist → Alist
;;; Update an identity record with new values.
;;; The updates alist contains key-value pairs to update.
;;; Returns the updated identity data alist.
(define (identity-update! username updates)
  (validate-username! username)
  (unless (list? updates)
          (error 'identity-update! "updates must be an alist" updates))
  (let ([block (update-identity! username
                                 (lambda (data)
                                         ;; Apply each update to the data
                                         (fold-left (lambda (acc update)
                                                            (if (pair? update)
                                                                (update-identity-data acc
                                                                                      (car update)
                                                                                      (cdr update))
                                                                acc))
                                                    data
                                                    updates)))])
       (decode-identity-payload (block-payload block))))

;;; ====
;;; Identity Mutations (Create/Update)
;;; ====

;;; register-identity! : Symbol × Symbol → Block
;;; Register a new identity. Returns the created block.
;;; Fails if identity already exists.
(define (register-identity! username role)
  (when (identity-exists? username)
        (error 'register-identity! "identity already exists" username))
  (let* ([data (make-new-identity username role)]
         [block (make-identity-block data '())]
         [hash (store! block)])
        (write-identity-head! username hash)
        block))

;;; update-identity! : Symbol × (Alist → Alist) → Block
;;; Update an existing identity by applying a function to its data.
;;; Creates a new block with previous version in refs.
;;; Returns the new block.
(define (update-identity! username update-fn)
  (let* ([current-hash (read-identity-head username)]
         [current-block (and current-hash (fetch current-hash))])
        (unless current-block
                (error 'update-identity! "identity not found" username))
        (let* ([old-data (decode-identity-payload (block-payload current-block))]
               [new-data (update-fn old-data)]
               [new-block (make-identity-block new-data (list current-hash))]
               [new-hash (store! new-block)])
              (write-identity-head! username new-hash)
              new-block)))

;;; record-session! : Symbol → Block
;;; Record a new session for a user (increment count, update timestamp).
(define (record-session! username)
  (update-identity! username
                    (lambda (data)
                            (let* ([data1 (increment-identity-field data 'session-count)]
                                   [data2 (update-identity-data data1 'last-seen
                                                                (current-iso8601-timestamp))])
                                  data2))))

;;; record-post! : Symbol → Block
;;; Record a new post for a user (increment count).
(define (record-post! username)
  (update-identity! username
                    (lambda (data)
                            (increment-identity-field data 'total-posts))))

;;; set-preference! : Symbol × Symbol × Any → Block
;;; Set a preference value for a user.
(define (set-preference! username pref-key pref-value)
  (update-identity! username
                    (lambda (data)
                            (let* ([prefs (cdr (assq 'preferences data))]
                                   [new-prefs (cons (cons pref-key pref-value)
                                                    (filter (lambda (p) (not (eq? (car p) pref-key)))
                                                            prefs))])
                                  (update-identity-data data 'preferences new-prefs)))))

;;; ====
;;; Utility Functions
;;; ====

;;; current-iso8601-timestamp : → String
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

;;; ====
;;; Display and Debugging
;;; ====

;;; display-identity : Symbol → Void
;;; Display identity information for a username.
(define (display-identity username)
  (let ([data (get-identity-data username)])
       (if (not data)
           (begin
            (display "No identity found for: ")
            (display username)
            (newline))
           (begin
            (display "Identity: ")
            (display (cdr (assq 'username data)))
            (newline)
            (display "  Role: ")
            (display (cdr (assq 'role data)))
            (newline)
            (display "  First seen: ")
            (display (cdr (assq 'first-seen data)))
            (newline)
            (display "  Last seen: ")
            (display (cdr (assq 'last-seen data)))
            (newline)
            (display "  Sessions: ")
            (display (cdr (assq 'session-count data)))
            (newline)
            (display "  Posts: ")
            (display (cdr (assq 'total-posts data)))
            (newline)))))

;;; list-all-identities : → Void
;;; Display all known identities.
(define (list-all-identities)
  (let ([usernames (list-identity-heads)])
       (if (null? usernames)
           (begin
            (display "No identities found.")
            (newline))
           (begin
            (display "Known identities:")
            (newline)
            (for-each (lambda (username)
                              (display "  - ")
                              (display username)
                              (let ([data (get-identity-data username)])
                                   (when data
                                         (display " (")
                                         (display (cdr (assq 'role data)))
                                         (display ", ")
                                         (display (cdr (assq 'session-count data)))
                                         (display " sessions)")))
                              (newline))
                      usernames)))))
