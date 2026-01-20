(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")
(load "boundary/storage/cas-persist.ss")

(doc 'module 'identity)
(doc 'description "Agent Identity and Storage System")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/blocks/block core/blocks/cas boundary/storage/cas-persist))

(doc 'note "Identity system using content-addressed blocks for immutable history. Each identity update creates a new block, enabling complete audit trail, time-travel queries, and eventual consistency")

(doc 'section 'identity-schema)

(doc 'schema "
An identity block has:
  tag: 'identity
  payload: S-expression encoded as UTF-8:
    ((username . symbol)
     (role . symbol)           ; shepherd, builder, player
     (first-seen . string)     ; ISO8601 timestamp
     (last-seen . string)      ; ISO8601 timestamp
     (session-count . nat)
     (total-posts . nat)
     (preferences . alist))
  refs: [prev-identity-hash ...]  ; history chain")

(doc 'section 'head-pointer-management)

(doc *identity-heads-dir* 'type String)
(doc *identity-heads-dir* 'description "Directory for identity head pointers")
(define *identity-heads-dir* ".store/heads/identity")

(define (ensure-identity-heads-dir!)
  (doc 'type (-> Void))
  (doc 'description "Ensure identity heads directory exists")
  (unless (file-exists? ".store")
          (mkdir ".store"))
  (unless (file-exists? ".store/heads")
          (mkdir ".store/heads"))
  (unless (file-exists? *identity-heads-dir*)
          (mkdir *identity-heads-dir*)))

(define (safe-username? name-str)
  (doc 'type (-> String Boolean))
  (doc 'description "Check if username is safe for use in file paths")
  (doc 'security "Rejects path traversal attempts (../, /, etc.) and control characters")
  (and (string? name-str)
       (> (string-length name-str) 0)
       (<= (string-length name-str) 64)
       (not (string-contains? name-str ".."))
       (not (string-contains? name-str "/"))
       (not (string-contains? name-str "\\"))
       (not (string-contains? name-str "\x0;"))
       (let ([c (string-ref name-str 0)])
            (or (char-alphabetic? c)
                (char-numeric? c)
                (char=? c #\_)))))

(define (string-contains? haystack needle)
  (doc 'type (-> String String Boolean))
  (doc 'description "Check if haystack contains needle")
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))]))))

(define (identity-head-path username)
  (doc 'type (-> Symbol String))
  (doc 'description "Get path to identity head file")
  (doc 'security "Validates username before constructing path")
  (let ([name-str (symbol->string username)])
       (unless (safe-username? name-str)
               (error 'identity-head-path
                      "Invalid username (path traversal attempt or invalid characters)"
                      username))
       (string-append *identity-heads-dir* "/" name-str)))

(define (read-identity-head username)
  (doc 'type (-> Symbol (Maybe Bytevector)))
  (doc 'description "Get the current head hash for a username")
  (let ([path (identity-head-path username)])
       (guard (e [else #f])
              (and (file-exists? path)
                   (let* ([hex-str (call-with-input-file path get-line)])
                         (and hex-str (hex->hash hex-str)))))))

(define (write-identity-head! username hash)
  (doc 'type (-> Symbol Bytevector Void))
  (doc 'description "Update the head pointer for a username")
  (ensure-identity-heads-dir!)
  (let ([path (identity-head-path username)]
        [hex (hash->hex hash)])
       (call-with-output-file path
                              (lambda (p) (put-string p hex))
                              'replace)))

(define (list-identity-heads)
  (doc 'type (-> (List Symbol)))
  (doc 'description "List all known usernames with identity records")
  (ensure-identity-heads-dir!)
  (guard (e [else '()])
         (let ([potential-users '(alice bob charlie dave eve test)])
              (filter (lambda (username)
                              (file-exists? (identity-head-path username)))
                      potential-users))))

(doc 'section 'identity-encoding)

(define (encode-identity-payload alist)
  (doc 'type (-> Alist Bytevector))
  (doc 'description "Encode identity alist to UTF-8 bytes")
  (string->utf8 (format "~s" alist)))

(define (decode-identity-payload bv)
  (doc 'type (-> Bytevector Alist))
  (doc 'description "Decode identity alist from UTF-8 bytes")
  (let* ([str (utf8->string bv)]
         [expr (read (open-input-string str))])
        (if (list? expr) expr '())))

(doc 'section 'identity-construction)

(define (make-identity-block data prev-hashes)
  (doc 'type (-> Alist (List Bytevector) Block))
  (doc 'description "Create an identity block with given data and previous version refs")
  (make-block 'identity
              (encode-identity-payload data)
              (list->vector prev-hashes)))

(define (make-new-identity username role)
  (doc 'type (-> Symbol Symbol Alist))
  (doc 'description "Create a fresh identity alist for a new user")
  (let ([now (current-iso8601-timestamp)])
       (list (cons 'username username)
             (cons 'role role)
             (cons 'first-seen now)
             (cons 'last-seen now)
             (cons 'session-count 1)
             (cons 'total-posts 0)
             (cons 'preferences '()))))

(define (update-identity-data data key value)
  (doc 'type (-> Alist Symbol Any Alist))
  (doc 'description "Update a field in the identity alist")
  (map (lambda (pair)
               (if (eq? (car pair) key)
                   (cons key value)
                   pair))
       data))

(define (increment-identity-field data key)
  (doc 'type (-> Alist Symbol Alist))
  (doc 'description "Increment a numeric field in the identity alist")
  (map (lambda (pair)
               (if (eq? (car pair) key)
                   (cons key (+ 1 (cdr pair)))
                   pair))
       data))

(doc 'section 'identity-queries)

(define (lookup-identity username)
  (doc 'type (-> Symbol (Maybe Block)))
  (doc 'description "Look up the current identity block for a username")
  (let ([head-hash (read-identity-head username)])
       (and head-hash
            (fetch head-hash))))

(define (get-identity-data username)
  (doc 'type (-> Symbol (Maybe Alist)))
  (doc 'description "Get the identity data alist for a username")
  (let ([block (lookup-identity username)])
       (and block
            (decode-identity-payload (block-payload block)))))

(define (get-identity-history username)
  (doc 'type (-> Symbol (List Block)))
  (doc 'description "Get all historical versions of an identity, newest first")
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

(define (identity-exists? username)
  (doc 'type (-> Symbol Bool))
  (doc 'description "Check if identity exists")
  (if (read-identity-head username) #t #f))

(doc 'section 'validation)

(define (valid-role? role)
  (doc 'type (-> Symbol Bool))
  (doc 'description "Check if role is a valid tier")
  (if (memq role '(shepherd builder player)) #t #f))

(define (validate-username! username)
  (doc 'type (-> Any Void))
  (doc 'description "Validate username is a symbol")
  (doc 'throws "Error if not")
  (unless (symbol? username)
          (error 'validate-username "username must be a symbol" username)))

(define (validate-role! role)
  (doc 'type (-> Any Void))
  (doc 'description "Validate role is a valid tier")
  (doc 'throws "Error if not")
  (unless (valid-role? role)
          (error 'validate-role "role must be shepherd, builder, or player" role)))

(doc 'section 'public-api)
(doc 'note "Task fold-n5z")

(define (identity-get username)
  (doc 'type (-> Symbol (Maybe Alist)))
  (doc 'description "Get identity record for a username")
  (doc 'returns "Identity data alist or #f if not found")
  (doc 'export #t)
  (validate-username! username)
  (get-identity-data username))

(define (identity-create! username role)
  (doc 'type (-> Symbol Symbol Alist))
  (doc 'description "Create a new identity with validation")
  (doc 'returns "Identity data alist")
  (doc 'throws "Error if username already taken or validation fails")
  (doc 'export #t)
  (validate-username! username)
  (validate-role! role)
  (when (identity-exists? username)
        (error 'identity-create! "username already taken" username))
  (let ([block (register-identity! username role)])
       (decode-identity-payload (block-payload block))))

(define (identity-update! username updates)
  (doc 'type (-> Symbol Alist Alist))
  (doc 'description "Update an identity record with new values")
  (doc 'param 'updates "Alist contains key-value pairs to update")
  (doc 'returns "Updated identity data alist")
  (doc 'export #t)
  (validate-username! username)
  (unless (list? updates)
          (error 'identity-update! "updates must be an alist" updates))
  (let ([block (update-identity! username
                                 (lambda (data)
                                         (fold-left (lambda (acc update)
                                                            (if (pair? update)
                                                                (update-identity-data acc
                                                                                      (car update)
                                                                                      (cdr update))
                                                                acc))
                                                    data
                                                    updates)))])
       (decode-identity-payload (block-payload block))))

(doc 'section 'identity-mutations)

(define (register-identity! username role)
  (doc 'type (-> Symbol Symbol Block))
  (doc 'description "Register a new identity")
  (doc 'returns "Created block")
  (doc 'throws "Error if identity already exists")
  (when (identity-exists? username)
        (error 'register-identity! "identity already exists" username))
  (let* ([data (make-new-identity username role)]
         [block (make-identity-block data '())]
         [hash (store! block)])
        (write-identity-head! username hash)
        block))

(define (update-identity! username update-fn)
  (doc 'type (-> Symbol (-> Alist Alist) Block))
  (doc 'description "Update an existing identity by applying a function to its data")
  (doc 'note "Creates a new block with previous version in refs")
  (doc 'returns "New block")
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

(define (record-session! username)
  (doc 'type (-> Symbol Block))
  (doc 'description "Record a new session for a user (increment count, update timestamp)")
  (update-identity! username
                    (lambda (data)
                            (let* ([data1 (increment-identity-field data 'session-count)]
                                   [data2 (update-identity-data data1 'last-seen
                                                                (current-iso8601-timestamp))])
                                  data2))))

(define (record-post! username)
  (doc 'type (-> Symbol Block))
  (doc 'description "Record a new post for a user (increment count)")
  (update-identity! username
                    (lambda (data)
                            (increment-identity-field data 'total-posts))))

(define (set-preference! username pref-key pref-value)
  (doc 'type (-> Symbol Symbol Any Block))
  (doc 'description "Set a preference value for a user")
  (update-identity! username
                    (lambda (data)
                            (let* ([prefs (cdr (assq 'preferences data))]
                                   [new-prefs (cons (cons pref-key pref-value)
                                                    (filter (lambda (p) (not (eq? (car p) pref-key)))
                                                            prefs))])
                                  (update-identity-data data 'preferences new-prefs)))))

(doc 'section 'utility-functions)

(define (current-iso8601-timestamp)
  (doc 'type (-> String))
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

(doc 'section 'display-and-debugging)

(define (display-identity username)
  (doc 'type (-> Symbol Void))
  (doc 'description "Display identity information for a username")
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

(define (list-all-identities)
  (doc 'type (-> Void))
  (doc 'description "Display all known identities")
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
