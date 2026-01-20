(load "core/base/prelude.ss")
(load "core/blocks/cas.ss")
(load "boundary/io/atomic.ss")
(load "boundary/io/file-lock.ss")

(doc 'module 'flashmob/heads)
(doc 'description "Manages named heads for sessions in .store/heads/flashmob/. Each session has a head file containing its current block hash.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'lock-aware-design)
(doc 'description "Public functions acquire locks; internal functions (%flashmob-write-head!, %flashmob-delete-head!) for use when lock already held")

(doc 'section 'head-file-format)
(doc 'description "Head files contain 66-character hex strings: 2 chars version byte (00), 64 chars SHA-256 hash")

(doc 'section 'configuration)

(define *flashmob-heads-dir* ".store/heads/flashmob")

(doc 'section 'path-utilities)

(define (flashmob-head-path id)
  (doc 'type '(-> String String))
  (doc 'description "Get the filesystem path for a session's head file")
  (string-append *flashmob-heads-dir* "/" id ".head"))

(define (flashmob-ensure-heads-dir!)
  (doc 'type '(-> Void))
  (doc 'description "Ensure the heads directory exists")
  (unless (file-exists? ".store")
    (mkdir ".store"))
  (unless (file-exists? ".store/heads")
    (mkdir ".store/heads"))
  (unless (file-exists? *flashmob-heads-dir*)
    (mkdir *flashmob-heads-dir*)))

(doc 'section 'read-write-operations)

(define (flashmob-read-head id)
  (doc 'type '(-> String (Maybe Bytevector)))
  (doc 'description "Read the current hash for a session ID")
  (doc 'returns "#f if the head file doesn't exist")
  (let ([path (flashmob-head-path id)])
    (guard (e [else #f])
      (if (file-exists? path)
          (let* ([content (call-with-input-file path
                           (lambda (port) (get-line port)))]
                 [trimmed (flashmob-string-trim content)]
                 [hex (if (and (> (string-length trimmed) 2)
                               (string=? (substring trimmed 0 2) "0x"))
                          (substring trimmed 2 (string-length trimmed))
                          trimmed)])
            (let ([len (string-length hex)])
              (if (or (= len 64) (= len 66))
                  (hex->hash hex)
                  #f)))
          #f))))

(define (%flashmob-write-head! id hash)
  (doc 'type '(-> String Bytevector Void))
  (doc 'description "INTERNAL: Write the current hash for a session ID (caller must hold lock)")
  (doc 'note "Uses atomic write-then-rename to prevent corruption")
  (flashmob-ensure-heads-dir!)
  (let ([path (flashmob-head-path id)]
        [hex (hash->hex hash)])
    (call-with-atomic-output-file path
      (lambda (port)
        (put-string port hex)
        (newline port))
      '(replace))))

(define (flashmob-write-head! id hash)
  (doc 'type '(-> String Bytevector Void))
  (doc 'description "PUBLIC: Write the current hash for a session ID with locking")
  (doc 'note "Uses atomic write-then-rename to prevent corruption")
  (let ([path (flashmob-head-path id)])
    (with-file-lock path
      (lambda ()
        (%flashmob-write-head! id hash)))))

(define (%flashmob-delete-head! id)
  (doc 'type '(-> String Void))
  (doc 'description "INTERNAL: Delete the head file for a session (caller must hold lock)")
  (let ([path (flashmob-head-path id)])
    (when (file-exists? path)
      (delete-file path))))

(define (flashmob-delete-head! id)
  (doc 'type '(-> String Void))
  (doc 'description "PUBLIC: Delete the head file for a session (for closing/deleting)")
  (let ([path (flashmob-head-path id)])
    (with-file-lock path
      (lambda ()
        (%flashmob-delete-head! id)))))

(define (flashmob-head-exists? id)
  (doc 'type '(-> String Boolean))
  (doc 'description "Check if a head file exists for a session ID")
  (file-exists? (flashmob-head-path id)))

(doc 'section 'compare-and-swap-occ)

(define (flashmob-cas-head! id expected-hash new-hash)
  (doc 'type '(-> String Bytevector Bytevector Boolean))
  (doc 'description "Atomically update head if current value matches expected")
  (doc 'returns "#t if successful, #f if current value differs")
  (doc 'note "Uses file locking to make the check-then-write truly atomic")
  (let ([path (flashmob-head-path id)])
    (with-file-lock path
      (lambda ()
        (let ([current (flashmob-read-head id)])
          (if (or (not current)
                  (not (bytevector=? current expected-hash)))
              #f
              (begin
                (%flashmob-write-head! id new-hash)
                #t)))))))

(doc 'section 'listing-operations)

(define (flashmob-list-heads)
  (doc 'type '(-> (List String)))
  (doc 'description "List all session IDs that have head files")
  (guard (e [else '()])
    (if (file-exists? *flashmob-heads-dir*)
        (let ([entries (directory-list *flashmob-heads-dir*)])
          (flashmob-filter-map
           (lambda (entry)
             (let ([len (string-length entry)])
               (if (and (> len 5)
                        (string=? (substring entry (- len 5) len) ".head"))
                   (substring entry 0 (- len 5))
                   #f)))
           entries))
        '())))

(define (flashmob-filter-map f lst)
  (doc 'type '(-> (-> a (Maybe b)) (List a) (List b)))
  (doc 'description "Map and filter in one pass")
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let ([result (f (car lst))])
          (loop (cdr lst)
                (if result (cons result acc) acc))))))

(doc 'section 'string-utilities)

(define (flashmob-string-trim str)
  (doc 'type '(-> String String))
  (doc 'description "Remove leading and trailing whitespace")
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                  (if (>= i len)
                      len
                      (if (char-whitespace? (string-ref str i))
                          (loop (+ i 1))
                          i)))]
         [end (let loop ([i (- len 1)])
                (if (< i start)
                    start
                    (if (char-whitespace? (string-ref str i))
                        (loop (- i 1))
                        (+ i 1))))])
    (substring str start end)))
