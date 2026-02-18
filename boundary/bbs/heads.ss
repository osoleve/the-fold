(load "core/base/prelude.ss")
(load "core/blocks/cas.ss")
(load "boundary/io/atomic.ss")
(load "boundary/io/file-lock.ss")

(doc 'module 'boundary/bbs/heads)
(doc 'description "BBS Head File Management - Manages named heads for issues in .store/heads/bbs/. Each issue has a head file containing its current block hash. Head files contain 66-character hex strings: 2 chars version byte (00), 64 chars SHA-256 hash")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude core/blocks/cas boundary/io/atomic boundary/io/file-lock))
(doc 'note "Lock-aware design: Public functions (bbs-write-head!, bbs-delete-head!) acquire locks. Internal functions (%bbs-write-head!, %bbs-delete-head!) for use when lock already held")

(doc 'section 'configuration)

(doc *bbs-heads-dir* 'type 'String)
(doc *bbs-heads-dir* 'description "Directory for BBS head files")
(define *bbs-heads-dir* ".store/heads/bbs")

(doc 'section 'path-utilities)

(define (bbs-head-path id)
  (doc 'type '(-> String String))
  (doc 'description "Get the filesystem path for an issue's head file. Uses board context if active, otherwise falls back to flat layout")
  (doc 'export #t)
  (string-append (bbs-current-heads-dir) "/" id ".head"))

(define (bbs-current-heads-dir)
  (doc 'type '(-> String))
  (doc 'description "Get the heads directory for the current context (board-aware or fallback).
When no board context is active but the board registry is loaded, routes to
the default 'issues' board to avoid writing to the legacy flat directory.")
  (doc 'export #t)
  (cond
   [(and (top-level-bound? 'board-context?)
         (board-context?))
    (board-heads-dir (current-board))]
   ;; No board context: route to default issues board if registry is loaded
   [(and (top-level-bound? 'board-ref)
         (board-ref "issues"))
    (board-heads-dir (board-ref "issues"))]
   [else *bbs-heads-dir*]))

(define (bbs-ensure-heads-dir!)
  (doc 'type '(-> Void))
  (doc 'description "Ensure the heads directory exists (board-aware)")
  (doc 'export #t)
  (unless (file-exists? ".store")
    (mkdir ".store"))
  (unless (file-exists? ".store/heads")
    (mkdir ".store/heads"))
  (unless (file-exists? ".store/heads/bbs")
    (mkdir ".store/heads/bbs"))
  (let ([dir (bbs-current-heads-dir)])
    (unless (file-exists? dir)
      (mkdir dir))))

(doc 'section 'read-write-operations)

(define (bbs-read-head id)
  (doc 'type '(-> String (Option Bytevector)))
  (doc 'description "Read the current hash for an issue ID. Returns #f if the head file doesn't exist")
  (doc 'export #t)
  (let ([path (bbs-head-path id)])
    (guard (e [else #f])
      (if (file-exists? path)
          (let* ([content (call-with-input-file path
                           (lambda (port) (get-line port)))]
                 [trimmed (string-trim content)]
                 [hex (if (and (> (string-length trimmed) 2)
                               (string=? (substring trimmed 0 2) "0x"))
                          (substring trimmed 2 (string-length trimmed))
                          trimmed)])
            (let ([len (string-length hex)])
              (if (or (= len 64) (= len 66))
                  (hex->hash hex)
                  #f)))
          #f))))

(define (%bbs-write-head! id hash)
  (doc 'type '(-> String Bytevector Void))
  (doc 'description "INTERNAL: Write the current hash for an issue ID (caller must hold lock). Uses atomic write-then-rename to prevent corruption")
  (bbs-ensure-heads-dir!)
  (let ([path (bbs-head-path id)]
        [hex (hash->hex hash)])
    (call-with-atomic-output-file path
      (lambda (port)
        (put-string port hex)
        (newline port))
      '(replace))))

(define (bbs-write-head! id hash)
  (doc 'type '(-> String Bytevector Void))
  (doc 'description "PUBLIC: Write the current hash for an issue ID with locking. Uses atomic write-then-rename to prevent corruption")
  (doc 'export #t)
  (let ([path (bbs-head-path id)])
    (with-file-lock path
      (lambda ()
        (%bbs-write-head! id hash)))))

(define (%bbs-delete-head! id)
  (doc 'type '(-> String Void))
  (doc 'description "INTERNAL: Delete the head file for an issue (caller must hold lock)")
  (let ([path (bbs-head-path id)])
    (when (file-exists? path)
      (delete-file path))))

(define (bbs-delete-head! id)
  (doc 'type '(-> String Void))
  (doc 'description "PUBLIC: Delete the head file for an issue (for closing/deleting)")
  (doc 'export #t)
  (let ([path (bbs-head-path id)])
    (with-file-lock path
      (lambda ()
        (%bbs-delete-head! id)))))

(define (bbs-head-exists? id)
  (doc 'type '(-> String Boolean))
  (doc 'description "Check if a head file exists for an issue ID")
  (doc 'export #t)
  (file-exists? (bbs-head-path id)))

(doc 'section 'compare-and-swap)

(define (bbs-cas-head! id expected-hash new-hash)
  (doc 'type '(-> String Bytevector Bytevector Boolean))
  (doc 'description "Atomically update head if current value matches expected. Returns #t if successful, #f if current value differs. Uses file locking to make the check-then-write truly atomic")
  (doc 'export #t)
  (let ([path (bbs-head-path id)])
    (with-file-lock path
      (lambda ()
        (let ([current (bbs-read-head id)])
          (if (or (not current)
                  (not (bytevector=? current expected-hash)))
              #f
              (begin
                (%bbs-write-head! id new-hash)
                #t)))))))

(doc 'section 'listing-operations)

(define (bbs-list-heads)
  (doc 'type '(-> (List String)))
  (doc 'description "List all IDs that have head files in the current heads directory")
  (doc 'export #t)
  (let ([dir (bbs-current-heads-dir)])
    (guard (e [else '()])
      (if (file-exists? dir)
          (let ([entries (directory-list dir)])
            (filter-map
             (lambda (entry)
               (let ([len (string-length entry)])
                 (if (and (> len 5)
                          (string=? (substring entry (- len 5) len) ".head"))
                     (substring entry 0 (- len 5))
                     #f)))
             entries))
          '()))))

(define (bbs-list-heads-in dir)
  (doc 'type '(-> String (List String)))
  (doc 'description "List all IDs with head files in a specific directory")
  (doc 'export #t)
  (guard (e [else '()])
    (if (file-exists? dir)
        (let ([entries (directory-list dir)])
          (filter-map
           (lambda (entry)
             (let ([len (string-length entry)])
               (if (and (> len 5)
                        (string=? (substring entry (- len 5) len) ".head"))
                   (substring entry 0 (- len 5))
                   #f)))
           entries))
        '())))

(define (bbs-list-issue-heads)
  (doc 'type '(-> (List String)))
  (doc 'description "List only issue IDs (fold-* prefix), excluding posts. Use this for issue index operations to avoid counting posts. In board context, lists all heads (no filtering needed since boards are isolated)")
  (doc 'export #t)
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      ;; In board context, all heads belong to this board
      (bbs-list-heads)
      ;; Legacy: filter by prefix in flat layout
      (filter (lambda (id)
                (and (>= (string-length id) 5)
                     (string=? (substring id 0 5) "fold-")))
              (bbs-list-heads))))

;;; filter-map and string-trim provided by prelude
