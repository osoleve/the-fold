(load "core/base/prelude.ss")
(load "core/blocks/cas.ss")
(load "boundary/io/atomic.ss")
(load "boundary/io/file-lock.ss")

(doc 'module 'boundary/history/heads)
(doc 'description "History head file management for branch tracking")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude core/blocks/cas boundary/io/atomic boundary/io/file-lock))
(doc 'note "Lock-aware design with public/internal functions")

(doc 'section 'configuration)

(doc *history-heads-root* 'type 'string)
(doc *history-heads-root* 'description "Root directory for history head files")
(define *history-heads-root* ".store/heads/history")

(doc 'section 'path-utilities)

(define (history-session-dir session-id)
  (doc 'type (-> String String))
  (doc 'description "Get directory path for a session's history heads")
  (doc 'export #t)
  (string-append *history-heads-root* "/" session-id))

(define (history-branch-path session-id branch-name)
  (doc 'type (-> String String String))
  (doc 'description "Get filesystem path for a branch head file")
  (doc 'export #t)
  (string-append (history-session-dir session-id) "/" branch-name ".head"))

(define (history-current-branch-path session-id)
  (doc 'type (-> String String))
  (doc 'description "Get path to the current branch marker file")
  (doc 'export #t)
  (string-append (history-session-dir session-id) "/__current__.head"))

(define (history-redo-stack-path session-id)
  (doc 'type (-> String String))
  (doc 'description "Get path to the redo stack file")
  (doc 'export #t)
  (string-append (history-session-dir session-id) "/__redo__.sexp"))

(define (history-ensure-session-dir! session-id)
  (doc 'type (-> String Void))
  (doc 'description "Ensure the session's history directory exists")
  (doc 'export #t)
  (unless (file-exists? ".store")
    (mkdir ".store"))
  (unless (file-exists? ".store/heads")
    (mkdir ".store/heads"))
  (unless (file-exists? *history-heads-root*)
    (mkdir *history-heads-root*))
  (let ([session-dir (history-session-dir session-id)])
    (unless (file-exists? session-dir)
      (mkdir session-dir))))

(doc 'section 'branch-head-operations)

(define (history-read-branch session-id branch-name)
  (doc 'type (-> String String (Option Bytevector)))
  (doc 'description "Read the current hash for a branch")
  (doc 'export #t)
  (let ([path (history-branch-path session-id branch-name)])
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

(define (%history-write-branch! session-id branch-name hash)
  (doc 'type (-> String String Bytevector Void))
  (doc 'description "INTERNAL: Write branch head (caller must hold lock)")
  (history-ensure-session-dir! session-id)
  (let ([path (history-branch-path session-id branch-name)]
        [hex (hash->hex hash)])
    (call-with-atomic-output-file path
      (lambda (port)
        (put-string port hex)
        (newline port))
      '(replace))))

(define (history-write-branch! session-id branch-name hash)
  (doc 'type (-> String String Bytevector Void))
  (doc 'description "PUBLIC: Write branch head with locking")
  (doc 'export #t)
  (let ([path (history-branch-path session-id branch-name)])
    (with-file-lock path
      (lambda ()
        (%history-write-branch! session-id branch-name hash)))))

(define (%history-delete-branch! session-id branch-name)
  (doc 'type (-> String String Void))
  (doc 'description "INTERNAL: Delete a branch head file (caller must hold lock)")
  (let ([path (history-branch-path session-id branch-name)])
    (when (file-exists? path)
      (delete-file path))))

(define (history-delete-branch! session-id branch-name)
  (doc 'type (-> String String Void))
  (doc 'description "PUBLIC: Delete a branch head file")
  (doc 'export #t)
  (let ([path (history-branch-path session-id branch-name)])
    (with-file-lock path
      (lambda ()
        (%history-delete-branch! session-id branch-name)))))

(define (history-clear-branch-head! session-id branch-name)
  (doc 'type (-> String String Void))
  (doc 'description "Clear branch head to empty state (for undo to before first command)")
  (doc 'export #t)
  (doc 'note "Keeps file with 'empty' marker so redo can still work")
  (history-ensure-session-dir! session-id)
  (let ([path (history-branch-path session-id branch-name)])
    (with-file-lock path
      (lambda ()
        (call-with-atomic-output-file path
          (lambda (port)
            (put-string port "empty")
            (newline port))
          '(replace))))))

(define (history-branch-exists? session-id branch-name)
  (doc 'type (-> String String Boolean))
  (doc 'description "Check if a branch exists")
  (doc 'export #t)
  (file-exists? (history-branch-path session-id branch-name)))

(doc 'section 'current-branch-management)

(define (history-read-current-branch session-id)
  (doc 'type (-> String String))
  (doc 'description "Read the current branch name (defaults to 'main')")
  (doc 'export #t)
  (let ([path (history-current-branch-path session-id)])
    (guard (e [else "main"])
      (if (file-exists? path)
          (let ([content (call-with-input-file path
                           (lambda (port) (get-line port)))])
            (string-trim content))
          "main"))))

(define (history-write-current-branch! session-id branch-name)
  (doc 'type (-> String String Void))
  (doc 'description "Set the current branch name")
  (doc 'export #t)
  (history-ensure-session-dir! session-id)
  (let ([path (history-current-branch-path session-id)])
    (with-file-lock path
      (lambda ()
        (call-with-atomic-output-file path
          (lambda (port)
            (put-string port branch-name)
            (newline port))
          '(replace))))))

(doc 'section 'redo-stack-management)

(define (%history-read-redo-stack session-id)
  (doc 'type (-> String (List Bytevector)))
  (doc 'description "INTERNAL: Read redo stack (caller must hold lock)")
  (let ([path (history-redo-stack-path session-id)])
    (guard (e [else '()])
      (if (file-exists? path)
          (let ([content (call-with-input-file path get-string-all)])
            (let ([data (read (open-input-string content))])
              (if (list? data)
                  (map hex->hash data)
                  '())))
          '()))))

(define (history-read-redo-stack session-id)
  (doc 'type (-> String (List Bytevector)))
  (doc 'description "PUBLIC: Read the redo stack for a session")
  (doc 'export #t)
  (%history-read-redo-stack session-id))

(define (%history-write-redo-stack! session-id stack)
  (doc 'type (-> String (List Bytevector) Void))
  (doc 'description "INTERNAL: Write redo stack (caller must hold lock)")
  (history-ensure-session-dir! session-id)
  (let ([path (history-redo-stack-path session-id)]
        [hex-list (map hash->hex stack)])
    (call-with-atomic-output-file path
      (lambda (port)
        (put-string port (format "~s" hex-list))
        (newline port))
      '(replace))))

(define (history-write-redo-stack! session-id stack)
  (doc 'type (-> String (List Bytevector) Void))
  (doc 'description "PUBLIC: Write the redo stack for a session")
  (doc 'export #t)
  (let ([path (history-redo-stack-path session-id)])
    (with-file-lock path
      (lambda ()
        (%history-write-redo-stack! session-id stack)))))

(define (history-push-redo! session-id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Push an entry hash onto the redo stack")
  (doc 'export #t)
  (doc 'note "Acquires lock across entire read-modify-write to prevent race")
  (let ([path (history-redo-stack-path session-id)])
    (with-file-lock path
      (lambda ()
        (let ([stack (%history-read-redo-stack session-id)])
          (%history-write-redo-stack! session-id (cons hash stack)))))))

(define (history-pop-redo! session-id)
  (doc 'type (-> String (Option Bytevector)))
  (doc 'description "Pop an entry hash from the redo stack")
  (doc 'export #t)
  (doc 'note "Acquires lock across entire read-modify-write to prevent race")
  (let ([path (history-redo-stack-path session-id)])
    (with-file-lock path
      (lambda ()
        (let ([stack (%history-read-redo-stack session-id)])
          (if (null? stack)
              #f
              (begin
                (%history-write-redo-stack! session-id (cdr stack))
                (car stack))))))))

(define (history-clear-redo! session-id)
  (doc 'type (-> String Void))
  (doc 'description "Clear the redo stack (called on new command after undo)")
  (doc 'export #t)
  (history-write-redo-stack! session-id '()))

(doc 'section 'compare-and-swap)

(define (history-cas-branch! session-id branch-name expected-hash new-hash)
  (doc 'type (-> String String Bytevector Bytevector Boolean))
  (doc 'description "Atomically update branch head if current value matches expected")
  (doc 'export #t)
  (doc 'returns "True if successful, false if current value differs")
  (let ([path (history-branch-path session-id branch-name)])
    (with-file-lock path
      (lambda ()
        (let ([current (history-read-branch session-id branch-name)])
          (if (or (not current)
                  (not (bytevector=? current expected-hash)))
              #f
              (begin
                (%history-write-branch! session-id branch-name new-hash)
                #t)))))))

(doc 'section 'listing-operations)

(define (history-list-branches session-id)
  (doc 'type (-> String (List String)))
  (doc 'description "List all branch names for a session")
  (doc 'export #t)
  (let ([session-dir (history-session-dir session-id)])
    (guard (e [else '()])
      (if (file-exists? session-dir)
          (let ([entries (directory-list session-dir)])
            (filter-map
              (lambda (entry)
                (let ([len (string-length entry)])
                  (cond
                    [(string=? entry "__current__.head") #f]
                    [(string=? entry "__redo__.sexp") #f]
                    [(and (> len 5)
                          (string=? (substring entry (- len 5) len) ".head"))
                     (substring entry 0 (- len 5))]
                    [else #f])))
              entries))
          '()))))

(define (history-session-exists? session-id)
  (doc 'type (-> String Boolean))
  (doc 'description "Check if a session has any history")
  (doc 'export #t)
  (file-exists? (history-session-dir session-id)))

(doc 'section 'utilities)

;;; filter-map and string-trim provided by prelude
