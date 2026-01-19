;;; boundary/history/heads.ss — History Head File Management
;;;
;;; Manages named heads for history branches in .store/heads/history/<session-id>/
;;; Each session can have multiple branches (main, experiment, etc.)
;;;
;;; Head files contain 66-character hex strings (33-byte CAS addresses).
;;;
;;; Special files:
;;;   __current__.head  - Name of active branch
;;;   __redo__.sexp     - Redo stack as list of entry hashes
;;;
;;; Lock-aware design (matches BBS pattern):
;;;   - Public functions acquire locks
;;;   - Internal %* functions for use when lock already held
;;;
;;; This is Shell code: impure (filesystem IO).

(load "core/base/prelude.ss")
(load "core/blocks/cas.ss")
(load "boundary/io/atomic.ss")
(load "boundary/io/file-lock.ss")

;;; ====
;;; Configuration
;;; ====

(define *history-heads-root* ".store/heads/history")

;;; ====
;;; Path Utilities
;;; ====

;;; history-session-dir : String -> String
;;; Get the directory path for a session's history heads.
(define (history-session-dir session-id)
  (string-append *history-heads-root* "/" session-id))

;;; history-branch-path : String String -> String
;;; Get the filesystem path for a branch head file.
(define (history-branch-path session-id branch-name)
  (string-append (history-session-dir session-id) "/" branch-name ".head"))

;;; history-current-branch-path : String -> String
;;; Get the path to the current branch marker file.
(define (history-current-branch-path session-id)
  (string-append (history-session-dir session-id) "/__current__.head"))

;;; history-redo-stack-path : String -> String
;;; Get the path to the redo stack file.
(define (history-redo-stack-path session-id)
  (string-append (history-session-dir session-id) "/__redo__.sexp"))

;;; history-ensure-session-dir! : String -> Void
;;; Ensure the session's history directory exists.
(define (history-ensure-session-dir! session-id)
  (unless (file-exists? ".store")
    (mkdir ".store"))
  (unless (file-exists? ".store/heads")
    (mkdir ".store/heads"))
  (unless (file-exists? *history-heads-root*)
    (mkdir *history-heads-root*))
  (let ([session-dir (history-session-dir session-id)])
    (unless (file-exists? session-dir)
      (mkdir session-dir))))

;;; ====
;;; Branch Head Operations
;;; ====

;;; history-read-branch : String String -> Bytevector | #f
;;; Read the current hash for a branch.
(define (history-read-branch session-id branch-name)
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

;;; %history-write-branch! : String String Bytevector -> Void
;;; INTERNAL: Write branch head (caller must hold lock).
(define (%history-write-branch! session-id branch-name hash)
  (history-ensure-session-dir! session-id)
  (let ([path (history-branch-path session-id branch-name)]
        [hex (hash->hex hash)])
    (call-with-atomic-output-file path
      (lambda (port)
        (put-string port hex)
        (newline port))
      '(replace))))

;;; history-write-branch! : String String Bytevector -> Void
;;; PUBLIC: Write branch head with locking.
(define (history-write-branch! session-id branch-name hash)
  (let ([path (history-branch-path session-id branch-name)])
    (with-file-lock path
      (lambda ()
        (%history-write-branch! session-id branch-name hash)))))

;;; %history-delete-branch! : String String -> Void
;;; INTERNAL: Delete a branch head file (caller must hold lock).
(define (%history-delete-branch! session-id branch-name)
  (let ([path (history-branch-path session-id branch-name)])
    (when (file-exists? path)
      (delete-file path))))

;;; history-delete-branch! : String String -> Void
;;; PUBLIC: Delete a branch head file.
(define (history-delete-branch! session-id branch-name)
  (let ([path (history-branch-path session-id branch-name)])
    (with-file-lock path
      (lambda ()
        (%history-delete-branch! session-id branch-name)))))

;;; history-clear-branch-head! : String String -> Void
;;; Clear a branch head to empty state (branch still exists but has no entries).
;;; This is used by undo when reverting to before the first command.
;;; The file is kept with an "empty" marker so redo can still work.
(define (history-clear-branch-head! session-id branch-name)
  (history-ensure-session-dir! session-id)
  (let ([path (history-branch-path session-id branch-name)])
    (with-file-lock path
      (lambda ()
        (call-with-atomic-output-file path
          (lambda (port)
            (put-string port "empty")  ; Marker that history-read-branch returns #f for
            (newline port))
          '(replace))))))

;;; history-branch-exists? : String String -> Boolean
;;; Check if a branch exists.
(define (history-branch-exists? session-id branch-name)
  (file-exists? (history-branch-path session-id branch-name)))

;;; ====
;;; Current Branch Management
;;; ====

;;; history-read-current-branch : String -> String
;;; Read the current branch name. Defaults to "main" if not set.
(define (history-read-current-branch session-id)
  (let ([path (history-current-branch-path session-id)])
    (guard (e [else "main"])
      (if (file-exists? path)
          (let ([content (call-with-input-file path
                           (lambda (port) (get-line port)))])
            (string-trim content))
          "main"))))

;;; history-write-current-branch! : String String -> Void
;;; Set the current branch name.
(define (history-write-current-branch! session-id branch-name)
  (history-ensure-session-dir! session-id)
  (let ([path (history-current-branch-path session-id)])
    (with-file-lock path
      (lambda ()
        (call-with-atomic-output-file path
          (lambda (port)
            (put-string port branch-name)
            (newline port))
          '(replace))))))

;;; ====
;;; Redo Stack Management
;;; ====

;;; %history-read-redo-stack : String -> (List Bytevector)
;;; INTERNAL: Read redo stack (caller must hold lock).
(define (%history-read-redo-stack session-id)
  (let ([path (history-redo-stack-path session-id)])
    (guard (e [else '()])
      (if (file-exists? path)
          (let ([content (call-with-input-file path get-string-all)])
            (let ([data (read (open-input-string content))])
              (if (list? data)
                  (map hex->hash data)
                  '())))
          '()))))

;;; history-read-redo-stack : String -> (List Bytevector)
;;; PUBLIC: Read the redo stack for a session.
(define (history-read-redo-stack session-id)
  (%history-read-redo-stack session-id))

;;; %history-write-redo-stack! : String (List Bytevector) -> Void
;;; INTERNAL: Write redo stack (caller must hold lock).
(define (%history-write-redo-stack! session-id stack)
  (history-ensure-session-dir! session-id)
  (let ([path (history-redo-stack-path session-id)]
        [hex-list (map hash->hex stack)])
    (call-with-atomic-output-file path
      (lambda (port)
        (put-string port (format "~s" hex-list))
        (newline port))
      '(replace))))

;;; history-write-redo-stack! : String (List Bytevector) -> Void
;;; PUBLIC: Write the redo stack for a session.
(define (history-write-redo-stack! session-id stack)
  (let ([path (history-redo-stack-path session-id)])
    (with-file-lock path
      (lambda ()
        (%history-write-redo-stack! session-id stack)))))

;;; history-push-redo! : String Bytevector -> Void
;;; Push an entry hash onto the redo stack.
;;; NOTE: Acquires lock across entire read-modify-write to prevent race.
(define (history-push-redo! session-id hash)
  (let ([path (history-redo-stack-path session-id)])
    (with-file-lock path
      (lambda ()
        (let ([stack (%history-read-redo-stack session-id)])
          (%history-write-redo-stack! session-id (cons hash stack)))))))

;;; history-pop-redo! : String -> Bytevector | #f
;;; Pop an entry hash from the redo stack.
;;; NOTE: Acquires lock across entire read-modify-write to prevent race.
(define (history-pop-redo! session-id)
  (let ([path (history-redo-stack-path session-id)])
    (with-file-lock path
      (lambda ()
        (let ([stack (%history-read-redo-stack session-id)])
          (if (null? stack)
              #f
              (begin
                (%history-write-redo-stack! session-id (cdr stack))
                (car stack))))))))

;;; history-clear-redo! : String -> Void
;;; Clear the redo stack (called on new command after undo).
(define (history-clear-redo! session-id)
  (history-write-redo-stack! session-id '()))

;;; ====
;;; Compare-and-Swap
;;; ====

;;; history-cas-branch! : String String Bytevector Bytevector -> Boolean
;;; Atomically update branch head if current value matches expected.
;;; Returns #t if successful, #f if current value differs.
(define (history-cas-branch! session-id branch-name expected-hash new-hash)
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

;;; ====
;;; Listing Operations
;;; ====

;;; history-list-branches : String -> (List String)
;;; List all branch names for a session.
(define (history-list-branches session-id)
  (let ([session-dir (history-session-dir session-id)])
    (guard (e [else '()])
      (if (file-exists? session-dir)
          (let ([entries (directory-list session-dir)])
            (filter-map
              (lambda (entry)
                (let ([len (string-length entry)])
                  (cond
                    ;; Skip special files
                    [(string=? entry "__current__.head") #f]
                    [(string=? entry "__redo__.sexp") #f]
                    ;; Extract branch name from .head files
                    [(and (> len 5)
                          (string=? (substring entry (- len 5) len) ".head"))
                     (substring entry 0 (- len 5))]
                    [else #f])))
              entries))
          '()))))

;;; history-session-exists? : String -> Boolean
;;; Check if a session has any history.
(define (history-session-exists? session-id)
  (file-exists? (history-session-dir session-id)))

;;; ====
;;; Utilities
;;; ====

;;; filter-map : (a -> b | #f) (List a) -> (List b)
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let ([result (f (car lst))])
          (loop (cdr lst)
                (if result (cons result acc) acc))))))

;;; string-trim : String -> String
(define (string-trim str)
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
