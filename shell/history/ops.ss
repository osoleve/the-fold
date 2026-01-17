;;; shell/history/ops.ss — Core History Operations
;;;
;;; Implements undo, redo, branching, and history recording.
;;;
;;; Operations:
;;;   history-record!     - Record a new command
;;;   history-undo!       - Undo last command
;;;   history-redo!       - Redo undone command
;;;   history-jump!       - Jump to specific index
;;;   history-branch!     - Create a new branch
;;;   history-checkout!   - Switch branches
;;;   history-delete-branch! - Delete a branch
;;;
;;; This is Shell code: impure (state mutation, CAS writes).

(load "shell/history/replay.ss")

;;; ====
;;; Timestamp Generation
;;; ====

;;; current-iso-timestamp : -> String
;;; Generate ISO 8601 timestamp.
(define (current-iso-timestamp)
  (let ([t (current-time)])
    (format "~a" t)))  ; Chez time object has reasonable string rep

;;; ====
;;; Command Recording
;;; ====

;;; history-record! : String String String Symbol (Option Symbol) -> Bytevector
;;; Record a successful command execution.
;;;
;;; Arguments:
;;;   session-id - Session identifier
;;;   command    - Command string that was executed
;;;   result     - Result value (for hashing)
;;;   result-type - 'success or 'error
;;;   defined-name - Symbol defined (or #f)
;;;
;;; Returns: hash of the new entry block
(define (history-record! session-id command result result-type defined-name)
  (let* ([branch (history-read-current-branch session-id)]
         [prev-hash (history-read-branch session-id branch)]
         [prev-index (if prev-hash
                         (let ([prev-entry (history-fetch prev-hash)])
                           (if prev-entry
                               (cdr (assq 'index (history-entry-data prev-entry)))
                               -1))
                         -1)]
         [new-index (+ prev-index 1)]
         [cmd-type (classify-command command)]
         [result-hash (if (eq? result-type 'success)
                          (hash->hex (sha256 (string->utf8 (format "~s" result))))
                          "")]
         [timestamp (current-iso-timestamp)]
         [entry (make-history-entry-block
                  session-id
                  new-index
                  command
                  cmd-type
                  result-type
                  result-hash
                  defined-name
                  timestamp
                  1  ; version
                  prev-hash)]
         [hash (history-store! entry)])
    ;; Update branch head
    (history-write-branch! session-id branch hash)
    ;; Clear redo stack (new command invalidates redo)
    (history-clear-redo! session-id)
    ;; Track definition if applicable
    (when defined-name
      (history-track-definition! session-id defined-name))
    hash))

;;; history-record-success! : String String Any (Option Symbol) -> Bytevector
;;; Convenience wrapper for successful commands.
(define (history-record-success! session-id command result defined-name)
  (history-record! session-id command result 'success defined-name))

;;; history-record-error! : String String Any -> Bytevector
;;; Record an error during command execution.
(define (history-record-error! session-id command error-value)
  (history-record! session-id command error-value 'error #f))

;;; ====
;;; Undo Operation
;;; ====

;;; history-undo! : String -> (ok String) | (error String)
;;; Undo the last command.
;;;
;;; Strategy:
;;;   1. Get current entry
;;;   2. Push current to redo stack
;;;   3. Move head to previous entry
;;;   4. Replay environment to new position
;;;
;;; Returns:
;;;   (ok <undone-command>) on success
;;;   (error <message>) on failure
(define (history-undo! session-id)
  (let* ([branch (history-read-current-branch session-id)]
         [current-hash (history-read-branch session-id branch)])
    (if (not current-hash)
        (list 'error "No history to undo")
        (let ([current-entry (history-fetch current-hash)])
          (if (not current-entry)
              (list 'error "Current entry not found")
              (let ([prev-hash (history-entry-prev current-entry)]
                    [undone-cmd (history-entry-command current-entry)])
                (if (not prev-hash)
                    ;; No previous - we're at the beginning
                    (begin
                      ;; Push to redo stack
                      (history-push-redo! session-id current-hash)
                      ;; Clear the branch head (keep file so redo works)
                      (history-clear-branch-head! session-id branch)
                      ;; Reset environment
                      (history-reset-environment! session-id)
                      (list 'ok undone-cmd))
                    ;; Has previous - move back
                    (begin
                      ;; Push to redo stack
                      (history-push-redo! session-id current-hash)
                      ;; Update head to previous
                      (history-write-branch! session-id branch prev-hash)
                      ;; Replay to new position
                      (let* ([prev-entry (history-fetch prev-hash)]
                             [target-index (cdr (assq 'index (history-entry-data prev-entry)))])
                        (history-reset-environment! session-id)
                        (history-replay-to-index session-id target-index 'safe))
                      (list 'ok undone-cmd)))))))))

;;; ====
;;; Redo Operation
;;; ====

;;; history-redo! : String -> (ok String) | (error String)
;;; Redo the last undone command.
;;;
;;; Returns:
;;;   (ok <redone-command>) on success
;;;   (error <message>) on failure
(define (history-redo! session-id)
  (let ([redo-hash (history-pop-redo! session-id)])
    (if (not redo-hash)
        (list 'error "Nothing to redo")
        (let ([redo-entry (history-fetch redo-hash)])
          (if (not redo-entry)
              (list 'error "Redo entry not found")
              (let* ([data (history-entry-data redo-entry)]
                     [cmd (cdr (assq 'command data))]
                     [branch (history-read-current-branch session-id)])
                ;; Execute the command
                (let ([result (replay-command cmd session-id)])
                  (cond
                    [(eq? result 'success)
                     ;; Update head to redo entry
                     (history-write-branch! session-id branch redo-hash)
                     ;; Track definition if applicable
                     (let ([defined-name (cdr (assq 'defined-name data))])
                       (when defined-name
                         (history-track-definition! session-id defined-name)))
                     (list 'ok cmd)]
                    [else
                     ;; Redo failed - push back onto stack
                     (history-push-redo! session-id redo-hash)
                     (list 'error (format "Redo failed: ~a"
                                          (if (pair? result) (cadr result) result)))]))))))))

;;; ====
;;; Jump Operation
;;; ====

;;; history-jump! : String Int -> (ok Int) | (error String)
;;; Jump to a specific history index.
;;;
;;; Note: This clears the redo stack as it creates a new timeline.
(define (history-jump! session-id target-index)
  (let* ([branch (history-read-current-branch session-id)]
         [target-entry (history-entry-at-index session-id branch target-index)])
    (if (not target-entry)
        (list 'error (format "No entry at index ~a" target-index))
        (let ([target-hash (hash-block target-entry)])
          ;; Update head to target
          (history-write-branch! session-id branch target-hash)
          ;; Clear redo stack
          (history-clear-redo! session-id)
          ;; Replay to target
          (history-reset-environment! session-id)
          (let ([result (history-replay-to-index session-id target-index 'safe)])
            (if (and (pair? result) (eq? (car result) 'ok))
                (list 'ok target-index)
                result))))))

;;; ====
;;; Branch Operations
;;; ====

;;; history-create-branch! : String Symbol -> (ok) | (error String)
;;; Create a new branch from the current position.
;;;
;;; The new branch starts at the same commit as the current branch.
(define (history-create-branch! session-id branch-name)
  (let* ([name (symbol->string branch-name)]
         [current-branch (history-read-current-branch session-id)])
    (if (history-branch-exists? session-id name)
        (list 'error (format "Branch '~a' already exists" name))
        (let ([current-hash (history-read-branch session-id current-branch)])
          ;; Create new branch at current position
          (when current-hash
            (history-write-branch! session-id name current-hash))
          ;; Switch to new branch
          (history-write-current-branch! session-id name)
          ;; Clear redo stack
          (history-clear-redo! session-id)
          (list 'ok)))))

;;; history-checkout! : String Symbol -> (ok) | (error String)
;;; Switch to a different branch.
;;;
;;; Note: This resets the environment and replays the target branch.
(define (history-checkout! session-id branch-name)
  (let ([name (symbol->string branch-name)])
    (if (not (history-branch-exists? session-id name))
        (list 'error (format "Branch '~a' does not exist" name))
        (begin
          ;; Switch branch
          (history-write-current-branch! session-id name)
          ;; Clear redo stack
          (history-clear-redo! session-id)
          ;; Reset and replay
          (history-reset-environment! session-id)
          (let ([target-index (history-current-index session-id)])
            (if (>= target-index 0)
                (let ([result (history-replay-to-index session-id target-index 'safe)])
                  (if (and (pair? result) (eq? (car result) 'ok))
                      (list 'ok)
                      result))
                (list 'ok)))))))  ; Empty branch

;;; history-delete-branch-op! : String Symbol -> (ok) | (error String)
;;; Delete a branch.
;;;
;;; Cannot delete the current branch.
(define (history-delete-branch-op! session-id branch-name)
  (let* ([name (symbol->string branch-name)]
         [current (history-read-current-branch session-id)])
    (cond
      [(string=? name current)
       (list 'error "Cannot delete current branch")]
      [(not (history-branch-exists? session-id name))
       (list 'error (format "Branch '~a' does not exist" name))]
      [else
       (history-delete-branch! session-id name)
       (list 'ok)])))

;;; ====
;;; History Display
;;; ====

;;; history-list : String Int -> (List Alist)
;;; Get history entries for display.
;;; Returns up to `limit` entries, newest first.
(define (history-list session-id limit)
  (let* ([branch (history-read-current-branch session-id)]
         [entries (history-walk session-id branch)]
         [sorted (reverse entries)]  ; newest first
         [limited (take-at-most sorted limit)])
    (map history-entry-data limited)))

;;; take-at-most : (List a) Int -> (List a)
(define (take-at-most lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-at-most (cdr lst) (- n 1)))))

;;; ====
;;; Export
;;; ====

;;; history-export : String -> String
;;; Export history as an executable Scheme script.
;;;
;;; Only exports definitions (skips effects and expressions).
(define (history-export session-id)
  (let* ([branch (history-read-current-branch session-id)]
         [entries (history-walk session-id branch)]
         [definitions (filter
                        (lambda (entry)
                          (eq? (cdr (assq 'cmd-type (history-entry-data entry)))
                               'definition))
                        entries)])
    (apply string-append
           (map (lambda (entry)
                  (string-append (cdr (assq 'command (history-entry-data entry)))
                                 "\n"))
                definitions))))

;;; ====
;;; Session Initialization
;;; ====

;;; history-init-session! : String -> Void
;;; Initialize history for a new session.
;;; Creates the session directory and sets up main branch.
(define (history-init-session! session-id)
  (history-ensure-session-dir! session-id)
  (unless (history-branch-exists? session-id "main")
    ;; Just ensure directory exists; head file created on first command
    #f)
  (unless (file-exists? (history-current-branch-path session-id))
    (history-write-current-branch! session-id "main")))
