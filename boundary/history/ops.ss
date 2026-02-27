(load "boundary/history/replay.ss")

(doc 'module 'boundary/history/ops)
(doc 'description "Core history operations: undo, redo, branching, recording")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/history/replay))

(doc 'section 'timestamp-generation)

(define (current-iso-timestamp)
  (doc 'type (-> String))
  (doc 'description "Generate ISO 8601 timestamp")
  (let ([t (current-time)])
    (format "~a" t)))

(doc 'section 'command-recording)

(define (history-record! session-id command result result-type defined-name)
  (doc 'type (-> String String String Symbol (Option Symbol) Bytevector))
  (doc 'description "Record a command execution")
  (doc 'export #t)
  (doc 'param 'session-id "Session identifier")
  (doc 'param 'command "Command string that was executed")
  (doc 'param 'result "Result value (for hashing)")
  (doc 'param 'result-type "'success or 'error")
  (doc 'param 'defined-name "Symbol defined (or #f)")
  (doc 'returns "Hash of the new entry block")
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
                  1
                  prev-hash)]
         [hash (history-store! entry)])
    (history-write-branch! session-id branch hash)
    (history-clear-redo! session-id)
    (when defined-name
      (history-track-definition! session-id defined-name))
    hash))

(define (history-record-success! session-id command result defined-name)
  (doc 'type (-> String String Any (Option Symbol) Bytevector))
  (doc 'description "Convenience wrapper for successful commands")
  (doc 'export #t)
  (history-record! session-id command result 'success defined-name))

(define (history-record-error! session-id command error-value)
  (doc 'type (-> String String Any Bytevector))
  (doc 'description "Record an error during command execution")
  (doc 'export #t)
  (history-record! session-id command error-value 'error #f))

(doc 'section 'undo-operation)

(define (history-undo! session-id)
  (doc 'type (-> String (List Symbol String)))
  (doc 'description "Undo the last command")
  (doc 'export #t)
  (doc 'returns "(ok <command>) on success, (error <message>) on failure")
  (doc 'note "Pushes current to redo stack, moves head to previous, replays environment")
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
                    (begin
                      (history-push-redo! session-id current-hash)
                      (history-clear-branch-head! session-id branch)
                      (history-reset-environment! session-id)
                      (list 'ok undone-cmd))
                    (begin
                      (history-push-redo! session-id current-hash)
                      (history-write-branch! session-id branch prev-hash)
                      (let* ([prev-entry (history-fetch prev-hash)]
                             [target-index (cdr (assq 'index (history-entry-data prev-entry)))])
                        (history-reset-environment! session-id)
                        (history-replay-to-index session-id target-index 'safe))
                      (list 'ok undone-cmd)))))))))

(doc 'section 'redo-operation)

(define (history-redo! session-id)
  (doc 'type (-> String (List Symbol String)))
  (doc 'description "Redo the last undone command")
  (doc 'export #t)
  (doc 'returns "(ok <command>) on success, (error <message>) on failure")
  (let ([redo-hash (history-pop-redo! session-id)])
    (if (not redo-hash)
        (list 'error "Nothing to redo")
        (let ([redo-entry (history-fetch redo-hash)])
          (if (not redo-entry)
              (list 'error "Redo entry not found")
              (let* ([data (history-entry-data redo-entry)]
                     [cmd (cdr (assq 'command data))]
                     [branch (history-read-current-branch session-id)])
                (let ([result (replay-command cmd session-id)])
                  (cond
                    [(eq? result 'success)
                     (history-write-branch! session-id branch redo-hash)
                     (let ([defined-name (cdr (assq 'defined-name data))])
                       (when defined-name
                         (history-track-definition! session-id defined-name)))
                     (list 'ok cmd)]
                    [else
                     (history-push-redo! session-id redo-hash)
                     (list 'error (format "Redo failed: ~a"
                                          (if (pair? result) (cadr result) result)))]))))))))

(doc 'section 'jump-operation)

(define (history-jump! session-id target-index)
  (doc 'type (-> String Int (List Symbol (U Int String))))
  (doc 'description "Jump to a specific history index")
  (doc 'export #t)
  (doc 'note "Clears redo stack as it creates a new timeline")
  (let* ([branch (history-read-current-branch session-id)]
         [target-entry (history-entry-at-index session-id branch target-index)])
    (if (not target-entry)
        (list 'error (format "No entry at index ~a" target-index))
        (let ([target-hash (hash-block target-entry)])
          (history-write-branch! session-id branch target-hash)
          (history-clear-redo! session-id)
          (history-reset-environment! session-id)
          (let ([result (history-replay-to-index session-id target-index 'safe)])
            (if (and (pair? result) (eq? (car result) 'ok))
                (list 'ok target-index)
                result))))))

(doc 'section 'branch-operations)

(define (history-create-branch! session-id branch-name)
  (doc 'type (-> String Symbol (List Symbol)))
  (doc 'description "Create a new branch from the current position")
  (doc 'export #t)
  (doc 'note "New branch starts at same commit as current branch")
  (let* ([name (symbol->string branch-name)]
         [current-branch (history-read-current-branch session-id)])
    (if (history-branch-exists? session-id name)
        (list 'error (format "Branch '~a' already exists" name))
        (let ([current-hash (history-read-branch session-id current-branch)])
          (when current-hash
            (history-write-branch! session-id name current-hash))
          (history-write-current-branch! session-id name)
          (history-clear-redo! session-id)
          (list 'ok)))))

(define (history-checkout! session-id branch-name)
  (doc 'type (-> String Symbol (List Symbol)))
  (doc 'description "Switch to a different branch")
  (doc 'export #t)
  (doc 'note "Resets environment and replays the target branch")
  (let ([name (symbol->string branch-name)])
    (if (not (history-branch-exists? session-id name))
        (list 'error (format "Branch '~a' does not exist" name))
        (begin
          (history-write-current-branch! session-id name)
          (history-clear-redo! session-id)
          (history-reset-environment! session-id)
          (let ([target-index (history-current-index session-id)])
            (if (>= target-index 0)
                (let ([result (history-replay-to-index session-id target-index 'safe)])
                  (if (and (pair? result) (eq? (car result) 'ok))
                      (list 'ok)
                      result))
                (list 'ok)))))))

(define (history-delete-branch-op! session-id branch-name)
  (doc 'type (-> String Symbol (List Symbol)))
  (doc 'description "Delete a branch (cannot delete current branch)")
  (doc 'export #t)
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

(doc 'section 'history-display)

(define (history-list session-id limit)
  (doc 'type (-> String Int (List Alist)))
  (doc 'description "Get history entries for display (up to limit, newest first)")
  (doc 'export #t)
  (let* ([branch (history-read-current-branch session-id)]
         [entries (history-walk session-id branch)]
         [sorted (reverse entries)]
         [limited (take limit sorted)])
    (map history-entry-data limited)))

(doc 'section 'export)

(define (history-export session-id)
  (doc 'type (-> String String))
  (doc 'description "Export history as executable Scheme script (definitions only)")
  (doc 'export #t)
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

(doc 'section 'session-initialization)

(define (history-init-session! session-id)
  (doc 'type (-> String Void))
  (doc 'description "Initialize history for a new session")
  (doc 'export #t)
  (doc 'note "Creates session directory and sets up main branch")
  (history-ensure-session-dir! session-id)
  (unless (history-branch-exists? session-id "main")
    #f)
  (unless (file-exists? (history-current-branch-path session-id))
    (history-write-current-branch! session-id "main")))
