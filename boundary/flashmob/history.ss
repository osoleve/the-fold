;;; boundary/flashmob/history.ss — Flashmob CAS-Based History
;;;
;;; Session history is maintained through block references.
;;; Each session block can reference a previous session, forming an
;;; immutable audit trail in the CAS.
;;;
;;; Benefits over file-based history:
;;;   - Immutable: Old sessions cannot be altered
;;;   - Content-addressed: Full provenance tracking
;;;   - Concurrent-safe: No file locking issues
;;;   - Time-travel: Can inspect any historical state
;;;
;;; This is Shell code: impure (CAS operations).

(load "boundary/flashmob/store.ss")

;;; ====
;;; Session Counter
;;; ====

;;; Persistent counter for session IDs.
;;; Stored in .flashmob/counter file.

(define *flashmob-counter-file* ".flashmob/counter")

;;; flashmob-ensure-dir! : -> Void
;;; Ensure the .flashmob directory exists.
(define (flashmob-ensure-dir!)
  (unless (file-exists? ".flashmob")
    (mkdir ".flashmob")))

;;; flashmob-read-counter : -> Int
;;; Read the current counter value from disk.
(define (flashmob-read-counter)
  (guard (e [else 0])
    (if (file-exists? *flashmob-counter-file*)
        (call-with-input-file *flashmob-counter-file*
          (lambda (port)
            (let ([val (read port)])
              (if (number? val) val 0))))
        0)))

;;; flashmob-write-counter! : Int -> Void
;;; Write the counter value to disk atomically.
(define (flashmob-write-counter! n)
  (flashmob-ensure-dir!)
  (call-with-atomic-output-file *flashmob-counter-file*
    (lambda (port)
      (write n port)
      (newline port))
    '(replace)))

;;; flashmob-next-session-id! : -> String
;;; Generate the next session ID.
;;; Format: "flashmob-NNNN" where NNNN is zero-padded.
(define (flashmob-next-session-id!)
  (let* ([current (flashmob-read-counter)]
         [next (+ current 1)])
    (flashmob-write-counter! next)
    (format "flashmob-~4,'0d" next)))

;;; ====
;;; Session Chain Navigation
;;; ====

;;; flashmob-history-chain : String -> (List String)
;;; Get the chain of session IDs, newest first.
;;; Follows prev-refs to build the history.
(define (flashmob-history-chain session-id)
  (let loop ([id session-id] [acc '()])
    (let ([blk (flashmob-fetch-session id)])
      (if (not blk)
          (reverse acc)
          (let ([prev-hash (session-block-prev blk)])
            (if (not prev-hash)
                (reverse (cons id acc))
                ;; Find the session ID for the prev hash
                (let ([prev-id (flashmob-find-session-by-hash prev-hash)])
                  (if prev-id
                      (loop prev-id (cons id acc))
                      (reverse (cons id acc))))))))))

;;; flashmob-find-session-by-hash : Bytevector -> String | #f
;;; Find a session ID by its block hash.
;;; Searches through all heads.
(define (flashmob-find-session-by-hash hash)
  (let ([all-ids (flashmob-list-heads)])
    (let loop ([ids all-ids])
      (if (null? ids)
          #f
          (let* ([id (car ids)]
                 [head-hash (flashmob-read-head id)])
            (if (and head-hash (bytevector=? head-hash hash))
                id
                ;; Also check history for this session
                (let ([found (flashmob-search-history-for-hash id hash)])
                  (if found
                      found
                      (loop (cdr ids))))))))))

;;; flashmob-search-history-for-hash : String Bytevector -> String | #f
;;; Search a session's history chain for a specific hash.
(define (flashmob-search-history-for-hash session-id target-hash)
  (let ([history (flashmob-session-history session-id)])
    (let loop ([blocks history])
      (if (null? blocks)
          #f
          (let* ([blk (car blocks)]
                 [blk-hash (hash-block blk)])
            (if (bytevector=? blk-hash target-hash)
                session-id  ; Found it - return the session ID
                (loop (cdr blocks))))))))

;;; ====
;;; History Summary
;;; ====

;;; flashmob-history-summary : String -> (List Alist)
;;; Get a summary of session history.
;;; Returns list of (session-id status finding-count triage-status timestamp).
(define (flashmob-history-summary session-id)
  (let ([chain (flashmob-history-chain session-id)])
    (map (lambda (id)
           (let ([data (flashmob-fetch-session-data id)])
             (if data
                 `((session-id . ,id)
                   (status . ,(cdr (assq 'status data)))
                   (finding-count . ,(cdr (assq 'finding-count data)))
                   (created-at . ,(cdr (assq 'created-at data))))
                 `((session-id . ,id)
                   (status . unknown)))))
         chain)))

;;; ====
;;; All Sessions
;;; ====

;;; flashmob-all-sessions : -> (List String)
;;; Get all session IDs (from heads directory).
(define (flashmob-all-sessions)
  (flashmob-list-heads))

;;; flashmob-active-session : -> String | #f
;;; Get the most recently created session.
;;; Returns #f if no sessions exist.
(define (flashmob-active-session)
  (let ([sessions (flashmob-all-sessions)])
    (if (null? sessions)
        #f
        ;; Sort by creation time (newest first) and return first
        (let ([sorted (sort (lambda (a b)
                             (let ([data-a (flashmob-fetch-session-data a)]
                                   [data-b (flashmob-fetch-session-data b)])
                               (string>? (cdr (assq 'created-at data-a))
                                         (cdr (assq 'created-at data-b)))))
                           sessions)])
          (if (null? sorted) #f (car sorted))))))

;;; ====
;;; Session Lifecycle
;;; ====

;;; flashmob-create-session! : (List String) -> String
;;; Create a new session for the given files.
;;; Returns the new session ID.
(define (flashmob-create-session! files)
  (let* ([session-id (flashmob-next-session-id!)]
         [blk (make-session-block session-id files
                                  (vector)    ; No agents yet
                                  (vector)    ; No findings yet
                                  #f          ; No triage yet
                                  #f          ; No prev (new session)
                                  1)]         ; Version 1
         [hash (flashmob-store! blk)])
    (flashmob-write-head! session-id hash)
    session-id))

;;; flashmob-update-session! : String (Vector Bytevector) (Vector Bytevector) (Option Bytevector) -> Bytevector
;;; Update a session with new agents, findings, and optional triage.
;;; Returns the new hash.
(define (flashmob-update-session! session-id agent-refs finding-refs triage-ref)
  (let* ([current-hash (flashmob-read-head session-id)]
         [current-blk (flashmob-fetch current-hash)]
         [current-data (session-block-data current-blk)]
         [version (+ (cdr (assq 'version current-data)) 1)]
         [files (cdr (assq 'files current-data))]
         [new-blk (make-session-block session-id files
                                      agent-refs finding-refs
                                      triage-ref
                                      current-hash  ; Link to previous
                                      version)]
         [new-hash (flashmob-store! new-blk)])
    (flashmob-write-head! session-id new-hash)
    new-hash))

;;; flashmob-close-session! : String -> Void
;;; Close a session (mark as complete, delete head).
;;; The session data remains in CAS for history.
(define (flashmob-close-session! session-id)
  (flashmob-delete-head! session-id))

;;; ====
;;; Session Stats
;;; ====

;;; flashmob-session-stats : String -> Alist | #f
;;; Get statistics for a session.
(define (flashmob-session-stats session-id)
  (let ([blk (flashmob-fetch-session session-id)])
    (if (not blk)
        #f
        (let* ([data (session-block-data blk)]
               [agent-count (cdr (assq 'agent-count data))]
               [finding-count (cdr (assq 'finding-count data))]
               [has-triage (cdr (assq 'has-triage data))])
          `((session-id . ,session-id)
            (status . ,(cdr (assq 'status data)))
            (files . ,(cdr (assq 'files data)))
            (agent-count . ,agent-count)
            (finding-count . ,finding-count)
            (triaged . ,has-triage)
            (version . ,(cdr (assq 'version data)))
            (created-at . ,(cdr (assq 'created-at data))))))))
