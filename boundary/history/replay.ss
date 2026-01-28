(load "boundary/history/store.ss")

(doc 'module 'boundary/history/replay)
(doc 'description "Command replay engine for restoring environment state")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/history/store))
(doc 'note "Chez Scheme doesn't support true unbinding, so we use tombstone pattern")

(doc 'section 'replay-state)

(doc *history-tracked-definitions* 'type 'hashtable)
(doc *history-tracked-definitions* 'description "Tracks which symbols have been defined in each session")
(define *history-tracked-definitions*
  (make-hashtable string-hash string=?))

(define (history-track-definition! session-id symbol)
  (doc 'type (-> String Symbol Void))
  (doc 'description "Record that a symbol was defined in a session")
  (doc 'export #t)
  (let ([current (hashtable-ref *history-tracked-definitions* session-id '())])
    (unless (memq symbol current)
      (hashtable-set! *history-tracked-definitions* session-id
                      (cons symbol current)))))

(define (history-tracked-definitions session-id)
  (doc 'type (-> String (List Symbol)))
  (doc 'description "Get all tracked definitions for a session")
  (doc 'export #t)
  (hashtable-ref *history-tracked-definitions* session-id '()))

(doc 'section 'environment-reset)

(doc *history-tombstone* 'type 'any)
(doc *history-tombstone* 'description "Marker value for undefined bindings")
(define *history-tombstone* (list 'history-undefined-tombstone))

(define (history-tombstone? v)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a value is our tombstone marker")
  (and (pair? v)
       (eq? (car v) 'history-undefined-tombstone)))

(define (history-reset-environment! session-id)
  (doc 'type (-> String Void))
  (doc 'description "Reset environment by tombstoning all tracked definitions")
  (doc 'export #t)
  (doc 'note "Doesn't truly unbind - sets to tombstone value")
  (let ([symbols (history-tracked-definitions session-id)])
    (for-each
      (lambda (sym)
        (guard (e [else #f])
          (when (top-level-bound? sym)
            (set-top-level-value! sym *history-tombstone*))))
      symbols)
    (hashtable-set! *history-tracked-definitions* session-id '())))

(doc 'section 'command-replay)

(doc *default-replay-mode* 'type 'symbol)
(doc *default-replay-mode* 'description "Default replay mode ('safe or 'full)")
(define *default-replay-mode* 'safe)

(define (should-replay? cmd-type mode)
  (doc 'type (-> Symbol Symbol Boolean))
  (doc 'description "Determine if a command type should be replayed in given mode")
  (doc 'note "'safe mode only replays definitions, 'full replays everything")
  (case mode
    [(safe) (eq? cmd-type 'definition)]
    [(full) #t]
    [else (eq? cmd-type 'definition)]))

(define (replay-command cmd-str session-id)
  (doc 'type (-> String Symbol (U Symbol (List Symbol String))))
  (doc 'description "Replay a single command string")
  (doc 'export #t)
  (doc 'returns "'success or '(error \"message\")")
  (guard (e [else
             (list 'error (if (message-condition? e)
                              (condition-message e)
                              "unknown error"))])
    (eval (read (open-input-string cmd-str)))
    'success))

(doc 'section 'replay-to-index)

(define (history-replay-to-index session-id target-index mode)
  (doc 'type (-> String Int Symbol (List Symbol (U Int String))))
  (doc 'description "Replay commands from index 0 to target-index")
  (doc 'export #t)
  (doc 'param 'session-id "Session to replay")
  (doc 'param 'target-index "Replay up to and including this index")
  (doc 'param 'mode "'safe or 'full")
  (doc 'returns "(ok <count>) on success, (error <message> <index>) on divergence")
  (let* ([branch (history-read-current-branch session-id)]
         [entries (history-walk session-id branch)])
    (let* ([sorted-entries
            (list-sort
              (lambda (a b)
                (< (cdr (assq 'index (history-entry-data a)))
                   (cdr (assq 'index (history-entry-data b)))))
              entries)]
           [to-replay
            (filter
              (lambda (entry)
                (let ([idx (cdr (assq 'index (history-entry-data entry)))])
                  (<= idx target-index)))
              sorted-entries)])
      (let loop ([remaining to-replay] [replayed 0])
        (if (null? remaining)
            (list 'ok replayed)
            (let* ([entry (car remaining)]
                   [data (history-entry-data entry)]
                   [cmd (cdr (assq 'command data))]
                   [cmd-type (cdr (assq 'cmd-type data))]
                   [index (cdr (assq 'index data))]
                   [original-result (cdr (assq 'result-type data))]
                   [defined-name (cdr (assq 'defined-name data))])
              (if (should-replay? cmd-type mode)
                  (let ([result (replay-command cmd session-id)])
                    (cond
                      [(eq? result 'success)
                       (when defined-name
                         (history-track-definition! session-id defined-name))
                       (loop (cdr remaining) (+ replayed 1))]
                      [(and (pair? result) (eq? (car result) 'error)
                            (eq? original-result 'success))
                       (list 'error
                             (format "Divergence at index ~a: ~a" index (cadr result))
                             index)]
                      [(and (pair? result) (eq? (car result) 'error)
                            (eq? original-result 'error))
                       (loop (cdr remaining) (+ replayed 1))]
                      [else
                       (loop (cdr remaining) (+ replayed 1))]))
                  (loop (cdr remaining) replayed))))))))

(doc 'section 'full-session-replay)

(define (history-replay-session! session-id mode)
  (doc 'type (-> String Symbol (List Symbol (U Int String))))
  (doc 'description "Reset environment and replay all history")
  (doc 'export #t)
  (history-reset-environment! session-id)
  (let ([current-idx (history-current-index session-id)])
    (if (< current-idx 0)
        (list 'ok 0)
        (history-replay-to-index session-id current-idx mode))))

(doc 'section 'checkpoint-based-replay)

(doc 'todo "Implement checkpoint-based replay for long sessions")
(doc 'note "Current implementation always replays from index 0, acceptable for < 100 commands")

(define (history-replay-from-checkpoint session-id target-index mode)
  (doc 'type (-> String Int Symbol (List Symbol (U Int String))))
  (doc 'description "Placeholder for checkpoint-based replay")
  (doc 'todo "Implement checkpoint finding and replay")
  (history-replay-to-index session-id target-index mode))

(doc 'section 'divergence-recovery)

(doc *history-divergence-handler* 'type 'procedure)
(doc *history-divergence-handler* 'description "Handler for replay divergence (returns 'continue or 'abort)")
(define *history-divergence-handler*
  (lambda (message index)
    (display (format "Replay divergence: ~a\n" message))
    'abort))

(define (history-set-divergence-handler! handler)
  (doc 'type (-> (-> String Int Symbol) Void))
  (doc 'description "Set custom divergence handler")
  (doc 'export #t)
  (set! *history-divergence-handler* handler))

;; Note: list-sort is a Chez Scheme built-in (O(n log n) merge sort)
