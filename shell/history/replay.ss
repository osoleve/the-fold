;;; shell/history/replay.ss — Command Replay Engine
;;;
;;; Replays command history to restore environment state.
;;;
;;; Replay Modes:
;;;   safe - Skip effect commands (I/O), only replay definitions
;;;   full - Replay all commands (may have side effects)
;;;
;;; Environment Reset Strategy:
;;;   Chez Scheme doesn't support true unbinding, so we track defined
;;;   symbols and overwrite them with a tombstone on reset. Replay
;;;   then rebuilds the environment from recorded definitions.
;;;
;;; Divergence Handling:
;;;   If a command that succeeded originally fails on replay, we
;;;   pause and report the divergence. The user can continue or abort.
;;;
;;; This is Shell code: impure (evaluation, environment mutation).

(load "shell/history/store.ss")

;;; ====
;;; Replay State
;;; ====

;;; *history-tracked-definitions* : Hashtable Session-ID -> (List Symbol)
;;; Tracks which symbols have been defined in each session.
(define *history-tracked-definitions*
  (make-hashtable string-hash string=?))

;;; history-track-definition! : String Symbol -> Void
;;; Record that a symbol was defined in a session.
(define (history-track-definition! session-id symbol)
  (let ([current (hashtable-ref *history-tracked-definitions* session-id '())])
    (unless (memq symbol current)
      (hashtable-set! *history-tracked-definitions* session-id
                      (cons symbol current)))))

;;; history-tracked-definitions : String -> (List Symbol)
;;; Get all tracked definitions for a session.
(define (history-tracked-definitions session-id)
  (hashtable-ref *history-tracked-definitions* session-id '()))

;;; ====
;;; Environment Reset
;;; ====

;;; *history-tombstone* : Any
;;; Marker value for "undefined" bindings.
(define *history-tombstone* (list 'history-undefined-tombstone))

;;; history-tombstone? : Any -> Boolean
;;; Check if a value is our tombstone marker.
(define (history-tombstone? v)
  (and (pair? v)
       (eq? (car v) 'history-undefined-tombstone)))

;;; history-reset-environment! : String -> Void
;;; Reset the environment by tombstoning all tracked definitions.
;;;
;;; Note: This doesn't truly unbind - it sets to a tombstone value.
;;; Code that checks (top-level-bound? x) will still see #t, but
;;; the value will be our tombstone marker.
(define (history-reset-environment! session-id)
  (let ([symbols (history-tracked-definitions session-id)])
    (for-each
      (lambda (sym)
        (guard (e [else #f])  ; Ignore errors for special forms etc.
          (when (top-level-bound? sym)
            (set-top-level-value! sym *history-tombstone*))))
      symbols)
    ;; Clear the tracking list
    (hashtable-set! *history-tracked-definitions* session-id '())))

;;; ====
;;; Command Replay
;;; ====

;;; replay-mode : Symbol
;;; 'safe = skip effects, 'full = replay everything
(define *default-replay-mode* 'safe)

;;; should-replay? : Symbol Symbol -> Boolean
;;; Determine if a command type should be replayed in given mode.
(define (should-replay? cmd-type mode)
  (case mode
    [(safe)
     ;; Only replay definitions in safe mode
     (eq? cmd-type 'definition)]
    [(full)
     ;; Replay everything in full mode
     #t]
    [else
     (eq? cmd-type 'definition)]))

;;; replay-command : String Symbol -> (success | (error String))
;;; Replay a single command string.
;;; Returns 'success or '(error "message").
(define (replay-command cmd-str session-id)
  (guard (e [else
             (list 'error (if (message-condition? e)
                              (condition-message e)
                              "unknown error"))])
    (eval (read (open-input-string cmd-str)))
    'success))

;;; ====
;;; Replay to Index
;;; ====

;;; history-replay-to-index : String Int Symbol -> (ok Int) | (error String Int)
;;; Replay commands from index 0 to target-index.
;;;
;;; Returns:
;;;   (ok <replayed-count>) on success
;;;   (error <message> <failed-at-index>) on divergence
;;;
;;; Arguments:
;;;   session-id   - Session to replay
;;;   target-index - Replay up to and including this index
;;;   mode         - 'safe or 'full
(define (history-replay-to-index session-id target-index mode)
  (let* ([branch (history-read-current-branch session-id)]
         [entries (history-walk session-id branch)])
    ;; Sort entries by index (oldest first for replay)
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
      ;; Replay each entry
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
                      ;; Success - track definition if applicable
                      [(eq? result 'success)
                       (when defined-name
                         (history-track-definition! session-id defined-name))
                       (loop (cdr remaining) (+ replayed 1))]
                      ;; Divergence - command failed that succeeded before
                      [(and (pair? result) (eq? (car result) 'error)
                            (eq? original-result 'success))
                       (list 'error
                             (format "Divergence at index ~a: ~a" index (cadr result))
                             index)]
                      ;; Error that also errored originally - continue
                      [(and (pair? result) (eq? (car result) 'error)
                            (eq? original-result 'error))
                       (loop (cdr remaining) (+ replayed 1))]
                      ;; Unknown state - treat as success
                      [else
                       (loop (cdr remaining) (+ replayed 1))]))
                  ;; Skip this command (effect in safe mode)
                  (loop (cdr remaining) replayed))))))))

;;; ====
;;; Full Session Replay
;;; ====

;;; history-replay-session! : String Symbol -> (ok Int) | (error String Int)
;;; Reset environment and replay all history.
(define (history-replay-session! session-id mode)
  (history-reset-environment! session-id)
  (let ([current-idx (history-current-index session-id)])
    (if (< current-idx 0)
        (list 'ok 0)  ; No history to replay
        (history-replay-to-index session-id current-idx mode))))

;;; ====
;;; Checkpoint-Based Replay
;;; ====

;;; Future optimization: store periodic checkpoints (every N commands)
;;; so we don't need to replay from index 0 every time.
;;;
;;; For now, we always replay from the beginning. This is acceptable
;;; for typical REPL sessions (< 100 commands), but could be slow
;;; for very long sessions.

;;; history-replay-from-checkpoint : String Int Symbol -> (ok Int) | (error String Int)
;;; Placeholder for checkpoint-based replay.
;;; Currently just replays from index 0.
(define (history-replay-from-checkpoint session-id target-index mode)
  ;; TODO: Implement checkpoint finding and replay
  (history-replay-to-index session-id target-index mode))

;;; ====
;;; Divergence Recovery
;;; ====

;;; *history-divergence-handler* : (String Int -> Symbol)
;;; Handler for replay divergence. Returns 'continue or 'abort.
;;; Default: abort on any divergence.
(define *history-divergence-handler*
  (lambda (message index)
    (display (format "Replay divergence: ~a\n" message))
    'abort))

;;; history-set-divergence-handler! : (String Int -> Symbol) -> Void
;;; Set custom divergence handler.
(define (history-set-divergence-handler! handler)
  (set! *history-divergence-handler* handler))

;;; ====
;;; Utilities
;;; ====

;;; list-sort : (a a -> Boolean) (List a) -> (List a)
;;; Sort a list using the given comparator.
(define (list-sort less? lst)
  ;; Simple insertion sort for small lists
  (let loop ([remaining lst] [sorted '()])
    (if (null? remaining)
        sorted
        (loop (cdr remaining)
              (insert-sorted less? (car remaining) sorted)))))

(define (insert-sorted less? x sorted)
  (cond
    [(null? sorted) (list x)]
    [(less? x (car sorted)) (cons x sorted)]
    [else (cons (car sorted) (insert-sorted less? x (cdr sorted)))]))
