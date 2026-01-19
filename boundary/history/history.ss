;;; boundary/history/history.ss — User-Facing History API
;;;
;;; Provides REPL commands for undo/redo and branching history.
;;;
;;; Commands:
;;;   (undo)              - Undo last command
;;;   (redo)              - Redo undone command
;;;   (history)           - Show history
;;;   (history n)         - Show last n entries
;;;   (jump n)            - Jump to history point n
;;;   (branch 'name)      - Create branch from current position
;;;   (branches)          - List all branches
;;;   (checkout 'name)    - Switch to branch
;;;   (delete-branch 'n)  - Delete merged branch
;;;   (history-save!)     - Force checkpoint
;;;   (history-export)    - Export as script
;;;
;;; This is Shell code: uses ops.ss for implementation.

(load "boundary/history/ops.ss")

;;; ====
;;; Session Context
;;; ====

;;; *current-session-id* : Parameter String
;;; Current session ID. Set by the REPL worker.
(define *current-session-id*
  (if (top-level-bound? '*current-session-id*)
      *current-session-id*
      (make-parameter "default")))

;;; ====
;;; Recording Hook
;;; ====

;;; *history-enabled* : Boolean
;;; Whether history recording is enabled.
(define *history-enabled* #t)

;;; history-enable! : -> Void
(define (history-enable!)
  (set! *history-enabled* #t)
  (display "History recording enabled.\n"))

;;; history-disable! : -> Void
(define (history-disable!)
  (set! *history-enabled* #f)
  (display "History recording disabled.\n"))

;;; ====
;;; User Commands
;;; ====

;;; undo : -> Void
;;; Undo the last command.
(define (undo)
  (let ([result (history-undo! (*current-session-id*))])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Undone: ~a\n" (cadr result)))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot undo: ~a\n" (cadr result)))]
      [else
       (display "Undo failed.\n")])))

;;; redo : -> Void
;;; Redo the last undone command.
(define (redo)
  (let ([result (history-redo! (*current-session-id*))])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Redone: ~a\n" (cadr result)))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot redo: ~a\n" (cadr result)))]
      [else
       (display "Redo failed.\n")])))

;;; history : [Int] -> Void
;;; Display command history.
;;; Optional argument limits the number of entries shown (default 20).
(define history
  (case-lambda
    [() (history-display 20)]
    [(n) (history-display n)]))

;;; history-display : Int -> Void
;;; Display history with formatting.
(define (history-display limit)
  (let* ([session-id (*current-session-id*)]
         [entries (history-list session-id limit)]
         [current-branch (history-read-current-branch session-id)]
         [current-index (history-current-index session-id)])
    (if (null? entries)
        (display "No history recorded yet.\n")
        (begin
          (display (format "History (branch: ~a, at index ~a):\n"
                           current-branch current-index))
          (display "─────────────────────────────────────────────────\n")
          (for-each
            (lambda (entry)
              (let* ([index (cdr (assq 'index entry))]
                     [cmd (cdr (assq 'command entry))]
                     [cmd-type (cdr (assq 'cmd-type entry))]
                     [result-type (cdr (assq 'result-type entry))]
                     [defined (cdr (assq 'defined-name entry))]
                     [marker (if (= index current-index) "►" " ")]
                     [type-char (case cmd-type
                                  [(definition) "D"]
                                  [(effect) "E"]
                                  [(expression) " "]
                                  [else "?"])]
                     [status-char (if (eq? result-type 'success) " " "✗")]
                     ;; Truncate long commands
                     [cmd-display (if (> (string-length cmd) 50)
                                      (string-append (substring cmd 0 47) "...")
                                      cmd)])
                (display (format "~a ~3d [~a~a] ~a~a\n"
                                 marker
                                 index
                                 type-char
                                 status-char
                                 cmd-display
                                 (if defined (format " → ~a" defined) "")))))
            entries)
          (display "─────────────────────────────────────────────────\n")
          (display "[D]=definition [E]=effect [✗]=error\n")))))

;;; jump : Int -> Void
;;; Jump to a specific history index.
(define (jump target-index)
  (let ([result (history-jump! (*current-session-id*) target-index)])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Jumped to index ~a\n" target-index))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot jump: ~a\n" (cadr result)))]
      [else
       (display "Jump failed.\n")])))

;;; ====
;;; Branch Commands
;;; ====

;;; branch : Symbol -> Void
;;; Create a new branch from the current position.
(define (branch name)
  (let ([result (history-create-branch! (*current-session-id*) name)])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Created and switched to branch '~a'\n" name))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot create branch: ~a\n" (cadr result)))]
      [else
       (display "Branch creation failed.\n")])))

;;; branches : -> Void
;;; List all branches.
(define (branches)
  (let* ([session-id (*current-session-id*)]
         [branch-list (history-list-branches session-id)]
         [current (history-read-current-branch session-id)])
    (if (null? branch-list)
        (display "No branches (history not yet initialized).\n")
        (begin
          (display "Branches:\n")
          (for-each
            (lambda (name)
              (let* ([marker (if (string=? name current) "* " "  ")]
                     [count (history-count session-id name)])
                (display (format "~a~a (~a commands)\n" marker name count))))
            branch-list)))))

;;; checkout : Symbol -> Void
;;; Switch to a different branch.
(define (checkout name)
  (let ([result (history-checkout! (*current-session-id*) name)])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Switched to branch '~a'\n" name))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot checkout: ~a\n" (cadr result)))]
      [else
       (display "Checkout failed.\n")])))

;;; delete-branch : Symbol -> Void
;;; Delete a branch.
(define (delete-branch name)
  (let ([result (history-delete-branch-op! (*current-session-id*) name)])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Deleted branch '~a'\n" name))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot delete: ~a\n" (cadr result)))]
      [else
       (display "Delete failed.\n")])))

;;; ====
;;; Persistence Commands
;;; ====

;;; history-save! : -> Void
;;; Force a checkpoint save.
;;; (Currently a no-op since every command is persisted immediately.)
(define (history-save!)
  (display "History is automatically persisted after each command.\n"))

;;; export-history : -> Void
;;; Export history as a replayable script.
(define (export-history)
  (let ([script (history-export (*current-session-id*))])
    (if (string=? script "")
        (display "No definitions to export.\n")
        (begin
          (display ";;; Exported definitions:\n")
          (display script)))))

;;; ====
;;; Help
;;; ====

;;; history-help : -> Void
;;; Display history command help.
(define (history-help)
  (display "\n")
  (display "  REPL History Commands\n")
  (display "  ─────────────────────────────────────────────────\n")
  (display "\n")
  (display "  UNDO/REDO:\n")
  (display "    (undo)              Undo last command\n")
  (display "    (redo)              Redo undone command\n")
  (display "    (jump n)            Jump to history index n\n")
  (display "\n")
  (display "  VIEWING:\n")
  (display "    (history)           Show last 20 commands\n")
  (display "    (history n)         Show last n commands\n")
  (display "    (export-history)    Export as Scheme script\n")
  (display "\n")
  (display "  BRANCHING:\n")
  (display "    (branch 'name)      Create branch at current point\n")
  (display "    (branches)          List all branches\n")
  (display "    (checkout 'name)    Switch to branch\n")
  (display "    (delete-branch 'n)  Delete a branch\n")
  (display "\n")
  (display "  CONTROL:\n")
  (display "    (history-enable!)   Enable history recording\n")
  (display "    (history-disable!)  Disable history recording\n")
  (display "    (history-help)      Show this help\n")
  (display "\n"))

;;; ====
;;; Initialization Message
;;; ====

(unless (top-level-bound? '*quiet*)
  (display "History module loaded. Use (history-help) for commands.\n"))
