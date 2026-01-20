(load "boundary/history/ops.ss")

(doc 'module 'boundary/history/history)
(doc 'description "User-facing REPL history API with undo/redo and branching")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/history/ops))

(doc 'section 'session-context)

(doc *current-session-id* 'type 'parameter)
(doc *current-session-id* 'description "Current session ID parameter (set by REPL worker)")
(define *current-session-id*
  (if (top-level-bound? '*current-session-id*)
      *current-session-id*
      (make-parameter "default")))

(doc 'section 'recording-hooks)

(doc *history-enabled* 'type 'boolean)
(doc *history-enabled* 'description "Whether history recording is enabled")
(define *history-enabled* #t)

(define (history-enable!)
  (doc 'type (-> Void))
  (doc 'description "Enable history recording")
  (doc 'export #t)
  (set! *history-enabled* #t)
  (display "History recording enabled.\n"))

(define (history-disable!)
  (doc 'type (-> Void))
  (doc 'description "Disable history recording")
  (doc 'export #t)
  (set! *history-enabled* #f)
  (display "History recording disabled.\n"))

(doc 'section 'user-commands)

(define (undo)
  (doc 'type (-> Void))
  (doc 'description "Undo the last command")
  (doc 'export #t)
  (let ([result (history-undo! (*current-session-id*))])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Undone: ~a\n" (cadr result)))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot undo: ~a\n" (cadr result)))]
      [else
       (display "Undo failed.\n")])))

(define (redo)
  (doc 'type (-> Void))
  (doc 'description "Redo the last undone command")
  (doc 'export #t)
  (let ([result (history-redo! (*current-session-id*))])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Redone: ~a\n" (cadr result)))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot redo: ~a\n" (cadr result)))]
      [else
       (display "Redo failed.\n")])))

(doc history 'type (-> (Option Int) Void))
(doc history 'description "Display command history (optional limit, default 20)")
(doc history 'export #t)
(define history
  (case-lambda
    [() (history-display 20)]
    [(n) (history-display n)]))

(define (history-display limit)
  (doc 'type (-> Int Void))
  (doc 'description "Display history with formatting")
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

(define (jump target-index)
  (doc 'type (-> Int Void))
  (doc 'description "Jump to a specific history index")
  (doc 'export #t)
  (let ([result (history-jump! (*current-session-id*) target-index)])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Jumped to index ~a\n" target-index))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot jump: ~a\n" (cadr result)))]
      [else
       (display "Jump failed.\n")])))

(doc 'section 'branch-commands)

(define (branch name)
  (doc 'type (-> Symbol Void))
  (doc 'description "Create a new branch from the current position")
  (doc 'export #t)
  (let ([result (history-create-branch! (*current-session-id*) name)])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Created and switched to branch '~a'\n" name))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot create branch: ~a\n" (cadr result)))]
      [else
       (display "Branch creation failed.\n")])))

(define (branches)
  (doc 'type (-> Void))
  (doc 'description "List all branches")
  (doc 'export #t)
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

(define (checkout name)
  (doc 'type (-> Symbol Void))
  (doc 'description "Switch to a different branch")
  (doc 'export #t)
  (let ([result (history-checkout! (*current-session-id*) name)])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Switched to branch '~a'\n" name))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot checkout: ~a\n" (cadr result)))]
      [else
       (display "Checkout failed.\n")])))

(define (delete-branch name)
  (doc 'type (-> Symbol Void))
  (doc 'description "Delete a branch")
  (doc 'export #t)
  (let ([result (history-delete-branch-op! (*current-session-id*) name)])
    (cond
      [(and (pair? result) (eq? (car result) 'ok))
       (display (format "Deleted branch '~a'\n" name))]
      [(and (pair? result) (eq? (car result) 'error))
       (display (format "Cannot delete: ~a\n" (cadr result)))]
      [else
       (display "Delete failed.\n")])))

(doc 'section 'persistence-commands)

(define (history-save!)
  (doc 'type (-> Void))
  (doc 'description "Force checkpoint save (no-op since auto-persisted)")
  (doc 'export #t)
  (display "History is automatically persisted after each command.\n"))

(define (export-history)
  (doc 'type (-> Void))
  (doc 'description "Export history as a replayable script")
  (doc 'export #t)
  (let ([script (history-export (*current-session-id*))])
    (if (string=? script "")
        (display "No definitions to export.\n")
        (begin
          (display ";;; Exported definitions:\n")
          (display script)))))

(doc 'section 'help)

(define (history-help)
  (doc 'type (-> Void))
  (doc 'description "Display history command help")
  (doc 'export #t)
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

(doc 'section 'initialization)

(unless (top-level-bound? '*quiet*)
  (display "History module loaded. Use (history-help) for commands.\n"))
