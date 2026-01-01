;;; task-tracker.ss - Task and Chore Tracking System for The Fold
;;;
;;; A lightweight task management system that uses the content-addressed
;;; store to maintain an immutable audit trail of tasks and their status.
;;;
;;; Usage:
;;;   (load "shell/task-tracker.ss")
;;;   (task-new! "Fix format string injection" 'critical 'security)
;;;   (task-list)
;;;   (task-complete! <task-hash>)
;;;   (task-note! <task-hash> "Additional finding...")
;;;

(unless (bound? '*fs*)
        (error 'task-tracker "Must load shell/repl.ss first for *fs* capability"))

;;; Task structure stored as content-addressed blocks
;;; {
;;;   tag: 'task
;;;   payload: (title priority category status created-by created-at)
;;;   refs: [parent-task-hash notes...]
;;; }

(define *task-file* ".store/tasks.index")

;;; Get all task hashes from the index
(define (task-index-read)
  (if (file-exists? *task-file*)
      (call-with-input-file *task-file*
                            (lambda (port)
                                    (let ([content (read port)])
                                         (if (eof-object? content) '() content))))
      '()))

;;; Write task index
(define (task-index-write! tasks)
  (call-with-output-file *task-file*
                         (lambda (port)
                                 (write tasks port)
                                 (newline port))
                         'replace))

;;; Add task hash to index
(define (task-index-add! hash)
  (let ([tasks (task-index-read)])
       (unless (member hash tasks)
               (task-index-write! (cons hash tasks)))))

;;; Create a new task
(define (task-new! title priority category)
  (let* ([timestamp (current-timestamp)]
         [session (if (bound? '*session*) *session* '((name . anonymous) (tier . unknown)))]
         [author (cdr (assq 'name session))]
         [task-data `((title . ,title)
                      (priority . ,priority)
                      (category . ,category)
                      (status . open)
                      (created-by . ,author)
                      (created-at . ,timestamp)
                      (notes . ()))]
         [payload (string->utf8 (format "~s" task-data))]
         [block (make-block 'task payload '())]
         [hash (hash-block block)])
        (fs-store! *fs* hash block)
        (task-index-add! hash)
        (printf "[TASK CREATED] ~a\n" title)
        (printf "  Hash: ~a\n" (hash->string hash))
        (printf "  Priority: ~a | Category: ~a\n" priority category)
        hash))

;;; Mark task as complete
(define (task-complete! task-hash)
  (let* ([block (fs-fetch *fs* task-hash)]
         [task-data (if block
                        (read (open-input-string (utf8->string (block-payload block))))
                        (error 'task-complete! "Task not found" task-hash))]
         [session (if (bound? '*session*) *session* '((name . anonymous)))]
         [author (cdr (assq 'name session))]
         [timestamp (current-timestamp)]
         [updated-data (map (lambda (pair)
                                    (if (eq? (car pair) 'status)
                                        `(status . completed)
                                        pair))
                            task-data)]
         [with-completion `(,@updated-data
                            (completed-by . ,author)
                            (completed-at . ,timestamp))]
         [payload (string->utf8 (format "~s" with-completion))]
         [new-block (make-block 'task payload (list task-hash))]
         [new-hash (hash-block new-block)])
        (fs-store! *fs* new-hash new-block)
        (task-index-add! new-hash)
        (printf "[TASK COMPLETED] ~a\n" (cdr (assq 'title task-data)))
        (printf "  New hash: ~a\n" (hash->string new-hash))
        new-hash))

;;; Add a note to a task
(define (task-note! task-hash note-text)
  (let* ([block (fs-fetch *fs* task-hash)]
         [task-data (if block
                        (read (open-input-string (utf8->string (block-payload block))))
                        (error 'task-note! "Task not found" task-hash))]
         [session (if (bound? '*session*) *session* '((name . anonymous)))]
         [author (cdr (assq 'name session))]
         [timestamp (current-timestamp)]
         [notes (cdr (assq 'notes task-data))]
         [new-note `((text . ,note-text)
                     (author . ,author)
                     (timestamp . ,timestamp))]
         [updated-data (map (lambda (pair)
                                    (if (eq? (car pair) 'notes)
                                        `(notes . ,(cons new-note notes))
                                        pair))
                            task-data)]
         [payload (string->utf8 (format "~s" updated-data))]
         [new-block (make-block 'task payload (list task-hash))]
         [new-hash (hash-block new-block)])
        (fs-store! *fs* new-hash new-block)
        (task-index-add! new-hash)
        (printf "[NOTE ADDED] ~a\n" (cdr (assq 'title task-data)))
        (printf "  New hash: ~a\n" (hash->string new-hash))
        new-hash))

;;; Get the latest version of each task (walk refs backward)
(define (task-latest-versions)
  (let* ([all-tasks (task-index-read)]
         [task-data-list (map (lambda (hash)
                                      (let ([block (fs-fetch *fs* hash)])
                                           (if block
                                               (cons hash (read (open-input-string
                                                                 (utf8->string (block-payload block)))))
                                               #f)))
                              all-tasks)])
        ;; Filter out failed fetches and group by original task
        (filter (lambda (x) x) task-data-list)))

;;; List all tasks
(define (task-list . args)
  (let* ([filter-status (if (null? args) #f (car args))]
         [tasks (task-latest-versions)]
         [filtered (if filter-status
                       (filter (lambda (task)
                                       (eq? (cdr (assq 'status (cdr task))) filter-status))
                               tasks)
                       tasks)])
        (printf "\n=== TASK TRACKER (~a tasks) ===\n\n" (length filtered))
        (for-each
         (lambda (task)
                 (let* ([hash (car task)]
                        [data (cdr task)]
                        [title (cdr (assq 'title data))]
                        [priority (cdr (assq 'priority data))]
                        [category (cdr (assq 'category data))]
                        [status (cdr (assq 'status data))]
                        [created-by (cdr (assq 'created-by data))]
                        [notes (cdr (assq 'notes data))]
                        [priority-symbol (cond
                                          [(eq? priority 'critical) "🔴"]
                                          [(eq? priority 'high) "🟠"]
                                          [(eq? priority 'medium) "🟡"]
                                          [(eq? priority 'low) "🟢"]
                                          [else "⚪"])]
                        [status-symbol (if (eq? status 'completed) "✓" "○")])
                       (printf "[%s] %s %s - %s\n" status-symbol priority-symbol category title)
                       (printf "    Hash: %s\n" (hash->string hash))
                       (printf "    By: %s | Priority: %s\n" created-by priority)
                       (when (pair? notes)
                             (printf "    Notes: %s\n" (length notes)))
                       (newline)))
         filtered)
        (printf "=== END ===\n\n")
        (void)))

;;; List only open tasks
(define (task-open)
  (task-list 'open))

;;; List only completed tasks
(define (task-completed)
  (task-list 'completed))

;;; Export task summary to string
(define (task-export)
  (let* ([tasks (task-latest-versions)]
         [open-tasks (filter (lambda (t)
                                     (eq? (cdr (assq 'status (cdr t))) 'open))
                             tasks)]
         [completed-tasks (filter (lambda (t)
                                          (eq? (cdr (assq 'status (cdr t))) 'completed))
                                  tasks)])
        (format "Task Summary: ~a open, ~a completed (total: ~a)\n\nOpen Tasks:\n~a\n\nCompleted Tasks:\n~a"
                (length open-tasks)
                (length completed-tasks)
                (length tasks)
                (apply string-append
                       (map (lambda (t)
                                    (format "  - [~a] ~a\n"
                                            (cdr (assq 'priority (cdr t)))
                                            (cdr (assq 'title (cdr t)))))
                            open-tasks))
                (apply string-append
                       (map (lambda (t)
                                    (format "  - ~a\n"
                                            (cdr (assq 'title (cdr t)))))
                            completed-tasks)))))

;;; Helper: convert hash bytevector to hex string
(define (hash->string hash)
  (if (bytevector? hash)
      (let ([len (bytevector-length hash)])
           (apply string-append
                  (map (lambda (i)
                               (format "~2,'0x" (bytevector-u8-ref hash i)))
                       (iota (min 8 len)))))
      "invalid-hash"))

;;; Register commands if command system is loaded
(when (bound? 'register-command!)
      (register-command!
       'task-new
       "Create a new task"
       "Create a task with title, priority, and category.\n  Usage: (task-new! \"title\" 'priority 'category)\n  Priority: critical, high, medium, low\n  Category: security, bug, feature, refactor, docs, chore"
       task-new!)
      
      (register-command!
       'task-list
       "List all tasks"
       "Display all tasks or filter by status.\n  Usage: (task-list) or (task-list 'open)"
       task-list)
      
      (register-command!
       'task-complete
       "Mark task complete"
       "Mark a task as completed.\n  Usage: (task-complete! <hash>)"
       task-complete!)
      
      (register-command!
       'task-note
       "Add note to task"
       "Add a note/update to a task.\n  Usage: (task-note! <hash> \"note text\")"
       task-note!)
      
      (register-command!
       'task-open
       "List open tasks"
       "Display all open tasks.\n  Usage: (task-open)"
       task-open)
      
      (register-command!
       'task-export
       "Export task summary"
       "Export a summary of all tasks.\n  Usage: (task-export)"
       task-export))

(display "Task tracker loaded. Use (task-new! \"title\" 'priority 'category) to create tasks.\n")
