;;; thimble/history.ss — REPL History Persistence
;;;
;;; Persistent command history across REPL sessions.
;;; This is Shell code: manages state, persists to disk.
;;;
;;; Dependencies:
;;;   shell/fs.ss
(load "core/base/prelude.ss")

;;; NOTE: string-contains?, string-split provided by core/prelude.ss
;;;
;;; Operations:
;;;   (history-init! path) — Initialize history from file
;;;   (history-add! entry) — Add entry to history
;;;   (history-save!) — Save history to disk
;;;   (history-list [n]) — Show last n entries (default: 10)
;;;   (history-search pattern) — Search history for pattern
;;;   (history-clear!) — Clear history
;;;   (history-replay n) — Re-execute history entry n
;;;
;;; Features:
;;;   - Persistent storage across sessions
;;;   - Duplicate suppression
;;;   - Search and replay
;;;   - Size limits
;;;   - Metadata tracking (timestamp, session)

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *history-max-size* 1000)
(define *history-file* ".fold-history")
(define *suppress-duplicates* #t)

;;; ============================================================
;;; History State
;;; ============================================================

;;; History entry structure:
;;; (entry timestamp session-id command result-summary)

(define-record-type history-entry
  (fields
   id              ; Sequential ID
   timestamp-ns    ; When executed
   session-id      ; Which session
   command         ; The command text
   success?        ; Did it succeed?
   result-summary)) ; First line of result

;;; Global history (most recent first)
(define *history* '())
(define *history-next-id* 1)
(define *history-path* #f)
(define *history-dirty* #f)

;;; ============================================================
;;; Initialization
;;; ============================================================

(define (history-init! path)
  "Initialize history system, loading from file if it exists"
  (set! *history-path* path)
  (set! *history* '())
  (set! *history-next-id* 1)
  (set! *history-dirty* #f)
  
  ;; Try to load existing history
  (guard (e [else
             (display (format "History: starting fresh (~a)\n" path))])
         (when (file-exists? path)
               (let ([entries (call-with-input-file path read)])
                    (when (and (list? entries) (not (null? entries)))
                          (set! *history* entries)
                          (set! *history-next-id*
                                (+ 1 (fold-left max 0
                                                (map history-entry-id entries))))
                          (display (format "History: loaded ~a entries from ~a\n"
                                           (length entries) path))))))
  
  ;; Setup auto-save on exit
  (when (not *history-path*)
        (display "Warning: history path not set, auto-save disabled\n")))

;;; ============================================================
;;; Adding Entries
;;; ============================================================

(define (history-add! command success? result)
  "Add command to history"
  (let* ([timestamp (current-nanoseconds)]
         [session-id (get-current-session-id)]
         [result-summary (summarize-result result)]
         [entry (make-history-entry
                 *history-next-id*
                 timestamp
                 session-id
                 command
                 success?
                 result-summary)])
        
        ;; Check for duplicate suppression
        (let ([should-add?
               (or (not *suppress-duplicates*)
                   (null? *history*)
                   (not (equal? command
                                (history-entry-command (car *history*)))))])
             
             (when should-add?
                   (set! *history* (cons entry *history*))
                   (set! *history-next-id* (+ *history-next-id* 1))
                   (set! *history-dirty* #t)
                   
                   ;; Trim if too large
                   (when (> (length *history*) *history-max-size*)
                         (set! *history* (take *history* *history-max-size*)))
                   
                   ;; Auto-save periodically (every 10 entries)
                   (when (and *history-dirty*
                              (= 0 (modulo (length *history*) 10)))
                         (history-save!))))))

(define (get-current-session-id)
  "Get current session ID or 'unknown"
  (guard (e [else 'unknown])
         (if (top-level-bound? 'read-session)
             (let ([session (read-session)])
                  (if session
                      (cdr (assq 'name session))
                      'unknown))
             'unknown)))

(define (summarize-result result)
  "Get first line of result for summary"
  (guard (e [else ""])
         (let* ([str (format "~a" result)]
                [lines (string-split str #\newline)])
               (if (null? lines)
                   ""
                   (let ([first (car lines)])
                        (if (> (string-length first) 60)
                            (string-append (substring first 0 57) "...")
                            first))))))

(define (current-nanoseconds)
  "Get current monotonic time in nanoseconds"
  (let ([t (current-time 'time-monotonic)])
       (+ (* (time-second t) 1000000000)
          (time-nanosecond t))))

;;; ============================================================
;;; Persistence
;;; ============================================================

(define (history-save!)
  "Save history to disk"
  (guard (e [else
             (display (format "Error saving history: ~a\n"
                              (if (condition? e)
                                  (condition-message e)
                                  e)))])
         (when *history-path*
               (call-with-output-file *history-path*
                                      (lambda (port)
                                              (write (reverse *history*) port))
                                      'replace)
               (set! *history-dirty* #f)
               'saved)))

;;; ============================================================
;;; Viewing History
;;; ============================================================

(define (history-list . args)
  "Show last n history entries (default: 10)"
  (let ([n (if (null? args) 10 (car args))]
        [entries *history*])
       
       (display "\n")
       (display "╔══════════════════════════════════════════════════════════════╗\n")
       (display "║                    COMMAND HISTORY                           ║\n")
       (display "╚══════════════════════════════════════════════════════════════╝\n")
       (display "\n")
       
       (if (null? entries)
           (display "  (no history)\n")
           (let ([to-show (take entries (min n (length entries)))])
                (for-each
                 (lambda (entry)
                         (display (format "  ~4a  ~a  ~a\n"
                                          (history-entry-id entry)
                                          (if (history-entry-success? entry) "✓" "✗")
                                          (truncate-string (history-entry-command entry) 50)))
                         (when (and (not (equal? (history-entry-result-summary entry) ""))
                                    (not (equal? (history-entry-result-summary entry) "#<void>")))
                               (display (format "         → ~a\n"
                                                (truncate-string (history-entry-result-summary entry) 50)))))
                 to-show)))
       
       (display "\n")
       (display (format "Showing ~a of ~a entries. Use (history-list n) to show more.\n"
                        (min n (length entries))
                        (length entries)))
       (display "Use (history-replay n) to re-execute entry n.\n")
       (display "\n")))

(define (truncate-string str max-len)
  "Truncate string to max length"
  (if (> (string-length str) max-len)
      (string-append (substring str 0 (- max-len 3)) "...")
      str))

(define (take lst n)
  "Take first n elements from list"
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;;; ============================================================
;;; Searching History
;;; ============================================================

(define (history-search pattern)
  "Search history for pattern (substring match)"
  (display "\n")
  (display (format "Searching history for: \"~a\"\n\n" pattern))
  
  (let ([matches (filter
                  (lambda (entry)
                          (string-contains? (history-entry-command entry) pattern))
                  *history*)])
       
       (if (null? matches)
           (display "  No matches found.\n")
           (begin
            (display (format "Found ~a matches:\n\n" (length matches)))
            (for-each
             (lambda (entry)
                     (display (format "  ~4a  ~a\n"
                                      (history-entry-id entry)
                                      (history-entry-command entry))))
             matches)))
       
       (display "\n")))

;;; ============================================================
;;; Replaying Commands
;;; ============================================================

(define (history-replay n)
  "Re-execute history entry n"
  (let ([entry (history-find-by-id n)])
       (if (not entry)
           (begin
            (display (format "History entry ~a not found.\n" n))
            #f)
           (begin
            (display (format "Replaying #~a: ~a\n"
                             n
                             (history-entry-command entry)))
            (guard (e [else
                       (display (format "Error: ~a\n"
                                        (if (condition? e)
                                            (condition-message e)
                                            e)))
                       #f])
                   (let ([cmd-expr (read (open-input-string
                                          (history-entry-command entry)))])
                        (eval cmd-expr (interaction-environment))))))))

(define (history-find-by-id id)
  "Find history entry by ID"
  (let loop ([entries *history*])
       (cond
        [(null? entries) #f]
        [(= (history-entry-id (car entries)) id) (car entries)]
        [else (loop (cdr entries))])))

;;; ============================================================
;;; History Management
;;; ============================================================

(define (history-clear!)
  "Clear all history"
  (display "Clear history? This cannot be undone. (yes/no): ")
  (let ([response (read)])
       (if (memq response '(yes y #t))
           (begin
            (set! *history* '())
            (set! *history-next-id* 1)
            (set! *history-dirty* #t)
            (history-save!)
            (display "History cleared.\n"))
           (display "Cancelled.\n"))))

(define (history-stats)
  "Show history statistics"
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                  HISTORY STATISTICS                          ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")
  
  (let* ([total (length *history*)]
         [successful (length (filter history-entry-success? *history*))]
         [failed (- total successful)]
         [sessions (remove-duplicates
                    (map history-entry-session-id *history*))]
         [session-count (length sessions)])
        
        (display (format "  Total entries:     ~a\n" total))
        (display (format "  Successful:        ~a (~,1f%)\n"
                         successful
                         (if (> total 0)
                             (* 100.0 (/ successful total))
                             0.0)))
        (display (format "  Failed:            ~a (~,1f%)\n"
                         failed
                         (if (> total 0)
                             (* 100.0 (/ failed total))
                             0.0)))
        (display (format "  Unique sessions:   ~a\n" session-count))
        (display (format "  Dirty:             ~a\n" *history-dirty*))
        (display (format "  File:              ~a\n" (or *history-path* "(not set)")))
        (display "\n")))

(define (remove-duplicates lst)
  "Remove duplicates from list"
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates
             (filter (lambda (x) (not (equal? x (car lst))))
                     (cdr lst))))))

;;; ============================================================
;;; Auto-save on Exit
;;; ============================================================

(define (history-shutdown!)
  "Save history and cleanup on shutdown"
  (when *history-dirty*
        (display "Saving history...")
        (history-save!)
        (display " done.\n")))

;;; ============================================================
;;; Help
;;; ============================================================

(define (history-help)
  "Display help for history commands"
  (display "
╔══════════════════════════════════════════════════════════════╗
║                    HISTORY COMMANDS                          ║
╚══════════════════════════════════════════════════════════════╝

VIEWING:
  (history-list [n])        Show last n entries (default: 10)
  (history-search pattern)  Search for pattern in history
  (history-stats)           Show statistics

REPLAY:
  (history-replay n)        Re-execute entry n

MANAGEMENT:
  (history-save!)           Force save to disk
  (history-clear!)          Clear all history
  (history-shutdown!)       Save and cleanup

CONFIGURATION:
  *history-max-size*        Max entries to keep (default: 1000)
  *suppress-duplicates*     Don't add duplicate consecutive commands
  *history-file*            File path (default: .fold-history)

INTEGRATION:
To enable automatic history tracking in the REPL, wrap eval calls:
  (history-add! command-text success? result)

EXAMPLE:
  (history-init! \".fold-history\")
  (history-list 20)
  (history-search \"define\")
  (history-replay 42)
  (history-stats)
"))

;;; ============================================================
;;; Initialization
;;; ============================================================

(display "\n")
(display "History module loaded.\n")
(display "Use (history-init! path) to initialize.\n")
(display "Use (history-help) for command reference.\n")
(display "\n")
