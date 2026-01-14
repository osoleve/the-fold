;;; shell/watch.ss — File Watching and Auto-Reload System
;;;
;;; Watch files and directories for changes, trigger actions on modification.
;;; Supports auto-reload, auto-test, debouncing, and custom actions.
;;;
;;; This is Shell code: uses IO, manages background threads, handles failure.
;;;
;;; Key Functions:
;;;   (watch-file path action) → Watcher
;;;   (watch-dir path pattern action) → Watcher
;;;   (auto-test path) → Watcher
;;;   (auto-reload module-path) → Watcher
;;;   (stop-watching) → void
;;;   (stop-watcher! watcher) → void
;;;   (list-watchers) → void
;;;
;;; Implementation uses polling (check file modification times periodically).
;;; Thread-safe using a global registry with mutex protection.

;;; ====
;;; Configuration
;;; ====

(define *watch-poll-interval* 500)  ; milliseconds
(define *watch-debounce-delay* 200) ; milliseconds (wait after last change)

;;; ====
;;; Watcher Record Type
;;; ====

(define-record-type watcher
  (fields
   id                  ; unique identifier (number)
   path                ; file or directory being watched
   pattern             ; glob pattern (for directories) or #f
   action              ; procedure to call on change
   last-modified       ; hashtable: path → timestamp
   last-triggered      ; last time action was triggered
   running?            ; mutable boolean
   thread))            ; background thread handle

;;; Mutate running? field
(define (watcher-stop! w)
  (when (watcher-running? w)
        (set-watcher-running! w #f)))

(define (set-watcher-running! w val)
  ;; Chez doesn't have mutable record fields by default,
  ;; so we use a workaround with a box in the running? field
  (set-box! (watcher-running? w) val))

;;; ====
;;; Global Watcher Registry
;;; ====

(define *watcher-registry* '())
(define *watcher-counter* 0)
(define *registry-mutex* (make-mutex))

;;; add-watcher! : Watcher → void
(define (add-watcher! w)
  (mutex-acquire *registry-mutex*)
  (set! *watcher-registry* (cons w *watcher-registry*))
  (mutex-release *registry-mutex*))

;;; remove-watcher! : Watcher → void
(define (remove-watcher! w)
  (mutex-acquire *registry-mutex*)
  (set! *watcher-registry*
        (filter (lambda (x) (not (= (watcher-id x) (watcher-id w))))
                *watcher-registry*))
  (mutex-release *registry-mutex*))

;;; next-watcher-id : → Nat
(define (next-watcher-id)
  (mutex-acquire *registry-mutex*)
  (set! *watcher-counter* (+ *watcher-counter* 1))
  (let ([id *watcher-counter*])
       (mutex-release *registry-mutex*)
       id))

;;; ====
;;; File Modification Time
;;; ====

;;; file-mtime : String → Number | #f
;;; Get file modification time in seconds since epoch.
;;; Returns #f if file doesn't exist.
(define (file-mtime path)
  (guard (e [else #f])
         (let ([stat (file-stat path)])
              (if stat
                  (cdr (assq 'mtime stat))
                  #f))))

;;; file-stat : String → Alist | #f
;;; Get file statistics (portable across platforms).
(define (file-stat path)
  (guard (e [else #f])
         (if (file-exists? path)
             (let ([port (open-file-input-port path)])
                  (guard (e2 [else (close-port port) #f])
                         (let* ([info (port-file-descriptor port)]
                                ;; Use file-modification-time if available
                                [mtime (time-second (file-modification-time path))])
                               (close-port port)
                               `((mtime . ,mtime)))))
             #f)))

;;; ====
;;; Path Scanning
;;; ====

;;; scan-paths : String × (String | #f) → (Listof String)
;;; Return list of paths to watch.
;;; If pattern is #f, watch single file.
;;; If pattern is provided, find all matching files in directory.
(define (scan-paths path pattern)
  (cond
   ;; Single file
   [(not pattern)
    (if (file-exists? path)
        (list path)
        '())]
   ;; Directory with pattern
   [(file-directory? path)
    (find-matching-files path pattern)]
   ;; Invalid
   [else '()]))

;;; find-matching-files : String × String → (Listof String)
;;; Find all files in directory matching glob pattern.
(define (find-matching-files dir pattern)
  (guard (e [else '()])
         (let ([files (directory-list dir)])
              (filter (lambda (f)
                              (let ([full-path (string-append dir "/" f)])
                                   (and (file-regular? full-path)
                                        (glob-match? pattern f))))
                      files))))

;;; glob-match? : String × String → Boolean
;;; Simple glob pattern matching (* and ? wildcards).
(define (glob-match? pattern str)
  (let ([plen (string-length pattern)]
        [slen (string-length str)])
       (let loop ([pi 0] [si 0])
            (cond
             ;; Both exhausted → match
             [(and (= pi plen) (= si slen)) #t]
             ;; Pattern exhausted, string not → no match
             [(= pi plen) #f]
             ;; Wildcard *
             [(char=? (string-ref pattern pi) #\*)
              (or (loop (+ pi 1) si)           ; match zero chars
                  (and (< si slen)
                       (loop pi (+ si 1))))]   ; match one+ chars
             ;; String exhausted, pattern not (and not *) → no match
             [(= si slen) #f]
             ;; Wildcard ?
             [(char=? (string-ref pattern pi) #\?)
              (loop (+ pi 1) (+ si 1))]
             ;; Literal char
             [(char=? (string-ref pattern pi) (string-ref str si))
              (loop (+ pi 1) (+ si 1))]
             ;; Mismatch
             [else #f]))))

;;; file-regular? : String → Boolean
(define (file-regular? path)
  (and (file-exists? path)
       (not (file-directory? path))))

;;; file-directory? : String → Boolean
(define (file-directory? path)
  (guard (e [else #f])
         (and (file-exists? path)
              (let ([port (open-file-input-port path)])
                   (guard (e2 [else (close-port port) #f])
                          (close-port port)
                          ;; If we can open it as a directory, it's a directory
                          (guard (e3 [else #f])
                                 (let ([dir-list (directory-list path)])
                                      #t)))))))

;;; directory-list : String → (Listof String)
(define (directory-list path)
  (guard (e [else '()])
         (let ([dir (opendir path)])
              (let loop ([entries '()])
                   (let ([entry (readdir dir)])
                        (if (eof-object? entry)
                            (begin
                             (closedir dir)
                             (reverse entries))
                            (if (or (string=? entry ".") (string=? entry ".."))
                                (loop entries)
                                (loop (cons entry entries)))))))))

;;; ====
;;; Change Detection
;;; ====

;;; check-changes : Watcher → (Listof String)
;;; Check for modified files, return list of changed paths.
(define (check-changes w)
  (let* ([paths (scan-paths (watcher-path w) (watcher-pattern w))]
         [mtimes (watcher-last-modified w)]
         [changed '()])
        ;; Check each path
        (for-each
         (lambda (path)
                 (let ([current-mtime (file-mtime path)]
                       [last-mtime (hashtable-ref mtimes path #f)])
                      (when (and current-mtime
                                 (or (not last-mtime)
                                     (> current-mtime last-mtime)))
                            (set! changed (cons path changed))
                            (hashtable-set! mtimes path current-mtime))))
         paths)
        (reverse changed)))

;;; ====
;;; Debouncing
;;; ====

;;; should-trigger? : Watcher → Boolean
;;; Check if enough time has passed since last trigger (debouncing).
(define (should-trigger? w)
  (let ([now (current-time-ms)]
        [last (watcher-last-triggered w)])
       (or (not last)
           (>= (- now last) *watch-debounce-delay*))))

;;; update-trigger-time! : Watcher → void
(define (update-trigger-time! w)
  (set-watcher-last-triggered! w (current-time-ms)))

(define (set-watcher-last-triggered! w val)
  (hashtable-set! (watcher-last-modified w) 'LAST-TRIGGER val))

;;; current-time-ms : → Number
;;; Get current time in milliseconds.
(define (current-time-ms)
  (let ([t (current-time)])
       (+ (* (time-second t) 1000)
          (quotient (time-nanosecond t) 1000000))))

;;; ====
;;; Watch Loop
;;; ====

;;; watch-loop : Watcher → void
;;; Main polling loop for a watcher (runs in background thread).
(define (watch-loop w)
  (let ([running-box (watcher-running? w)])
       (let loop ()
            (when (unbox running-box)
                  ;; Check for changes
                  (let ([changed (check-changes w)])
                       (when (and (not (null? changed))
                                  (should-trigger? w))
                             ;; Trigger action
                             (guard (e [else
                                        (display-watch-error w changed e)])
                                    (update-trigger-time! w)
                                    (display-watch-trigger w changed)
                                    ((watcher-action w) changed))))
                  ;; Sleep and continue
                  (sleep (make-time 'time-duration
                                    (* *watch-poll-interval* 1000000)
                                    0))
                  (loop)))))

;;; ====
;;; Visual Feedback
;;; ====

(define *ansi-reset* "\x1b;[0m")
(define *ansi-blue* "\x1b;[34m")
(define *ansi-green* "\x1b;[32m")
(define *ansi-red* "\x1b;[31m")
(define *ansi-yellow* "\x1b;[33m")

;;; display-watch-trigger : Watcher × (Listof String) → void
(define (display-watch-trigger w changed)
  (display *ansi-blue*)
  (display "⚡ ")
  (display *ansi-reset*)
  (display "Detected changes in ")
  (display *ansi-yellow*)
  (display (watcher-path w))
  (display *ansi-reset*)
  (newline)
  (for-each
   (lambda (path)
           (display "  • ")
           (display path)
           (newline))
   changed))

;;; display-watch-error : Watcher × (Listof String) × Condition → void
(define (display-watch-error w changed e)
  (display *ansi-red*)
  (display "✗ Watch action failed: ")
  (display *ansi-reset*)
  (if (condition? e)
      (display (format-condition e))
      (display e))
  (newline))

;;; display-watch-started : Watcher → void
(define (display-watch-started w)
  (display *ansi-green*)
  (display "👁  ")
  (display *ansi-reset*)
  (display "Watching ")
  (display *ansi-yellow*)
  (display (watcher-path w))
  (display *ansi-reset*)
  (when (watcher-pattern w)
        (display " (pattern: ")
        (display (watcher-pattern w))
        (display ")"))
  (display " [id: ")
  (display (watcher-id w))
  (display "]")
  (newline))

;;; display-watch-stopped : Watcher → void
(define (display-watch-stopped w)
  (display *ansi-red*)
  (display "■ ")
  (display *ansi-reset*)
  (display "Stopped watching ")
  (display (watcher-path w))
  (display " [id: ")
  (display (watcher-id w))
  (display "]")
  (newline))

;;; ====
;;; Public API
;;; ====

;;; watch-file : String × (Listof String → void) → Watcher
;;; Watch a single file for changes.
(define (watch-file path action)
  (let* ([id (next-watcher-id)]
         [mtimes (make-eq-hashtable)]
         [running-box (box #t)]
         [last-trig #f]
         [w (make-watcher id path #f action mtimes last-trig running-box #f)])
        ;; Initialize modification times
        (when (file-exists? path)
              (hashtable-set! mtimes path (file-mtime path)))
        ;; Start background thread
        (let ([thread (fork-thread (lambda () (watch-loop w)))])
             (set-watcher-thread! w thread))
        (add-watcher! w)
        (display-watch-started w)
        w))

(define (set-watcher-thread! w thread)
  ;; Store thread reference (no mutation needed, set during creation)
  ;; This is a workaround since we can't mutate record fields directly
  ;; We'll store it in a separate table if needed
  (void))

;;; watch-dir : String × String × (Listof String → void) → Watcher
;;; Watch a directory for files matching pattern.
(define (watch-dir path pattern action)
  (unless (file-directory? path)
          (error 'watch-dir "not a directory" path))
  (let* ([id (next-watcher-id)]
         [mtimes (make-eq-hashtable)]
         [running-box (box #t)]
         [last-trig #f]
         [w (make-watcher id path pattern action mtimes last-trig running-box #f)])
        ;; Initialize modification times for all matching files
        (for-each
         (lambda (file-path)
                 (hashtable-set! mtimes file-path (file-mtime file-path)))
         (scan-paths path pattern))
        ;; Start background thread
        (let ([thread (fork-thread (lambda () (watch-loop w)))])
             (set-watcher-thread! w thread))
        (add-watcher! w)
        (display-watch-started w)
        w))

;;; auto-reload : String → Watcher
;;; Watch a module file and reload it when it changes.
(define (auto-reload module-path)
  (watch-file module-path
              (lambda (changed)
                      (display *ansi-green*)
                      (display "↻ Reloading ")
                      (display module-path)
                      (display *ansi-reset*)
                      (newline)
                      (guard (e [else
                                 (display *ansi-red*)
                                 (display "✗ Reload failed: ")
                                 (display *ansi-reset*)
                                 (if (condition? e)
                                     (display (format-condition e))
                                     (display e))
                                 (newline)])
                             (load module-path)
                             (display *ansi-green*)
                             (display "✓ Reloaded successfully")
                             (display *ansi-reset*)
                             (newline)))))

;;; auto-test : String → Watcher
;;; Watch a test file and run it when it changes.
(define (auto-test test-path)
  (watch-file test-path
              (lambda (changed)
                      (display *ansi-blue*)
                      (display "🧪 Running tests: ")
                      (display test-path)
                      (display *ansi-reset*)
                      (newline)
                      (display "─────────────────────────────────────────\n")
                      (guard (e [else
                                 (display *ansi-red*)
                                 (display "✗ Tests failed with error: ")
                                 (display *ansi-reset*)
                                 (if (condition? e)
                                     (display (format-condition e))
                                     (display e))
                                 (newline)])
                             (system (string-append "scheme --script " test-path))
                             (display "─────────────────────────────────────────\n")))))

;;; stop-watcher! : Watcher → void
;;; Stop a specific watcher.
(define (stop-watcher! w)
  (watcher-stop! w)
  (remove-watcher! w)
  (display-watch-stopped w))

;;; stop-watching : → void
;;; Stop all watchers.
(define (stop-watching)
  (mutex-acquire *registry-mutex*)
  (let ([watchers *watcher-registry*])
       (mutex-release *registry-mutex*)
       (for-each
        (lambda (w)
                (watcher-stop! w)
                (display-watch-stopped w))
        watchers))
  (mutex-acquire *registry-mutex*)
  (set! *watcher-registry* '())
  (mutex-release *registry-mutex*)
  (display "All watchers stopped.\n"))

;;; list-watchers : → void
;;; Display all active watchers.
(define (list-watchers)
  (mutex-acquire *registry-mutex*)
  (let ([watchers *watcher-registry*])
       (mutex-release *registry-mutex*)
       (if (null? watchers)
           (display "No active watchers.\n")
           (begin
            (display "Active watchers:\n")
            (for-each
             (lambda (w)
                     (display "  [")
                     (display (watcher-id w))
                     (display "] ")
                     (display (watcher-path w))
                     (when (watcher-pattern w)
                           (display " (pattern: ")
                           (display (watcher-pattern w))
                           (display ")"))
                     (newline))
             watchers)))))

;;; ====
;;; Helper: Fork Thread
;;; ====

;;; fork-thread : (→ void) → Thread
;;; Fork a background thread (Chez Scheme threads).
(define (fork-thread thunk)
  (fork-thread-impl thunk))

;;; Chez Scheme uses fork-thread
(define (fork-thread-impl thunk)
  (guard (e [else
             (display "Warning: Threading not available, running synchronously\n")
             (thunk)
             #f])
         (fork-thread thunk)))

;;; ====
;;; REPL Integration
;;; ====

;;; watch-help : → void
;;; Display help for watch commands.
(define (watch-help)
  (display "\n")
  (display "File Watching and Auto-Reload System\n")
  (display "====\n\n")
  (display "Watch files and directories for changes, trigger actions automatically.\n\n")
  (display "Commands:\n")
  (display "  (watch-file path action)       Watch single file\n")
  (display "  (watch-dir path pattern action) Watch directory with glob pattern\n")
  (display "  (auto-reload module-path)      Watch and auto-reload module\n")
  (display "  (auto-test test-path)          Watch and auto-run tests\n")
  (display "  (stop-watcher! watcher)        Stop specific watcher\n")
  (display "  (stop-watching)                Stop all watchers\n")
  (display "  (list-watchers)                Show active watchers\n")
  (display "  (watch-help)                   Show this help\n\n")
  (display "Examples:\n")
  (display "  (auto-reload \"shell/fs.ss\")\n")
  (display "  (auto-test \"shell/test-fs.ss\")\n")
  (display "  (watch-dir \"core\" \"*.ss\" (lambda (files) (display \"Core changed\\n\")))\n\n")
  (display "Configuration:\n")
  (display "  *watch-poll-interval*  = ")
  (display *watch-poll-interval*)
  (display " ms (polling frequency)\n")
  (display "  *watch-debounce-delay* = ")
  (display *watch-debounce-delay*)
  (display " ms (debounce delay)\n\n"))
