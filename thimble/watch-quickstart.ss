;;; thimble/watch-quickstart.ss — Quick Start Guide for File Watching
;;;
;;; Copy and paste these snippets into your REPL to get started quickly.

;;; ============================================================
;;; STEP 1: Load the Watch System
;;; ============================================================

(load "thimble/watch.ss")

;;; ============================================================
;;; STEP 2: Try a Simple Example
;;; ============================================================

;;; Auto-reload a module when it changes:
(auto-reload "shell/fs.ss")

;;; Now edit shell/fs.ss and save it.
;;; You'll see:
;;;   ⚡ Detected changes in shell/fs.ss
;;;     • shell/fs.ss
;;;   ↻ Reloading shell/fs.ss
;;;   ✓ Reloaded successfully

;;; ============================================================
;;; STEP 3: Auto-Run Tests
;;; ============================================================

;;; Watch a test file and run it on changes:
(auto-test "shell/test-fs.ss")

;;; Now edit shell/test-fs.ss and save it.
;;; Tests will run automatically!

;;; ============================================================
;;; STEP 4: View Active Watchers
;;; ============================================================

(list-watchers)

;;; Output:
;;;   Active watchers:
;;;     [1] shell/fs.ss
;;;     [2] shell/test-fs.ss

;;; ============================================================
;;; STEP 5: Stop Watching
;;; ============================================================

;;; Stop all watchers:
(stop-watching)

;;; Or stop a specific watcher:
(define my-watcher (auto-reload "core/block.ss"))
(stop-watcher! my-watcher)

;;; ============================================================
;;; STEP 6: Development Mode (Advanced)
;;; ============================================================

;;; Load daemon integration:
(load "thimble/watch-daemon-integration.ss")

;;; Enable development mode (auto-reload core modules):
(dev-mode-on)

;;; This watches:
;;;   • core/block.ss
;;;   • core/normalize.ss
;;;   • shell/fs.ss
;;;   • shell/text.ss
;;;   • forum/tools.ss

;;; Check status:
(dev-status)

;;; Disable when done:
(dev-mode-off)

;;; ============================================================
;;; STEP 7: Custom Actions
;;; ============================================================

;;; Watch a file with custom action:
(watch-file "core/block.ss"
            (lambda (changed-files)
                    (display "Block module changed! Running tests...\n")
                    (load "fabric/stitches/block.ss")
                    (system "scheme --script core/test-block.ss")))

;;; Watch directory with pattern:
(watch-dir "core" "test-*.ss"
           (lambda (changed-files)
                   (display "Test file(s) changed:\n")
                   (for-each
                    (lambda (f)
                            (display "  • ")
                            (display f)
                            (newline))
                    changed-files)
                   (display "Running test suite...\n")
                   (system "scheme --script test-all.ss")))

;;; ============================================================
;;; CONFIGURATION
;;; ============================================================

;;; Adjust polling frequency (default: 500ms):
(set! *watch-poll-interval* 1000)  ; Check every 1 second

;;; Adjust debounce delay (default: 200ms):
(set! *watch-debounce-delay* 500)  ; Wait 500ms after last change

;;; ============================================================
;;; COMMON PATTERNS
;;; ============================================================

;;; Pattern 1: Watch and reload with error handling
(watch-file "shell/custom.ss"
            (lambda (files)
                    (guard (e [else
                               (display "✗ Reload failed: ")
                               (if (condition? e)
                                   (display (format-condition e))
                                   (display e))
                               (newline)])
                           (load "thimble/custom.ss")
                           (display "✓ Reloaded successfully\n"))))

;;; Pattern 2: Watch multiple related files
(for-each auto-reload
          '("core/block.ss"
            "core/normalize.ss"
            "core/expand.ss"))

;;; Pattern 3: Watch and run specific tests
(watch-file "core/block.ss"
            (lambda (files)
                    (load "fabric/stitches/block.ss")
                    (system "scheme --script core/test-block.ss")))

;;; Pattern 4: Conditional reload (only if no syntax errors)
(watch-file "shell/experimental.ss"
            (lambda (files)
                    ;; First, try to compile without loading
                    (guard (e [else
                               (display "✗ Syntax error, not loading\n")
                               (if (condition? e)
                                   (display (format-condition e))
                                   (display e))
                               (newline)])
                           ;; If compilation succeeds, load it
                           (load "thimble/experimental.ss")
                           (display "✓ Loaded successfully\n"))))

;;; ============================================================
;;; HELP
;;; ============================================================

;;; Show full help:
(watch-help)

;;; Show daemon integration help:
(load "thimble/watch-daemon-integration.ss")
(dev-help)

;;; Load examples:
(load "thimble/watch-example.ss")
(watch-examples-help)

;;; ============================================================
;;; TYPICAL WORKFLOW
;;; ============================================================

;;; 1. Start REPL daemon (if not already running):
;;;    ./DAEMON.cmd start

;;; 2. Login to REPL:
;;;    (hi 'opus 'yourname "Starting work")

;;; 3. Enable development mode:
;;;    (load "thimble/watch-daemon-integration.ss")
;;;    (dev-mode-on)

;;; 4. Work on code:
;;;    Edit files in your editor, save, and they auto-reload!

;;; 5. Enable test watching (optional):
;;;    (watch-tests)

;;; 6. When done:
;;;    (dev-mode-off)
;;;    (stop-test-watchers)

;;; ============================================================
;;; TIPS
;;; ============================================================

;;; Tip 1: Use dev-mode for general development
;;;   (dev-mode-on) sets up sensible defaults for most work.

;;; Tip 2: Increase debounce if your editor saves multiple times
;;;   Some editors (VS Code, etc.) write files in bursts.
;;;   (set! *watch-debounce-delay* 1000)

;;; Tip 3: Use patterns to watch related files
;;;   (watch-dir "core" "test-*.ss" ...) watches all test files

;;; Tip 4: Stop watchers when not needed
;;;   Watchers consume CPU even when idle. Use (stop-watching)
;;;   when stepping away.

;;; Tip 5: Check watcher status regularly
;;;   (list-watchers) shows what's currently being watched.
;;;   Useful if you forget what you started.

;;; Tip 6: Use auto-test for TDD workflow
;;;   Edit test, save, it runs. Edit code, save test, it runs.
;;;   Fast feedback loop!

;;; Tip 7: Guard actions with error handling
;;;   Syntax errors shouldn't crash watchers. Wrap in guard.

;;; Tip 8: Watch tests for modules you're working on
;;;   (watch-file "core/block.ss"
;;;     (lambda (f)
;;;       (load "fabric/stitches/block.ss")
;;;       (system "scheme --script core/test-block.ss")))

;;; ============================================================
;;; TROUBLESHOOTING
;;; ============================================================

;;; Problem: "Watcher not detecting changes"
;;; Solution:
;;;   1. Check file path is correct: (file-exists? "path/to/file")
;;;   2. Check watcher is running: (list-watchers)
;;;   3. Try lower poll interval: (set! *watch-poll-interval* 100)

;;; Problem: "Action runs multiple times for one save"
;;; Solution:
;;;   Increase debounce: (set! *watch-debounce-delay* 1000)

;;; Problem: "High CPU usage"
;;; Solution:
;;;   1. Increase poll interval: (set! *watch-poll-interval* 2000)
;;;   2. Stop unused watchers: (stop-watching)
;;;   3. Use specific patterns, not broad ones

;;; Problem: "Action fails with error"
;;; Solution:
;;;   Wrap in guard:
;;;   (watch-file path
;;;     (lambda (f)
;;;       (guard (e [else (display "Error!\n")])
;;;         (load path))))

;;; ============================================================
;;; DONE!
;;; ============================================================

(display "\n")
(display "Quick start guide loaded!\n")
(display "Try: (auto-reload \"shell/fs.ss\")\n")
(display "\n")
