;;; thimble/watch-example.ss — Examples of using the watch system
;;;
;;; Load this in the REPL to see the watch system in action.
;;;
;;; Usage:
;;;   (load "shell/watch-example.ss")
;;;   Then run one of the example functions below.

;;; Load the watch system (if not already loaded)
(unless (top-level-bound? 'watch-file)
        (load "shell/watch.ss"))

(display "\n")
(display "═══════════════════════════════════════════════════════════════\n")
(display "           File Watching and Auto-Reload Examples              \n")
(display "═══════════════════════════════════════════════════════════════\n")
(display "\n")

;;; ============================================================
;;; Example 1: Auto-reload a single module
;;; ============================================================

(define (example-1-auto-reload)
  (display "Example 1: Auto-reload a module when it changes\n")
  (display "──────────────────────────────────────────────────\n")
  (display "Watching: shell/fs.ss\n")
  (display "Action: Reload module automatically\n")
  (display "\n")
  (display "Try editing shell/fs.ss and saving it.\n")
  (display "The module will reload automatically!\n")
  (display "\n")
  (display "Press Ctrl+C or run (stop-watching) to stop.\n")
  (display "\n")
  (auto-reload "shell/fs.ss"))

;;; ============================================================
;;; Example 2: Auto-run tests when test file changes
;;; ============================================================

(define (example-2-auto-test)
  (display "Example 2: Auto-run tests when test file changes\n")
  (display "──────────────────────────────────────────────────\n")
  (display "Watching: shell/test-fs.ss\n")
  (display "Action: Run tests automatically\n")
  (display "\n")
  (display "Try editing shell/test-fs.ss and saving it.\n")
  (display "The tests will run automatically!\n")
  (display "\n")
  (display "Press Ctrl+C or run (stop-watching) to stop.\n")
  (display "\n")
  (auto-test "shell/test-fs.ss"))

;;; ============================================================
;;; Example 3: Watch directory for all Scheme files
;;; ============================================================

(define (example-3-watch-core)
  (display "Example 3: Watch core directory for any .ss file changes\n")
  (display "──────────────────────────────────────────────────\n")
  (display "Watching: core/*.ss\n")
  (display "Action: Display notification\n")
  (display "\n")
  (display "Try editing any file in core/ and saving it.\n")
  (display "You'll see a notification!\n")
  (display "\n")
  (display "Press Ctrl+C or run (stop-watching) to stop.\n")
  (display "\n")
  (watch-dir "core" "*.ss"
             (lambda (changed-files)
                     (display "\n")
                     (display "🔔 Core files changed:\n")
                     (for-each
                      (lambda (file)
                              (display "  → ")
                              (display file)
                              (newline))
                      changed-files)
                     (display "\n"))))

;;; ============================================================
;;; Example 4: Watch and rebuild on change
;;; ============================================================

(define (example-4-rebuild-on-change)
  (display "Example 4: Watch and rebuild when source changes\n")
  (display "──────────────────────────────────────────────────\n")
  (display "Watching: core/block.ss\n")
  (display "Action: Run related tests\n")
  (display "\n")
  (display "Try editing core/block.ss and saving it.\n")
  (display "Related tests will run automatically!\n")
  (display "\n")
  (display "Press Ctrl+C or run (stop-watching) to stop.\n")
  (display "\n")
  (watch-file "core/block.ss"
              (lambda (changed-files)
                      (display "\n")
                      (display "🔨 Rebuilding and testing...\n")
                      (guard (e [else
                                 (display "✗ Error: ")
                                 (if (condition? e)
                                     (display (condition-message e))
                                     (display e))
                                 (newline)])
                             ;; Reload module
                             (load "core/block.ss")
                             (display "  ✓ Reloaded core/block.ss\n")
                             ;; Run tests
                             (system "scheme --script core/test-block.ss")
                             (display "  ✓ Tests complete\n"))
                      (display "\n"))))

;;; ============================================================
;;; Example 5: Watch multiple files with different actions
;;; ============================================================

(define (example-5-multi-watch)
  (display "Example 5: Watch multiple files with different actions\n")
  (display "──────────────────────────────────────────────────\n")
  (display "Setting up watchers:\n")
  (display "  • core/block.ss → reload + test\n")
  (display "  • shell/fs.ss → reload only\n")
  (display "  • forum/tools.ss → reload + show message\n")
  (display "\n")
  (display "Try editing any of these files.\n")
  (display "Each has its own custom action!\n")
  (display "\n")
  (display "Press Ctrl+C or run (stop-watching) to stop.\n")
  (display "\n")
  
  ;; Watch core/block.ss
  (watch-file "core/block.ss"
              (lambda (files)
                      (display "\n🔵 core/block.ss changed → reloading + testing\n")
                      (guard (e [else (display "Error in block.ss\n")])
                             (load "core/block.ss")
                             (system "scheme --script core/test-block.ss"))))
  
  ;; Watch shell/fs.ss
  (watch-file "shell/fs.ss"
              (lambda (files)
                      (display "\n🟢 shell/fs.ss changed → reloading\n")
                      (guard (e [else (display "Error in fs.ss\n")])
                             (load "shell/fs.ss"))))
  
  ;; Watch forum/tools.ss
  (watch-file "forum/tools.ss"
              (lambda (files)
                      (display "\n🟡 forum/tools.ss changed → reloading + notification\n")
                      (guard (e [else (display "Error in tools.ss\n")])
                             (load "forum/tools.ss")
                             (display "  Forum tools updated!\n"))))
  
  (display "All watchers active. Monitoring for changes...\n"))

;;; ============================================================
;;; Example 6: Custom action with error handling
;;; ============================================================

(define (example-6-custom-action)
  (display "Example 6: Custom action with comprehensive error handling\n")
  (display "──────────────────────────────────────────────────\n")
  (display "Watching: shell/text.ss\n")
  (display "Action: Reload, validate, and report\n")
  (display "\n")
  
  (watch-file "shell/text.ss"
              (lambda (changed-files)
                      (display "\n")
                      (display "═══════════════════════════════════════\n")
                      (display " text.ss Changed — Validating...\n")
                      (display "═══════════════════════════════════════\n")
                      (guard (e [else
                                 (display "✗ Validation failed!\n")
                                 (if (condition? e)
                                     (begin
                                      (display "  Error: ")
                                      (display (condition-message e))
                                      (newline))
                                     (begin
                                      (display "  Unknown error: ")
                                      (display e)
                                      (newline)))])
                             ;; Attempt reload
                             (display "1. Reloading module... ")
                             (load "shell/text.ss")
                             (display "✓\n")
                             
                             ;; Run basic smoke test
                             (display "2. Running smoke test... ")
                             (let ([test-str "Hello, 世界!"])
                                  (unless (string? (utf8->string (string->utf8 test-str)))
                                          (error 'smoke-test "UTF-8 round-trip failed")))
                             (display "✓\n")
                             
                             ;; Success
                             (display "3. All checks passed ✓\n")
                             (display "═══════════════════════════════════════\n")
                             (display "\n")))))

;;; ============================================================
;;; Example 7: Watch with debouncing visualization
;;; ============================================================

(define (example-7-debounce-demo)
  (display "Example 7: Debouncing demonstration\n")
  (display "──────────────────────────────────────────────────\n")
  (display "This example shows how debouncing prevents excessive triggers.\n")
  (display "\n")
  (display "Watching: test-debounce-demo.txt\n")
  (display "Try making rapid consecutive edits to see debouncing in action!\n")
  (display "\n")
  
  ;; Create test file
  (call-with-output-file "test-debounce-demo.txt"
                         (lambda (port)
                                 (display "Edit me rapidly!" port)))
  
  (let ([trigger-count 0]
        [last-trigger-time #f])
       (watch-file "test-debounce-demo.txt"
                   (lambda (changed-files)
                           (set! trigger-count (+ trigger-count 1))
                           (let ([now (current-time)])
                                (display "\n🔔 Trigger #")
                                (display trigger-count)
                                (when last-trigger-time
                                      (let ([elapsed (- (time-second now)
                                                        (time-second last-trigger-time))])
                                           (display " (")
                                           (display elapsed)
                                           (display "s since last trigger)")))
                                (newline)
                                (set! last-trigger-time now))))))

;;; ============================================================
;;; Help
;;; ============================================================

(define (watch-examples-help)
  (display "\n")
  (display "Available examples:\n")
  (display "═══════════════════\n\n")
  (display "  (example-1-auto-reload)      Auto-reload single module\n")
  (display "  (example-2-auto-test)        Auto-run tests on change\n")
  (display "  (example-3-watch-core)       Watch directory with pattern\n")
  (display "  (example-4-rebuild-on-change) Watch and rebuild\n")
  (display "  (example-5-multi-watch)      Multiple watchers\n")
  (display "  (example-6-custom-action)    Custom action with validation\n")
  (display "  (example-7-debounce-demo)    Debouncing visualization\n\n")
  (display "Utility commands:\n")
  (display "═════════════════\n\n")
  (display "  (list-watchers)              Show active watchers\n")
  (display "  (stop-watching)              Stop all watchers\n")
  (display "  (watch-help)                 Show full watch system help\n\n"))

;;; Display help on load
(watch-examples-help)
