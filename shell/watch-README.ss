;;; shell/watch-README.ss — File Watching System Documentation
;;;
;;; This file documents the file watching and auto-reload system.
;;; It exists as a .ss file (not .md) to comply with The Fold's
;;; "everything inside is Scheme" principle.
;;;
;;; To read this documentation, load it in the REPL or view the source.

;;; ============================================================
;;; OVERVIEW
;;; ============================================================

;;; The watch system provides file and directory monitoring with
;;; automatic action triggers when changes are detected.
;;;
;;; Key features:
;;;   • Watch single files or directories with glob patterns
;;;   • Auto-reload modules when they change
;;;   • Auto-run tests on save
;;;   • Debouncing to prevent excessive triggers
;;;   • Visual feedback with ANSI color codes
;;;   • Thread-safe multi-watcher support
;;;   • Integration with REPL daemon

;;; ============================================================
;;; ARCHITECTURE
;;; ============================================================

;;; The watch system uses a polling-based approach:
;;;
;;;   1. Each watcher runs in a background thread
;;;   2. Thread polls file modification times at regular intervals
;;;   3. When changes detected, debounce timer starts
;;;   4. After debounce delay, action is triggered
;;;   5. Visual feedback shown in terminal
;;;
;;; Why polling instead of OS events?
;;;   • Portable across all platforms
;;;   • Simpler implementation
;;;   • No external dependencies
;;;   • Predictable behavior
;;;   • Easy to debug

;;; ============================================================
;;; BASIC USAGE
;;; ============================================================

;;; Load the watch system:
(load "shell/watch.ss")

;;; Watch a single file:
(watch-file "core/blocks/block.ss"
            (lambda (changed-files)
                    (display "Block module changed!\n")
                    (load "core/blocks/block.ss")))

;;; Watch a directory with pattern:
(watch-dir "core" "*.ss"
           (lambda (changed-files)
                   (display "Core module(s) changed:\n")
                   (for-each display changed-files)))

;;; Auto-reload convenience function:
(auto-reload "shell/fs.ss")

;;; Auto-test convenience function:
(auto-test "shell/test-fs.ss")

;;; Stop all watchers:
(stop-watching)

;;; ============================================================
;;; API REFERENCE
;;; ============================================================

;;; watch-file : String × (Listof String → void) → Watcher
;;;
;;; Watch a single file for changes.
;;;
;;; Parameters:
;;;   path   - File path to watch
;;;   action - Procedure called with list of changed files
;;;
;;; Returns:
;;;   Watcher object
;;;
;;; Example:
;;;   (watch-file "core/blocks/block.ss"
;;;     (lambda (files)
;;;       (load (car files))))

;;; watch-dir : String × String × (Listof String → void) → Watcher
;;;
;;; Watch a directory for files matching a glob pattern.
;;;
;;; Parameters:
;;;   path    - Directory path to watch
;;;   pattern - Glob pattern (e.g., "*.ss", "test-*.ss")
;;;   action  - Procedure called with list of changed files
;;;
;;; Returns:
;;;   Watcher object
;;;
;;; Example:
;;;   (watch-dir "core" "*.ss"
;;;     (lambda (files)
;;;       (display "Changed: ")
;;;       (display files)
;;;       (newline)))

;;; auto-reload : String → Watcher
;;;
;;; Convenience function: watch file and reload on change.
;;;
;;; Parameters:
;;;   module-path - Path to module to watch and reload
;;;
;;; Returns:
;;;   Watcher object
;;;
;;; Example:
;;;   (auto-reload "shell/fs.ss")
;;;
;;; Visual feedback:
;;;   ⚡ Detected changes in shell/fs.ss
;;;     • shell/fs.ss
;;;   ↻ Reloading shell/fs.ss
;;;   ✓ Reloaded successfully

;;; auto-test : String → Watcher
;;;
;;; Convenience function: watch test file and run on change.
;;;
;;; Parameters:
;;;   test-path - Path to test file to watch and run
;;;
;;; Returns:
;;;   Watcher object
;;;
;;; Example:
;;;   (auto-test "shell/test-fs.ss")
;;;
;;; Visual feedback:
;;;   ⚡ Detected changes in shell/test-fs.ss
;;;     • shell/test-fs.ss
;;;   🧪 Running tests: shell/test-fs.ss
;;;   ─────────────────────────────────────────
;;;   [test output appears here]
;;;   ─────────────────────────────────────────

;;; stop-watcher! : Watcher → void
;;;
;;; Stop a specific watcher.
;;;
;;; Parameters:
;;;   watcher - Watcher object to stop
;;;
;;; Example:
;;;   (define w (auto-reload "shell/fs.ss"))
;;;   (stop-watcher! w)

;;; stop-watching : → void
;;;
;;; Stop all active watchers.
;;;
;;; Example:
;;;   (stop-watching)
;;;
;;; Visual feedback:
;;;   ■ Stopped watching shell/fs.ss [id: 1]
;;;   ■ Stopped watching core/block.ss [id: 2]
;;;   All watchers stopped.

;;; list-watchers : → void
;;;
;;; Display all active watchers.
;;;
;;; Example:
;;;   (list-watchers)
;;;
;;; Output:
;;;   Active watchers:
;;;     [1] shell/fs.ss
;;;     [2] core/block.ss
;;;     [3] core (pattern: *.ss)

;;; ============================================================
;;; CONFIGURATION
;;; ============================================================

;;; *watch-poll-interval* : Nat
;;;
;;; Polling interval in milliseconds (default: 500ms).
;;; How often to check for file changes.
;;;
;;; Lower = more responsive, higher CPU usage
;;; Higher = less responsive, lower CPU usage
;;;
;;; Example:
;;;   (set! *watch-poll-interval* 1000)  ; 1 second

;;; *watch-debounce-delay* : Nat
;;;
;;; Debounce delay in milliseconds (default: 200ms).
;;; Minimum time between consecutive action triggers.
;;;
;;; Prevents excessive triggers when files are rapidly modified
;;; (e.g., during IDE auto-save sequences).
;;;
;;; Example:
;;;   (set! *watch-debounce-delay* 500)  ; 0.5 seconds

;;; ============================================================
;;; GLOB PATTERNS
;;; ============================================================

;;; The watch system supports simple glob patterns:
;;;
;;;   *  - Match zero or more characters
;;;   ?  - Match exactly one character
;;;
;;; Examples:
;;;   "*.ss"        - All Scheme files
;;;   "test-*.ss"   - Test files
;;;   "block?.ss"   - block1.ss, blockA.ss, etc.
;;;   "*-*-*.ss"    - Files with two hyphens
;;;
;;; Note: Patterns are matched against filenames only,
;;; not full paths. Directory watching is non-recursive.

;;; ============================================================
;;; DAEMON INTEGRATION
;;; ============================================================

;;; Development Mode:
;;;
;;; The watch system integrates with the REPL daemon to provide
;;; a seamless development experience.
;;;
;;; Load daemon integration:
(load "shell/watch-daemon-integration.ss")

;;; Enable development mode:
(dev-mode-on)
;;;
;;; This sets up watchers for:
;;;   • core/block.ss
;;;   • core/normalize.ss
;;;   • shell/fs.ss
;;;   • shell/text.ss
;;;   • forum/tools.ss
;;;
;;; All modules will auto-reload on save.

;;; Disable development mode:
(dev-mode-off)

;;; Check status:
(dev-status)

;;; Watch all tests:
(watch-tests)
;;;
;;; Sets up auto-runners for all test files.
;;; Tests run automatically when saved.

;;; Stop test watchers:
(stop-test-watchers)

;;; ============================================================
;;; ADVANCED USAGE
;;; ============================================================

;;; Custom Actions with Error Handling:

(watch-file "core/blocks/block.ss"
            (lambda (changed-files)
                    (guard (e [else
                               (display "Error reloading: ")
                               (if (condition? e)
                                   (display (format-condition e))
                                   (display e))
                               (newline)])
                           (load "core/blocks/block.ss")
                           (display "Reloaded successfully\n")
                           ;; Run smoke test
                           (let ([test-block (make-block 'test #vu8(1 2 3) empty-refs)])
                                (unless (block? test-block)
                                        (error 'smoke-test "block creation failed")))
                           (display "Tests passed\n"))))

;;; Smart Reload (reload module + dependents):

(load "shell/watch-daemon-integration.ss")
(watch-with-smart-reload "core/blocks/block.ss")
;;;
;;; When core/block.ss changes, automatically reloads:
;;;   1. core/block.ss itself
;;;   2. All modules that depend on it
;;;
;;; Dependency graph is maintained in *module-deps*.

;;; Multiple Watchers with Coordination:

(define *reload-in-progress* #f)

(watch-file "core/blocks/block.ss"
            (lambda (files)
                    (unless *reload-in-progress*
                            (set! *reload-in-progress* #t)
                            (guard (e [else
                                       (set! *reload-in-progress* #f)
                                       (raise e)])
                                   (load "core/blocks/block.ss")
                                   (load "core/blocks/normalize.ss")  ; depends on block
                                   (load "shell/fs.ss")         ; depends on block
                                   (set! *reload-in-progress* #f)
                                   (display "Cascade reload complete\n")))))

;;; ============================================================
;;; VISUAL FEEDBACK
;;; ============================================================

;;; The watch system uses ANSI color codes for visual feedback:
;;;
;;;   🔵 Blue   - Change detection
;;;   🟢 Green  - Success (reload, tests passed)
;;;   🔴 Red    - Error (reload failed, tests failed)
;;;   🟡 Yellow - File paths
;;;
;;; Examples:
;;;
;;;   ⚡ Detected changes in shell/fs.ss
;;;     • shell/fs.ss
;;;   ↻ Reloading shell/fs.ss
;;;   ✓ Reloaded successfully
;;;
;;;   ⚡ Detected changes in shell/test-fs.ss
;;;     • shell/test-fs.ss
;;;   🧪 Running tests: shell/test-fs.ss
;;;   ─────────────────────────────────────────
;;;   [test output]
;;;   ─────────────────────────────────────────
;;;
;;;   ✗ Watch action failed: variable block? is not bound

;;; ============================================================
;;; TROUBLESHOOTING
;;; ============================================================

;;; Q: Watcher not detecting changes?
;;; A: Check these:
;;;    1. Is the file path correct? (absolute vs relative)
;;;    2. Is the file actually being modified? (check mtime)
;;;    3. Is the poll interval too long? (decrease *watch-poll-interval*)
;;;    4. Is the watcher still running? (check with list-watchers)

;;; Q: Action triggered multiple times for one change?
;;; A: Increase *watch-debounce-delay*.
;;;    Some editors save files multiple times in quick succession.

;;; Q: High CPU usage?
;;; A: Increase *watch-poll-interval*.
;;;    Watching too many files? Use specific patterns.

;;; Q: Watcher stopped unexpectedly?
;;; A: Check for exceptions in the action procedure.
;;;    Unhandled exceptions will be displayed but won't stop the watcher.

;;; Q: Threading not available?
;;; A: The watch system falls back to synchronous mode if threading
;;;    is not available. Actions will block the REPL during execution.

;;; ============================================================
;;; EXAMPLES
;;; ============================================================

;;; Load comprehensive examples:
(load "shell/watch-example.ss")

;;; Available example functions:
;;;   (example-1-auto-reload)      - Auto-reload single module
;;;   (example-2-auto-test)        - Auto-run tests on change
;;;   (example-3-watch-core)       - Watch directory with pattern
;;;   (example-4-rebuild-on-change)- Watch and rebuild
;;;   (example-5-multi-watch)      - Multiple watchers
;;;   (example-6-custom-action)    - Custom action with validation
;;;   (example-7-debounce-demo)    - Debouncing visualization

;;; ============================================================
;;; IMPLEMENTATION NOTES
;;; ============================================================

;;; Thread Safety:
;;;   • Global watcher registry protected by mutex
;;;   • Each watcher has independent state
;;;   • No shared mutable state between watchers
;;;   • Safe for concurrent watcher creation/removal

;;; Memory Management:
;;;   • Stopped watchers removed from registry
;;;   • File modification time cache per watcher
;;;   • No memory leaks from long-running watchers

;;; Platform Compatibility:
;;;   • Uses portable Scheme APIs only
;;;   • File modification time via file-stat
;;;   • Directory listing via opendir/readdir
;;;   • Threading via fork-thread (Chez Scheme)

;;; Performance:
;;;   • Polling overhead: ~1ms per watched file per poll
;;;   • Recommended maximum: ~100 watched files
;;;   • Debouncing prevents action spam
;;;   • No inotify/kqueue dependency

;;; Limitations:
;;;   • Non-recursive directory watching
;;;   • Simple glob patterns only (no regex)
;;;   • Polling-based (not event-driven)
;;;   • Filesystem timestamp resolution dependent

;;; ============================================================
;;; FUTURE ENHANCEMENTS
;;; ============================================================

;;; Potential improvements (not yet implemented):
;;;
;;;   • Recursive directory watching
;;;   • Full regex pattern support
;;;   • OS event integration (inotify, kqueue, etc.)
;;;   • Conditional actions (only if tests pass)
;;;   • Action chaining and dependencies
;;;   • Configuration file support
;;;   • Watcher persistence across daemon restarts
;;;   • Performance metrics and logging
;;;   • Dynamic dependency graph discovery
;;;   • Integration with build systems

;;; ============================================================
;;; SEE ALSO
;;; ============================================================

;;; Related files:
;;;   shell/watch.ss                    - Main implementation
;;;   shell/test-watch.ss               - Test suite
;;;   shell/watch-example.ss            - Usage examples
;;;   shell/watch-daemon-integration.ss - Daemon integration
;;;   shell/repl-daemon.ss              - REPL daemon
;;;   shell/fs.ss                       - Filesystem operations

;;; ============================================================
;;; LICENSE
;;; ============================================================

;;; This file is part of The Fold.
;;; See covenant/ for license and governance.

;;; End of documentation.
