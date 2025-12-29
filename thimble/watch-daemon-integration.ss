;;; thimble/watch-daemon-integration.ss — Watch system integration with REPL daemon
;;;
;;; Shows how to use the watch system within the daemon for development workflows.
;;; This enables live reloading during development without manual intervention.

;;; Load dependencies
(unless (top-level-bound? 'watch-file)
        (load "thimble/watch.ss"))

;;; ============================================================
;;; Development Mode Toggle
;;; ============================================================

(define *dev-mode* #f)
(define *dev-watchers* '())

;;; dev-mode-on : → void
;;; Enable development mode with auto-reload watchers.
(define (dev-mode-on)
  (when *dev-mode*
        (display "Development mode already active.\n")
        (list-watchers)
        (void))
  (unless *dev-mode*
          (display "\n")
          (display "╔════════════════════════════════════════════════╗\n")
          (display "║     DEVELOPMENT MODE ACTIVATED                 ║\n")
          (display "╚════════════════════════════════════════════════╝\n")
          (display "\n")
          (display "Setting up auto-reload watchers...\n\n")
          
          ;; Watch core modules
          (display "Core modules:\n")
          (set! *dev-watchers*
                (cons (watch-file "core/block.ss"
                                  (lambda (f)
                                          (display "↻ Reloading core/block.ss\n")
                                          (guard (e [else (display "✗ Failed to reload\n")])
                                                 (load "fabric/stitches/block.ss")
                                                 (display "✓ Reloaded\n"))))
                      *dev-watchers*))
          (display "  ✓ Watching core/block.ss\n")
          
          (set! *dev-watchers*
                (cons (watch-file "core/normalize.ss"
                                  (lambda (f)
                                          (display "↻ Reloading core/normalize.ss\n")
                                          (guard (e [else (display "✗ Failed to reload\n")])
                                                 (load "fabric/stitches/normalize.ss")
                                                 (display "✓ Reloaded\n"))))
                      *dev-watchers*))
          (display "  ✓ Watching core/normalize.ss\n")
          
          ;; Watch shell modules
          (display "\nShell modules:\n")
          (set! *dev-watchers*
                (cons (watch-file "shell/fs.ss"
                                  (lambda (f)
                                          (display "↻ Reloading shell/fs.ss\n")
                                          (guard (e [else (display "✗ Failed to reload\n")])
                                                 (load "thimble/fs.ss")
                                                 (display "✓ Reloaded\n"))))
                      *dev-watchers*))
          (display "  ✓ Watching shell/fs.ss\n")
          
          (set! *dev-watchers*
                (cons (watch-file "shell/text.ss"
                                  (lambda (f)
                                          (display "↻ Reloading shell/text.ss\n")
                                          (guard (e [else (display "✗ Failed to reload\n")])
                                                 (load "thimble/text.ss")
                                                 (display "✓ Reloaded\n"))))
                      *dev-watchers*))
          (display "  ✓ Watching shell/text.ss\n")
          
          ;; Watch forum modules
          (display "\nForum modules:\n")
          (set! *dev-watchers*
                (cons (watch-file "forum/tools.ss"
                                  (lambda (f)
                                          (display "↻ Reloading forum/tools.ss\n")
                                          (guard (e [else (display "✗ Failed to reload\n")])
                                                 (load "forum/tools.ss")
                                                 (display "✓ Reloaded\n"))))
                      *dev-watchers*))
          (display "  ✓ Watching forum/tools.ss\n")
          
          (set! *dev-mode* #t)
          (display "\n")
          (display "Development mode active. Files will auto-reload on change.\n")
          (display "Use (dev-mode-off) to disable.\n")
          (display "\n")))

;;; dev-mode-off : → void
;;; Disable development mode and stop all watchers.
(define (dev-mode-off)
  (unless *dev-mode*
          (display "Development mode not active.\n")
          (void))
  (when *dev-mode*
        (display "\n")
        (display "Stopping development mode...\n")
        (for-each stop-watcher! *dev-watchers*)
        (set! *dev-watchers* '())
        (set! *dev-mode* #f)
        (display "✓ Development mode deactivated.\n")
        (display "\n")))

;;; dev-status : → void
;;; Show current development mode status.
(define (dev-status)
  (display "\n")
  (display "Development Mode Status\n")
  (display "═══════════════════════\n\n")
  (if *dev-mode*
      (begin
       (display "Status: ")
       (display "\x1b;[32mACTIVE\x1b;[0m")
       (display "\n")
       (display "Watchers: ")
       (display (length *dev-watchers*))
       (display "\n\n")
       (list-watchers))
      (begin
       (display "Status: ")
       (display "\x1b;[31mINACTIVE\x1b;[0m")
       (display "\n")
       (display "\nUse (dev-mode-on) to activate.\n")))
  (display "\n"))

;;; ============================================================
;;; Test Auto-Runner
;;; ============================================================

(define *test-watchers* '())

;;; watch-tests : → void
;;; Watch all test files and auto-run on change.
(define (watch-tests)
  (display "\n")
  (display "Setting up test auto-runners...\n\n")
  
  ;; Core tests
  (for-each
   (lambda (test-file)
           (when (file-exists? test-file)
                 (let ([w (auto-test test-file)])
                      (set! *test-watchers* (cons w *test-watchers*))
                      (display "  ✓ Watching ")
                      (display test-file)
                      (newline))))
   '("core/test-block.ss"
     "core/test-normalize.ss"
     "core/test-sha256.ss"))
  
  ;; Shell tests
  (for-each
   (lambda (test-file)
           (when (file-exists? test-file)
                 (let ([w (auto-test test-file)])
                      (set! *test-watchers* (cons w *test-watchers*))
                      (display "  ✓ Watching ")
                      (display test-file)
                      (newline))))
   '("shell/test-fs.ss"
     "shell/test-text.ss"
     "shell/test-validate.ss"))
  
  (display "\n")
  (display (length *test-watchers*))
  (display " test files being watched.\n")
  (display "Tests will run automatically on save.\n")
  (display "Use (stop-test-watchers) to disable.\n")
  (display "\n"))

;;; stop-test-watchers : → void
(define (stop-test-watchers)
  (display "Stopping test watchers...\n")
  (for-each stop-watcher! *test-watchers*)
  (set! *test-watchers* '())
  (display "✓ Test watchers stopped.\n"))

;;; ============================================================
;;; Smart Reload (reload module + dependencies)
;;; ============================================================

;;; Module dependency graph (manually maintained for now)
(define *module-deps*
  '(("core/block.ss" . ())
    ("core/sha256.ss" . ())
    ("core/normalize.ss" . ("core/block.ss"))
    ("core/expand.ss" . ("core/block.ss" "core/normalize.ss"))
    ("shell/fs.ss" . ("core/block.ss" "core/sha256.ss"))
    ("forum/tools.ss" . ("core/block.ss" "shell/fs.ss"))))

;;; smart-reload : String → void
;;; Reload a module and all modules that depend on it.
(define (smart-reload module-path)
  (display "\n")
  (display "Smart reload: ")
  (display module-path)
  (display "\n")
  (display "─────────────────────────────────────\n")
  
  ;; First, reload the module itself
  (display "1. Reloading ")
  (display module-path)
  (display "... ")
  (guard (e [else
             (display "✗\n")
             (display "   Error: ")
             (if (condition? e)
                 (display (condition-message e))
                 (display e))
             (newline)])
         (load module-path)
         (display "✓\n"))
  
  ;; Find and reload dependents
  (let ([dependents (find-dependents module-path)])
       (unless (null? dependents)
               (display "2. Reloading dependents:\n")
               (for-each
                (lambda (dep)
                        (display "   • ")
                        (display dep)
                        (display "... ")
                        (guard (e [else
                                   (display "✗\n")
                                   (display "     Error: ")
                                   (if (condition? e)
                                       (display (condition-message e))
                                       (display e))
                                   (newline)])
                               (load dep)
                               (display "✓\n")))
                dependents)))
  
  (display "─────────────────────────────────────\n")
  (display "Reload complete.\n\n"))

;;; find-dependents : String → (Listof String)
;;; Find all modules that depend on the given module.
(define (find-dependents module-path)
  (filter
   (lambda (entry)
           (member module-path (cdr entry)))
   *module-deps*))

;;; watch-with-smart-reload : String → Watcher
;;; Watch a module and use smart reload when it changes.
(define (watch-with-smart-reload module-path)
  (watch-file module-path
              (lambda (changed)
                      (smart-reload module-path))))

;;; ============================================================
;;; Daemon Startup Hook
;;; ============================================================

;;; auto-dev-mode : → void
;;; Automatically enable dev mode when daemon starts.
;;; Call this from the daemon startup if desired.
(define (auto-dev-mode)
  (display "\n")
  (display "Tip: Development mode is available!\n")
  (display "     Use (dev-mode-on) to enable auto-reload.\n")
  (display "     Use (watch-tests) to enable auto-testing.\n")
  (display "\n"))

;;; ============================================================
;;; Help
;;; ============================================================

(define (dev-help)
  (display "\n")
  (display "Development Mode Commands\n")
  (display "═════════════════════════\n\n")
  (display "Enable/disable development mode:\n")
  (display "  (dev-mode-on)            Enable auto-reload for core modules\n")
  (display "  (dev-mode-off)           Disable development mode\n")
  (display "  (dev-status)             Show current status\n\n")
  (display "Test automation:\n")
  (display "  (watch-tests)            Watch all test files, auto-run on change\n")
  (display "  (stop-test-watchers)     Stop test watchers\n\n")
  (display "Smart reload:\n")
  (display "  (smart-reload path)      Reload module + dependents\n")
  (display "  (watch-with-smart-reload path) Watch with smart reload\n\n")
  (display "General watch commands:\n")
  (display "  (watch-file path action) Watch single file\n")
  (display "  (watch-dir path pattern action) Watch directory\n")
  (display "  (auto-reload path)       Simple auto-reload\n")
  (display "  (auto-test path)         Auto-run tests\n")
  (display "  (list-watchers)          Show all watchers\n")
  (display "  (stop-watching)          Stop all watchers\n\n")
  (display "Examples:\n")
  (display "  (dev-mode-on)                      ; Enable full dev mode\n")
  (display "  (watch-tests)                      ; Auto-run tests\n")
  (display "  (auto-reload \"shell/custom.ss\")    ; Watch single file\n")
  (display "  (watch-with-smart-reload \"core/block.ss\") ; Smart reload\n\n"))

;;; Export help by default
(display "\n")
(display "Daemon integration loaded.\n")
(display "Use (dev-help) for development commands.\n")
(display "\n")
