;;; shell/test-watch-integration.ss — Integration test for watch system
;;;
;;; This test verifies the complete watch system works end-to-end.
;;; Run this to validate the implementation.

(display "Watch System Integration Test\n")
(display "====\n\n")

;;; ====
;;; Test 1: Load watch system
;;; ====

(display "Test 1: Loading watch system... ")
(guard (e [else
           (display "✗\n")
           (display "Error: ")
           (if (condition? e)
               (display (condition-message e))
               (display e))
           (newline)
           (exit 1)])
       (load "shell/watch.ss")
       (display "✓\n"))

;;; ====
;;; Test 2: Verify exports
;;; ====

(display "Test 2: Verifying exports... ")
(let ([required-bindings
       '(watch-file
         watch-dir
         auto-reload
         auto-test
         stop-watcher!
         stop-watching
         list-watchers
         watch-help
         *watch-poll-interval*
         *watch-debounce-delay*)])
     (for-each
      (lambda (binding)
              (unless (top-level-bound? binding)
                      (display "✗\n")
                      (display "Missing binding: ")
                      (display binding)
                      (newline)
                      (exit 1)))
      required-bindings)
     (display "✓\n"))

;;; ====
;;; Test 3: Configuration values
;;; ====

(display "Test 3: Checking configuration... ")
(unless (and (number? *watch-poll-interval*)
             (> *watch-poll-interval* 0))
        (display "✗\n")
        (display "Invalid *watch-poll-interval*\n")
        (exit 1))
(unless (and (number? *watch-debounce-delay*)
             (>= *watch-debounce-delay* 0))
        (display "✗\n")
        (display "Invalid *watch-debounce-delay*\n")
        (exit 1))
(display "✓\n")

;;; ====
;;; Test 4: Create and stop a watcher
;;; ====

(display "Test 4: Creating and stopping watcher... ")
(guard (e [else
           (display "✗\n")
           (display "Error: ")
           (if (condition? e)
               (display (condition-message e))
               (display e))
           (newline)
           (exit 1)])
       ;; Create test file
       (call-with-output-file "test-integration-tmp.txt"
                              (lambda (port)
                                      (display "test content" port)))
       
       ;; Create watcher
       (let ([w (watch-file "test-integration-tmp.txt"
                            (lambda (files) (void)))])
            (unless (watcher? w)
                    (display "✗\n")
                    (display "watch-file did not return watcher\n")
                    (exit 1))
            
            ;; Stop watcher
            (stop-watcher! w))
       
       ;; Cleanup
       (when (file-exists? "test-integration-tmp.txt")
             (delete-file "test-integration-tmp.txt"))
       
       (display "✓\n"))

;;; ====
;;; Test 5: Load daemon integration
;;; ====

(display "Test 5: Loading daemon integration... ")
(guard (e [else
           (display "✗\n")
           (display "Error: ")
           (if (condition? e)
               (display (condition-message e))
               (display e))
           (newline)
           (exit 1)])
       (load "shell/watch-daemon-integration.ss")
       (display "✓\n"))

;;; ====
;;; Test 6: Verify daemon integration exports
;;; ====

(display "Test 6: Verifying daemon integration... ")
(let ([required-bindings
       '(dev-mode-on
         dev-mode-off
         dev-status
         watch-tests
         stop-test-watchers
         smart-reload
         watch-with-smart-reload
         dev-help)])
     (for-each
      (lambda (binding)
              (unless (top-level-bound? binding)
                      (display "✗\n")
                      (display "Missing binding: ")
                      (display binding)
                      (newline)
                      (exit 1)))
      required-bindings)
     (display "✓\n"))

;;; ====
;;; Test 7: Load examples
;;; ====

(display "Test 7: Loading examples... ")
(guard (e [else
           (display "✗\n")
           (display "Error: ")
           (if (condition? e)
               (display (condition-message e))
               (display e))
           (newline)
           (exit 1)])
       (load "shell/watch-example.ss")
       (display "✓\n"))

;;; ====
;;; Test 8: Verify example exports
;;; ====

(display "Test 8: Verifying examples... ")
(let ([required-bindings
       '(example-1-auto-reload
         example-2-auto-test
         example-3-watch-core
         example-4-rebuild-on-change
         example-5-multi-watch
         example-6-custom-action
         example-7-debounce-demo
         watch-examples-help)])
     (for-each
      (lambda (binding)
              (unless (top-level-bound? binding)
                      (display "✗\n")
                      (display "Missing binding: ")
                      (display binding)
                      (newline)
                      (exit 1)))
      required-bindings)
     (display "✓\n"))

;;; ====
;;; Test 9: Verify glob matching
;;; ====

(display "Test 9: Testing glob matching... ")
(unless (and (glob-match? "*.ss" "test.ss")
             (not (glob-match? "*.ss" "test.txt"))
             (glob-match? "test-*.ss" "test-watch.ss")
             (glob-match? "???.ss" "foo.ss")
             (not (glob-match? "???.ss" "foobar.ss")))
        (display "✗\n")
        (display "Glob matching failed\n")
        (exit 1))
(display "✓\n")

;;; ====
;;; Test 10: Cleanup
;;; ====

(display "Test 10: Cleanup... ")
(guard (e [else
           (display "✗\n")
           (display "Warning: Cleanup failed\n")])
       (stop-watching)
       (display "✓\n"))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "═══════════════════════════════════════\n")
(display "  All Integration Tests Passed ✓\n")
(display "═══════════════════════════════════════\n")
(display "\n")
(display "Watch system is ready to use!\n")
(display "\n")
(display "Quick start:\n")
(display "  (load \"shell/watch.ss\")\n")
(display "  (auto-reload \"your-module.ss\")\n")
(display "\n")
(display "Or:\n")
(display "  (load \"shell/watch-daemon-integration.ss\")\n")
(display "  (dev-mode-on)\n")
(display "\n")
(display "For help:\n")
(display "  (watch-help)\n")
(display "  (dev-help)\n")
(display "  (load \"shell/watch-quickstart.ss\")\n")
(display "\n")
