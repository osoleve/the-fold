;;; shell/tools/test-export.ss — Tests for Export Utilities
;;;
;;; Run from project root: scheme --script shell/tools/test-export.ss

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")
(load "shell/fs.ss")
(load "forum/tools.ss")
(load "shell/tools/export.ss")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test assertion helper
(define (assert-equal name actual expected)
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

(define (assert-true name condition)
  (set! test-count (+ test-count 1))
  (if condition
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: #t\n")
       (printf "    Actual:   #f\n"))))

;;; ============================================================
;;; Test 1: Format Header
;;; ============================================================

(printf "\n=== Test 1: Format Header ===\n")

(let ([header (format-header "TEST")])
     (assert-true "header contains title"
                  (string-contains? header "TEST"))
     (assert-true "header contains separator"
                  (string-contains? header "=")))

;;; ============================================================
;;; Test 2: Format Section Header
;;; ============================================================

(printf "\n=== Test 2: Format Section Header ===\n")

(let ([section (format-section-header 'poetry)])
     (assert-true "section contains channel name"
                  (string-contains? section "poetry"))
     (assert-true "section contains #"
                  (string-contains? section "#")))

;;; ============================================================
;;; Test 3: Format Post
;;; ============================================================

(printf "\n=== Test 3: Format Post ===\n")

(let* ([post `((author . alice)
               (tier . builder)
               (timestamp . 12345)
               (body . "Hello world"))]
       [formatted (format-post post)])
      (assert-true "post contains author"
                   (string-contains? formatted "alice"))
      (assert-true "post contains tier"
                   (string-contains? formatted "builder"))
      (assert-true "post contains body"
                   (string-contains? formatted "Hello world")))

;;; ============================================================
;;; Test 4: Export Channel to String (Empty)
;;; ============================================================

(printf "\n=== Test 4: Export Channel to String (Empty) ===\n")

(let* ([fs (mint-fs-capability ".store")]
       ;; Use a channel that likely doesn't exist or is empty
       [result (export-channel-to-string fs 'nonexistent-channel-xyz)])
      (assert-equal "empty channel returns empty string" result ""))

;;; ============================================================
;;; Test 5: Count Objects
;;; ============================================================

(printf "\n=== Test 5: Count Objects ===\n")

;; Test with .store directory
(let ([count (count-objects ".store")])
     (assert-true "count is non-negative" (>= count 0))
     (assert-true "count is number" (number? count)))

;;; Test with non-existent directory
(let ([count (count-objects "/nonexistent-path-xyz")])
     (assert-equal "non-existent path returns 0" count 0))

;;; ============================================================
;;; Test 6: Forum Stats
;;; ============================================================

(printf "\n=== Test 6: Forum Stats ===\n")

;; Should not error
(guard (e [else (assert-true "forum-stats failed" #f)])
       (forum-stats)
       (assert-true "forum-stats succeeded" #t))

;;; ============================================================
;;; Test 7: Export Functions (File Creation)
;;; ============================================================

(printf "\n=== Test 7: Export Functions (File Creation) ===\n")

;; Test export-forums to temporary file
(let ([temp-file "/tmp/test-forum-export.txt"])
     ;; Clean up if exists
     (when (file-exists? temp-file)
           (delete-file temp-file))
     
     ;; Export
     (guard (e [else
                (printf "    Export error: ~a\n"
                        (if (condition? e) (condition-message e) e))
                (assert-true "export-forums failed" #f)])
            (export-forums temp-file)
            (assert-true "export file created" (file-exists? temp-file))
            
            ;; Check contents
            (when (file-exists? temp-file)
                  (let ([contents (call-with-input-file temp-file
                                                        (lambda (port)
                                                                (get-string-all port)))])
                       (assert-true "export contains header"
                                    (string-contains? contents "FORUM EXPORT"))
                       (assert-true "export contains end marker"
                                    (string-contains? contents "END OF EXPORT"))))
            
            ;; Clean up
            (when (file-exists? temp-file)
                  (delete-file temp-file))))

;;; ============================================================
;;; Test 8: Export Single Channel
;;; ============================================================

(printf "\n=== Test 8: Export Single Channel ===\n")

(let ([temp-file "/tmp/test-channel-export.txt"])
     ;; Clean up if exists
     (when (file-exists? temp-file)
           (delete-file temp-file))
     
     ;; Export a channel
     (guard (e [else
                (printf "    Export channel error: ~a\n"
                        (if (condition? e) (condition-message e) e))
                (assert-true "export-channel failed" #f)])
            (export-channel 'poetry temp-file)
            (assert-true "channel export file created" (file-exists? temp-file))
            
            ;; Check contents
            (when (file-exists? temp-file)
                  (let ([contents (call-with-input-file temp-file
                                                        (lambda (port)
                                                                (get-string-all port)))])
                       (assert-true "channel export contains channel name"
                                    (string-contains? contents "poetry"))))
            
            ;; Clean up
            (when (file-exists? temp-file)
                  (delete-file temp-file))))

;;; ============================================================
;;; Test 9: Export Chat
;;; ============================================================

(printf "\n=== Test 9: Export Chat ===\n")

(let ([temp-file "/tmp/test-chat-export.txt"])
     ;; Clean up if exists
     (when (file-exists? temp-file)
           (delete-file temp-file))
     
     ;; Export chat
     (guard (e [else
                (printf "    Export chat error: ~a\n"
                        (if (condition? e) (condition-message e) e))
                (assert-true "export-chat failed" #f)])
            (export-chat temp-file)
            (assert-true "chat export file created" (file-exists? temp-file))
            
            ;; Clean up
            (when (file-exists? temp-file)
                  (delete-file temp-file))))

;;; ============================================================
;;; Test 10: Export Store Manifest
;;; ============================================================

(printf "\n=== Test 10: Export Store Manifest ===\n")

(let ([temp-file "/tmp/test-manifest.txt"])
     ;; Clean up if exists
     (when (file-exists? temp-file)
           (delete-file temp-file))
     
     ;; Export manifest
     (guard (e [else
                (printf "    Export manifest error: ~a\n"
                        (if (condition? e) (condition-message e) e))
                (assert-true "export-store-manifest failed" #f)])
            (export-store-manifest temp-file)
            (assert-true "manifest file created" (file-exists? temp-file))
            
            ;; Check contents
            (when (file-exists? temp-file)
                  (let ([contents (call-with-input-file temp-file
                                                        (lambda (port)
                                                                (get-string-all port)))])
                       (assert-true "manifest contains header"
                                    (string-contains? contents "STORE MANIFEST"))
                       (assert-true "manifest contains objects section"
                                    (string-contains? contents "OBJECTS"))))
            
            ;; Clean up
            (when (file-exists? temp-file)
                  (delete-file temp-file))))

;;; ============================================================
;;; Test 11: Export Help
;;; ============================================================

(printf "\n=== Test 11: Export Help ===\n")

;; Should not error
(guard (e [else (assert-true "export-help failed" #f)])
       (export-help)
       (assert-true "export-help succeeded" #t))

;;; ============================================================
;;; Test 12: Channel Configuration
;;; ============================================================

(printf "\n=== Test 12: Channel Configuration ===\n")

(assert-true "has export channels" (list? *export-channels*))
(assert-true "export channels not empty" (not (null? *export-channels*)))
(assert-true "poetry in channels" (memq 'poetry *export-channels*))
(assert-true "chat in channels" (memq 'chat *export-channels*))

;;; ============================================================
;;; Test Summary
;;; ============================================================

(printf "\n========================================\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
