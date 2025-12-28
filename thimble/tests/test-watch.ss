;;; Test harness for shell/watch.ss

(load "thimble/watch.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
        (display "✗\n    expected: ")
        (display expected)
        (display "\n    got: ")
        (display actual)))
  (newline))

;;; Test helpers
(define test-dir "./test-watch-tmp")

(define (clean-test-dir!)
  (when (file-exists? test-dir)
    (system (string-append "rm -rf " test-dir))))

(define (setup-test-dir!)
  (clean-test-dir!)
  (mkdir test-dir))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port)
      (display content port))))

(define (sleep-ms ms)
  (sleep (make-time 'time-duration (* ms 1000000) 0)))

(display "File Watching System Tests\n")
(display "===========================\n\n")

;;; Test 1: Glob pattern matching
(display "Test 1: Glob pattern matching\n")
(test "exact match" #t (glob-match? "foo.ss" "foo.ss"))
(test "no match" #f (glob-match? "foo.ss" "bar.ss"))
(test "wildcard *" #t (glob-match? "*.ss" "foo.ss"))
(test "wildcard * prefix" #t (glob-match? "test-*.ss" "test-watch.ss"))
(test "wildcard ? single char" #t (glob-match? "foo?.ss" "foo1.ss"))
(test "wildcard ? no match" #f (glob-match? "foo?.ss" "foo12.ss"))
(test "multiple *" #t (glob-match? "*-*-*.ss" "foo-bar-baz.ss"))

;;; Test 2: File modification time
(display "\nTest 2: File modification time\n")
(setup-test-dir!)
(let ([test-file (string-append test-dir "/mtime-test.txt")])
  (write-test-file test-file "initial content")
  (let ([mtime1 (file-mtime test-file)])
    (test "mtime exists" #t (number? mtime1))
    (sleep-ms 1100)  ; Wait for filesystem timestamp resolution
    (write-test-file test-file "modified content")
    (let ([mtime2 (file-mtime test-file)])
      (test "mtime updated" #t (> mtime2 mtime1)))))

;;; Test 3: Scan paths (single file)
(display "\nTest 3: Scan paths (single file)\n")
(let ([test-file (string-append test-dir "/scan-test.txt")])
  (write-test-file test-file "test")
  (let ([paths (scan-paths test-file #f)])
    (test "single file found" 1 (length paths))
    (test "correct path" test-file (car paths))))

;;; Test 4: Scan paths (directory with pattern)
(display "\nTest 4: Scan paths (directory with pattern)\n")
(write-test-file (string-append test-dir "/test1.ss") "1")
(write-test-file (string-append test-dir "/test2.ss") "2")
(write-test-file (string-append test-dir "/other.txt") "3")
(let ([paths (scan-paths test-dir "*.ss")])
  (test "found .ss files" 2 (length paths)))

;;; Test 5: Watcher creation
(display "\nTest 5: Watcher creation\n")
(let* ([test-file (string-append test-dir "/watch-target.txt")]
       [triggered #f]
       [action (lambda (files) (set! triggered #t))])
  (write-test-file test-file "initial")
  (let ([w (watch-file test-file action)])
    (test "watcher created" #t (watcher? w))
    (test "has id" #t (number? (watcher-id w)))
    (test "has path" test-file (watcher-path w))
    (test "no pattern" #f (watcher-pattern w))
    (test "has action" #t (procedure? (watcher-action w)))
    (stop-watcher! w)))

;;; Test 6: Change detection
(display "\nTest 6: Change detection\n")
(let* ([test-file (string-append test-dir "/change-detect.txt")]
       [changed-files '()]
       [action (lambda (files) (set! changed-files files))])
  (write-test-file test-file "version 1")
  (let ([w (watch-file test-file action)])
    ;; Wait for watcher to initialize
    (sleep-ms 100)
    ;; Modify file
    (sleep-ms 1100)  ; Wait for filesystem timestamp resolution
    (write-test-file test-file "version 2")
    ;; Wait for detection (poll interval + debounce)
    (sleep-ms (+ *watch-poll-interval* *watch-debounce-delay* 500))
    (test "change detected" #t (not (null? changed-files)))
    (when (not (null? changed-files))
      (test "correct file" test-file (car changed-files)))
    (stop-watcher! w)))

;;; Test 7: Directory watching
(display "\nTest 7: Directory watching\n")
(let* ([changed-files '()]
       [action (lambda (files) (set! changed-files files))])
  (let ([w (watch-dir test-dir "*.ss" action)])
    ;; Wait for initialization
    (sleep-ms 100)
    ;; Create new matching file
    (sleep-ms 1100)
    (write-test-file (string-append test-dir "/new-file.ss") "new")
    ;; Wait for detection
    (sleep-ms (+ *watch-poll-interval* *watch-debounce-delay* 500))
    (test "new file detected" #t (not (null? changed-files)))
    (stop-watcher! w)))

;;; Test 8: Multiple watchers
(display "\nTest 8: Multiple watchers\n")
(let* ([file1 (string-append test-dir "/multi1.txt")]
       [file2 (string-append test-dir "/multi2.txt")]
       [trigger1 #f]
       [trigger2 #f]
       [action1 (lambda (f) (set! trigger1 #t))]
       [action2 (lambda (f) (set! trigger2 #t))])
  (write-test-file file1 "content1")
  (write-test-file file2 "content2")
  (let ([w1 (watch-file file1 action1)]
        [w2 (watch-file file2 action2)])
    (sleep-ms 100)
    (sleep-ms 1100)
    (write-test-file file1 "modified1")
    (sleep-ms (+ *watch-poll-interval* *watch-debounce-delay* 500))
    (test "watcher 1 triggered" #t trigger1)
    (test "watcher 2 not triggered" #f trigger2)
    (stop-watcher! w1)
    (stop-watcher! w2)))

;;; Test 9: Stop all watchers
(display "\nTest 9: Stop all watchers\n")
(let ([w1 (watch-file (string-append test-dir "/stop1.txt")
                      (lambda (f) (void)))]
      [w2 (watch-file (string-append test-dir "/stop2.txt")
                      (lambda (f) (void)))])
  (write-test-file (string-append test-dir "/stop1.txt") "1")
  (write-test-file (string-append test-dir "/stop2.txt") "2")
  (sleep-ms 100)
  (stop-watching)
  (test "all watchers stopped" #t
        (begin
          (mutex-acquire *registry-mutex*)
          (let ([empty (null? *watcher-registry*)])
            (mutex-release *registry-mutex*)
            empty))))

;;; Test 10: Debouncing
(display "\nTest 10: Debouncing\n")
(let* ([test-file (string-append test-dir "/debounce.txt")]
       [trigger-count 0]
       [action (lambda (f) (set! trigger-count (+ trigger-count 1)))])
  (write-test-file test-file "initial")
  (let ([w (watch-file test-file action)])
    (sleep-ms 100)
    ;; Rapid modifications (should be debounced)
    (sleep-ms 1100)
    (write-test-file test-file "v1")
    (sleep-ms 50)
    (write-test-file test-file "v2")
    (sleep-ms 50)
    (write-test-file test-file "v3")
    ;; Wait for debounce + one more poll
    (sleep-ms (+ *watch-debounce-delay* *watch-poll-interval* 500))
    (test "debouncing works" #t (<= trigger-count 2))
    (stop-watcher! w)))

;;; Cleanup
(display "\nCleaning up...\n")
(stop-watching)
(clean-test-dir!)
(display "✓ All tests complete\n")
