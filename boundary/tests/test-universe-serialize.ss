;;; boundary/universe-serialize-test.ss — Tests for Universe Serialization
;;;
;;; Test harness for boundary/universe-serialize.ss

;;; Load dependencies
(load "core/base/prelude.ss")

;;; NOTE: string utilities provided by core/prelude.ss
;;;   - string-contains?

;;; Load the library
(import (shell universe-serialize))

;;; ====
;;; Test Infrastructure
;;; ====

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! pass-count (+ pass-count 1))
       (display "✓"))
      (begin
       (set! fail-count (+ fail-count 1))
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(define (test-pred name pred actual)
  (set! test-count (+ test-count 1))
  (display "  ")
  (display name)
  (display ": ")
  (if (pred actual)
      (begin
       (set! pass-count (+ pass-count 1))
       (display "✓"))
      (begin
       (set! fail-count (+ fail-count 1))
       (display "✗\n    predicate failed for: ")
       (display actual)))
  (newline))

;;; ====
;;; Test Data Setup
;;; ====

;;; Create a temporary test directory with .sexp files
(define test-dir "./test-universe")

(define (clean-test-dir!)
  (when (file-exists? test-dir)
        (system (string-append "rm -rf " test-dir))))

(define (setup-test-dir!)
  (clean-test-dir!)
  (mkdir test-dir)
  
  ;; Create subdirectories (including nested ones)
  (mkdir (string-append test-dir "/forum"))
  (mkdir (string-append test-dir "/forum/poetry"))
  (mkdir (string-append test-dir "/scripture"))
  (mkdir (string-append test-dir "/playpen"))
  (mkdir (string-append test-dir "/docs"))
  (mkdir (string-append test-dir "/docs/decisions"))
  
  ;; Create test .sexp files
  (call-with-output-file (string-append test-dir "/forum/poetry/test1.sexp")
                         (lambda (port)
                                 (write '((author . "test-author")
                                          (content . "Test poem")) port)))
  
  (call-with-output-file (string-append test-dir "/forum/test2.sexp")
                         (lambda (port)
                                 (write '((type . "engineering")
                                          (data . 42)) port)))
  
  (call-with-output-file (string-append test-dir "/docs/decisions/protocol.sexp")
                         (lambda (port)
                                 (write '((title . "Test Protocol")
                                          (rules . ("rule1" "rule2"))) port)))
  
  (call-with-output-file (string-append test-dir "/playpen/experiment.sexp")
                         (lambda (port)
                                 (write '((experiment . "duckie")
                                          (result . success)) port)))
  
  ;; Create a non-.sexp file (should be ignored)
  (call-with-output-file (string-append test-dir "/forum/readme.txt")
                         (lambda (port)
                                 (display "This should be ignored" port)))
  
  ;; Create a malformed .sexp file (should be skipped with warning)
  (call-with-output-file (string-append test-dir "/forum/broken.sexp")
                         (lambda (port)
                                 (display "((this is not valid scheme" port))))

;;; ====
;;; Run Tests
;;; ====

(display "Universe Serialization Tests\n")
(display "====\n\n")

;;; Setup
(setup-test-dir!)

;;; Test 1: Path utilities
(display "Test 1: Path utilities\n")
(test "sexp-file? positive" #t (sexp-file? "test.sexp"))
(test "sexp-file? negative" #f (sexp-file? "test.ss"))
(test "sexp-file? short name" #f (sexp-file? ".sxp"))
(test "make-relative-path"
      "forum/test.sexp"
      (make-relative-path "/home/fold" "/home/fold/forum/test.sexp"))
(test "make-relative-path with trailing slash"
      "forum/test.sexp"
      (make-relative-path "/home/fold/" "/home/fold/forum/test.sexp"))

;;; Test 2: File scanning
(display "\nTest 2: File scanning\n")
(define all-sexp-files (scan-sexp-files test-dir))
(display "  Found files:\n")
(for-each (lambda (f)
                  (display "    ")
                  (display (make-relative-path test-dir f))
                  (newline))
          all-sexp-files)
(test-pred "scan-sexp-files returns list" list? all-sexp-files)
(test "found multiple files" #t (> (length all-sexp-files) 0))
(test "found poetry file"
      #t
      (any (lambda (f) (string-contains? f "poetry/test1.sexp")) all-sexp-files))
(test "found scripture file"
      #t
      (any (lambda (f) (string-contains? f "docs/decisions/protocol.sexp")) all-sexp-files))
(test "ignored txt file"
      #f
      (any (lambda (f) (string-contains? f "readme.txt")) all-sexp-files))

;;; Helper function
(define (any pred lst)
  (if (null? lst)
      #f
      (or (pred (car lst))
          (any pred (cdr lst)))))

;;; Test 3: Filtered scanning
(display "\nTest 3: Filtered scanning\n")
(define forum-files (scan-sexp-files-filtered test-dir '("forum")))
(display "  Forum files:\n")
(for-each (lambda (f)
                  (display "    ")
                  (display (make-relative-path test-dir f))
                  (newline))
          forum-files)
(test "filter by forum directory" #t (> (length forum-files) 0))
(test "forum filter includes poetry"
      #t
      (any (lambda (f) (string-contains? f "poetry/test1.sexp")) forum-files))
(test "forum filter excludes scripture"
      #f
      (any (lambda (f) (string-contains? f "docs/decisions")) forum-files))

(define multi-filter (scan-sexp-files-filtered test-dir '("docs/decisions" "user")))
(test "multi-directory filter" #t (> (length multi-filter) 0))
(test "multi-filter excludes forum"
      #f
      (any (lambda (f) (string-contains? f "forum")) multi-filter))

;;; Test 4: Reading individual files
(display "\nTest 4: Reading individual files\n")
(define test-file (string-append test-dir "/forum/poetry/test1.sexp"))
(define read-data (read-sexp-file test-file))
(test-pred "read-sexp-file returns data" pair? read-data)
(test "read correct author"
      "test-author"
      (cdr (assoc 'author read-data)))

(define broken-file (string-append test-dir "/forum/broken.sexp"))
(display "  Reading broken file (should show warning):\n")
(define broken-data (read-sexp-file broken-file))
(test "broken file returns #f" #f broken-data)

;;; Test 5: Full universe serialization
(display "\nTest 5: Full universe serialization\n")
(define universe (serialize-universe test-dir))
(test-pred "universe is proper list" pair? universe)
(test "universe has 'files' key"
      'files
      (caar universe))

(define files-list (cadr (car universe)))
(test-pred "files list is a list" list? files-list)
(display "  Serialized files count: ")
(display (length files-list))
(newline)
(test "serialized multiple files" #t (> (length files-list) 0))

;; Check that paths are relative
(define first-file (car files-list))
(test-pred "first file is pair" pair? first-file)
(define first-path (car first-file))
(test "paths are relative (no leading /)"
      #f
      (and (> (string-length first-path) 0)
           (char=? (string-ref first-path 0) #\/)))

;;; Test 6: Filtered serialization
(display "\nTest 6: Filtered serialization\n")
(define forum-universe (serialize-universe-filtered test-dir '("forum")))
(define forum-files-data (cadr (car forum-universe)))
(display "  Forum-only files count: ")
(display (length forum-files-data))
(newline)
(test "filtered universe has files" #t (> (length forum-files-data) 0))
(test "filtered universe smaller than full"
      #t
      (< (length forum-files-data) (length files-list)))

;;; Test 7: Writing universe to file
(display "\nTest 7: Writing universe to file\n")
(define output-file (string-append test-dir "/universe-dump.sexp"))
(write-universe universe output-file #f)
(test "output file exists" #t (file-exists? output-file))

(define output-file-pretty (string-append test-dir "/universe-dump-pretty.sexp"))
(write-universe-pretty universe output-file-pretty)
(test "pretty output file exists" #t (file-exists? output-file-pretty))

;; Verify we can read it back
(define reloaded (call-with-input-file output-file read))
(test "reloaded universe matches" universe reloaded)

;; Check pretty output is larger (has formatting)
(define size-compact (file-length output-file))
(define size-pretty (file-length output-file-pretty))
(display "  Compact size: ")
(display size-compact)
(display " bytes\n")
(display "  Pretty size: ")
(display size-pretty)
(display " bytes\n")
(test "pretty output is larger" #t (> size-pretty size-compact))

;;; Test 8: Empty directory handling
(display "\nTest 8: Empty directory handling\n")
(define empty-dir (string-append test-dir "/empty"))
(mkdir empty-dir)
(define empty-scan (scan-sexp-files empty-dir))
(test "empty directory returns empty list" '() empty-scan)
(define empty-universe (serialize-universe empty-dir))
(define empty-files (cadr (car empty-universe)))
(test "empty universe has no files" '() empty-files)

;;; ====
;;; Test Summary
;;; ====

(display "\n====\n")
(display "Test Summary\n")
(display "====\n")
(display "Total tests: ")
(display test-count)
(newline)
(display "Passed: ")
(display pass-count)
(newline)
(display "Failed: ")
(display fail-count)
(newline)

(if (= fail-count 0)
    (display "\n✓ All tests passed!\n")
    (begin
     (display "\n✗ Some tests failed.\n")
     (exit 1)))

;;; Cleanup
(display "\nCleaning up test directory...\n")
(clean-test-dir!)

(display "Tests complete.\n")
