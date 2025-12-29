;;; meta/test-parse.ss — Tests for the metadata tag parser
;;;
;;; Run with: scheme --script meta/test-parse.ss
;;; Or load in REPL: (load "fabric/patterns/parse.ss") (load "fabric/patterns/test-parse.ss")

(load "fabric/patterns/parse.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *tests-run* 0)
(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (set! *tests-run* (+ *tests-run* 1))
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display (format "  PASS: ~a\n" name)))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display (format "  FAIL: ~a\n" name))
       (display (format "    Expected: ~s\n" expected))
       (display (format "    Actual:   ~s\n" actual)))))

(define (test-summary)
  (display (format "\n~a tests: ~a passed, ~a failed\n"
                   *tests-run* *tests-passed* *tests-failed*))
  (if (= *tests-failed* 0)
      (display "All tests passed!\n")
      (display "SOME TESTS FAILED\n")))

;;; ============================================================
;;; Basic Extraction Tests
;;; ============================================================

(display "\n=== Basic Extraction Tests ===\n\n")

;; Simple key:value
(test "simple key:value"
      '((status . "complete"))
      (extract-tags "Hello @status:complete world"))

;; Multiple tags
(test "multiple tags"
      '((status . "draft") (priority . "high"))
      (extract-tags "@status:draft @priority:high"))

;; Flag (no value)
(test "flag tag"
      '((todo . #t))
      (extract-tags "Fix this @todo"))

;; Mixed flags and values
(test "mixed flags and values"
      '((status . "review") (urgent . #t))
      (extract-tags "@status:review and @urgent"))

;; At line start
(test "tag at line start"
      '((important . #t))
      (extract-tags "@important notice"))

;; With hyphens in key
(test "hyphenated key"
      '((high-priority . "yes"))
      (extract-tags "This is @high-priority:yes"))

;; With path in value
(test "path in value"
      '((see . "core/block.ss"))
      (extract-tags "Check @see:core/block.ss"))

;; Hierarchical value
(test "hierarchical value"
      '((topic . "architecture/blocks"))
      (extract-tags "About @topic:architecture/blocks"))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(display "\n=== Edge Cases ===\n\n")

;; Empty string
(test "empty string"
      '()
      (extract-tags ""))

;; No tags
(test "no tags"
      '()
      (extract-tags "Just regular text"))

;; @ without valid key
(test "@ with capital letter"
      '()
      (extract-tags "Hello @World"))

;; @ in middle of word (should not match)
(test "@ mid-word"
      '()
      (extract-tags "email@example.com"))

;; Tag at end of text
(test "tag at end"
      '((done . #t))
      (extract-tags "Completed @done"))

;; Multiple @ symbols, only valid ones match
(test "multiple @ mixed validity"
      '((valid . #t))
      (extract-tags "user@email.com and @valid tag"))

;; Value with underscores
(test "underscore in value"
      '((file . "my_file.ss"))
      (extract-tags "See @file:my_file.ss"))

;; Value with dots
(test "dots in value"
      '((version . "1.2.3"))
      (extract-tags "Using @version:1.2.3"))

;; Case insensitivity in key
(test "key normalized to lowercase"
      '((status . "OK"))
      (extract-tags "@status:OK"))

;;; ============================================================
;;; Position Tests
;;; ============================================================

(display "\n=== Position Tests ===\n\n")

(let ([positions (extract-tag-positions "Hello @status:done world")])
     (test "position start"
           6
           (caddr (car positions)))
     (test "position end"
           18
           (cadddr (car positions))))

;;; ============================================================
;;; Security Tests
;;; ============================================================

(display "\n=== Security Tests ===\n\n")

;; Dangerous shell characters filtered in values
;; Note: $ stops parsing since it's not in char-value?, resulting in flag
(test "dangerous char stops parsing"
      '((evil . #t))
      (extract-tags "@evil:$rm"))

;; But if dangerous chars are in valid positions, they get sanitized
(test "sanitize embedded dangerous"
      '((cmd . "rm"))
      (safe-extract-tags "@cmd:rm&rf"))

;; Path traversal in value (allowed, just characters)
(test "path traversal chars"
      '((path . "../secret"))
      (extract-tags "@path:../secret"))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(display "\n=== Utility Tests ===\n\n")

(test "has-tag? present"
      #t
      (has-tag? '((status . "done") (todo . #t)) 'status))

(test "has-tag? absent"
      #f
      (has-tag? '((status . "done")) 'priority))

(test "get-tag present"
      "done"
      (get-tag '((status . "done")) 'status))

(test "get-tag absent"
      #f
      (get-tag '((status . "done")) 'priority))

(test "format-tag with value"
      "@status:done"
      (format-tag 'status "done"))

(test "format-tag flag"
      "@urgent"
      (format-tag 'urgent #t))

(test "tags->string"
      "@status:done @priority:high"
      (tags->string '((status . "done") (priority . "high"))))

;;; ============================================================
;;; Validation Tests
;;; ============================================================

(display "\n=== Validation Tests ===\n\n")

(test "valid-tag-key? valid"
      #t
      (valid-tag-key? "status"))

(test "valid-tag-key? with hyphen"
      #t
      (valid-tag-key? "high-priority"))

(test "valid-tag-key? starts with number"
      #f
      (valid-tag-key? "1status"))

(test "valid-tag-key? empty"
      #f
      (valid-tag-key? ""))

(test "valid-tag-value? valid"
      #t
      (valid-tag-value? "core/block.ss"))

(test "valid-tag-value? with special chars"
      #t
      (valid-tag-value? "my_file-v1.2/path"))

;;; ============================================================
;;; Summary
;;; ============================================================

(test-summary)
