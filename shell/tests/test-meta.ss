;;; shell/test-meta.ss --- Tests for Inline Metadata Tag Parser
;;;
;;; Run with: scheme -q < test-meta.ss
;;; From the shell/ directory

(load "meta.ss")

;;; ====
;;; Test Framework
;;; ====

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
       (set! pass-count (+ pass-count 1))
       (display "  PASS: ")
       (display name)
       (newline))
      (begin
       (set! fail-count (+ fail-count 1))
       (display "  FAIL: ")
       (display name)
       (newline)
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual:   ")
       (write actual)
       (newline))))

(define (run-tests)
  (display "=== Metadata Tag Parser Tests ===")
  (newline)
  (newline)
  
  ;;; ----
  (display "--- Basic Tag Extraction ---")
  (newline)
  
  (test "simple tag"
        '(("draft" . #t))
        (extract-tags "This is @draft text"))
  
  (test "key-value tag"
        '(("status" . "complete"))
        (extract-tags "This is @status:complete"))
  
  (test "multiple tags"
        '(("draft" . #t) ("priority" . "high"))
        (extract-tags "@draft @priority:high"))
  
  (test "tag at start"
        '(("todo" . #t))
        (extract-tags "@todo Fix this"))
  
  (test "tag at end"
        '(("done" . #t))
        (extract-tags "All finished @done"))
  
  (test "no tags"
        '()
        (extract-tags "Plain text with no markers"))
  
  (test "empty string"
        '()
        (extract-tags ""))
  
  ;;; ----
  (display "--- Path Values ---")
  (newline)
  
  (test "see with path"
        '(("see" . "core/blocks/block.ss"))
        (extract-tags "@see:core/block.ss"))
  
  (test "ref with id"
        '(("ref" . "0001-genesis"))
        (extract-tags "@ref:0001-genesis"))
  
  (test "complex path"
        '(("see" . "../docs/decisions/001.md"))
        (extract-tags "@see:../docs/decisions/001.md"))
  
  ;;; ----
  (display "--- Email Address Exclusion ---")
  (newline)
  
  (test "email not a tag"
        '()
        (extract-tags "Contact user@domain.com for help"))
  
  (test "email with subdomain"
        '()
        (extract-tags "Send to admin@mail.example.org"))
  
  (test "tag after email"
        '(("urgent" . #t))
        (extract-tags "Email user@domain.com @urgent"))
  
  (test "lone @ not a tag"
        '()
        (extract-tags "@ is just an at sign"))
  
  ;;; ----
  (display "--- Word Boundaries ---")
  (newline)
  
  (test "tag after newline"
        '(("todo" . #t))
        (extract-tags "First line\n@todo second"))
  
  (test "tag after paren"
        '(("see" . "block.ss"))
        (extract-tags "(also @see:block.ss)"))
  
  (test "tag in quotes"
        '(("important" . #t))
        (extract-tags "She said \"@important stuff\""))
  
  ;;; ----
  (display "--- has-tag? ---")
  (newline)
  
  (test "has-tag? true"
        #t
        (has-tag? "This is @draft" "draft"))
  
  (test "has-tag? false"
        #f
        (has-tag? "This is @draft" "reviewed"))
  
  (test "has-tag? with value"
        #t
        (has-tag? "@status:complete" "status"))
  
  ;;; ----
  (display "--- get-tag-value ---")
  (newline)
  
  (test "get value"
        "high"
        (get-tag-value "@priority:high" "priority"))
  
  (test "get simple tag"
        #t
        (get-tag-value "@draft" "draft"))
  
  (test "get missing tag"
        #f
        (get-tag-value "@draft" "status"))
  
  ;;; ----
  (display "--- get-all-tag-values ---")
  (newline)
  
  (test "multiple same tags"
        '("block.ss" "types.ss")
        (get-all-tag-values "@see:block.ss @see:types.ss" "see"))
  
  (test "no matches"
        '()
        (get-all-tag-values "@todo" "see"))
  
  ;;; ----
  (display "--- strip-tags ---")
  (newline)
  
  (test "strip simple"
        "This is  text"
        (strip-tags "This is @draft text"))
  
  (test "strip key-value"
        "Ready  for review"
        (strip-tags "Ready @status:complete for review"))
  
  (test "strip multiple"
        "  Do this"
        (strip-tags "@todo @priority:high Do this"))
  
  (test "strip preserves non-tags"
        "Email user@domain.com"
        (strip-tags "Email user@domain.com"))
  
  (test "strip empty result"
        ""
        (strip-tags "@only @tags @here"))
  
  ;;; ----
  (display "--- highlight-tags ---")
  (newline)
  
  (test "highlight simple"
        "This is [TAG:draft] text"
        (highlight-tags "This is @draft text"))
  
  (test "highlight key-value"
        "[TAG:status=complete] done"
        (highlight-tags "@status:complete done"))
  
  (test "highlight preserves email"
        "user@domain.com [TAG:urgent]"
        (highlight-tags "user@domain.com @urgent"))
  
  ;;; ----
  (display "--- Convenience Functions ---")
  (newline)
  
  (test "get-status explicit"
        "wip"
        (get-status "@status:wip in progress"))
  
  (test "get-status bare"
        "draft"
        (get-status "This is @draft"))
  
  (test "get-status none"
        #f
        (get-status "No status here"))
  
  (test "get-priority"
        "critical"
        (get-priority "@priority:critical"))
  
  (test "get-references"
        '("core/blocks/block.ss" "0001")
        (get-references "@see:core/block.ss @ref:0001"))
  
  (test "is-draft?"
        #t
        (is-draft? "@draft"))
  
  (test "is-draft? via status"
        #t
        (is-draft? "@status:draft"))
  
  (test "is-complete?"
        #t
        (is-complete? "@complete"))
  
  (test "is-todo?"
        #t
        (is-todo? "@todo fix this"))
  
  (test "is-high-priority? high"
        #t
        (is-high-priority? "@priority:high"))
  
  (test "is-high-priority? critical"
        #t
        (is-high-priority? "@priority:critical"))
  
  (test "is-high-priority? low"
        #f
        (is-high-priority? "@priority:low"))
  
  ;;; ----
  (display "--- Edge Cases ---")
  (newline)
  
  (test "tag with underscore"
        '(("work_in_progress" . #t))
        (extract-tags "@work_in_progress"))
  
  (test "tag with hyphen"
        '(("needs-review" . #t))
        (extract-tags "@needs-review"))
  
  (test "value with hyphen"
        '(("id" . "abc-123-def"))
        (extract-tags "@id:abc-123-def"))
  
  (test "adjacent tags"
        '(("a" . #t) ("b" . #t))
        (extract-tags "@a @b"))
  
  ;; When colon has no value (whitespace follows), it's a simple tag
  ;; The colon remains in text, tag stops before it
  (test "colon no value stays simple"
        '(("tag" . #t))
        (extract-tags "@tag: with space"))
  
  (test "colon with immediate value"
        '(("tag" . "value"))
        (extract-tags "@tag:value here"))
  
  ;;; ----
  (display "--- Real World Examples ---")
  (newline)
  
  (test "forum post metadata"
        '(("status" . "complete") ("topic" . "architecture"))
        (extract-tags "The Block is born. @status:complete @topic:architecture"))
  
  (test "wishlist reference"
        '(("see" . "core/blocks/block.ss") ("todo" . "implement") ("priority" . "high"))
        (extract-tags "We now have content-addressing. @see:core/block.ss
The next step is normalization. @todo:implement @priority:high"))
  
  (test "mixed prose and tags"
        '(("draft" . #t) ("needs-review" . #t))
        (extract-tags "This documentation is @draft and @needs-review by the team."))
  
  ;;; ----
  ;;; Summary
  (newline)
  (display "=== Test Summary ===")
  (newline)
  (display "Total: ") (display test-count) (newline)
  (display "Passed: ") (display pass-count) (newline)
  (display "Failed: ") (display fail-count) (newline)
  
  (if (= fail-count 0)
      (display "All tests passed!")
      (display "Some tests failed."))
  (newline))

;;; Run the tests
(run-tests)
