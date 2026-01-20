(doc 'module 'test-markdown)
(doc 'description "Tests for markdown utilities")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(load "boundary/tools/markdown.ss")

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

;;; ====
;;; Test 1: Bold
;;; ====

(printf "\n=== Test 1: Bold ===\n")

(assert-equal "bold text"
              (bold "hello")
              "**hello**")
(assert-equal "bold empty"
              (bold "")
              "****")
(assert-equal "bold with spaces"
              (bold "hello world")
              "**hello world**")

;;; ====
;;; Test 2: Italic
;;; ====

(printf "\n=== Test 2: Italic ===\n")

(assert-equal "italic text"
              (italic "hello")
              "*hello*")
(assert-equal "italic empty"
              (italic "")
              "**")
(assert-equal "italic with spaces"
              (italic "hello world")
              "*hello world*")

;;; ====
;;; Test 3: Code
;;; ====

(printf "\n=== Test 3: Code ===\n")

(assert-equal "code span"
              (code "foo")
              "`foo`")
(assert-equal "code with spaces"
              (code "let x = 10")
              "`let x = 10`")
(assert-equal "code empty"
              (code "")
              "``")

;;; ====
;;; Test 4: Link
;;; ====

(printf "\n=== Test 4: Link ===\n")

(assert-equal "basic link"
              (link "GitHub" "https://github.com")
              "[GitHub](https://github.com)")
(assert-equal "link with empty text"
              (link "" "url")
              "[](url)")
(assert-equal "link with empty url"
              (link "text" "")
              "[text]()")

;;; ====
;;; Test 5: Header
;;; ====

(printf "\n=== Test 5: Header ===\n")

(assert-equal "h1 header"
              (header 1 "Title")
              "# Title\n")
(assert-equal "h2 header"
              (header 2 "Subtitle")
              "## Subtitle\n")
(assert-equal "h3 header"
              (header 3 "Section")
              "### Section\n")
(assert-equal "h6 header"
              (header 6 "Deep")
              "###### Deep\n")

;;; ====
;;; Test 6: Section
;;; ====

(printf "\n=== Test 6: Section ===\n")

(let ([result (section "Title")])
     (assert-true "section contains title"
                  (string-contains? result "Title"))
     (assert-true "section contains separator"
                  (string-contains? result "─")))

(let ([result (section "Title" "Content here")])
     (assert-true "section with content contains title"
                  (string-contains? result "Title"))
     (assert-true "section with content contains content"
                  (string-contains? result "Content")))

;;; ====
;;; Test 7: Quote
;;; ====

(printf "\n=== Test 7: Quote ===\n")

(assert-equal "single line quote"
              (quote "Hello")
              "> Hello\n")

(let ([result (quote "Line 1\nLine 2")])
     (assert-true "multi-line quote has > for each line"
                  (and (string-contains? result "> Line 1")
                       (string-contains? result "> Line 2"))))

;;; ====
;;; Test 8: Code Block
;;; ====

(printf "\n=== Test 8: Code Block ===\n")

(let ([result (code-block "code here")])
     (assert-true "code block has backticks"
                  (string-contains? result "```"))
     (assert-true "code block contains code"
                  (string-contains? result "code here")))

(let ([result (code-block "scheme code" "scheme")])
     (assert-true "code block with lang has language"
                  (string-contains? result "```scheme"))
     (assert-true "code block with lang contains code"
                  (string-contains? result "scheme code")))

;; Test with trailing newline
(let ([result (code-block "code\n")])
     (assert-true "code block preserves newline"
                  (string-contains? result "code\n")))

;; Test without trailing newline
(let ([result (code-block "code")])
     (assert-true "code block adds newline"
                  (string-contains? result "code\n")))

;;; ====
;;; Test 9: Bullet List (No Indent)
;;; ====

(printf "\n=== Test 9: Bullet List (No Indent) ===\n")

(let ([empty-result (bullet-list (list))])
     (assert-equal "empty bullet list" empty-result ""))

(let ([result (bullet-list (list "one" "two" "three"))])
     (assert-true "bullet list has bullets"
                  (string-contains? result "•"))
     (assert-true "bullet list contains items"
                  (and (string-contains? result "one")
                       (string-contains? result "two")
                       (string-contains? result "three"))))

(let ([result (bullet-list (list "single"))])
     (assert-true "single item bullet list"
                  (and (string-contains? result "•")
                       (string-contains? result "single"))))

;;; ====
;;; Test 10: Bullet List (With Indent)
;;; ====

(printf "\n=== Test 10: Bullet List (With Indent) ===\n")

(let ([result (bullet-list (list "one" "two") 1)])
     (assert-true "indented bullet list has items"
                  (and (string-contains? result "one")
                       (string-contains? result "two")))
     (assert-true "indented bullet list has bullets"
                  (string-contains? result "•")))

(let ([result (bullet-list (list "item") 2)])
     (assert-true "double indented has spaces"
                  (string-contains? result "    •")))

;;; ====
;;; Test 11: Numbered List (No Indent)
;;; ====

(printf "\n=== Test 11: Numbered List (No Indent) ===\n")

(assert-equal "empty numbered list"
              (numbered-list (list))
              "")

(let ([result (numbered-list (list "first" "second" "third"))])
     (assert-true "numbered list has numbers"
                  (and (string-contains? result "1.")
                       (string-contains? result "2.")
                       (string-contains? result "3.")))
     (assert-true "numbered list contains items"
                  (and (string-contains? result "first")
                       (string-contains? result "second")
                       (string-contains? result "third"))))

(let ([result (numbered-list (list "only"))])
     (assert-true "single numbered item"
                  (and (string-contains? result "1.")
                       (string-contains? result "only"))))

;;; ====
;;; Test 12: Numbered List (With Indent)
;;; ====

(printf "\n=== Test 12: Numbered List (With Indent) ===\n")

(let ([result (numbered-list (list "a" "b") 1)])
     (assert-true "indented numbered list has numbers"
                  (and (string-contains? result "1.")
                       (string-contains? result "2.")))
     (assert-true "indented numbered list has items"
                  (and (string-contains? result "a")
                       (string-contains? result "b"))))

;;; ====
;;; Test 13: Combining Formatting
;;; ====

(printf "\n=== Test 13: Combining Formatting ===\n")

(let ([result (bold (italic "text"))])
     (assert-equal "bold italic combines"
                   result
                   "***text***"))

(let ([result (link (bold "Bold Link") "url")])
     (assert-equal "bold link combines"
                   result
                   "[**Bold Link**](url)"))

(let ([result (code (bold "code"))])
     (assert-true "code bold combines"
                  (and (string-contains? result "`")
                       (string-contains? result "**"))))

;;; ====
;;; Test 14: Lists with Formatted Items
;;; ====

(printf "\n=== Test 14: Lists with Formatted Items ===\n")

(let ([result (bullet-list (list (bold "Bold Item")
                                 (italic "Italic Item")
                                 (code "code")))])
     (assert-true "bullet list with formatting has bold"
                  (string-contains? result "**"))
     (assert-true "bullet list with formatting has italic"
                  (string-contains? result "*"))
     (assert-true "bullet list with formatting has code"
                  (string-contains? result "`")))

;;; ====
;;; Test 15: Special Characters
;;; ====

(printf "\n=== Test 15: Special Characters ===\n")

(assert-equal "bold with asterisks"
              (bold "a * b")
              "**a * b**")

(assert-equal "code with backticks"
              (code "x = `val`")
              "`x = `val``")

(assert-equal "link with brackets"
              (link "text[0]" "url")
              "[text[0]](url)")

;;; ====
;;; Test 16: Empty/Edge Cases
;;; ====

(printf "\n=== Test 16: Empty/Edge Cases ===\n")

(assert-equal "header with empty title"
              (header 1 "")
              "# \n")

(assert-equal "section with empty title"
              (section "")
              "\n\n\n\n")

(let ([result (quote "")])
     (assert-equal "quote empty string"
                   result
                   "> \n"))

;;; ====
;;; Test 17: Real-World Examples
;;; ====

(printf "\n=== Test 17: Real-World Examples ===\n")

;; Create a sample document
(let ([doc (string-append
            (header 1 "My Document")
            "\n"
            (section "Introduction" "This is the intro.\n")
            (bullet-list (list "Point 1" "Point 2" "Point 3"))
            "\n"
            (code-block "(define x 10)" "scheme"))])
     (assert-true "document contains header" (string-contains? doc "# My Document"))
     (assert-true "document contains section" (string-contains? doc "Introduction"))
     (assert-true "document contains list" (string-contains? doc "•"))
     (assert-true "document contains code block" (string-contains? doc "```scheme")))

;;; ====
;;; Test Summary
;;; ====

(printf "\n====\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
