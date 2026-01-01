;;; tests/test-markdown.ss
(load "shell/markdown.ss")

(define (assert-equal? actual expected msg)
  (if (equal? actual expected)
      (display (string-append "PASS: " msg "\n"))
      (begin
       (display (string-append "FAIL: " msg " - Expected " (format "~s" expected) ", got " (format "~s" actual) "\n"))
       (exit 1))))

(define (test-inline)
  (display "\n=== Inline Formatting ===\n")
  (assert-equal? (bold "foo") "**foo**" "Bold")
  (assert-equal? (italic "foo") "*foo*" "Italic")
  (assert-equal? (code "foo") "`foo`" "Code")
  (assert-equal? (link "foo" "url") "[foo](url)" "Link"))

(define (test-blocks)
  (display "\n=== Block Formatting ===\n")
  (assert-equal? (header 1 "Title") "# Title\n" "H1")
  (assert-equal? (header 2 "Title") "## Title\n" "H2")
  (assert-equal? (section "Title" "Content") "Title\n─────\nContent\n\n" "Section")
  (assert-equal? (code-block "foo") "```\nfoo\n```\n" "Code block default")
  (assert-equal? (code-block "foo" "scm") "```scm\nfoo\n```\n" "Code block lang"))

(define (test-lists)
  (display "\n=== Lists ===\n")
  (assert-equal? (bullet-list (list "a" "b")) "• a\n• b\n" "Bullet list")
  (assert-equal? (numbered-list (list "a" "b")) "1. a\n2. b\n" "Numbered list")
  
  (display "--- Indented Lists ---\
")
  (assert-equal? (bullet-list (list "a") 1) "  • a\n" "Indented bullet")
  (assert-equal? (numbered-list (list "a") 1) "  1. a\n" "Indented numbered"))

(test-inline)
(test-blocks)
(test-lists)
