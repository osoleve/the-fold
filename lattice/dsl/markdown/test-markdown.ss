;;; lattice/dsl/markdown/test-markdown.ss — Tests for Markdown Parser
;;;
;;; Run with: scheme --script lattice/dsl/markdown/test-markdown.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/dsl/markdown/block-parser.ss")
(load "lattice/dsl/markdown/html.ss")

;;; ============================================================
;;; AST Tests
;;; ============================================================

(test-group "markdown-ast"
  (define-test "md-heading creates correct structure"
    (assert-equal '(h1 (text "Hello"))
                  (md-h1 (list (md-text "Hello")))))

  (define-test "md-paragraph creates correct structure"
    (assert-equal '(p (text "Test"))
                  (md-paragraph (list (md-text "Test")))))

  (define-test "md-code-block creates correct structure"
    (assert-equal '(code-block "scheme" "(+ 1 2)")
                  (md-code-block "scheme" "(+ 1 2)")))

  (define-test "md-link creates correct structure"
    (assert-equal '(link "http://example.com" "" ((text "Example")))
                  (md-link "http://example.com" "" (list (md-text "Example")))))

  (define-test "md-heading? recognizes headings"
    (assert-true (md-heading? '(h1 (text "Test")))))

  (define-test "md-block? recognizes blocks"
    (assert-true (md-block? '(p (text "Test")))))

  (define-test "md-inline? recognizes inlines"
    (assert-true (md-inline? '(text "Test")))))

;;; ============================================================
;;; HTML Rendering Tests
;;; ============================================================

(test-group "markdown-html"
  (define-test "html-escape escapes special chars"
    (assert-equal "&lt;div&gt;" (html-escape "<div>")))

  (define-test "render heading"
    (assert-equal "<h1>\nHello</h1>\n"
                  (render-html (md-h1 (list (md-text "Hello"))))))

  (define-test "render paragraph"
    (assert-equal "<p>\nTest</p>\n"
                  (render-html (md-paragraph (list (md-text "Test"))))))

  (define-test "render code block"
    (assert-equal "<pre><code class=\"language-scheme\">(+ 1 2)</code></pre>\n"
                  (render-html (md-code-block "scheme" "(+ 1 2)"))))

  (define-test "render strong"
    (assert-equal "<strong>bold</strong>"
                  (render-html (md-strong (list (md-text "bold"))))))

  (define-test "render emphasis"
    (assert-equal "<em>italic</em>"
                  (render-html (md-em (list (md-text "italic"))))))

  (define-test "render inline code"
    (assert-equal "<code>code</code>"
                  (render-html (md-code "code"))))

  (define-test "render link"
    (assert-equal "<a href=\"http://example.com\">Example</a>"
                  (render-html (md-link "http://example.com" ""
                                        (list (md-text "Example"))))))

  (define-test "render horizontal rule"
    (assert-equal "<hr>\n"
                  (render-html (md-hr)))))

;;; ============================================================
;;; Inline Parser Tests
;;; ============================================================

(test-group "markdown-inline-parser"
  (define-test "parse plain text"
    (let ([result (parse-inline "hello world")])
      (assert-true (right? result))
      (assert-equal '((text "hello world"))
                    (from-right result))))

  (define-test "parse inline code"
    (let ([result (parse-inline "`code`")])
      (assert-true (right? result))
      (assert-equal '((code "code"))
                    (from-right result))))

  (define-test "parse bold with **"
    (let ([result (parse-inline "**bold**")])
      (assert-true (right? result))
      (let ([parsed (from-right result)])
        (assert-true (eq? 'strong (caar parsed))))))

  (define-test "parse italic with *"
    (let ([result (parse-inline "*italic*")])
      (assert-true (right? result))
      (let ([parsed (from-right result)])
        (assert-true (eq? 'em (caar parsed))))))

  (define-test "parse link"
    (let ([result (parse-inline "[Example](http://example.com)")])
      (assert-true (right? result))
      (let ([parsed (from-right result)])
        (assert-true (eq? 'link (caar parsed)))))))

;;; ============================================================
;;; Block Parser Tests
;;; ============================================================

(test-group "markdown-block-parser"
  (define-test "parse h1"
    (let ([result (parse-markdown "# Hello\n")])
      (assert-true (right? result))
      (let ([doc (from-right result)])
        (assert-equal 'document (car doc))
        (assert-equal 'h1 (caadr doc)))))

  (define-test "parse h2"
    (let ([result (parse-markdown "## Hello\n")])
      (assert-true (right? result))
      (let ([doc (from-right result)])
        (assert-equal 'h2 (caadr doc)))))

  (define-test "parse paragraph"
    (let ([result (parse-markdown "Hello world\n")])
      (assert-true (right? result))
      (let ([doc (from-right result)])
        (assert-equal 'p (caadr doc)))))

  (define-test "parse fenced code block"
    (let ([result (parse-markdown "```scheme\n(+ 1 2)\n```\n")])
      (assert-true (right? result))
      (let ([doc (from-right result)])
        (assert-equal 'code-block (caadr doc)))))

  (define-test "parse horizontal rule"
    (let ([result (parse-markdown "---\n")])
      (assert-true (right? result))
      (let ([doc (from-right result)])
        (assert-equal 'hr (caadr doc)))))

  (define-test "parse unordered list"
    (let ([result (parse-markdown "- item 1\n- item 2\n")])
      (assert-true (right? result))
      (let ([doc (from-right result)])
        (assert-equal 'ul (caadr doc)))))

  (define-test "parse ordered list"
    (let ([result (parse-markdown "1. first\n2. second\n")])
      (assert-true (right? result))
      (let ([doc (from-right result)])
        (assert-equal 'ol (caadr doc)))))

  (define-test "parse blockquote"
    (let ([result (parse-markdown "> quoted text\n")])
      (assert-true (right? result))
      (let ([doc (from-right result)])
        (assert-equal 'blockquote (caadr doc))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group "markdown-integration"
  (define-test "parse and render heading"
    (let* ([result (parse-markdown "# Hello World\n")]
           [doc (from-right result)]
           [html (render-html doc)])
      (assert-true (string-contains? html "<h1>"))))

  (define-test "parse and render paragraph with emphasis"
    (let* ([result (parse-markdown "This is **bold** text\n")]
           [doc (from-right result)]
           [html (render-html doc)])
      (assert-true (string-contains? html "<strong>"))))

  (define-test "full document round-trip"
    (let* ([md "# Title\n\nA paragraph with `code`.\n\n```scheme\n(+ 1 2)\n```\n"]
           [result (parse-markdown md)])
      (assert-true (right? result))
      (let* ([doc (from-right result)]
             [html (render-html doc)])
        (assert-true (string-contains? html "<h1>"))
        (assert-true (string-contains? html "<code>"))
        (assert-true (string-contains? html "language-scheme"))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
