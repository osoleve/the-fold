(load "core/testing/test-framework.ss")
(load "boundary/lsp/capabilities.ss")

(doc 'module 'lsp/test-capabilities)
(doc 'description "Tests for LSP Capabilities")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "Testing capabilities.ss\n")
(display "====\n\n")

;;; ====
;;; Find References Tests
;;; ====

;; Set up a test document
(doc-open! "file:///test-refs.ss" 1 "(define foo 42)\n(display foo)\n(+ foo 1)")
(define test-content "(define foo 42)\n(display foo)\n(+ foo 1)")

(test-group find-references
  (define-test find-symbol-positions-count
    (let ([positions (find-symbol-positions test-content "foo")])
      (assert-equal 3 (length positions))))

  (define-test find-symbol-positions-first
    (let ([positions (find-symbol-positions test-content "foo")])
      (assert-equal 8 (car positions))))

  (define-test complete-symbol-match-true
    (assert-true (complete-symbol-match? "foo bar" 0 3)))

  (define-test complete-symbol-match-false-start
    (assert-false (complete-symbol-match? "afoo bar" 1 3)))

  (define-test complete-symbol-match-false-end
    (assert-false (complete-symbol-match? "foob bar" 0 3)))

  (define-test find-symbol-positions-excludes-comment
    (assert-equal '(0) (find-symbol-positions "foo ; foo in comment" "foo")))

  (define-test find-symbol-positions-excludes-string
    (assert-equal '(0) (find-symbol-positions "foo \"contains foo\"" "foo")))

  (define-test find-symbol-positions-mixed
    (assert-equal '(0 24) (find-symbol-positions "bar ; bar comment\n\"bar\" bar" "bar"))))

;; Clean up
(doc-close! "file:///test-refs.ss")

;;; ====
;;; Workspace Symbol Tests
;;; ====

;; Set up test documents
(doc-open! "file:///ws1.ss" 1 "(define alpha 1)\n(define beta 2)")
(doc-open! "file:///ws2.ss" 1 "(define alpha-two 3)")

(test-group workspace-symbols
  (define-test workspace-symbols-finds-matches
    (let ([results (compute-workspace-symbols "alpha")])
      (assert-true (json-array? results))))

  (define-test workspace-symbols-count
    (let ([results (compute-workspace-symbols "alpha")])
      (assert-equal 2 (length (cdr results)))))

  (define-test workspace-symbols-beta-count
    (let ([results (compute-workspace-symbols "beta")])
      (assert-equal 1 (length (cdr results)))))

  (define-test workspace-symbols-empty-query
    (let ([results (compute-workspace-symbols "")])
      (assert-equal 3 (length (cdr results))))))

;; Clean up
(doc-close! "file:///ws1.ss")
(doc-close! "file:///ws2.ss")

;;; ====
;;; Case-Insensitive Search Tests
;;; ====

(test-group case-insensitive-search
  (define-test string-contains-ci-match
    (assert-true (string-contains-ci? "HelloWorld" "world")))

  (define-test string-contains-ci-no-match
    (assert-false (string-contains-ci? "hello" "xyz")))

  (define-test string-downcase
    (assert-equal "hello" (string-downcase "HeLLo"))))

;;; ====
;;; Symbol Kind Tests
;;; ====

(test-group symbol-kind
  (define-test symbol-kind-define
    (assert-equal 12 (symbol-kind->lsp-kind 'define)))

  (define-test symbol-kind-syntax
    (assert-equal 14 (symbol-kind->lsp-kind 'syntax)))

  (define-test symbol-kind-variable
    (assert-equal 13 (symbol-kind->lsp-kind 'variable))))

;;; ====
;;; Formatting Tests
;;; ====

(test-group formatting
  (define-test read-all-sexps-count
    (if *pretty-available*
        (let ([exprs (read-all-sexps-from-string "(+ 1 2) (- 3 4)")])
          (assert-equal 2 (length exprs)))
        (assert-true #t)))

  (define-test read-all-sexps-first
    (if *pretty-available*
        (let ([exprs (read-all-sexps-from-string "(+ 1 2) (- 3 4)")])
          (assert-equal '(+ 1 2) (car exprs)))
        (assert-true #t)))

  (define-test format-scheme-code-works
    (if *pretty-available*
        (let ([formatted (format-scheme-code "(define x 1)" 2)])
          (assert-true (string? formatted)))
        (assert-true #t)))

  (define-test end-of-document-line
    (if *pretty-available*
        (begin
          (doc-open! "file:///fmt.ss" 1 "line1\nline2")
          (let ([end-pos (end-of-document (doc-get "file:///fmt.ss"))])
            (doc-close! "file:///fmt.ss")
            (assert-equal 1 (json-get end-pos "line"))))
        (assert-true #t)))

  (define-test end-of-document-char
    (if *pretty-available*
        (begin
          (doc-open! "file:///fmt2.ss" 1 "line1\nline2")
          (let ([end-pos (end-of-document (doc-get "file:///fmt2.ss"))])
            (doc-close! "file:///fmt2.ss")
            (assert-equal 5 (json-get end-pos "character"))))
        (assert-true #t))))

;;; ====
;;; Rename Tests
;;; ====

;; Set up test documents for rename
(doc-open! "file:///rename1.ss" 1 "(define foo 42)\n(+ foo 1)")
(doc-open! "file:///rename2.ss" 1 "(display foo)")

(test-group rename
  (define-test rename-edits-in-doc-count
    (let ([edits (compute-rename-edits-in-doc "file:///rename1.ss" "foo" "bar")])
      (assert-equal 2 (length edits))))

  (define-test rename-edits-in-doc-is-text-edit
    (let ([edits (compute-rename-edits-in-doc "file:///rename1.ss" "foo" "bar")])
      (assert-true (if (and (json-object? (car edits))
                            (json-get (car edits) "newText"))
                       #t #f))))

  (define-test rename-edits-in-doc-newText
    (let ([edits (compute-rename-edits-in-doc "file:///rename1.ss" "foo" "bar")])
      (assert-equal "bar" (json-get (car edits) "newText"))))

  (define-test rename-edits-uri-count
    (let ([edits-by-uri (compute-rename-edits "foo" "bar")])
      (assert-equal 2 (length edits-by-uri))))

  (define-test compute-rename-returns-object
    (let* ([doc (doc-get "file:///rename1.ss")]
           [pos (make-position 0 8)]
           [result (compute-rename doc pos "bar")])
      (assert-true (json-object? result))))

  (define-test compute-rename-has-changes
    (let* ([doc (doc-get "file:///rename1.ss")]
           [pos (make-position 0 8)]
           [result (compute-rename doc pos "bar")])
      (assert-true (if (json-get result "changes") #t #f)))))

;; Clean up
(doc-close! "file:///rename1.ss")
(doc-close! "file:///rename2.ss")

;;; ====
;;; Scope-Aware Rename Tests
;;; ====

(doc-open! "file:///scope1.ss" 1
           "(define foo 1)\n(let ((foo 2)) foo)\nfoo")

(test-group scope-aware-rename
  (define-test binding-type-global-def
    (let* ([doc (doc-get "file:///scope1.ss")]
           [global-pos (make-position 0 8)])
      (assert-equal 'global
                    (symbol-binding-type doc (lsp-position->offset doc global-pos)))))

  (define-test binding-type-let-def
    (let* ([doc (doc-get "file:///scope1.ss")]
           [let-bind-pos (make-position 1 7)])
      (assert-equal 'local-def
                    (symbol-binding-type doc (lsp-position->offset doc let-bind-pos)))))

  (define-test scope-total-foo-count
    (let* ([doc (doc-get "file:///scope1.ss")]
           [content (document-content doc)]
           [all-foo-positions (find-symbol-positions content "foo")])
      (assert-equal 4 (length all-foo-positions))))

  (define-test scope-filtered-has-positions
    (let* ([doc (doc-get "file:///scope1.ss")]
           [content (document-content doc)]
           [all-foo-positions (find-symbol-positions content "foo")]
           [filtered (filter-shadowed-positions content "foo" all-foo-positions)])
      (assert-true (> (length filtered) 0)))))

(doc-close! "file:///scope1.ss")

;;; ====
;;; Snippet Completions Tests
;;; ====

(test-group snippet-completions
  (define-test snippet-completions-finds-matches
    (let ([snippets (snippet-completions "def")])
      (assert-true (> (length snippets) 0))))

  (define-test snippet-has-insertTextFormat
    (let* ([snippets (snippet-completions "def")]
           [item (car snippets)])
      (assert-equal 2 (json-get item "insertTextFormat"))))

  (define-test snippet-has-insertText
    (let* ([snippets (snippet-completions "def")]
           [item (car snippets)])
      (assert-true (if (json-get item "insertText") #t #f))))

  (define-test empty-prefix-returns-all-snippets
    (let ([snippets (snippet-completions "")])
      (assert-true (>= (length snippets) 10)))))

;;; ====
;;; Code Action Tests
;;; ====

(test-group code-actions
  (define-test string-contains-true
    (assert-true (string-contains? "hello world" "world")))

  (define-test string-contains-false
    (assert-false (string-contains? "hello world" "xyz")))

  (define-test extract-undefined-name
    (assert-equal "foo" (extract-undefined-name "undefined variable: foo")))

  (define-test extract-undefined-name-none
    (assert-equal #f (extract-undefined-name "some other error")))

  (define-test code-actions-returns-array
    (begin
      (doc-open! "file:///action-test.ss" 1 "(define x 1)")
      (let* ([doc (doc-get "file:///action-test.ss")]
             [diag (json-obj "message" "undefined variable: bar"
                             "range" (make-range (make-position 0 0) (make-position 0 5)))]
             [context (json-obj "diagnostics" (json-arr diag))]
             [actions (compute-code-actions doc "file:///action-test.ss"
                                            (make-range (make-position 0 0) (make-position 0 5))
                                            context)])
        (doc-close! "file:///action-test.ss")
        (assert-true (json-array? actions)))))

  (define-test code-actions-for-undefined-var
    (begin
      (doc-open! "file:///action-test2.ss" 1 "(define x 1)")
      (let* ([doc (doc-get "file:///action-test2.ss")]
             [diag (json-obj "message" "undefined variable: bar"
                             "range" (make-range (make-position 0 0) (make-position 0 5)))]
             [context (json-obj "diagnostics" (json-arr diag))]
             [actions (compute-code-actions doc "file:///action-test2.ss"
                                            (make-range (make-position 0 0) (make-position 0 5))
                                            context)])
        (doc-close! "file:///action-test2.ss")
        (assert-true (> (length (cdr actions)) 0))))))

;;; ====
;;; Semantic Tokens Tests
;;; ====

(test-group semantic-tokens
  (define-test classify-symbol-keyword
    (assert-equal *token-keyword* (car (classify-symbol "define"))))

  (define-test classify-symbol-operator
    (assert-equal *token-operator* (car (classify-symbol "+"))))

  (define-test classify-symbol-operator-null
    (assert-equal *token-operator* (car (classify-symbol "null?"))))

  (define-test classify-symbol-keyword-set
    (assert-equal *token-keyword* (car (classify-symbol "set!"))))

  (define-test classify-symbol-predicate
    (assert-equal *token-function* (car (classify-symbol "my-pred?"))))

  (define-test classify-symbol-mutator
    (assert-equal *token-function* (car (classify-symbol "mutate!"))))

  (define-test classify-symbol-constant
    (assert-equal *mod-readonly* (cdr (classify-symbol "*foo*"))))

  (define-test tokenize-scheme-count
    (let* ([tokens (tokenize-scheme "(define x 42) ; comment")]
           [count (length tokens)])
      (assert-true (> count 3))))

  (define-test tokenize-scheme-has-tokens
    (let* ([tokens (tokenize-scheme "(define x 42) ; comment")]
           [count (length tokens)])
      (assert-true (> count 0))))

  (define-test encode-tokens-length
    (let* ([tokens '((0 0 6 0 0) (0 7 1 2 0) (0 9 2 4 0))]
           [encoded (encode-tokens tokens)])
      (assert-equal 15 (length encoded))))

  (define-test encode-tokens-first-deltaLine
    (let* ([tokens '((0 0 6 0 0) (0 7 1 2 0) (0 9 2 4 0))]
           [encoded (encode-tokens tokens)])
      (assert-equal 0 (car encoded))))

  (define-test semantic-tokens-has-data
    (begin
      (doc-open! "file:///semantic-test.ss" 1 "(define foo 42)")
      (let* ([doc (doc-get "file:///semantic-test.ss")]
             [result (compute-semantic-tokens doc)])
        (doc-close! "file:///semantic-test.ss")
        (assert-true (if (json-get result "data") #t #f))))))

;;; ====
;;; Incremental Document Sync Tests
;;; ====

(test-group incremental-sync
  (define-test full-replacement
    (let ([result (apply-single-change "hello world" (json-obj "text" "goodbye"))])
      (assert-equal "goodbye" result)))

  (define-test range-replacement
    (let ([result (apply-single-change "hello world"
                                       (json-obj "range" (json-obj "start" (json-obj "line" 0 "character" 0)
                                                                   "end" (json-obj "line" 0 "character" 5))
                                                 "text" "hi"))])
      (assert-equal "hi world" result)))

  (define-test string-split-newlines-count
    (let ([lines (string-split-newlines "a\nb\nc")])
      (assert-equal 3 (length lines))))

  (define-test string-split-newlines-first
    (let ([lines (string-split-newlines "a\nb\nc")])
      (assert-equal "a" (car lines))))

  (define-test lines-offset-0
    (let ([lines '("abc" "defgh" "ij")])
      (assert-equal 0 (lines-offset lines 0))))

  (define-test lines-offset-1
    (let ([lines '("abc" "defgh" "ij")])
      (assert-equal 4 (lines-offset lines 1))))

  (define-test lines-offset-2
    (let ([lines '("abc" "defgh" "ij")])
      (assert-equal 10 (lines-offset lines 2)))))

;;; ====
;;; Hover Tests
;;; ====

;; Set up test document
(doc-open! "file:///hover-test.ss" 1 "(define my-func 42)\n(+ my-func 1)")

(test-group hover
  (define-test primitive-type-plus
    (assert-equal "(Int → Int → Int)" (primitive-type "+")))

  (define-test primitive-type-car
    (assert-equal "((Pair α β) → α)" (primitive-type "car")))

  (define-test primitive-type-cons
    (assert-equal "(α → β → (Pair α β))" (primitive-type "cons")))

  (define-test primitive-type-unknown
    (assert-equal #f (primitive-type "unknown-prim")))

  (define-test format-hover-text-with-type
    (let ([hover (format-hover-text "+" #f "(Int -> Int -> Int)" #f)])
      (assert-true (string? hover))))

  (define-test format-hover-text-contains-type
    (let ([hover (format-hover-text "+" #f "(Int -> Int -> Int)" #f)])
      (assert-true (string-contains? hover "Int"))))

  (define-test format-hover-text-with-type-and-desc
    (let ([hover (format-hover-text "add" #f "(Int -> Int -> Int)" "Adds two numbers")])
      (assert-true (string? hover))))

  (define-test format-hover-text-contains-desc
    (let ([hover (format-hover-text "add" #f "(Int -> Int -> Int)" "Adds two numbers")])
      (assert-true (string-contains? hover "Adds two numbers"))))

  (define-test format-hover-text-unknown
    (let ([hover (format-hover-text "unknown-symbol" #f #f #f)])
      (assert-equal #f hover)))

  (define-test compute-hover-returns-value
    (let* ([doc (doc-get "file:///hover-test.ss")]
           [pos (make-position 1 1)]
           [result (compute-hover doc pos)])
      (assert-true (or (json-object? result) (eq? result 'null))))))

;; Clean up
(doc-close! "file:///hover-test.ss")

;;; ====
;;; Completion Tests
;;; ====

;; Set up test document
(doc-open! "file:///completion-test.ss" 1 "(def")

(test-group completion
  (define-test keyword-completions-not-empty
    (let ([completions (keyword-completions "def")])
      (assert-true (> (length completions) 0))))

  (define-test keyword-completions-has-define
    (let* ([completions (keyword-completions "def")]
           [labels (map (lambda (c) (json-get c "label")) completions)])
      (assert-true (if (member "define" labels) #t #f))))

  (define-test keyword-completions-empty-prefix
    (let ([completions (keyword-completions "")])
      (assert-true (> (length completions) 10))))

  (define-test primitive-completions-not-empty
    (let ([completions (primitive-completions "ca")])
      (assert-true (> (length completions) 0))))

  (define-test primitive-completions-has-car
    (let* ([completions (primitive-completions "ca")]
           [labels (map (lambda (c) (json-get c "label")) completions)])
      (assert-true (if (member "car" labels) #t #f))))

  (define-test completion-prefix-at-offset
    (let ([prefix (completion-prefix-at-offset (doc-get "file:///completion-test.ss") 4)])
      (assert-equal "def" prefix)))

  (define-test find-completion-start-at-symbol-end
    (assert-equal 1 (find-completion-start "(abc" 4)))

  (define-test find-completion-start-at-paren
    (assert-equal 0 (find-completion-start "(abc" 0)))

  (define-test find-completion-start-mid-symbol
    (assert-equal 0 (find-completion-start "foo" 2)))

  (define-test compute-completions-returns-object
    (let* ([doc (doc-get "file:///completion-test.ss")]
           [pos (make-position 0 4)]
           [result (compute-completions doc pos)])
      (assert-true (json-object? result))))

  (define-test compute-completions-has-items
    (let* ([doc (doc-get "file:///completion-test.ss")]
           [pos (make-position 0 4)]
           [result (compute-completions doc pos)])
      (assert-true (if (json-get result "items") #t #f)))))

;; Clean up
(doc-close! "file:///completion-test.ss")

;;; ====
;;; Go-to-Definition Tests
;;; ====

;; Set up test document
(doc-open! "file:///goto-test.ss" 1 "(define my-var 42)\n(+ my-var 1)")

(test-group go-to-definition
  (define-test compute-definition-returns-value
    (let* ([doc (doc-get "file:///goto-test.ss")]
           [pos (make-position 1 3)]
           [result (compute-definition doc pos)])
      (assert-true (or (eq? result 'null) (json-object? result)))))

  (define-test lookup-symbol-info-unknown
    (let ([info (lookup-symbol-info "unknown-symbol")])
      (assert-equal #f info))))

;; Clean up
(doc-close! "file:///goto-test.ss")

;;; ====
;;; Document Symbols Tests
;;; ====

;; Set up test document with multiple definitions
(doc-open! "file:///symbols-test.ss" 1
           "(define foo 1)\n(define (bar x) (+ x 1))\n(define-syntax my-macro\n  (syntax-rules () [(_ x) x]))")

(test-group document-symbols
  (define-test extract-definitions-count
    (let ([defs (extract-definitions "(define foo 1)\n(define bar 2)\n(define-syntax baz)")])
      (assert-equal 3 (length defs))))

  (define-test extract-definitions-first-name
    (let ([defs (extract-definitions "(define foo 1)\n(define bar 2)\n(define-syntax baz)")])
      (assert-equal "foo" (car (car defs)))))

  (define-test extract-definitions-second-name
    (let ([defs (extract-definitions "(define foo 1)\n(define bar 2)\n(define-syntax baz)")])
      (assert-equal "bar" (car (cadr defs)))))

  (define-test extract-definitions-third-name
    (let ([defs (extract-definitions "(define foo 1)\n(define bar 2)\n(define-syntax baz)")])
      (assert-equal "baz" (car (caddr defs)))))

  (define-test definition-line-define
    (assert-true (definition-line? "(define foo 1)")))

  (define-test definition-line-define-syntax
    (assert-true (definition-line? "(define-syntax bar)")))

  (define-test definition-line-other
    (assert-false (definition-line? "(+ 1 2)")))

  (define-test definition-line-indented
    (assert-true (definition-line? "  (define foo 1)")))

  (define-test extract-definition-name-var
    (assert-equal "foo" (extract-definition-name "(define foo 1)")))

  (define-test extract-definition-name-fn
    (assert-equal "bar" (extract-definition-name "(define (bar x) x)")))

  (define-test extract-definition-name-syntax
    (assert-equal "baz" (extract-definition-name "(define-syntax baz)")))

  (define-test compute-document-symbols-is-array
    (let* ([doc (doc-get "file:///symbols-test.ss")]
           [symbols (compute-document-symbols doc)])
      (assert-true (json-array? symbols))))

  (define-test compute-document-symbols-count
    (let* ([doc (doc-get "file:///symbols-test.ss")]
           [symbols (compute-document-symbols doc)])
      (assert-equal 3 (length (cdr symbols)))))

  (define-test definition->document-symbol-name
    (let* ([doc (doc-get "file:///symbols-test.ss")]
           [def '("test-name" define 5)]
           [sym (definition->document-symbol doc def)])
      (assert-equal "test-name" (json-get sym "name"))))

  (define-test definition->document-symbol-kind
    (let* ([doc (doc-get "file:///symbols-test.ss")]
           [def '("test-name" define 5)]
           [sym (definition->document-symbol doc def)])
      (assert-equal 12 (json-get sym "kind")))))

;; Clean up
(doc-close! "file:///symbols-test.ss")

;;; ====
;;; Signature Help Tests
;;; ====

(test-group signature-help
  (define-test lookup-signature-map-found
    (let ([sig (lookup-signature "map")])
      (assert-true (if sig #t #f))))

  (define-test lookup-signature-map-label
    (let ([sig (lookup-signature "map")])
      (assert-equal "(map f lst)" (cadr sig))))

  (define-test lookup-signature-unknown
    (let ([sig (lookup-signature "unknown-function")])
      (assert-equal #f sig)))

  (define-test find-enclosing-call-fn-name
    (begin
      (doc-open! "file:///sig-test.ss" 1 "(map foo bar)")
      (let* ([doc (doc-get "file:///sig-test.ss")]
             [call-info (find-enclosing-call doc 5)])
        (doc-close! "file:///sig-test.ss")
        (if call-info
            (assert-equal "map" (car call-info))
            (assert-true #f)))))

  (define-test find-enclosing-call-param-idx
    (begin
      (doc-open! "file:///sig-test2.ss" 1 "(map foo bar)")
      (let* ([doc (doc-get "file:///sig-test2.ss")]
             [call-info (find-enclosing-call doc 5)])
        (doc-close! "file:///sig-test2.ss")
        (if call-info
            (assert-equal 0 (cdr call-info))
            (assert-true #f)))))

  (define-test extract-symbol-at-basic
    (assert-equal "foo" (extract-symbol-at "  foo bar" 2)))

  (define-test extract-symbol-at-whitespace
    (assert-equal "bar" (extract-symbol-at "  bar" 0))))

;;; ====
;;; String Utility Tests
;;; ====

(test-group string-utilities
  (define-test string-prefix-true
    (assert-true (string-prefix? "define" "def")))

  (define-test string-prefix-false
    (assert-false (string-prefix? "define" "xyz")))

  (define-test string-prefix-exact
    (assert-true (string-prefix? "foo" "foo")))

  (define-test string-prefix-empty
    (assert-true (string-prefix? "anything" "")))

  (define-test string-suffix-true
    (assert-true (string-suffix? "hello?" "?")))

  (define-test string-suffix-false
    (assert-false (string-suffix? "hello" "?")))

  (define-test string-suffix-exact
    (assert-true (string-suffix? "foo" "foo")))

  (define-test symbol-char-letter
    (assert-true (symbol-char? #\a)))

  (define-test symbol-char-digit
    (assert-true (symbol-char? #\5)))

  (define-test symbol-char-hyphen
    (assert-true (symbol-char? #\-)))

  (define-test symbol-char-question
    (assert-true (symbol-char? #\?)))

  (define-test symbol-char-bang
    (assert-true (symbol-char? #\!)))

  (define-test symbol-char-paren
    (assert-false (symbol-char? #\()))

  (define-test symbol-char-space
    (assert-false (symbol-char? #\ )))

  (define-test string-trim-left-basic
    (assert-equal "hello" (string-trim-left "  hello")))

  (define-test string-trim-left-no-trim
    (assert-equal "hello" (string-trim-left "hello")))

  (define-test string-trim-left-all-spaces
    (assert-equal "" (string-trim-left "   "))))

;;; ====
;;; Tokenizer Helpers Tests
;;; ====

(test-group tokenizer-helpers
  (define-test find-line-end-basic
    (assert-equal 5 (find-line-end "hello\nworld" 0)))

  (define-test find-line-end-no-newline
    (assert-equal 5 (find-line-end "hello" 0)))

  (define-test find-string-end-basic
    (assert-equal 6 (find-string-end "hello\"rest" 0)))

  (define-test find-string-end-with-escape
    (assert-equal 7 (find-string-end "he\\\"lo\"rest" 0)))

  (define-test find-string-end-unclosed
    (assert-equal #f (find-string-end "hello" 0)))

  (define-test find-number-end-int
    (assert-equal 3 (find-number-end "123abc" 0)))

  (define-test find-number-end-float
    (assert-equal 5 (find-number-end "12.34abc" 0)))

  (define-test find-number-end-negative
    (assert-equal 4 (find-number-end "-123abc" 0))))

;;; ====
;;; Parameter Flattening Tests
;;; ====

(test-group parameter-flattening
  (define-test flatten-params-proper-list
    (assert-equal '(a b c) (flatten-params '(a b c))))

  (define-test flatten-params-dotted-tail
    (assert-equal '(a b rest) (flatten-params '(a b . rest))))

  (define-test flatten-params-single-symbol
    (assert-equal '(args) (flatten-params 'args)))

  (define-test flatten-params-empty
    (assert-equal '() (flatten-params '())))

  (define-test flatten-params-single-element
    (assert-equal '(x) (flatten-params '(x)))))

;;; ====
;;; Multi-line String/Comment Detection Tests
;;; ====

(define multiline-str "\"line1\nline2\nline3\"")
(define multiline-with-semi "\"line1\n; fake comment\nline3\"")
(define multiline-code "foo\n\"bar\nfoo\nbaz\"\nfoo")

(test-group multiline-string-comment-detection
  (define-test inside-string-outside
    (assert-false (inside-string? "foo bar" 3)))

  (define-test inside-string-inside
    (assert-true (inside-string? "foo \"bar\" baz" 6)))

  (define-test inside-string-at-quote
    (assert-false (inside-string? "foo \"bar\" baz" 4)))

  (define-test inside-string-multiline-line1
    (assert-true (inside-string? multiline-str 3)))

  (define-test inside-string-multiline-line2
    (assert-true (inside-string? multiline-str 10)))

  (define-test inside-string-multiline-line3
    (assert-true (inside-string? multiline-str 16)))

  (define-test inside-string-escaped-quote
    (assert-true (inside-string? "\"foo\\\"bar\"" 6)))

  (define-test inside-comment-in-comment
    (assert-true (inside-comment? "foo ; comment" 8)))

  (define-test inside-comment-before-comment
    (assert-false (inside-comment? "foo ; comment" 2)))

  (define-test inside-comment-semicolon-in-string
    (assert-false (inside-comment? "\"foo ; bar\"" 6)))

  (define-test inside-comment-multiline-semicolon-in-string
    (assert-false (inside-comment? multiline-with-semi 10)))

  (define-test find-symbol-positions-multiline-string
    (assert-equal '(0 18) (find-symbol-positions multiline-code "foo"))))

;;; ====
;;; Let Initializer Binding Tests
;;; ====

(define let-init-form '(define (test-fn outer-var)
                         (let ([x (+ outer-var 1)])
                           x)))

(define let*-form '(define (test-fn p)
                     (let* ([x (+ p 1)]
                            [y (* x 2)])
                       (+ x y))))

(define let-form '(define (test-fn p)
                    (let ([x (+ p 1)]
                          [y (+ x 2)])
                      (+ x y))))

(define letrec-form '(define (test-fn)
                       (letrec ([even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))]
                                [odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))])
                         (even? 4))))

(test-group let-initializer-bindings
  (define-test let-initializer-finds-param
    (assert-equal '((outer-var . param))
                  (extract-local-bindings let-init-form 'outer-var)))

  (define-test let*-sequential-scoping
    (let ([bindings (extract-local-bindings let*-form 'x)])
      (assert-true (if (assq 'x bindings) #t #f))))

  (define-test let-parallel-binding
    (let ([bindings (extract-local-bindings let-form 'x)])
      (assert-false (if (assq 'x bindings) #t #f))))

  (define-test letrec-mutual-recursion
    (let ([bindings (extract-local-bindings letrec-form 'odd?)])
      (assert-true (if (assq 'odd? bindings) #t #f)))))

;;; ====
;;; Run Tests
;;; ====

(print-summary)
(when (> *tests-failed* 0)
  (exit 1))
