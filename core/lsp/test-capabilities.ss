;;; core/lsp/test-capabilities.ss — Tests for LSP Capabilities

(load "core/lsp/capabilities.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(display "Testing capabilities.ss\n")
(display "=======================\n\n")

;;; ============================================================
;;; Find References Tests
;;; ============================================================

(display "Find References:\n")

;; Set up a test document
(doc-open! "file:///test-refs.ss" 1 "(define foo 42)\n(display foo)\n(+ foo 1)")

;; Test symbol position finding
(define test-content "(define foo 42)\n(display foo)\n(+ foo 1)")
(define positions (find-symbol-positions test-content "foo"))
(test "find-symbol-positions count" 3 (length positions))
(test "find-symbol-positions first" 8 (car positions))  ; "foo" in define

;; Test complete-symbol-match?
(test "complete-symbol-match? true" #t (complete-symbol-match? "foo bar" 0 3))
(test "complete-symbol-match? false start" #f (complete-symbol-match? "afoo bar" 1 3))
(test "complete-symbol-match? false end" #f (complete-symbol-match? "foob bar" 0 3))

;; Clean up
(doc-close! "file:///test-refs.ss")

;;; ============================================================
;;; Workspace Symbol Tests
;;; ============================================================

(display "\nWorkspace Symbol:\n")

;; Set up test documents
(doc-open! "file:///ws1.ss" 1 "(define alpha 1)\n(define beta 2)")
(doc-open! "file:///ws2.ss" 1 "(define alpha-two 3)")

;; Test workspace symbol search
(let ([results (compute-workspace-symbols "alpha")])
     (test "workspace-symbols finds matches" #t (json-array? results))
     (test "workspace-symbols count" 2 (length (cdr results))))  ; 2 matches

(let ([results (compute-workspace-symbols "beta")])
     (test "workspace-symbols beta count" 1 (length (cdr results))))

(let ([results (compute-workspace-symbols "")])
     (test "workspace-symbols empty query" 3 (length (cdr results))))  ; all 3 definitions

;; Clean up
(doc-close! "file:///ws1.ss")
(doc-close! "file:///ws2.ss")

;;; ============================================================
;;; Case-Insensitive Search Tests
;;; ============================================================

(display "\nCase-Insensitive Search:\n")

(test "string-contains-ci? match" #t (string-contains-ci? "HelloWorld" "world"))
(test "string-contains-ci? no match" #f (string-contains-ci? "hello" "xyz"))
(test "string-downcase" "hello" (string-downcase "HeLLo"))

;;; ============================================================
;;; Symbol Kind Tests
;;; ============================================================

(display "\nSymbol Kind:\n")

(test "symbol-kind->lsp-kind define" 12 (symbol-kind->lsp-kind 'define))
(test "symbol-kind->lsp-kind syntax" 14 (symbol-kind->lsp-kind 'syntax))
(test "symbol-kind->lsp-kind variable" 13 (symbol-kind->lsp-kind 'variable))

;;; ============================================================
;;; Formatting Tests (if available)
;;; ============================================================

(display "\nFormatting:\n")

(if *pretty-available*
    (begin
     ;; Test read-all-sexps
     (let ([exprs (read-all-sexps "(+ 1 2) (- 3 4)")])
          (test "read-all-sexps count" 2 (length exprs))
          (test "read-all-sexps first" '(+ 1 2) (car exprs)))
     
     ;; Test format-scheme-code
     (let ([formatted (format-scheme-code "(define x 1)" 2)])
          (test "format-scheme-code works" #t (string? formatted)))
     
     ;; Test end-of-document
     (doc-open! "file:///fmt.ss" 1 "line1\nline2")
     (let ([end-pos (end-of-document (doc-get "file:///fmt.ss"))])
          (test "end-of-document line" 1 (json-get end-pos "line"))
          (test "end-of-document char" 5 (json-get end-pos "character")))
     (doc-close! "file:///fmt.ss"))
    (begin
     (display "  (skipping - pretty printer not available)\n")))

;;; ============================================================
;;; Rename Tests
;;; ============================================================

(display "\nRename:\n")

;; Set up test documents for rename
(doc-open! "file:///rename1.ss" 1 "(define foo 42)\n(+ foo 1)")
(doc-open! "file:///rename2.ss" 1 "(display foo)")

;; Test compute-rename-edits-in-doc
(let ([edits (compute-rename-edits-in-doc "file:///rename1.ss" "foo" "bar")])
     (test "rename-edits-in-doc count" 2 (length edits))
     (test "rename-edits-in-doc is text-edit" #t
           (if (and (json-object? (car edits))
                    (json-get (car edits) "newText"))
               #t #f))
     (test "rename-edits-in-doc newText" "bar" (json-get (car edits) "newText")))

;; Test compute-rename-edits across documents
(let ([edits-by-uri (compute-rename-edits "foo" "bar")])
     (test "rename-edits uri count" 2 (length edits-by-uri)))

;; Test full compute-rename
(let* ([doc (doc-get "file:///rename1.ss")]
       [pos (make-position 0 8)]  ; Position of "foo" in define
       [result (compute-rename doc pos "bar")])
      (test "compute-rename returns object" #t (json-object? result))
      (test "compute-rename has changes" #t (if (json-get result "changes") #t #f)))

;; Clean up
(doc-close! "file:///rename1.ss")
(doc-close! "file:///rename2.ss")

;;; ============================================================
;;; Snippet Completions Tests
;;; ============================================================

(display "\nSnippet Completions:\n")

(let ([snippets (snippet-completions "def")])
     (test "snippet-completions finds matches" #t (> (length snippets) 0))
     (let ([item (car snippets)])
          (test "snippet has insertTextFormat" 2 (json-get item "insertTextFormat"))
          (test "snippet has insertText" #t (if (json-get item "insertText") #t #f))))

(let ([snippets (snippet-completions "")])
     (test "empty prefix returns all snippets" #t (>= (length snippets) 10)))

;;; ============================================================
;;; Code Action Tests
;;; ============================================================

(display "\nCode Actions:\n")

;; Test string-contains?
(test "string-contains? true" #t (string-contains? "hello world" "world"))
(test "string-contains? false" #f (string-contains? "hello world" "xyz"))

;; Test extract-undefined-name
(test "extract-undefined-name" "foo" (extract-undefined-name "undefined variable: foo"))
(test "extract-undefined-name none" #f (extract-undefined-name "some other error"))

;; Test code action generation for diagnostics
(doc-open! "file:///action-test.ss" 1 "(define x 1)")
(let* ([doc (doc-get "file:///action-test.ss")]
       [diag (json-obj "message" "undefined variable: bar"
                       "range" (make-range (make-position 0 0) (make-position 0 5)))]
       [context (json-obj "diagnostics" (json-arr diag))]
       [actions (compute-code-actions doc "file:///action-test.ss"
                                      (make-range (make-position 0 0) (make-position 0 5))
                                      context)])
      (test "code-actions returns array" #t (json-array? actions))
      (test "code-actions for undefined var" #t (> (length (cdr actions)) 0)))

(doc-close! "file:///action-test.ss")

;;; ============================================================
;;; Semantic Tokens Tests
;;; ============================================================

(display "\nSemantic Tokens:\n")

;; Test classify-symbol
(test "classify-symbol keyword" *token-keyword* (car (classify-symbol "define")))
(test "classify-symbol operator" *token-operator* (car (classify-symbol "+")))
(test "classify-symbol operator null?" *token-operator* (car (classify-symbol "null?")))  ; In operators list
(test "classify-symbol keyword set!" *token-keyword* (car (classify-symbol "set!")))      ; In keywords list
(test "classify-symbol predicate" *token-function* (car (classify-symbol "my-pred?")))    ; Ends with ?
(test "classify-symbol mutator" *token-function* (car (classify-symbol "mutate!")))       ; Ends with !
(test "classify-symbol constant" *mod-readonly* (cdr (classify-symbol "*foo*")))

;; Test tokenize-scheme
(let* ([tokens (tokenize-scheme "(define x 42) ; comment")]
       [count (length tokens)])
      (test "tokenize-scheme count" #t (> count 3))
      ;; Check that we have keyword, variable, number, comment
      (test "tokenize-scheme has tokens" #t (> count 0)))

;; Test encode-tokens
(let* ([tokens '((0 0 6 0 0) (0 7 1 2 0) (0 9 2 4 0))]  ; keyword, var, number
       [encoded (encode-tokens tokens)])
      (test "encode-tokens length" 15 (length encoded))  ; 3 tokens * 5 ints
      (test "encode-tokens first deltaLine" 0 (car encoded)))

;; Test full semantic tokens
(doc-open! "file:///semantic-test.ss" 1 "(define foo 42)")
(let* ([doc (doc-get "file:///semantic-test.ss")]
       [result (compute-semantic-tokens doc)])
      (test "semantic-tokens has data" #t (if (json-get result "data") #t #f)))
(doc-close! "file:///semantic-test.ss")

;;; ============================================================
;;; Incremental Document Sync Tests
;;; ============================================================

(display "\nIncremental Sync:\n")

;; Test apply-single-change with full replacement
(let ([result (apply-single-change "hello world" (json-obj "text" "goodbye"))])
     (test "full replacement" "goodbye" result))

;; Test apply-single-change with range
(let ([change (json-obj "range" (json-obj "start" (json-obj "line" 0 "character" 0)
                                          "end" (json-obj "line" 0 "character" 5))
                        "text" "hi")]
      [result (apply-single-change "hello world" (json-obj "range" (json-obj "start" (json-obj "line" 0 "character" 0)
                                                                             "end" (json-obj "line" 0 "character" 5))
                                                           "text" "hi"))])
     (test "range replacement" "hi world" result))

;; Test string-split-newlines
(let ([lines (string-split-newlines "a\nb\nc")])
     (test "string-split-newlines count" 3 (length lines))
     (test "string-split-newlines first" "a" (car lines)))

;; Test lines-offset
(let ([lines '("abc" "defgh" "ij")])
     (test "lines-offset 0" 0 (lines-offset lines 0))
     (test "lines-offset 1" 4 (lines-offset lines 1))   ; "abc" + \n
     (test "lines-offset 2" 10 (lines-offset lines 2))) ; "abc\n" + "defgh\n"

;;; ============================================================
;;; Hover Tests
;;; ============================================================

(display "\nHover:\n")

;; Set up test document
(doc-open! "file:///hover-test.ss" 1 "(define my-func 42)\n(+ my-func 1)")

;; Test primitive-type lookup
(test "primitive-type +" "(Int -> Int -> Int)" (primitive-type "+"))
(test "primitive-type car" "((Pair a b) -> a)" (primitive-type "car"))
(test "primitive-type cons" "(a -> b -> (Pair a b))" (primitive-type "cons"))
(test "primitive-type unknown" #f (primitive-type "unknown-prim"))

;; Test format-hover-text with type info
(let ([hover (format-hover-text "+" #f "(Int -> Int -> Int)")])
     (test "format-hover-text with type" #t (string? hover))
     (test "format-hover-text contains type" #t (string-contains? hover "Int")))

;; Test format-hover-text with no info
(let ([hover (format-hover-text "unknown-symbol" #f #f)])
     (test "format-hover-text unknown" #f hover))

;; Test compute-hover with primitive
(let* ([doc (doc-get "file:///hover-test.ss")]
       [pos (make-position 1 1)]  ; Position of +
       [result (compute-hover doc pos)])
      ;; Result should be a hover object or null
      (test "compute-hover returns value" #t (or (json-object? result) (eq? result 'null))))

;; Clean up
(doc-close! "file:///hover-test.ss")

;;; ============================================================
;;; Completion Tests
;;; ============================================================

(display "\nCompletion:\n")

;; Set up test document
(doc-open! "file:///completion-test.ss" 1 "(def")

;; Test keyword-completions
(let ([completions (keyword-completions "def")])
     (test "keyword-completions not empty" #t (> (length completions) 0))
     ;; Should include "define"
     (let ([labels (map (lambda (c) (json-get c "label")) completions)])
          (test "keyword-completions has define" #t (member "define" labels))))

(let ([completions (keyword-completions "")])
     (test "keyword-completions empty prefix" #t (> (length completions) 10)))

;; Test primitive-completions
(let ([completions (primitive-completions "ca")])
     (test "primitive-completions not empty" #t (> (length completions) 0))
     (let ([labels (map (lambda (c) (json-get c "label")) completions)])
          (test "primitive-completions has car" #t (member "car" labels))))

;; Test completion-prefix-at-offset
(let ([prefix (completion-prefix-at-offset (doc-get "file:///completion-test.ss") 4)])
     (test "completion-prefix-at-offset" "def" prefix))

;; Test find-completion-start
(test "find-completion-start at symbol end" 1 (find-completion-start "(abc" 4))
(test "find-completion-start at paren" 0 (find-completion-start "(abc" 0))
(test "find-completion-start mid-symbol" 0 (find-completion-start "foo" 2))

;; Test compute-completions
(let* ([doc (doc-get "file:///completion-test.ss")]
       [pos (make-position 0 4)]
       [result (compute-completions doc pos)])
      (test "compute-completions returns object" #t (json-object? result))
      (test "compute-completions has items" #t (if (json-get result "items") #t #f)))

;; Clean up
(doc-close! "file:///completion-test.ss")

;;; ============================================================
;;; Go-to-Definition Tests
;;; ============================================================

(display "\nGo-to-Definition:\n")

;; Set up test document
(doc-open! "file:///goto-test.ss" 1 "(define my-var 42)\n(+ my-var 1)")

;; Test compute-definition (will return null if index not available)
(let* ([doc (doc-get "file:///goto-test.ss")]
       [pos (make-position 1 3)])  ; Position of "my-var" reference
      ;; compute-definition returns null or location
      (let ([result (compute-definition doc pos)])
           (test "compute-definition returns value" #t
                 (or (eq? result 'null) (json-object? result)))))

;; Test lookup-symbol-info (depends on index availability)
(let ([info (lookup-symbol-info "unknown-symbol")])
     (test "lookup-symbol-info unknown" #f info))

;; Clean up
(doc-close! "file:///goto-test.ss")

;;; ============================================================
;;; Document Symbols Tests
;;; ============================================================

(display "\nDocument Symbols:\n")

;; Set up test document with multiple definitions
(doc-open! "file:///symbols-test.ss" 1
           "(define foo 1)\n(define (bar x) (+ x 1))\n(define-syntax my-macro\n  (syntax-rules () [(_ x) x]))")

;; Test extract-definitions
(let ([defs (extract-definitions "(define foo 1)\n(define bar 2)\n(define-syntax baz)")])
     (test "extract-definitions count" 3 (length defs))
     (test "extract-definitions first name" "foo" (car (car defs)))
     (test "extract-definitions second name" "bar" (car (cadr defs)))
     (test "extract-definitions third name" "baz" (car (caddr defs))))

;; Test definition-line?
(test "definition-line? define" #t (definition-line? "(define foo 1)"))
(test "definition-line? define-syntax" #t (definition-line? "(define-syntax bar)"))
(test "definition-line? other" #f (definition-line? "(+ 1 2)"))
(test "definition-line? indented" #t (definition-line? "  (define foo 1)"))

;; Test extract-definition-name
(test "extract-definition-name var" "foo" (extract-definition-name "(define foo 1)"))
(test "extract-definition-name fn" "bar" (extract-definition-name "(define (bar x) x)"))
(test "extract-definition-name syntax" "baz" (extract-definition-name "(define-syntax baz)"))

;; Test compute-document-symbols
(let* ([doc (doc-get "file:///symbols-test.ss")]
       [symbols (compute-document-symbols doc)])
      (test "compute-document-symbols is array" #t (json-array? symbols))
      (test "compute-document-symbols count" 3 (length (cdr symbols))))

;; Test definition->document-symbol
(let* ([doc (doc-get "file:///symbols-test.ss")]
       [def '("test-name" define 5)]
       [sym (definition->document-symbol doc def)])
      (test "definition->document-symbol name" "test-name" (json-get sym "name"))
      (test "definition->document-symbol kind" 12 (json-get sym "kind")))

;; Clean up
(doc-close! "file:///symbols-test.ss")

;;; ============================================================
;;; Signature Help Tests
;;; ============================================================

(display "\nSignature Help:\n")

;; Test lookup-signature
(let ([sig (lookup-signature "map")])
     (test "lookup-signature map found" #t (if sig #t #f))
     (test "lookup-signature map label" "(map f lst)" (cadr sig)))

(let ([sig (lookup-signature "unknown-function")])
     (test "lookup-signature unknown" #f sig))

;; Test find-enclosing-call
(doc-open! "file:///sig-test.ss" 1 "(map foo bar)")
(let* ([doc (doc-get "file:///sig-test.ss")]
       [call-info (find-enclosing-call doc 5)])  ; Position inside "map"
      (if call-info
          (begin
           (test "find-enclosing-call fn name" "map" (car call-info))
           (test "find-enclosing-call param idx" 0 (cdr call-info)))
          (test "find-enclosing-call found" #t #f)))

;; Test extract-symbol-at
(test "extract-symbol-at basic" "foo" (extract-symbol-at "  foo bar" 2))
(test "extract-symbol-at whitespace" "bar" (extract-symbol-at "  bar" 0))

;; Clean up
(doc-close! "file:///sig-test.ss")

;;; ============================================================
;;; String Utility Tests
;;; ============================================================

(display "\nString Utilities:\n")

;; Test string-prefix?
(test "string-prefix? true" #t (string-prefix? "define" "def"))
(test "string-prefix? false" #f (string-prefix? "define" "xyz"))
(test "string-prefix? exact" #t (string-prefix? "foo" "foo"))
(test "string-prefix? empty" #t (string-prefix? "anything" ""))

;; Test string-suffix?
(test "string-suffix? true" #t (string-suffix? "hello?" "?"))
(test "string-suffix? false" #f (string-suffix? "hello" "?"))
(test "string-suffix? exact" #t (string-suffix? "foo" "foo"))

;; Test symbol-char?
(test "symbol-char? letter" #t (symbol-char? #\a))
(test "symbol-char? digit" #t (symbol-char? #\5))
(test "symbol-char? hyphen" #t (symbol-char? #\-))
(test "symbol-char? question" #t (symbol-char? #\?))
(test "symbol-char? bang" #t (symbol-char? #\!))
(test "symbol-char? paren" #f (symbol-char? #\())
(test "symbol-char? space" #f (symbol-char? #\ ))

;; Test string-trim-left
(test "string-trim-left basic" "hello" (string-trim-left "  hello"))
(test "string-trim-left no trim" "hello" (string-trim-left "hello"))
(test "string-trim-left all spaces" "" (string-trim-left "   "))

;;; ============================================================
;;; Tokenizer Helpers Tests
;;; ============================================================

(display "\nTokenizer Helpers:\n")

;; Test find-line-end
(test "find-line-end basic" 5 (find-line-end "hello\nworld" 0))
(test "find-line-end no newline" 5 (find-line-end "hello" 0))

;; Test find-string-end
(test "find-string-end basic" 6 (find-string-end "hello\"rest" 0))
(test "find-string-end with escape" 8 (find-string-end "he\\\"lo\"rest" 0))
(test "find-string-end unclosed" #f (find-string-end "hello" 0))

;; Test find-number-end
(test "find-number-end int" 3 (find-number-end "123abc" 0))
(test "find-number-end float" 5 (find-number-end "12.34abc" 0))
(test "find-number-end negative" 4 (find-number-end "-123abc" 0))

;;; Summary
(display "\n=======================\n")
(printf "Passed: ~a, Failed: ~a\n" tests-passed tests-failed)
(when (> tests-failed 0)
      (exit 1))
