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

;;; Summary
(display "\n=======================\n")
(printf "Passed: ~a, Failed: ~a\n" tests-passed tests-failed)
(when (> tests-failed 0)
      (exit 1))
