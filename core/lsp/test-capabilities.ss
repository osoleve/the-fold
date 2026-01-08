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

;;; Summary
(display "\n=======================\n")
(printf "Passed: ~a, Failed: ~a\n" tests-passed tests-failed)
(when (> tests-failed 0)
      (exit 1))
