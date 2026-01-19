;;; core/lsp/test-documents.ss — Tests for Document Management

(load "boundary/lsp/documents.ss")

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

(display "Testing documents.ss\n")
(display "====\n\n")

;;; ====
;;; UTF-16 Conversion Tests
;;; ====

(display "UTF-16 Conversion:\n")

;; ASCII characters are 1 UTF-16 unit
(test "char-utf16-length ASCII" 1 (char-utf16-length #\a))
(test "char-utf16-length space" 1 (char-utf16-length #\space))

;; BMP characters (U+0000 to U+FFFF) are 1 UTF-16 unit
(test "char-utf16-length BMP (é)" 1 (char-utf16-length (integer->char #x00E9)))
(test "char-utf16-length BMP (中)" 1 (char-utf16-length (integer->char #x4E2D)))

;; Non-BMP characters (> U+FFFF) are 2 UTF-16 units (surrogate pairs)
(test "char-utf16-length emoji" 2 (char-utf16-length (integer->char #x1F600)))
(test "char-utf16-length music symbol" 2 (char-utf16-length (integer->char #x1D11E)))

;; UTF-16 offset conversions
(test "utf16->char ASCII only" 5 (utf16-offset->char-offset "hello" 5))
(test "utf16->char with emoji" 1 (utf16-offset->char-offset "😀b" 2))  ; emoji is 2 units
(test "char->utf16 ASCII only" 5 (char-offset->utf16-offset "hello" 5))
(test "char->utf16 with emoji" 2 (char-offset->utf16-offset "😀b" 1))  ; char 1 starts at UTF-16 pos 2

;;; ====
;;; Line Computation Tests
;;; ====

(display "\nLine Computation:\n")

;; Create a test document
(define test-content "line one\nline two\nline three")
(define test-doc (make-document "file:///test.ss" 1 test-content))

(test "line-count" 3 (line-count test-doc))
(test "get-line-content 0" "line one" (get-line-content test-doc 0))
(test "get-line-content 1" "line two" (get-line-content test-doc 1))
(test "get-line-content 2" "line three" (get-line-content test-doc 2))

;; Edge cases
(define empty-doc (make-document "file:///empty.ss" 1 ""))
(test "empty doc line-count" 1 (line-count empty-doc))
(test "empty doc get-line-content" "" (get-line-content empty-doc 0))

;;; ====
;;; Position Conversion Tests
;;; ====

(display "\nPosition Conversion:\n")

;; LSP position to offset
(define pos-0-0 '(json-object ("line" . 0) ("character" . 0)))
(define pos-0-5 '(json-object ("line" . 0) ("character" . 5)))
(define pos-1-0 '(json-object ("line" . 1) ("character" . 0)))

(test "lsp-position->offset (0,0)" 0 (lsp-position->offset test-doc pos-0-0))
(test "lsp-position->offset (0,5)" 5 (lsp-position->offset test-doc pos-0-5))
(test "lsp-position->offset (1,0)" 9 (lsp-position->offset test-doc pos-1-0))

;; Offset to LSP position
(let ([pos (offset->lsp-position test-doc 0)])
     (test "offset->lsp-position 0 line" 0 (json-get pos "line"))
     (test "offset->lsp-position 0 char" 0 (json-get pos "character")))

(let ([pos (offset->lsp-position test-doc 9)])
     (test "offset->lsp-position 9 line" 1 (json-get pos "line"))
     (test "offset->lsp-position 9 char" 0 (json-get pos "character")))

;;; ====
;;; Symbol Extraction Tests
;;; ====

(display "\nSymbol Extraction:\n")

(define code-doc (make-document "file:///code.ss" 1 "(define foo 42)"))

;; At start of symbol
(test "symbol-at-offset start" "define" (symbol-at-offset code-doc 1))
;; In middle of symbol
(test "symbol-at-offset middle" "define" (symbol-at-offset code-doc 3))
;; At end of symbol (boundary fix test)
(test "symbol-at-offset end" "define" (symbol-at-offset code-doc 7))
;; On paren
(test "symbol-at-offset paren" #f (symbol-at-offset code-doc 0))
;; On whitespace but after symbol
(test "symbol-at-offset after symbol" "define" (symbol-at-offset code-doc 7))

;; Symbol with special characters
(define special-doc (make-document "file:///s.ss" 1 "(string->number x)"))
(test "symbol-at-offset special" "string->number" (symbol-at-offset special-doc 1))

;;; ====
;;; Document Store Tests
;;; ====

(display "\nDocument Store:\n")

;; Open document
(doc-open! "file:///store-test.ss" 1 "test content")
(test "doc-get after open" #t (if (doc-get "file:///store-test.ss") #t #f))
(test "doc-get content" "test content"
      (document-content (doc-get "file:///store-test.ss")))

;; Update document
(doc-update! "file:///store-test.ss" 2 "new content")
(test "doc-get after update" "new content"
      (document-content (doc-get "file:///store-test.ss")))

;; Close document
(doc-close! "file:///store-test.ss")
(test "doc-get after close" #f (doc-get "file:///store-test.ss"))

;;; Summary
(display "\n====\n")
(printf "Passed: ~a, Failed: ~a\n" tests-passed tests-failed)
(when (> tests-failed 0)
      (exit 1))
