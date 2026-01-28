(load "core/testing/test-framework.ss")
(load "boundary/lsp/documents.ss")

(doc 'module 'lsp/test-documents)
(doc 'description "Tests for Document Management")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "Testing documents.ss\n")
(display "====\n\n")

;;; ====
;;; UTF-16 Conversion Tests
;;; ====

(test-group utf16-conversion
  ;; ASCII characters are 1 UTF-16 unit
  (define-test char-utf16-length-ascii
    (assert-equal 1 (char-utf16-length #\a)))

  (define-test char-utf16-length-space
    (assert-equal 1 (char-utf16-length #\space)))

  ;; BMP characters (U+0000 to U+FFFF) are 1 UTF-16 unit
  (define-test char-utf16-length-bmp-e-acute
    (assert-equal 1 (char-utf16-length (integer->char #x00E9))))

  (define-test char-utf16-length-bmp-chinese
    (assert-equal 1 (char-utf16-length (integer->char #x4E2D))))

  ;; Non-BMP characters (> U+FFFF) are 2 UTF-16 units (surrogate pairs)
  (define-test char-utf16-length-emoji
    (assert-equal 2 (char-utf16-length (integer->char #x1F600))))

  (define-test char-utf16-length-music-symbol
    (assert-equal 2 (char-utf16-length (integer->char #x1D11E))))

  ;; UTF-16 offset conversions
  (define-test utf16->char-ascii-only
    (assert-equal 5 (utf16-offset->char-offset "hello" 5)))

  (define-test utf16->char-with-emoji
    (assert-equal 1 (utf16-offset->char-offset "😀b" 2)))

  (define-test char->utf16-ascii-only
    (assert-equal 5 (char-offset->utf16-offset "hello" 5)))

  (define-test char->utf16-with-emoji
    (assert-equal 2 (char-offset->utf16-offset "😀b" 1))))

;;; ====
;;; Line Computation Tests
;;; ====

;; Test documents
(define test-content "line one\nline two\nline three")
(define test-doc (make-document "file:///test.ss" 1 test-content))

(test-group line-computation
  (define-test line-count-basic
    (assert-equal 3 (line-count test-doc)))

  (define-test get-line-content-0
    (assert-equal "line one" (get-line-content test-doc 0)))

  (define-test get-line-content-1
    (assert-equal "line two" (get-line-content test-doc 1)))

  (define-test get-line-content-2
    (assert-equal "line three" (get-line-content test-doc 2))))

;; Edge cases
(define empty-doc (make-document "file:///empty.ss" 1 ""))

(test-group line-computation-edge-cases
  (define-test empty-doc-line-count
    (assert-equal 1 (line-count empty-doc)))

  (define-test empty-doc-get-line-content
    (assert-equal "" (get-line-content empty-doc 0))))

;;; ====
;;; Line Ending Tests
;;; ====

;; Unix style (\n)
(define unix-doc (make-document "file:///unix.ss" 1 "line1\nline2\nline3"))

;; Windows style (\r\n)
(define windows-doc (make-document "file:///win.ss" 1 "line1\r\nline2\r\nline3"))

;; Old Mac style (\r)
(define mac-doc (make-document "file:///mac.ss" 1 "line1\rline2\rline3"))

;; Mixed line endings
(define mixed-doc (make-document "file:///mixed.ss" 1 "unix\nwindows\r\nmac\rend"))

(test-group line-endings-unix
  (define-test unix-line-count
    (assert-equal 3 (line-count unix-doc)))

  (define-test unix-line-0
    (assert-equal "line1" (get-line-content unix-doc 0)))

  (define-test unix-line-1
    (assert-equal "line2" (get-line-content unix-doc 1)))

  (define-test unix-line-2
    (assert-equal "line3" (get-line-content unix-doc 2))))

(test-group line-endings-windows
  (define-test windows-line-count
    (assert-equal 3 (line-count windows-doc)))

  (define-test windows-line-0
    (assert-equal "line1" (get-line-content windows-doc 0)))

  (define-test windows-line-1
    (assert-equal "line2" (get-line-content windows-doc 1)))

  (define-test windows-line-2
    (assert-equal "line3" (get-line-content windows-doc 2))))

(test-group line-endings-mac
  (define-test mac-line-count
    (assert-equal 3 (line-count mac-doc)))

  (define-test mac-line-0
    (assert-equal "line1" (get-line-content mac-doc 0)))

  (define-test mac-line-1
    (assert-equal "line2" (get-line-content mac-doc 1)))

  (define-test mac-line-2
    (assert-equal "line3" (get-line-content mac-doc 2))))

(test-group line-endings-mixed
  (define-test mixed-line-count
    (assert-equal 4 (line-count mixed-doc)))

  (define-test mixed-line-0
    (assert-equal "unix" (get-line-content mixed-doc 0)))

  (define-test mixed-line-1
    (assert-equal "windows" (get-line-content mixed-doc 1)))

  (define-test mixed-line-2
    (assert-equal "mac" (get-line-content mixed-doc 2)))

  (define-test mixed-line-3
    (assert-equal "end" (get-line-content mixed-doc 3))))

;;; ====
;;; Position Conversion Tests
;;; ====

(define pos-0-0 '(json-object ("line" . 0) ("character" . 0)))
(define pos-0-5 '(json-object ("line" . 0) ("character" . 5)))
(define pos-1-0 '(json-object ("line" . 1) ("character" . 0)))

(test-group position-conversion
  (define-test lsp-position->offset-0-0
    (assert-equal 0 (lsp-position->offset test-doc pos-0-0)))

  (define-test lsp-position->offset-0-5
    (assert-equal 5 (lsp-position->offset test-doc pos-0-5)))

  (define-test lsp-position->offset-1-0
    (assert-equal 9 (lsp-position->offset test-doc pos-1-0)))

  (define-test offset->lsp-position-0-line
    (let ([pos (offset->lsp-position test-doc 0)])
      (assert-equal 0 (json-get pos "line"))))

  (define-test offset->lsp-position-0-char
    (let ([pos (offset->lsp-position test-doc 0)])
      (assert-equal 0 (json-get pos "character"))))

  (define-test offset->lsp-position-9-line
    (let ([pos (offset->lsp-position test-doc 9)])
      (assert-equal 1 (json-get pos "line"))))

  (define-test offset->lsp-position-9-char
    (let ([pos (offset->lsp-position test-doc 9)])
      (assert-equal 0 (json-get pos "character")))))

;;; ====
;;; Symbol Extraction Tests
;;; ====

(define code-doc (make-document "file:///code.ss" 1 "(define foo 42)"))
(define special-doc (make-document "file:///s.ss" 1 "(string->number x)"))
(define pipe-doc (make-document "file:///p.ss" 1 "(|foo bar| x)"))
(define pipe-special-doc (make-document "file:///ps.ss" 1 "|hello world!|"))
(define multi-pipe-doc (make-document "file:///mp.ss" 1 "(|a b| |c d|)"))
(define empty-pipe-doc (make-document "file:///ep.ss" 1 "(|| x)"))
(define escaped-pipe-doc (make-document "file:///esc.ss" 1 "|a\\|b|"))
(define unbalanced-pipe-doc (make-document "file:///unbal.ss" 1 "|foo"))
(define double-escape-doc (make-document "file:///dbl.ss" 1 "|a\\\\|"))

(test-group symbol-extraction
  (define-test symbol-at-offset-start
    (assert-equal "define" (symbol-at-offset code-doc 1)))

  (define-test symbol-at-offset-middle
    (assert-equal "define" (symbol-at-offset code-doc 3)))

  (define-test symbol-at-offset-end
    (assert-equal "define" (symbol-at-offset code-doc 7)))

  (define-test symbol-at-offset-paren
    (assert-equal #f (symbol-at-offset code-doc 0)))

  (define-test symbol-at-offset-after-symbol
    (assert-equal "define" (symbol-at-offset code-doc 7)))

  (define-test symbol-at-offset-special
    (assert-equal "string->number" (symbol-at-offset special-doc 1))))

(test-group symbol-extraction-pipe-quoted
  (define-test symbol-at-offset-pipe-quoted-inside
    (assert-equal "|foo bar|" (symbol-at-offset pipe-doc 2)))

  (define-test symbol-at-offset-pipe-quoted-middle
    (assert-equal "|foo bar|" (symbol-at-offset pipe-doc 5)))

  (define-test symbol-at-offset-pipe-quoted-start
    (assert-equal "|foo bar|" (symbol-at-offset pipe-doc 1)))

  (define-test symbol-at-offset-pipe-quoted-end-pipe
    (assert-equal "|foo bar|" (symbol-at-offset pipe-doc 9)))

  (define-test symbol-at-offset-pipe-quoted-after
    (assert-equal "|foo bar|" (symbol-at-offset pipe-doc 10)))

  (define-test symbol-at-offset-pipe-quoted-special
    (assert-equal "|hello world!|" (symbol-at-offset pipe-special-doc 1)))

  (define-test symbol-at-offset-multi-pipe-first
    (assert-equal "|a b|" (symbol-at-offset multi-pipe-doc 2)))

  (define-test symbol-at-offset-multi-pipe-second
    (assert-equal "|c d|" (symbol-at-offset multi-pipe-doc 8)))

  (define-test symbol-at-offset-multi-pipe-between
    (assert-equal "|a b|" (symbol-at-offset multi-pipe-doc 6)))

  (define-test symbol-at-offset-empty-pipe
    (assert-equal "||" (symbol-at-offset empty-pipe-doc 1)))

  (define-test symbol-at-offset-empty-pipe-end
    (assert-equal "||" (symbol-at-offset empty-pipe-doc 2)))

  (define-test symbol-at-offset-escaped-pipe-inside
    (assert-equal "|a\\|b|" (symbol-at-offset escaped-pipe-doc 1)))

  (define-test symbol-at-offset-escaped-pipe-on-escape
    (assert-equal "|a\\|b|" (symbol-at-offset escaped-pipe-doc 3)))

  (define-test symbol-at-offset-escaped-pipe-end
    (assert-equal "|a\\|b|" (symbol-at-offset escaped-pipe-doc 5)))

  (define-test symbol-at-offset-unbalanced-pipe
    (assert-equal "|foo" (symbol-at-offset unbalanced-pipe-doc 2)))

  (define-test symbol-at-offset-double-escape
    (assert-equal "|a\\\\|" (symbol-at-offset double-escape-doc 2))))

;;; ====
;;; Document Store Tests
;;; ====

(test-group document-store
  (define-test doc-get-after-open
    (begin
      (doc-open! "file:///store-test.ss" 1 "test content")
      (assert-true (if (doc-get "file:///store-test.ss") #t #f))))

  (define-test doc-get-content
    (assert-equal "test content"
                  (document-content (doc-get "file:///store-test.ss"))))

  (define-test doc-get-after-update
    (begin
      (doc-update! "file:///store-test.ss" 2 "new content")
      (assert-equal "new content"
                    (document-content (doc-get "file:///store-test.ss")))))

  (define-test doc-get-after-close
    (begin
      (doc-close! "file:///store-test.ss")
      (assert-equal #f (doc-get "file:///store-test.ss")))))

;;; ====
;;; Run Tests
;;; ====

(print-summary)
(when (> *tests-failed* 0)
  (exit 1))
