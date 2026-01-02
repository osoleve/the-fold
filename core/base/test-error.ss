;;; fabric/stitches/test-error.ss — Tests for the unified error system

(load "core/base/error.ss")

(display "
")
(display "╔══════════════════════════════════════════════════════════════════╗
")
(display "║                   Error System Tests                              ║
")
(display "╚══════════════════════════════════════════════════════════════════╝
")
(display "
")

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(define *pass* 0)
(define *fail* 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! *pass* (+ *pass* 1))
       (display "✓
"))
      (begin
       (set! *fail* (+ *fail* 1))
       (display "✗
")
       (display "    expected: ") (display expected) (newline)
       (display "    actual:   ") (display actual) (newline))))

(define (test-contains name substring actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (and (string? actual)
           (string? substring)
           (string-contains actual substring))
      (begin
       (set! *pass* (+ *pass* 1))
       (display "✓
"))
      (begin
       (set! *fail* (+ *fail* 1))
       (display "✗
")
       (display "    expected to contain: ") (display substring) (newline)
       (display "    actual: ") (display actual) (newline))))

(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))]))))

;;; ============================================================
;;; Error Construction Tests
;;; ============================================================

(display "Error Construction
")
(display "──────────────────
")

(let ([err (make-error 'infer 'unbound-variable no-span 'x)])
     (test "error? recognizes error" #t (error? err))
     (test "error-phase" 'infer (error-phase err))
     (test "error-code" 'unbound-variable (error-code err))
     (test "error-details" '(x) (error-details err)))

(test "error? rejects non-error" #f (error? '(ok 42)))
(test "error? rejects short list" #f (error? '(error foo)))

(display "
")

;;; ============================================================
;;; Span Tests
;;; ============================================================

(display "Source Spans
")
(display "────────────
")

(let ([s (make-span "test.ss" 10 5 10 15)])
     (test "span? recognizes span" #t (span? s))
     (test "span-file" "test.ss" (span-file s))
     (test "span-line" 10 (span-line s))
     (test "span-column" 5 (span-column s)))

(test "span? rejects non-span" #f (span? '(not a span)))
(test "no-span is a span" #t (span? no-span))

(display "
")

;;; ============================================================
;;; Error Message Lookup Tests
;;; ============================================================

(display "Message Lookup
")
(display "──────────────
")

(test "infer unbound message" "Variable is not defined"
      (lookup-error-message 'infer 'unbound-variable))

(test "infer type-mismatch message" "Types do not match"
      (lookup-error-message 'infer 'type-mismatch))

(test "eval division-by-zero message" "Division by zero"
      (lookup-error-message 'eval 'division-by-zero))

(test "unknown code falls back to symbol" "unknown-code"
      (lookup-error-message 'infer 'unknown-code))

(display "
")

;;; ============================================================
;;; Error Formatting Tests
;;; ============================================================

(display "Error Formatting
")
(display "────────────────
")

(let* ([err (make-error 'infer 'unbound-variable no-span 'foo)]
       [formatted (format-error err)])
      (test-contains "formatted has phase" "[infer]" formatted)
      (test-contains "formatted has message" "Variable is not defined" formatted)
      (test-contains "formatted has variable" "'foo'" formatted))

(let* ([span (make-span "test.ss" 5 10 5 15)]
       [err (make-error 'infer 'type-mismatch span 'Int 'Bool)]
       [formatted (format-error err)])
      (test-contains "type error has location" "test.ss:5:10" formatted)
      (test-contains "type error has expected" "expected: Int" formatted)
      (test-contains "type error has actual" "actual:   Bool" formatted))

(display "
")

;;; ============================================================
;;; Suggestion Tests
;;; ============================================================

(display "Suggestions
")
(display "───────────
")

(let ([suggestion (get-suggestion 'infer 'unknown-primitive '(+))])
     (test-contains "suggests add for +" "add" suggestion))

(let ([suggestion (get-suggestion 'infer 'unbound-variable '(lambda))])
     (test-contains "suggests fn for lambda" "fn" suggestion))

(let ([suggestion (get-suggestion 'parse 'unclosed-list '())])
     (test-contains "suggests counting parens" "parentheses" suggestion))

(display "
")

;;; ============================================================
;;; Edit Distance Tests
;;; ============================================================

(display "Edit Distance
")
(display "─────────────
")

(test "same string = 0" 0 (edit-distance "hello" "hello"))
(test "one char diff = 1" 1 (edit-distance "hello" "hallo"))
(test "one insertion = 1" 1 (edit-distance "hello" "helloo"))
(test "one deletion = 1" 1 (edit-distance "hello" "helo"))
(test "transposition = 2" 2 (edit-distance "hello" "hlelo"))
(test "completely different" 4 (edit-distance "hello" "world"))

(display "
")

;;; ============================================================
;;; Helper Function Tests
;;; ============================================================

(display "Helper Functions
")
(display "────────────────
")

(let ([err (unbound-error 'myvar (make-span "file.ss" 1 1 1 5))])
     (test "unbound-error phase" 'infer (error-phase err))
     (test "unbound-error code" 'unbound-variable (error-code err)))

(let ([err (type-error 'Int 'String no-span)])
     (test "type-error has Int" 'Int (car (error-details err)))
     (test "type-error has String" 'String (cadr (error-details err))))

(display "
")

;;; ============================================================
;;; String Split Tests
;;; ============================================================

(display "String Split
")
(display "────────────
")

(test "split simple" '("a" "b" "c") (string-split "a,b,c" #\,))
(test "split by newline" '("line1" "line2") (string-split "line1\nline2" #\newline))
(test "split no delim" '("hello") (string-split "hello" #\,))

(display "
")

;;; ============================================================
;;; Summary
;;; ============================================================

(display "────────────────────────────────────────────────────────
")
(display (format "  Total: ~a tests
" (+ *pass* *fail*)))
(display (format "  Passed: ~a
" *pass*))
(display (format "  Failed: ~a
" *fail*))
(display "────────────────────────────────────────────────────────
")

(if (= *fail* 0)
    (display "✓ All error system tests passed!
")
    (display "✗ Some tests failed.
"))
