;;; Test harness for shell/text.ss

(load "C:/Users/andre/Documents/ccverse/thimble/text.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
        (display "✗\n    expected: ")
        (display expected)
        (display "\n    got: ")
        (display actual)))
  (newline))

(define (test-pred name pred val)
  (display "  ")
  (display name)
  (display ": ")
  (if (pred val)
      (display "✓")
      (display "✗"))
  (newline))

;;; Helper to check that a thunk raises an error
(define (raises? thunk)
  (guard (exn [else #t])
    (thunk)
    #f))

(display "Text Hygiene Tests\n")
(display "==================\n\n")

;;; Test 1: Clean ASCII text passes
(display "Test 1: Clean ASCII text\n")
(let ([result (validate-text "Hello, World!")])
  (test "ok?" #t (text-result-ok? result))
  (test "value" "Hello, World!" (text-result-value result))
  (test "no warnings" '() (text-result-warnings result)))

;;; Test 2: Allowed whitespace
(display "\nTest 2: Allowed whitespace\n")
(let ([result (validate-text "line1\nline2\ttabbed")])
  (test "ok?" #t (text-result-ok? result)))

;;; Test 3: Control characters rejected
(display "\nTest 3: Control characters\n")
(let ([result (validate-text (string #\a #\nul #\b))])
  (test "rejected" #f (text-result-ok? result)))

;;; Test 4: Zero-width space detected
(display "\nTest 4: Invisible characters (ZWSP)\n")
(let* ([zwsp (integer->char #x200B)]
       [evil-str (string #\h #\e #\l zwsp #\l #\o)]
       [result (validate-text evil-str)])
  (test "ok with warnings" #t (text-result-ok? result))
  (test "has warnings" #t (> (length (text-result-warnings result)) 0)))

;;; Test 5: Strip invisible
(display "\nTest 5: Strip invisible\n")
(let* ([zwsp (integer->char #x200B)]
       [evil-str (string #\h #\e #\l zwsp #\l #\o)]
       [clean (strip-invisible evil-str)])
  (test "stripped" "hello" clean))

;;; Test 6: Bidi override warning
(display "\nTest 6: Bidi override characters\n")
(let* ([rlo (integer->char #x202E)]  ; RIGHT-TO-LEFT OVERRIDE
       [evil-str (string #\a #\b rlo #\c)]
       [result (validate-text evil-str)])
  (test "ok with warnings" #t (text-result-ok? result))
  (test "has warnings" #t (> (length (text-result-warnings result)) 0)))

;;; Test 7: Sanitize text
(display "\nTest 7: Sanitize text\n")
(let* ([zwsp (integer->char #x200B)]
       [bom (integer->char #xFEFF)]
       [messy (string bom #\h #\e zwsp #\l #\l #\o)]
       [clean (sanitize-text messy)])
  (test "sanitized" "hello" clean))

;;; Test 8: Symbol validation
(display "\nTest 8: Symbol validation\n")
(let ([result (validate-symbol "valid-symbol")])
  (test "ok" #t (text-result-ok? result)))
(let ([result (validate-symbol "")])
  (test "empty rejected" #f (text-result-ok? result)))

;;; Test 9: ASCII only check
(display "\nTest 9: ASCII-only detection\n")
(test "ascii" #t (ascii-only? "hello123"))
(test "unicode" #f (ascii-only? "héllo"))

;;; Test 10: Confusable detection
(display "\nTest 10: Homoglyph/confusable detection\n")
(test "clean ASCII" #f (contains-confusable? "hello"))
;; Cyrillic 'а' looks like Latin 'a'
(let ([cyrillic-a (integer->char #x0430)])
  (test "cyrillic а" #t (contains-confusable? (string #\h #\e #\l #\l cyrillic-a))))

;;; Test 11: Safe-text API
(display "\nTest 11: safe-text API\n")
(test "clean" "hello" (safe-text "hello"))
(let* ([zwsp (integer->char #x200B)]
       [messy (string #\h zwsp #\i)])
  (test "sanitized" "hi" (safe-text messy)))

;;; Test 12: Safe-symbol API
(display "\nTest 12: safe-symbol API\n")
(test "valid" 'hello (safe-symbol "hello"))
(test "invalid" #f (safe-symbol ""))

;;; Test 13: Private use characters
(display "\nTest 13: Private use characters\n")
(let* ([pua (integer->char #xE000)]
       [str (string #\a pua #\b)]
       [result (validate-text str)])
  (test "ok with warnings" #t (text-result-ok? result))
  (test "has PUA warning" #t (> (length (text-result-warnings result)) 0)))

;;; Test 14: Noncharacters rejected
(display "\nTest 14: Noncharacter rejection\n")
(let* ([nonchar (integer->char #xFFFE)]
       [str (string #\a nonchar #\b)]
       [result (validate-text str)])
  (test "rejected" #f (text-result-ok? result)))

;;; Test 15: NFC normalization guard
(display "\nTest 15: NFC normalization\n")
(test "normalize-nfc ascii" "hello" (normalize-nfc "hello"))
(let* ([e-acute (integer->char #x00E9)]
       [str (string #\h e-acute #\l #\l #\o)])
  (test "normalize-nfc rejects non-ASCII" #t
        (raises? (lambda () (normalize-nfc str)))))

(display "\n✓ All text hygiene tests complete.\n")
