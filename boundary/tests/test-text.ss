(load "boundary/ui/text.ss")

(doc 'module 'test-text)
(doc 'description "Test harness for boundary/ui/text.ss - text hygiene validation")
(doc 'layer 'boundary)
(doc 'purity 'partial)

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

(doc 'section 'helper-functions)

(doc raises? 'description "Check that a thunk raises an error")
(define (raises? thunk)
  (guard (exn [else #t])
         (thunk)
         #f))

(display "Text Hygiene Tests\n")
(display "====\n\n")

(doc 'section 'validation-tests)

(doc 'test "Clean ASCII text passes")
(display "Test 1: Clean ASCII text\n")
(let ([result (validate-text "Hello, World!")])
     (test "ok?" #t (text-result-ok? result))
     (test "value" "Hello, World!" (text-result-value result))
     (test "no warnings" '() (text-result-warnings result)))

(doc 'test "Allowed whitespace")
(display "\nTest 2: Allowed whitespace\n")
(let ([result (validate-text "line1\nline2\ttabbed")])
     (test "ok?" #t (text-result-ok? result)))

(doc 'test "Control characters rejected")
(display "\nTest 3: Control characters\n")
(let ([result (validate-text (string #\a #\nul #\b))])
     (test "rejected" #f (text-result-ok? result)))

(doc 'test "Zero-width space detected")
(display "\nTest 4: Invisible characters (ZWSP)\n")
(let* ([zwsp (integer->char #x200B)]
       [evil-str (string #\h #\e #\l zwsp #\l #\o)]
       [result (validate-text evil-str)])
      (test "ok with warnings" #t (text-result-ok? result))
      (test "has warnings" #t (> (length (text-result-warnings result)) 0)))

(doc 'test "Strip invisible")
(display "\nTest 5: Strip invisible\n")
(let* ([zwsp (integer->char #x200B)]
       [evil-str (string #\h #\e #\l zwsp #\l #\o)]
       [clean (strip-invisible evil-str)])
      (test "stripped" "hello" clean))

(doc 'test "Bidi override characters")
(display "\nTest 6: Bidi override characters\n")
(let* ([rlo (integer->char #x202E)]  ; RIGHT-TO-LEFT OVERRIDE
       [evil-str (string #\a #\b rlo #\c)]
       [result (validate-text evil-str)])
      (test "ok with warnings" #t (text-result-ok? result))
      (test "has warnings" #t (> (length (text-result-warnings result)) 0)))

(doc 'test "Sanitize text")
(display "\nTest 7: Sanitize text\n")
(let* ([zwsp (integer->char #x200B)]
       [bom (integer->char #xFEFF)]
       [messy (string bom #\h #\e zwsp #\l #\l #\o)]
       [clean (sanitize-text messy)])
      (test "sanitized" "hello" clean))

(doc 'test "Symbol validation")
(display "\nTest 8: Symbol validation\n")
(let ([result (validate-symbol "valid-symbol")])
     (test "ok" #t (text-result-ok? result)))
(let ([result (validate-symbol "")])
     (test "empty rejected" #f (text-result-ok? result)))

(doc 'test "ASCII-only detection")
(display "\nTest 9: ASCII-only detection\n")
(test "ascii" #t (ascii-only? "hello123"))
(test "unicode" #f (ascii-only? "héllo"))

(doc 'test "Homoglyph/confusable detection")
(display "\nTest 10: Homoglyph/confusable detection\n")
(test "clean ASCII" #f (contains-confusable? "hello"))
;; Cyrillic 'а' looks like Latin 'a'
(let ([cyrillic-a (integer->char #x0430)])
     (test "cyrillic а" #t (contains-confusable? (string #\h #\e #\l #\l cyrillic-a))))

(doc 'test "safe-text API")
(display "\nTest 11: safe-text API\n")
(test "clean" "hello" (safe-text "hello"))
(let* ([zwsp (integer->char #x200B)]
       [messy (string #\h zwsp #\i)])
      (test "sanitized" "hi" (safe-text messy)))

(doc 'test "safe-symbol API")
(display "\nTest 12: safe-symbol API\n")
(test "valid" 'hello (safe-symbol "hello"))
(test "invalid" #f (safe-symbol ""))

(doc 'test "Private use characters")
(display "\nTest 13: Private use characters\n")
(let* ([pua (integer->char #xE000)]
       [str (string #\a pua #\b)]
       [result (validate-text str)])
      (test "ok with warnings" #t (text-result-ok? result))
      (test "has PUA warning" #t (> (length (text-result-warnings result)) 0)))

(doc 'test "Noncharacter rejection")
(display "\nTest 14: Noncharacter rejection\n")
(let* ([nonchar (integer->char #xFFFE)]
       [str (string #\a nonchar #\b)]
       [result (validate-text str)])
      (test "rejected" #f (text-result-ok? result)))

(doc 'section 'nfc-normalization-tests)

(doc 'test "NFC normalization - ASCII passthrough")
(display "\nTest 15: NFC normalization - ASCII passthrough\n")
(test "normalize-nfc ascii" "hello" (normalize-nfc "hello"))
(test "normalize-nfc empty" "" (normalize-nfc ""))
(test "normalize-nfc numbers" "12345" (normalize-nfc "12345"))
(test "normalize-nfc mixed ascii" "Hello, World! 123" (normalize-nfc "Hello, World! 123"))

(doc 'test "NFC normalization - precomposed characters")
(display "\nTest 16: NFC normalization - precomposed characters\n")
(let* ([e-acute (integer->char #x00E9)]  ; precomposed e-acute
       [str (string #\h e-acute #\l #\l #\o)])
      (test "precomposed e-acute unchanged" str (normalize-nfc str)))
(let* ([n-tilde (integer->char #x00F1)]  ; precomposed n-tilde
       [str (string #\E #\s #\p #\a n-tilde #\a)])
      (test "precomposed n-tilde unchanged" str (normalize-nfc str)))

(doc 'test "NFC normalization - decomposed to precomposed")
(display "\nTest 17: NFC normalization - decomposed to precomposed\n")
;; e + combining acute accent -> e-acute
(let* ([base-e (integer->char #x0065)]       ; 'e'
       [combining-acute (integer->char #x0301)] ; combining acute
       [decomposed (string #\h base-e combining-acute #\l #\l #\o)]
       [e-acute (integer->char #x00E9)]
       [expected (string #\h e-acute #\l #\l #\o)])
      (test "decomposed e+acute -> precomposed" expected (normalize-nfc decomposed)))

;; a + combining ring -> a-ring
(let* ([base-a (integer->char #x0061)]       ; 'a'
       [combining-ring (integer->char #x030A)] ; combining ring above
       [decomposed (string base-a combining-ring)]
       [a-ring (integer->char #x00E5)]
       [expected (string a-ring)])
      (test "decomposed a+ring -> precomposed" expected (normalize-nfc decomposed)))

;; n + combining tilde -> n-tilde
(let* ([base-n (integer->char #x006E)]       ; 'n'
       [combining-tilde (integer->char #x0303)] ; combining tilde
       [decomposed (string base-n combining-tilde)]
       [n-tilde (integer->char #x00F1)]
       [expected (string n-tilde)])
      (test "decomposed n+tilde -> precomposed" expected (normalize-nfc decomposed)))

(doc 'test "NFC normalization - canonical equivalence")
(display "\nTest 18: NFC normalization - canonical equivalence\n")
;; Both precomposed and decomposed forms should normalize to the same result
(let* ([e-acute-precomposed (integer->char #x00E9)]
       [precomposed (string #\c #\a #\f e-acute-precomposed)]
       [base-e (integer->char #x0065)]
       [combining-acute (integer->char #x0301)]
       [decomposed (string #\c #\a #\f base-e combining-acute)])
      (test "precomposed == decomposed after NFC"
            (normalize-nfc precomposed)
            (normalize-nfc decomposed)))

(doc 'test "NFC normalization - multiple combining marks")
(display "\nTest 19: NFC normalization - multiple combining marks\n")
;; o + combining diaeresis + combining macron (should stay as combining marks)
(let* ([base-o (integer->char #x006F)]
       [combining-diaeresis (integer->char #x0308)]
       [combining-macron (integer->char #x0304)]
       [input (string base-o combining-diaeresis combining-macron)]
       ;; o-diaeresis + macron (partial composition)
       [o-diaeresis (integer->char #x00F6)]
       [expected (string o-diaeresis combining-macron)])
      (test "multiple combining marks - partial compose" expected (normalize-nfc input)))

(doc 'test "NFC normalization - uppercase variants")
(display "\nTest 20: NFC normalization - uppercase variants\n")
(let* ([base-E (integer->char #x0045)]       ; 'E'
       [combining-acute (integer->char #x0301)]
       [decomposed (string base-E combining-acute)]
       [E-acute (integer->char #x00C9)]
       [expected (string E-acute)])
      (test "uppercase decomposed E+acute -> precomposed" expected (normalize-nfc decomposed)))

(let* ([base-A (integer->char #x0041)]       ; 'A'
       [combining-grave (integer->char #x0300)]
       [decomposed (string base-A combining-grave)]
       [A-grave (integer->char #x00C0)]
       [expected (string A-grave)])
      (test "uppercase decomposed A+grave -> precomposed" expected (normalize-nfc decomposed)))

(doc 'test "NFC normalization - non-Latin scripts")
(display "\nTest 21: NFC normalization - non-Latin scripts\n")
;; Greek letters without diacritics should pass through
(let* ([alpha (integer->char #x03B1)]  ; lowercase alpha
       [beta (integer->char #x03B2)]   ; lowercase beta
       [str (string alpha beta)])
      (test "Greek letters passthrough" str (normalize-nfc str)))

;; Greek with tonos (precomposed)
(let* ([alpha-tonos (integer->char #x03AC)]  ; alpha with tonos
       [str (string alpha-tonos)])
      (test "Greek alpha-tonos unchanged" str (normalize-nfc str)))

(doc 'test "NFC normalization - combining mark ordering")
(display "\nTest 22: NFC normalization - combining mark ordering\n")
;; When multiple combining marks, they should be ordered by CCC
(let* ([base-a (integer->char #x0061)]
       [cedilla (integer->char #x0327)]       ; CCC 202
       [acute (integer->char #x0301)]         ; CCC 230
       ;; Input with marks in wrong order (acute before cedilla)
       [wrong-order (string base-a acute cedilla)]
       ;; Both should normalize to same form
       [right-order (string base-a cedilla acute)])
      ;; a + cedilla -> no precomposed form, but acute can compose with a
      ;; Actually: a + cedilla = a-ogonek? No, cedilla is different
      ;; The result depends on composition rules
      (test "combining marks reordered by CCC"
            (normalize-nfc right-order)
            (normalize-nfc wrong-order)))

(doc 'test "NFC normalization - homoglyphs unchanged")
(display "\nTest 23: NFC normalization - homoglyphs unchanged\n")
;; Cyrillic letters that look like Latin should remain Cyrillic
;; (homoglyph detection is separate from NFC)
(let* ([cyrillic-a (integer->char #x0430)]  ; Cyrillic small a
       [str (string cyrillic-a)])
      (test "Cyrillic-a stays Cyrillic" str (normalize-nfc str)))

(doc 'test "NFC normalization - extended Latin")
(display "\nTest 24: NFC normalization - extended Latin\n")
;; S with caron (decomposed)
(let* ([base-S (integer->char #x0053)]       ; 'S'
       [combining-caron (integer->char #x030C)]
       [decomposed (string base-S combining-caron)]
       [S-caron (integer->char #x0160)]
       [expected (string S-caron)])
      (test "S+caron -> S-caron" expected (normalize-nfc decomposed)))

;; Z with dot above (decomposed)
(let* ([base-z (integer->char #x007A)]       ; 'z'
       [combining-dot (integer->char #x0307)]
       [decomposed (string base-z combining-dot)]
       [z-dot (integer->char #x017C)]
       [expected (string z-dot)])
      (test "z+dot -> z-dot" expected (normalize-nfc decomposed)))

(doc 'test "NFC normalization - idempotency")
(display "\nTest 25: NFC normalization - idempotency\n")
;; Normalizing an already-NFC string should return the same string
(let* ([e-acute (integer->char #x00E9)]
       [str (string #\c #\a #\f e-acute)]
       [once (normalize-nfc str)]
       [twice (normalize-nfc once)])
      (test "NFC idempotent" once twice))

(let* ([base-e (integer->char #x0065)]
       [combining-acute (integer->char #x0301)]
       [decomposed (string base-e combining-acute)]
       [once (normalize-nfc decomposed)]
       [twice (normalize-nfc once)])
      (test "NFC idempotent (from decomposed)" once twice))

(display "\n=== All text hygiene tests complete ===\n")
