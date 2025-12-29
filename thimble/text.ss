;;; thimble/text.ss — Text Canonicalization and Glitchling Quarantine
;;;
;;; The Fold accepts text from Outside. Outside is hostile.
;;; Glitchlings hide in encoding errors, homoglyphs, bidi markers,
;;; zero-width spaces, and other textual chaos.
;;;
;;; This is Shell code: defensive, paranoid, sanitizing.
;;;
;;; Responsibilities:
;;;   1. Validate UTF-8 encoding
;;;   2. Reject or quarantine malformed sequences
;;;   3. NFC normalize (for deterministic hashing)
;;;   4. Strip or flag dangerous invisible characters
;;;   5. Detect homoglyph attacks (confusables)
;;;
;;; Design principle: REJECT by default. Quarantine what we must keep.

;;; ============================================================
;;; Character Classification
;;; ============================================================

;;; ASCII printable range (safe baseline)
(define (ascii-printable? c)
  (let ([cp (char->integer c)])
       (and (>= cp 32) (<= cp 126))))

;;; ASCII control characters (dangerous)
(define (ascii-control? c)
  (let ([cp (char->integer c)])
       (or (< cp 32) (= cp 127))))

;;; Common whitespace we allow
(define (allowed-whitespace? c)
  (memq c '(#\space #
ewline #	ab #eturn)))

;;; Zero-width and invisible characters (Glitchling habitat)
(define (invisible-char? c)
  (let ([cp (char->integer c)])
       (or
        ;; Zero-width characters
        (= cp #x200B)   ; ZERO WIDTH SPACE
        (= cp #x200C)   ; ZERO WIDTH NON-JOINER
        (= cp #x200D)   ; ZERO WIDTH JOINER
        (= cp #xFEFF)   ; BYTE ORDER MARK / ZERO WIDTH NO-BREAK SPACE
        ;; Bidirectional control
        (= cp #x200E)   ; LEFT-TO-RIGHT MARK
        (= cp #x200F)   ; RIGHT-TO-LEFT MARK
        (= cp #x202A)   ; LEFT-TO-RIGHT EMBEDDING
        (= cp #x202B)   ; RIGHT-TO-LEFT EMBEDDING
        (= cp #x202C)   ; POP DIRECTIONAL FORMATTING
        (= cp #x202D)   ; LEFT-TO-RIGHT OVERRIDE
        (= cp #x202E)   ; RIGHT-TO-LEFT OVERRIDE
        (= cp #x2066)   ; LEFT-TO-RIGHT ISOLATE
        (= cp #x2067)   ; RIGHT-TO-LEFT ISOLATE
        (= cp #x2068)   ; FIRST STRONG ISOLATE
        (= cp #x2069)   ; POP DIRECTIONAL ISOLATE
        ;; Other invisibles
        (= cp #x00AD)   ; SOFT HYPHEN
        (= cp #x034F)   ; COMBINING GRAPHEME JOINER
        (= cp #x061C)   ; ARABIC LETTER MARK
        (= cp #x115F)   ; HANGUL CHOSEONG FILLER
        (= cp #x1160)   ; HANGUL JUNGSEONG FILLER
        (= cp #x17B4)   ; KHMER VOWEL INHERENT AQ
        (= cp #x17B5)   ; KHMER VOWEL INHERENT AA
        (and (>= cp #x180B) (<= cp #x180E))  ; Mongolian variation selectors
        (and (>= cp #xFE00) (<= cp #xFE0F))  ; Variation selectors
        (and (>= cp #xE0100) (<= cp #xE01EF))))) ; Variation selectors supplement

;;; Private Use Area (could hide anything)
(define (private-use? c)
  (let ([cp (char->integer c)])
       (or (and (>= cp #xE000) (<= cp #xF8FF))     ; BMP Private Use
           (and (>= cp #xF0000) (<= cp #xFFFFD))   ; Supplementary Private Use A
           (and (>= cp #x100000) (<= cp #x10FFFD))))) ; Supplementary Private Use B

;;; Noncharacter code points (forever reserved, never valid text)
(define (noncharacter? c)
  (let ([cp (char->integer c)])
       (or (and (>= cp #xFDD0) (<= cp #xFDEF))
           (= (bitwise-and cp #xFFFF) #xFFFE)
           (= (bitwise-and cp #xFFFF) #xFFFF))))

;;; ============================================================
;;; Text Validation
;;; ============================================================

;;; A validation result
(define-record-type text-result
  (fields ok? value warnings))

(define (text-ok value)
  (make-text-result #t value '()))

(define (text-ok-with-warnings value warnings)
  (make-text-result #t value warnings))

(define (text-error msg)
  (make-text-result #f msg '()))

;;; validate-text : String → TextResult
;;; Check text for dangerous content. Returns result with warnings.
(define (validate-text str)
  (let ([chars (string->list str)]
        [warnings '()])
       (let loop ([chars chars] [pos 0] [warns '()])
            (if (null? chars)
                (if (null? warns)
                    (text-ok str)
                    (text-ok-with-warnings str (reverse warns)))
                (let ([c (car chars)])
                     (cond
                      ;; Noncharacters are never valid
                      [(noncharacter? c)
                       (text-error (format "Noncharacter at position ~a" pos))]
                      
                      ;; Invisible chars get warnings
                      [(invisible-char? c)
                       (loop (cdr chars) (+ pos 1)
                             (cons (format "Invisible character U+~a at position ~a"
                                           (number->string (char->integer c) 16) pos)
                                   warns))]
                      
                      ;; Private use gets warnings (may be intentional)
                      [(private-use? c)
                       (loop (cdr chars) (+ pos 1)
                             (cons (format "Private use character U+~a at position ~a"
                                           (number->string (char->integer c) 16) pos)
                                   warns))]
                      
                      ;; Control chars (except allowed whitespace) are rejected
                      [(and (ascii-control? c) (not (allowed-whitespace? c)))
                       (text-error (format "Control character at position ~a" pos))]
                      
                      ;; Everything else passes
                      [else
                       (loop (cdr chars) (+ pos 1) warns)]))))))

;;; ============================================================
;;; Sanitization
;;; ============================================================

;;; strip-invisible : String → String
;;; Remove all invisible characters.
(define (strip-invisible str)
  (list->string
   (filter (lambda (c) (not (invisible-char? c)))
           (string->list str))))

;;; sanitize-text : String → String
;;; Remove invisible chars and control chars (keeping allowed whitespace).
(define (sanitize-text str)
  (list->string
   (filter
    (lambda (c)
            (and (not (invisible-char? c))
                 (or (not (ascii-control? c))
                     (allowed-whitespace? c))))
    (string->list str))))

;;; ============================================================
;;; NFC Normalization
;;; ============================================================

;;; NFC (Canonical Composition) ensures deterministic hashing.
;;; Different encodings of the same character (e.g., é as single
;;; codepoint vs e+combining-acute) normalize to the same form.
;;;
;;; We use Chez Scheme's built-in R6RS string-normalize-nfc.

;;; ascii-only? : String → Boolean
(define (ascii-only? str)
  (let loop ([chars (string->list str)])
       (or (null? chars)
           (and (< (char->integer (car chars)) 128)
                (loop (cdr chars))))))

;;; normalize-nfc : String → String
;;; Normalize text to Unicode NFC form for deterministic hashing.
;;; Uses R6RS string-normalize-nfc from Chez Scheme.
(define (normalize-nfc str)
  (string-normalize-nfc str))

;;; ============================================================
;;; Symbol Hygiene
;;; ============================================================

;;; validate-symbol : String → TextResult
;;; Stricter validation for strings that will become Scheme symbols.
(define (validate-symbol str)
  (cond
   [(= (string-length str) 0)
    (text-error "Empty symbol")]
   [(not (ascii-only? str))
    (text-error "Symbol contains non-ASCII characters")]
   [(string-any? invisible-char? str)
    (text-error "Symbol contains invisible characters")]
   [(string-any? ascii-control? str)
    (text-error "Symbol contains control characters")]
   [else
    (text-ok str)]))

(define (string-any? pred str)
  (let loop ([chars (string->list str)])
       (and (not (null? chars))
            (or (pred (car chars))
                (loop (cdr chars))))))

;;; ============================================================
;;; Homoglyph Detection (Basic)
;;; ============================================================

;;; Common confusables with ASCII (stored as code points)
;;; Each entry: (ascii-char . (confusable-codepoint ...))
(define confusable-pairs
  `((# . (#x0430))       ; Cyrillic а
    (#