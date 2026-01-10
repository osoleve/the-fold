;;; shell/text.ss — Text Canonicalization and Glitchling Quarantine
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
;;;
;;; NOTE: Standard string utilities are provided by core/prelude.ss.
;;;       string-any? is unique to this module.

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
  (memq c '(#\space #\newline #\tab #\return)))

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

;;; Full NFC (Canonical Composition) normalization for Unicode text.
;;; Algorithm:
;;;   1. Decompose to NFD (Canonical Decomposition)
;;;   2. Sort combining marks by Canonical Combining Class (CCC)
;;;   3. Compose using canonical composition rules
;;;
;;; This ensures deterministic hashing regardless of input form.

;;; ascii-only? : String → Boolean
(define (ascii-only? str)
  (let loop ([chars (string->list str)])
       (or (null? chars)
           (and (< (char->integer (car chars)) 128)
                (loop (cdr chars))))))

;;; ------------------------------------------------------------
;;; Canonical Combining Class (CCC) Table
;;; ------------------------------------------------------------
;;; Most characters have CCC=0 (starter/base).
;;; Combining marks have CCC > 0, which determines their ordering.

(define ccc-table
  ;; Format: (codepoint . ccc)
  ;; Common combining marks
  '((#x0300 . 230)   ; COMBINING GRAVE ACCENT
    (#x0301 . 230)   ; COMBINING ACUTE ACCENT
    (#x0302 . 230)   ; COMBINING CIRCUMFLEX ACCENT
    (#x0303 . 230)   ; COMBINING TILDE
    (#x0304 . 230)   ; COMBINING MACRON
    (#x0305 . 230)   ; COMBINING OVERLINE
    (#x0306 . 230)   ; COMBINING BREVE
    (#x0307 . 230)   ; COMBINING DOT ABOVE
    (#x0308 . 230)   ; COMBINING DIAERESIS
    (#x0309 . 230)   ; COMBINING HOOK ABOVE
    (#x030A . 230)   ; COMBINING RING ABOVE
    (#x030B . 230)   ; COMBINING DOUBLE ACUTE ACCENT
    (#x030C . 230)   ; COMBINING CARON
    (#x030D . 230)   ; COMBINING VERTICAL LINE ABOVE
    (#x030E . 230)   ; COMBINING DOUBLE VERTICAL LINE ABOVE
    (#x030F . 230)   ; COMBINING DOUBLE GRAVE ACCENT
    (#x0310 . 230)   ; COMBINING CANDRABINDU
    (#x0311 . 230)   ; COMBINING INVERTED BREVE
    (#x0312 . 230)   ; COMBINING TURNED COMMA ABOVE
    (#x0313 . 230)   ; COMBINING COMMA ABOVE
    (#x0314 . 230)   ; COMBINING REVERSED COMMA ABOVE
    (#x0315 . 232)   ; COMBINING COMMA ABOVE RIGHT
    (#x0316 . 220)   ; COMBINING GRAVE ACCENT BELOW
    (#x0317 . 220)   ; COMBINING ACUTE ACCENT BELOW
    (#x0318 . 220)   ; COMBINING LEFT TACK BELOW
    (#x0319 . 220)   ; COMBINING RIGHT TACK BELOW
    (#x031A . 232)   ; COMBINING LEFT ANGLE ABOVE
    (#x031B . 216)   ; COMBINING HORN
    (#x031C . 220)   ; COMBINING LEFT HALF RING BELOW
    (#x031D . 220)   ; COMBINING UP TACK BELOW
    (#x031E . 220)   ; COMBINING DOWN TACK BELOW
    (#x031F . 220)   ; COMBINING PLUS SIGN BELOW
    (#x0320 . 220)   ; COMBINING MINUS SIGN BELOW
    (#x0321 . 202)   ; COMBINING PALATALIZED HOOK BELOW
    (#x0322 . 202)   ; COMBINING RETROFLEX HOOK BELOW
    (#x0323 . 220)   ; COMBINING DOT BELOW
    (#x0324 . 220)   ; COMBINING DIAERESIS BELOW
    (#x0325 . 220)   ; COMBINING RING BELOW
    (#x0326 . 220)   ; COMBINING COMMA BELOW
    (#x0327 . 202)   ; COMBINING CEDILLA
    (#x0328 . 202)   ; COMBINING OGONEK
    (#x0329 . 220)   ; COMBINING VERTICAL LINE BELOW
    (#x032A . 220)   ; COMBINING BRIDGE BELOW
    (#x032B . 220)   ; COMBINING INVERTED DOUBLE ARCH BELOW
    (#x032C . 220)   ; COMBINING CARON BELOW
    (#x032D . 220)   ; COMBINING CIRCUMFLEX ACCENT BELOW
    (#x032E . 220)   ; COMBINING BREVE BELOW
    (#x032F . 220)   ; COMBINING INVERTED BREVE BELOW
    (#x0330 . 220)   ; COMBINING TILDE BELOW
    (#x0331 . 220)   ; COMBINING MACRON BELOW
    (#x0332 . 220)   ; COMBINING LOW LINE
    (#x0333 . 220)   ; COMBINING DOUBLE LOW LINE
    (#x0334 . 1)     ; COMBINING TILDE OVERLAY
    (#x0335 . 1)     ; COMBINING SHORT STROKE OVERLAY
    (#x0336 . 1)     ; COMBINING LONG STROKE OVERLAY
    (#x0337 . 1)     ; COMBINING SHORT SOLIDUS OVERLAY
    (#x0338 . 1)     ; COMBINING LONG SOLIDUS OVERLAY
    (#x0339 . 220)   ; COMBINING RIGHT HALF RING BELOW
    (#x033A . 220)   ; COMBINING INVERTED BRIDGE BELOW
    (#x033B . 220)   ; COMBINING SQUARE BELOW
    (#x033C . 220)   ; COMBINING SEAGULL BELOW
    (#x033D . 230)   ; COMBINING X ABOVE
    (#x033E . 230)   ; COMBINING VERTICAL TILDE
    (#x033F . 230)   ; COMBINING DOUBLE OVERLINE
    (#x0340 . 230)   ; COMBINING GRAVE TONE MARK
    (#x0341 . 230)   ; COMBINING ACUTE TONE MARK
    (#x0342 . 230)   ; COMBINING GREEK PERISPOMENI
    (#x0343 . 230)   ; COMBINING GREEK KORONIS
    (#x0344 . 230)   ; COMBINING GREEK DIALYTIKA TONOS
    (#x0345 . 240)   ; COMBINING GREEK YPOGEGRAMMENI
    (#x0346 . 230)   ; COMBINING BRIDGE ABOVE
    (#x0347 . 220)   ; COMBINING EQUALS SIGN BELOW
    (#x0348 . 220)   ; COMBINING DOUBLE VERTICAL LINE BELOW
    (#x0349 . 220)   ; COMBINING LEFT ANGLE BELOW
    (#x034A . 230)   ; COMBINING NOT TILDE ABOVE
    (#x034B . 230)   ; COMBINING HOMOTHETIC ABOVE
    (#x034C . 230)   ; COMBINING ALMOST EQUAL TO ABOVE
    (#x034D . 220)   ; COMBINING LEFT RIGHT ARROW BELOW
    (#x034E . 220)   ; COMBINING UPWARDS ARROW BELOW
    ;; Hebrew
    (#x05B0 . 10)    ; HEBREW POINT SHEVA
    (#x05B1 . 11)    ; HEBREW POINT HATAF SEGOL
    (#x05B2 . 12)    ; HEBREW POINT HATAF PATAH
    (#x05B3 . 13)    ; HEBREW POINT HATAF QAMATS
    (#x05B4 . 14)    ; HEBREW POINT HIRIQ
    (#x05B5 . 15)    ; HEBREW POINT TSERE
    (#x05B6 . 16)    ; HEBREW POINT SEGOL
    (#x05B7 . 17)    ; HEBREW POINT PATAH
    (#x05B8 . 18)    ; HEBREW POINT QAMATS
    (#x05B9 . 19)    ; HEBREW POINT HOLAM
    (#x05BB . 20)    ; HEBREW POINT QUBUTS
    (#x05BC . 21)    ; HEBREW POINT DAGESH
    (#x05BD . 22)    ; HEBREW POINT METEG
    (#x05BF . 23)    ; HEBREW POINT RAFE
    (#x05C1 . 24)    ; HEBREW POINT SHIN DOT
    (#x05C2 . 25)    ; HEBREW POINT SIN DOT
    ;; Arabic
    (#x064B . 27)    ; ARABIC FATHATAN
    (#x064C . 28)    ; ARABIC DAMMATAN
    (#x064D . 29)    ; ARABIC KASRATAN
    (#x064E . 30)    ; ARABIC FATHA
    (#x064F . 31)    ; ARABIC DAMMA
    (#x0650 . 32)    ; ARABIC KASRA
    (#x0651 . 33)    ; ARABIC SHADDA
    (#x0652 . 34)    ; ARABIC SUKUN
    (#x0670 . 35)    ; ARABIC LETTER SUPERSCRIPT ALEF
    ;; Thai
    (#x0E38 . 103)   ; THAI CHARACTER SARA U
    (#x0E39 . 103)   ; THAI CHARACTER SARA UU
    (#x0E3A . 9)     ; THAI CHARACTER PHINTHU
    (#x0E48 . 107)   ; THAI CHARACTER MAI EK
    (#x0E49 . 107)   ; THAI CHARACTER MAI THO
    (#x0E4A . 107)   ; THAI CHARACTER MAI TRI
    (#x0E4B . 107))) ; THAI CHARACTER MAI CHATTAWA

(define (get-ccc cp)
  (let ([entry (assv cp ccc-table)])
       (if entry (cdr entry) 0)))

;;; ------------------------------------------------------------
;;; Canonical Decomposition Table
;;; ------------------------------------------------------------
;;; Maps precomposed characters to their canonical decomposition.
;;; Format: (precomposed . (base combining...))

(define decomposition-table
  '(;; Latin Extended-A and Extended-B (common accented letters)
    ;; A with diacritics
    (#x00C0 . (#x0041 #x0300))  ; A-grave
    (#x00C1 . (#x0041 #x0301))  ; A-acute
    (#x00C2 . (#x0041 #x0302))  ; A-circumflex
    (#x00C3 . (#x0041 #x0303))  ; A-tilde
    (#x00C4 . (#x0041 #x0308))  ; A-diaeresis
    (#x00C5 . (#x0041 #x030A))  ; A-ring
    (#x00C7 . (#x0043 #x0327))  ; C-cedilla
    (#x00C8 . (#x0045 #x0300))  ; E-grave
    (#x00C9 . (#x0045 #x0301))  ; E-acute
    (#x00CA . (#x0045 #x0302))  ; E-circumflex
    (#x00CB . (#x0045 #x0308))  ; E-diaeresis
    (#x00CC . (#x0049 #x0300))  ; I-grave
    (#x00CD . (#x0049 #x0301))  ; I-acute
    (#x00CE . (#x0049 #x0302))  ; I-circumflex
    (#x00CF . (#x0049 #x0308))  ; I-diaeresis
    (#x00D1 . (#x004E #x0303))  ; N-tilde
    (#x00D2 . (#x004F #x0300))  ; O-grave
    (#x00D3 . (#x004F #x0301))  ; O-acute
    (#x00D4 . (#x004F #x0302))  ; O-circumflex
    (#x00D5 . (#x004F #x0303))  ; O-tilde
    (#x00D6 . (#x004F #x0308))  ; O-diaeresis
    (#x00D9 . (#x0055 #x0300))  ; U-grave
    (#x00DA . (#x0055 #x0301))  ; U-acute
    (#x00DB . (#x0055 #x0302))  ; U-circumflex
    (#x00DC . (#x0055 #x0308))  ; U-diaeresis
    (#x00DD . (#x0059 #x0301))  ; Y-acute
    ;; a with diacritics
    (#x00E0 . (#x0061 #x0300))  ; a-grave
    (#x00E1 . (#x0061 #x0301))  ; a-acute
    (#x00E2 . (#x0061 #x0302))  ; a-circumflex
    (#x00E3 . (#x0061 #x0303))  ; a-tilde
    (#x00E4 . (#x0061 #x0308))  ; a-diaeresis
    (#x00E5 . (#x0061 #x030A))  ; a-ring
    (#x00E7 . (#x0063 #x0327))  ; c-cedilla
    (#x00E8 . (#x0065 #x0300))  ; e-grave
    (#x00E9 . (#x0065 #x0301))  ; e-acute
    (#x00EA . (#x0065 #x0302))  ; e-circumflex
    (#x00EB . (#x0065 #x0308))  ; e-diaeresis
    (#x00EC . (#x0069 #x0300))  ; i-grave
    (#x00ED . (#x0069 #x0301))  ; i-acute
    (#x00EE . (#x0069 #x0302))  ; i-circumflex
    (#x00EF . (#x0069 #x0308))  ; i-diaeresis
    (#x00F1 . (#x006E #x0303))  ; n-tilde
    (#x00F2 . (#x006F #x0300))  ; o-grave
    (#x00F3 . (#x006F #x0301))  ; o-acute
    (#x00F4 . (#x006F #x0302))  ; o-circumflex
    (#x00F5 . (#x006F #x0303))  ; o-tilde
    (#x00F6 . (#x006F #x0308))  ; o-diaeresis
    (#x00F9 . (#x0075 #x0300))  ; u-grave
    (#x00FA . (#x0075 #x0301))  ; u-acute
    (#x00FB . (#x0075 #x0302))  ; u-circumflex
    (#x00FC . (#x0075 #x0308))  ; u-diaeresis
    (#x00FD . (#x0079 #x0301))  ; y-acute
    (#x00FF . (#x0079 #x0308))  ; y-diaeresis
    ;; Extended Latin
    (#x0100 . (#x0041 #x0304))  ; A-macron
    (#x0101 . (#x0061 #x0304))  ; a-macron
    (#x0102 . (#x0041 #x0306))  ; A-breve
    (#x0103 . (#x0061 #x0306))  ; a-breve
    (#x0104 . (#x0041 #x0328))  ; A-ogonek
    (#x0105 . (#x0061 #x0328))  ; a-ogonek
    (#x0106 . (#x0043 #x0301))  ; C-acute
    (#x0107 . (#x0063 #x0301))  ; c-acute
    (#x0108 . (#x0043 #x0302))  ; C-circumflex
    (#x0109 . (#x0063 #x0302))  ; c-circumflex
    (#x010A . (#x0043 #x0307))  ; C-dot
    (#x010B . (#x0063 #x0307))  ; c-dot
    (#x010C . (#x0043 #x030C))  ; C-caron
    (#x010D . (#x0063 #x030C))  ; c-caron
    (#x010E . (#x0044 #x030C))  ; D-caron
    (#x010F . (#x0064 #x030C))  ; d-caron
    (#x0112 . (#x0045 #x0304))  ; E-macron
    (#x0113 . (#x0065 #x0304))  ; e-macron
    (#x0114 . (#x0045 #x0306))  ; E-breve
    (#x0115 . (#x0065 #x0306))  ; e-breve
    (#x0116 . (#x0045 #x0307))  ; E-dot
    (#x0117 . (#x0065 #x0307))  ; e-dot
    (#x0118 . (#x0045 #x0328))  ; E-ogonek
    (#x0119 . (#x0065 #x0328))  ; e-ogonek
    (#x011A . (#x0045 #x030C))  ; E-caron
    (#x011B . (#x0065 #x030C))  ; e-caron
    (#x011C . (#x0047 #x0302))  ; G-circumflex
    (#x011D . (#x0067 #x0302))  ; g-circumflex
    (#x011E . (#x0047 #x0306))  ; G-breve
    (#x011F . (#x0067 #x0306))  ; g-breve
    (#x0120 . (#x0047 #x0307))  ; G-dot
    (#x0121 . (#x0067 #x0307))  ; g-dot
    (#x0122 . (#x0047 #x0327))  ; G-cedilla
    (#x0123 . (#x0067 #x0327))  ; g-cedilla
    (#x0124 . (#x0048 #x0302))  ; H-circumflex
    (#x0125 . (#x0068 #x0302))  ; h-circumflex
    (#x0128 . (#x0049 #x0303))  ; I-tilde
    (#x0129 . (#x0069 #x0303))  ; i-tilde
    (#x012A . (#x0049 #x0304))  ; I-macron
    (#x012B . (#x0069 #x0304))  ; i-macron
    (#x012C . (#x0049 #x0306))  ; I-breve
    (#x012D . (#x0069 #x0306))  ; i-breve
    (#x012E . (#x0049 #x0328))  ; I-ogonek
    (#x012F . (#x0069 #x0328))  ; i-ogonek
    (#x0130 . (#x0049 #x0307))  ; I-dot
    (#x0134 . (#x004A #x0302))  ; J-circumflex
    (#x0135 . (#x006A #x0302))  ; j-circumflex
    (#x0136 . (#x004B #x0327))  ; K-cedilla
    (#x0137 . (#x006B #x0327))  ; k-cedilla
    (#x0139 . (#x004C #x0301))  ; L-acute
    (#x013A . (#x006C #x0301))  ; l-acute
    (#x013B . (#x004C #x0327))  ; L-cedilla
    (#x013C . (#x006C #x0327))  ; l-cedilla
    (#x013D . (#x004C #x030C))  ; L-caron
    (#x013E . (#x006C #x030C))  ; l-caron
    (#x0143 . (#x004E #x0301))  ; N-acute
    (#x0144 . (#x006E #x0301))  ; n-acute
    (#x0145 . (#x004E #x0327))  ; N-cedilla
    (#x0146 . (#x006E #x0327))  ; n-cedilla
    (#x0147 . (#x004E #x030C))  ; N-caron
    (#x0148 . (#x006E #x030C))  ; n-caron
    (#x014C . (#x004F #x0304))  ; O-macron
    (#x014D . (#x006F #x0304))  ; o-macron
    (#x014E . (#x004F #x0306))  ; O-breve
    (#x014F . (#x006F #x0306))  ; o-breve
    (#x0150 . (#x004F #x030B))  ; O-double-acute
    (#x0151 . (#x006F #x030B))  ; o-double-acute
    (#x0154 . (#x0052 #x0301))  ; R-acute
    (#x0155 . (#x0072 #x0301))  ; r-acute
    (#x0156 . (#x0052 #x0327))  ; R-cedilla
    (#x0157 . (#x0072 #x0327))  ; r-cedilla
    (#x0158 . (#x0052 #x030C))  ; R-caron
    (#x0159 . (#x0072 #x030C))  ; r-caron
    (#x015A . (#x0053 #x0301))  ; S-acute
    (#x015B . (#x0073 #x0301))  ; s-acute
    (#x015C . (#x0053 #x0302))  ; S-circumflex
    (#x015D . (#x0073 #x0302))  ; s-circumflex
    (#x015E . (#x0053 #x0327))  ; S-cedilla
    (#x015F . (#x0073 #x0327))  ; s-cedilla
    (#x0160 . (#x0053 #x030C))  ; S-caron
    (#x0161 . (#x0073 #x030C))  ; s-caron
    (#x0162 . (#x0054 #x0327))  ; T-cedilla
    (#x0163 . (#x0074 #x0327))  ; t-cedilla
    (#x0164 . (#x0054 #x030C))  ; T-caron
    (#x0165 . (#x0074 #x030C))  ; t-caron
    (#x0168 . (#x0055 #x0303))  ; U-tilde
    (#x0169 . (#x0075 #x0303))  ; u-tilde
    (#x016A . (#x0055 #x0304))  ; U-macron
    (#x016B . (#x0075 #x0304))  ; u-macron
    (#x016C . (#x0055 #x0306))  ; U-breve
    (#x016D . (#x0075 #x0306))  ; u-breve
    (#x016E . (#x0055 #x030A))  ; U-ring
    (#x016F . (#x0075 #x030A))  ; u-ring
    (#x0170 . (#x0055 #x030B))  ; U-double-acute
    (#x0171 . (#x0075 #x030B))  ; u-double-acute
    (#x0172 . (#x0055 #x0328))  ; U-ogonek
    (#x0173 . (#x0075 #x0328))  ; u-ogonek
    (#x0174 . (#x0057 #x0302))  ; W-circumflex
    (#x0175 . (#x0077 #x0302))  ; w-circumflex
    (#x0176 . (#x0059 #x0302))  ; Y-circumflex
    (#x0177 . (#x0079 #x0302))  ; y-circumflex
    (#x0178 . (#x0059 #x0308))  ; Y-diaeresis
    (#x0179 . (#x005A #x0301))  ; Z-acute
    (#x017A . (#x007A #x0301))  ; z-acute
    (#x017B . (#x005A #x0307))  ; Z-dot
    (#x017C . (#x007A #x0307))  ; z-dot
    (#x017D . (#x005A #x030C))  ; Z-caron
    (#x017E . (#x007A #x030C))  ; z-caron
    ;; Greek with diacritics
    (#x0386 . (#x0391 #x0301))  ; Alpha-tonos
    (#x0388 . (#x0395 #x0301))  ; Epsilon-tonos
    (#x0389 . (#x0397 #x0301))  ; Eta-tonos
    (#x038A . (#x0399 #x0301))  ; Iota-tonos
    (#x038C . (#x039F #x0301))  ; Omicron-tonos
    (#x038E . (#x03A5 #x0301))  ; Upsilon-tonos
    (#x038F . (#x03A9 #x0301))  ; Omega-tonos
    (#x0390 . (#x03B9 #x0308 #x0301))  ; iota-diaeresis-tonos
    (#x03AA . (#x0399 #x0308))  ; Iota-diaeresis
    (#x03AB . (#x03A5 #x0308))  ; Upsilon-diaeresis
    (#x03AC . (#x03B1 #x0301))  ; alpha-tonos
    (#x03AD . (#x03B5 #x0301))  ; epsilon-tonos
    (#x03AE . (#x03B7 #x0301))  ; eta-tonos
    (#x03AF . (#x03B9 #x0301))  ; iota-tonos
    (#x03B0 . (#x03C5 #x0308 #x0301))  ; upsilon-diaeresis-tonos
    (#x03CA . (#x03B9 #x0308))  ; iota-diaeresis
    (#x03CB . (#x03C5 #x0308))  ; upsilon-diaeresis
    (#x03CC . (#x03BF #x0301))  ; omicron-tonos
    (#x03CD . (#x03C5 #x0301))  ; upsilon-tonos
    (#x03CE . (#x03C9 #x0301))  ; omega-tonos
    ;; Hangul compatibility Jamo compositions handled separately
    ))

;;; ------------------------------------------------------------
;;; Canonical Composition Table
;;; ------------------------------------------------------------
;;; Maps (base, combining) pairs back to precomposed characters.
;;; This is the inverse of decomposition for "primary composites".

(define composition-table
  ;; Built as inverse of decomposition-table for length-2 decompositions
  ;; Format: ((base . combining) . precomposed)
  '(((#x0041 . #x0300) . #x00C0)  ; A + grave
    ((#x0041 . #x0301) . #x00C1)  ; A + acute
    ((#x0041 . #x0302) . #x00C2)  ; A + circumflex
    ((#x0041 . #x0303) . #x00C3)  ; A + tilde
    ((#x0041 . #x0304) . #x0100)  ; A + macron
    ((#x0041 . #x0306) . #x0102)  ; A + breve
    ((#x0041 . #x0307) . #x0226)  ; A + dot
    ((#x0041 . #x0308) . #x00C4)  ; A + diaeresis
    ((#x0041 . #x030A) . #x00C5)  ; A + ring
    ((#x0041 . #x0328) . #x0104)  ; A + ogonek
    ((#x0043 . #x0301) . #x0106)  ; C + acute
    ((#x0043 . #x0302) . #x0108)  ; C + circumflex
    ((#x0043 . #x0307) . #x010A)  ; C + dot
    ((#x0043 . #x030C) . #x010C)  ; C + caron
    ((#x0043 . #x0327) . #x00C7)  ; C + cedilla
    ((#x0044 . #x030C) . #x010E)  ; D + caron
    ((#x0045 . #x0300) . #x00C8)  ; E + grave
    ((#x0045 . #x0301) . #x00C9)  ; E + acute
    ((#x0045 . #x0302) . #x00CA)  ; E + circumflex
    ((#x0045 . #x0304) . #x0112)  ; E + macron
    ((#x0045 . #x0306) . #x0114)  ; E + breve
    ((#x0045 . #x0307) . #x0116)  ; E + dot
    ((#x0045 . #x0308) . #x00CB)  ; E + diaeresis
    ((#x0045 . #x030C) . #x011A)  ; E + caron
    ((#x0045 . #x0328) . #x0118)  ; E + ogonek
    ((#x0047 . #x0302) . #x011C)  ; G + circumflex
    ((#x0047 . #x0306) . #x011E)  ; G + breve
    ((#x0047 . #x0307) . #x0120)  ; G + dot
    ((#x0047 . #x0327) . #x0122)  ; G + cedilla
    ((#x0048 . #x0302) . #x0124)  ; H + circumflex
    ((#x0049 . #x0300) . #x00CC)  ; I + grave
    ((#x0049 . #x0301) . #x00CD)  ; I + acute
    ((#x0049 . #x0302) . #x00CE)  ; I + circumflex
    ((#x0049 . #x0303) . #x0128)  ; I + tilde
    ((#x0049 . #x0304) . #x012A)  ; I + macron
    ((#x0049 . #x0306) . #x012C)  ; I + breve
    ((#x0049 . #x0307) . #x0130)  ; I + dot
    ((#x0049 . #x0308) . #x00CF)  ; I + diaeresis
    ((#x0049 . #x0328) . #x012E)  ; I + ogonek
    ((#x004A . #x0302) . #x0134)  ; J + circumflex
    ((#x004B . #x0327) . #x0136)  ; K + cedilla
    ((#x004C . #x0301) . #x0139)  ; L + acute
    ((#x004C . #x030C) . #x013D)  ; L + caron
    ((#x004C . #x0327) . #x013B)  ; L + cedilla
    ((#x004E . #x0301) . #x0143)  ; N + acute
    ((#x004E . #x0303) . #x00D1)  ; N + tilde
    ((#x004E . #x030C) . #x0147)  ; N + caron
    ((#x004E . #x0327) . #x0145)  ; N + cedilla
    ((#x004F . #x0300) . #x00D2)  ; O + grave
    ((#x004F . #x0301) . #x00D3)  ; O + acute
    ((#x004F . #x0302) . #x00D4)  ; O + circumflex
    ((#x004F . #x0303) . #x00D5)  ; O + tilde
    ((#x004F . #x0304) . #x014C)  ; O + macron
    ((#x004F . #x0306) . #x014E)  ; O + breve
    ((#x004F . #x0308) . #x00D6)  ; O + diaeresis
    ((#x004F . #x030B) . #x0150)  ; O + double-acute
    ((#x0052 . #x0301) . #x0154)  ; R + acute
    ((#x0052 . #x030C) . #x0158)  ; R + caron
    ((#x0052 . #x0327) . #x0156)  ; R + cedilla
    ((#x0053 . #x0301) . #x015A)  ; S + acute
    ((#x0053 . #x0302) . #x015C)  ; S + circumflex
    ((#x0053 . #x030C) . #x0160)  ; S + caron
    ((#x0053 . #x0327) . #x015E)  ; S + cedilla
    ((#x0054 . #x030C) . #x0164)  ; T + caron
    ((#x0054 . #x0327) . #x0162)  ; T + cedilla
    ((#x0055 . #x0300) . #x00D9)  ; U + grave
    ((#x0055 . #x0301) . #x00DA)  ; U + acute
    ((#x0055 . #x0302) . #x00DB)  ; U + circumflex
    ((#x0055 . #x0303) . #x0168)  ; U + tilde
    ((#x0055 . #x0304) . #x016A)  ; U + macron
    ((#x0055 . #x0306) . #x016C)  ; U + breve
    ((#x0055 . #x0308) . #x00DC)  ; U + diaeresis
    ((#x0055 . #x030A) . #x016E)  ; U + ring
    ((#x0055 . #x030B) . #x0170)  ; U + double-acute
    ((#x0055 . #x0328) . #x0172)  ; U + ogonek
    ((#x0057 . #x0302) . #x0174)  ; W + circumflex
    ((#x0059 . #x0301) . #x00DD)  ; Y + acute
    ((#x0059 . #x0302) . #x0176)  ; Y + circumflex
    ((#x0059 . #x0308) . #x0178)  ; Y + diaeresis
    ((#x005A . #x0301) . #x0179)  ; Z + acute
    ((#x005A . #x0307) . #x017B)  ; Z + dot
    ((#x005A . #x030C) . #x017D)  ; Z + caron
    ;; Lowercase
    ((#x0061 . #x0300) . #x00E0)  ; a + grave
    ((#x0061 . #x0301) . #x00E1)  ; a + acute
    ((#x0061 . #x0302) . #x00E2)  ; a + circumflex
    ((#x0061 . #x0303) . #x00E3)  ; a + tilde
    ((#x0061 . #x0304) . #x0101)  ; a + macron
    ((#x0061 . #x0306) . #x0103)  ; a + breve
    ((#x0061 . #x0308) . #x00E4)  ; a + diaeresis
    ((#x0061 . #x030A) . #x00E5)  ; a + ring
    ((#x0061 . #x0328) . #x0105)  ; a + ogonek
    ((#x0063 . #x0301) . #x0107)  ; c + acute
    ((#x0063 . #x0302) . #x0109)  ; c + circumflex
    ((#x0063 . #x0307) . #x010B)  ; c + dot
    ((#x0063 . #x030C) . #x010D)  ; c + caron
    ((#x0063 . #x0327) . #x00E7)  ; c + cedilla
    ((#x0064 . #x030C) . #x010F)  ; d + caron
    ((#x0065 . #x0300) . #x00E8)  ; e + grave
    ((#x0065 . #x0301) . #x00E9)  ; e + acute
    ((#x0065 . #x0302) . #x00EA)  ; e + circumflex
    ((#x0065 . #x0304) . #x0113)  ; e + macron
    ((#x0065 . #x0306) . #x0115)  ; e + breve
    ((#x0065 . #x0307) . #x0117)  ; e + dot
    ((#x0065 . #x0308) . #x00EB)  ; e + diaeresis
    ((#x0065 . #x030C) . #x011B)  ; e + caron
    ((#x0065 . #x0328) . #x0119)  ; e + ogonek
    ((#x0067 . #x0302) . #x011D)  ; g + circumflex
    ((#x0067 . #x0306) . #x011F)  ; g + breve
    ((#x0067 . #x0307) . #x0121)  ; g + dot
    ((#x0067 . #x0327) . #x0123)  ; g + cedilla
    ((#x0068 . #x0302) . #x0125)  ; h + circumflex
    ((#x0069 . #x0300) . #x00EC)  ; i + grave
    ((#x0069 . #x0301) . #x00ED)  ; i + acute
    ((#x0069 . #x0302) . #x00EE)  ; i + circumflex
    ((#x0069 . #x0303) . #x0129)  ; i + tilde
    ((#x0069 . #x0304) . #x012B)  ; i + macron
    ((#x0069 . #x0306) . #x012D)  ; i + breve
    ((#x0069 . #x0308) . #x00EF)  ; i + diaeresis
    ((#x0069 . #x0328) . #x012F)  ; i + ogonek
    ((#x006A . #x0302) . #x0135)  ; j + circumflex
    ((#x006B . #x0327) . #x0137)  ; k + cedilla
    ((#x006C . #x0301) . #x013A)  ; l + acute
    ((#x006C . #x030C) . #x013E)  ; l + caron
    ((#x006C . #x0327) . #x013C)  ; l + cedilla
    ((#x006E . #x0301) . #x0144)  ; n + acute
    ((#x006E . #x0303) . #x00F1)  ; n + tilde
    ((#x006E . #x030C) . #x0148)  ; n + caron
    ((#x006E . #x0327) . #x0146)  ; n + cedilla
    ((#x006F . #x0300) . #x00F2)  ; o + grave
    ((#x006F . #x0301) . #x00F3)  ; o + acute
    ((#x006F . #x0302) . #x00F4)  ; o + circumflex
    ((#x006F . #x0303) . #x00F5)  ; o + tilde
    ((#x006F . #x0304) . #x014D)  ; o + macron
    ((#x006F . #x0306) . #x014F)  ; o + breve
    ((#x006F . #x0308) . #x00F6)  ; o + diaeresis
    ((#x006F . #x030B) . #x0151)  ; o + double-acute
    ((#x0072 . #x0301) . #x0155)  ; r + acute
    ((#x0072 . #x030C) . #x0159)  ; r + caron
    ((#x0072 . #x0327) . #x0157)  ; r + cedilla
    ((#x0073 . #x0301) . #x015B)  ; s + acute
    ((#x0073 . #x0302) . #x015D)  ; s + circumflex
    ((#x0073 . #x030C) . #x0161)  ; s + caron
    ((#x0073 . #x0327) . #x015F)  ; s + cedilla
    ((#x0074 . #x030C) . #x0165)  ; t + caron
    ((#x0074 . #x0327) . #x0163)  ; t + cedilla
    ((#x0075 . #x0300) . #x00F9)  ; u + grave
    ((#x0075 . #x0301) . #x00FA)  ; u + acute
    ((#x0075 . #x0302) . #x00FB)  ; u + circumflex
    ((#x0075 . #x0303) . #x0169)  ; u + tilde
    ((#x0075 . #x0304) . #x016B)  ; u + macron
    ((#x0075 . #x0306) . #x016D)  ; u + breve
    ((#x0075 . #x0308) . #x00FC)  ; u + diaeresis
    ((#x0075 . #x030A) . #x016F)  ; u + ring
    ((#x0075 . #x030B) . #x0171)  ; u + double-acute
    ((#x0075 . #x0328) . #x0173)  ; u + ogonek
    ((#x0077 . #x0302) . #x0175)  ; w + circumflex
    ((#x0079 . #x0301) . #x00FD)  ; y + acute
    ((#x0079 . #x0302) . #x0177)  ; y + circumflex
    ((#x0079 . #x0308) . #x00FF)  ; y + diaeresis
    ((#x007A . #x0301) . #x017A)  ; z + acute
    ((#x007A . #x0307) . #x017C)  ; z + dot
    ((#x007A . #x030C) . #x017E)  ; z + caron
    ;; Greek
    ((#x0391 . #x0301) . #x0386)  ; Alpha + tonos
    ((#x0395 . #x0301) . #x0388)  ; Epsilon + tonos
    ((#x0397 . #x0301) . #x0389)  ; Eta + tonos
    ((#x0399 . #x0301) . #x038A)  ; Iota + tonos
    ((#x0399 . #x0308) . #x03AA)  ; Iota + diaeresis
    ((#x039F . #x0301) . #x038C)  ; Omicron + tonos
    ((#x03A5 . #x0301) . #x038E)  ; Upsilon + tonos
    ((#x03A5 . #x0308) . #x03AB)  ; Upsilon + diaeresis
    ((#x03A9 . #x0301) . #x038F)  ; Omega + tonos
    ((#x03B1 . #x0301) . #x03AC)  ; alpha + tonos
    ((#x03B5 . #x0301) . #x03AD)  ; epsilon + tonos
    ((#x03B7 . #x0301) . #x03AE)  ; eta + tonos
    ((#x03B9 . #x0301) . #x03AF)  ; iota + tonos
    ((#x03B9 . #x0308) . #x03CA)  ; iota + diaeresis
    ((#x03BF . #x0301) . #x03CC)  ; omicron + tonos
    ((#x03C5 . #x0301) . #x03CD)  ; upsilon + tonos
    ((#x03C5 . #x0308) . #x03CB)  ; upsilon + diaeresis
    ((#x03C9 . #x0301) . #x03CE)  ; omega + tonos
    ))

;;; ------------------------------------------------------------
;;; Composition Exclusions
;;; ------------------------------------------------------------
;;; Some characters should not be composed (singletons, non-starters).

(define composition-exclusions
  '(#x0340  ; COMBINING GRAVE TONE MARK (singleton)
    #x0341  ; COMBINING ACUTE TONE MARK (singleton)
    #x0343  ; COMBINING GREEK KORONIS (singleton)
    #x0344  ; COMBINING GREEK DIALYTIKA TONOS (singleton)
    #x0374  ; GREEK NUMERAL SIGN (singleton)
    #x037E  ; GREEK QUESTION MARK (singleton)
    #x0387  ; GREEK ANO TELEIA (singleton)
    ))

;;; ------------------------------------------------------------
;;; NFC Algorithm Implementation
;;; ------------------------------------------------------------

;;; is-starter? : Integer → Boolean
;;; A starter has CCC=0 and is not a combining mark.
(define (is-starter? cp)
  (= (get-ccc cp) 0))

;;; decompose-char : Integer → (listof Integer)
;;; Recursively decompose a codepoint to NFD form.
(define (decompose-char cp)
  (let ([entry (assv cp decomposition-table)])
       (if entry
           ;; Recursively decompose each component
           (apply append (map decompose-char (cdr entry)))
           ;; No decomposition; return as singleton list
           (list cp))))

;;; decompose-to-nfd : (listof Integer) → (listof Integer)
;;; Apply canonical decomposition to a list of codepoints.
(define (decompose-to-nfd cps)
  (apply append (map decompose-char cps)))

;;; sort-combining-marks : (listof Integer) → (listof Integer)
;;; Sort combining marks by CCC using canonical ordering.
;;; Starters (CCC=0) block the sort; only sort within combining sequences.
(define (sort-combining-marks cps)
  (let loop ([input cps] [acc '()])
       (if (null? input)
           (reverse acc)
           (let ([cp (car input)])
                (if (is-starter? cp)
                    ;; Starter: emit accumulated combining marks (sorted), then this starter
                    (loop (cdr input) (cons cp acc))
                    ;; Combining mark: collect consecutive combining marks for sorting
                    (let collect ([rest (cdr input)] [combining (list cp)])
                         (if (or (null? rest) (is-starter? (car rest)))
                             ;; Sort combining marks by CCC (stable sort)
                             (let ([sorted (sort-by-ccc (reverse combining))])
                                  (loop rest (append (reverse sorted) acc)))
                             (collect (cdr rest) (cons (car rest) combining)))))))))

;;; sort-by-ccc : (listof Integer) → (listof Integer)
;;; Sort codepoints by their Canonical Combining Class.
;;; Uses a stable insertion sort.
(define (sort-by-ccc cps)
  (let loop ([rest cps] [sorted '()])
       (if (null? rest)
           sorted
           (loop (cdr rest) (insert-by-ccc (car rest) sorted)))))

(define (insert-by-ccc cp sorted)
  (let ([ccc (get-ccc cp)])
       (let loop ([before '()] [after sorted])
            ;; Use < (not <=) to ensure stable sort: equal CCCs preserve original order
            (if (or (null? after) (< ccc (get-ccc (car after))))
                (append (reverse before) (cons cp after))
                (loop (cons (car after) before) (cdr after))))))

;;; compose-pair : Integer × Integer → Integer | #f
;;; Try to compose two codepoints into a single precomposed character.
;;; Returns the composed codepoint or #f if no composition exists.
(define (compose-pair base combining)
  (let ([entry (assoc (cons base combining) composition-table)])
       (if entry
           (cdr entry)
           #f)))

;;; canonical-compose : (listof Integer) → (listof Integer)
;;; Apply canonical composition to NFD-normalized codepoints.
(define (canonical-compose cps)
  (if (null? cps)
      '()
      (let loop ([starter (car cps)]
                 [rest (cdr cps)]
                 [between '()]  ; combining marks between starter and current
                 [result '()])
           (if (null? rest)
               ;; Emit final starter and any remaining combining marks
               (reverse (append (reverse between) (cons starter result)))
               (let* ([current (car rest)]
                      [current-ccc (get-ccc current)])
                     (cond
                      ;; Current is a starter
                      [(= current-ccc 0)
                       ;; Try to compose with previous starter if no blockers
                       (let ([composed (and (null? between) (compose-pair starter current))])
                            (if composed
                                ;; Composition succeeded; new starter
                                (loop composed (cdr rest) '() result)
                                ;; Cannot compose; emit previous starter + marks, start new
                                (loop current (cdr rest) '()
                                      (append (reverse between) (cons starter result)))))]
                      ;; Current is a combining mark
                      [else
                       ;; Check if blocked by intervening mark with same or higher CCC
                       (let ([blocked? (and (not (null? between))
                                            (>= (get-ccc (car between)) current-ccc))])
                            (if blocked?
                                ;; Cannot compose; add to between marks
                                (loop starter (cdr rest) (cons current between) result)
                                ;; Try to compose
                                (let ([composed (compose-pair starter current)])
                                     (if composed
                                         ;; Composition succeeded
                                         (loop composed (cdr rest) between result)
                                         ;; Cannot compose; add to between marks
                                         (loop starter (cdr rest) (cons current between) result)))))]))))))

;;; normalize-nfc : String → String
;;; Transform text to NFC (Canonical Composition) form.
;;; This ensures deterministic hashing regardless of input encoding.
(define (normalize-nfc str)
  (if (ascii-only? str)
      str  ; Fast path for ASCII
      ;; Full NFC: decompose, sort, compose
      (let* ([cps (map char->integer (string->list str))]
             [nfd (decompose-to-nfd cps)]
             [sorted (sort-combining-marks nfd)]
             [nfc (canonical-compose sorted)])
            (list->string (map integer->char nfc)))))

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
  `((#\a . (#x0430))       ; Cyrillic а
    (#\c . (#x0441))       ; Cyrillic с
    (#\e . (#x0435))       ; Cyrillic е
    (#\o . (#x043E #x03BF)); Cyrillic о, Greek ο
    (#\p . (#x0440))       ; Cyrillic р
    (#\x . (#x0445))       ; Cyrillic х
    (#\y . (#x0443))       ; Cyrillic у
    (#\0 . (#x041E))       ; Cyrillic О
    (#\1 . (#x0406 #x04C0)); Cyrillic І, Ӏ
    ))

;;; contains-confusable? : String → Boolean
;;; Check if string contains characters that look like ASCII but aren't.
(define (contains-confusable? str)
  (let loop ([chars (string->list str)])
       (and (not (null? chars))
            (or (is-confusable? (car chars))
                (loop (cdr chars))))))

(define (is-confusable? c)
  (let ([cp (char->integer c)])
       (and (> cp 127)  ; Only non-ASCII can be confusables
            (any (lambda (pair)
                         (and (memv cp (cdr pair)) #t))  ; Convert list to boolean
                 confusable-pairs))))

(define (any pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (any pred (cdr lst)))))

;;; ============================================================
;;; Quarantine Zone
;;; ============================================================

;;; Text that fails validation but must be preserved goes here.
;;; Quarantined text is:
;;;   - Stored with its original bytes
;;;   - Marked with a quarantine tag
;;;   - Never evaluated as code
;;;   - Subject to review

(define-record-type quarantined-text
  (fields original-bytes reason timestamp))

;;; quarantine : Bytevector × String → QuarantinedText
(define (quarantine bytes reason)
  (make-quarantined-text bytes reason (current-time)))

;;; current-time-iso : → String
;;; ISO 8601 timestamp in UTC
;;; NOTE: Renamed from current-time to avoid shadowing Chez Scheme's
;;;       built-in (current-time) which returns a time object.
;;;       Use (current-timestamp) from forum/tools.ss for string timestamps.
(define (current-time-iso)
  (let ([d (current-date 0)])  ; 0 = UTC offset
       (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
               (date-year d)
               (date-month d)
               (date-day d)
               (date-hour d)
               (date-minute d)
               (date-second d))))

;;; ============================================================
;;; High-Level API
;;; ============================================================

;;; safe-text : String → String | #f
;;; Return sanitized text, or #f if unrecoverable.
(define (safe-text str)
  (let ([result (validate-text str)])
       (if (text-result-ok? result)
           (sanitize-text (text-result-value result))
           #f)))

;;; safe-symbol : String → Symbol | #f
;;; Validate and intern a symbol, or #f if invalid.
(define (safe-symbol str)
  (let ([result (validate-symbol str)])
       (if (text-result-ok? result)
           (string->symbol (text-result-value result))
           #f)))
