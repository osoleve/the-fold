;;; thimble/string-utils.ss — Canonical String Utilities
;;;
;;; This is the ONE TRUE PLACE for string utilities in The Fold.
;;; After observing 23+ different implementations of string-split,
;;; string-contains?, and string-trim scattered across the codebase,
;;; we establish this as the canonical source.
;;;
;;; This is Thimble code: practical, well-tested, defensive.
;;;
;;; Design principles:
;;;   1. Pure functions - no mutation
;;;   2. Tier 5-6 operations - built on Scheme string primitives
;;;   3. UTF-8 aware - proper Unicode handling
;;;   4. Consistent naming - follows Scheme conventions
;;;   5. Clear error handling - graceful with edge cases
;;;
;;; Dependencies: fabric/stitches/prelude.ss (for string-join)
;;;
;;; Response to wishlist item #3 (High Priority Tools)
;;; Root: dc857f05c0d58dcb417b9631a064161abac9956b5c75f0e49a4d739329e7075f

;;; ============================================================
;;; String Splitting and Joining
;;; ============================================================

;;; string-split : String × Char → (List String)
;;; Split string by character delimiter.
;;; Returns list of substrings, preserving empty strings.
;;;
;;; Examples:
;;;   (string-split "foo,bar,baz" #\,) => ("foo" "bar" "baz")
;;;   (string-split "a,,b" #\,)        => ("a" "" "b")
;;;   (string-split "" #\,)            => ("")
(define (string-split str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) result))]
      [(char=? (car chars) delim)
       (loop (cdr chars)
             '()
             (cons (list->string (reverse current)) result))]
      [else
       (loop (cdr chars)
             (cons (car chars) current)
             result)])))

;;; string-split-lines : String → (List String)
;;; Split string by newlines (\n).
;;; Handles both Unix (\n) and Windows (\r\n) line endings.
;;;
;;; Examples:
;;;   (string-split-lines "foo\nbar\nbaz") => ("foo" "bar" "baz")
;;;   (string-split-lines "a\r\nb\r\nc")   => ("a" "b" "c")
(define (string-split-lines str)
  ;; First normalize Windows line endings to Unix
  (let ([normalized (string-replace str "\r\n" "\n")])
    (string-split normalized #\newline)))

;;; string-join : (List String) × String → String
;;; Join strings with separator.
;;; Re-exported from fabric/stitches/prelude.ss for convenience.
;;;
;;; Examples:
;;;   (string-join '("foo" "bar" "baz") ", ") => "foo, bar, baz"
;;;   (string-join '("a") ",")                => "a"
;;;   (string-join '() ",")                   => ""
;;;
;;; Note: This is imported from prelude.ss. If prelude is not loaded,
;;; use this implementation:
(define (string-join strs sep)
  (cond
    [(null? strs) ""]
    [(null? (cdr strs)) (car strs)]
    [else
     (let loop ([remaining (cdr strs)]
                [result (car strs)])
       (if (null? remaining)
           result
           (loop (cdr remaining)
                 (string-append result sep (car remaining)))))]))

;;; ============================================================
;;; String Search and Pattern Matching
;;; ============================================================

;;; string-contains? : String × String → Bool
;;; Check if haystack contains needle as a substring.
;;;
;;; Examples:
;;;   (string-contains? "hello world" "world") => #t
;;;   (string-contains? "hello" "bye")         => #f
;;;   (string-contains? "test" "")             => #t
(define (string-contains? haystack needle)
  (and (>= (string-length haystack) (string-length needle))
       (let ([needle-len (string-length needle)])
         (cond
           [(= needle-len 0) #t]  ; Empty string is in everything
           [else
            (let loop ([pos 0])
              (cond
                [(> (+ pos needle-len) (string-length haystack)) #f]
                [(string=? (substring haystack pos (+ pos needle-len)) needle) #t]
                [else (loop (+ pos 1))]))]))))

;;; string-starts-with? : String × String → Bool
;;; Check if string starts with prefix.
;;;
;;; Examples:
;;;   (string-starts-with? "hello world" "hello") => #t
;;;   (string-starts-with? "test" "best")         => #f
;;;   (string-starts-with? "foo" "")              => #t
(define (string-starts-with? str prefix)
  (let ([str-len (string-length str)]
        [prefix-len (string-length prefix)])
    (and (>= str-len prefix-len)
         (string=? (substring str 0 prefix-len) prefix))))

;;; string-ends-with? : String × String → Bool
;;; Check if string ends with suffix.
;;;
;;; Examples:
;;;   (string-ends-with? "hello world" "world") => #t
;;;   (string-ends-with? "test" "best")         => #f
;;;   (string-ends-with? "foo" "")              => #t
(define (string-ends-with? str suffix)
  (let ([str-len (string-length str)]
        [suffix-len (string-length suffix)])
    (and (>= str-len suffix-len)
         (string=? (substring str (- str-len suffix-len) str-len) suffix))))

;;; string-index : String × Char → (Maybe Int)
;;; Find first occurrence of character in string.
;;; Returns position (0-indexed) or #f if not found.
;;;
;;; Examples:
;;;   (string-index "hello" #\l)  => 2
;;;   (string-index "hello" #\x)  => #f
(define (string-index str ch)
  (let loop ([pos 0])
    (cond
      [(= pos (string-length str)) #f]
      [(char=? (string-ref str pos) ch) pos]
      [else (loop (+ pos 1))])))

;;; string-index-of : String × String → (Maybe Int)
;;; Find first occurrence of substring in string.
;;; Returns position (0-indexed) or #f if not found.
;;;
;;; Examples:
;;;   (string-index-of "hello world" "world") => 6
;;;   (string-index-of "test" "best")         => #f
(define (string-index-of haystack needle)
  (let ([needle-len (string-length needle)]
        [haystack-len (string-length haystack)])
    (cond
      [(= needle-len 0) 0]  ; Empty string found at position 0
      [(> needle-len haystack-len) #f]
      [else
       (let loop ([pos 0])
         (cond
           [(> (+ pos needle-len) haystack-len) #f]
           [(string=? (substring haystack pos (+ pos needle-len)) needle) pos]
           [else (loop (+ pos 1))]))])))

;;; ============================================================
;;; String Replacement
;;; ============================================================

;;; string-replace : String × String × String → String
;;; Replace all occurrences of old substring with new substring.
;;;
;;; Examples:
;;;   (string-replace "hello world" "world" "universe")
;;;     => "hello universe"
;;;   (string-replace "aaa" "a" "b") => "bbb"
;;;   (string-replace "test" "x" "y") => "test"
(define (string-replace str old new)
  (let ([old-len (string-length old)])
    (cond
      [(= old-len 0) str]  ; Can't replace empty string
      [else
       (let loop ([pos 0] [parts '()])
         (let ([idx (string-index-of (substring str pos (string-length str)) old)])
           (if idx
               (let ([actual-pos (+ pos idx)])
                 (loop (+ actual-pos old-len)
                       (append parts
                               (list (substring str pos actual-pos)
                                     new))))
               ;; No more occurrences, append rest
               (string-join (append parts (list (substring str pos (string-length str))))
                           ""))))])))

;;; string-replace-first : String × String × String → String
;;; Replace only the first occurrence of old substring with new substring.
;;;
;;; Examples:
;;;   (string-replace-first "hello hello" "hello" "hi")
;;;     => "hi hello"
;;;   (string-replace-first "aaa" "a" "b") => "baa"
(define (string-replace-first str old new)
  (let ([idx (string-index-of str old)])
    (if idx
        (string-append
          (substring str 0 idx)
          new
          (substring str (+ idx (string-length old)) (string-length str)))
        str)))

;;; ============================================================
;;; String Trimming and Whitespace
;;; ============================================================

;;; whitespace? : Char → Bool
;;; Check if character is whitespace.
;;; Includes: space, tab, newline, carriage return.
(define (whitespace? ch)
  (memv ch '(#\space #\tab #\newline #\return)))

;;; string-trim : String → String
;;; Remove leading and trailing whitespace.
;;;
;;; Examples:
;;;   (string-trim "  hello  ") => "hello"
;;;   (string-trim "\n\ttest\n") => "test"
;;;   (string-trim "   ")        => ""
(define (string-trim str)
  (let ([chars (string->list str)])
    ;; Remove leading whitespace
    (let ([trimmed-left (let loop ([cs chars])
                          (if (or (null? cs) (not (whitespace? (car cs))))
                              cs
                              (loop (cdr cs))))])
      ;; Remove trailing whitespace
      (let ([trimmed-both (reverse
                           (let loop ([cs (reverse trimmed-left)])
                             (if (or (null? cs) (not (whitespace? (car cs))))
                                 cs
                                 (loop (cdr cs)))))])
        (list->string trimmed-both)))))

;;; string-trim-left : String → String
;;; Remove leading whitespace only.
;;;
;;; Examples:
;;;   (string-trim-left "  hello  ") => "hello  "
(define (string-trim-left str)
  (let loop ([chars (string->list str)])
    (if (or (null? chars) (not (whitespace? (car chars))))
        (list->string chars)
        (loop (cdr chars)))))

;;; string-trim-right : String → String
;;; Remove trailing whitespace only.
;;;
;;; Examples:
;;;   (string-trim-right "  hello  ") => "  hello"
(define (string-trim-right str)
  (list->string
    (reverse
      (let loop ([chars (reverse (string->list str))])
        (if (or (null? chars) (not (whitespace? (car chars))))
            chars
            (loop (cdr chars)))))))

;;; ============================================================
;;; String Case Conversion
;;; ============================================================

;;; string-downcase : String → String
;;; Convert string to lowercase.
;;; Note: This is a built-in in Chez Scheme, so we just use it directly.
(define (string-downcase-custom str)
  (list->string (map char-downcase (string->list str))))

;;; string-upcase : String → String
;;; Convert string to uppercase.
;;; Note: This is a built-in in Chez Scheme, so we just use it directly.
(define (string-upcase-custom str)
  (list->string (map char-upcase (string->list str))))

;;; ============================================================
;;; String Predicates
;;; ============================================================

;;; string-empty? : String → Bool
;;; Check if string is empty.
(define (string-empty? str)
  (= (string-length str) 0))

;;; string-blank? : String → Bool
;;; Check if string is empty or contains only whitespace.
;;;
;;; Examples:
;;;   (string-blank? "")        => #t
;;;   (string-blank? "   ")     => #t
;;;   (string-blank? "  a  ")   => #f
(define (string-blank? str)
  (let loop ([chars (string->list str)])
    (cond
      [(null? chars) #t]
      [(whitespace? (car chars)) (loop (cdr chars))]
      [else #f])))

;;; ============================================================
;;; Export Note
;;; ============================================================
;;;
;;; To use these utilities in your code, load this file:
;;;   (load "thimble/string-utils.ss")
;;;
;;; All functions are now available in the global namespace.
;;;
;;; TODO: Once The Fold has a proper module system, convert this
;;; to a module with explicit exports.
