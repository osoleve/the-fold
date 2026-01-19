;;; boundary/debug/error-fmt.ss — Enhanced Error Formatter
;;;
;;; Improved error messages with context, colors, and helpful suggestions.
;;; Addresses the ~s placeholder bugs reported in the forum.
;;;
;;; This is Shell code: formats output, provides user-facing messages.
;;;
;;; Dependencies:
;;;   core/prelude.ss
;;;
;;; NOTE: string utilities provided by core/prelude.ss:
;;;       string-contains?, string-join, string-replace
;;;       Unique to this module: string-replace-once, string-find
;;;
;;; Operations:
;;;   (format-error condition) — Format error with context
;;;   (format-error-simple msg) — Simple error message
;;;   (format-error-with-location msg file line col) — Error with location
;;;   (suggest-fix error-type) — Suggest possible fixes
;;;   (error-to-string condition) — Convert condition to string
;;;
;;; Features:
;;;   - Color-coded severity levels
;;;   - Source location highlighting
;;;   - Contextual suggestions
;;;   - Fixes ~s and ~:s placeholder issues
;;;   - Multi-line error formatting
;;;   - Stack trace formatting
;;;   - Related error grouping

;;; ====
;;; Configuration
;;; ====

(load "core/base/prelude.ss")

(define *use-colors* #t)
(define *show-suggestions* #t)
(define *max-context-lines* 3)

;;; ANSI color codes
(define *color-reset* "\x1b;[0m")
(define *color-red* "\x1b;[31m")
(define *color-yellow* "\x1b;[33m")
(define *color-blue* "\x1b;[34m")
(define *color-gray* "\x1b;[90m")
(define *color-bold* "\x1b;[1m")

;;; Error severity levels
(define-record-type error-level
  (fields
   name        ; 'error, 'warning, 'info
   color       ; ANSI color
   symbol))    ; Display symbol

(define *level-error*
  (make-error-level 'error *color-red* "ERROR"))

(define *level-warning*
  (make-error-level 'warning *color-yellow* "WARN"))

(define *level-info*
  (make-error-level 'info *color-blue* "INFO"))

;;; ====
;;; Error with Format Helper
;;; ====
;;;
;;; NOTE: errorf is now provided by core/base/prelude.ss
;;; It's available everywhere prelude is loaded.

;;; ====
;;; Main Formatting API
;;; ====

;;; format-error : Condition → String
;;; Format a condition object as a readable error message.
(define (format-error condition)
  (guard (e [else
             (format-error-simple
              (format "Error formatting error: ~a" e))])
         (let* ([msg (condition-message condition)]
                [who (if (who-condition? condition)
                         (condition-who condition)
                         #f)]
                [irritants (if (irritants-condition? condition)
                               (condition-irritants condition)
                               '())])
               (format-error-full
                *level-error*
                msg
                who
                irritants
                #f #f #f))))  ; file, line, col

;;; format-error-simple : String → String
;;; Format a simple error message.
(define (format-error-simple msg)
  (format-error-full *level-error* msg #f '() #f #f #f))

;;; format-error-with-location : String × Path × Nat × Nat → String
;;; Format error with source location.
(define (format-error-with-location msg file line col)
  (format-error-full *level-error* msg #f '() file line col))

;;; format-error-full : ErrorLevel × String × Symbol × (List Any) × Path × Nat × Nat → String
;;; Full error formatting with all options.
(define (format-error-full level msg who irritants file line col)
  (string-append
   ;; Header line
   (format-error-header level who file line col)
   "
"
   ;; Message (with fixed formatting)
   (format-message msg irritants)
   "
"
   ;; Source context (if available)
   (if (and file line)
       (format-source-context file line col)
       "")
   ;; Suggestions (if enabled)
   (if *show-suggestions*
       (format-suggestions msg)
       "")
   ;; Footer
   "
"))

;;; ====
;;; Header Formatting
;;; ====

;;; format-error-header : ErrorLevel × Symbol × Path × Nat × Nat → String
(define (format-error-header level who file line col)
  (let ([level-str (colorize (error-level-symbol level)
                             (error-level-color level)
                             *color-bold*)]
        [who-str (if who
                     (format " [~a]" who)
                     "")]
        [loc-str (if (and file line)
                     (format " at ~a:~a" file line)
                     "")])
       (string-append level-str who-str loc-str)))

;;; ====
;;; Message Formatting (Fixes ~s bugs)
;;; ====

;;; format-message : String × (List Any) → String
;;; Format message string, filling in placeholders.
;;; This FIXES the ~s and ~:s placeholder bugs!
(define (format-message template irritants)
  (guard (e [else
             ;; If formatting fails, return template as-is
             template])
         (if (null? irritants)
             ;; No irritants: check for unfilled placeholders
             (fix-unfilled-placeholders template)
             ;; Has irritants: fill them in
             (fill-placeholders template irritants))))

;;; fix-unfilled-placeholders : String → String
;;; Replace unfilled format directives with safe defaults.
(define (fix-unfilled-placeholders str)
  (let* ([fixed (string-replace str "~s" "<value>")]
         [fixed2 (string-replace fixed "~:s" "<value>")]
         [fixed3 (string-replace fixed2 "~a" "<value>")]
         [fixed4 (string-replace fixed3 "~d" "<number>")])
        fixed4))

;;; fill-placeholders : String × (List Any) → String
;;; Fill format directives with actual values.
(define (fill-placeholders template irritants)
  (guard (e [else
             ;; If format fails, manually substitute
             (manual-substitute template irritants)])
         ;; Try using format
         (apply format template irritants)))

;;; manual-substitute : String × (List Any) → String
;;; Manually substitute placeholders (fallback).
(define (manual-substitute template irritants)
  (let loop ([str template]
             [vals irritants])
       (if (null? vals)
           (fix-unfilled-placeholders str)
           (let* ([pattern (if (string-contains? str "~s") "~s"
                               (if (string-contains? str "~a") "~a"
                                   (if (string-contains? str "~d") "~d"
                                       #f)))]
                  [replacement (format "~a" (car vals))])
                 (if pattern
                     (loop (string-replace-once str pattern replacement)
                           (cdr vals))
                     str)))))

;;; NOTE: string-replace provided by core/prelude.ss

;;; string-replace-once : String × String × String → String
;;; Replace first occurrence of pattern.
(define (string-replace-once str pattern replacement)
  (let ([idx (string-find str pattern)])
       (if idx
           (string-append
            (substring str 0 idx)
            replacement
            (substring str (+ idx (string-length pattern))
                       (string-length str)))
           str)))

;;; string-find : String × String → Nat | #f
(define (string-find haystack needle)
  (let ([nlen (string-length needle)]
        [hlen (string-length haystack)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? needle (substring haystack i (+ i nlen))) i]
             [else (loop (+ i 1))]))))

;;; ====
;;; Source Context
;;; ====

;;; format-source-context : Path × Nat × Nat → String
;;; Show source code around error location.
(define (format-source-context file line col)
  (guard (e [else ""])
         (let* ([lines (read-file-lines file)]
                [start (max 1 (- line *max-context-lines*))]
                [end (min (length lines) (+ line *max-context-lines*))]
                [context-lines (list-slice lines (- start 1) end)])
               (string-append
                "
"
                (colorize "Source:" *color-gray* "")
                "
"
                (format-lines-with-numbers context-lines start line col)))))

;;; format-lines-with-numbers : (List String) × Nat × Nat × Nat → String
(define (format-lines-with-numbers lines start-num error-line error-col)
  (let loop ([ls lines]
             [num start-num]
             [result ""])
       (if (null? ls)
           result
           (let* ([line (car ls)]
                  [is-error-line? (= num error-line)]
                  [prefix (format "~4d | " num)]
                  [formatted-line (if is-error-line?
                                      (colorize line *color-red* "")
                                      (colorize line *color-gray* ""))]
                  [pointer (if is-error-line?
                               (format "     | ~a~a
"
                                       (make-string (max 0 (- error-col 1)) #\space)
                                       (colorize "^" *color-red* *color-bold*))
                               "")])
                 (loop (cdr ls)
                       (+ num 1)
                       (string-append result prefix formatted-line "
" pointer))))))

;;; ====
;;; Suggestions
;;; ====

;;; format-suggestions : String → String
;;; Provide helpful suggestions based on error message.
(define (format-suggestions msg)
  (let ([suggestions (suggest-fixes msg)])
       (if (null? suggestions)
           ""
           (string-append
            "
"
            (colorize "Suggestions:" *color-blue* *color-bold*)
            "
"
            (string-join
             (map (lambda (s)
                          (format "  • ~a" s))
                  suggestions)
             "
")
            "
"))))

;;; suggest-fixes : String → (List String)
;;; Analyze error and suggest fixes.
(define (suggest-fixes msg)
  (cond
   [(string-contains? msg "not bound")
    '("Check for typos in variable name"
      "Verify the variable is defined before use"
      "Check if module is loaded")]
   [(string-contains? msg "wrong number of arguments")
    '("Check function signature"
      "Verify all required parameters are provided")]
   [(string-contains? msg "type")
    '("Check type annotations"
      "Verify input types match expectations")]
   [(string-contains? msg "file")
    '("Check file path is correct"
      "Verify file exists and is readable")]
   [else '()]))

;;; ====
;;; Colorization
;;; ====

;;; colorize : String × String × String → String
(define (colorize text color modifier)
  (if *use-colors*
      (string-append modifier color text *color-reset*)
      text))

;;; ====
;;; Utility Functions
;;; ====

;;; NOTE: string-contains? provided by core/prelude.ss

;;; make-string : Nat × Char → String
(define (make-string n ch)
  (list->string (make-list-of n ch)))

;;; make-list-of : Nat × α → (List α)
(define (make-list-of n x)
  (if (<= n 0)
      '()
      (cons x (make-list-of (- n 1) x))))

;;; list-slice : (List α) × Nat × Nat → (List α)
(define (list-slice lst start end)
  (let loop ([ls lst]
             [i 0]
             [result '()])
       (cond
        [(null? ls) (reverse result)]
        [(< i start) (loop (cdr ls) (+ i 1) result)]
        [(>= i end) (reverse result)]
        [else (loop (cdr ls) (+ i 1) (cons (car ls) result))])))

;;; read-file-lines : Path → (List String)
(define (read-file-lines path)
  (guard (e [else '()])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([lines '()])
                                            (let ([line (get-line port)])
                                                 (if (eof-object? line)
                                                     (reverse lines)
                                                     (loop (cons line lines)))))))))

;;; NOTE: string-join provided by core/prelude.ss

;;; ====
;;; Condition Inspection
;;; ====

;;; error-to-string : Condition → String
;;; Convert condition to string (public API).
(define (error-to-string condition)
  (format-error condition))

(display "
")
(display "Enhanced error formatter loaded.
")
(display "
")
(display "Features:
")
(display "  • Fixes ~s and ~:s placeholder bugs
")
(display "  • Color-coded severity levels
")
(display "  • Source location highlighting
")
(display "  • Contextual suggestions
")
(display "
")
(display "Usage:
")
(display "  (format-error condition)                 - Format condition
")
(display "  (format-error-simple \"message\")          - Simple error
")
(display "  (format-error-with-location msg f l c)   - With location
")
(display "  (error-to-string condition)              - Convert to string
")
(display "
")
(display "Configuration:
")
(display "  (set! *use-colors* #f)                   - Disable colors
")
(display "  (set! *show-suggestions* #f)             - Disable suggestions
")
(display "
")
