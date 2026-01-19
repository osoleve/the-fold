;;; boundary/debug/exploration-error-handler.ss — Enhanced Error Handler for Exploration Scripts
;;;
;;; Fixes the ~s placeholder bug in exploration scripts by providing
;;; proper error message formatting for Chez Scheme conditions.
;;;
;;; This addresses the issue where (condition-message e) returns raw
;;; format templates like "~s is not a valid index for ~s" instead of
;;; properly formatted messages.
;;;
;;; Usage in exploration scripts:
;;;   (guard (e (else (display (format-exploration-error e))))
;;;   Instead of: (guard (e (else (display (condition-message e))))
;;;
;;; NOTE: string-contains? provided by core/prelude.ss.
;;;       string-replace-all and string-find-pattern are unique to this module.

(load "core/base/prelude.ss")

;;; ====
;;; Error Message Formatting
;;; ====

;;; format-exploration-error : Condition → String
;;; Format a Chez Scheme condition for display in exploration scripts.
;;; This fixes the ~s placeholder bug by extracting and formatting
;;; the actual error details.
(define (format-exploration-error condition)
  (let* ([msg (condition-message condition)]
         [who (if (who-condition? condition) (condition-who condition) #f)]
         [irritants (if (irritants-condition? condition)
                        (condition-irritants condition)
                        '())])
        (format-condition-message msg who irritants)))

;;; format-condition-message : String × Symbol × List → String
;;; Format the condition message, handling ~s placeholders properly.
(define (format-condition-message msg who irritants)
  (let ([formatted-msg (if (null? irritants)
                           ;; No irritants - try to fix unfilled placeholders
                           (fix-format-placeholders msg)
                           ;; Has irritants - fill them in
                           (fill-format-placeholders msg irritants))])
       (if who
           (string-append (symbol->string who) ": " formatted-msg)
           formatted-msg)))

;;; fix-format-placeholders : String → String
;;; Replace unfilled format directives with descriptive placeholders.
(define (fix-format-placeholders msg)
  ;; Handle common Chez Scheme error message patterns
  (cond
   ;; String index errors
   [(string-contains? msg "~s is not a valid index for ~s")
    "Invalid string index"]
   ;; List/car/cdr errors
   [(string-contains? msg "~s is not a pair")
    "Expected a pair but got a non-pair value"]
   ;; Type errors
   [(string-contains? msg "~s is not a number")
    "Expected a number but got a non-numeric value"]
   ;; Division by zero
   [(string-contains? msg "~s by zero")
    "Division by zero"]
   ;; Generic format placeholder fixes
   [else
    (let ([fixed msg])
         ;; Replace common format directives
         (set! fixed (string-replace-all fixed "~s" "<value>"))
         (set! fixed (string-replace-all fixed "~a" "<value>"))
         (set! fixed (string-replace-all fixed "~d" "<number>"))
         (set! fixed (string-replace-all fixed "~x" "<hex>"))
         fixed)]))

;;; fill-format-placeholders : String × List → String
;;; Fill format placeholders with actual values from irritants.
(define (fill-format-placeholders msg irritants)
  (guard (e [else (fix-format-placeholders msg)])  ; Fallback if formatting fails
         ;; Try to use Scheme's format function
         (apply format (cons msg irritants))))

;;; ====
;;; String Utilities
;;; ====

;;; NOTE: string-contains? provided by core/prelude.ss

;;; string-replace-all : String × String × String → String
;;; Replace all occurrences of pattern with replacement.
(define (string-replace-all str pattern replacement)
  (let loop ([s str]
             [result ""])
       (let ([idx (string-find-pattern s pattern)])
            (if idx
                (loop (substring s (+ idx (string-length pattern)) (string-length s))
                      (string-append result
                                     (substring s 0 idx)
                                     replacement))
                (string-append result s)))))

;;; string-find-pattern : String × String → Nat | #f
;;; Find first occurrence of pattern in string.
(define (string-find-pattern str pattern)
  (let ([plen (string-length pattern)]
        [slen (string-length str)])
       (let loop ([i 0])
            (cond
             [(> (+ i plen) slen) #f]
             [(string=? pattern (substring str i (+ i plen))) i]
             [else (loop (+ i 1))]))))

;;; ====
;;; Condition Inspection Helpers
;;; ====

;;; who-condition? : Any → Bool
(define (who-condition? condition)
  (and (condition? condition)
       (memq 'who (condition-rtd condition))))

;;; condition-who : Condition → Symbol | #f
(define (condition-who condition)
  (if (who-condition? condition)
      (condition-ref condition 'who)
      #f))

;;; irritants-condition? : Any → Bool
(define (irritants-condition? condition)
  (and (condition? condition)
       (memq 'irritants (condition-rtd condition))))

;;; condition-irritants : Condition → List | '()
(define (condition-irritants condition)
  (if (irritants-condition? condition)
      (condition-ref condition 'irritants)
      '()))

;;; ====
;;; Enhanced Guard Macro
;;; ====

;;; exploration-guard : (() → α) × (Condition → α) → α
;;; Enhanced guard that formats errors properly.
(define-syntax exploration-guard
  (syntax-rules ()
                [(_ body handler)
                 (guard (e (else (handler (format-exploration-error e))))
                        body)]))

;;; ====
;;; Testing and Demonstration
;;; ====

;;; demonstrate-error-formatting : → void
;;; Show before/after comparison of error formatting.
(define (demonstrate-error-formatting)
  (display "╔════════════════════════════════════════════════════════════╗\n")
  (display "║        EXPLORATION ERROR FORMATTER DEMONSTRATION          ║\n")
  (display "╚════════════════════════════════════════════════════════════╝\n\n")
  
  (display "BEFORE (raw condition-message):\n")
  (guard (e (else
             (display "  Raw: ")
             (display (condition-message e))
             (newline)))
         (string-ref "abc" 10))
  
  (display "\nAFTER (format-exploration-error):\n")
  (guard (e (else
             (display "  Formatted: ")
             (display (format-exploration-error e))
             (newline)))
         (string-ref "abc" 10))
  
  (display "\nMore examples:\n")
  
  ;; List operations
  (display "\n  (car '()): ")
  (guard (e (else (display (format-exploration-error e))))
         (car '()))
  
  (display "\n  Type errors: ")
  (guard (e (else (display (format-exploration-error e))))
         (+ "hello" 5))
  
  (display "\n\n✓ Error formatter ready for use!\n")
  (display "  Use (format-exploration-error e) instead of (condition-message e)\n")
  (display "  Or use (exploration-guard body handler) for automatic formatting\n"))

;;; ====
;;; Module Export
;;; ====

(display "Enhanced exploration error handler loaded.\n")
(display "Use (format-exploration-error condition) for properly formatted errors.\n")
