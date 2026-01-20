(load "core/base/prelude.ss")

(doc 'module 'exploration-error-handler)
(doc 'description "Enhanced Error Handler for Exploration Scripts")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude))

(doc 'note "Fixes the ~s placeholder bug in exploration scripts by providing proper error message formatting for Chez Scheme conditions")
(doc 'note "Usage: (guard (e (else (display (format-exploration-error e)))) instead of (condition-message e)")
(doc 'note "String utilities: string-contains? provided by core/prelude.ss; string-replace-all and string-find-pattern unique to this module")

(doc 'section 'main-api)

(doc format-exploration-error 'export #t)
(define (format-exploration-error condition)
  (doc 'type '(-> Condition String))
  (doc 'description "Format a Chez Scheme condition for display in exploration scripts - fixes ~s placeholder bug")
  (let* ([msg (condition-message condition)]
         [who (if (who-condition? condition) (condition-who condition) #f)]
         [irritants (if (irritants-condition? condition)
                        (condition-irritants condition)
                        '())])
        (format-condition-message msg who irritants)))

(define (format-condition-message msg who irritants)
  (doc 'type '(-> String (Maybe Symbol) (List Any) String))
  (doc 'description "Format the condition message, handling ~s placeholders properly")
  (let ([formatted-msg (if (null? irritants)
                           (fix-format-placeholders msg)
                           (fill-format-placeholders msg irritants))])
       (if who
           (string-append (symbol->string who) ": " formatted-msg)
           formatted-msg)))

(define (fix-format-placeholders msg)
  (doc 'type '(-> String String))
  (doc 'description "Replace unfilled format directives with descriptive placeholders")
  (cond
   [(string-contains? msg "~s is not a valid index for ~s")
    "Invalid string index"]
   [(string-contains? msg "~s is not a pair")
    "Expected a pair but got a non-pair value"]
   [(string-contains? msg "~s is not a number")
    "Expected a number but got a non-numeric value"]
   [(string-contains? msg "~s by zero")
    "Division by zero"]
   [else
    (let ([fixed msg])
         (set! fixed (string-replace-all fixed "~s" "<value>"))
         (set! fixed (string-replace-all fixed "~a" "<value>"))
         (set! fixed (string-replace-all fixed "~d" "<number>"))
         (set! fixed (string-replace-all fixed "~x" "<hex>"))
         fixed)]))

(define (fill-format-placeholders msg irritants)
  (doc 'type '(-> String (List Any) String))
  (doc 'description "Fill format placeholders with actual values from irritants")
  (guard (e [else (fix-format-placeholders msg)])
         (apply format (cons msg irritants))))

(doc 'section 'string-utilities)

(define (string-replace-all str pattern replacement)
  (doc 'type '(-> String String String String))
  (doc 'description "Replace all occurrences of pattern with replacement")
  (let loop ([s str]
             [result ""])
       (let ([idx (string-find-pattern s pattern)])
            (if idx
                (loop (substring s (+ idx (string-length pattern)) (string-length s))
                      (string-append result
                                     (substring s 0 idx)
                                     replacement))
                (string-append result s)))))

(define (string-find-pattern str pattern)
  (doc 'type '(-> String String (Maybe Nat)))
  (doc 'description "Find first occurrence of pattern in string")
  (let ([plen (string-length pattern)]
        [slen (string-length str)])
       (let loop ([i 0])
            (cond
             [(> (+ i plen) slen) #f]
             [(string=? pattern (substring str i (+ i plen))) i]
             [else (loop (+ i 1))]))))

(doc 'section 'condition-helpers)

(define (who-condition? condition)
  (doc 'type '(-> Any Bool))
  (doc 'description "Check if condition has 'who field")
  (and (condition? condition)
       (memq 'who (condition-rtd condition))))

(define (condition-who condition)
  (doc 'type '(-> Condition (Maybe Symbol)))
  (doc 'description "Extract 'who field from condition")
  (if (who-condition? condition)
      (condition-ref condition 'who)
      #f))

(define (irritants-condition? condition)
  (doc 'type '(-> Any Bool))
  (doc 'description "Check if condition has 'irritants field")
  (and (condition? condition)
       (memq 'irritants (condition-rtd condition))))

(define (condition-irritants condition)
  (doc 'type '(-> Condition (List Any)))
  (doc 'description "Extract 'irritants field from condition")
  (if (irritants-condition? condition)
      (condition-ref condition 'irritants)
      '()))

(doc 'section 'convenience-macro)

(doc exploration-guard 'export #t)
(define-syntax exploration-guard
  (doc 'description "Enhanced guard that formats errors properly")
  (syntax-rules ()
                [(_ body handler)
                 (guard (e (else (handler (format-exploration-error e))))
                        body)]))

(doc 'section 'demo)

(doc demonstrate-error-formatting 'export #t)
(define (demonstrate-error-formatting)
  (doc 'type '(-> void))
  (doc 'description "Show before/after comparison of error formatting")
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

  (display "\n  (car '()): ")
  (guard (e (else (display (format-exploration-error e))))
         (car '()))

  (display "\n  Type errors: ")
  (guard (e (else (display (format-exploration-error e))))
         (+ "hello" 5))

  (display "\n\n✓ Error formatter ready for use!\n")
  (display "  Use (format-exploration-error e) instead of (condition-message e)\n")
  (display "  Or use (exploration-guard body handler) for automatic formatting\n"))

(display "Enhanced exploration error handler loaded.\n")
(display "Use (format-exploration-error condition) for properly formatted errors.\n")
