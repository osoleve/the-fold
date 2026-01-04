;;; condition-formatter.ss
;;; Centralized condition formatting for better error messages.
;;; Fixes the #<compound condition> problem by properly decomposing
;;; compound conditions and extracting all available information.

;;; ============================================================
;;; String Utilities for Placeholder Fixing
;;; ============================================================

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
             [(string=? (substring str i (+ i plen)) pattern) i]
             [else (loop (+ i 1))]))))

;;; fix-unfilled-placeholders : String → String
;;; Replace common unfilled format placeholders with readable text.
(define (fix-unfilled-placeholders msg)
  (let* ([msg (string-replace-all msg "~s" "<value>")]
         [msg (string-replace-all msg "~a" "<value>")]
         [msg (string-replace-all msg "~d" "<number>")]
         [msg (string-replace-all msg "~x" "<hex>")]
         [msg (string-replace-all msg "~b" "<binary>")])
        msg))

;;; ============================================================
;;; Condition Inspection and Decomposition
;;; ============================================================

;;; try-extract-condition-info : Condition → String | #f
;;; Try to extract useful information from a simple condition.
;;; Returns #f if we can't extract anything useful.
(define (try-extract-condition-info sc)
  (cond
   [(message-condition? sc)
    ;; Message condition - try to get the message
    (guard (e [else #f])
           (condition-message sc))]
   
   [(who-condition? sc)
    ;; Who condition - identify where the error occurred
    (guard (e [else #f])
           (format "in ~s" (condition-who sc)))]
   
   [(irritants-condition? sc)
    ;; Irritants - the problematic values
    (guard (e [else #f])
           (let ([irr (condition-irritants sc)])
                (if (null? irr)
                    #f
                    (format "with values: ~s" irr))))]
   
   [(syntax-violation? sc)
    ;; Syntax error
    (guard (e [else #f])
           (format "syntax error in: ~s" (syntax-violation-form sc)))]
   
   [else
    ;; Unknown condition type - try to get its record type name
    (guard (e [else #f])
           (let ([rtd (record-rtd sc)])
                (if rtd
                    (format "[~a]" (record-type-name rtd))
                    #f)))]))

;;; format-simple-condition : SimpleCondition → String
;;; Format a simple condition component, with fallback for unknown types.
(define (format-simple-condition sc)
  (let ([info (try-extract-condition-info sc)])
       (if info
           info
           ;; Last resort: try to display it, but trap if that fails too
           (guard (e [else "[unknown condition]"])
                  (format "~a" sc)))))

;;; ============================================================
;;; Main Formatter
;;; ============================================================

;;; safe-format-with-irritants : String × List → String
;;; Safely format a message with irritants, fixing placeholders if format fails.
(define (safe-format-with-irritants msg irritants)
  (guard (e [else (fix-unfilled-placeholders msg)])
         (if (null? irritants)
             msg
             (apply format (cons msg irritants)))))

;;; format-condition : Condition → String
;;; Format any condition (simple or compound) into a readable error message.
;;; This is the main entry point that should be used everywhere.
(define (format-condition e)
  (cond
   ;; Try message condition first (most common case)
   [(message-condition? e)
    (let ([msg (guard (ex [else "Error (message unavailable)"])
                      (condition-message e))]
          [irr (if (irritants-condition? e)
                   (guard (ex [else '()])
                          (condition-irritants e))
                   '())])
         (safe-format-with-irritants msg irr))]
   
   ;; Compound condition without a message - decompose it
   [(condition? e)
    (let ([parts (guard (ex [else '()])
                        (simple-conditions e))])
         (if (null? parts)
             "Unknown error (no condition details available)"
             (let ([formatted-parts (map format-simple-condition parts)])
                  ;; Filter out empty/useless parts and join with " | "
                  (let ([useful-parts (filter (lambda (s)
                                                      (and (string? s)
                                                           (> (string-length s) 0)
                                                           (not (string=? s "[unknown condition]"))))
                                              formatted-parts)])
                       (if (null? useful-parts)
                           "Error (no readable details available)"
                           (apply string-append
                                  (let loop ([ps useful-parts])
                                       (cond
                                        [(null? ps) '()]
                                        [(null? (cdr ps)) (list (car ps))]
                                        [else (cons (car ps)
                                                    (cons " | " (loop (cdr ps))))]))))))))]
   
   ;; Not even a condition object - format as-is with protection
   [else
    (guard (ex [else "Error (could not format error message)"])
           (format "~a" e))]))

;;; format-condition-verbose : Condition → String
;;; Like format-condition but includes all condition components, even unknown ones.
;;; Useful for debugging the error handling itself.
(define (format-condition-verbose e)
  (cond
   [(message-condition? e)
    (let ([msg (guard (ex [else "Error (message unavailable)"])
                      (condition-message e))]
          [irr (if (irritants-condition? e)
                   (guard (ex [else '()])
                          (condition-irritants e))
                   '())])
         (string-append
          "MESSAGE: " (safe-format-with-irritants msg irr)
          (if (condition? e)
              (let ([parts (simple-conditions e)])
                   (if (> (length parts) 1)
                       (string-append "\nCOMPONENTS: "
                                      (apply string-append
                                             (map (lambda (p)
                                                          (string-append "\n  - " (format-simple-condition p)))
                                                  parts)))
                       ""))
              "")))]
   
   [(condition? e)
    (let ([parts (guard (ex [else '()])
                        (simple-conditions e))])
         (string-append
          "COMPOUND CONDITION:\n"
          (apply string-append
                 (map (lambda (p)
                              (string-append "  - " (format-simple-condition p) "\n"))
                      parts))))]
   
   [else
    (guard (ex [else "Error (could not format error message)"])
           (format "ERROR VALUE: ~a" e))]))
