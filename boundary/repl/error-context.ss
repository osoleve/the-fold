;;; boundary/repl/error-context.ss — Enhanced error context capture
;;;
;;; Provides better error diagnostics for development:
;;;   (last-error)       - Show details of last error
;;;   (with-context ctx expr) - Evaluate with context label
;;;
;;; This is Shell code: uses mutable state.

;;; ====
;;; State
;;; ====

;;; *last-error* : (or #f (list condition context timestamp))
(define *last-error* #f)

;;; *error-context-stack* : (List String)
;;; Stack of context labels for nested operations.
(define *error-context-stack* '())

;;; ====
;;; Context Management
;;; ====

;;; push-context! : String -> Void
(define (push-context! label)
  (set! *error-context-stack* (cons label *error-context-stack*)))

;;; pop-context! : -> Void
(define (pop-context!)
  (unless (null? *error-context-stack*)
    (set! *error-context-stack* (cdr *error-context-stack*))))

;;; current-context : -> (List String)
(define (current-context)
  *error-context-stack*)

;;; format-context : -> String
(define (format-context)
  (if (null? *error-context-stack*)
      ""
      (string-append "Context: "
                     (apply string-append
                            (map (lambda (c) (string-append c " > "))
                                 (reverse *error-context-stack*))))))

;;; ====
;;; Error Capture
;;; ====

;;; capture-error! : Condition -> Void
;;; Store error with current context for later inspection.
;;; Only captures if no error is already stored OR if current context
;;; is deeper (preserves the context closest to the actual error).
(define (capture-error! e)
  (when (or (not *last-error*)
            (> (length (current-context))
               (length (cadr *last-error*))))
    (set! *last-error*
          (list e
                (current-context)
                (current-time)))))

;;; format-error-detail : Condition -> String
;;; Format an error with all available details.
(define (format-error-detail e)
  (let ([lines '()])
    ;; Basic message
    (when (message-condition? e)
      (set! lines (cons (format "Message: ~a" (condition-message e)) lines)))
    ;; Who raised it
    (when (who-condition? e)
      (set! lines (cons (format "Who: ~a" (condition-who e)) lines)))
    ;; Irritants
    (when (irritants-condition? e)
      (let ([irr (condition-irritants e)])
        (unless (null? irr)
          (set! lines (cons (format "Irritants: ~s" irr) lines)))))
    ;; Syntax info if available
    (when (syntax-violation? e)
      (set! lines (cons (format "Form: ~s" (syntax-violation-form e)) lines))
      (when (syntax-violation-subform e)
        (set! lines (cons (format "Subform: ~s" (syntax-violation-subform e)) lines))))
    ;; Reverse to get natural order
    (apply string-append
           (map (lambda (l) (string-append "  " l "\n"))
                (reverse lines)))))

;;; ====
;;; Public API
;;; ====

;;; last-error : -> Void
;;; Display the last captured error with full details.
(define (last-error)
  (if (not *last-error*)
      (display "No errors captured.\n")
      (let ([e (car *last-error*)]
            [ctx (cadr *last-error*)]
            [ts (caddr *last-error*)])
        (display "Last Error\n")
        (display "====\n")
        (display (format-error-detail e))
        (unless (null? ctx)
          (display (format "Context stack: ~a\n"
                           (string-join (reverse ctx) " > "))))
        (display (format "Time: ~a\n" ts)))))

;;; clear-error! : -> Void
;;; Clear the last error.
(define (clear-error!)
  (set! *last-error* #f)
  (display "Error cleared.\n"))

;;; with-context : String Expr -> Any
;;; Evaluate expression with a context label.
;;; If an error occurs, the label is included in error info.
(define-syntax with-context
  (syntax-rules ()
    [(_ label body ...)
     (begin
       (push-context! label)
       (guard (e [else
                  (capture-error! e)
                  (pop-context!)
                  (raise e)])
         (let ([result (begin body ...)])
           (pop-context!)
           result)))]))

;;; string-join : (List String) String -> String
;;; Join strings with separator.
(define (string-join lst sep)
  (if (null? lst)
      ""
      (let loop ([rest (cdr lst)]
                 [acc (car lst)])
        (if (null? rest)
            acc
            (loop (cdr rest)
                  (string-append acc sep (car rest)))))))

;;; ====
;;; Startup (respects *quiet* mode)
;;; ====

(unless (and (top-level-bound? '*quiet*) *quiet*)
  (display "Error context ready.\n")
  (display "  (last-error)     - Show last error details\n")
  (display "  (clear-error!)   - Clear captured error\n"))
