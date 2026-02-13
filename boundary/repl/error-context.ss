(doc 'module 'error-context)
(doc 'description "Enhanced error context capture for better development diagnostics")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'state)

(doc *last-error* 'type "(or #f (list condition context timestamp))")
(define *last-error* #f)

(doc *error-context-stack* 'type "(List String)")
(doc *error-context-stack* 'description "Stack of context labels for nested operations")
(define *error-context-stack* '())

(doc 'section 'context-management)

(doc push-context! 'type "String -> Void")
(define (push-context! label)
  (set! *error-context-stack* (cons label *error-context-stack*)))

(doc pop-context! 'type "-> Void")
(define (pop-context!)
  (unless (null? *error-context-stack*)
    (set! *error-context-stack* (cdr *error-context-stack*))))

(doc current-context 'type "-> (List String)")
(define (current-context)
  *error-context-stack*)

(doc format-context 'type "-> String")
(define (format-context)
  (if (null? *error-context-stack*)
      ""
      (string-append "Context: "
                     (apply string-append
                            (map (lambda (c) (string-append c " > "))
                                 (reverse *error-context-stack*))))))

(doc 'section 'error-capture)

(doc capture-error! 'type "Condition -> Void")
(doc capture-error! 'description "Store error with current context for later inspection")
(doc capture-error! 'note "Only captures if no error is already stored OR if current context is deeper (preserves the context closest to the actual error)")
(define (capture-error! e)
  (when (or (not *last-error*)
            (> (length (current-context))
               (length (cadr *last-error*))))
    (set! *last-error*
          (list e
                (current-context)
                (current-time)))))

(doc format-error-detail 'type "Condition -> String")
(doc format-error-detail 'description "Format an error with all available details")
(define (format-error-detail e)
  (let ([lines '()])
    (when (message-condition? e)
      (let ([msg (condition-message e)]
            [irr (if (irritants-condition? e) (condition-irritants e) '())])
        ;; Apply irritants to message template when both are present,
        ;; so "expected ~s, got ~s" renders as "expected 3, got 4"
        ;; instead of showing raw format directives
        (set! lines
              (cons (format "Message: ~a"
                            (if (null? irr)
                                msg
                                (guard (exn [else msg])
                                  (apply format msg irr))))
                    lines))))
    (when (who-condition? e)
      (set! lines (cons (format "Who: ~a" (condition-who e)) lines)))
    (when (irritants-condition? e)
      (let ([irr (condition-irritants e)])
        (unless (null? irr)
          (set! lines (cons (format "Irritants: ~s" irr) lines)))))
    (when (syntax-violation? e)
      (set! lines (cons (format "Form: ~s" (syntax-violation-form e)) lines))
      (when (syntax-violation-subform e)
        (set! lines (cons (format "Subform: ~s" (syntax-violation-subform e)) lines))))
    (apply string-append
           (map (lambda (l) (string-append "  " l "\n"))
                (reverse lines)))))

(doc 'section 'public-api)

(doc last-error 'type "-> Void")
(doc last-error 'description "Display the last captured error with full details")
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

(doc clear-error! 'type "-> Void")
(doc clear-error! 'description "Clear the last error")
(define (clear-error!)
  (set! *last-error* #f)
  (display "Error cleared.\n"))

(doc with-context 'type "String Expr -> Any")
(doc with-context 'description "Evaluate expression with a context label. If an error occurs, the label is included in error info")
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

(doc string-join 'type "(List String) String -> String")
(doc string-join 'description "Join strings with separator")
(define (string-join lst sep)
  (if (null? lst)
      ""
      (let loop ([rest (cdr lst)]
                 [acc (car lst)])
        (if (null? rest)
            acc
            (loop (cdr rest)
                  (string-append acc sep (car rest)))))))

(doc 'section 'startup)

(unless (and (top-level-bound? '*quiet*) *quiet*)
  (display "Error context ready.\n")
  (display "  (last-error)     - Show last error details\n")
  (display "  (clear-error!)   - Clear captured error\n"))
