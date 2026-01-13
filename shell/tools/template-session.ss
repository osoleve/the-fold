;;; shell/tools/template-session.ss — Template Session Manager
;;; @module template-session
;;; @requires lattice/dsl/template/template
;;;
;;; Stateful session manager for grammar-driven code construction.
;;; Tracks current template, provides undo, displays status.
;;;
;;; This is Shell code: impure, handles state and IO.
;;;
;;; Usage:
;;;   (ts-start '(define $sig $body))   ; Start session
;;;   (ts-fill '$sig '($name $args))    ; Fill a hole
;;;   (ts-holes)                         ; Show remaining holes
;;;   (ts-undo)                          ; Revert last fill
;;;   (ts-compile)                       ; Compile when complete

(load "lattice/dsl/template/template.ss")

;;; ============================================================
;;; Session State
;;; ============================================================

;;; Current session state: #(template undo-stack)
(define *template-session* (make-parameter #f))

;;; session-template : → Template | #f
(define (session-template)
  (let ([s (*template-session*)])
    (and s (vector-ref s 0))))

;;; session-undo-stack : → (List Template)
(define (session-undo-stack)
  (let ([s (*template-session*)])
    (if s (vector-ref s 1) '())))

;;; set-session-template! : Template → Unit
(define (set-session-template! t)
  (let ([s (*template-session*)])
    (when s
      (vector-set! s 0 t))))

;;; push-undo! : Template → Unit
(define (push-undo! t)
  (let ([s (*template-session*)])
    (when s
      (vector-set! s 1 (cons t (vector-ref s 1))))))

;;; pop-undo! : → Template | #f
(define (pop-undo!)
  (let ([s (*template-session*)])
    (and s
         (let ([stack (vector-ref s 1)])
           (and (pair? stack)
                (begin
                  (vector-set! s 1 (cdr stack))
                  (car stack)))))))

;;; ============================================================
;;; Session API
;;; ============================================================

;;; ts-start : Expr → Unit
;;; Start a new template session with the given expression.
(define (ts-start expr)
  (let ([t (new-template expr)])
    (*template-session* (vector t '()))
    (ts-show-status)))

;;; ts-active? : → Bool
;;; Is there an active session?
(define (ts-active?)
  (and (*template-session*) #t))

;;; ts-holes : → (List Symbol)
;;; Get current holes.
(define (ts-holes)
  (let ([t (session-template)])
    (if t
        (template-holes t)
        (error 'ts-holes "No active session"))))

;;; ts-fill : Symbol × Expr → Unit
;;; Fill a hole with a value.
(define (ts-fill hole-sym value)
  (let ([t (session-template)])
    (unless t
      (error 'ts-fill "No active session"))
    (unless (memq hole-sym (template-holes t))
      (error 'ts-fill (string-append "Unknown hole: " (symbol->string hole-sym))))
    (push-undo! t)
    (set-session-template! (fill-hole t hole-sym value))
    (ts-show-status)))

;;; ts-undo : → Unit
;;; Undo the last fill operation.
(define (ts-undo)
  (let ([prev (pop-undo!)])
    (if prev
        (begin
          (set-session-template! prev)
          (display "Undone.\n")
          (ts-show-status))
        (display "Nothing to undo.\n"))))

;;; ts-compile : → Expr
;;; Compile the template if complete.
(define (ts-compile)
  (let ([t (session-template)])
    (unless t
      (error 'ts-compile "No active session"))
    (if (template-complete? t)
        (let ([result (compile-template t)])
          (display "Compiled:\n")
          (pretty-print result)
          (newline)
          result)
        (begin
          (display "Cannot compile - ")
          (display (template-status t))
          (newline)
          #f))))

;;; ts-show : → Unit
;;; Pretty-print the current template.
(define (ts-show)
  (let ([t (session-template)])
    (if t
        (begin
          (display "Template:\n")
          (pretty-print (template-expr t))
          (newline))
        (display "No active session.\n"))))

;;; ts-show-status : → Unit
;;; Display status after an operation.
(define (ts-show-status)
  (let ([t (session-template)])
    (when t
      (display (template-status t))
      (newline))))

;;; ts-status : → String
;;; Get status string.
(define (ts-status)
  (let ([t (session-template)])
    (if t
        (template-status t)
        "No active session")))

;;; ts-reset : → Unit
;;; Clear the current session.
(define (ts-reset)
  (*template-session* #f)
  (display "Session cleared.\n"))

;;; ============================================================
;;; Pretty Print Helper
;;; ============================================================

;;; pretty-print : Expr → Unit
;;; Print expression with indentation.
(define (pretty-print expr)
  (write expr))

;;; ============================================================
;;; Shorthand Aliases
;;; ============================================================

;;; t> : Expr → Unit  (alias for ts-start)
(define t> ts-start)

;;; t! : → Expr  (alias for ts-compile)
(define t! ts-compile)

;;; t? : → Unit  (alias for ts-show + ts-show-status)
(define (t?)
  (ts-show)
  (ts-show-status))
