;;; lattice/dsl/template/template.ss — Grammar-Driven Code Construction
;;; @module template
;;; @requires core/base/prelude
;;;
;;; A DSL for constructing S-expressions via EBNF-like production statements.
;;; Holes ($name) are non-terminals that get filled incrementally.
;;; Once all holes are filled, the template compiles to an S-expression.
;;;
;;; This is Lattice code: pure, total, no IO.
;;;
;;; Key concepts:
;;;   - Holes: Symbols starting with $ (e.g., $body, $args)
;;;   - Templates: Expressions containing holes
;;;   - Filling: Replace a hole with a value (which may contain new holes)
;;;   - Compilation: Extract the final S-expression when complete

(load "core/base/prelude.ss")

;;; ============================================================
;;; Compat: sexpr->string (from block.ss)
;;; ============================================================

;;; sexpr->string : Any → String
;;; Convert any s-expression to a string using write semantics.
(define (sexpr->string obj)
  (let ([port (open-output-string)])
    (write obj port)
    (get-output-string port)))

;;; ============================================================
;;; Hole Detection
;;; ============================================================

;;; hole? : Any → Bool
;;; Returns #t if x is a hole (symbol starting with $).
(define (hole? x)
  (and (symbol? x)
       (let ([s (symbol->string x)])
         (and (> (string-length s) 1)
              (char=? (string-ref s 0) #\$)))))

;;; hole-name : Symbol → Symbol
;;; Extract the name from a hole: $foo → foo
(define (hole-name h)
  (let ([s (symbol->string h)])
    (string->symbol (substring s 1 (string-length s)))))

;;; find-holes : Expr → (List Symbol)
;;; Recursively find all holes in an expression.
;;; Returns unique holes in order of first occurrence (left-to-right).
(define (find-holes expr)
  (define (collect expr)
    (cond
      [(hole? expr) (list expr)]
      [(pair? expr) (append (collect (car expr)) (collect (cdr expr)))]
      [else '()]))
  (remove-duplicates (collect expr)))

;;; ============================================================
;;; String Utilities (needed before compilation)
;;; ============================================================

;;; holes->string : (List Symbol) → String
;;; Format hole list for display: ($a $b $c) → "$a, $b, $c"
(define (holes->string holes)
  (if (null? holes)
      ""
      (let loop ([h holes] [acc ""])
        (if (null? h)
            acc
            (loop (cdr h)
                  (if (string=? acc "")
                      (symbol->string (car h))
                      (string-append acc ", " (symbol->string (car h)))))))))

;;; ============================================================
;;; Template Type
;;; ============================================================

;;; Template record: holds expression and cached hole list
(define-record-type template
  (fields expr holes))

;;; new-template : Expr → Template
;;; Create a template from an expression, computing holes.
(define (new-template expr)
  (make-template expr (find-holes expr)))

;;; template-complete? : Template → Bool
;;; Returns #t if the template has no unfilled holes.
(define (template-complete? t)
  (null? (template-holes t)))

;;; template-hole-count : Template → Nat
;;; Returns the number of unfilled holes.
(define (template-hole-count t)
  (length (template-holes t)))

;;; ============================================================
;;; Substitution
;;; ============================================================

;;; subst-hole : Expr × Symbol × Expr → Expr
;;; Replace all occurrences of hole-sym with value in expr.
(define (subst-hole expr hole-sym value)
  (cond
    [(eq? expr hole-sym) value]
    [(pair? expr) (cons (subst-hole (car expr) hole-sym value)
                        (subst-hole (cdr expr) hole-sym value))]
    [else expr]))

;;; fill-hole : Template × Symbol × Expr → Template
;;; Fill a single hole, returning a new template.
;;; The value may contain holes, which propagate to the result.
(define (fill-hole t hole-sym value)
  (let ([new-expr (subst-hole (template-expr t) hole-sym value)])
    (new-template new-expr)))

;;; fill-holes : Template × Alist → Template
;;; Fill multiple holes from an association list.
;;; alist: ((hole-sym . value) ...)
(define (fill-holes t alist)
  (if (null? alist)
      t
      (fill-holes (fill-hole t (caar alist) (cdar alist))
                  (cdr alist))))

;;; ============================================================
;;; Compilation
;;; ============================================================

;;; compile-template : Template → Expr | Error
;;; If complete, return the expression; otherwise error.
(define (compile-template t)
  (if (template-complete? t)
      (template-expr t)
      (error 'compile-template
             (string-append "unfilled holes: " (holes->string (template-holes t))))))

;;; try-compile-template : Template → (Ok Expr) | (Err (List Symbol))
;;; Safe compilation that returns a result type.
(define (try-compile-template t)
  (if (template-complete? t)
      (list 'ok (template-expr t))
      (list 'err (template-holes t))))

;;; ============================================================
;;; Display Utilities
;;; ============================================================

;;; template->string : Template → String
;;; Pretty-print a template expression.
(define (template->string t)
  (sexpr->string (template-expr t)))

;;; template-status : Template → String
;;; Return a status string describing remaining holes.
(define (template-status t)
  (let ([holes (template-holes t)])
    (if (null? holes)
        "Complete!"
        (string-append "Holes: " (holes->string holes)))))

;;; ============================================================
;;; Implicit Parens Helper
;;; ============================================================

;;; wrap-if-multiple : (List Token) → Expr
;;; If list has >1 element, wrap in parens (make a list).
;;; Single element stays as-is.
(define (wrap-if-multiple tokens)
  (if (and (pair? tokens) (null? (cdr tokens)))
      (car tokens)
      tokens))
