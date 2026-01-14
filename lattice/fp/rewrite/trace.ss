;;; core/fp/rewrite/trace.ss — Transformation Trace Structures
;;;
;;; Data structures for recording step-by-step rewrite traces.
;;; Traces capture the history of rule applications for:
;;;   - Educational display of transformation steps
;;;   - Verification that equivalence is preserved
;;;   - Debugging and inspection
;;;
;;; Trace Structure:
;;;   ((initial . Expr)        - Starting expression
;;;    (steps . List)          - List of trace steps
;;;    (final . Expr)          - Result expression
;;;    (verified? . Boolean))  - Whether equivalence was verified
;;;
;;; Step Structure:
;;;   ((from . Expr)           - Expression before rewrite
;;;    (to . Expr)             - Expression after rewrite
;;;    (rule . Symbol)         - Name of rule applied
;;;    (position . Path)       - Path into expression tree
;;;    (bindings . Alist))     - Variable bindings used
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Trace Step Construction
;;; ====

;;; make-trace-step : Expr × Expr × Symbol × Path × Alist → Step
;;; Create a single trace step recording a rewrite.
(define (make-trace-step from to rule position bindings)
  `((from . ,from)
    (to . ,to)
    (rule . ,rule)
    (position . ,position)
    (bindings . ,bindings)))

;;; ====
;;; Trace Step Accessors
;;; ====

;;; step? : Any → Boolean
(define (step? x)
  (and (pair? x)
       (assq 'from x)
       (assq 'to x)
       (assq 'rule x)))

;;; step-from : Step → Expr
(define (step-from s)
  (cdr (assq 'from s)))

;;; step-to : Step → Expr
(define (step-to s)
  (cdr (assq 'to s)))

;;; step-rule : Step → Symbol
(define (step-rule s)
  (cdr (assq 'rule s)))

;;; step-position : Step → Path
(define (step-position s)
  (let ([pos (assq 'position s)])
       (if pos (cdr pos) '())))

;;; step-bindings : Step → Alist
(define (step-bindings s)
  (let ([binds (assq 'bindings s)])
       (if binds (cdr binds) '())))

;;; ====
;;; Trace Construction
;;; ====

;;; make-trace : Expr → Trace
;;; Create a new trace starting with an initial expression.
(define (make-trace initial)
  `((initial . ,initial)
    (steps . ())
    (final . ,initial)
    (verified? . #f)))

;;; ====
;;; Trace Predicates
;;; ====

;;; trace? : Any → Boolean
(define (trace? x)
  (and (pair? x)
       (assq 'initial x)
       (assq 'steps x)
       (assq 'final x)))

;;; ====
;;; Trace Accessors
;;; ====

;;; trace-initial : Trace → Expr
(define (trace-initial t)
  (cdr (assq 'initial t)))

;;; trace-steps : Trace → (List Step)
(define (trace-steps t)
  (cdr (assq 'steps t)))

;;; trace-final : Trace → Expr
(define (trace-final t)
  (cdr (assq 'final t)))

;;; trace-verified? : Trace → Boolean
(define (trace-verified? t)
  (let ([v (assq 'verified? t)])
       (if v (cdr v) #f)))

;;; trace-step-count : Trace → Nat
(define (trace-step-count t)
  (length (trace-steps t)))

;;; ====
;;; Trace Modification
;;; ====

;;; trace-add-step : Trace × Step → Trace
;;; Add a step to the trace, updating the final expression.
(define (trace-add-step trace step)
  `((initial . ,(trace-initial trace))
    (steps . ,(append (trace-steps trace) (list step)))
    (final . ,(step-to step))
    (verified? . #f)))

;;; trace-set-verified : Trace × Boolean → Trace
;;; Mark the trace as verified (or not).
(define (trace-set-verified trace verified?)
  `((initial . ,(trace-initial trace))
    (steps . ,(trace-steps trace))
    (final . ,(trace-final trace))
    (verified? . ,verified?)))

;;; trace-append : Trace × Trace → Trace
;;; Append two traces (second must start where first ends).
(define (trace-append t1 t2)
  `((initial . ,(trace-initial t1))
    (steps . ,(append (trace-steps t1) (trace-steps t2)))
    (final . ,(trace-final t2))
    (verified? . #f)))

;;; ====
;;; Trace Display Helpers
;;; ====

;;; trace-rules-used : Trace → (List Symbol)
;;; Get list of all rules used in a trace.
(define (trace-rules-used trace)
  (map step-rule (trace-steps trace)))

;;; trace-rules-unique : Trace → (List Symbol)
;;; Get unique rules used in a trace.
(define (trace-rules-unique trace)
  (let loop ([rules (trace-rules-used trace)] [seen '()] [acc '()])
       (cond
        [(null? rules) (reverse acc)]
        [(memq (car rules) seen) (loop (cdr rules) seen acc)]
        [else (loop (cdr rules) (cons (car rules) seen) (cons (car rules) acc))])))

;;; step->string : Step → String
;;; Format a step for display.
(define (step->string s)
  (format "~a: ~a → ~a"
          (step-rule s)
          (step-from s)
          (step-to s)))

;;; trace->summary : Trace → String
;;; Brief summary of a trace.
(define (trace->summary t)
  (format "~a step~a: ~a → ~a"
          (trace-step-count t)
          (if (= (trace-step-count t) 1) "" "s")
          (trace-initial t)
          (trace-final t)))
